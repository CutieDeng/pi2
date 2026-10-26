#lang tstring racket
;; host-index.rkt — 这台主机上的项目索引（design-memory.md §8，用户 2026-10-25：「可配置模型，建议本地模型；维护一组关键词和关键句的描述，与路径关联；
;; 不必做成所有文件都生成关键词的超大索引」）。粒度 = git 仓库；文件一级不描述（模型知道路径后用现有工具在项目里找）。
;;   discover-repos     从根目录往下找有 .git 的目录（跳过 ~/Library、node_modules、隐藏目录、.Trash；不进 .git）
;;   repo-fingerprint   git HEAD + README / AGENTS.md / 清单文件的修改时刻 → 变了才重新描述
;;   describe-input     给模型的输入：README / AGENTS.md / 清单文件的开头、两层目录树、最近 15 条提交标题（只读白名单里的文件名）
;;   describe-messages / parse-description   让模型写中英双语的短关键词、关键句、别名、一句话总结；解析（容忍 ```json 围栏）
;;   search-projects    按名字 / 关键词 / 关键句 / 总结的词命中个数打分（中英都写了 → 词法匹配就能中英互查）
;;   make-host-index-tool   host_projects（只读）；云端模型缺省不能用（会话可以单独打开），本机模型缺省能用

(require json racket/date (file "tool.rkt") (only-in (file "memory.rkt") query-terms))

(define PRUNE-NAMES '("node_modules" ".Trash" "Library" "compiled" "__pycache__" "target" "dist" "build" ".build" "Pods" "DerivedData"))
(define MANIFESTS '("info.rkt" "package.json" "Cargo.toml" "pyproject.toml" "go.mod" "pom.xml" "build.gradle" "CMakeLists.txt" "Makefile" "Gemfile" "composer.json" "deno.json"))
(define READMES '("README.md" "README.zh.md" "README" "README.rst" "README.txt" "readme.md"))
(define MAX-DEPTH 7)

;; ---------------------------------------------------------------- 发现
(define (git-dir? d) (let ([g (build-path d ".git")]) (or (directory-exists? g) (file-exists? g))))
;; → (listof 路径字符串)，按路径排；deadline：毫秒时刻，过了就停（返回已找到的）
(define (discover-repos roots #:max-depth [max-depth MAX-DEPTH] #:deadline [deadline #f])
  (define found '())
  (define (walk d depth)
    (when (and (or (not deadline) (< (current-inexact-milliseconds) deadline)))
      (when (git-dir? d) (set! found (cons (path->string (simplify-path d)) found)))
      (when (< depth max-depth)
        (for ([n (in-list (with-handlers ([exn:fail? (lambda (_e) '())]) (directory-list d)))])
          (define s (path->string n))
          (define p (build-path d n))
          (when (and (not (regexp-match? #rx"^[.]" s)) (not (member s PRUNE-NAMES))
                     (directory-exists? p) (not (link-exists? p)))
            (walk p (add1 depth)))))))
  (for ([r (in-list roots)])
    (define d (simplify-path (path->complete-path (expand-user-path r))))
    (when (directory-exists? d) (walk d 0)))
  (sort (remove-duplicates found) string<?))

;; ---------------------------------------------------------------- git（带时限；读输出用线程，大输出不卡管道）
(define (git-out dir . args)
  (with-handlers ([exn:fail? (lambda (_e) #f)])
    (define git (find-executable-path "git"))
    (and git
         (let-values ([(p out in err) (apply subprocess #f #f #f git "-C" dir args)])
           (close-output-port in)
           (define acc (box ""))
           (define reader (thread (lambda () (set-box! acc (port->string out)))))
           (define done? (sync/timeout 3 p))
           (unless done? (subprocess-kill p #t))
           (sync/timeout 1 reader)
           (close-input-port out) (close-input-port err)
           (and done? (eqv? (subprocess-status p) 0) (string-trim (unbox acc)))))))

;; 目录里真有的这些文件名（按目录列表逐字比：不区分大小写的文件系统上 readme.md 也「存在」，会重复算）
(define (present dir names)
  (define here (with-handlers ([exn:fail? (lambda (_e) '())]) (map path->string (directory-list dir))))
  (filter (lambda (n) (let ([parts (string-split n "/")]) (and (member (car parts) here) (file-exists? (build-path dir n))))) names))
(define (first-existing dir names) (let ([l (present dir names)]) (and (pair? l) (car l))))
(define (mtime dir n) (let ([p (build-path dir n)]) (if (file-exists? p) (file-or-directory-modify-seconds p) 0)))

;; 指纹：HEAD + 这些文件的修改时刻（任何一个变了 → 重新描述）
(define (repo-fingerprint dir)
  (define head (or (git-out dir "rev-parse" "HEAD") "nohead"))
  (define files (present dir (append READMES '("AGENTS.md" "CLAUDE.md") MANIFESTS)))
  (string-join (cons (substring head 0 (min 12 (string-length head))) (for/list ([n (in-list files)]) (format "~a@~a" n (mtime dir n)))) " "))

(define (repo-info dir)
  (hasheq 'dir dir 'name (let-values ([(_b n _d) (split-path (string->path dir))]) (path->string n))
          'fingerprint (repo-fingerprint dir)
          'remote (or (git-out dir "config" "--get" "remote.origin.url") 'null)
          'mtime (or (let ([s (git-out dir "log" "-1" "--format=%ct")]) (and s (string->number s))) (file-or-directory-modify-seconds dir))))

;; ---------------------------------------------------------------- 给模型的输入
(define (head-of p n)
  (with-handlers ([exn:fail? (lambda (_e) "")])
    (call-with-input-file p (lambda (in) (let ([b (read-bytes n in)]) (if (eof-object? b) "" (bytes->string/utf-8 b (integer->char #xFFFD))))))))
(define (tree-lines dir #:depth [depth 2] #:max [mx 60])
  (define lines '())
  (let walk ([d (string->path dir)] [lv 0])
    (when (< (length lines) mx)
      (define entries (with-handlers ([exn:fail? (lambda (_e) '())]) (sort (map path->string (directory-list d)) string<?)))
      (define files (filter (lambda (n) (and (not (regexp-match? #rx"^[.]" n)) (file-exists? (build-path d n)))) entries))
      (define dirs (filter (lambda (n) (and (not (regexp-match? #rx"^[.]" n)) (not (member n PRUNE-NAMES)) (directory-exists? (build-path d n)))) entries))
      (set! lines (cons (format "~a~a/  (~a files~a)" (make-string (* 2 lv) #\space) (if (= lv 0) "." (let-values ([(_b n _x) (split-path d)]) (path->string n)))
                                (length files) (if (null? files) "" (string-append ": " (string-join (take files (min 6 (length files))) ", "))))
                        lines))
      (when (< lv depth) (for ([n (in-list dirs)]) (walk (build-path d n) (add1 lv))))))
  (reverse lines))
(define (describe-input dir)
  (define parts
    (append
     (let ([r (first-existing dir READMES)]) (if r (list (format "=== ~a\n~a" r (head-of (build-path dir r) 2500))) '()))
     (let ([a (first-existing dir '("AGENTS.md" "CLAUDE.md"))]) (if a (list (format "=== ~a\n~a" a (head-of (build-path dir a) 1500))) '()))
     (for/list ([m (in-list (present dir MANIFESTS))] [_ (in-range 2)])
       (format "=== ~a\n~a" m (head-of (build-path dir m) 1200)))
     (list (string-append "=== tree (2 levels)\n" (string-join (tree-lines dir) "\n"))
           (string-append "=== recent commits\n" (or (git-out dir "log" "--oneline" "-15") "")))))
  (string-join parts "\n\n"))

;; ---------------------------------------------------------------- 让模型描述
(define DESCRIBE-PROMPT
  (string-append
   "Describe this software project for a search index that people query in English and in Chinese. Reply with JSON only:\n"
   "{\"short\": [up to 10 short keywords, 1-3 words each; give each one in English AND in Chinese, e.g. \"gui\", \"图形界面\"],\n"
   " \"long\": [up to 6 key phrases or sentences on what it is, what it does, what it is built with; each in English, then the same in Chinese],\n"
   " \"names\": [names people may call it],\n"
   " \"summary_en\": \"one sentence\", \"summary_zh\": \"一句话\"}\n"
   "Use what the files and recent commits show; when the README looks out of date, trust the commits and the tree."))
(define (describe-messages input) (list (hasheq 'role "user" 'content (string-append DESCRIBE-PROMPT "\n\n" input))))

;; 模型写的列表有几种形状（本机 gemma 实测：有时平的 ["a","甲"]，有时成对 [["a","甲"]]，也可能 [{"en":"a","zh":"甲"}]）→ 摊平成字符串表
(define (flat-strings v)
  (cond [(string? v) (list v)]
        [(list? v) (append-map flat-strings v)]
        [(hash? v) (append-map flat-strings (for/list ([k (in-list (sort (hash-keys v) symbol<?))]) (hash-ref v k)))]   ; en 在 zh 前
        [else '()]))
(define (strs v n [maxlen 200])
  (for/list ([x (in-list (flat-strings v))] [_ (in-range n)] #:when (non-empty-string? (string-trim x)))
    (let ([s (string-trim x)]) (if (> (string-length s) maxlen) (substring s 0 maxlen) s))))
;; 模型回的文字 → hasheq 或 #f
(define (parse-description text)
  (define m (regexp-match #px"(?s:\\{.*\\})" (or text "")))
  (define j (and m (with-handlers ([exn:fail? (lambda (_e) #f)]) (string->jsexpr (car m)))))
  (and (hash? j)
       (let ([d (hasheq 'short (strs (hash-ref j 'short '()) 24 60) 'long (strs (hash-ref j 'long '()) 16 300) 'names (strs (hash-ref j 'names '()) 8 60)
                        'summary_en (let ([s (hash-ref j 'summary_en "")]) (if (string? s) (string-trim s) ""))
                        'summary_zh (let ([s (hash-ref j 'summary_zh "")]) (if (string? s) (string-trim s) "")))])
         (and (or (pair? (hash-ref d 'short)) (pair? (hash-ref d 'long)) (non-empty-string? (hash-ref d 'summary_en))) d))))

;; ---------------------------------------------------------------- 查
;; projects：(listof (hasheq 'dir 'name 'remote 'mtime 'desc (hasheq …) 或 'null))
(define (project-text p)
  (define d (hash-ref p 'desc #f))
  (string-downcase
   (string-join (append (list (format "~a" (hash-ref p 'name "")) (format "~a" (hash-ref p 'dir "")) (format "~a" (hash-ref p 'remote "")))
                        (if (hash? d) (append (hash-ref d 'names '()) (hash-ref d 'short '()) (hash-ref d 'long '())
                                              (list (hash-ref d 'summary_en "") (hash-ref d 'summary_zh ""))) '()))
                " ")))
(define (search-projects projects query #:limit [limit 20])
  (define recent (sort projects > #:key (lambda (p) (let ([m (hash-ref p 'mtime 0)]) (if (real? m) m 0)))))
  (cond
    [(not (and (string? query) (non-empty-string? (string-trim query)))) (take recent (min limit (length recent)))]
    [else
     (define terms (query-terms query))
     (define scored (for*/list ([p (in-list recent)] [s (in-value (let ([t (project-text p)]) (for/sum ([w (in-list terms)]) (if (string-contains? t w) 1 0))))] #:when (> s 0))
                      (cons s p)))
     (map cdr (let ([l (sort scored > #:key car)]) (take l (min limit (length l)))))]))

(define (fmt-project p)
  (define d (hash-ref p 'desc #f))
  (define day (let ([m (hash-ref p 'mtime #f)]) (if (real? m) (let ([dt (seconds->date m)]) (format "~a-~a-~a" (date-year dt) (~r (date-month dt) #:min-width 2 #:pad-string "0") (~r (date-day dt) #:min-width 2 #:pad-string "0"))) "?")))
  (string-append
   (format "~a  ~a  (last commit ~a~a)" (hash-ref p 'name "?") (hash-ref p 'dir "?") day (let ([r (hash-ref p 'remote #f)]) (if (string? r) (format ", ~a" r) "")))
   (if (hash? d)
       (format "\n  ~a / ~a\n  keywords: ~a" (hash-ref d 'summary_en "") (hash-ref d 'summary_zh "") (string-join (hash-ref d 'short '()) ", "))
       "\n  (not described yet)")))

;; ---------------------------------------------------------------- 工具
(struct fn-tool (name level spec run)
  #:methods gen:tool
  [(define (tool-name t) (fn-tool-name t))
   (define (tool-permission-level t) (fn-tool-level t))
   (define (tool-spec t) (fn-tool-spec t))
   (define (tool-run t input ctx) ((fn-tool-run t) input ctx))])

;; view：() → (values 项目表或#f 允许? 不允许的原因)
(define (make-host-index-tool #:view view)
  (fn-tool "host_projects" 'read-only
           (function-spec "host_projects"
                          "Find software projects (git repositories) on the user's machine: path, last commit, remote, and a short description with keywords (English and Chinese). Use it when the user mentions another project, or you need to know where something lives. With no query: the most recently changed projects."
                          (hasheq 'query (hasheq 'type "string" 'description "Words to look for: a name, a technology, what it does (any language). Optional.")
                                  'limit (hasheq 'type "integer" 'description "At most this many (default 20)."))
                          '())
           (lambda (input _ctx)
             (define-values (projects allowed? why) (view))
             (define limit (let ([v (input-ref input 'limit 20)]) (if (exact-positive-integer? v) (min v 100) 20)))
             (cond
               [(not allowed?) (err-outcome why)]
               [(not projects) (err-outcome "the project index is not available: no app is connected, or it has not scanned this machine yet")]
               [else
                (define hits (search-projects projects (input-ref input 'query #f) #:limit limit))
                (ok-outcome (if (null? hits) "no matching projects"
                                (string-append (format "~a of ~a projects\n" (length hits) (length projects))
                                               (string-join (map fmt-project hits) "\n"))))]))))

(provide discover-repos repo-fingerprint repo-info describe-input describe-messages parse-description DESCRIBE-PROMPT
         search-projects fmt-project make-host-index-tool git-out tree-lines)
