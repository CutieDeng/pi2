#lang tstring racket
;; tools/git.rkt — 一等公民 git 工具（真实项目开发的最低要求：结构化 diff/commit）。
;;
;; 相比让模型走裸 bash 拼 `git …`：
;;   * argv 数组直传 git（不过 shell）——commit message 等含空格/引号的参数零转义地狱，
;;     弱模型(deepseek/本地)也能可靠产出；也杜绝 shell 注入。
;;   * 权限分级为 'mutating（介于只读工具与 bash 'dangerous 之间）：normal 模式会征询，
;;     yolo 放行；比 bash 更贴合「仓库内改动」的语义。
;;   * 统一在 workdir 内执行，输出封顶截断。

(require
 racket/port racket/list racket/string
 (file "../tool.rkt")
 (file "../model.rkt")                  ; current-session-id / config-model（自动化提交的元数据）
 (only-in (file "../credentials.rkt") config-home)   ; 用户级 commit-meta.rktd
 (file "bash.rkt")                      ; truncate-output（复用输出封顶）
 (file "../commit-datum.rkt")           ; git_history：提交消息的 datum 解析与查询
) ; end require

(define GIT-EXE (or (find-executable-path "git") "/usr/bin/git"))
(define GIT-TIMEOUT-SECS 120)

;; 跑 git（argv 直传，不过 shell）→ (values exit-code combined-output)。
(define (run-git workdir args)
  (parameterize ([current-directory workdir])
    (define-values (proc out in _err)
      (apply subprocess #f #f 'stdout GIT-EXE args))
    (close-output-port in)
    (define buf (open-output-string))
    (define pump (thread (lambda () (copy-port out buf))))
    (define done (sync/timeout GIT-TIMEOUT-SECS proc))
    (cond
      [(not done)
       (subprocess-kill proc #t)
       (thread-wait pump)
       (values 'timeout (get-output-string buf))]
      [else
       (thread-wait pump)
       (close-input-port out)
       (values (subprocess-status proc) (get-output-string buf))])
  ) ; end parameterize
) ; end define run-git

(struct git-tool ()
  #:methods gen:tool
  [(define (tool-name _t) "git")
   (define (tool-permission-level _t) 'mutating)
   (define (tool-spec _t)
     (function-spec "git"
                    (string-append
                     "Run git in the working directory. Pass args as an array of strings "
                     "(argv, NOT a shell string) — e.g. [\"status\",\"--short\"], [\"diff\"], "
                     "[\"commit\",\"-m\",\"my message with spaces\"], [\"add\",\"-A\"], [\"log\",\"--oneline\",\"-10\"]. "
                     "No shell quoting needed. Returns combined stdout+stderr.")
                    (hasheq 'args (hasheq 'type "array"
                                          'description "git subcommand + flags as separate strings"
                                          'items (hasheq 'type "string")))
                    (list "args")
     ) ; end function-spec
   ) ; end define tool-spec
   (define (tool-run _t input ctx)
     (define raw (input-ref input 'args))
     (define args0 (and (list? raw) (filter string? raw)))
     ;; 模型经这个工具提交 = 自动化提交 → 消息里加 (pi2 agent (session …) (model …))（用户 2026-10-26：自动化提交必须写元数据）
     (define args (and args0 (pair? args0) (tag-commit-args args0 (tool-ctx-workdir ctx) (tool-ctx-config ctx))))
     (cond
       [(or (not args) (null? args))
        (err-outcome "missing required parameter: args (a non-empty array of strings, e.g. [\"status\"])")]
       [else
        (define-values (code out) (run-git (tool-ctx-workdir ctx) args))
        (define output (truncate-output out))
        (define label f"git {(string-join args " ")}")
        (cond
          [(eq? code 'timeout)
           (err-outcome f"git timed out after {GIT-TIMEOUT-SECS}s\n{output}" #:display f"{label} — timeout")]
          [(zero? code)
           (ok-outcome (if (string=? (string-trim output) "") "(no output)" output) #:display label)]
          [else
           (err-outcome f"git exited {code}\n{output}" #:display f"{label} — exit {code}")])]
     ) ; end cond
   ) ; end define tool-run
  ] ; end methods
) ; end struct git-tool

(define (make-git-tool) (git-tool))

;; git commit 的参数：-m / --message[=] / -mTEXT / -F / --file[=] 收成一条消息（git 自己也是多个 -m 用空行连起来），
;; 加自动化标签后用一个 -m 传；没有消息参数（--amend --no-edit 等）→ 原样。
(define (tag-commit-args args workdir cfg)
  (cond
    [(not (equal? (car args) "commit")) args]
    [else
     (define (read-msg-file f) (with-handlers ([exn:fail? (lambda (_e) #f)]) (file->string* (if (absolute-path? f) f (build-path workdir f)))))
     (let loop ([xs (cdr args)] [msgs '()] [rest '()] [bad? #f])
       (cond
         [(null? xs)
          (if (or bad? (null? msgs))
              args
              (let* ([msg (string-join (reverse msgs) "\n\n")]
                     [tag (automation-tag 'agent #:session (current-session-id) #:model (and cfg (config-model cfg)))])
                (append (list "commit" "-m" (add-automation-meta msg tag)) (reverse rest))))]   ; -m 放最前：rest 里可能有 -- 路径
         [(and (member (car xs) '("-m" "--message")) (pair? (cdr xs))) (loop (cddr xs) (cons (cadr xs) msgs) rest bad?)]
         [(regexp-match #rx"^--message=(.*)$" (car xs)) => (lambda (m) (loop (cdr xs) (cons (cadr m) msgs) rest bad?))]
         [(regexp-match #rx"^-m(.+)$" (car xs)) => (lambda (m) (loop (cdr xs) (cons (cadr m) msgs) rest bad?))]
         [(and (member (car xs) '("-F" "--file")) (pair? (cdr xs)))
          (define c (read-msg-file (cadr xs))) (loop (cddr xs) (if c (cons c msgs) msgs) rest (or bad? (not c)))]
         [(regexp-match #rx"^--file=(.*)$" (car xs)) => (lambda (m) (define c (read-msg-file (cadr m))) (loop (cdr xs) (if c (cons c msgs) msgs) rest (or bad? (not c))))]
         [(equal? (car xs) "--") (loop '() msgs (append (reverse xs) rest) bad?)]
         [else (loop (cdr xs) msgs (cons (car xs) rest) bad?)]))]))
(define (file->string* p) (call-with-input-file p port->string))

;; ---------------------------------------------------------------- git_history（只读）
;; 按 TYPE 与元数据查提交（消息是 Racket datum：(TYPE "title" (元数据 …) "detail")，见 commit-datum.rkt）。
;; 改动文件取 git 的实际记录。只读：rev 不许以 - 开头（否则 --output=文件 之类会让 git log 写文件）；path 放在 -- 之后。
(define HISTORY-SCAN 2000)              ; 有过滤条件时最多扫多少个提交
(define HISTORY-MAX 200)

(define (history-run workdir input)
  (define (strs k) (let ([v (input-ref input k #f)]) (cond [(string? v) (list v)] [(list? v) (filter string? v)] [else '()])))
  (define types (map string->symbol (strs 'type)))
  (define meta-strs (strs 'meta))
  (define metas (map parse-meta-query meta-strs))
  (define rev (input-str input 'rev #f))
  (define path (input-str input 'path #f))
  (define limit (max 1 (min HISTORY-MAX (or (input-int input 'limit #f) 20))))
  (define grep (input-str input 'grep #f))
  (define filtered? (or (pair? types) (pair? metas) (and grep (not (string=? grep "")))))
  (cond
    [(for/or ([m metas] [s meta-strs]) (and (not m) s))
     => (lambda (s) (err-outcome (format "meta query is not a readable Racket datum: ~s (examples: \"breaking\", \"(scope gui)\")" s)))]
    [(and rev (regexp-match? #rx"^-" rev)) (err-outcome "rev must be a revision or range (e.g. \"v1..HEAD\"), not an option")]
    [else
     (define args
       (append (list "log" "--no-color" "--format=%x1e%H%x1f%aI%x1f%B%x1f" "--name-only"
                     (format "-n~a" (if filtered? HISTORY-SCAN limit)))
               (let ([v (input-str input 'since #f)]) (if v (list (string-append "--since=" v)) '()))
               (let ([v (input-str input 'until #f)]) (if v (list (string-append "--until=" v)) '()))
               (if rev (list rev) '())
               (if path (list "--full-diff" "--" path) '())))   ; --full-diff：按路径挑提交，但列出提交的全部改动文件
     (define-values (code out) (run-git workdir args))
     ;; 标签规则：内置 < 用户 {config-home}/commit-meta.rktd < 项目 <仓库根>/.pi/commit-meta.rktd
     (define root (let-values ([(c o) (run-git workdir (list "rev-parse" "--show-toplevel"))]) (and (eqv? c 0) (string-trim o))))
     (define-values (user-rules user-err) (read-commit-meta-file (build-path (config-home) "commit-meta.rktd")))
     (define-values (proj-rules proj-err) (read-commit-meta-file (and root (build-path root ".pi" "commit-meta.rktd"))))
     (define rules (merge-rules BUILTIN-RULES user-rules proj-rules))
     (define rule-errs (filter values (list user-err proj-err)))
     (cond
       [(eq? code 'timeout) (err-outcome (format "git log timed out after ~as" GIT-TIMEOUT-SECS))]
       [(not (zero? code)) (err-outcome (format "git log exited ~a\n~a" code (truncate-output out)))]
       [else
        (define all (parse-git-log out))
        (define hits (for/list ([c all] #:when (commit-matches? c #:types types #:metas metas #:grep grep)) c))
        (define shown (take hits (min limit (length hits))))
        (define detail? (eq? (input-ref input 'detail #f) #t))
        (define files? (eq? (input-ref input 'files #f) #t))
        (define (line c)
          (define ls (commit-labels c rules))
          (string-append
           (format "~a ~a ~a ~s~a meta=~s files=~a~a"
                   (substring (commit-hash c) 0 (min 7 (string-length (commit-hash c)))) (substring (commit-date c) 0 (min 10 (string-length (commit-date c))))
                   (or (commit-type c) "(text)") (commit-title c)
                   (apply string-append (map (lambda (l) (format " [~a]" l)) ls))
                   (commit-meta c) (length (commit-files c))
                   (if (eq? (commit-shape c) 'legacy) " (3-field message)" ""))
           (if (and files? (pair? (commit-files c)))
               (string-append "\n    " (string-join (take (commit-files c) (min 30 (length (commit-files c)))) "\n    ")
                              (if (> (length (commit-files c)) 30) (format "\n    … ~a more" (- (length (commit-files c)) 30)) ""))
               "")
           (if (and detail? (not (string=? (commit-detail c) "")))
               (let ([d (commit-detail c)]) (string-append "\n    | " (string-replace (if (> (string-length d) 800) (string-append (substring d 0 800) "…") d) "\n" "\n    | ")))
               "")))
        (define-values (by-type by-meta) (history-stats hits))
        (define stats
          (if (eq? (input-ref input 'stats #f) #t)
              (format "types: ~a\nmetadata heads: ~a\n"
                      (string-join (for/list ([p by-type]) (format "~a ~a" (car p) (cdr p))) ", ")
                      (if (null? by-meta) "(none)" (string-join (for/list ([p by-meta]) (format "~a ~a" (car p) (cdr p))) ", ")))
              ""))
        (define warn (if (null? rule-errs) "" (string-append "commit-meta rules ignored: " (string-join rule-errs "; ") "\n")))
        (if (eq? (input-ref input 'changelog #f) #t)
            ;; changelog：全部命中的提交（不受 limit 限制），按标签规则归节 / 排除，其余按 TYPE
            (ok-outcome (string-append warn (changelog-markdown hits rules #:title (if rev (format "Changelog (~a)" rev) "Changelog")))
                        #:display (format "git_history — changelog of ~a commit(s)" (length hits)))
            (ok-outcome #:ui (commits-ui shown (length hits) rules)
                        (string-append warn
                                       (format "~a matching commit(s) of ~a scanned~a:\n" (length hits) (length all)
                                               (if (> (length hits) (length shown)) (format ", showing ~a" (length shown)) ""))
                                       stats
                                       (string-join (map line shown) "\n"))
                        #:display (format "git_history — ~a commit(s)" (length hits))))])]))

;; 给界面的结构化结果（工具卡片画成一行行的提交：hash、TYPE 标记、标题、标签片）：只有显示用的字段，不回填模型。
;; 标签片 auto = 自动化提交的 pi2 标签（界面用强调色画，一眼区分）。
(define (commits-ui cs total rules)
  (hasheq 'kind "commits" 'total total
          'commits (for/list ([c (in-list cs)])
                     (define labels (commit-labels c rules))
                     (hasheq 'hash (substring (commit-hash c) 0 (min 7 (string-length (commit-hash c))))
                             'date (substring (commit-date c) 0 (min 10 (string-length (commit-date c))))
                             'type (if (commit-type c) (symbol->string (commit-type c)) 'null)
                             'title (commit-title c)
                             'files (length (commit-files c))
                             'labels (for/list ([l (in-list labels)] [h (in-list (label-heads c rules))])
                                       (hasheq 'text l 'auto (eq? h 'pi2)))))))
;; 与 commit-labels 一一对应的元数据头（同样跳过 #:label #f 的）
(define (label-heads c rules)
  (for*/list ([item (in-list (commit-meta c))]
              [h (in-value (meta-head item))] #:when h
              [r (in-value (rule-for* h rules))]
              #:unless (and r (eq? (tag-rule-label r) #f)))
    h))
(define (rule-for* head rules) (for/first ([r (in-list rules)] #:when (eq? (tag-rule-head r) head)) r))

(struct git-history-tool ()
  #:methods gen:tool
  [(define (tool-name _t) "git_history")
   (define (tool-permission-level _t) 'read-only)
   (define (tool-spec _t)
     (function-spec "git_history"
                    (string-append
                     "Query this repository's commits. Commit messages here are Racket datums (TYPE \"title\" (metadata ...) \"detail\"): "
                     "TYPE is FEAT / FIX / REFACTOR / TEST / DOCS / BUILD; metadata items are symbols (flags, e.g. breaking) or lists headed by a symbol (tags, e.g. (scope gui)). "
                     "Filter by type and metadata; changed files come from git itself. Read-only.")
                    (hasheq 'type (hasheq 'type "array" 'items (hasheq 'type "string") 'description "Commit types to include, e.g. [\"FEAT\",\"FIX\"] (default: all).")
                            'meta (hasheq 'type "array" 'items (hasheq 'type "string")
                                          'description "Metadata queries, all must match; each is a Racket datum: \"breaking\" = flag or tag named breaking; \"(scope gui)\" = tag scope whose arguments include gui.")
                            'path (hasheq 'type "string" 'description "Only commits that touched this path.")
                            'grep (hasheq 'type "string" 'description "Case-insensitive text in title or detail.")
                            'rev (hasheq 'type "string" 'description "Revision or range, e.g. \"v1..HEAD\" (default: HEAD).")
                            'since (hasheq 'type "string" 'description "git date, e.g. \"2 weeks ago\" or \"2026-10-01\".")
                            'until (hasheq 'type "string" 'description "git date.")
                            'limit (hasheq 'type "integer" 'description "Max commits to show (default 20, max 200).")
                            'detail (hasheq 'type "boolean" 'description "Include each commit's detail text (clipped).")
                            'files (hasheq 'type "boolean" 'description "List each commit's changed files.")
                            'stats (hasheq 'type "boolean" 'description "Count types and metadata heads over the matches.")
                            'changelog (hasheq 'type "boolean" 'description "Return a markdown changelog of ALL matches instead of the list: sections from the project's tag rules (.pi/commit-meta.rktd: a tag can get its own section or be left out), the rest grouped by TYPE."))
                    '()))
   (define (tool-run _t input ctx) (history-run (tool-ctx-workdir ctx) input))])

(define (make-git-history-tool) (git-history-tool))

(provide make-git-tool make-git-history-tool run-git)
