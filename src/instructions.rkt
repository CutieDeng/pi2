#lang tstring racket
;; instructions.rkt — 人写的指令分层（design-memory.md §2.1 M1，对齐 Codex 的 AGENTS.md 规则，再加项目 / 工作区两层）。
;; 注入顺序（后者在后，冲突时后者优先）：
;;   1. 全局：{config-home}/AGENTS.override.md，没有就 {config-home}/AGENTS.md
;;   2. 目录 catalog 里的指令（界面推来，GUI 那台机器的设置）：项目 → super-workspace → 工作区
;;   3. 目录树：从 git 根走到工作目录，每一级取一个文件：AGENTS.override.md > AGENTS.md > CLAUDE.md > .pi/AGENTS.md > PI.md
;;      （没有 git 根 = 只看工作目录本身）；聊天会话（匿名工作区）没有这一层
;; 合计上限 max-bytes（缺省 32 KiB，按 UTF-8 字节）：超了的那一段截断，后面的整段略去；每段都留在清单里（标 truncated / omitted）。

(require racket/file racket/string (file "credentials.rkt") (only-in (file "test-flow.rkt") load-flow flow-instructions flow-file))

(define MAX-BYTES 32768)
(define GLOBAL-FILES '("AGENTS.override.md" "AGENTS.md"))
(define DIR-FILES '("AGENTS.override.md" "AGENTS.md" "CLAUDE.md" ".pi/AGENTS.md" "PI.md"))
(define HEADER "\n\n## Project instructions")          ; 系统提示词里这一节的开头（resources.rkt 旧格式同一个开头：split-instructions 认它）

;; 读文件，最多 limit 字节（大文件不整个读进来）；读不了 → #f
(define (read-limited p limit)
  (with-handlers ([exn:fail? (lambda (_e) #f)])
    (call-with-input-file p
      (lambda (in) (let ([b (read-bytes (add1 limit) in)]) (if (eof-object? b) "" (bytes->string/utf-8 b (integer->char #xFFFD))))))))

(define (first-file dir names limit)
  (for/or ([n (in-list names)])
    (define p (build-path dir n))
    (and (file-exists? p)
         (let ([t (read-limited p limit)])
           (and t (non-empty-string? (string-trim t)) (cons (path->string (simplify-path p)) t))))))

;; 工作目录往上找 git 根（有 .git 目录或文件的一级）；找不到 → #f
(define (git-root dir)
  (let loop ([d (simplify-path (path->complete-path dir))] [k 0])
    (cond
      [(> k 64) #f]
      [(or (directory-exists? (build-path d ".git")) (file-exists? (build-path d ".git"))) d]
      [else (let-values ([(up _n _d?) (split-path d)]) (if (path? up) (loop up (add1 k)) #f))])))

;; git 根 → 工作目录，一路上的目录（含两端）
(define (tree-dirs workdir)
  (define wd (simplify-path (path->complete-path workdir)))
  (define root (git-root wd))
  (if (not root)
      (list wd)
      (let loop ([d wd] [acc '()])
        (define acc2 (cons d acc))
        (if (equal? (path->directory-path d) (path->directory-path root))
            acc2
            (let-values ([(up _n _d?) (split-path d)]) (if (path? up) (loop up acc2) acc2))))))

;; 截到 n 字节以内（字符边界）
(define (truncate-bytes s n)
  (let loop ([k (min (string-length s) n)])
    (if (<= (bytes-length (string->bytes/utf-8 (substring s 0 k))) n) (substring s 0 k) (loop (sub1 k)))))

;; → 段的表：每段 (hasheq 'kind "global"|"project"|"super_workspace"|"workspace"|"dir" 'label 路径或名字 'text 注入的文字
;;                        'bytes 原文字节数 'truncated 布尔 'omitted 布尔)
;; catalog：界面推来的 (listof (hasheq 'kind 'name 'text))，已按 项目 → super-workspace → 工作区 排好
(define (collect-instructions workdir #:home [home (config-home)] #:catalog [catalog '()] #:tree? [tree? #t] #:max-bytes [max-bytes MAX-BYTES])
  (define raw
    (append
     (let ([g (and home (directory-exists? home) (first-file home GLOBAL-FILES max-bytes))])
       (if g (list (list "global" (car g) (cdr g))) '()))
     (for/list ([c (in-list catalog)]
                #:when (and (hash? c) (string? (hash-ref c 'text #f)) (non-empty-string? (string-trim (hash-ref c 'text)))))
       (list (format "~a" (hash-ref c 'kind "project")) (format "~a" (hash-ref c 'name "?")) (hash-ref c 'text)))
     (if (and tree? workdir (directory-exists? workdir))
         (for*/list ([d (in-list (tree-dirs workdir))] [f (in-value (first-file d DIR-FILES max-bytes))] #:when f)
           (list "dir" (car f) (cdr f)))
         '())
     ;; 工作区测试流程（.pi/tests.rktd，design-goalmode.md §12）：告诉模型有哪些步骤、什么时候跑、完成前要验收报告
     (if (and tree? workdir (directory-exists? workdir))
         (let-values ([(fl _err) (with-handlers ([exn:fail? (lambda (_e) (values #f #f))]) (load-flow workdir))])
           (if fl (list (list "tests" (flow-file fl) (flow-instructions fl))) '()))
         '())))
  (let loop ([xs raw] [left max-bytes] [acc '()])
    (cond
      [(null? xs) (reverse acc)]
      [else
       (define-values (kind label text) (apply values (car xs)))
       (define b (bytes-length (string->bytes/utf-8 text)))
       (define (seg shown tr? om?) (hasheq 'kind kind 'label label 'text shown 'bytes b 'truncated tr? 'omitted om?))
       (cond
         [(<= b left) (loop (cdr xs) (- left b) (cons (seg text #f #f) acc))]
         [(> left 0) (loop (cdr xs) 0 (cons (seg (truncate-bytes text left) #t #f) acc))]
         [else (loop (cdr xs) 0 (cons (seg "" #f #t) acc))])])))

(define (heading s)
  (define k (hash-ref s 'kind))
  (define l (hash-ref s 'label))
  (define kind-name (string-replace k "_" "-"))
  (cond [(equal? k "global") f"### Global instructions ({l})"]
        [(equal? k "dir") f"### {l}"]
        [(equal? k "tests") f"### Workspace test flow ({l})"]
        [else (string-append "### " kind-name " \"" l "\" (settings)")]))

;; 段 → 系统提示词里的一节（没有可注入的 → ""）
(define (instructions-addendum segs #:max-bytes [max-bytes MAX-BYTES])
  (define shown (filter (lambda (s) (non-empty-string? (hash-ref s 'text))) segs))
  (if (null? shown)
      ""
      (string-append
       HEADER
       "\nThese come from the user's instruction files and settings, from general to specific; when two conflict, the later one takes precedence.\n"
       (apply string-append
              (for/list ([s (in-list shown)])
                (string-append "\n" (heading s) "\n\n" (string-trim (hash-ref s 'text) #:left? #f)
                               (if (hash-ref s 'truncated) (format "\n[… truncated: instructions exceed the ~a KiB limit]" (quotient max-bytes 1024)) "")
                               "\n"))))))

;; 清单（不含正文：给会话记录 / 信息面板）
(define (instructions-summary segs)
  (for/list ([s (in-list segs)])
    (hasheq 'kind (hash-ref s 'kind) 'label (hash-ref s 'label) 'bytes (hash-ref s 'bytes)
            'truncated (hash-ref s 'truncated) 'omitted (hash-ref s 'omitted))))

;; 系统提示词 → (values 不含指令那一节的部分 指令那一节或 "")。指令节总是最后追加的（main.rkt 与本模块）
(define (split-instructions sys) (split-sections sys (list HEADER)))
;; 一般化：在几种节开头里最早出现的那个处切开（内核追加的节——指令、记忆——都在提示词末尾）
(define (split-sections sys headers)
  (cond
    [(not (string? sys)) (values sys "")]
    [else
     (define at (for*/fold ([best #f]) ([h (in-list headers)] [m (in-value (regexp-match-positions (regexp-quote h) sys))] #:when m)
                  (if (or (not best) (< (caar m) best)) (caar m) best)))
     (if at (values (substring sys 0 at) (substring sys at)) (values sys ""))]))

(define INSTRUCTIONS-HEADER HEADER)
(provide MAX-BYTES GLOBAL-FILES DIR-FILES git-root tree-dirs INSTRUCTIONS-HEADER split-sections
         collect-instructions instructions-addendum instructions-summary split-instructions)
