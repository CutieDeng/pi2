#lang racket
;; instructions-test.rkt — 指令分层（design-memory.md M1）：全局 / catalog / 目录树、override、git 根、上限、聊天会话
(require rackunit racket/file (file "../src/instructions.rkt") (file "../src/resources.rkt"))

(define (put! p s) (make-directory* (let-values ([(d _n _x) (split-path p)]) d)) (call-with-output-file p (lambda (o) (write-string s o)) #:exists 'truncate))

(define tmp (make-temporary-file "pi2-instr-~a" 'directory))
(define home (build-path tmp "home"))
(define repo (build-path tmp "repo"))
(define sub (build-path repo "pkg" "web"))
(make-directory* home) (make-directory* sub) (make-directory* (build-path repo ".git"))

(define (kinds segs) (map (lambda (s) (hash-ref s 'kind)) segs))
(define (labels segs) (map (lambda (s) (let ([l (hash-ref s 'label)]) (if (string-prefix? l (path->string tmp)) (substring l (string-length (path->string (simplify-path tmp)))) l))) segs))

(test-case "没有任何文件 → 空；附加节为空串"
  (check-equal? (collect-instructions sub #:home home) '())
  (check-equal? (instructions-addendum '()) ""))

(test-case "git 根 → 工作目录一路上每级一个文件，按路径顺序；全局在最前；catalog 在全局与目录树之间"
  (put! (build-path home "AGENTS.md") "global: answer in Chinese")
  (put! (build-path repo "AGENTS.md") "repo: run make test")
  (put! (build-path repo "pkg" "CLAUDE.md") "pkg: claude-named file")
  (put! (build-path sub "AGENTS.md") "web: use pnpm")
  (define segs (collect-instructions sub #:home home
                                     #:catalog (list (hasheq 'kind "project" 'name "Shop" 'text "project: prices in cents")
                                                     (hasheq 'kind "workspace" 'name "web@mac" 'text "  ")   ; 空白：不算
                                                     (hasheq 'kind "workspace" 'name "repo@mac" 'text "workspace: staging first"))))
  (check-equal? (kinds segs) '("global" "project" "workspace" "dir" "dir" "dir"))
  (check-equal? (labels segs) '("/home/AGENTS.md" "Shop" "repo@mac" "/repo/AGENTS.md" "/repo/pkg/CLAUDE.md" "/repo/pkg/web/AGENTS.md"))
  (define add (instructions-addendum segs))
  (check-true (string-prefix? add "\n\n## Project instructions"))
  (define (pos s) (car (car (regexp-match-positions (regexp-quote s) add))))
  (check-true (< (pos "global: answer") (pos "project: prices") (pos "workspace: staging") (pos "repo: run") (pos "pkg: claude") (pos "web: use pnpm")))
  (check-true (string-contains? add "### project \"Shop\" (settings)"))
  ;; 不含正文的清单
  (check-false (hash-has-key? (car (instructions-summary segs)) 'text)))

(test-case "同一级：AGENTS.override.md 盖过 AGENTS.md；全局同样"
  (put! (build-path sub "AGENTS.override.md") "web override")
  (put! (build-path home "AGENTS.override.md") "global override")
  (define segs (collect-instructions sub #:home home))
  (check-equal? (labels segs) '("/home/AGENTS.override.md" "/repo/AGENTS.md" "/repo/pkg/CLAUDE.md" "/repo/pkg/web/AGENTS.override.md"))
  (delete-file (build-path sub "AGENTS.override.md"))
  (delete-file (build-path home "AGENTS.override.md")))

(test-case "没有 git 根：只看工作目录本身"
  (define lone (build-path tmp "lone" "a"))
  (make-directory* lone)
  (put! (build-path tmp "lone" "AGENTS.md") "parent, not a git root")
  (put! (build-path lone "AGENTS.md") "lone a")
  (check-equal? (labels (collect-instructions lone #:home #f)) '("/lone/a/AGENTS.md"))
  (check-false (git-root lone)))

(test-case "聊天会话（tree? #f）：只有全局与 catalog"
  (check-equal? (kinds (collect-instructions sub #:home home #:tree? #f #:catalog (list (hasheq 'kind "project" 'name "P" 'text "p"))))
                '("global" "project")))

(test-case "上限：超了的那段截断（字符边界），后面的整段略去，都留在清单里"
  (define segs (collect-instructions sub #:home home #:max-bytes 40))
  ;; global 25 字节放得下；repo 19 字节只剩 15 → 截断；后面两段略去
  (check-equal? (map (lambda (s) (list (hash-ref s 'truncated) (hash-ref s 'omitted))) segs) '((#f #f) (#t #f) (#f #t) (#f #t)))
  (check-equal? (hash-ref (cadr segs) 'text) "repo: run make ")
  (check-equal? (hash-ref (cadr segs) 'bytes) 19)
  (define add (instructions-addendum segs #:max-bytes 40))
  (check-true (string-contains? add "[… truncated"))
  (check-false (string-contains? add "pnpm"))
  ;; 多字节字符不截半个
  (put! (build-path home "AGENTS.md") "全局：中文回答")
  (define s2 (collect-instructions sub #:home home #:max-bytes 7))
  (check-equal? (hash-ref (car s2) 'text) "全局")
  (put! (build-path home "AGENTS.md") "global: answer in Chinese"))

(test-case "split-instructions：认 main.rkt 旧格式与本模块格式；没有这一节 → 原样"
  (define base "You are pi.\n\n## Available skills\n- x")
  (define-values (b1 a1) (split-instructions (string-append base (project-instructions-addendum "use tabs" "/r/AGENTS.md"))))
  (check-equal? b1 base)
  (check-true (string-contains? a1 "use tabs"))
  (define-values (b2 a2) (split-instructions (string-append base (instructions-addendum (collect-instructions sub #:home home)))))
  (check-equal? b2 base)
  (define-values (b3 a3) (split-instructions base))
  (check-equal? (list b3 a3) (list base "")))

(test-case "命令行这条路（resources.rkt）：同样分层，多段时正文每段有来源标题"
  (define-values (p b) (find-project-instructions sub #:home home))
  (check-true (string-contains? p " + "))
  (check-true (string-contains? b "### "))
  (check-true (string-contains? b "web: use pnpm")))

(delete-directory/files tmp)
