#lang racket
;; commit-watch-test.rkt — bash 里直接 git commit：没带 pi2 标签的新提交 → 工具结果末尾提示（不改提交）。两种 bash 工具都包了。

(require rackunit racket/file
         (file "../src/tool.rkt") (file "../src/tools/bash.rkt") (file "../src/tools/bash-persistent.rkt"))

(define repo (make-temporary-file "pi2-commit-watch-~a" 'directory))
(define ctx (tool-ctx (path->string repo) void #f))
(define bash (make-bash-tool))
(define pbash (make-persistent-bash-tool))
(define (run t cmd) (tool-outcome-content (tool-run t (hasheq 'command cmd) ctx)))
(define GIT "git -c user.email=t@t -c user.name=t")

(void (run bash "git init -q"))

(test-case "first commit in an empty repo, plain message → notice with hash and title"
  (define o (run bash (format "echo a > a.txt && git add -A && ~a commit -q -m 'Add a'" GIT)))
  (check-regexp-match #px"\\[pi2\\] This command created 1 commit\\(s\\) without pi2 automation metadata: [0-9a-f]{7} \"Add a\"" o))

(test-case "no new commit → no notice; tagged commits (datum metadata or trailer) → no notice"
  (check-false (regexp-match? #rx"\\[pi2\\]" (run bash "echo hi")))
  (check-false (regexp-match? #rx"\\[pi2\\]" (run bash (format "echo b >> a.txt && ~a commit -qam '(FIX \"b\" ((pi2 agent)) \"d\")'" GIT))))
  (check-false (regexp-match? #rx"\\[pi2\\]" (run bash (format "echo c >> a.txt && ~a commit -qam 'c' -m 'Pi2-Automation: agent'" GIT)))))

(test-case "several commits in one command: only the untagged ones are listed; the commits themselves are not changed"
  (define o (run bash (format "echo d >> a.txt && ~a commit -qam 'd plain' && echo e >> a.txt && ~a commit -qam '(FEAT \"e\" ((pi2 agent)) \"x\")' && echo f >> a.txt && ~a commit -qam '(FEAT \"f\" () \"x\")'" GIT GIT GIT)))
  (check-regexp-match #rx"created 2 commit\\(s\\)" o)
  (check-regexp-match #rx"\"d plain\"" o)
  (check-regexp-match #rx"\"f\"" o)
  (check-false (regexp-match? #rx"\"e\"" o))
  (check-regexp-match #rx"^\\(FEAT \"f\" \\(\\) \"x\"\\)" (run bash "git log -1 --format=%B")))

(test-case "persistent bash (dsv4-b) is watched too"
  (define o (run pbash (format "echo g >> a.txt && ~a commit -qam 'g'" GIT)))
  (check-regexp-match #rx"\\[pi2\\] This command created 1 commit" o))

(test-case "not a git repo → nothing extra"
  (define d (make-temporary-file "pi2-not-repo-~a" 'directory))
  (check-equal? (tool-outcome-content (tool-run bash (hasheq 'command "echo x") (tool-ctx (path->string d) void #f))) "x\n"))
