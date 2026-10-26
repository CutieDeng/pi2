#lang racket
;; commit-datum-test.rkt — 提交消息 datum 的解析 / 元数据查询 / git_history 工具（临时仓库，离线）。
;;   结构化的是 TYPE 与元数据；标题、说明是自由文字；改动文件取 git 的实际记录。
;;   提交消息是不可信输入：#reader / #lang / 图记号一律当纯文字，不执行、不卡住。

(require rackunit racket/file
         (file "../src/commit-datum.rkt") (file "../src/tool.rkt") (file "../src/tools/git.rkt"))

(define (parse s) (call-with-values (lambda () (parse-commit-message s)) list))

(test-case "shapes: 4 fields; legacy 3 fields = empty metadata; anything else is plain text"
  (check-equal? (parse "(FEAT \"t\"\n\n(breaking (scope gui))\n\"d\"\n)") '(datum FEAT "t" (breaking (scope gui)) "d"))
  (check-equal? (parse "(REFACTOR \"t\"\n\n\"d\"\n)") '(legacy REFACTOR "t" () "d"))
  (check-equal? (parse "Fix the thing\n\nlonger text") '(text #f "Fix the thing" () "longer text"))
  (check-equal? (car (parse "(FEAT \"t\" () \"d\") (extra)")) 'text "two datums → text")
  (check-equal? (car (parse "(FEAT \"t\" () 42)")) 'text "detail must be a string"))

(test-case "untrusted messages: #reader / #lang / graph notation are read as plain text, nothing runs, nothing hangs"
  (define ran (make-temporary-file "pi2-commit-ran-~a"))
  (delete-file ran)
  (for ([m (list (format "#reader(file ~s) x" (path->string ran))
                 "#lang racket\n(FEAT \"t\" () \"d\")"
                 "#0=(FEAT \"t\" #0# \"d\")"
                 "(FEAT \"t\" (#0=(a . #0#)) \"d\")")])
    (check-equal? (car (parse m)) 'text m))
  (check-false (file-exists? ran)))

(test-case "metadata queries: flag, tag, tag with arguments, other datums"
  (define meta '(breaking (scope gui kernel) (CI github) 42))
  (check-true (meta-match? 'breaking meta))
  (check-true (meta-match? 'scope meta) "a bare symbol also finds a tag with that head")
  (check-true (meta-match? '(scope gui) meta))
  (check-true (meta-match? '(scope kernel gui) meta) "argument order does not matter")
  (check-false (meta-match? '(scope web) meta))
  (check-false (meta-match? 'revert meta))
  (check-true (meta-match? 42 meta))
  (check-equal? (map meta-head meta) '(breaking scope CI #f)))

;; ------------------------------------------------------------ git_history 工具
(define repo (make-temporary-file "pi2-git-history-~a" 'directory))
(define (sh . args) (parameterize ([current-directory repo]) (apply system* (find-executable-path "git") "-c" "user.email=t@t" "-c" "user.name=t" args)))
(define (commit! file msg)
  (call-with-output-file (build-path repo file) #:exists 'append (lambda (o) (displayln msg o)))
  (void (sh "add" "-A") (sh "commit" "-q" "-m" msg)))
(void (sh "init" "-q"))
(make-directory* (build-path repo "gui"))
(commit! "gui/a.rkt" "(FEAT \"Add picker\"\n\n(breaking (scope gui kernel))\n\"Detail mentions picker.\"\n)")
(commit! "b.rkt" "(FIX \"Fix crash\"\n\n((CI github))\n\"Automated fix.\"\n)")
(commit! "c.md" "(DOCS \"Old style\"\n\n\"no metadata field\"\n)")
(commit! "d.txt" "plain text commit\n\nbody")

(define tool (make-git-history-tool))
(define ctx (tool-ctx (path->string repo) void #f))
(define (run h) (tool-run tool h ctx))
(define (out h) (tool-outcome-content (run h)))

(test-case "git_history is read-only and lists every commit by default"
  (check-eq? (tool-permission-level tool) 'read-only)
  (define o (out (hasheq)))
  (check-regexp-match #rx"^4 matching commit\\(s\\) of 4 scanned" o)
  (check-regexp-match #rx"\\(text\\) \"plain text commit\"" o)
  (check-regexp-match #rx"DOCS \"Old style\" meta=\\(\\) files=1 \\(3-field message\\)" o))

(test-case "filter by TYPE (case-insensitive) and by metadata"
  (check-regexp-match #rx"^2 matching" (out (hasheq 'type '("feat" "FIX"))))
  (check-regexp-match #rx"^1 matching.*Add picker" (out (hasheq 'meta '("breaking"))))
  (check-regexp-match #rx"^1 matching.*Add picker" (out (hasheq 'meta '("(scope gui)" "breaking"))))
  (check-regexp-match #rx"^0 matching" (out (hasheq 'meta '("(scope web)"))))
  (check-regexp-match #rx"^1 matching.*Fix crash" (out (hasheq 'meta '("CI"))))
  (check-regexp-match #rx"^1 matching.*Add picker" (out (hasheq 'grep "PICKER"))))

(test-case "path, files, detail, stats, limit"
  (define o (out (hasheq 'path "gui" 'files #t 'detail #t)))
  (check-regexp-match #rx"^1 matching" o)
  (check-regexp-match #rx"\n    gui/a.rkt" o)
  (check-regexp-match #rx"\\| Detail mentions picker\\." o)
  (check-regexp-match #rx"types: .*FEAT 1" (out (hasheq 'stats #t)))
  (check-regexp-match #rx"metadata heads: (breaking 1, scope 1, CI 1|.*CI 1.*)" (out (hasheq 'stats #t)))
  (check-regexp-match #rx"^3 matching commit\\(s\\) of 4 scanned, showing 2" (out (hasheq 'limit 2 'type '("FEAT" "FIX" "DOCS")))))

(test-case "bad input: unreadable metadata query; rev that looks like an option (git log --output would write a file)"
  (check-true (tool-outcome-is-error? (run (hasheq 'meta '("(scope gui")))))
  (define r (run (hasheq 'rev "--output=x.txt")))
  (check-true (tool-outcome-is-error? r))
  (check-false (file-exists? (build-path repo "x.txt"))))

;; ------------------------------------------------------------ 自动化提交的元数据（用户 2026-10-26：自动化提交必须写元数据，以区分）
(require (file "../src/model.rkt") (only-in (file "../src/worktree.rkt") automation-commit-message))

(define TAG (automation-tag 'agent #:session "s-1" #:model "m-1"))

(test-case "add-automation-meta: datum → only the metadata text changes (CJK title kept); legacy → metadata inserted; text → trailer; idempotent"
  (define d "(FEAT \"加标签 picker\"\n\n((scope gui))\n\"说明\"\n)")
  (define d2 (add-automation-meta d TAG))
  (check-equal? d2 "(FEAT \"加标签 picker\"\n\n((scope gui) (pi2 agent (session \"s-1\") (model \"m-1\")))\n\"说明\"\n)")
  (check-equal? (add-automation-meta d2 TAG) d2)
  (check-equal? (call-with-values (lambda () (parse-commit-message (add-automation-meta "(DOCS \"old\"\n\n\"d\"\n)" TAG))) list)
                (list 'datum 'DOCS "old" (list TAG) "d"))
  (define t (add-automation-meta "Fix crash\n\nbody\n" TAG))
  (check-equal? t "Fix crash\n\nbody\n\nPi2-Automation: agent; session=s-1; model=m-1")
  (check-equal? (add-automation-meta t TAG) t)
  (check-equal? (call-with-values (lambda () (parse-commit-message t)) list)
                (list 'text #f "Fix crash" (list '(pi2 agent (session "s-1") (model "m-1"))) "body\n\nPi2-Automation: agent; session=s-1; model=m-1")
                "the trailer reads back as the same metadata"))

(test-case "datum-convention?: most recent messages are datums"
  (check-true (datum-convention? (list "(FEAT \"a\" () \"b\")" "(FIX \"c\"\n\n\"d\"\n)" "plain")))
  (check-false (datum-convention? (list "plain" "also plain" "(FEAT \"a\" () \"b\")")))
  (check-false (datum-convention? '())))

(test-case "tag rules: parse, merge (builtin < user < project), labels, changelog sections / exclusion"
  (define proj (parse-commit-meta '(commit-meta (tag breaking #:label "BREAKING" #:changelog (section "Breaking changes"))
                                                (tag CI #:changelog exclude)
                                                (tag wip #:label #f))))
  (check-exn #rx"bad option" (lambda () (parse-commit-meta '(commit-meta (tag x #:color "red")))))
  (check-exn #rx"must be" (lambda () (parse-commit-meta '(tests))))
  (define rules (merge-rules BUILTIN-RULES '() proj))
  (define (c type meta) (commit "abcdef1234" "2026-10-26" type "t" meta "" '() 'datum))
  (check-equal? (commit-labels (c 'FEAT '(breaking (scope gui kernel) wip (pi2 agent (session "s")))) rules)
                '("BREAKING" "scope gui kernel" "pi2 agent"))
  (check-equal? (commit-section (c 'FEAT '((scope gui))) rules) "Features")
  (check-equal? (commit-section (c 'FIX '(breaking)) rules) "Breaking changes")
  (check-equal? (commit-section (c 'FIX '(breaking (CI x))) rules) 'exclude "exclude wins")
  (check-equal? (commit-section (c 'FEAT (list TAG)) rules) "Automated (pi2)" "builtin: automation commits get their own section")
  (check-equal? (commit-section (commit "h" "d" #f "t" '() "" '() 'text) rules) "Other")
  (define md (changelog-markdown (list (c 'FEAT '()) (c 'FIX '(breaking)) (c 'DOCS '((CI y))) (c 'FEAT (list TAG))) rules))
  (check-regexp-match #rx"### Breaking changes\n- t \\(abcdef1\\) \\[BREAKING\\]\n\n### Automated \\(pi2\\)\n- t \\(abcdef1\\) \\[pi2 agent\\]\n\n### Features\n- t \\(abcdef1\\)\n" md)
  (check-regexp-match #rx"1 commit\\(s\\) left out" md))

;; 临时仓库：git 工具提交、goal 的 driver 消息、changelog 读项目规则
(define repo2 (make-temporary-file "pi2-git-auto-~a" 'directory))
(define (sh2 . args) (parameterize ([current-directory repo2]) (with-output-to-string (lambda () (apply system* (find-executable-path "git") "-c" "user.email=t@t" "-c" "user.name=t" args)))))
(void (sh2 "init" "-q"))
(define (last-msg) (sh2 "log" "-1" "--format=%B"))
(define gtool (make-git-tool))
(define gctx (tool-ctx (path->string repo2) void (struct-copy config (default-config) [model "m-1"])))
(define (git! . args)
  (void (sh2 "config" "user.email" "t@t") (sh2 "config" "user.name" "t"))
  (parameterize ([current-session-id "s-1"]) (tool-run gtool (hasheq 'args args) gctx)))
(define (touch! f) (call-with-output-file (build-path repo2 f) #:exists 'append (lambda (o) (displayln "x" o))) (void (sh2 "add" "-A")))

(test-case "git tool commit = automated: the pi2 tag (session, model) goes into the message; -m / -F / -- pathspec / no-message forms"
  (touch! "a.txt")
  (check-false (tool-outcome-is-error? (git! "commit" "-m" "(FEAT \"Add a\"\n\n()\n\"why\"\n)")))
  (check-equal? (string-trim (last-msg)) "(FEAT \"Add a\"\n\n((pi2 agent (session \"s-1\") (model \"m-1\")))\n\"why\"\n)")
  (touch! "b.txt")
  (git! "commit" "-m" "plain subject" "-m" "second paragraph")
  (check-equal? (string-trim (last-msg)) "plain subject\n\nsecond paragraph\n\nPi2-Automation: agent; session=s-1; model=m-1")
  (call-with-output-file (build-path repo2 "msg.txt") (lambda (o) (write-string "(FIX \"From file\" () \"d\")" o)))
  (touch! "c.txt")
  (void (sh2 "reset" "-q" "msg.txt"))
  (git! "commit" "-F" "msg.txt" "--" "c.txt")
  (check-regexp-match #rx"^\\(FIX \"From file\" \\(\\(pi2 agent" (last-msg))
  (check-equal? (string-trim (sh2 "show" "--format=" "--name-only" "HEAD")) "c.txt" "the -- pathspec still applies")
  (define before (last-msg))
  (git! "commit" "--amend" "--no-edit")
  (check-equal? (last-msg) before "no message flag → unchanged"))

(test-case "goal driver / worker / merge messages follow the repo's convention and carry (pi2 kind (session …))"
  (parameterize ([current-session-id "s-2"])
    (check-equal? (automation-commit-message (path->string repo2) 'DOCS "mark task t1 done" 'goal-driver)
                  "(DOCS \"mark task t1 done\"\n\n((pi2 goal-driver (session \"s-2\")))\n\"mark task t1 done\"\n)\n"
                  "repo2 history is mostly datum")
    (define plain (make-temporary-file "pi2-git-plain-~a" 'directory))
    (parameterize ([current-directory plain])
      (void (system* (find-executable-path "git") "init" "-q"))
      (for ([i 3]) (call-with-output-file (format "f~a" i) (lambda (o) (displayln i o)))
        (void (system* (find-executable-path "git") "add" "-A")
              (system* (find-executable-path "git") "-c" "user.email=t@t" "-c" "user.name=t" "commit" "-q" "-m" (format "plain ~a" i)))))
    (check-equal? (automation-commit-message (path->string plain) 'FEAT "merge b" 'goal-merge)
                  "merge b\n\nPi2-Automation: goal-merge; session=s-2\n")))

(test-case "git_history: labels in the list, changelog with the project's .pi/commit-meta.rktd"
  (make-directory* (build-path repo2 ".pi"))
  (call-with-output-file (build-path repo2 ".pi" "commit-meta.rktd")
    (lambda (o) (write-string "(commit-meta (tag CI #:changelog exclude))" o)))
  (touch! "d.txt")
  (void (sh2 "commit" "-q" "-m" "(BUILD \"CI bump\"\n\n((CI github))\n\"d\"\n)"))
  (define h (make-git-history-tool))
  (define (o x) (tool-outcome-content (tool-run h x gctx)))
  (check-regexp-match #rx"FEAT \"Add a\" \\[pi2 agent\\]" (o (hasheq)))
  (define md (o (hasheq 'changelog #t)))
  (check-regexp-match #rx"### Automated \\(pi2\\)\n- From file [^\n]*\n- plain subject [^\n]*\\[pi2 agent\\]\n- Add a" md)
  (check-false (regexp-match? #rx"CI bump" md))
  (check-regexp-match #rx"1 commit\\(s\\) left out" md)
  (call-with-output-file (build-path repo2 ".pi" "commit-meta.rktd") #:exists 'truncate (lambda (o) (write-string "(commit-meta (tag CI #:color 1))" o)))
  (check-regexp-match #rx"^commit-meta rules ignored: .*bad option" (o (hasheq))))
