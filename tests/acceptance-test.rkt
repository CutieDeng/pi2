#lang tstring racket
;; acceptance-test.rkt — Goal 模式的验收加固（acceptance.rkt；用户 2026-10-26 a + b + c）：
;;   a. 被保护文件改了 / 删了 → 恢复、告诉模型；验收在恢复后的状态上跑；构建产物 / 没跟踪的文件不保护
;;   b. 独立审查：复现了的问题否决「完成」并进验收；复现不了的、太空的不算；审查命令只在副本里跑
;;   c. 先写验收：红 → 确认 → 冻结 → 开工；没确认 / 一开始就过 → 不开工

(require rackunit racket/async-channel racket/file racket/string racket/list racket/system racket/port
         (file "../src/model.rkt") (file "../src/event.rkt") (file "../src/provider.rkt") (file "../src/tool.rkt")
         (file "../src/permission.rkt") (file "../src/loop.rkt") (file "../src/session.rkt") (file "../src/plugin.rkt")
         (file "../src/goal.rkt") (file "../src/acceptance.rkt"))

(define (put! dir rel s) (define p (build-path dir rel)) (make-parent-directory* p) (call-with-output-file p (lambda (o) (write-string s o)) #:exists 'truncate))
(define (git! dir . args) (parameterize ([current-output-port (open-output-nowhere)] [current-error-port (open-output-nowhere)])
                            (apply system* (find-executable-path "git") "-C" (path->string dir) "-c" "user.email=t@t" "-c" "user.name=t" args)))

(test-case "glob → regexp"
  (check-true (regexp-match? (glob->regexp "tests/**") "tests/a/b.py"))
  (check-true (regexp-match? (glob->regexp "**/test_*.py") "test_x.py"))
  (check-true (regexp-match? (glob->regexp "**/test_*.py") "a/b/test_x.py"))
  (check-false (regexp-match? (glob->regexp "**/test_*.py") "a/xtest_x.py"))
  (check-false (regexp-match? (glob->regexp "*.py") "a/b.py")))

(test-case "a. guard: test files + tracked files named in the command are protected; build outputs and sources are not; changes are restored"
  (define dir (make-temporary-file "pi2-acc-~a" 'directory))
  (put! dir "tests/test_a.py" "assert real()") (put! dir "src/a.py" "def real(): return 1") (put! dir "check.sh" "python3 -m unittest") (put! dir "out/app" "binary")
  (git! dir "init" "-q") (git! dir "add" "tests" "src" "check.sh") (git! dir "commit" "-q" "-m" "x")
  (define g (make-guard (path->string dir) (list "./check.sh && ./out/app --check")))
  (check-equal? (guard-paths g) '("check.sh" "tests/test_a.py"))
  (put! dir "tests/test_a.py" "assert True")
  (delete-file (build-path dir "check.sh"))
  (put! dir "src/a.py" "changed")                                                  ; 源码：随便改
  (check-equal? (sort (guard-check+restore! g) string<? #:key car) '(("check.sh" . deleted) ("tests/test_a.py" . modified)))
  (check-equal? (file->string (build-path dir "tests/test_a.py")) "assert real()")
  (check-equal? (file->string (build-path dir "check.sh")) "python3 -m unittest")
  (check-equal? (file->string (build-path dir "src/a.py")) "changed")
  (check-equal? (guard-check+restore! g) '())
  (check-equal? (guard-paths (make-guard (path->string dir) '("./check.sh") #:protect '())) '())   ; 关掉
  (delete-directory/files dir))

(test-case "b. review checks run only in a scratch copy; trivial checks are not evidence"
  (define dir (make-temporary-file "pi2-acc-~a" 'directory))
  (put! dir "a.txt" "x")
  (define rs (run-checks-in-copy (list "touch SHOULD_NOT_EXIST; test -f a.txt && false") (path->string dir)))
  (check-equal? (cadr (car rs)) 1)
  (check-false (file-exists? (build-path dir "SHOULD_NOT_EXIST")))
  (check-equal? (map (lambda (f) (hash-ref f 'check))
                     (parse-findings "{\"findings\": [{\"problem\": \"p\", \"check\": \"false\"}, {\"problem\": \"q\", \"check\": \"exit 1\"}, {\"problem\": \"r\", \"check\": \"grep -q X a.txt\"}]}"))
                '("grep -q X a.txt"))
  (delete-directory/files dir))

;; ---------------------------------------------------------------- 经 run-goal!（脚本模型：干活的直接写文件；审查 / 出题按系统提示分）
(define (scripted #:worker worker #:reviewer [reviewer (lambda () "{\"verdict\": \"done\", \"findings\": []}")] #:spec [spec (lambda () "{}")])
  (define prompts (box '()))
  (values
   (provider "s" (lambda (msgs _t)
                   (define sys (or (config-system-prompt (current-config)) ""))
                   (define ch (make-async-channel))
                   (define text
                     (cond [(string-prefix? sys "You are an independent reviewer") (reviewer)]
                           [(string-prefix? sys "You write acceptance tests") (spec)]
                           [else (set-box! prompts (cons (message-text (last msgs)) (unbox prompts))) (worker (length (unbox prompts)))]))
                   (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant text)))
                   (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 1 1)))
                   ch) void)
   prompts))
(define (run-it dir prov until #:approve [approve #f] #:max-turns [mt 6])
  (define cfg (struct-copy config (default-config) [workdir (path->string dir)] [permission-mode 'yolo]))
  (define host (make-plugin-host))
  (define d (make-deps #:provider prov #:registry (make-registry '()) #:bus (make-bus) #:policy (make-policy cfg) #:plugin-host host))
  (define sess (session-open! (build-path dir "s.rktd") cfg))
  (define log (box '()))
  (run-goal! d (make-initial-state cfg) sess "make the feature" until mt host #:emit (lambda (s) (set-box! log (cons s (unbox log)))) #:approve approve)
  (session-close! sess)
  (string-join (reverse (unbox log)) "\n"))

(test-case "a via run-goal!: the model rewrites the test to pass → restored, told, not done; then really implements → done"
  (define dir (make-temporary-file "pi2-acc-~a" 'directory))
  (put! dir "tests/check.sh" "grep -q DONE src/feat.txt")
  (define-values (prov prompts)
    (scripted #:worker (lambda (n)
                         (case n
                           [(1) (put! dir "tests/check.sh" "exit 0")]                ; 篡改
                           [else (put! dir "src/feat.txt" "DONE")])
                         "working")))
  (define log (run-it dir prov (list "sh tests/check.sh")))
  (check-regexp-match #rx"guard: restored 1 protected file" log)
  (check-regexp-match #rx"DONE ✓" log)
  (check-equal? (file->string (build-path dir "tests/check.sh")) "grep -q DONE src/feat.txt")
  (check-regexp-match #rx"PROTECTED acceptance files[^\n]*\n  tests/check.sh" (car (reverse (unbox prompts))))   ; 一开始就告诉
  (check-regexp-match #rx"have been RESTORED" (cadr (reverse (unbox prompts))))
  (delete-directory/files dir))

(test-case "b via run-goal!: acceptance passes but the review reproduces a problem → not done, the check joins the acceptance; fixed → reviewed again → done"
  (define dir (make-temporary-file "pi2-acc-~a" 'directory))
  (define reviews (box 0))
  (define-values (prov prompts)
    (scripted #:worker (lambda (n) (if (= n 1) (put! dir "src/feat.txt" "half") (put! dir "src/feat.txt" "COMPLETE")) "working")
              #:reviewer (lambda ()
                           (set-box! reviews (add1 (unbox reviews)))
                           (if (= (unbox reviews) 1)
                               (string-append "```json\n{\"verdict\": \"not_done\", \"findings\": ["
                                              "{\"problem\": \"feature only half done\", \"check\": \"grep -q COMPLETE src/feat.txt\"},"
                                              "{\"problem\": \"empty claim\", \"check\": \"false\"},"
                                              "{\"problem\": \"wrong claim\", \"check\": \"test -f src/feat.txt\"}]}\n```")
                               "{\"verdict\": \"done\", \"findings\": []}"))))
  (define log (run-it dir prov (list "test -f src/feat.txt")))
  (check-regexp-match #rx"REPRODUCED: feature only half done" log)
  (check-regexp-match #rx"not reproduced \\(ignored\\): wrong claim" log)
  (check-false (regexp-match? #rx"empty claim" log))
  (check-regexp-match #rx"independent review found nothing reproducible" log)
  (check-equal? (unbox reviews) 2)
  (check-regexp-match #rx"independent review found problems[^\n]*\n  - feature only half done\n    \\$ grep -q COMPLETE src/feat.txt" (cadr (reverse (unbox prompts))))
  (delete-directory/files dir))

(test-case "c via run-goal!: no acceptance → the test writer writes failing tests → approved → frozen → work → done; the worker cannot edit them"
  (define dir (make-temporary-file "pi2-acc-~a" 'directory))
  (define asked (box #f))
  (define-values (prov prompts)
    (scripted #:worker (lambda (n)
                         (when (= n 1) (put! dir "acceptance/test_goal.sh" "exit 0"))   ; 想改冻结的测试
                         (put! dir "app.txt" "READY") "working")
              #:spec (lambda () "{\"files\": [{\"path\": \"acceptance/test_goal.sh\", \"content\": \"grep -q READY app.txt\"}], \"command\": \"sh acceptance/test_goal.sh\"}")))
  (define log (run-it dir prov '() #:approve (lambda (text detail) (set-box! asked detail) 'yes)))
  (check-equal? (hash-ref (unbox asked) 'files) '("acceptance/test_goal.sh"))
  (check-regexp-match #rx"acceptance tests written \\(they fail now" log)
  (check-regexp-match #rx"guard: restored 1 protected file" log)
  (check-regexp-match #rx"DONE ✓" log)
  (check-equal? (file->string (build-path dir "acceptance/test_goal.sh")) "grep -q READY app.txt")
  (delete-directory/files dir))

(test-case "c: not approved → files removed, not started; tests that already pass → rejected"
  (define dir (make-temporary-file "pi2-acc-~a" 'directory))
  (define-values (prov _p)
    (scripted #:worker (lambda (n) "working")
              #:spec (lambda () "{\"files\": [{\"path\": \"acceptance/t.sh\", \"content\": \"grep -q READY app.txt\"}], \"command\": \"sh acceptance/t.sh\"}")))
  (define log (run-it dir prov '() #:approve (lambda (t d) 'no)))
  (check-regexp-match #rx"not approved" log)
  (check-regexp-match #rx"NOT STARTED" log)
  (check-false (file-exists? (build-path dir "acceptance/t.sh")))
  (put! dir "app.txt" "READY")
  (define log2 (run-it dir prov '() #:approve (lambda (t d) 'yes)))
  (check-regexp-match #rx"already pass before any work" log2)
  (delete-directory/files dir))

(test-case "c via run-rpc!: goal {tests_first} → a permission_request (tool acceptance_tests) shows the files and command → yes → done"
  (local-require json (file "../src/rpc.rkt"))
  (define dir (make-temporary-file "pi2-acc-~a" 'directory))
  (void (putenv "PI_CONFIG_HOME" (path->string (build-path dir "cfg"))))
  (define-values (prov _p)
    (scripted #:worker (lambda (n) (put! dir "app.txt" "READY") "working")
              #:spec (lambda () "{\"files\": [{\"path\": \"acceptance/t.sh\", \"content\": \"grep -q READY app.txt\"}], \"command\": \"sh acceptance/t.sh\"}")))
  (define cfg (struct-copy config (default-config) [workdir (path->string dir)] [permission-mode 'yolo]))
  (define sess (session-open! (fresh-session-path (path->string dir)) cfg))
  (define host (make-plugin-host))
  (define d (make-deps #:provider prov #:registry (make-registry '()) #:bus (make-bus) #:policy (make-policy cfg) #:plugin-host host))
  (define-values (in-r in-w) (make-pipe)) (define-values (out-r out-w) (make-pipe))
  (thread (lambda () (parameterize ([current-input-port in-r] [current-output-port out-w])
                       (run-rpc! d (make-initial-state cfg) sess #:plugin-host host #:fetch-models (lambda (_e) #f)))
            (close-output-port out-w)))
  (define (send! j) (write-string (jsexpr->string j) in-w) (newline in-w) (flush-output in-w))
  (define (read-until pred)
    (let loop () (define l (sync/timeout 20 (read-line-evt out-r 'linefeed)))
      (unless (string? l) (error 'read-until "timeout"))
      (define j (string->jsexpr l)) (if (pred j) j (loop))))
  (read-until (lambda (j) (equal? (hash-ref j 'type #f) "session")))
  (send! (hasheq 'type "goal" 'goal "make app ready" 'until '() 'tests_first #t 'max_turns 4))
  (define pr (read-until (lambda (j) (equal? (hash-ref j 'type #f) "permission_request"))))
  (check-equal? (hash-ref pr 'tool) "acceptance_tests")
  (check-equal? (hash-ref (hash-ref pr 'input) 'files) '("acceptance/t.sh"))
  (check-equal? (hash-ref (hash-ref pr 'input) 'command) "sh acceptance/t.sh")
  (send! (hasheq 'type "permission" 'id (hash-ref pr 'id) 'decision "yes"))
  (define end (read-until (lambda (j) (and (equal? (hash-ref j 'type #f) "goal_status") (regexp-match? #rx"goal ended" (hash-ref j 'text ""))))))
  (check-regexp-match #rx"DONE" (hash-ref end 'text))
  (send! (hasheq 'type "shutdown"))
  (void (read-until (lambda (j) (equal? (hash-ref j 'type #f) "bye"))))
  (delete-directory/files dir))
