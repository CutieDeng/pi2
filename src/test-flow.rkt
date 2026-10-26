#lang tstring racket
;; test-flow.rkt — 工作区的测试流程（design-goalmode.md §12，T1）。
;; 用户（2026-10-26）：workspace 上挂一个 test 流程类型，自动执行和验证；人可查看，模型可设置、查看、修改。决定：
;;   1. 随项目共享：仓库里的 .pi/tests.rktd（read，不 eval）
;;   2. 什么时候跑由 AI 决定（tests_run）；验收报告时必须跑（acceptance_report：驱动跑全部步骤，报告由驱动写，模型不能只靠自己说）
;;   3. 先做 T1（格式 + 运行器 + 解析 + 存放 + 接入防篡改与 goal 模式）；模型提议修改 + GUI 面板 = T2
;; 文件形如：
;;   (test-flow
;;     (name "pi2")
;;     (step unit "raco test -j 4 tests" #:parse raco #:timeout 900)
;;     (step quick "raco test tests/compact-test.rkt" #:parse raco)
;;     (protect "tests/**" "run-tests.sh"))
;; 位置：从工作目录往上（到 git 根为止）第一个有 .pi/tests.rktd 的目录 = 项目根；命令在项目根下跑。
;; 解析（结果是结构化的，不只是退出码）：raco（raco test）、unittest（python -m unittest）、pytest、exit（只看退出码，缺省）。
;; 信任：文件跟着仓库来（别人的仓库也可能带一个）→ 某个内容（sha1）第一次跑要用户确认（「始终」= 记住这个内容）；yolo 模式不问；
;;   内容一变就要重新确认。记在 {config-home}/trusted-test-flows.rktd。

(require racket/file racket/string racket/list file/sha1 racket/date
         (file "tool.rkt") (file "model.rkt") (file "permission.rkt") (file "credentials.rkt") (file "shell-util.rkt") (file "diff.rkt"))

(define FLOW-FILE ".pi/tests.rktd")
(define PARSERS '(raco unittest pytest exit))

(struct step (id cmd parse timeout) #:transparent)
(struct flow (root file name steps protect sha) #:transparent)

;; ---------------------------------------------------------------- 找、读
(define (git-root dir)
  (let loop ([d (simplify-path (path->complete-path dir))] [k 0])
    (cond [(> k 64) #f]
          [(or (directory-exists? (build-path d ".git")) (file-exists? (build-path d ".git"))) d]
          [else (let-values ([(up _n _x) (split-path d)]) (if (path? up) (loop up (add1 k)) #f))])))
;; → 项目根（字符串）或 #f
(define (find-flow-root workdir)
  (and workdir (directory-exists? workdir)
       (let ([stop (git-root workdir)])
         (let loop ([d (simplify-path (path->complete-path workdir))] [k 0])
           (cond [(> k 64) #f]
                 [(file-exists? (build-path d FLOW-FILE)) (regexp-replace #rx"(.)/+$" (path->string d) "\\1")]   ; 不带结尾的 /（信任记录按它比）
                 [(and stop (equal? (path->directory-path d) (path->directory-path stop))) #f]
                 [else (let-values ([(up _n _x) (split-path d)]) (if (path? up) (loop up (add1 k)) #f))])))))

;; datum → flow，或 raise（说明哪里不对）
(define (parse-flow datum root file sha)
  (unless (and (pair? datum) (eq? (car datum) 'test-flow)) (error 'test-flow "~a must be (test-flow …)" FLOW-FILE))
  (define name (or (for/first ([c (cdr datum)] #:when (and (pair? c) (eq? (car c) 'name) (pair? (cdr c)) (string? (cadr c)))) (cadr c)) "tests"))
  (define steps
    (for/list ([c (cdr datum)] #:when (and (pair? c) (eq? (car c) 'step)))
      (unless (and (>= (length c) 3) (symbol? (cadr c)) (string? (caddr c)))
        (error 'test-flow "a step must be (step <id> \"<command>\" …): ~s" c))
      (define opts (let loop ([xs (cdddr c)] [h (hasheq)])
                     (cond [(null? xs) h]
                           [(and (keyword? (car xs)) (pair? (cdr xs))) (loop (cddr xs) (hash-set h (string->symbol (keyword->string (car xs))) (cadr xs)))]
                           [else (error 'test-flow "bad step option in ~s" c)])))
      (define p (hash-ref opts 'parse 'exit))
      (unless (memq p PARSERS) (error 'test-flow "unknown #:parse ~a (one of ~a)" p PARSERS))
      (define t (hash-ref opts 'timeout 600))
      (unless (exact-positive-integer? t) (error 'test-flow "#:timeout must be a positive integer (seconds): ~s" c))
      (step (cadr c) (caddr c) p t)))
  (when (null? steps) (error 'test-flow "~a has no steps" FLOW-FILE))
  (unless (equal? (length (remove-duplicates (map step-id steps))) (length steps)) (error 'test-flow "step ids must be unique"))
  (define protect (append* (for/list ([c (cdr datum)] #:when (and (pair? c) (eq? (car c) 'protect))) (filter string? (cdr c)))))
  (flow root file name steps protect sha))

;; → (values flow #f) | (values #f #f)（没有）| (values #f 错误说明)
(define (load-flow workdir)
  (define root (find-flow-root workdir))
  (cond
    [(not root) (values #f #f)]
    [else
     (define f (build-path root FLOW-FILE))
     (with-handlers ([exn:fail? (lambda (e) (values #f (format "~a: ~a" (path->string f) (exn-message e))))])
       (define bs (file->bytes f))
       (define datum (with-input-from-bytes bs (lambda () (parameterize ([read-accept-reader #f] [read-accept-lang #f]) (read)))))
       (values (parse-flow datum root (path->string f) (sha1 (open-input-bytes bs))) #f))]))

;; ---------------------------------------------------------------- 信任
(define (trust-path) (build-path (config-home) "trusted-test-flows.rktd"))
(define (trusted? fl)
  (define p (trust-path))
  (and (file-exists? p)
       (with-handlers ([exn:fail? (lambda (_e) #f)])
         (for/or ([d (in-list (file->list p))]) (and (list? d) (equal? d (list 'trust (flow-root fl) (flow-sha fl))))))))
(define (trust! fl)
  (make-directory* (config-home))
  (with-output-to-file (trust-path) #:exists 'append (lambda () (write (list 'trust (flow-root fl) (flow-sha fl))) (newline))))
;; → #t | 原因字符串。ask：(text detail) → 'yes | 'always | 'no …；#f = 没法问
(define (ensure-trusted fl #:mode [mode #f] #:ask [ask #f])
  (cond
    [(or (eq? mode 'yolo) (trusted? fl)) #t]
    [(not ask) "this workspace's test flow is not trusted yet and there is no one to ask (the user must approve it once)"]
    [else
     (define a (ask (format "Run this workspace's test flow? It comes with the repository (~a); the commands below will run in ~a:\n~a"
                            (flow-file fl) (flow-root fl) (string-join (for/list ([s (flow-steps fl)]) (format "  ~a: $ ~a" (step-id s) (step-cmd s))) "\n"))
                    (hasheq 'file (flow-file fl) 'root (flow-root fl) 'steps (for/list ([s (flow-steps fl)]) (hasheq 'id (symbol->string (step-id s)) 'command (step-cmd s))))))
     (cond [(eq? a 'always) (trust! fl) #t]
           [(eq? a 'yes) #t]
           [else "the user did not approve running this workspace's test flow"])]))

;; ---------------------------------------------------------------- 解析输出（语义化结果）
(define (num rx s) (let ([m (regexp-match rx s)]) (and m (string->number (cadr m)))))
(define (parse-output kind out)
  (case kind
    [(raco)
     (define passed (num #px"(\\d+) tests? passed" out))
     (define fm (regexp-match #px"(\\d+)/(\\d+) test failures?" out))
     (define failures (remove-duplicates (append (regexp-match* #px"location:\\s+(\\S+)" out #:match-select cadr)
                                                 (regexp-match* #px"raco test: \\(file \"([^\"]+)\"\\)\\s*\n[^\n]*raised an exception" out #:match-select cadr))))
     (hasheq 'passed (or passed (and fm (- (string->number (caddr fm)) (string->number (cadr fm))))) 'failed (if fm (string->number (cadr fm)) (and passed 0))
             'total (if fm (string->number (caddr fm)) passed) 'skipped #f 'failures failures)]
    [(unittest)
     (define total (num #px"Ran (\\d+) tests?" out))
     (define f (or (num #px"failures=(\\d+)" out) 0)) (define e (or (num #px"errors=(\\d+)" out) 0)) (define sk (num #px"skipped=(\\d+)" out))
     (hasheq 'total total 'failed (and total (+ f e)) 'skipped sk 'passed (and total (- total f e (or sk 0)))
             'failures (regexp-match* #px"(?m:^(?:FAIL|ERROR): (\\S+ \\([^)]*\\)))" out #:match-select cadr))]
    [(pytest)
     (define summary (let ([m (regexp-match* #px"(?m:^=+ (.*?) in [\\d.]+s.*=+\\s*$)" out #:match-select cadr)]) (if (pair? m) (last m) "")))
     (define (n word) (or (num (pregexp (string-append "(\\d+) " word)) summary) 0))
     (define p (n "passed")) (define fl (+ (n "failed") (n "errors?"))) (define sk (n "skipped"))
     (hasheq 'passed p 'failed fl 'skipped sk 'total (+ p fl sk) 'failures (regexp-match* #px"(?m:^(?:FAILED|ERROR) (\\S+))" out #:match-select cadr))]
    [else (hasheq 'passed #f 'failed #f 'total #f 'skipped #f 'failures '())]))

;; ---------------------------------------------------------------- 跑
;; → (listof hasheq)：'step 'cmd 'exit 'ok 'ms 'passed 'failed 'skipped 'total 'failures 'tail
(define (run-flow fl #:only [ids #f])
  (for/list ([s (in-list (flow-steps fl))] #:when (or (not ids) (memq (step-id s) ids)))
    (define t0 (current-inexact-milliseconds))
    (define-values (code out) (shell-run (step-cmd s) (flow-root fl) #:timeout (step-timeout s)))
    (define parsed (parse-output (step-parse s) out))
    (hash-set* parsed 'step (symbol->string (step-id s)) 'cmd (step-cmd s) 'exit (if (eq? code 'timeout) "timeout" code)
               'ok (eqv? code 0) 'ms (round (- (current-inexact-milliseconds) t0)) 'tail (clip-tail out 2500))))

;; 工作区里改过的（git）被保护文件：流程文件、flow 的 protect、常见测试文件
(define TEST-GLOBS '("tests/**" "test/**" "**/test_*.py" "**/*_test.py" "**/*_test.go" "**/*.test.ts" "**/*.test.js" "**/*-test.rkt" "**/conftest.py"))
(define (changed-protected fl)
  (define-values (code out) (shell-run "git status --porcelain" (flow-root fl) #:timeout 30))
  (define rxs (map glob->regexp (append (list FLOW-FILE) (flow-protect fl) TEST-GLOBS)))
  (if (eqv? code 0)
      (for*/list ([l (in-list (string-split out "\n"))] #:when (> (string-length l) 3)
                  [p (in-value (string-trim (substring l 3)))] #:when (ormap (lambda (rx) (regexp-match? rx p)) rxs))
        (string-append (string-trim (substring l 0 2)) " " p))
      '()))

(define (fmt-counts r)
  (define (n k) (hash-ref r k #f))
  (string-join (filter values (list (and (n 'passed) (format "~a passed" (n 'passed))) (and (n 'failed) (format "~a failed" (n 'failed)))
                                    (and (n 'skipped) (> (n 'skipped) 0) (format "~a skipped" (n 'skipped)))))
               " · "))
(define (format-report fl results #:title title #:claim [claim #f] #:changed [changed '()])
  (define ok? (andmap (lambda (r) (hash-ref r 'ok)) results))
  (string-append
   (format "~a — test flow \"~a\" (~a), run by pi2 just now in ~a\n" title (flow-name fl) FLOW-FILE (flow-root fl))
   (if claim (format "Claimed by the agent: ~a\n" claim) "")
   (apply string-append
          (for/list ([r (in-list results)])
            (define c (fmt-counts r))
            (format "  ~a ~a  $ ~a  (exit ~a, ~a s~a)~a\n" (if (hash-ref r 'ok) "✓" "✗") (hash-ref r 'step) (hash-ref r 'cmd) (hash-ref r 'exit)
                    (/ (round (/ (hash-ref r 'ms) 100.0)) 10.0) (if (string=? c "") "" (string-append "; " c))
                    (let ([fs (hash-ref r 'failures '())]) (if (pair? fs) (format "\n      failing: ~a" (string-join (take fs (min 8 (length fs))) ", ")) "")))))
   (if (pair? changed)
       (format "Test / protected files with uncommitted changes (git status; ~a): ~a~a\n" (length changed)
               (string-join (take changed (min 10 (length changed))) ", ") (if (> (length changed) 10) (format ", … and ~a more" (- (length changed) 10)) ""))
       "")
   (format "Result: ~a\n" (if ok? "PASS" (format "FAIL (~a of ~a steps failed)" (count (lambda (r) (not (hash-ref r 'ok))) results) (length results))))
   (apply string-append
          (for/list ([r (in-list results)] #:unless (hash-ref r 'ok))
            (format "--- ~a (tail) ---\n~a\n" (hash-ref r 'step) (hash-ref r 'tail))))))

;; 一节给模型看的说明（指令收集时加进系统提示词）
(define (flow-instructions fl)
  (string-append
   (format "This workspace has a test flow in ~a (project root ~a). Steps:\n" FLOW-FILE (flow-root fl))
   (string-join (for/list ([s (flow-steps fl)]) (format "  ~a: $ ~a" (step-id s) (step-cmd s))) "\n")
   "\nRun steps with the tests_run tool whenever it helps you (you decide when). Before you tell the user a task is complete, call acceptance_report: "
   "it runs every step and its output is the acceptance report the user sees. Do not edit " FLOW-FILE
   (if (pair? (flow-protect fl)) (format " or the protected files (~a)" (string-join (flow-protect fl) ", ")) "")
   " to make checks pass."))

;; ---------------------------------------------------------------- 历史（T2）：内核数据目录里，按项目根一个文件（不进仓库：结果不是共享的定义）
(define current-test-history-dir (make-parameter #f))    ; rpc 设成 {会话目录}/test-runs；#f = 不记
(define (history-file root) (let ([d (current-test-history-dir)]) (and d (build-path d (string-append (sha1 (open-input-string root)) ".rktd")))))
(define (pad2 n) (~r n #:min-width 2 #:pad-string "0"))
(define (now-iso) (let ([d (seconds->date (current-seconds))]) (format "~a-~a-~a ~a:~a:~a" (date-year d) (pad2 (date-month d)) (pad2 (date-day d)) (pad2 (date-hour d)) (pad2 (date-minute d)) (pad2 (date-second d)))))
(define (step-summary r) (for/hasheq ([k '(step ok exit ms passed failed skipped total failures)]) (values k (let ([v (hash-ref r k #f)]) (if (eq? v #f) (if (eq? k 'ok) #f 'null) v)))))
(define (history-append! fl kind results)
  (define f (history-file (flow-root fl)))
  (when f
    (make-directory* (let-values ([(d _n _x) (split-path f)]) d))
    (with-output-to-file f #:exists 'append
      (lambda () (write (hasheq 'ts (now-iso) 'kind kind 'ok (andmap (lambda (r) (hash-ref r 'ok)) results) 'sha (flow-sha fl)
                                'steps (map step-summary results)))
                 (newline)))))
(define (history-read root #:limit [limit 20])           ; 新的在前
  (define f (history-file root))
  (if (and f (file-exists? f))
      (let ([all (with-handlers ([exn:fail? (lambda (_e) '())]) (filter hash? (file->list f)))]) (take (reverse all) (min limit (length all))))
      '()))

;; ---------------------------------------------------------------- 提议修改（T2）：模型给出新内容 → 记一条提议 → 用户在界面看 diff、批准才写
(define (proposal-root workdir) (or (find-flow-root workdir) (let ([g (git-root workdir)]) (and g (regexp-replace #rx"(.)/+$" (path->string g) "\\1"))) workdir))
(define (file-sha root) (let ([f (build-path root FLOW-FILE)]) (if (file-exists? f) (sha1 (open-input-bytes (file->bytes f))) "")))
;; → (values #t 新 flow) | (values #f 原因)
(define (validate-content content root)
  (with-handlers ([exn:fail? (lambda (e) (values #f (exn-message e)))])
    (define datum (parameterize ([read-accept-reader #f] [read-accept-lang #f]) (with-input-from-string content read)))
    (values #t (parse-flow datum root (path->string (build-path root FLOW-FILE)) (sha1 (open-input-string content))))))
;; 应用（用户批准后）：文件得还是提议时那个内容（不然冲突）→ 写 → 信任新内容（用户刚看过、批准了）
(define (apply-proposal! root content base-sha)
  (define-values (ok? v) (validate-content content root))
  (cond
    [(not ok?) (values #f (format "the proposed flow is invalid: ~a" v))]
    [(not (equal? (file-sha root) base-sha)) (values #f "the test flow changed after this proposal was made; ask for a new proposal")]
    [else
     (make-directory* (build-path root ".pi"))
     (call-with-output-file (build-path root FLOW-FILE) #:exists 'truncate (lambda (o) (write-string content o)))
     (define-values (fl _e) (load-flow root))
     (when fl (trust! fl))
     (values #t fl)]))

;; ---------------------------------------------------------------- 写（先写验收确认后加一步；驱动写，模型不直接改）
(define (append-step! root id cmd #:parse [p 'exit])
  (define f (build-path root FLOW-FILE))
  (define-values (fl0 _e) (load-flow root))
  (define was-trusted? (and fl0 (trusted? fl0)))
  (define datum (if (file-exists? f) (with-input-from-file f read) '(test-flow)))
  (define new (append datum (list (list 'step id cmd '#:parse p))))
  (make-directory* (build-path root ".pi"))
  (call-with-output-file f #:exists 'truncate (lambda (o) (pretty-write new o)))
  (define-values (fl1 _e1) (load-flow root))
  (when (and fl1 (or was-trusted? (not fl0))) (trust! fl1))                    ; 用户刚确认过这条；原来就信任的（或新建的）→ 新内容也信任
  fl1)

;; ---------------------------------------------------------------- 工具
(struct fn-tool (name level spec run)
  #:methods gen:tool
  [(define (tool-name t) (fn-tool-name t))
   (define (tool-permission-level t) (fn-tool-level t))
   (define (tool-spec t) (fn-tool-spec t))
   (define (tool-run t input ctx) ((fn-tool-run t) input ctx))])

(define (with-flow ctx k)
  (define-values (fl err) (load-flow (tool-ctx-workdir ctx)))
  (cond
    [err (err-outcome (format "the test flow file is invalid: ~a" err))]
    [(not fl) (err-outcome (format "this workspace has no test flow (~a); ask the user whether to define one" FLOW-FILE))]
    [else
     (define tr (ensure-trusted fl #:mode (config-permission-mode (tool-ctx-config ctx))
                                #:ask (let ([a (current-asker)])
                                        (and a (lambda (text detail) (parameterize ([current-permission-request (hasheq 'tool "test_flow" 'level "trust" 'input detail)]) (a text)))))))
     (if (eq? tr #t) (k fl) (err-outcome tr))]))

(define (make-test-flow-tools)
  (list
   (fn-tool "tests_run" 'mutating
            (function-spec "tests_run"
                           "Run this workspace's test flow (.pi/tests.rktd) or some of its steps, and get structured results (per step: exit code, passed / failed / skipped counts, failing tests, output tail). Use it whenever checking your work helps."
                           (hasheq 'steps (hasheq 'type "array" 'items (hasheq 'type "string") 'description "Step ids to run (default: all)."))
                           '())
            (lambda (input ctx)
              (with-flow ctx
                (lambda (fl)
                  (define ids (let ([v (input-ref input 'steps #f)]) (and (list? v) (pair? v) (map string->symbol (filter string? v)))))
                  (define unknown (if ids (filter (lambda (i) (not (memq i (map step-id (flow-steps fl))))) ids) '()))
                  (cond
                    [(pair? unknown) (err-outcome (format "unknown step(s): ~a; steps are: ~a" unknown (map step-id (flow-steps fl))))]
                    [else
                     (define rs (run-flow fl #:only ids))
                     (history-append! fl "run" rs)
                     (record! 'tests (hasheq 'kind "run" 'ok (andmap (lambda (r) (hash-ref r 'ok)) rs) 'steps (map (lambda (r) (hash-remove r 'tail)) rs)))
                     (ok-outcome (format-report fl rs #:title "Test run"))])))))
   (fn-tool "acceptance_report" 'mutating
            (function-spec "acceptance_report"
                           "Call this when you believe the task is complete, before telling the user. pi2 runs EVERY step of the workspace's test flow and lists changed test / protected files; that output is the acceptance report the user sees. Do not claim completion without it."
                           (hasheq 'summary (hasheq 'type "string" 'description "What you did and why you believe it is complete (one or two sentences)."))
                           '("summary"))
            (lambda (input ctx)
              (with-flow ctx
                (lambda (fl)
                  (define rs (run-flow fl))
                  (define changed (changed-protected fl))
                  (define ok? (andmap (lambda (r) (hash-ref r 'ok)) rs))
                  (history-append! fl "acceptance" rs)
                  (record! 'tests (hasheq 'kind "acceptance" 'ok ok? 'summary (format "~a" (input-ref input 'summary "")) 'changed changed
                                          'steps (map (lambda (r) (hash-remove r 'tail)) rs)))
                  (ok-outcome (string-append (format-report fl rs #:title "Acceptance report" #:claim (input-ref input 'summary #f) #:changed changed)
                                             (if ok? "" "\nThe acceptance report FAILS: the task is not complete. Fix the failures (do not weaken the tests) and report again.")))))))
   (fn-tool "tests_propose" 'read-only
            (function-spec "tests_propose"
                           "Propose a change to this workspace's test flow (.pi/tests.rktd), or a new one. Give the COMPLETE new file content. Nothing changes now: the user reviews the diff and approves or rejects it. Never use this to make failing checks pass; propose only what the project genuinely needs (a new test suite, a faster subset, a better parser)."
                           (hasheq 'content (hasheq 'type "string" 'description "The complete new content of .pi/tests.rktd: (test-flow (name NAME) (step ID COMMAND #:parse raco|unittest|pytest|exit #:timeout SECONDS) ... (protect GLOB ...)).")
                                   'reason (hasheq 'type "string" 'description "Why the change is needed (one or two sentences)."))
                           '("content" "reason"))
            (lambda (input ctx)
              (define content (let ([v (input-ref input 'content "")]) (if (string? v) v "")))
              (define reason (let ([v (input-ref input 'reason "")]) (if (string? v) (string-trim v) "")))
              (define root (proposal-root (tool-ctx-workdir ctx)))
              (define-values (ok? v) (validate-content content root))
              (cond
                [(not ok?) (err-outcome (format "the proposed flow is invalid: ~a" v))]
                [(string=? reason "") (err-outcome "reason must say why the change is needed")]
                [else
                 (define f (build-path root FLOW-FILE))
                 (define old (if (file-exists? f) (file->string f) ""))
                 (define d (try-file-diff FLOW-FILE old content))
                 (record! 'tests (hasheq 'kind "proposal" 'id (sha1 (open-input-string (string-append root "|" content))) 'root root 'content content
                                         'base_sha (file-sha root) 'reason reason 'diff (or d 'null)))
                 (ok-outcome "Proposed. The user reviews the diff in the Tests panel; it takes effect only after they approve it. Until then the current flow applies.")])))))

(provide FLOW-FILE (struct-out step) (struct-out flow) find-flow-root load-flow parse-flow parse-output run-flow format-report changed-protected
         flow-instructions trusted? trust! ensure-trusted append-step! make-test-flow-tools TEST-GLOBS
         current-test-history-dir history-append! history-read step-summary proposal-root file-sha validate-content apply-proposal!)
