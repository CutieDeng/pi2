#lang tstring racket
;; plugin-test.rkt — 插件运行时（design-plugins.md M1–M3）。
;; 覆盖：受信/沙箱载入、失控隔离、卸载；钩子运行器；经 run-turn! 的端到端
;; （插件工具被模型调用、on-tool-call 拦截/改参、on-tool-result 改结果、before-turn 注入）。

(require
 rackunit
 racket/async-channel
 racket/file
 racket/runtime-path
 (file "../src/model.rkt")
 (file "../src/event.rkt")
 (file "../src/provider.rkt")
 (file "../src/tool.rkt")
 (file "../src/permission.rkt")
 (file "../src/loop.rkt")
 (file "../src/plugin.rkt")
) ; end require

(define-runtime-path plugins-dir "../examples/plugins")
(define (plug name) (build-path plugins-dir name))
(define DUMMY-CTX #f)

;; ---------------------------------------------------------------- 载入 / 隔离 / 卸载

(test-case "trusted plugin loads via dynamic-require and registers a working tool"
  (define host (make-plugin-host))
  (check-false (host-lookup host "echo"))
  (load-plugin-trusted! host (plug "echo-tool.rkt"))
  (define t (host-lookup host "echo"))
  (check-true (tool? t))
  (check-equal? (tool-name t) "echo")
  (define out (tool-run t (hasheq 'text "hi") DUMMY-CTX))
  (check-false (tool-outcome-is-error? out))
  (check-equal? (tool-outcome-content out) "echo: hi")
) ; end test-case

(test-case "sandbox plugin runs its tool inside a restricted evaluator"
  (define host (make-plugin-host))
  (load-plugin-sandbox! host (plug "calc-sandbox.rkt"))
  (define t (host-lookup host "calc"))
  (check-equal? (tool-outcome-content (tool-run t (hasheq 'a 2 'op "+" 'b 3) DUMMY-CTX)) "5")
  (check-equal? (tool-outcome-content (tool-run t (hasheq 'a 6 'op "*" 'b 7) DUMMY-CTX)) "42")
) ; end test-case

(test-case "runaway sandbox plugin is contained by the eval-time limit; host survives"
  (define host (make-plugin-host))
  (load-plugin-sandbox! host (plug "runaway-sandbox.rkt") #:eval-limits (list 1 20))
  (define out (tool-run (host-lookup host "runaway") (hasheq) DUMMY-CTX))
  (check-true (tool-outcome-is-error? out))
  (check-true (string-contains? (tool-outcome-content out) "resource limit"))
  (load-plugin-trusted! host (plug "echo-tool.rkt"))
  (check-true (tool? (host-lookup host "echo")))       ; 宿主存活
) ; end test-case

(test-case "unload removes the plugin's tools"
  (define host (make-plugin-host))
  (define lp (load-plugin-trusted! host (plug "echo-tool.rkt")))
  (check-true (tool? (host-lookup host "echo")))
  (unload-plugin! host lp)
  (check-false (host-lookup host "echo"))
) ; end test-case

(test-case "load-plugins-dir! discovers trusted + sandbox by filename convention"
  (define host (make-plugin-host))
  (load-plugins-dir! host plugins-dir)
  (check-true (tool? (host-lookup host "echo")))       ; echo-tool.rkt → trusted
  (check-true (tool? (host-lookup host "calc")))       ; calc-sandbox.rkt → sandbox
) ; end test-case

;; ---------------------------------------------------------------- 钩子运行器（纯）

(test-case "run-tool-call-hooks: block and replace"
  (define host (make-plugin-host))
  (define api (make-plugin-api host (loaded-plugin "p" 'trusted (make-custodian) '() '())))
  ((plugin-api-on! api) 'tool-call
   (lambda (name input) (if (string=? name "x") (hook-block "no") #f)))
  (define-values (dec inp) (run-tool-call-hooks host "x" (hasheq)))
  (check-equal? dec (cons 'deny "no"))
  ;; replace
  (define host2 (make-plugin-host))
  (define api2 (make-plugin-api host2 (loaded-plugin "p" 'trusted (make-custodian) '() '())))
  ((plugin-api-on! api2) 'tool-call (lambda (name input) (hook-replace (hash-set input 'k 1))))
  (define-values (dec2 inp2) (run-tool-call-hooks host2 "y" (hasheq)))
  (check-equal? dec2 'allow)
  (check-equal? (hash-ref inp2 'k) 1)
) ; end test-case

(test-case "run-context-hooks / run-before-turn-hooks fold correctly"
  (define host (make-plugin-host))
  (define api (make-plugin-api host (loaded-plugin "p" 'trusted (make-custodian) '() '())))
  ((plugin-api-on! api) 'context (lambda (w) (hook-replace (cons 'HEAD w))))
  (check-equal? (run-context-hooks host '(a b)) '(HEAD a b))
  ((plugin-api-on! api) 'before-turn (lambda (st) (text-msg 'user "[inj]")))
  (check-equal? (length (run-before-turn-hooks host 'ignored)) 1)
) ; end test-case

;; ---------------------------------------------------------------- 端到端（run-turn!）

(define tmpdir (make-temporary-file "pi2-plugtest-~a" 'directory))

(define (mock-provider script-box)
  (provider
   "mock"
   (lambda (_msgs _tools)
     (define ch (make-async-channel))
     (define msg (car (unbox script-box)))
     (set-box! script-box (cdr (unbox script-box)))
     (thread
      (lambda ()
        (async-channel-put ch (evt:message (now-ms) msg))
        (async-channel-put ch (evt:turn-end (now-ms)
                                            (if (null? (message-tool-uses msg)) "stop" "tool_calls")
                                            (usage 10 5)))))
     ch)
   void))

(define (host-deps host script)
  (define cfg (struct-copy config (default-config)
                           [workdir (path->string tmpdir)] [permission-mode 'yolo]))
  (values (make-deps #:provider (mock-provider (box script))
                     #:registry (host-registry host)
                     #:bus (make-bus)
                     #:policy (make-policy cfg)
                     #:plugin-host host)
          cfg))

(define (tool-results st)
  (for*/list ([m (in-list (state-history-list st))]
              [b (in-list (message-blocks m))]
              #:when (tool-result-block? b))
    (tool-result-block-content b)))

(define (call-echo text) (message 'assistant (list (tool-use-block "c1" "echo" (hasheq 'text text)))))

(test-case "plugin tool is callable by the model end-to-end"
  (define host (make-plugin-host))
  (load-plugin-trusted! host (plug "echo-tool.rkt"))
  (define-values (d cfg) (host-deps host (list (call-echo "hi") (text-msg 'assistant "done"))))
  (define st (run-turn! (make-initial-state cfg) (text-msg 'user "go") d))
  (check-not-false (member "echo: hi" (tool-results st)))
) ; end test-case

(test-case "on-tool-call hook blocks a plugin/tool call"
  (define host (make-plugin-host))
  (load-plugin-trusted! host (plug "echo-tool.rkt"))
  (define api (make-plugin-api host (loaded-plugin "guard" 'trusted (make-custodian) '() '())))
  ((plugin-api-on! api) 'tool-call
   (lambda (name input) (if (string=? name "echo") (hook-block "not allowed here") #f)))
  (define-values (d cfg) (host-deps host (list (call-echo "hi") (text-msg 'assistant "ok"))))
  (define st (run-turn! (make-initial-state cfg) (text-msg 'user "go") d))
  (check-true (ormap (lambda (c) (string-contains? c "blocked by plugin: not allowed here"))
                     (tool-results st)))
) ; end test-case

(test-case "on-tool-call mutates input; on-tool-result rewrites the outcome"
  (define host (make-plugin-host))
  (load-plugin-trusted! host (plug "echo-tool.rkt"))
  (define api (make-plugin-api host (loaded-plugin "x" 'trusted (make-custodian) '() '())))
  ((plugin-api-on! api) 'tool-call (lambda (name input) (hook-replace (hash-set input 'text "MUT"))))
  ((plugin-api-on! api) 'tool-result
   (lambda (name input outcome) (hook-replace (ok-outcome (string-upcase (tool-outcome-content outcome))))))
  (define-values (d cfg) (host-deps host (list (call-echo "hi") (text-msg 'assistant "ok"))))
  (define st (run-turn! (make-initial-state cfg) (text-msg 'user "go") d))
  (check-not-false (member "ECHO: MUT" (tool-results st)))   ; 改参→"echo: MUT"→改结果→大写
) ; end test-case

(test-case "before-turn hook injects a message into the turn"
  (define host (make-plugin-host))
  (define api (make-plugin-api host (loaded-plugin "inj" 'trusted (make-custodian) '() '())))
  ((plugin-api-on! api) 'before-turn (lambda (st) (text-msg 'user "[injected-ctx]")))
  (define-values (d cfg) (host-deps host (list (text-msg 'assistant "hi"))))
  (define st (run-turn! (make-initial-state cfg) (text-msg 'user "go") d))
  (check-not-false (member "[injected-ctx]" (map message-text (state-history-list st))))
) ; end test-case

(delete-directory/files tmpdir)
(displayln "plugin-test: all passed")
