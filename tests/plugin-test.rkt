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

;; ---------------------------------------------------------------- 能力授权（M3+）

(test-case "read-plugin-caps reads the sidecar .rktd"
  (check-equal? (read-plugin-caps (plug "writer-sandbox.rkt")) '(fs-write))
  (check-equal? (read-plugin-caps (plug "echo-tool.rkt")) '())    ; 无旁置清单
) ; end test-case

(test-case "sandbox fs-write is denied by default, allowed when the capability is granted"
  (define wdir (make-temporary-file "pi2-caps-~a" 'directory))
  (parameterize ([current-directory wdir])
    (define target (path->string (build-path wdir "out.txt")))
    ;; 未授予 fs-write：沙箱拒写
    (define h1 (make-plugin-host))
    (load-plugin-sandbox! h1 (plug "writer-sandbox.rkt"))          ; #:caps '()
    (define o1 (tool-run (host-lookup h1 "writer") (hasheq 'path target 'text "x") DUMMY-CTX))
    (check-true (tool-outcome-is-error? o1))
    (check-false (file-exists? target))
    ;; 授予 fs-write：可写
    (define h2 (make-plugin-host))
    (load-plugin-sandbox! h2 (plug "writer-sandbox.rkt") #:caps '(fs-write))
    (define o2 (tool-run (host-lookup h2 "writer") (hasheq 'path target 'text "hi") DUMMY-CTX))
    (check-false (tool-outcome-is-error? o2))
    (check-true (file-exists? target)))
  (delete-directory/files wdir)
) ; end test-case

(test-case "grants persist (always) and reload from the .rktd store"
  (define store (make-temporary-file "pi2-grants-~a.rktd"))
  (delete-file store)
  (define g (make-grants store))
  (check-false (grants-has? g "p" 'trust))
  (grants-add! g "p" 'trust)                        ; 持久化
  (grants-add! g "p" 'network #:persist? #f)        ; 仅本次
  (define g2 (make-grants store))                   ; 重载
  (check-true (grants-has? g2 "p" 'trust))          ; always 项恢复
  (check-false (grants-has? g2 "p" 'network))       ; 一次性项不恢复
  (delete-file store)
) ; end test-case

(test-case "trust gate: deny → not loaded; always → loaded and persisted"
  (define host (make-plugin-host))
  (define store (make-temporary-file "pi2-grants-~a.rktd"))
  (delete-file store)
  (define g (make-grants store))
  (check-false (gated-load-trusted! host (plug "echo-tool.rkt") g (lambda (_q) 'no)))
  (check-false (host-lookup host "echo"))           ; 拒绝 → 未加载
  (gated-load-trusted! host (plug "echo-tool.rkt") g (lambda (_q) 'always))
  (check-true (tool? (host-lookup host "echo")))
  (check-true (grants-has? (make-grants store) "echo-tool" 'trust))   ; 已持久化
  (delete-file store)
) ; end test-case

(test-case "gated-load-sandbox! prompts per declared capability"
  (define asked (box '()))
  (define host (make-plugin-host))
  (gated-load-sandbox! host (plug "writer-sandbox.rkt") (make-grants)
                       (lambda (q) (set-box! asked (cons q (unbox asked))) 'no))
  (check-equal? (length (unbox asked)) 1)           ; 声明 1 项能力 → 询问 1 次
  (check-true (tool? (host-lookup host "writer")))   ; 仍加载（能力被拒→沙箱限制）
) ; end test-case

;; ---------------------------------------------------------------- 多供应商（M4）

(test-case "plugin registers an LLM provider, selectable at startup and usable in a turn"
  (define host (make-plugin-host))
  (load-plugin-trusted! host (plug "echo-provider.rkt"))
  (define factory (host-provider host "echollm"))
  (check-true (procedure? factory))
  (check-not-false (member "echollm" (host-provider-names host)))
  ;; 用该 provider 跑一轮：assistant 回复来自插件供应商（非内置 openai，无需联网）。
  (define cfg (struct-copy config (default-config)
                           [workdir (path->string tmpdir)] [permission-mode 'yolo]))
  (define prov (factory cfg))
  (define d (make-deps #:provider prov #:registry (host-registry host) #:bus (make-bus)
                       #:policy (make-policy cfg) #:plugin-host host))
  (define st (run-turn! (make-initial-state cfg) (text-msg 'user "ping") d))
  (check-not-false (member "echo-llm reply: ping" (map message-text (state-history-list st))))
) ; end test-case

(test-case "dispatch provider switches the active provider at runtime (/provider)"
  (define host (make-plugin-host))
  (load-plugin-trusted! host (plug "echo-provider.rkt"))       ; 注册 echollm
  (check-not-false (member "echollm" (host-available-providers host)))
  (check-equal? (host-current-provider host) "openai")         ; 默认
  (check-false (host-set-provider! host "nope"))               ; 未知名 → 不改
  (check-equal? (host-current-provider host) "openai")
  (check-true (host-set-provider! host "echollm"))             ; 切换
  (check-equal? (host-current-provider host) "echollm")
  ;; 分发器按当前选用委派：跑一轮 → echollm 回复（离线）
  (define cfg (struct-copy config (default-config)
                           [workdir (path->string tmpdir)] [permission-mode 'yolo]))
  (define disp (make-dispatch-provider host cfg))
  (define d (make-deps #:provider disp #:registry (host-registry host) #:bus (make-bus)
                       #:policy (make-policy cfg) #:plugin-host host))
  (define st (run-turn! (make-initial-state cfg) (text-msg 'user "ping") d))
  (check-not-false (member "echo-llm reply: ping" (map message-text (state-history-list st))))
) ; end test-case

(test-case "register-shortcut! stores a kev handler; host-shortcut looks it up"
  (define host (make-plugin-host))
  (define api (make-plugin-api host (loaded-plugin "s" 'trusted (make-custodian) '() '())))
  (define ran (box #f))
  ((plugin-api-register-shortcut! api) (kchar #\g '(ctrl)) (lambda (_ctx) (set-box! ran #t)))
  (define h (host-shortcut host (kchar #\g '(ctrl))))
  (check-true (procedure? h))
  (h (make-ctx host))
  (check-true (unbox ran))
  (check-false (host-shortcut host (kchar #\x '(ctrl))))   ; 未注册键 → #f
) ; end test-case

(test-case "ctx.select / ctx.confirm route through host-injected UI"
  (define host (make-plugin-host))
  (host-set-select! host (lambda (title opts) (car opts)))     ; 模拟：总选第一项
  (host-set-confirm! host (lambda (title) #t))
  (define ctx (make-ctx host))
  (check-equal? ((plugin-ctx-select ctx) "pick" '("a" "b")) "a")
  (check-true ((plugin-ctx-confirm ctx) "ok?"))
) ; end test-case

(delete-directory/files tmpdir)
(displayln "plugin-test: all passed")
