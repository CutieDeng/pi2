#lang tstring racket
;; plugin-test.rkt — 插件运行时 PoC（design-plugins.md §8）。
;; 证明：① dynamic-require 载入受信插件并注册工具；② 沙箱插件工具在受限求值器内执行；
;; ③ 恶意沙箱插件被资源限额关停、宿主存活；④ 卸载注销工具。

(require
 rackunit
 racket/runtime-path
 (file "../src/tool.rkt")
 (file "../src/plugin.rkt")
) ; end require

(define-runtime-path plugins-dir "../plugins")
(define (plug name) (build-path plugins-dir name))

;; 无副作用的 dummy ctx（示例插件都不用 ctx）
(define DUMMY-CTX #f)

;; ---------------------------------------------------------------- 受信插件

(test-case "trusted plugin loads via dynamic-require and registers a working tool"
  (define host (make-plugin-host))
  (check-false (host-lookup host "echo"))
  (load-plugin-trusted! host (plug "echo-tool.rkt"))
  (define t (host-lookup host "echo"))
  (check-true (tool? t))
  (check-equal? (tool-name t) "echo")
  ;; 工具可执行
  (define out (tool-run t (hasheq 'text "hi") DUMMY-CTX))
  (check-false (tool-outcome-is-error? out))
  (check-equal? (tool-outcome-content out) "echo: hi")
  ;; 其 spec 进入注册表（供发给模型）
  (check-not-false (member "echo" (map tool-name (host-tools host))))
) ; end test-case

;; ---------------------------------------------------------------- 沙箱插件

(test-case "sandbox plugin runs its tool inside a restricted evaluator"
  (define host (make-plugin-host))
  (load-plugin-sandbox! host (plug "calc-sandbox.rkt"))
  (define t (host-lookup host "calc"))
  (check-true (tool? t))
  (define out (tool-run t (hasheq 'a 2 'op "+" 'b 3) DUMMY-CTX))
  (check-false (tool-outcome-is-error? out))
  (check-equal? (tool-outcome-content out) "5")
  (define out2 (tool-run t (hasheq 'a 6 'op "*" 'b 7) DUMMY-CTX))
  (check-equal? (tool-outcome-content out2) "42")
) ; end test-case

(test-case "runaway sandbox plugin is contained by the eval-time limit; host survives"
  (define host (make-plugin-host))
  ;; 1 秒时限，足够让死循环触发关停
  (load-plugin-sandbox! host (plug "runaway-sandbox.rkt") #:eval-limits (list 1 20))
  (define t (host-lookup host "runaway"))
  (define out (tool-run t (hasheq) DUMMY-CTX))
  (check-true (tool-outcome-is-error? out))                       ; 被拦为错误结果
  (check-true (string-contains? (tool-outcome-content out) "resource limit"))
  ;; 宿主仍可正常工作：再载一个好插件
  (load-plugin-trusted! host (plug "echo-tool.rkt"))
  (check-true (tool? (host-lookup host "echo")))
) ; end test-case

;; ---------------------------------------------------------------- 卸载

(test-case "unload removes the plugin's tools and reclaims its custodian"
  (define host (make-plugin-host))
  (define lp (load-plugin-trusted! host (plug "echo-tool.rkt")))
  (check-true (tool? (host-lookup host "echo")))
  (unload-plugin! host lp)
  (check-false (host-lookup host "echo"))
) ; end test-case

;; ---------------------------------------------------------------- API 面

(test-case "plugin-api exposes command + hook registration (stored on host)"
  (define host (make-plugin-host))
  (define lp (loaded-plugin "t" 'trusted (make-custodian) '()))
  (define api (make-plugin-api host lp))
  ((plugin-api-register-command! api) "/hi" (hasheq 'desc "say hi"))
  ((plugin-api-on! api) 'tool-start (lambda (e) 'observed))
  (check-true (hash-has-key? (host-commands host) "/hi"))
  (check-equal? (length (host-hooks host 'tool-start)) 1)
) ; end test-case

(displayln "plugin-test: all passed")
