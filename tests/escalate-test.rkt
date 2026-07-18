#lang tstring racket
;; escalate-test.rkt — 失败驱动的模型升级梯（escalate.rkt + loop.rkt 集成）离线单测。

(require
 rackunit
 racket/async-channel
 racket/file
 racket/pvector
 (file "../src/model.rkt")
 (file "../src/event.rkt")
 (file "../src/provider.rkt")
 (file "../src/tool.rkt")
 (file "../src/permission.rkt")
 (file "../src/plugin.rkt")
 (file "../src/providers.rkt")
 (file "../src/auto.rkt")
 (file "../src/escalate.rkt")
 (file "../src/loop.rkt")
) ; end require

;; ---------------------------------------------------------------- 单元：梯与 climb

(test-case "默认梯：flash/off → pro/high → pro/max"
  (define l (escalation-ladder))
  (check-equal? (length l) 3)
  (check-equal? (cdr (list-ref l 0)) 'off)
  (check-equal? (cdr (list-ref l 1)) 'high)
  (check-equal? (cdr (list-ref l 2)) 'max)
  (check-equal? (car (list-ref l 0)) (light-model))
  (check-equal? (car (list-ref l 1)) (pro-model))
) ; end test-case

(test-case "ladder-rung-of：按 model 名定位起点档（找不到→0）"
  (check-equal? (ladder-rung-of (light-model)) 0)
  (check-equal? (ladder-rung-of (pro-model)) 1)          ; 首个匹配 pro 的档
  (check-equal? (ladder-rung-of "some-other-model") 0)
) ; end test-case

(test-case "escalate-step：逐级climb，顶端封顶"
  (set-reasoning-effort! 'off)
  (define st0 (make-initial-state (struct-copy config (default-config) [model (light-model)])))
  (define-values (st1 r1 d1) (escalate-step st0 #f 0))
  (check-equal? r1 1)
  (check-equal? (car d1) (pro-model))
  (check-equal? (cdr d1) 'high)
  (check-equal? (config-model (agent-state-config st1)) (pro-model))
  (check-equal? (current-reasoning-effort) 'high)         ; 设了 reasoning box
  (define-values (st2 r2 d2) (escalate-step st1 #f r1))
  (check-equal? r2 2)
  (check-equal? (cdr d2) 'max)
  ;; 顶端再climb → #f，不动
  (define-values (st3 r3 d3) (escalate-step st2 #f r2))
  (check-equal? r3 2)
  (check-false d3)
  (set-reasoning-effort! 'off)
) ; end test-case

(test-case "自定义梯可覆盖；escalation-active? gated 到 deepseek"
  (set-escalation-ladder! (list (cons "a" 'low) (cons "b" 'high)))
  (check-equal? (length (escalation-ladder)) 2)
  (set-escalation-ladder! #f)                             ; 复位默认
  ;; gating：需 host 当前 provider base = deepseek 且开关 on
  (define host (make-plugin-host))
  (register-builtin-providers! host)
  (host-set-provider! host "deepseek")
  (check-true (escalation-active? host))
  (host-set-provider! host "lmstudio")
  (check-false (escalation-active? host))
  (host-set-provider! host "deepseek")
  (set-escalate! #f)
  (check-false (escalation-active? host))                 ; 开关 off
  (set-escalate! #t)
) ; end test-case

;; ---------------------------------------------------------------- 集成：run-turn! 真升级

;; 恒失败工具：任何调用都返回 error outcome。
(struct failer-tool ()
  #:methods gen:tool
  [(define (tool-name _t) "failer")
   (define (tool-permission-level _t) 'read-only)
   (define (tool-spec _t) (hasheq))
   (define (tool-run _t _input _ctx) (tool-outcome "always fails" #t #f))]
) ; end struct failer-tool

;; 每次都吐一个对 failer 的工具调用（迫使 loop 反复失败）。
(define (make-failer-provider)
  (provider "esc-mock"
    (lambda (_m _t)
      (define ch (make-async-channel))
      (thread (lambda ()
                (async-channel-put ch
                  (evt:message (now-ms) (message 'assistant (list (tool-use-block "c" "failer" (hasheq))))))
                (async-channel-put ch (evt:turn-end (now-ms) "tool_calls" (usage 1 1)))))
      ch)
    void))

(define tmpdir (make-temporary-file "pi2-esctest-~a" 'directory))

(define (esc-deps host model)
  (define cfg (struct-copy config (default-config)
                           [workdir (path->string tmpdir)]
                           [permission-mode 'yolo]
                           [model model]
                           [turn-max-calls 5]))   ; 小预算，几轮后收尾
  (values (make-deps #:provider (make-failer-provider)
                     #:registry (make-registry (list (failer-tool)))
                     #:bus (make-bus)
                     #:policy (make-policy cfg)
                     #:plugin-host host)
          cfg))

(test-case "run-turn!：deepseek 下反复失败 → 自动升级到 pro/max"
  (set-escalate! #t)
  (set-escalate-threshold! 1)                    ; 每失败一轮就升，测试快
  (set-reasoning-effort! 'off)
  (define host (make-plugin-host))
  (register-builtin-providers! host)
  (host-set-provider! host "deepseek")
  (define-values (d cfg) (esc-deps host (light-model)))
  (define st (run-turn! (make-initial-state cfg) (text-msg 'user "do the thing") d))
  (check-equal? (config-model (agent-state-config st)) (pro-model))   ; 已从 flash 升到 pro
  (check-equal? (current-reasoning-effort) 'max)                       ; 升到梯顶
  (set-escalate-threshold! 2)                    ; 复位默认
  (set-reasoning-effort! 'off)
) ; end test-case

(test-case "run-turn!：非 deepseek（lmstudio）下不升级，模型不变"
  (set-escalate! #t)
  (set-escalate-threshold! 1)
  (define host (make-plugin-host))
  (register-builtin-providers! host)
  (host-set-provider! host "lmstudio")
  (define-values (d cfg) (esc-deps host "gemma-x"))
  (define st (run-turn! (make-initial-state cfg) (text-msg 'user "do the thing") d))
  (check-equal? (config-model (agent-state-config st)) "gemma-x")     ; 未升级
  (set-escalate-threshold! 2)
) ; end test-case

(test-case "run-turn!：escalate off 时即使 deepseek 也不升级"
  (set-escalate! #f)
  (set-escalate-threshold! 1)
  (define host (make-plugin-host))
  (register-builtin-providers! host)
  (host-set-provider! host "deepseek")
  (define-values (d cfg) (esc-deps host (light-model)))
  (define st (run-turn! (make-initial-state cfg) (text-msg 'user "do the thing") d))
  (check-equal? (config-model (agent-state-config st)) (light-model)) ; 关了就不升
  (set-escalate! #t)
  (set-escalate-threshold! 2)
) ; end test-case

(delete-directory/files tmpdir)
