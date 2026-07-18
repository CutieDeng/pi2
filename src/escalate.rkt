#lang tstring racket
;; escalate.rkt — 失败驱动的模型升级梯（自适应：弱模型在一轮里反复失败 → 自动换更强的）。
;;
;; 与 auto.rkt 互补，构成完整的自适应闭环：
;;   * auto.rkt   在「turn 之前」按启发式挑**起点档位**（短任务 flash / 难任务 pro）。
;;   * escalate.rkt 在「turn 之内」按**连续失败**信号沿梯 climb（flash → pro/high → pro/max）。
;; 二者都非侵入：只改 config-model + 设 reasoning box，内核 loop/provider 不动。
;; 默认梯用 deepseek 模型，故 gated 到 deepseek base（非 deepseek 时不生效，避免误切供应商）。
;; 模型名复用 auto.rkt 的 light-model/pro-model（可 env PI_AUTO_* / auto.rktd 覆盖）。

(require
 racket/list
 (file "model.rkt")                     ; config / struct-copy / set-reasoning-effort!
 (file "plugin.rkt")                    ; host-current-provider / provider-base-name
 (file "auto.rkt")                      ; light-model / pro-model
) ; end require

;; ---------------------------------------------------------------- 开关 / 阈值（进程级 box）

(define escalate-on-box (box #t))       ; 默认开（gated 到 deepseek 才实际生效）
(define (escalate-on?) (unbox escalate-on-box))
(define (set-escalate! on?) (set-box! escalate-on-box (and on? #t)))

;; 连续「失败轮」达到阈值即 climb 一级（默认 2：给弱模型两轮机会,再升）。
(define escalate-threshold-box (box 2))
(define (escalate-threshold) (unbox escalate-threshold-box))
(define (set-escalate-threshold! n)
  (when (exact-positive-integer? n) (set-box! escalate-threshold-box n)))

;; ---------------------------------------------------------------- 升级梯

;; cheap→strong 的 (model . reasoning) 档位。默认 flash/off → pro/high → pro/max
;; （先加思考再加成本：中间档只提 thinking，仍是 pro）。
(define ladder-override-box (box #f))
(define (escalation-ladder)
  (or (unbox ladder-override-box)
      (list (cons (light-model) 'off)
            (cons (pro-model)    'high)
            (cons (pro-model)    'max))))
(define (set-escalation-ladder! rungs) (set-box! ladder-override-box rungs))

;; 当前 model 在梯上的位置（首个 car=model 的档；找不到 → 0，即从底climb）。
(define (ladder-rung-of model)
  (define l (escalation-ladder))
  (or (for/first ([r (in-list l)] [i (in-naturals)] #:when (equal? (car r) model)) i) 0))

;; ---------------------------------------------------------------- 接入点

;; 是否应对当前 provider 生效：有 host、开关 on 且 base=deepseek。
(define (escalation-active? host)
  (and host
       (escalate-on?)
       (string=? (provider-base-name (host-current-provider host)) "deepseek")))

;; 从 rung climb 一级。已在顶端 → (values st rung #f)（无法再升）；否则改 config-model + 设
;; reasoning box，返回 (values st* new-rung (cons model reasoning))。
(define (escalate-step st host rung)
  (define l (escalation-ladder))
  (cond
    [(>= (add1 rung) (length l)) (values st rung #f)]
    [else
     (define next (list-ref l (add1 rung)))
     (define model (car next))
     (define eff (cdr next))
     (set-reasoning-effort! eff)
     (values (struct-copy agent-state st
                          [config (struct-copy config (agent-state-config st) [model model])])
             (add1 rung)
             (cons model eff))]
  ) ; end cond
) ; end define escalate-step

(provide
 escalate-on? set-escalate!
 escalate-threshold set-escalate-threshold!
 escalation-ladder set-escalation-ladder! ladder-rung-of
 escalation-active? escalate-step)
