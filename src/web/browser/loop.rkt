#lang racket/base
;; browser/loop.rkt — L9 · 事件循环（design-chrome.md L9 / §2.4）
;; 职责: run-until-quiet!——微任务清空 → 网络完成回调 → 定时器虚拟快进(仅当网络静默,
;;       L2.1 前置条件) → 安定判定。消费 L2 taskq/vclock、L3 变更计数。
;; 安定语义(设计 L9 的字面读法): 微/net 队列空 ∧ 网络静默 ∧ 无 vtime 预算内宏任务
;;       → 把"DOM 200ms(虚拟)无变更"窗口补满(队列已空,计数必不变) → 'quiet。
;;       vtime 触顶(自旋 interval/rAF 类页面)→ 'budget-hit(DOM 已尽力,L10 带降级结果)。
;;       超出 vtime 预算的远期定时器(长轮询)直接放弃,不算触顶。
;; 任务失败: 脚本级隔离——记 on-task-error 继续;budget 异常穿透为 'budget-hit。
;; 不做: 事件绑定/DOMContentLoaded 派发(L8/L10)、请求发起(L8)
;; 依赖: racket/base browser/cell/{budget,cell} browser/dom/mutation

(require racket/contract
         "cell/budget.rkt" "cell/cell.rkt"
         "dom/mutation.rkt")

(provide
 (contract-out
  [run-until-quiet! (->* (cell? ddoc?)
                         (#:on-task-error (-> exn? any)
                          #:net-timeout-ms exact-positive-integer?)
                         (or/c 'quiet 'budget-hit))]
 ) ; end contract-out
) ; end provide

(define QUIET-WINDOW-MS 200)   ; 安定窗口(虚拟)
(define NET-POLL-S 0.005)      ; 等网络的真实轮询片
(define FAR-HORIZON-MS 1000)   ; 截断判别视界: 被放弃的定时器距 now 在此内 → 是被
                               ; 预算截断的活跃页(budget-hit),否则是长轮询(quiet)

(define (run-until-quiet! c doc
                          #:on-task-error [on-err void]
                          #:net-timeout-ms [net-timeout-ms 10000])
  (define b (cell-budget c))
  (define vc (cell-vclock c))
  (define cap (budget-vtime-ms b))
  (define mut-snap (ddoc-mutations doc))
  (define last-mut-vt (vclock-now vc))
  (define net-deadline (+ (current-inexact-milliseconds) net-timeout-ms))
  (define net-gone? #f)   ; 网络等待超时 → 此后视为静默(挂死的请求不再拦安定)

  (define (observe!)   ; 变更计数 → 最近变更的虚拟时刻
    (define m (ddoc-mutations doc))
    (unless (= m mut-snap)
      (set! mut-snap m)
      (set! last-mut-vt (vclock-now vc))
    ) ; end unless
  ) ; end define observe!

  (define (run-task! th)
    (with-handlers ([exn:fail:budget? raise]           ; 预算穿透到顶层
                    [(lambda (_) #t) (lambda (e) (on-err e))])   ; 脚本级隔离
      (cell-eval! c th)
    ) ; end with-handlers
    (observe!)
  ) ; end define run-task!

  (define (drain-micro!)
    (let loop ()
      (define th (taskq-next-micro! c))
      (when th (run-task! th) (loop))
    ) ; end loop
  ) ; end define drain-micro!

  (define (drain-net!)
    (let loop ()
      (define th (taskq-next-net! c))
      (when th
        (run-task! th)
        (drain-micro!)   ; 每个任务后微任务检查点
        (loop))
    ) ; end loop
  ) ; end define drain-net!

  (define (advance-to! t)
    (when (> t (vclock-now vc))
      (vclock-advance! vc (- t (vclock-now vc)))
    ) ; end when
  ) ; end define advance-to!

  (with-handlers ([exn:fail:budget? (lambda (_) 'budget-hit)])
    (let loop ()
      (drain-micro!)
      (drain-net!)
      (cond
        ;; 在飞网络: 不快进,真实等待(超时后视为静默)
        [(and (not net-gone?) (> (cell-net-inflight c) 0))
         (cond
           [(> (current-inexact-milliseconds) net-deadline)
            (set! net-gone? #t) (loop)]
           [else (sleep NET-POLL-S) (loop)]
         ) ; end cond
        ] ; end 在飞
        [else
         (define-values (t th) (taskq-peek-macro c))
         (cond
           [(and t (<= t cap))   ; vtime 预算内的定时器: 快进执行
            (taskq-pop-macro! c)
            (advance-to! t)
            (run-task! th)
            (loop)]
           [else   ; 无可跑宏任务(空 或 仅剩预算外定时器): 补满安定窗口收官
            (define truncated? (and t (<= (- t (vclock-now vc)) FAR-HORIZON-MS)))
            (advance-to! (+ last-mut-vt QUIET-WINDOW-MS))
            (if (or truncated? (>= (vclock-now vc) cap)) 'budget-hit 'quiet)]
         ) ; end cond
        ] ; end else
      ) ; end cond
    ) ; end loop
  ) ; end with-handlers
) ; end define run-until-quiet!
