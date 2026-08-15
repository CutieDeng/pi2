#lang racket/base
;; browser/cell/cell.rkt — L2 · 页隔离单元（design-chrome.md L2 / §2.4）
;; 职责: 四层隔离装配(namespace+security-guard+custodian+时间片) · taskq · vclock
;;       · CPU/内存预算强制 · kill 与存活票据(设计 §8 问题 7 的原子写回)
;; 抢占(stock 档): worker 线程 + current-process-milliseconds 每 ~10ms 轮询——
;;       与 engine 时间片同语义(engine 亦线程封装);feat-fuel/档1 燃料插桩由 L7 接入
;; 已知简化: cell 内自旋线程的 CPU 只计主 worker(B2 namespace 收紧后无 thread 可用)
;; 不做: 事件循环调度(L9)、webapi 绑定注入(L8)
;; 依赖: racket/base browser/cell/budget

(require racket/contract "budget.rkt")

(provide
 (contract-out
  [vclock? (-> any/c boolean?)]
  [vclock-now (-> vclock? exact-nonnegative-integer?)]
  [cell? (-> any/c boolean?)]
  [exn:fail:budget? (-> any/c boolean?)]
  [exn:fail:budget-kind (-> exn:fail:budget? symbol?)]
  [make-cell (->* () (#:budget budget?) cell?)]
  [cell-budget (-> cell? budget?)]
  [cell-vclock (-> cell? vclock?)]
  [cell-ns (-> cell? namespace?)]
  [cell-cpu-used (-> cell? exact-nonnegative-integer?)]
  [cell-alive? (-> cell? boolean?)]
  [cell-eval! (-> cell? any/c any)]
  [cell-kill! (-> cell? void?)]
  [with-cell (-> budget? (-> cell? any) any)]
  ;; taskq —— 数据在 L2,L8 只入队、L9 只出队(§1 依赖单向)
  [taskq-micro! (-> cell? (-> any) boolean?)]
  [taskq-macro! (-> cell? exact-nonnegative-integer? (-> any) boolean?)]
  [taskq-net! (-> cell? (-> any) boolean?)]
  ;; 网络在飞计数(L8 发请求前 begin,完成回调 complete——原子递减+入队)
  [cell-net-inflight (-> cell? exact-nonnegative-integer?)]
  [cell-net-begin! (-> cell? boolean?)]
  [taskq-net-complete! (-> cell? (-> any) boolean?)]
  [taskq-next-micro! (-> cell? (or/c (-> any) #f))]
  [taskq-next-net! (-> cell? (or/c (-> any) #f))]
  [taskq-peek-macro (-> cell? (values (or/c exact-nonnegative-integer? #f) (or/c (-> any) #f)))]
  [taskq-pop-macro! (-> cell? void?)]
  [taskq-empty? (-> cell? boolean?)]
  ;; vclock
  [vclock-advance! (-> vclock? exact-nonnegative-integer? void?)]
 ) ; end contract-out
) ; end provide

;; ---------------------------------------------------------------- 结构

(struct vclock ([now #:mutable]))

(define (vclock-advance! vc ms)
  (set-vclock-now! vc (+ (vclock-now vc) ms))
) ; end define vclock-advance!

;; taskq: micro/net 为 FIFO(新任务 cons 到 in,出队时倒 out);macro 为按 vtime 升序表
(struct taskq ([micro-in #:mutable] [micro-out #:mutable]
               [macro #:mutable]
               [net-in #:mutable] [net-out #:mutable]))

(struct cell (cust ns guard budget vclock tq sema
              [cpu-used #:mutable] [dead? #:mutable] [net-inflight #:mutable]))

(struct exn:fail:budget exn:fail (kind))   ; kind: 'cpu | 'wall | 'killed

(define (budget-error kind fmt . args)
  (raise (exn:fail:budget (string-append "budget: " (apply format fmt args))
                          (current-continuation-marks) kind))
) ; end define budget-error

;; ---------------------------------------------------------------- 装配

(define (make-deny-all-guard)
  (make-security-guard
   (current-security-guard)
   (lambda (who path modes) (error 'cell "file access denied: ~a ~a" who path))
   (lambda (who host port mode) (error 'cell "network access denied: ~a ~a:~a" who host port))
   (lambda (who path target) (error 'cell "link access denied: ~a" who))
  ) ; end make-security-guard
) ; end define make-deny-all-guard

(define (make-cell #:budget [b DEFAULT-BUDGET])
  (define cust (make-custodian))
  (custodian-limit-memory cust (budget-mem-bytes b) cust)
  ;; namespace 在装配期(guard 之外)创建;B2 收紧为"仅 webapi 白名单"的空 namespace
  (define ns (make-base-namespace))
  (cell cust ns (make-deny-all-guard) b (vclock 0)
        (taskq '() '() '() '() '())
        (make-semaphore 1) 0 #f 0)
) ; end define make-cell

(define (cell-alive? c) (not (cell-dead? c)))

(define (cell-kill! c)
  (call-with-semaphore (cell-sema c)
    (lambda ()
      (set-cell-dead?! c #t)
      (custodian-shutdown-all (cell-cust c))
    ) ; end lambda
  ) ; end call-with-semaphore
) ; end define cell-kill!

(define (with-cell b proc)
  (define c (make-cell #:budget b))
  (dynamic-wind
   void
   (lambda () (proc c))
   (lambda () (cell-kill! c))
  ) ; end dynamic-wind
) ; end define with-cell

;; ---------------------------------------------------------------- 求值与抢占

(define SLICE-S 0.01)   ; ~10ms 收权一次

;; form 为过程 → 直接调用;否则 eval 于页 namespace。
;; 页代码在 worker 线程(挂 cell custodian)+ deny-all guard 下执行;
;; 宿主按片轮询 CPU,超预算 kill-thread 抛 exn:fail:budget。
(define (cell-eval! c form)
  (unless (cell-alive? c) (budget-error 'killed "cell is dead"))
  (define b (cell-budget c))
  (define ch (make-channel))
  (define worker
    (parameterize ([current-custodian (cell-cust c)])
      (thread
       (lambda ()
         (define r
           (with-handlers ([(lambda (_) #t) (lambda (e) (cons 'exn e))])
             (cons 'ok
                   (parameterize ([current-namespace (cell-ns c)]
                                  [current-security-guard (cell-guard c)])
                     (if (procedure? form) (form) (eval form))))
           ) ; end with-handlers
         ) ; end define r
         (channel-put ch r)
       ) ; end lambda
      ) ; end thread
    ) ; end parameterize
  ) ; end define worker
  (define wall-limit (max 1000 (* 5 (budget-cpu-ms b))))
  (define t0 (current-inexact-milliseconds))
  (define done-evt (wrap-evt ch (lambda (v) v)))
  (define dead-evt (wrap-evt (thread-dead-evt worker) (lambda (_) '(killed))))
  (define (worker-cpu last)
    (with-handlers ([(lambda (_) #t) (lambda (_) last)])
      (current-process-milliseconds worker))
  ) ; end define worker-cpu
  (let loop ([last-cpu 0])
    (define r (sync/timeout SLICE-S done-evt dead-evt))
    (cond
      [(not r)   ; 片到期:收权检查
       (define cpu (worker-cpu last-cpu))
       (cond
         [(> (+ (cell-cpu-used c) cpu) (budget-cpu-ms b))
          (kill-thread worker)
          (set-cell-cpu-used! c (+ (cell-cpu-used c) cpu))
          (budget-error 'cpu "cpu budget exhausted (~a ms)" (budget-cpu-ms b))]
         [(> (- (current-inexact-milliseconds) t0) wall-limit)
          (kill-thread worker)
          (set-cell-cpu-used! c (+ (cell-cpu-used c) cpu))
          (budget-error 'wall "wall-clock safety limit (~a ms)" wall-limit)]
         [else (loop cpu)]
       ) ; end cond
      ] ; end not r
      [(eq? (car r) 'killed)   ; worker 未交结果即死 → custodian 秒杀(内存超限/外部 kill)
       (set-cell-dead?! c #t)
       (budget-error 'killed "cell custodian shut down (memory limit or kill)")]
      [else
       (set-cell-cpu-used! c (+ (cell-cpu-used c) (worker-cpu last-cpu)))
       (if (eq? (car r) 'ok) (cdr r) (raise (cdr r)))]
    ) ; end cond
  ) ; end loop
) ; end define cell-eval!

;; ---------------------------------------------------------------- taskq
;; 全部入/出队过 cell 信号量;死 cell 入队返回 #f(§8 问题 7:写回持存活票据)。

(define (with-lock c thunk) (call-with-semaphore (cell-sema c) thunk))

(define (taskq-micro! c thunk)
  (with-lock c (lambda ()
    (cond [(cell-dead? c) #f]
          [else (set-taskq-micro-in! (cell-tq c) (cons thunk (taskq-micro-in (cell-tq c)))) #t])))
) ; end define taskq-micro!

(define (taskq-net! c thunk)
  (with-lock c (lambda ()
    (cond [(cell-dead? c) #f]
          [else (set-taskq-net-in! (cell-tq c) (cons thunk (taskq-net-in (cell-tq c)))) #t])))
) ; end define taskq-net!

;; 发请求前登记在飞;死 cell 拒绝(存活票据)
(define (cell-net-begin! c)
  (with-lock c (lambda ()
    (cond [(cell-dead? c) #f]
          [else (set-cell-net-inflight! c (add1 (cell-net-inflight c))) #t])))
) ; end define cell-net-begin!

;; 完成回调: 原子地 递减在飞 + 入 net 队列;死 cell 只递减(响应丢弃)
(define (taskq-net-complete! c thunk)
  (with-lock c (lambda ()
    (set-cell-net-inflight! c (max 0 (sub1 (cell-net-inflight c))))
    (cond [(cell-dead? c) #f]
          [else (set-taskq-net-in! (cell-tq c) (cons thunk (taskq-net-in (cell-tq c)))) #t])))
) ; end define taskq-net-complete!

(define (taskq-macro! c vtime thunk)
  (with-lock c (lambda ()
    (cond
      [(cell-dead? c) #f]
      [else
       (define tq (cell-tq c))
       ;; 按 vtime 稳定升序插入(同刻保投递序)
       (set-taskq-macro! tq
         (let ins ([xs (taskq-macro tq)])
           (cond [(null? xs) (list (cons vtime thunk))]
                 [(<= (caar xs) vtime) (cons (car xs) (ins (cdr xs)))]
                 [else (cons (cons vtime thunk) xs)])))
       #t])))
) ; end define taskq-macro!

;; FIFO 出队(两栈队列)
(define (pop-fifo! c get-in set-in! get-out set-out!)
  (with-lock c (lambda ()
    (define tq (cell-tq c))
    (when (and (null? (get-out tq)) (pair? (get-in tq)))
      (set-out! tq (reverse (get-in tq)))
      (set-in! tq '())
    ) ; end when
    (cond [(null? (get-out tq)) #f]
          [else (define x (car (get-out tq)))
                (set-out! tq (cdr (get-out tq)))
                x])))
) ; end define pop-fifo!

(define (taskq-next-micro! c)
  (pop-fifo! c taskq-micro-in set-taskq-micro-in! taskq-micro-out set-taskq-micro-out!)
) ; end define taskq-next-micro!

(define (taskq-next-net! c)
  (pop-fifo! c taskq-net-in set-taskq-net-in! taskq-net-out set-taskq-net-out!)
) ; end define taskq-next-net!

(define (taskq-peek-macro c)
  (with-lock c (lambda ()
    (define m (taskq-macro (cell-tq c)))
    (if (null? m) (values #f #f) (values (caar m) (cdar m)))))
) ; end define taskq-peek-macro

(define (taskq-pop-macro! c)
  (with-lock c (lambda ()
    (define tq (cell-tq c))
    (unless (null? (taskq-macro tq))
      (set-taskq-macro! tq (cdr (taskq-macro tq))))
    (void)))
) ; end define taskq-pop-macro!

(define (taskq-empty? c)
  (with-lock c (lambda ()
    (define tq (cell-tq c))
    (and (null? (taskq-micro-in tq)) (null? (taskq-micro-out tq))
         (null? (taskq-macro tq))
         (null? (taskq-net-in tq)) (null? (taskq-net-out tq)))))
) ; end define taskq-empty?
