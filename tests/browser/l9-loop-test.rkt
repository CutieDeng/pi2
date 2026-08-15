#lang racket/base
;; tests/browser/l9-loop-test.rkt — L9 loop：安定态/快进/网络等待/预算/确定性重放

(require rackunit
         (file "../../src/web/browser/cell/budget.rkt")
         (file "../../src/web/browser/cell/cell.rkt")
         (file "../../src/web/browser/loop.rkt")
         (file "../../src/web/browser/dom/node.rkt")
         (file "../../src/web/browser/dom/mutation.rkt")
         (file "../../src/web/browser/dom/serialize.rkt"))

(define (fresh [b (make-budget)])
  (define c (make-cell #:budget b))
  (define doc (make-document (make-element 'body)))
  (values c doc)
) ; end define fresh

;; -------- 空页: 直接安定,窗口补满
(let-values ([(c doc) (fresh)])
  (check-equal? (run-until-quiet! c doc) 'quiet)
  (check-equal? (vclock-now (cell-vclock c)) 200)
  (cell-kill! c)
) ; end let

;; -------- 骨架屏: 300ms 定时器填充 DOM → 快进执行 → 安定
(define (skeleton-run!)
  (define-values (c doc) (fresh))
  (taskq-macro! c 300 (lambda ()
                        (dom-append! doc (ddoc-root doc) (make-text "content"))))
  (define r (run-until-quiet! c doc))
  (define snap (list r (vclock-now (cell-vclock c)) (dom->sexp (ddoc-root doc))))
  (cell-kill! c)
  snap
) ; end define skeleton-run!

(let ()
  (define s1 (skeleton-run!))
  (check-equal? (car s1) 'quiet)
  (check-equal? (cadr s1) 500)                       ; 300 + 200 窗口
  (check-equal? (caddr s1) '(body () "content"))
  (check-equal? (skeleton-run!) s1)                  ; 确定性重放: 逐位一致
) ; end let

;; -------- 序: 微任务先于宏任务;宏任务后微任务检查点
(let-values ([(c doc) (fresh)])
  (define order '())
  (define (mark! x) (set! order (cons x order)))
  (taskq-macro! c 0 (lambda () (mark! 'macro) (taskq-micro! c (lambda () (mark! 'micro-after)))))
  (taskq-micro! c (lambda () (mark! 'micro-first)))
  (check-equal? (run-until-quiet! c doc) 'quiet)
  (check-equal? (reverse order) '(micro-first macro micro-after))
  (cell-kill! c)
) ; end let

;; -------- rAF 型自续定时器不改 DOM: vtime 触顶 → budget-hit,宿主不被拖死
(let-values ([(c doc) (fresh (make-budget #:vtime-ms 500))])
  (define ticks 0)
  (define (raf!)
    (set! ticks (add1 ticks))
    (taskq-macro! c (+ (vclock-now (cell-vclock c)) 16) raf!)
  ) ; end define raf!
  (taskq-macro! c 16 raf!)
  (check-equal? (run-until-quiet! c doc) 'budget-hit)
  (check-true (>= ticks 30))                          ; 500/16 ≈ 31 tick 都跑到了
  (cell-kill! c)
) ; end let

;; -------- 远期定时器(预算外长轮询): 放弃,不算触顶
(let-values ([(c doc) (fresh (make-budget #:vtime-ms 1000))])
  (define fired #f)
  (taskq-macro! c 60000 (lambda () (set! fired #t)))
  (check-equal? (run-until-quiet! c doc) 'quiet)
  (check-false fired)
  (check-equal? (vclock-now (cell-vclock c)) 200)
  (cell-kill! c)
) ; end let

;; -------- CPU 预算: 恶意任务被抢占 → budget-hit
(let-values ([(c doc) (fresh (make-budget #:cpu-ms 80))])
  (taskq-macro! c 0 (lambda () (let spin () (spin))))
  (define t0 (current-inexact-milliseconds))
  (check-equal? (run-until-quiet! c doc) 'budget-hit)
  (check-true (< (- (current-inexact-milliseconds) t0) 5000))
  (cell-kill! c)
) ; end let

;; -------- 脚本级失败隔离: 坏任务记错继续,后续任务照跑
(let-values ([(c doc) (fresh)])
  (define errs '())
  (taskq-macro! c 10 (lambda () (error 'task "bad script")))
  (taskq-macro! c 20 (lambda () (dom-append! doc (ddoc-root doc) (make-text "ok"))))
  (check-equal? (run-until-quiet! c doc #:on-task-error (lambda (e) (set! errs (cons e errs))))
                'quiet)
  (check-equal? (length errs) 1)
  (check-true (regexp-match? #rx"bad script" (exn-message (car errs))))
  (check-equal? (dom->sexp (ddoc-root doc)) '(body () "ok"))
  (cell-kill! c)
) ; end let

;; -------- 在飞网络: 快进被挡,真实等待完成回调后才动定时器
(let-values ([(c doc) (fresh)])
  (define order '())
  (check-true (cell-net-begin! c))
  (check-equal? (cell-net-inflight c) 1)
  ;; 500ms(虚拟)的定时器 vs 50ms(真实)后返回的网络——网络必须先落
  (taskq-macro! c 500 (lambda () (set! order (cons 'timer order))))
  (thread (lambda ()
            (sleep 0.05)
            (taskq-net-complete! c (lambda () (set! order (cons 'net order))))))
  (check-equal? (run-until-quiet! c doc) 'quiet)
  (check-equal? (reverse order) '(net timer))
  (check-equal? (cell-net-inflight c) 0)
  (cell-kill! c)
) ; end let

;; -------- 网络挂死: 超时视为静默,不永久卡住
(let-values ([(c doc) (fresh)])
  (check-true (cell-net-begin! c))
  (define t0 (current-inexact-milliseconds))
  (check-equal? (run-until-quiet! c doc #:net-timeout-ms 60) 'quiet)
  (check-true (< (- (current-inexact-milliseconds) t0) 3000))
  (cell-kill! c)
) ; end let

;; -------- kill 后的 net 完成被丢弃(存活票据,§8 问题 7)
(let-values ([(c doc) (fresh)])
  (check-true (cell-net-begin! c))
  (cell-kill! c)
  (check-false (taskq-net-complete! c (lambda () 'late)))
  (check-equal? (cell-net-inflight c) 0)   ; 计数仍归位
) ; end let
