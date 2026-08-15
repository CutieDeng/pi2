#lang racket/base
;; tests/browser/l2-cell-test.rkt — L2 cell：隔离四层/预算强制/taskq/vclock

(require rackunit
         (file "../../src/web/browser/cell/budget.rkt")
         (file "../../src/web/browser/cell/cell.rkt"))

;; -------- budget prefab 往返
(let ()
  (define b (make-budget #:cpu-ms 123))
  (check-equal? (budget-cpu-ms b) 123)
  (check-equal? (budget-mem-bytes DEFAULT-BUDGET) (* 128 1024 1024))
  (define s (open-output-string))
  (write b s)
  (check-equal? (read (open-input-string (get-output-string s))) b)
) ; end let

;; -------- 基本求值与命名空间隔离
(let ()
  (define c1 (make-cell))
  (define c2 (make-cell))
  (check-equal? (cell-eval! c1 '(+ 1 2)) 3)
  (cell-eval! c1 '(define secret 42))
  (check-equal? (cell-eval! c1 'secret) 42)          ; 同 cell 状态保持
  (check-exn exn:fail? (lambda () (cell-eval! c2 'secret)))   ; 跨 cell 不可见
  (check-true (procedure? (cell-eval! c1 '(lambda (x) x))))
  (check-equal? (cell-eval! c1 (lambda () 'thunk-ok)) 'thunk-ok)
  ;; 页面世界异常穿回宿主(L10 再分类)
  (check-exn #rx"boom" (lambda () (cell-eval! c1 '(error 'x "boom"))))
  (check-true (cell-alive? c1))                       ; 异常不杀 cell
  (cell-kill! c1) (cell-kill! c2)
) ; end let

;; -------- security-guard: 页世界 IO 不存在
(let ()
  (define c (make-cell))
  (check-exn #rx"file access denied"
             (lambda () (cell-eval! c '(open-input-file "/etc/passwd"))))
  (check-exn #rx"denied"
             (lambda () (cell-eval! c '(begin (require racket/tcp)
                                              (tcp-connect "example.org" 80)))))
  (check-true (cell-alive? c))
  (cell-kill! c)
) ; end let

;; -------- CPU 预算: while(1) 拖不死宿主
(let ()
  (define c (make-cell #:budget (make-budget #:cpu-ms 100)))
  (define t0 (current-inexact-milliseconds))
  (check-exn (lambda (e) (and (exn:fail:budget? e) (eq? (exn:fail:budget-kind e) 'cpu)))
             (lambda () (cell-eval! c '(let loop ([i 0]) (loop (add1 i))))))
  (check-true (< (- (current-inexact-milliseconds) t0) 5000))   ; 宿主很快拿回控制权
  (check-true (> (cell-cpu-used c) 0))
  (cell-kill! c)
) ; end let

;; -------- wall 安全阀: 纯 sleep 不耗 CPU 也被回收
(let ()
  (define c (make-cell #:budget (make-budget #:cpu-ms 50)))   ; wall-limit = 1000ms
  (check-exn (lambda (e) (and (exn:fail:budget? e) (eq? (exn:fail:budget-kind e) 'wall)))
             (lambda () (cell-eval! c '(sleep 30))))
  (cell-kill! c)
) ; end let

;; -------- 内存预算: custodian 秒杀,且不波及其它 cell
(let ()
  (define victim (make-cell #:budget (make-budget #:mem-bytes (* 8 1024 1024))))
  (define bystander (make-cell))
  (check-exn (lambda (e) (and (exn:fail:budget? e)
                              (memq (exn:fail:budget-kind e) '(killed cpu wall))))
             (lambda ()
               (cell-eval! victim
                           '(let loop ([acc '()])
                              (loop (cons (make-bytes (* 1024 1024) 65) acc))))))
  (check-false (cell-alive? victim))
  ;; 死 cell 拒绝再求值
  (check-exn (lambda (e) (and (exn:fail:budget? e) (eq? (exn:fail:budget-kind e) 'killed)))
             (lambda () (cell-eval! victim '(+ 1 1))))
  ;; 旁观 cell 不受影响
  (check-equal? (cell-eval! bystander '(* 6 7)) 42)
  (cell-kill! bystander)
) ; end let

;; -------- with-cell: 离开即回收
(let ()
  (define escaped #f)
  (define r (with-cell (make-budget)
              (lambda (c) (set! escaped c) (cell-eval! c '(list 1 2)))))
  (check-equal? r '(1 2))
  (check-false (cell-alive? escaped))
) ; end let

;; -------- vclock
(let ()
  (define c (make-cell))
  (define vc (cell-vclock c))
  (check-equal? (vclock-now vc) 0)
  (vclock-advance! vc 300)
  (check-equal? (vclock-now vc) 300)
  (cell-kill! c)
) ; end let

;; -------- taskq: micro FIFO / macro 按 vtime 稳定序 / net 存活票据
(let ()
  (define c (make-cell))
  (check-true (taskq-empty? c))

  (taskq-micro! c (lambda () 'a))
  (taskq-micro! c (lambda () 'b))
  (check-equal? ((taskq-next-micro! c)) 'a)
  (taskq-micro! c (lambda () 'c))
  (check-equal? ((taskq-next-micro! c)) 'b)
  (check-equal? ((taskq-next-micro! c)) 'c)
  (check-false (taskq-next-micro! c))

  (taskq-macro! c 100 (lambda () 'at100-first))
  (taskq-macro! c 50 (lambda () 'at50))
  (taskq-macro! c 100 (lambda () 'at100-second))
  (let-values ([(t th) (taskq-peek-macro c)])
    (check-equal? t 50)
    (check-equal? (th) 'at50))
  (taskq-pop-macro! c)
  (let-values ([(t th) (taskq-peek-macro c)])
    (check-equal? (th) 'at100-first))    ; 同刻保投递序
  (taskq-pop-macro! c)
  (let-values ([(t th) (taskq-peek-macro c)])
    (check-equal? (th) 'at100-second))
  (taskq-pop-macro! c)
  (check-true (taskq-empty? c))

  ;; 宿主线程投递 net 完成事件;kill 后投递被拒(存活票据)
  (define posted (make-channel))
  (thread (lambda () (channel-put posted (taskq-net! c (lambda () 'resp)))))
  (check-true (channel-get posted))
  (check-equal? ((taskq-next-net! c)) 'resp)
  (cell-kill! c)
  (check-false (taskq-net! c (lambda () 'late)))
  (check-false (taskq-micro! c (lambda () 'late)))
) ; end let
