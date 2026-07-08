#lang tstring racket
;; event.rkt — 事件总线（design.md §4.8）
;; 每订阅者一个 async-channel + 消费线程：发布方永不被慢订阅者阻塞。

(require
 racket/async-channel
) ; end require

(struct sub
  (chan     ; async-channel
   thd      ; 消费线程
  ) ; end fields
) ; end struct sub

(struct bus
  (subs     ; box of (listof sub)
   sema     ; semaphore 保护 subs
  ) ; end fields
) ; end struct bus

(define (make-bus)
  (bus (box '()) (make-semaphore 1))
) ; end define make-bus

(define (bus-publish! b e)
  (for ([s (in-list (unbox (bus-subs b)))])
    (async-channel-put (sub-chan s) e)
  ) ; end for
) ; end define bus-publish!

;; 订阅：handler 在独立线程上被调用；返回退订 thunk。
;; handler 抛异常只记日志，不打断消费循环。
(define (bus-subscribe! b handler)
  (define ch (make-async-channel))
  (define thd
    (thread
     (lambda ()
       (let loop ()
         (define e (async-channel-get ch))
         (if (and (pair? e) (eq? (car e) 'drain-sentinel))
             (semaphore-post (cdr e))
             (with-handlers ([exn:fail?
                              (lambda (ex)
                                (log-warning f"bus handler error: {(exn-message ex)}")
                              ) ; end lambda
                             ]) ; end handlers
               (handler e)
             ) ; end with-handlers
         ) ; end if
         (loop)
       ) ; end let loop
     ) ; end lambda
    ) ; end thread
  ) ; end define thd
  (define s (sub ch thd))
  (call-with-semaphore (bus-sema b)
    (lambda ()
      (set-box! (bus-subs b) (cons s (unbox (bus-subs b))))
    ) ; end lambda
  ) ; end call-with-semaphore
  ;; 退订 thunk
  (lambda ()
    (call-with-semaphore (bus-sema b)
      (lambda ()
        (set-box! (bus-subs b) (remq s (unbox (bus-subs b))))
      ) ; end lambda
    ) ; end call-with-semaphore
    (kill-thread thd)
  ) ; end lambda
) ; end define bus-subscribe!

;; 等待总线上所有已投递事件被消费（测试/退出前排空用）：
;; 逐订阅者投递一个哨兵并等待其被取出。
(define (bus-drain! b)
  (define done (make-semaphore 0))
  (define n
    (call-with-semaphore (bus-sema b)
      (lambda ()
        (for ([s (in-list (unbox (bus-subs b)))])
          (async-channel-put (sub-chan s) (cons 'drain-sentinel done))
        ) ; end for
        (length (unbox (bus-subs b)))
      ) ; end lambda
    ) ; end call-with-semaphore
  ) ; end define n
  (for ([_ (in-range n)])
    (semaphore-wait done)
  ) ; end for
) ; end define bus-drain!

(provide
 bus?
 make-bus
 bus-publish!
 bus-subscribe!
 bus-drain!
) ; end provide
