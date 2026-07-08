#lang racket-tstring
;; base-test.rkt — model / rktd / event 单测（M1 基础层）

(require
 rackunit
 racket/pvector
 racket/file
 (file "../model.rkt")
 (file "../rktd.rkt")
 (file "../event.rkt")
) ; end require

;; ---------------------------------------------------------------- model

(test-case "prefab message write/read roundtrip"
  (define m
    (message 'assistant
             (list (text-block "hi")
                   (tool-use-block "id1" "bash" (hasheq 'command "ls"))
             ) ; end list
    ) ; end message
  ) ; end define m
  (define back (read (open-input-string (format "~s" m))))
  (check-equal? back m)
  (check-equal? (message-text m) "hi")
  (check-equal? (length (message-tool-uses m)) 1)
) ; end test-case

(test-case "state-append is persistent"
  (define st0 (make-initial-state (default-config)))
  (define st1 (state-append st0 (text-msg 'user "a")))
  (define st2 (state-append st1 (text-msg 'user "b")))
  (check-equal? (pvector-length (agent-state-history st0)) 0)
  (check-equal? (pvector-length (agent-state-history st1)) 1)
  (check-equal? (pvector-length (agent-state-history st2)) 2)
  (check-equal? (message-text (pvector-ref (agent-state-history st2) 1)) "b")
) ; end test-case

(test-case "usage-add"
  (check-equal? (usage-add (usage 1 2) (usage 10 20)) (usage 11 22))
) ; end test-case

;; ---------------------------------------------------------------- rktd

(test-case "datum-log append + fold roundtrip"
  (define p (make-temporary-file "pi2-test-~a.rktd"))
  (delete-file p)
  (define lg (datum-log-open! p))
  (datum-log-append! lg (message 'user (list (text-block "第一条 with 中文"))))
  (datum-log-append! lg (message 'assistant (list (text-block "second"))))
  (datum-log-close! lg)
  (define all (datum-log-fold p cons '()))
  (check-equal? (length all) 2)
  (check-equal? (message-text (car all)) "second")            ; cons 逆序
  (check-equal? (message-text (cadr all)) "第一条 with 中文")
  ;; 流式序列可提前停止
  (define first-only
    (for/first ([d (in-datum-log p)]) d)
  ) ; end define first-only
  (check-equal? (message-text first-only) "第一条 with 中文")
  ;; 首 datum 快速读取
  (check-equal? (datum-log-first p) first-only)
  (delete-file p)
) ; end test-case

(test-case "datum-log tolerates truncated tail"
  (define p (make-temporary-file "pi2-trunc-~a.rktd"))
  (call-with-output-file p #:exists 'truncate
    (lambda (out)
      (writeln '(rec msg "ts" (ok)) out)
      (display "#s(message user (#s(text-bl" out)   ; 截断的尾 datum
    ) ; end lambda
  ) ; end call-with-output-file
  (define all (datum-log-fold p cons '()))
  (check-equal? all '((rec msg "ts" (ok))))
  (delete-file p)
) ; end test-case

;; ---------------------------------------------------------------- event

(test-case "bus publish/subscribe/unsubscribe"
  (define b (make-bus))
  (define got (box '()))
  (define unsub
    (bus-subscribe! b
      (lambda (e)
        (set-box! got (cons e (unbox got)))
      ) ; end lambda
    ) ; end bus-subscribe!
  ) ; end define unsub
  (bus-publish! b 'e1)
  (bus-publish! b 'e2)
  (bus-drain! b)
  (check-equal? (reverse (unbox got)) '(e1 e2))
  (unsub)
  (bus-publish! b 'e3)
  (bus-drain! b)
  (check-equal? (reverse (unbox got)) '(e1 e2))
) ; end test-case

(test-case "slow/crashing handler does not block publisher"
  (define b (make-bus))
  (define n (box 0))
  (bus-subscribe! b
    (lambda (_e)
      (error "boom")
    ) ; end lambda
  ) ; end bus-subscribe!
  (bus-subscribe! b
    (lambda (_e)
      (set-box! n (add1 (unbox n)))
    ) ; end lambda
  ) ; end bus-subscribe!
  (for ([i (in-range 10)])
    (bus-publish! b i)
  ) ; end for
  (bus-drain! b)
  (check-equal? (unbox n) 10)
) ; end test-case

(displayln "base-test: all passed")
