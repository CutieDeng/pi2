#lang tstring racket
;; context-test.rkt — token 估算与裁剪单测

(require
 rackunit
 racket/pvector
 racket/list
 racket/async-channel
 (file "../src/model.rkt")
 (file "../src/provider.rkt")
 (file "../src/context.rkt")
) ; end require

;; ---------------------------------------------------------------- 估算

(test-case "estimate: english vs cjk weighting, memo consistency"
  (define en (estimate-string-tokens "the quick brown fox jumps over"))
  (define zh (estimate-string-tokens "快速的棕色狐狸跳过了懒惰的狗"))
  (check-true (> en 0))
  (check-true (> zh 0))
  ;; 估算是确定性的：同串同值（memo 一致性的前提）
  (check-equal? (estimate-string-tokens "hello") (estimate-string-tokens "hello"))
) ; end test-case

(test-case "estimate cache memoizes by index"
  (define hist
    (for/fold ([pv (pvector)]) ([i (in-range 20)])
      (pvector-cons-right pv (text-msg 'user f"message number {i} with some content"))
    ) ; end for/fold
  ) ; end define hist
  (define cache (make-estimate-cache))
  (define t1 (history-tokens hist cache))
  (define t2 (history-tokens hist cache))     ; 第二次全命中 memo
  (check-equal? t1 t2)
  (check-true (> t1 0))
) ; end test-case

;; ---------------------------------------------------------------- 裁剪

(define (mk-turn i)
  (list (text-msg 'user f"user question {i}")
        (text-msg 'assistant f"assistant answer {i}")
  ) ; end list
) ; end define mk-turn

(test-case "context-fit: passthrough when under budget"
  (define hist
    (for/fold ([pv (pvector)]) ([m (in-list (append-map mk-turn (range 3)))])
      (pvector-cons-right pv m)
    ) ; end for/fold
  ) ; end define hist
  (define cfg (struct-copy config (default-config) [context-budget 100000]))
  (define window (context-fit hist cfg))
  (check-equal? (length window) 6)            ; 全透传
) ; end test-case

(test-case "context-fit: trims middle, keeps anchor + recent"
  ;; 构造超预算历史：许多轮，每轮内容较大
  (define big (make-string 400 #\x))
  (define msgs
    (append
     (list (text-msg 'user f"ANCHOR TASK {big}"))
     (append-map
      (lambda (i)
        (list (text-msg 'user f"middle q {i} {big}")
              (text-msg 'assistant f"middle a {i} {big}")
        ) ; end list
      ) ; end lambda
      (range 20)
     ) ; end append-map
     (list (text-msg 'user "RECENT QUESTION")
           (text-msg 'assistant "RECENT ANSWER")
     ) ; end list
    ) ; end append
  ) ; end define msgs
  (define hist
    (for/fold ([pv (pvector)]) ([m (in-list msgs)])
      (pvector-cons-right pv m)
    ) ; end for/fold
  ) ; end define hist
  (define cfg (struct-copy config (default-config) [context-budget 500]))
  (define window (context-fit hist cfg))
  (check-true (< (length window) (length msgs)) "should trim")
  ;; 锚点保留（首条 user，含省略说明）
  (check-true (regexp-match? #rx"ANCHOR TASK" (message-text (first window))))
  ;; 最近轮保留
  (check-true (regexp-match? #rx"RECENT ANSWER"
                             (message-text (last window))))
  ;; 估算后的窗口不超预算（留边际）
  (check-true (<= (estimate-tokens window) (config-context-budget cfg)))
) ; end test-case

;; ---------------------------------------------------------------- compact!

(test-case "compact! summarizes head, keeps recent (mock provider)"
  (define mock
    (provider "mock"
              (lambda (_msgs _tools)
                (define ch (make-async-channel))
                (thread
                 (lambda ()
                   (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant "SUMMARY: did X, file a.txt")))
                   (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 5 5)))
                 ) ; end lambda
                ) ; end thread
                ch
              ) ; end lambda
              void
    ) ; end provider
  ) ; end define mock
  (define st0 (make-initial-state (default-config)))
  (define st
    (for/fold ([s st0]) ([i (in-range 10)])
      (state-append s (text-msg (if (even? i) 'user 'assistant) f"msg {i}"))
    ) ; end for/fold
  ) ; end define st
  (define st* (compact! st mock #:keep-recent 4))
  (define hist (state-history-list st*))
  ;; summary(1) + recent(4) = 5
  (check-equal? (length hist) 5)
  (check-true (regexp-match? #rx"compacted context summary" (message-text (first hist))))
  (check-true (regexp-match? #rx"SUMMARY: did X" (message-text (first hist))))
  (check-equal? (message-text (last hist)) "msg 9")
) ; end test-case

(displayln "context-test: all passed")
