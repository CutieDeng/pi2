#lang tstring racket
;; compact-persist-test.rkt — 会话记录 P0（design-content.md §7）：消息在追加进历史时落盘；压缩、出错撤回、中断、清空都记下；
;; 重放（session-replay）得到的历史与内核内存里的一致。之前的缺陷：轮内自动压缩后按下标落盘 → 本轮消息丢失。

(require rackunit json racket/async-channel racket/pvector
         (file "../src/model.rkt") (file "../src/event.rkt") (file "../src/provider.rkt") (file "../src/tool.rkt")
         (file "../src/permission.rkt") (file "../src/loop.rkt") (file "../src/session.rkt") (file "../src/plugin.rkt")
         (file "../src/rpc.rkt") (file "../src/rktd.rkt"))

(define dir (make-temporary-file "pi2-cp-~a" 'directory))
(void (putenv "PI_CONFIG_HOME" (path->string dir)))

;; 脚本模型：问摘要 → SUMMARY；文字含 boom → 报错；含 hang → 吐一段后挂住直到取消；其余 → ANSWER
(define (make-prov)
  (define cancel (make-semaphore 0))
  (provider "s"
            (lambda (msgs _t)
              (define ch (make-async-channel))
              (define last-text (message-text (last msgs)))
              (thread (lambda ()
                        (cond
                          [(regexp-match? #rx"(?i:summar)" last-text)
                           (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant "SUMMARY")))
                           (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 1 1)))]
                          [(regexp-match? #rx"boom" last-text)
                           (async-channel-put ch (evt:error (now-ms) (exn:fail "boom: unexpected upstream failure" (current-continuation-marks)) #f))]
                          [(regexp-match? #rx"hang" last-text)
                           (async-channel-put ch (evt:delta (now-ms) 'text "partial"))
                           (semaphore-wait cancel)]
                          [else
                           (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant "ANSWER")))
                           (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 1 1)))])))
              ch)
            (lambda () (semaphore-post cancel))))

(define (start #:prefill [n 0] #:budget [budget 100000])
  (define cfg (struct-copy config (default-config) [workdir (path->string dir)] [permission-mode 'yolo] [context-budget budget]))
  (define path (fresh-session-path (path->string dir)))
  (define s0 (session-open! path cfg))
  (for ([i n]) (session-append-msg! s0 (text-msg (if (even? i) 'user 'assistant) (make-string 200 #\x))))
  (session-close! s0)
  (define st (session-replay path #:config cfg))
  (define sess (session-open! path cfg))
  (define host (make-plugin-host))
  (define d (make-deps #:provider (make-prov) #:registry (make-registry '()) #:bus (make-bus) #:policy (make-policy cfg) #:plugin-host host))
  (define-values (in-r in-w) (make-pipe)) (define-values (out-r out-w) (make-pipe))
  (thread (lambda () (parameterize ([current-input-port in-r] [current-output-port out-w])
                       (run-rpc! d st sess #:plugin-host host #:fetch-models (lambda (_e) #f)))
            (close-output-port out-w)))
  (define (send! j) (write-string (jsexpr->string j) in-w) (newline in-w) (flush-output in-w))
  (define (expect! type) (let loop () (define j (string->jsexpr (sync/timeout 10 (read-line-evt out-r 'linefeed)))) (if (equal? (hash-ref j 'type #f) type) j (loop))))
  (expect! "ready")
  (values path cfg send! expect!))

(define (texts msgs) (for/list ([m msgs]) (hash-ref m 'text "")))
(define (kernel-history send! expect!) (send! (hasheq 'type "history")) (texts (hash-ref (expect! "history") 'messages)))
(define (replayed path cfg) (for/list ([m (in-pvector (agent-state-history (session-replay path #:config cfg)))]) (message-text m)))
(define (count-recs path type) (for/sum ([d (in-datum-log path)]) (if (and (rec? d) (eq? (rec-type d) type)) 1 0)))
(define (finish! send! expect!) (send! (hasheq 'type "shutdown")) (expect! "bye"))

(test-case "auto compaction inside a turn: the turn's messages are recorded, plus the compacted history; replay = kernel"
  (define-values (path cfg send! expect!) (start #:prefill 20 #:budget 300))
  (send! (hasheq 'type "prompt" 'text "new question after a long history"))
  (expect! "turn_complete")
  (define kh (kernel-history send! expect!))
  (finish! send! expect!)
  (check-regexp-match #rx"^\\[compacted context summary\\]" (car kh))
  (check-equal? (count-recs path 'msg) 22)                       ; 20 + 用户 + 回答（之前是 20）
  (check-equal? (count-recs path 'compact) 1)
  (check-equal? (replayed path cfg) kh))

(test-case "a failed turn: its messages are recorded and then reverted; replay = state before the turn"
  (define-values (path cfg send! expect!) (start #:prefill 4))
  (send! (hasheq 'type "prompt" 'text "ok first"))
  (expect! "turn_complete")
  (send! (hasheq 'type "prompt" 'text "this will boom"))
  (define tc (expect! "turn_complete"))
  (check-equal? (hash-ref tc 'outcome) "error")
  (define kh (kernel-history send! expect!))
  (finish! send! expect!)
  (check-equal? (length kh) 6)
  (check-true (>= (count-recs path 'msg) 7))                     ; 出错那轮的用户消息也在文件里（所有信息都有记录）
  (check-equal? (replayed path cfg) kh))

(test-case "an interrupted turn keeps finished steps + marker; replay = kernel"
  (define-values (path cfg send! expect!) (start #:prefill 2))
  (send! (hasheq 'type "prompt" 'text "please hang"))
  (sleep 0.3)
  (send! (hasheq 'type "interrupt"))
  (define tc (expect! "turn_complete"))
  (check-true (hash-ref tc 'interrupted))
  (define kh (kernel-history send! expect!))
  (finish! send! expect!)
  (check-regexp-match #rx"^<turn_aborted>" (last kh))
  (check-equal? (replayed path cfg) kh))

(test-case "clear is recorded: replay gives an empty history; the old messages stay in the file"
  (define-values (path cfg send! expect!) (start #:prefill 4))
  (send! (hasheq 'type "clear"))
  (expect! "ok")
  (send! (hasheq 'type "prompt" 'text "after clear"))
  (expect! "turn_complete")
  (define kh (kernel-history send! expect!))
  (finish! send! expect!)
  (check-equal? kh '("after clear" "ANSWER"))
  (check-equal? (replayed path cfg) kh)
  (check-equal? (count-recs path 'msg) 6))

(test-case "callers without a record sink (old path): a compacted turn is written as one compact record with the whole history"
  (define cfg (struct-copy config (default-config) [workdir (path->string dir)]))
  (define path (fresh-session-path (path->string dir)))
  (define sess (session-open! path cfg))
  (define before (for/fold ([st (make-initial-state cfg)]) ([i 6]) (state-append st (text-msg 'user (format "m~a" i)))))
  (for ([m (in-pvector (agent-state-history before))]) (session-append-msg! sess m))
  (define after (state-append (struct-copy agent-state before [history (pvector (text-msg 'user "SUMMARY") (text-msg 'user "m5"))]) (text-msg 'user "new")))
  (session-persist-turn! sess before after)
  (session-close! sess)
  (check-equal? (count-recs path 'compact) 1)
  (check-equal? (replayed path cfg) '("SUMMARY" "m5" "new")))
