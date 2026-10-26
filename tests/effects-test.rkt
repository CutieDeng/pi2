#lang tstring racket
;; effects-test.rkt — 幂等（方案 B：改数据的请求必须带 #:op）：效果账本 + racket_eval 的 http-request。
;; 假服务器记每个路径收到几次、收到的 Idempotency-Key；/drop 路径读完请求就断开（模拟中途失败）。

(require
 rackunit
 json
 racket/tcp
 racket/file
 (file "../src/model.rkt")
 (file "../src/session.rkt")
 (file "../src/effects.rkt")
 (file "../src/tools/racket-eval.rkt")
) ; end require

(define cfg-dir (make-temporary-file "pi2-eff-~a" 'directory))
(void (putenv "PI_CONFIG_HOME" (path->string cfg-dir)))
(define work (make-temporary-file "pi2-eff-ws-~a" 'directory))

(define hits (make-hash))                         ; 路径 → 次数
(define idem-keys '())
(define listener (tcp-listen 0 16 #t "127.0.0.1"))
(define port (let-values ([(_a p _b _c) (tcp-addresses listener #t)]) p))
(void (thread (lambda ()
                (let loop ()
                  (define-values (in out) (tcp-accept listener))
                  (thread (lambda ()
                            (define path (cadr (regexp-match #rx"^[A-Z]+ ([^ ]+)" (read-line in 'return-linefeed))))
                            (let hdrs ([len 0])
                              (define l (read-line in 'return-linefeed))
                              (cond [(or (eof-object? l) (string=? l ""))
                                     (read-bytes len in)
                                     (hash-update! hits path add1 0)
                                     (cond
                                       [(regexp-match? #rx"^/drop" path) (close-output-port out) (close-input-port in)]   ; 中途断开
                                       [else
                                        (define body (jsexpr->string (hasheq 'id (format "inv-~a" (hash-ref hits path)) 'path path)))
                                        (fprintf out "HTTP/1.1 201 Created\r\nContent-Type: application/json\r\nContent-Length: ~a\r\nConnection: close\r\n\r\n~a"
                                                 (bytes-length (string->bytes/utf-8 body)) body)
                                        (close-output-port out) (close-input-port in)])]
                                    [(regexp-match #rx"(?i:^idempotency-key: (.*))$" l) => (lambda (m) (set! idem-keys (cons (cadr m) idem-keys)) (hdrs len))]
                                    [(regexp-match #rx"(?i:^content-length: ([0-9]+))$" l) => (lambda (m) (hdrs (string->number (cadr m))))]
                                    [else (hdrs len)]))))
                  (loop)))))

(define (url path) (format "http://127.0.0.1:~a~a" port path))
(define (run code) (call-with-values (lambda () (run-racket code (path->string work))) list))
(define (fresh-ledger) (make-ledger (format "test-~a" (random 1000000))))

(test-case "POST without #:op is refused (nothing is sent) and the error teaches how to label"
  (parameterize ([current-effect-ledger (fresh-ledger)])
    (match-define (list ok? text) (run (format "(http-post ~s \"{}\")" (url "/a"))))
    (check-false ok?)
    (check-regexp-match #rx"need #:op" text)
    (check-equal? (hash-ref hits "/a" 0) 0)))

(test-case "same #:op runs once: later calls return the recorded result; Idempotency-Key is derived from (session, op)"
  (define lg (fresh-ledger))
  (parameterize ([current-effect-ledger lg])
    (set! idem-keys '())
    (match-define (list ok1 t1) (run (format "(define r (http-post ~s \"{\\\"n\\\":1}\" #:op \"create/1\")) (list (hash-ref (hash-ref r 'json) 'id) (hash-ref r 'replayed))" (url "/inv"))))
    (check-true ok1 t1)
    (check-regexp-match #rx"\\(\"inv-1\" #f\\)" t1)
    (match-define (list ok2 t2) (run (format "(define r (http-post ~s \"{\\\"n\\\":1, \\\"ts\\\": 2}\" #:op \"create/1\")) (list (hash-ref (hash-ref r 'json) 'id) (hash-ref r 'replayed))" (url "/inv"))))
    (check-true ok2 t2)
    (check-regexp-match #rx"\\(\"inv-1\" #t\\)" t2)                               ; 同一结果，没再发
    (check-regexp-match #rx"already done, returned the recorded result" t2)
    (check-equal? (hash-ref hits "/inv") 1)
    (check-equal? idem-keys (list (idempotency-key lg "create/1")))
    ;; 新 op = 新操作
    (run (format "(http-post ~s \"{}\" #:op \"create/2\")" (url "/inv")))
    (check-equal? (hash-ref hits "/inv") 2)
    ;; 同 op 不同 URL：冲突，不悄悄返回旧结果
    (match-define (list ok3 t3) (run (format "(http-post ~s \"{}\" #:op \"create/1\")" (url "/other"))))
    (check-false ok3)
    (check-regexp-match #rx"was already used for POST" t3)
    (check-equal? (cadr (run "(hash-ref (effect-status \"create/1\") 'state)")) "'done")
    (check-equal? (cadr (run "(effect-status \"never\")")) "'none")))

(test-case "an interrupted request leaves the op pending: no automatic resend; #:fresh sends again with the same key"
  (define lg (fresh-ledger))
  (parameterize ([current-effect-ledger lg])
    (match-define (list ok1 t1) (run (format "(http-post ~s \"{}\" #:op \"pay/9\")" (url "/drop"))))
    (check-false ok1)                                                             ; 连接断了：结果未知
    (check-equal? (effect-state (effect-ref lg "pay/9")) 'pending)
    (match-define (list ok2 t2) (run (format "(http-post ~s \"{}\" #:op \"pay/9\")" (url "/drop"))))
    (check-false ok2)
    (check-regexp-match #rx"outcome is unknown" t2)
    (check-equal? (hash-ref hits "/drop") 1)                                       ; 没有自动重发
    (set! idem-keys '())
    (run (format "(http-post ~s \"{}\" #:op \"pay/9\" #:fresh #t)" (url "/drop")))
    (check-equal? (hash-ref hits "/drop") 2)
    (check-equal? idem-keys (list (idempotency-key lg "pay/9")))))                 ; 显式重发：同一个幂等键

(test-case "the ledger is kept in the session file: a resumed session still returns the recorded result"
  (define p (build-path cfg-dir "sess.rktd"))
  (define s (session-open! p (default-config)))
  (session-append-msg! s (text-msg 'user "x"))
  (define lg (make-ledger "sess-A" #:append! (lambda (e) (session-append-effect! s e))))
  (parameterize ([current-effect-ledger lg])
    (run (format "(http-post ~s \"{}\" #:op \"order/77\")" (url "/orders"))))
  (session-close! s)
  (check-equal? (hash-ref hits "/orders") 1)
  ;; 恢复：从会话文件重建账本
  (define lg2 (make-ledger "sess-A" (session-effects p)))
  (parameterize ([current-effect-ledger lg2])
    (match-define (list ok t) (run (format "(hash-ref (http-post ~s \"{}\" #:op \"order/77\") 'replayed)" (url "/orders"))))
    (check-regexp-match #rx"already done, returned the recorded result \\(not sent again\\)\n#t$" t))
  (check-equal? (hash-ref hits "/orders") 1))

(tcp-close listener)
(delete-directory/files cfg-dir)
(delete-directory/files work)
