#lang tstring racket
;; rpc-effects-test.rkt — RPC 下的效果账本：同一会话里两轮用同一个 #:op 只执行一次；新会话有自己的账本；切回原会话仍按记录返回。

(require
 rackunit json racket/async-channel racket/file racket/tcp
 (file "../src/model.rkt") (file "../src/event.rkt") (file "../src/provider.rkt") (file "../src/tool.rkt")
 (file "../src/permission.rkt") (file "../src/loop.rkt") (file "../src/session.rkt") (file "../src/plugin.rkt")
 (file "../src/providers.rkt") (file "../src/rpc.rkt") (file "../src/tools/racket-eval.rkt")
) ; end require

(define cfg-dir (make-temporary-file "pi2-rpceff-~a" 'directory))
(void (putenv "PI_CONFIG_HOME" (path->string cfg-dir)))

(define posts 0)
(define listener (tcp-listen 0 8 #t "127.0.0.1"))
(define port (let-values ([(_a p _b _c) (tcp-addresses listener #t)]) p))
(void (thread (lambda ()
                (let loop ()
                  (define-values (in out) (tcp-accept listener))
                  (thread (lambda ()
                            (let hdrs ([len 0])
                              (define l (read-line in 'return-linefeed))
                              (cond [(or (eof-object? l) (string=? l ""))
                                     (read-bytes len in)
                                     (set! posts (add1 posts))
                                     (define body (format "{\"id\":\"o~a\"}" posts))
                                     (fprintf out "HTTP/1.1 201 Created\r\nContent-Length: ~a\r\nConnection: close\r\n\r\n~a" (string-length body) body)
                                     (close-output-port out) (close-input-port in)]
                                    [(regexp-match #rx"(?i:^content-length: ([0-9]+))$" l) => (lambda (m) (hdrs (string->number (cadr m))))]
                                    [else (hdrs len)]))))
                  (loop)))))

;; 脚本模型：每轮先调一次 racket_eval（POST，op = order/1），拿到结果后回答
(define CODE (format "(hash-ref (hash-ref (http-post \"http://127.0.0.1:~a/orders\" \"{}\" #:op \"order/1\") 'json) 'id)" port))
(define (scripted)
  (provider "s"
            (lambda (msgs _tools)
              (define ch (make-async-channel))
              (define last-m (last msgs))
              (thread (lambda ()
                        (cond
                          [(ormap tool-result-block? (message-blocks last-m))
                           (define r (tool-result-block-content (findf tool-result-block? (message-blocks last-m))))
                           (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant (format "result: ~a" r))))
                           (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 1 1)))]
                          [else
                           (async-channel-put ch (evt:message (now-ms) (message 'assistant (list (tool-use-block (format "c~a" (random 100000)) "racket_eval" (hasheq 'code CODE))))))
                           (async-channel-put ch (evt:turn-end (now-ms) "tool_calls" (usage 1 1)))])))
              ch)
            void))

(test-case "same #:op across turns runs once per session; a new session has its own ledger; switching back keeps the record"
  (define dir (make-temporary-file "pi2-rpceff-ws-~a" 'directory))
  (define cfg (struct-copy config (default-config) [workdir (path->string dir)] [permission-mode 'yolo]))
  (define host (make-plugin-host))
  (define d (make-deps #:provider (scripted) #:registry (make-registry (list (make-racket-eval-tool)))
                       #:bus (make-bus) #:policy (make-policy cfg) #:plugin-host host))
  (define sess-path (fresh-session-path (path->string dir)))
  (define sess (session-open! sess-path cfg))
  (define-values (in-r in-w) (make-pipe))
  (define-values (out-r out-w) (make-pipe))
  (thread (lambda () (parameterize ([current-input-port in-r] [current-output-port out-w])
                       (run-rpc! d (make-initial-state cfg) sess #:plugin-host host #:fetch-models (lambda (_e) #f)))
            (close-output-port out-w)))
  (define (send! j) (write-string (jsexpr->string j) in-w) (newline in-w) (flush-output in-w))
  (define (expect! type)
    (let loop ()
      (define j (string->jsexpr (sync/timeout 10 (read-line-evt out-r 'linefeed))))
      (if (equal? (hash-ref j 'type #f) type) j (loop))))
  (define (turn!)
    (send! (hasheq 'type "prompt" 'text "place the order"))
    (define te (expect! "tool_end"))
    (expect! "turn_complete")
    (hash-ref te 'content))
  (expect! "ready")
  (check-regexp-match #rx"o1" (turn!))
  (check-regexp-match #rx"already done.*\n\"o1\"" (turn!))                       ; 第 2 轮：返回记录的结果
  (check-equal? posts 1)
  (send! (hasheq 'type "new_session"))
  (expect! "session")
  (check-regexp-match #rx"o2" (turn!))                                           ; 新会话：自己的账本，真正执行
  (check-equal? posts 2)
  (send! (hasheq 'type "resume" 'path (path->string sess-path)))
  (expect! "session")
  (check-regexp-match #rx"already done.*\n\"o1\"" (turn!))                       ; 切回原会话：按记录返回
  (check-equal? posts 2)
  (send! (hasheq 'type "shutdown"))
  (expect! "bye"))

(tcp-close listener)
(delete-directory/files cfg-dir)
