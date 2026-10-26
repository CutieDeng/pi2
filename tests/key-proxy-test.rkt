#lang tstring racket
;; key-proxy-test.rkt — 本地签发凭证：假上游记下收到的认证头与路径，按块流回带 usage 的 SSE。
;; 检查：token 换成真 key（x-api-key / Bearer 两种头各自沿用）、路径原样、响应完整流回；
;; 不认识的 token 401、范围外的模型 403、花费到上限 402、请求数超限 429、吊销后 401；统计。

(require rackunit json racket/tcp net/http-client (file "../src/key-proxy.rkt"))

(define seen '())                                      ; 上游收到的 (路径 . 认证头)
(define up (tcp-listen 0 16 #t "127.0.0.1"))
(define up-port (let-values ([(_a p _b _c) (tcp-addresses up #t)]) p))
(void (thread (lambda ()
                (with-handlers ([exn:fail? void])                ; 测试结束关掉监听：接受线程退出
                (let loop ()
                  (define-values (in out) (tcp-accept up))
                  (thread (lambda ()
                            (define path (cadr (regexp-match #rx"^[A-Z]+ ([^ ]+)" (read-line in 'return-linefeed))))
                            (let hdrs ([len 0] [auth #f])
                              (define l (read-line in 'return-linefeed))
                              (cond
                                [(or (eof-object? l) (string=? l ""))
                                 (read-bytes len in)
                                 (set! seen (cons (cons path auth) seen))
                                 ;; 分三块流回，最后一块带 usage；不给长度，关连接结束
                                 (fprintf out "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nConnection: close\r\n\r\n")
                                 (for ([chunk (list "event: a\ndata: {\"x\":1}\n\n" "event: b\ndata: {\"x\":2}\n\n"
                                                    "event: c\ndata: {\"usage\":{\"input_tokens\":1000000,\"output_tokens\":0}}\n\n")])
                                   (write-string chunk out) (flush-output out) (sleep 0.02))
                                 (close-output-port out) (close-input-port in)]
                                [(regexp-match? #rx"(?i:^(x-api-key|authorization): )" l) (hdrs len l)]          ; 整行记下
                                [(regexp-match #rx"(?i:^content-length: ([0-9]+))$" l) => (lambda (m) (hdrs (string->number (cadr m)) auth))]
                                [else (hdrs len auth)]))))
                  (loop))))))

(define pport (proxy-start!))
(define (call token #:style [style 'x-api-key] #:model [model "deepseek-v4-pro"] #:path [path "/anthropic/v1/messages"])
  (define-values (status _h in)
    (http-sendrecv "127.0.0.1" path #:port pport #:method #"POST"
                   #:headers (list "Content-Type: application/json"
                                   (if (eq? style 'x-api-key) (format "x-api-key: ~a" token) (format "Authorization: Bearer ~a" token)))
                   #:data (jsexpr->string (hasheq 'model model 'stream #t))))
  (list (string->number (bytes->string/utf-8 (cadr (regexp-match #rx#" ([0-9]+)" status)))) (port->string in)))

(test-case "token swapped for the real key (same header style), path kept, stream passed through; scope, cap, revoke"
  (define tok (issue-grant! #:endpoint (format "http://127.0.0.1:~a/anthropic" up-port) #:key "sk-real-123"
                            #:model "deepseek-v4-pro" #:budget 1.0 #:label "test"))
  (check-equal? (grant-endpoint tok 4567) "http://127.0.0.1:4567/anthropic")
  (match-define (list c1 b1) (call tok))
  (check-equal? c1 200)
  (check-regexp-match #rx"\\{\"x\":1\\}.*\\{\"x\":2\\}.*input_tokens\":1000000" b1)
  (check-equal? (car seen) (cons "/anthropic/v1/messages" "x-api-key: sk-real-123"))
  (match-define (list c2 _b2) (call tok #:style 'bearer))
  (check-equal? c2 200)
  (check-equal? (cdar seen) "Authorization: Bearer sk-real-123")
  (check-false (ormap (lambda (s) (regexp-match? #rx"pi2p-" (cdr s))) seen))            ; token 从没到上游
  ;; 两次各 $0.55 → 到上限
  (match-define (list c3 b3) (call tok))
  (check-equal? c3 402)
  (check-regexp-match #rx"spending cap" b3)
  (check-equal? (length seen) 2)                                                     ; 被拒的没发到上游
  (define st (grant-stats tok))
  (check-equal? (hash-ref st 'requests) 2)
  (check-equal? (hash-ref st 'input_tokens) 2000000)
  (check-= (hash-ref st 'spent) 1.1 1e-9)
  ;; 范围外的模型、不认识的 token、吊销
  (define tok2 (issue-grant! #:endpoint (format "http://127.0.0.1:~a/anthropic" up-port) #:key "sk-real-123" #:model "deepseek-v4-flash" #:max-requests 1))
  (check-equal? (car (call tok2 #:model "deepseek-v4-pro")) 403)
  (check-equal? (car (call "pi2p-nope" #:model "deepseek-v4-flash")) 401)
  (check-equal? (car (call tok2 #:model "deepseek-v4-flash")) 200)
  (check-equal? (car (call tok2 #:model "deepseek-v4-flash")) 429)                   ; 请求数上限 1
  (revoke-grant! tok2)
  (check-equal? (car (call tok2 #:model "deepseek-v4-flash")) 401))

(tcp-close up)
