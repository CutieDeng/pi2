#lang tstring racket
;; key-proxy.rkt — 本地签发凭证：远端子 agent 的模型请求经本机转发，真 key 不离开本机。
;; 本机（父 agent 所在的内核进程）开一个只听 127.0.0.1 的 HTTP 转发；每次远端子 agent 运行签发一个临时 token（grant）：
;;   范围 = 一个上游（父会话当前供应商的端点）+ 一个模型 + 花费上限（美元估算）+ 请求数上限 + 有效期；运行结束即吊销。
;; 远端经 ssh 反向隧道（-R 远端端口 → 本机转发端口）访问它，用 token 当 key（x-api-key 或 Authorization: Bearer 都认）；
;; 转发时把 token 换成真 key（沿用远端用的那种头），请求路径原样拼到上游，响应以 chunked 原样流回。
;; 拒绝（JSON 错误体）：401 token 不认识 / 已吊销 / 过期；403 模型不在范围内；429 请求数超限；402 花费到上限。
;; 花费：扫描响应里的 usage 字段（input_tokens / output_tokens / prompt_tokens / completion_tokens，取每次响应里的最大值），
;; 按 pricing.rkt 估算。估算在响应结束后才知道 → 越过上限的那一次请求照常完成（与 §19 同一口径）。

(require racket/tcp net/http-client net/url json racket/random file/sha1 (file "pricing.rkt") (file "model.rkt"))

;; origin：(vector host port ssl?)；base-path：上游端点的路径部分（远端端点 = http://127.0.0.1:端口 + base-path）
(struct grant (token origin base-path key model budget expires max-requests label
               [requests #:mutable] [spent #:mutable] [in-tokens #:mutable] [out-tokens #:mutable] [revoked? #:mutable]))

(define grants (make-hash))                            ; token → grant
(define sema (make-semaphore 1))
(define-syntax-rule (locked body ...) (call-with-semaphore sema (lambda () body ...)))

(define listener #f)
(define the-port #f)
(define MAX-BODY (* 8 1024 1024))

;; ---------------------------------------------------------------- 签发 / 吊销

(define (endpoint-origin ep)
  (define u (string->url ep))
  (values (vector (url-host u) (or (url-port u) (if (equal? (url-scheme u) "https") 443 80)) (equal? (url-scheme u) "https"))
          (string-append "/" (string-join (for/list ([pp (in-list (url-path u))]) (path/param-path pp)) "/")))
) ; end define endpoint-origin

;; → token。endpoint：上游端点（父会话 config 的 endpoint）；key：真 key（可为 #f：本地端点不要 key）
(define (issue-grant! #:endpoint endpoint #:key key #:model model #:budget [budget #f] #:ttl [ttl 3600]
                      #:max-requests [max-requests 200] #:label [label ""])
  (define-values (origin base) (endpoint-origin endpoint))
  (define token (string-append "pi2p-" (bytes->hex-string (crypto-random-bytes 24))))
  (locked (hash-set! grants token (grant token origin (if (equal? base "/") "" base) key model budget
                                         (+ (current-seconds) ttl) max-requests label 0 0.0 0 0 #f)))
  token
) ; end define issue-grant!

(define (grant-stats token)
  (locked
   (define g (hash-ref grants token #f))
   (and g (hasheq 'requests (grant-requests g) 'spent (grant-spent g) 'input_tokens (grant-in-tokens g)
                  'output_tokens (grant-out-tokens g) 'revoked (grant-revoked? g) 'model (grant-model g))))
) ; end define grant-stats

;; 吊销：之后的请求一律 401；→ 最终统计
(define (revoke-grant! token)
  (define st (grant-stats token))
  (locked (let ([g (hash-ref grants token #f)]) (when g (set-grant-revoked?! g #t))))
  st
) ; end define revoke-grant!

;; 远端应配置的端点（port = 远端看到的端口：ssh -R 时是远端端口，本机直连时就是转发端口）
(define (grant-endpoint token port)
  (define g (locked (hash-ref grants token #f)))
  (and g (format "http://127.0.0.1:~a~a" port (grant-base-path g)))
) ; end define grant-endpoint

;; ---------------------------------------------------------------- 转发

(define (reply-error! out code msg)
  (define body (jsexpr->bytes (hasheq 'type "error" 'error (hasheq 'type "pi2_key_proxy" 'message (string-append "pi2 key proxy: " msg)))))
  (with-handlers ([exn:fail? void])
    (fprintf out "HTTP/1.1 ~a ~a\r\nContent-Type: application/json\r\nContent-Length: ~a\r\nConnection: close\r\n\r\n"
             code (case code [(401) "Unauthorized"] [(402) "Payment Required"] [(403) "Forbidden"] [(429) "Too Many Requests"]
                    [(502) "Bad Gateway"] [else "Bad Request"]) (bytes-length body))
    (write-bytes body out) (flush-output out))
) ; end define reply-error!

(define USAGE-RX #px"\"(input_tokens|output_tokens|prompt_tokens|completion_tokens)\"\\s*:\\s*([0-9]+)")

(define (handle! in out)
  (define req-line (read-line in 'return-linefeed))
  (define headers
    (let loop ([acc '()])
      (define l (read-line in 'return-linefeed))
      (if (or (eof-object? l) (string=? l "")) (reverse acc)
          (loop (let ([m (regexp-match #rx"^([^:]+):[ \t]*(.*)$" l)]) (if m (cons (cons (string-downcase (cadr m)) (caddr m)) acc) acc))))))
  (define (hdr k) (let ([p (assoc k headers)]) (and p (cdr p))))
  (define m (and (string? req-line) (regexp-match #rx"^([A-Z]+) ([^ ]+) HTTP/1\\.[01]$" req-line)))
  (define len (let ([v (hdr "content-length")]) (or (and v (string->number (string-trim v))) 0)))
  (cond
    [(not m) (reply-error! out 400 "bad request line")]
    [(> len MAX-BODY) (reply-error! out 400 "request body too large")]
    [else
     (define method (cadr m))
     (define path (caddr m))
     (define body (if (> len 0) (read-bytes len in) #""))
     (define style (cond [(hdr "x-api-key") 'x-api-key] [else 'bearer]))
     (define token (or (hdr "x-api-key")
                       (let ([a (hdr "authorization")]) (and a (let ([mm (regexp-match #rx"^[Bb]earer +(.+)$" a)]) (and mm (cadr mm)))))))
     (define req-model (with-handlers ([exn:fail? (lambda (_e) #f)]) (let ([j (bytes->jsexpr body)]) (and (hash? j) (hash-ref j 'model #f)))))
     ;; 检查 grant（全在锁里：计数先占上，失败的请求不占）
     (define ge
       (locked
        (define g (and token (hash-ref grants token #f)))
        (cond
          [(not g) (cons #f (cons 401 "unknown token"))]
          [(grant-revoked? g) (cons #f (cons 401 "token revoked (the sub-agent run has ended)"))]
          [(> (current-seconds) (grant-expires g)) (cons #f (cons 401 "token expired"))]
          [(and (string? req-model) (not (equal? req-model (grant-model g))))
           (cons #f (cons 403 (format "model ~a is outside this token's scope (allowed: ~a)" req-model (grant-model g))))]
          [(>= (grant-requests g) (grant-max-requests g)) (cons #f (cons 429 (format "request limit reached (~a)" (grant-max-requests g))))]
          [(and (grant-budget g) (>= (grant-spent g) (grant-budget g)))
           (cons #f (cons 402 (format "spending cap for this sub-agent reached: estimated $~a of $~a" (grant-spent g) (grant-budget g))))]
          [else (set-grant-requests! g (add1 (grant-requests g))) (cons g #f)])))
     (define g (car ge))
     (define err (cdr ge))
     (cond
       [err (reply-error! out (car err) (cdr err))]
       [else
        (define o (grant-origin g))
        (define upstream-headers
          (append (list "Content-Type: application/json")
                  (cond [(not (grant-key g)) '()]
                        [(eq? style 'x-api-key) (list (string-append "x-api-key: " (grant-key g)))]
                        [else (list (string-append "Authorization: Bearer " (grant-key g)))])
                  (for/list ([k (in-list '("anthropic-version" "anthropic-beta" "accept"))] #:when (hdr k))
                    (format "~a: ~a" k (hdr k)))))
        (with-handlers ([exn:fail? (lambda (e) (reply-error! out 502 (format "upstream: ~a" (exn-message e))))])
          (define conn (http-conn-open (vector-ref o 0) #:port (vector-ref o 1) #:ssl? (vector-ref o 2)))
          (define-values (status resp-headers rin)
            (http-conn-sendrecv! conn path #:method (string->bytes/utf-8 method) #:headers upstream-headers #:data body))
          (define ct (for/or ([h (in-list resp-headers)])
                       (let ([mm (regexp-match #rx#"^(?i:content-type):[ \t]*(.*)$" h)]) (and mm (cadr mm)))))
          (write-bytes status out)
          (write-bytes #"\r\n" out)
          (when ct (fprintf out "Content-Type: ~a\r\n" ct))
          (write-string "Transfer-Encoding: chunked\r\nConnection: close\r\n\r\n" out)
          (flush-output out)
          ;; 原样流回（按块 chunked），同时扫 usage
          (define buf (make-bytes 16384))
          (define tail #"")
          (define seen (make-hash))
          (let loop ()
            (define n (read-bytes-avail! buf rin))
            (unless (eof-object? n)
              (when (> n 0)
                (fprintf out "~x\r\n" n) (write-bytes buf out 0 n) (write-bytes #"\r\n" out) (flush-output out)
                (define text (bytes-append tail (subbytes buf 0 n)))
                (for ([mm (in-list (regexp-match* USAGE-RX text #:match-select values))])
                  (define k (bytes->string/utf-8 (cadr mm)))
                  (hash-update! seen k (lambda (v) (max v (string->number (bytes->string/utf-8 (caddr mm))))) 0))
                (set! tail (subbytes text (max 0 (- (bytes-length text) 64)))))
              (loop)))
          (write-bytes #"0\r\n\r\n" out) (flush-output out)
          (http-conn-close! conn)
          (define in-t (max (hash-ref seen "input_tokens" 0) (hash-ref seen "prompt_tokens" 0)))
          (define out-t (max (hash-ref seen "output_tokens" 0) (hash-ref seen "completion_tokens" 0)))
          (define usd (or (estimate-cost (grant-model g) (usage in-t out-t)) 0.0))
          (locked (set-grant-spent! g (+ (grant-spent g) usd))
                  (set-grant-in-tokens! g (+ (grant-in-tokens g) in-t))
                  (set-grant-out-tokens! g (+ (grant-out-tokens g) out-t))))])])
  (with-handlers ([exn:fail? void]) (close-output-port out))
  (with-handlers ([exn:fail? void]) (close-input-port in))
) ; end define handle!

;; 启动（每个进程一个，惰性）；→ 本机端口
(define (proxy-start!)
  (locked
   (unless listener
     (set! listener (tcp-listen 0 32 #t "127.0.0.1"))
     (set! the-port (let-values ([(_a p _b _c) (tcp-addresses listener #t)]) p))
     (define l listener)
     (thread (lambda ()
               (let loop ()
                 (define-values (in out) (tcp-accept l))
                 (thread (lambda () (handle! in out)))
                 (loop))))))
  the-port
) ; end define proxy-start!

(provide proxy-start! issue-grant! revoke-grant! grant-stats grant-endpoint)
