#lang tstring racket
;; secrets.rkt — 密钥句柄（内核侧；与 GUI 的 src/gui/compute/secret-handle.rkt 同一算法、同一盐值文件）。
;; 句柄 = HMAC-SHA256(本机盐值, 原值) 前 8 个十六进制字符；盐值 {config-home}/secret-salt（32 字节随机，0600）。

(require racket/file racket/random file/sha1 (file "credentials.rkt"))

(define (salt-path) (build-path (config-home) "secret-salt"))

(define (load-or-create-salt)
  (define p (salt-path))
  (cond
    [(file-exists? p) (file->bytes p)]
    [else
     (make-directory* (config-home))
     (define s (crypto-random-bytes 32))
     (call-with-output-file p #:exists 'truncate (lambda (out) (write-bytes s out)))
     (file-or-directory-permissions p #o600)
     s])
) ; end define load-or-create-salt

(define (hmac-sha256 key msg)
  (define k (let ([k0 (if (> (bytes-length key) 64) (sha256-bytes key) key)])
              (bytes-append k0 (make-bytes (- 64 (bytes-length k0)) 0))))
  (define (xor-pad b) (apply bytes (for/list ([x (in-bytes k)]) (bitwise-xor x b))))
  (sha256-bytes (bytes-append (xor-pad #x5c) (sha256-bytes (bytes-append (xor-pad #x36) msg))))
) ; end define hmac-sha256

(define (secret-handle value)
  (substring (bytes->hex-string (hmac-sha256 (load-or-create-salt) (string->bytes/utf-8 value))) 0 8)
) ; end define secret-handle

(define (secret-text value) (string-append "⟦secret:" (secret-handle value) "⟧"))

;; ---------------------------------------------------------------- 已知密钥（宿主侧；不进沙箱、不进模型）
;; 来源：凭据文件里的全部值（实例 key、按环境变量名存的 key）；名字以 _API_KEY / _TOKEN / _SECRET 结尾的环境变量；
;; 本次运行里登记的（聊天里的密钥 chip 等）。→ (listof (vector 句柄 标签 原值))，按句柄去重。

(define session-secrets (make-hash))                  ; 句柄 → (cons 标签 原值)
;; 别名：原值 → 客户端算的句柄。GUI 连远端内核（无头模式）时两边盐值不同，同一把 key 在 GUI 与内核得到不同句柄；
;; 模型看到的是 GUI 写进提示的那个，所以内核按 GUI 给的句柄登记（工具输出过滤也用它）。
(define aliases (make-hash))

(define (handle-string? h) (and (string? h) (regexp-match? #px"^[0-9a-f]{8}$" h)))

;; #:handle：客户端给的句柄（格式不对、或已指向别的原值 → 不用，按本机算）
(define (register-secret! value label #:handle [alias #f])
  (define taken (and (handle-string? alias) (secret-value alias)))
  (define h (if (and (handle-string? alias) (or (not taken) (equal? taken value))) alias (secret-handle value)))
  (unless (equal? h (secret-handle value)) (hash-set! aliases value h))
  (hash-set! session-secrets h (cons label value))
  h
) ; end define register-secret!

(define (known-secrets)
  (define acc (make-hash))
  (define (add! value label [given #f])
    (when (and (string? value) (>= (string-length value) 8))
      (define h (or given (hash-ref aliases value #f) (secret-handle value)))
      (unless (hash-ref acc h #f) (hash-set! acc h (vector h label value)))))
  (define creds (with-handlers ([exn:fail? (lambda (_e) (hash))]) (load-credentials)))
  ;; 凭据文件里 secret:<句柄> 的条目：句柄就是名字里的那个（可能是客户端的别名，重启后仍有效）
  (for ([(k v) (in-hash creds)])
    (define m (regexp-match #px"^secret:([0-9a-f]{8})$" k))
    (when (and m (string? v)) (hash-set! aliases v (cadr m))))
  (for ([(k v) (in-hash creds)])
    (add! v (cond [(regexp-match #rx"^provider:([^:]+):(.*)$" k)
                   => (lambda (m) (format "provider ~a" (if (equal? (caddr m) "default") (cadr m) (format "~a[~a]" (cadr m) (caddr m)))))]
                  [else (format "key ~a" k)])))
  (for ([n (in-list (environment-variables-names (current-environment-variables)))])
    (define name (bytes->string/utf-8 n #\?))
    (when (regexp-match? #rx"(_API_KEY|_TOKEN|_SECRET)$" name)
      (add! (getenv name) (format "env ~a" name))))
  (for ([(h lv) (in-hash session-secrets)]) (add! (cdr lv) (car lv) h))
  (sort (hash-values acc) string<? #:key (lambda (v) (vector-ref v 1)))
) ; end define known-secrets

(define (secret-value handle)
  (for/first ([v (in-list (known-secrets))] #:when (equal? (vector-ref v 0) handle)) (vector-ref v 2))
) ; end define secret-value

;; 文本里出现的原值 → ⟦secret:句柄⟧（工具输出给模型 / 写会话之前过一遍）
(define (redact-secrets str)
  (if (string? str)
      (for/fold ([s str]) ([v (in-list (known-secrets))])
        (if (string-contains? s (vector-ref v 2)) (string-replace s (vector-ref v 2) (string-append "⟦secret:" (vector-ref v 0) "⟧")) s))
      str)
) ; end define redact-secrets

(provide secret-handle secret-text hmac-sha256 register-secret! known-secrets secret-value redact-secrets)
