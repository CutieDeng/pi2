#lang racket/base
;; secret-handle.rkt — 密钥句柄（GUI 侧；内核 src/secrets.rkt 用同一算法、同一盐值文件，两边算出的句柄相同）。
;; 句柄 = HMAC-SHA256(本机盐值, 原值) 的前 8 个十六进制字符。盐值 32 字节随机，{config-home}/secret-salt（0600）；
;; 没有盐值推不回原值；同一把 key 在本机得到同一个句柄（日志里可以对照），换一台机器不同。
;; 文本形式：⟦secret:xxxxxxxx⟧（日志、请求里用；不含原值、不含长度）。

(require racket/file racket/random "gui-settings.rkt")

(define (bytes->hex bs) (apply string-append (for/list ([b (in-bytes bs)]) (string-append (if (< b 16) "0" "") (number->string b 16)))))

(define (salt-path) (build-path (gui-config-home) "secret-salt"))

(define (load-or-create-salt)
  (define p (salt-path))
  (cond
    [(file-exists? p) (file->bytes p)]
    [else
     (make-directory* (gui-config-home))
     (define s (crypto-random-bytes 32))
     (call-with-output-file p #:exists 'truncate (lambda (out) (write-bytes s out)))
     (file-or-directory-permissions p #o600)
     s])
) ; end define load-or-create-salt

;; HMAC-SHA256（块长 64）
(define (hmac-sha256 key msg)
  (define k (let ([k0 (if (> (bytes-length key) 64) (sha256-bytes key) key)])
              (bytes-append k0 (make-bytes (- 64 (bytes-length k0)) 0))))
  (define (xor-pad b) (apply bytes (for/list ([x (in-bytes k)]) (bitwise-xor x b))))
  (sha256-bytes (bytes-append (xor-pad #x5c) (sha256-bytes (bytes-append (xor-pad #x36) msg))))
) ; end define hmac-sha256

(define (secret-handle value)
  (substring (bytes->hex (hmac-sha256 (load-or-create-salt) (string->bytes/utf-8 value))) 0 8)
) ; end define secret-handle

(define (secret-text value) (string-append "⟦secret:" (secret-handle value) "⟧"))

(provide secret-handle secret-text hmac-sha256)
