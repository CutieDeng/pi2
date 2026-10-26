#lang tstring racket
;; effects.rkt — 效果账本（幂等）：改数据的请求由模型用 #:op 标出「这是哪个操作」，框架按 (会话, op) 记账：
;;   同一个 op 在同一会话里只执行一次，再调用返回记录的结果（跨轮次、恢复会话都有效）；
;;   同 op 但方法 / URL 不同 → 冲突（模型把标签用错了，不悄悄返回旧结果）；
;;   发出前记 pending、收到响应记 done；看到停在 pending 的（崩溃 / 中断）→ 结果未知，不自动重发，要代码显式 #:fresh；
;;   服务端幂等键 = sha256(会话标识 | op) 前 32 位：重试时不变，模型不用自己生成随机串。
;; 账本可挂会话文件（append! 回调写 (rec 'effect …)），也可只在内存（测试、无会话时）。

(require file/sha1)

;; state：'pending | 'done；response：jsexpr hash（status headers body json）或 #f；body-fp：请求体的 sha256 十六进制
(struct effect (op state method url body-fp idem-key at response) #:prefab)

;; id：会话标识（幂等键的一部分）；table：op → 最新 effect；append!：#f 或 (effect → void)
(struct ledger (id table append! sema))

(define (make-ledger id [effects '()] #:append! [append! #f])
  (define t (make-hash))
  (for ([e (in-list effects)]) (hash-set! t (effect-op e) e))
  (ledger id t append! (make-semaphore 1))
) ; end define make-ledger

;; 当前账本：RPC 在轮线程里 parameterize；子 agent / 工具线程继承。没设时用进程内的内存账本
(define fallback-ledger (make-ledger "process"))
(define current-effect-ledger (make-parameter #f))
(define (active-ledger) (or (current-effect-ledger) fallback-ledger))

(define (idempotency-key lg op)
  (substring (bytes->hex-string (sha256-bytes (string->bytes/utf-8 (string-append (ledger-id lg) "|" op)))) 0 32)
) ; end define idempotency-key

(define (body-fingerprint body)
  (bytes->hex-string (sha256-bytes (cond [(bytes? body) body] [(string? body) (string->bytes/utf-8 body)] [else #""])))
) ; end define body-fingerprint

(define (effect-ref lg op) (call-with-semaphore (ledger-sema lg) (lambda () (hash-ref (ledger-table lg) op #f))))

(define (record! lg e)
  (call-with-semaphore (ledger-sema lg) (lambda () (hash-set! (ledger-table lg) (effect-op e) e)))
  (when (ledger-append! lg) ((ledger-append! lg) e))
) ; end define record!

;; 改数据的方法（要 #:op）
(define (unsafe-method? m) (not (member (string-upcase (format "~a" m)) '("GET" "HEAD" "OPTIONS"))))

;; 执行前的判定：→ (cons 'send 幂等键) | (cons 'replay effect) | (cons 'conflict effect) | (cons 'unknown effect)
(define (effect-decide lg op method url body fresh?)
  (define e (effect-ref lg op))
  (define m (string-upcase (format "~a" method)))
  (cond
    [(not e) (cons 'send (idempotency-key lg op))]
    [(not (and (equal? (effect-method e) m) (equal? (effect-url e) url))) (cons 'conflict e)]
    [fresh? (cons 'send (effect-idem-key e))]                       ; 显式重发：沿用同一个幂等键
    [(eq? (effect-state e) 'done) (cons 'replay e)]
    [else (cons 'unknown e)])
) ; end define effect-decide

(define (effect-begin! lg op method url body key)
  (record! lg (effect op 'pending (string-upcase (format "~a" method)) url (body-fingerprint body) key (current-seconds) #f))
) ; end define effect-begin!

;; 响应体超过上限只记摘要（再次命中时不能回放，要 #:fresh）
(define MAX-RECORDED-BODY 65536)
(define (effect-done! lg op response)
  (define e (effect-ref lg op))
  (define body (hash-ref response 'body ""))
  (define stored (if (> (string-length body) MAX-RECORDED-BODY)
                     (hash-set* response 'body #f 'json #f 'body_truncated #t)
                     response))
  (record! lg (struct-copy effect e [state 'done] [at (current-seconds)] [response stored]))
) ; end define effect-done!

(provide (struct-out effect) (struct-out ledger) make-ledger current-effect-ledger active-ledger
         idempotency-key body-fingerprint effect-ref unsafe-method? effect-decide effect-begin! effect-done!
         MAX-RECORDED-BODY)
