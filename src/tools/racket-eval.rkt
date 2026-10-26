#lang tstring racket
;; tools/racket-eval.rkt — racket_eval：在 pi2 进程内的沙箱里跑模型写的 Racket 代码（racket/base）。
;; 用途：用用户的密钥调 HTTP 接口而模型看不到密钥——进程内、按语义注入（design-gui.md §16.5）。
;;
;; 沙箱（racket/sandbox，实测见 tests/racket-eval-test.rkt）：
;;   可读工作目录下的文件；不能写文件、不能起子进程、不能直接联网；环境变量为空（getenv 拿不到 key）；
;;   ffi/unsafe 与 racket/unsafe/ops 的受保护绑定不可用（代码检查器拒绝）；内存 / 时间有上限。
;; 注入的能力（宿主侧实现）：
;;   (secret "句柄") → 不透明值，**只装句柄**；打印成 #<secret 句柄>。原值只在宿主侧组请求时取，沙箱里永远拿不到。
;;   (bearer s) / (secret-append "前缀" s …) → 拼接值（同样不透明）。
;;   (http-request 方法 url #:headers (list (cons 名 值) …) #:body 字符串) → (hash 'status 'headers 'body 'json)（'json：正文是 JSON 时已解析）；
;;     供应商帐号的 key 只能发往该供应商档案的 API 主机（deepseek 的 key 只能发 api.deepseek.com），发往别处直接报错；
;;     header 值可以是字符串、secret 或拼接值；请求由宿主在宿主的安全守卫下发出。http-get / http-post 是便捷形式。
;; 输出（打印的值 + 标准输出 + 每次 HTTP 请求的审计行）在给模型之前把出现的原值换成 ⟦secret:句柄⟧。
;; 权限级别 dangerous：每次调用都在权限卡片上显示整段代码，由用户确认。

(require
 racket/sandbox
 net/http-client
 net/url
 json
 (file "../tool.rkt")
 (file "../secrets.rkt")
 (file "../providers.rkt")              ; 供应商帐号的 key 只能发往该供应商的 API 主机
 (file "../effects.rkt")                ; 效果账本：改数据的请求按 #:op 只执行一次
) ; end require

;; 密钥绑定的主机：标签是「provider 名[帐号]」的 → 档案端点的主机；其它（环境变量、手动登记）→ #f（不限）
(define (bound-host label)
  (define m (and (string? label) (regexp-match #rx"^provider ([^[]+)" label)))
  (define prof (and m (profile-by-name (cadr m))))
  (and prof (url-host (string->url (provider-profile-endpoint prof))))
) ; end define bound-host

(define (secret-label handle)
  (for/first ([v (in-list (known-secrets))] #:when (equal? (vector-ref v 0) handle)) (vector-ref v 1))
) ; end define secret-label

(define MAX-OUTPUT 16000)

;; 不透明值：只有句柄；拼接值：字符串与 secret 的序列
(struct secret-ref (handle)
  #:methods gen:custom-write [(define (write-proc v out _mode) (fprintf out "#<secret ~a>" (secret-ref-handle v)))])
(struct secret-concat (parts)
  #:methods gen:custom-write [(define (write-proc v out _mode) (fprintf out "#<secret-value ~a>" (length (secret-concat-parts v))))])

(define host-guard (current-security-guard))           ; 模块实例化时的守卫：宿主侧联网用

;; 宿主侧：值 → 真正的字符串（只在组请求时调用）
(define (resolve-value v)
  (cond [(string? v) v]
        [(secret-ref? v) (or (secret-value (secret-ref-handle v))
                             (error 'racket_eval f"unknown secret handle: {(secret-ref-handle v)}"))]
        [(secret-concat? v) (apply string-append (map resolve-value (secret-concat-parts v)))]
        [else (error 'racket_eval "header values must be strings or secrets")])
) ; end define resolve-value

(define (handles-in v)
  (cond [(secret-ref? v) (list (secret-ref-handle v))]
        [(secret-concat? v) (append-map handles-in (secret-concat-parts v))]
        [else '()])
) ; end define handles-in

;; 一次调用的能力（审计行记在 audit 盒子里）。能力函数被沙箱里的代码调用时，带的是沙箱的参数化
;; （安全守卫、空的环境变量）：读凭据文件、读环境变量、联网都要先切回宿主的（host-env：调用工具时的环境变量）
(define (make-capabilities audit host-env lg)
  (define (host thunk)
    (parameterize ([current-security-guard host-guard] [current-environment-variables host-env]) (thunk)))
  (define (secret h) (host (lambda () (secret* h))))
  (define (secret* h)
    (unless (string? h) (raise-argument-error 'secret "string?" h))
    (unless (secret-value h)
      (define avail (string-join (map (lambda (v) (vector-ref v 0)) (known-secrets)) ", "))
      (error 'secret f"unknown secret handle {h}; available: {avail}"))
    (secret-ref h))
  (define (secret-append . parts) (secret-concat parts))
  (define (bearer s) (secret-concat (list "Bearer " s)))
  (define (http-request method url #:headers [headers '()] #:body [body #f] #:op [op #f] #:fresh [fresh? #f])
    (define m (string-upcase (format "~a" method)))
    (define u (string->url url))
    (define ssl? (equal? (url-scheme u) "https"))
    (define port (or (url-port u) (if ssl? 443 80)))
    (define path (let ([p (string-join (map (lambda (pp) (path/param-path pp)) (url-path u)) "/")]
                       [q (url-query u)])
                   (string-append "/" p (if (null? q) "" (string-append "?" (alist->form-urlencoded* q))))))
    (define used (remove-duplicates (append-map (lambda (h) (handles-in (cdr h))) headers)))
    (define used-note (if (null? used) "" (string-append " (secrets: " (string-join used ", ") ")")))
    ;; 供应商帐号的 key 只能发往该供应商的主机（防止代码把 key 发到别处）
    (host (lambda ()
            (for ([h (in-list used)])
              (define bh (bound-host (secret-label h)))
              (when (and bh (not (equal? bh (url-host u))))
                (error 'http-request f"secret {h} may only be sent to {bh}, not {(url-host u)}")))))
    ;; 改数据的请求：模型用 #:op 标出「这是哪个操作」，框架按 (会话, op) 记账（effects.rkt）
    (define unsafe? (unsafe-method? m))
    (when (and unsafe? (not (and (string? op) (regexp-match? #rx"[^ ]" op))))
      (error 'http-request
             (string-append m " requests change remote state and need #:op \"<label>\": a short name for this operation, "
                            "e.g. #:op \"create-invoice/order-123\". Reuse the same label when retrying the same operation "
                            "(it then runs once; later calls return the recorded result); use a new label for a new operation. "
                            "(effect-status label) shows what happened.")))
    (define decision (and unsafe? (host (lambda () (effect-decide lg op m url body fresh?)))))
    (define op-note (if unsafe? (format " [op ~a~a]" op (if fresh? ", fresh" "")) ""))
    (define line (format "http ~a ~a://~a~a~a~a" m (url-scheme u) (url-host u) path used-note op-note))
    (case (and decision (car decision))
      [(replay)
       (define e (cdr decision))
       (define r (effect-response e))
       (when (hash-ref r 'body_truncated #f)
         (error 'http-request f"op {op} already ran, but its result was too large to keep; pass #:fresh #t to send it again"))
       (set-box! audit (cons (string-append line f" — already done, returned the recorded result (not sent again)") (unbox audit)))
       (hash-set* r 'op op 'replayed #t)]
      [(conflict)
       (define e (cdr decision))
       (error 'http-request f"op {op} was already used for {(effect-method e)} {(effect-url e)}; this request is {m} {url}. Use a new #:op for a different operation.")]
      [(unknown)
       (error 'http-request f"op {op} was started earlier but its outcome is unknown (interrupted or failed mid-way). Check the remote state (e.g. with a GET), then pass #:fresh #t to send it again with the same Idempotency-Key.")]
      [else
       (define idem (and decision (cdr decision)))
       (set-box! audit (cons line (unbox audit)))
       (when unsafe? (host (lambda () (effect-begin! lg op m url body idem))))   ; 发出前记 pending：崩在中间也知道「结果未知」
       (define headers* (if (and idem (not (for/or ([h (in-list headers)]) (string-ci=? (format "~a" (car h)) "Idempotency-Key"))))
                            (append headers (list (cons "Idempotency-Key" idem)))
                            headers))
       (define resp
         (host                                                                 ; 回到宿主守卫 / 环境发请求
          (lambda ()
            (define-values (status hs in)
              (http-sendrecv (url-host u) path #:ssl? ssl? #:port port #:method m
                             #:headers (for/list ([h (in-list headers*)]) (format "~a: ~a" (car h) (resolve-value (cdr h))))
                             #:data body))
            (define code (let ([mm (regexp-match #rx#" ([0-9][0-9][0-9])" status)]) (if mm (string->number (bytes->string/utf-8 (cadr mm))) 0)))
            (define resp-body (bytes->string/utf-8 (port->bytes in) #\?))     ; 不叫 body：会遮住请求的 #:body 参数
            (hash 'status code
                  'headers (for/list ([h (in-list hs)]) (bytes->string/utf-8 h #\?))
                  'body resp-body
                  ;; 正文是 JSON 就解析好（实测：模型解析 JSON 时反复出错、每次重试都重发请求）
                  'json (with-handlers ([exn:fail? (lambda (_e) #f)]) (string->jsexpr resp-body))))))
       (when unsafe? (host (lambda () (effect-done! lg op resp))))
       (if unsafe? (hash-set* resp 'op op 'replayed #f) resp)]))
  (define (http-get url #:headers [headers '()]) (http-request "GET" url #:headers headers))
  (define (http-post url body #:headers [headers '()] #:op [op #f] #:fresh [fresh? #f])
    (http-request "POST" url #:headers headers #:body body #:op op #:fresh fresh?))
  ;; 查账本：→ 'none | (hash 'state 'pending|'done 'method 'url 'at 'response)
  (define (effect-status op)
    (define e (host (lambda () (effect-ref lg op))))
    (if e
        (hash 'state (effect-state e) 'method (effect-method e) 'url (effect-url e) 'at (effect-at e) 'response (effect-response e))
        'none))
  (list (cons 'secret secret) (cons 'secret-append secret-append) (cons 'bearer bearer)
        (cons 'http-request http-request) (cons 'http-get http-get) (cons 'http-post http-post)
        (cons 'effect-status effect-status))
) ; end define make-capabilities

(define (alist->form-urlencoded* q)
  (string-join (for/list ([kv (in-list q)]) (if (cdr kv) (format "~a=~a" (car kv) (cdr kv)) (format "~a" (car kv)))) "&")
) ; end define alist->form-urlencoded*

(define (clip s) (if (> (string-length s) MAX-OUTPUT) (string-append (substring s 0 MAX-OUTPUT) "\n…(truncated)") s))

;; 跑一段代码：→ (values 成功? 文本)
(define (run-racket code workdir #:time [secs 20] #:memory [mb 128])
  (define audit (box '()))
  (define host-env (current-environment-variables))
  (define ev
    (parameterize ([sandbox-path-permissions (list (list 'read (path->complete-path workdir)))]
                   [sandbox-network-guard (lambda args (error 'racket_eval "direct network access is not allowed; use http-request"))]
                   [sandbox-make-environment-variables (lambda () (make-environment-variables))]
                   [sandbox-memory-limit mb]
                   [sandbox-eval-limits (list secs mb)]
                   [sandbox-output 'string]
                   [sandbox-error-output 'string]
                   [current-directory workdir])
      (make-evaluator 'racket/base)))
  (call-in-sandbox-context ev
    (lambda () (for ([c (in-list (make-capabilities audit host-env (active-ledger)))]) (namespace-set-variable-value! (car c) (cdr c)))))
  (define-values (ok? result-text)
    (with-handlers ([exn:fail? (lambda (e) (values #f (format "error: ~a" (exn-message e))))])
      (define vs (call-with-values (lambda () (ev code)) list))
      (values #t (string-join (for/list ([v (in-list vs)] #:unless (void? v)) (format "~v" v)) "\n"))))
  (define out (get-output ev))
  (define err (get-error-output ev))
  (kill-evaluator ev)
  (define text
    (string-join (filter (lambda (s) (not (string=? s "")))
                         (list (string-join (reverse (unbox audit)) "\n")
                               (if (string=? out "") "" (string-append "stdout:\n" out))
                               (if (string=? err "") "" (string-append "stderr:\n" err))
                               result-text))
                 "\n"))
  (values ok? (clip (redact-secrets (if (string=? text "") "(no output)" text))))
) ; end define run-racket

(define (secrets-line)
  (define ks (known-secrets))
  (if (null? ks)
      "No secrets are registered."
      (string-append "Available secrets (handle = what it is): "
                     (string-join (for/list ([v (in-list ks)]) (format "~a = ~a" (vector-ref v 0) (vector-ref v 1))) "; ") "."))
) ; end define secrets-line

(struct racket-eval-tool ()
  #:methods gen:tool
  [(define (tool-name _t) "racket_eval")
   (define (tool-permission-level _t) 'dangerous)
   (define (tool-spec _t)
     (function-spec "racket_eval"
                    (string-append
                     "Run Racket code (#lang racket/base) in a sandbox inside pi2. Files under the working directory are readable; "
                     "no file writes, no subprocesses, no direct network. Use it to call HTTP APIs with the user's secrets without "
                     "ever seeing them: (http-request \"POST\" url #:headers (list (cons \"Authorization\" (bearer (secret \"HANDLE\")))) "
                     "#:body json-string) returns a hash with 'status, 'headers, 'body (string) and 'json (the parsed body as hasheq/list, or #f); "
                     "also http-get, http-post. JSON object keys are symbols: (hash-ref (hash-ref r 'json) 'name). "
                     "POST/PUT/PATCH/DELETE need #:op \"label\" naming the operation (e.g. #:op \"create-invoice/order-123\"): the same label runs "
                     "once per session and later calls return the recorded result, so reuse it when retrying and use a new label for a new "
                     "operation; (effect-status label) shows what happened. (secret \"HANDLE\") is an "
                     "opaque value (printing it shows only the handle); combine with (bearer s) or (secret-append \"prefix\" s). "
                     "Returns printed results, stdout and one audit line per HTTP request. "
                     (secrets-line))
                    (hasheq 'code (hasheq 'type "string" 'description "Racket expressions to evaluate (racket/base; require other libraries as needed)"))
                    (list "code")))
   (define (tool-run _t input ctx)
     (define code (input-str input 'code))
     (cond
       [(not code) (err-outcome "missing required parameter: code")]
       [else
        (define-values (ok? text) (run-racket code (tool-ctx-workdir ctx)))
        (if ok? (ok-outcome text #:display "racket_eval") (err-outcome text))]))])

(define (make-racket-eval-tool) (racket-eval-tool))

(provide make-racket-eval-tool run-racket)
