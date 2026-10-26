#lang tstring racket
;; wire.rkt — 调用格式自动适配（design-gui.md §15.2；用户不感知，调试面板可见）。
;; OpenAI 形状的端点有两种格式：Chat Completions（旧，几乎所有兼容端点都支持）与 Responses（新，OpenAI 官方首选；
;; 有的模型只支持它）。Anthropic Messages 由供应商档案的 kind 决定，不在这里。
;;
;;   默认：api.openai.com → responses；其它端点 → chat。
;;   出错且还没吐出内容、错误说明格式不对 → 换另一种格式重发同一请求；成功的格式按「端点|模型」缓存（{config-home}/wire-cache.rktd）。
;;     chat → responses：「only supported in v1/responses」「use the responses api」之类
;;     responses → chat：/responses 返回 404 / 405，或「unknown url / not found / unsupported endpoint」

(require
 racket/async-channel
 racket/file
 (file "model.rkt")
 (file "provider.rkt")
 (file "provider-responses.rkt")
 (file "credentials.rkt")               ; config-home
) ; end require

(define (cache-path) (build-path (config-home) "wire-cache.rktd"))
(define cache #f)
(define cache-sema (make-semaphore 1))

(define (wire-cache)
  (unless cache
    (define raw (with-handlers ([exn:fail? (lambda (_e) (hash))])
                  (if (file-exists? (cache-path))
                      (call-with-input-file (cache-path) (lambda (in) (parameterize ([read-accept-reader #f]) (read in))))
                      (hash))))
    (set! cache (make-hash (if (hash? raw) (for/list ([(k v) (in-hash raw)] #:when (and (string? k) (memq v '(chat responses)))) (cons k v)) '()))))
  cache
) ; end define wire-cache

(define (reload-wire-cache!) (set! cache #f))

(define (cache-key endpoint model) (format "~a|~a" endpoint model))

(define (default-wire endpoint)
  (if (and (string? endpoint) (regexp-match? #rx"//api[.]openai[.]com" endpoint)) 'responses 'chat)
) ; end define default-wire

;; 当前对「端点|模型」用哪种格式
(define (wire-for endpoint model)
  (call-with-semaphore cache-sema (lambda () (hash-ref (wire-cache) (cache-key endpoint model) (lambda () (default-wire endpoint)))))
) ; end define wire-for

(define (remember-wire! endpoint model w)
  (define changed?
    (call-with-semaphore cache-sema
      (lambda ()
        (define k (cache-key endpoint model))
        (and (not (eq? (hash-ref (wire-cache) k #f) w)) (begin (hash-set! (wire-cache) k w) #t)))))
  (when changed?
    (with-handlers ([exn:fail? void])
      (make-directory* (config-home))
      (define tmp (path-add-extension (cache-path) #".tmp"))
      (call-with-output-file tmp #:exists 'truncate (lambda (out) (write (for/hash ([(k v) (in-hash (wire-cache))]) (values k v)) out)))
      (rename-file-or-directory tmp (cache-path) #t)))
) ; end define remember-wire!

;; 错误说明当前格式不对 → 应改用的格式；否则 #f
(define (wire-mismatch msg current)
  (define m (string-downcase msg))
  (case current
    [(chat) (and (regexp-match? #rx"v1/responses|responses api|only supported in|use the responses|not supported in v1/chat/completions" m)
                 'responses)]
    [(responses) (and (or (regexp-match? #rx"http 40[45]" m)
                          (regexp-match? #rx"unknown url|unknown endpoint|unsupported endpoint|invalid url|no such endpoint|route not found" m))
                      'chat)]
    [else #f])
) ; end define wire-mismatch

;; OpenAI 形状端点的 provider：每个请求按缓存选格式；格式不对就换另一种重发
(define (make-openai-auto-provider cfg)
  (define chat (make-openai-provider cfg))
  (define resp (make-responses-provider cfg))
  (define forwarders (make-stream-set))
  (provider
   f"openai-auto:{(config-model cfg)}"
   (lambda (msgs tools)
     (define ecfg (or (current-config) cfg))
     (define ep (config-endpoint ecfg))
     (define model (config-model ecfg))
     (define eff (current-reasoning-effort))
     (define out (make-async-channel))
     (stream-set-spawn! forwarders
      (lambda ()
        (let attempt ([w (wire-for ep model)] [tried '()])
          (define p (if (eq? w 'responses) resp chat))
          (define ch (parameterize ([current-config ecfg] [current-reasoning-effort eff]) (provider-stream! p msgs tools)))
          (let fwd ([saw? #f])
            (define e (sync ch))
            (cond
              [(evt:error? e)
               (define alt (and (not saw?) (wire-mismatch (exn-message (evt:error-exn e)) w)))
               (if (and alt (not (memq alt tried)))
                   (begin (log-info f"wire: {ep} {model}: {w} → {alt}") (attempt alt (cons w tried)))
                   (async-channel-put out e))]
              [(evt:turn-end? e) (remember-wire! ep model w) (async-channel-put out e)]   ; 成功才记
              [else (async-channel-put out e) (fwd (or saw? (evt:delta? e) (evt:message? e)))])))))
     out)
   (lambda () (stream-set-cancel-all! forwarders) (provider-cancel! chat) (provider-cancel! resp)))   ; 两种格式的在途请求都取消
) ; end define make-openai-auto-provider

(provide make-openai-auto-provider wire-for remember-wire! wire-mismatch default-wire reload-wire-cache!)
