#lang tstring racket
;; provider.rkt — OpenAI 兼容流式 LLM 客户端（design.md §4.1 / §5.1）
;; 测试/默认后端：本地 LM Studio (http://localhost:1234/v1)。

(require
 json
 net/http-client
 net/url
 racket/async-channel
 racket/string
 racket/list
 (file "model.rkt")
 (file "stream.rkt")
) ; end require

;; ---------------------------------------------------------------- provider

(struct provider
  (name          ; string
   stream!-proc  ; (-> (listof message) (listof jsexpr) async-channel)
   cancel!-proc  ; (-> void)
  ) ; end fields
) ; end struct provider

(define (provider-stream! p msgs tool-specs)
  ((provider-stream!-proc p) msgs tool-specs)
) ; end define provider-stream!

(define (provider-cancel! p)
  ((provider-cancel!-proc p))
) ; end define provider-cancel!

;; ---------------------------------------------------- message -> OpenAI json

;; 一条内部 message 可能映射为多条 OpenAI 消息（tool-result → role:"tool"）
(define (message->openai-msgs m)
  (define blocks (message-blocks m))
  (define tool-results (filter tool-result-block? blocks))
  (cond
    [(pair? tool-results)
     (for/list ([tr (in-list tool-results)])
       (hasheq 'role "tool"
               'tool_call_id (tool-result-block-tool-use-id tr)
               'content (let ([c (tool-result-block-content tr)])
                          (if (string? c) c (jsexpr->string c))
                        ) ; end let
       ) ; end hasheq
     ) ; end for/list
    ] ; end tool-result case
    [(eq? (message-role m) 'assistant)
     (define tcs (message-tool-uses m))
     (define base
       (hasheq 'role "assistant"
               'content (message-text m)
       ) ; end hasheq
     ) ; end define base
     (list
      (if (null? tcs)
          base
          (hash-set base 'tool_calls
                    (for/list ([tc (in-list tcs)])
                      (hasheq 'id (tool-use-block-id tc)
                              'type "function"
                              'function
                              (hasheq 'name (tool-use-block-name tc)
                                      'arguments (jsexpr->string (tool-use-block-input tc))
                              ) ; end hasheq function
                      ) ; end hasheq
                    ) ; end for/list
          ) ; end hash-set
      ) ; end if
     ) ; end list
    ] ; end assistant case
    [else
     (list (hasheq 'role (symbol->string (message-role m))
                   'content (message-text m)
           ) ; end hasheq
     ) ; end list
    ] ; end else
  ) ; end cond
) ; end define message->openai-msgs

(define (build-request-body cfg msgs tool-specs #:stream? [stream? #t])
  (define sys
    (if (config-system-prompt cfg)
        (list (hasheq 'role "system" 'content (config-system-prompt cfg)))
        '()
    ) ; end if
  ) ; end define sys
  (define body
    (hasheq 'model (config-model cfg)
            'messages (append sys (append-map message->openai-msgs msgs))
            'max_tokens (config-max-tokens cfg)
            'temperature (config-temperature cfg)
            'stream stream?
    ) ; end hasheq
  ) ; end define body
  (define body+u
    (if stream?
        (hash-set body 'stream_options (hasheq 'include_usage #t))
        body
    ) ; end if
  ) ; end define body+u
  ;; 推理强度：非 off 时加 reasoning_effort（OpenAI o 系/gpt-5、Gemini、Grok 均识别；
  ;; LM Studio 等不识别的端点会忽略未知字段）。
  (define body+r
    (let ([eff (current-reasoning-effort)])
      (if (eq? eff 'off)
          body+u
          ;; OpenAI reasoning_effort 仅 low|medium|high；'max 无对应 → 钳到 "high"。
          (hash-set body+u 'reasoning_effort (symbol->string (if (eq? eff 'max) 'high eff)))
      ) ; end if
    ) ; end let
  ) ; end define body+r
  (if (pair? tool-specs)
      (hash-set body+r 'tools tool-specs)
      body+r
  ) ; end if
) ; end define build-request-body

;; ---------------------------------------------------------------- HTTP

(define (endpoint-parts endpoint)
  (define u (string->url endpoint))
  (values (url-host u)
          (or (url-port u) (if (equal? (url-scheme u) "https") 443 80))
          (equal? (url-scheme u) "https")
          (string-join
           (for/list ([pp (in-list (url-path u))]) (path/param-path pp))
           "/"
          ) ; end string-join
  ) ; end values
) ; end define endpoint-parts

;; 发起 POST，返回 (values status-code header-list body-input-port)
(define (post! cfg path body-jsexpr)
  (define-values (host port ssl? base-path) (endpoint-parts (config-endpoint cfg)))
  (define conn (http-conn-open host #:port port #:ssl? ssl?))
  (define headers
    (append
     (list "Content-Type: application/json")
     (if (config-api-key cfg)
         (list f"Authorization: Bearer {(config-api-key cfg)}")
         '()
     ) ; end if
    ) ; end append
  ) ; end define headers
  (define-values (status resp-headers in)
    (http-conn-sendrecv! conn f"/{base-path}{path}"
                         #:method #"POST"
                         #:headers headers
                         #:data (jsexpr->bytes body-jsexpr)
    ) ; end http-conn-sendrecv!
  ) ; end define-values
  (values (status-code status) resp-headers in)
) ; end define post!

(define (status-code status-bytes)
  (define s (bytes->string/utf-8 status-bytes))
  (define m (regexp-match #rx" ([0-9][0-9][0-9])" s))
  (if m (string->number (cadr m)) 0)
) ; end define status-code

;; ------------------------------------------------------------- 流式主流程

(define RETRY-DELAYS '(1 2 4))

;; 单次尝试：解析 SSE 流，事件发往 ch。返回 'ok 或 (list 'retryable status)。
(define (try-stream-once! cfg msgs tool-specs ch started?)
  (define-values (code _headers in)
    (post! cfg "/chat/completions" (build-request-body cfg msgs tool-specs))
  ) ; end define-values
  (cond
    [(and (>= code 200) (< code 300))
     (define acc (make-accumulator))
     (sse-pump! in
       (lambda (_type data)
         (when data
           (set-box! started? #t)
           (for ([d (in-list (accumulator-feed! acc data))])
             (async-channel-put ch d)
           ) ; end for
         ) ; end when
       ) ; end lambda
     ) ; end sse-pump!
     (define-values (msg stop-reason u) (accumulator-finish acc))
     (async-channel-put ch (evt:message (now-ms) msg))
     (async-channel-put ch (evt:turn-end (now-ms) stop-reason u))
     'ok
    ] ; end 2xx case
    [(or (= code 429) (>= code 500))
     (define body (port->string in))
     (log-warning f"provider: HTTP {code}, retryable: {body}")
     (list 'retryable code)
    ] ; end retryable case
    [else
     (define body (port->string in))
     (error 'provider f"HTTP {code}: {body}")
    ] ; end fatal case
  ) ; end cond
) ; end define try-stream-once!

(define (stream-with-retry! cfg msgs tool-specs ch)
  (define started? (box #f))
  (let loop ([delays RETRY-DELAYS])
    (define r
      (with-handlers ([exn:fail:network?
                       (lambda (e)
                         (log-warning f"provider: network error: {(exn-message e)}")
                         (list 'retryable 'network)
                       ) ; end lambda
                      ]) ; end handlers
        (try-stream-once! cfg msgs tool-specs ch started?)
      ) ; end with-handlers
    ) ; end define r
    (cond
      [(eq? r 'ok) (void)]
      [(unbox started?)
       ;; 已产出增量后不再重试，避免重复内容
       (error 'provider f"stream interrupted mid-flight ({(cadr r)})")
      ] ; end started case
      [(null? delays)
       (error 'provider f"retries exhausted ({(cadr r)})")
      ] ; end exhausted case
      [else
       (sleep (car delays))
       (loop (cdr delays))
      ] ; end retry case
    ) ; end cond
  ) ; end let loop
) ; end define stream-with-retry!

;; ---------------------------------------------------------------- 构造器

(define (make-openai-provider cfg)
  (define current-cust (box #f))
  (provider
   f"openai:{(config-model cfg)}"
   ;; stream!
   (lambda (msgs tool-specs)
     ;; 每次请求取当前生效的 config（运行时 /model 等切换即时生效）；未设时回退创建 cfg。
     (define ecfg (or (current-config) cfg))
     (define ch (make-async-channel))
     (define cust (make-custodian))
     (set-box! current-cust cust)
     (parameterize ([current-custodian cust])
       (thread
        (lambda ()
          (with-handlers ([exn:fail?
                           (lambda (e)
                             (async-channel-put ch (evt:error (now-ms) e #f))
                           ) ; end lambda
                          ]) ; end handlers
            (stream-with-retry! ecfg msgs tool-specs ch)
          ) ; end with-handlers
        ) ; end lambda
       ) ; end thread
     ) ; end parameterize
     ch
   ) ; end stream! lambda
   ;; cancel!
   (lambda ()
     (define c (unbox current-cust))
     (when c
       (custodian-shutdown-all c)
     ) ; end when
   ) ; end cancel! lambda
  ) ; end provider
) ; end define make-openai-provider

;; port->string 需要 racket/port
(require racket/port)

;; ---------------------------------------------------------------- provide

(provide
 (struct-out provider)
 provider-stream!
 provider-cancel!
 make-openai-provider
 message->openai-msgs
 build-request-body
) ; end provide
