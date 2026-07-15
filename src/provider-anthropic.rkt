#lang tstring racket
;; provider-anthropic.rkt — Anthropic 原生 Messages API 流式客户端。
;; 与内核 provider.rkt（OpenAI 兼容）并列、互不侵入；线路格式/鉴权头不同：
;;   POST {endpoint}/v1/messages, headers: x-api-key / anthropic-version。
;; 复用 stream.rkt 的通用 SSE 泵，另写 Anthropic 事件累加器。
;; provider 每次请求读 current-config 取 live 的 model/endpoint/key（同 openai 路径）。

(require
 json
 net/http-client
 net/url
 racket/async-channel
 racket/string
 racket/list
 racket/port
 (file "model.rkt")
 (file "provider.rkt")                  ; provider 结构 + provider-stream!/cancel!
 (file "stream.rkt")                    ; sse-pump!
) ; end require

(define ANTHROPIC-VERSION "2023-06-01")

;; json null → dflt
(define (jsref j key [dflt #f])
  (define v (if (hash? j) (hash-ref j key dflt) dflt))
  (if (eq? v 'null) dflt v)
) ; end define jsref

;; ---------------------------------------------- 内部 message → Anthropic json

;; assistant：thinking 块（须首位、带签名回传）+ text 块 + tool_use 块。
;; 扩展思考 + 工具调用时，Anthropic 要求把带签名的 thinking 块原样回传，否则下一轮 400；
;; 故只回传**有签名**的 thinking 块（无签名者跳过，避免 API 拒绝）。
(define (assistant->content m)
  (define blocks (message-blocks m))
  (append
   (for/list ([b (in-list blocks)] #:when (thinking-block? b)
              #:when (and (thinking-block-signature b)
                          (non-empty-string? (thinking-block-signature b))))
     (hasheq 'type "thinking"
             'thinking (thinking-block-text b)
             'signature (thinking-block-signature b)))
   (for/list ([b (in-list blocks)] #:when (text-block? b)
              #:when (non-empty-string? (text-block-text b)))
     (hasheq 'type "text" 'text (text-block-text b)))
   (for/list ([b (in-list blocks)] #:when (tool-use-block? b))
     (hasheq 'type "tool_use"
             'id (tool-use-block-id b)
             'name (tool-use-block-name b)
             'input (tool-use-block-input b)))
  ) ; end append
) ; end define assistant->content

;; 一条内部 message → 一条 Anthropic message（或 #f 跳过空 assistant）。
(define (message->anthropic m)
  (define blocks (message-blocks m))
  (define tool-results (filter tool-result-block? blocks))
  (cond
    [(pair? tool-results)
     ;; 工具回填：role user，content 为 tool_result 块列表
     (hasheq 'role "user"
             'content
             (for/list ([tr (in-list tool-results)])
               (define c (tool-result-block-content tr))
               (hasheq 'type "tool_result"
                       'tool_use_id (tool-result-block-tool-use-id tr)
                       'content (if (string? c) c (jsexpr->string c))
                       'is_error (tool-result-block-is-error? tr))))
    ] ; end tool-result case
    [(eq? (message-role m) 'assistant)
     (define content (assistant->content m))
     (if (null? content) #f (hasheq 'role "assistant" 'content content))
    ] ; end assistant case
    [else
     ;; user / 其它文本
     (hasheq 'role "user" 'content (message-text m))
    ] ; end else
  ) ; end cond
) ; end define message->anthropic

;; OpenAI function spec → Anthropic tool
(define (openai-spec->anthropic-tool spec)
  (define fn (jsref spec 'function (hasheq)))
  (hasheq 'name (jsref fn 'name "")
          'description (jsref fn 'description "")
          'input_schema (jsref fn 'parameters (hasheq 'type "object" 'properties (hasheq))))
) ; end define openai-spec->anthropic-tool

;; ------------------------------------------------------------ 提示词缓存（建议①）
;; Anthropic prompt caching：在稳定前缀上打 cache_control ephemeral 断点，
;; 命中后该前缀不再重新编码/计费（TTL ~5min）。三处断点（≤4 上限）：
;;   system（静态）、tools 末块（缓存整段 tools+system）、最后一条消息末块（缓存对话前缀）。
;; 短于模型最小缓存长度的块会被 API 忽略（不缓存亦不报错），故对小 prompt 安全无副作用。
;; 因 SUB-SYSTEM + tools 在所有子 agent 间字节一致，同一前缀缓存跨兄弟子 agent 复用（建议④由此自动兑现）。
(define EPHEMERAL (hasheq 'type "ephemeral"))

;; system 作为带缓存的文本块数组
(define (system->cached sys)
  (list (hasheq 'type "text" 'text sys 'cache_control EPHEMERAL))
) ; end define system->cached

;; tools 末块打 cache_control（缓存 system+tools 静态前缀）
(define (tools->cached specs)
  (define tools (map openai-spec->anthropic-tool specs))
  (if (null? tools)
      tools
      (append (drop-right tools 1)
              (list (hash-set (last tools) 'cache_control EPHEMERAL))))
) ; end define tools->cached

;; 给消息列表最后一条的最后一个 content block 打 cache_control（缓存对话前缀，助多步复用）
(define (mark-last-message-cache anth-msgs)
  (cond
    [(null? anth-msgs) anth-msgs]
    [else
     (define lastm (last anth-msgs))
     (define content (hash-ref lastm 'content))
     (define content*
       (if (string? content)
           (list (hasheq 'type "text" 'text content 'cache_control EPHEMERAL))
           (append (drop-right content 1)
                   (list (hash-set (last content) 'cache_control EPHEMERAL)))))
     (append (drop-right anth-msgs 1)
             (list (hash-set lastm 'content content*)))]
  ) ; end cond
) ; end define mark-last-message-cache

;; 推理强度 → 扩展思考 token 预算（Anthropic 最小 1024）。
(define (effort->budget eff)
  (case eff [(low) 1024] [(medium) 4096] [(high) 12288] [else 1024])
) ; end define effort->budget

(define (build-anthropic-body cfg msgs tool-specs)
  (define anth-msgs
    (mark-last-message-cache (filter values (map message->anthropic msgs))))
  (define base
    (hasheq 'model (config-model cfg)
            'max_tokens (config-max-tokens cfg)
            'temperature (config-temperature cfg)
            'messages anth-msgs
            'stream #t))
  ;; 扩展思考（reasoning_effort → thinking）：需 temperature=1，且 max_tokens 要容纳思考+输出。
  (define eff (current-reasoning-effort))
  (define base*
    (if (eq? eff 'off)
        base
        (let ([budget (effort->budget eff)])
          (hash-set* base
                     'thinking (hasheq 'type "enabled" 'budget_tokens budget)
                     'temperature 1
                     'max_tokens (+ budget (config-max-tokens cfg))))
    ) ; end if
  ) ; end define base*
  (define with-sys
    (if (config-system-prompt cfg)
        (hash-set base* 'system (system->cached (config-system-prompt cfg)))
        base*))
  (if (pair? tool-specs)
      (hash-set with-sys 'tools (tools->cached tool-specs))
      with-sys)
) ; end define build-anthropic-body

;; ---------------------------------------------------------------- HTTP

(define (endpoint-parts endpoint)
  (define u (string->url endpoint))
  (values (url-host u)
          (or (url-port u) (if (equal? (url-scheme u) "https") 443 80))
          (equal? (url-scheme u) "https")
          (string-join
           (for/list ([pp (in-list (url-path u))]) (path/param-path pp))
           "/"))
) ; end define endpoint-parts

(define (status-code status-bytes)
  (define s (bytes->string/utf-8 status-bytes))
  (define m (regexp-match #rx" ([0-9][0-9][0-9])" s))
  (if m (string->number (cadr m)) 0)
) ; end define status-code

(define (post! cfg body-jsexpr)
  (define-values (host port ssl? base-path) (endpoint-parts (config-endpoint cfg)))
  (define conn (http-conn-open host #:port port #:ssl? ssl?))
  (define headers
    (append
     (list "Content-Type: application/json"
           f"anthropic-version: {ANTHROPIC-VERSION}")
     (if (config-api-key cfg)
         (list f"x-api-key: {(config-api-key cfg)}")
         '())))
  (define-values (status resp-headers in)
    (http-conn-sendrecv! conn f"/{base-path}/v1/messages"
                         #:method #"POST"
                         #:headers headers
                         #:data (jsexpr->bytes body-jsexpr)))
  (values (status-code status) in)
) ; end define post!

;; ------------------------------------------------------------ Anthropic 累加器

;; 累积中的 tool_use 槽（按 content block index 寻址）
(struct au-slot (id name args) #:mutable)   ; args: output-string port

(struct anth-acc
  (content        ; output-string — 正文
   think          ; output-string — 扩展思考文本
   sig            ; output-string — 思考块签名（回传用）
   slots          ; hash: index -> au-slot
   stop-reason     ; box
   in-tokens      ; box
   out-tokens     ; box
  ) ; end fields
) ; end struct anth-acc

(define (make-anth-acc)
  (anth-acc (open-output-string) (open-output-string) (open-output-string)
            (make-hash) (box #f) (box 0) (box 0))
) ; end define make-anth-acc

;; 喂一个 SSE 事件；返回 (listof evt:delta) 供转发渲染。
(define (anth-feed! acc evt-type data)
  (define out '())
  (case evt-type
    [("message_start")
     (define u (jsref (jsref data 'message (hasheq)) 'usage (hasheq)))
     (set-box! (anth-acc-in-tokens acc) (or (jsref u 'input_tokens 0) 0))]
    [("content_block_start")
     (define idx (jsref data 'index 0))
     (define cb (jsref data 'content_block (hasheq)))
     (when (equal? (jsref cb 'type) "tool_use")
       (hash-set! (anth-acc-slots acc) idx
                  (au-slot (jsref cb 'id "") (jsref cb 'name "") (open-output-string))))]
    [("content_block_delta")
     (define idx (jsref data 'index 0))
     (define delta (jsref data 'delta (hasheq)))
     (case (jsref delta 'type)
       [("text_delta")
        (define t (jsref delta 'text ""))
        (when (and (string? t) (non-empty-string? t))
          (write-string t (anth-acc-content acc))
          (set! out (list (evt:delta (now-ms) 'text t))))]
       [("input_json_delta")
        (define frag (jsref delta 'partial_json ""))
        (define slot (hash-ref (anth-acc-slots acc) idx #f))
        (when (and slot (string? frag))
          (write-string frag (au-slot-args slot))
          (set! out (list (evt:delta (now-ms) 'tool-json frag))))]
       [("thinking_delta")
        (define t (jsref delta 'thinking ""))
        (when (and (string? t) (non-empty-string? t))
          (write-string t (anth-acc-think acc))            ; 存以回传
          (set! out (list (evt:delta (now-ms) 'thinking t))))]
       [("signature_delta")
        (define s (jsref delta 'signature ""))
        (when (string? s) (write-string s (anth-acc-sig acc)))]   ; 无显示，仅存签名
       [else (void)])]
    [("message_delta")
     (define d (jsref data 'delta (hasheq)))
     (when (jsref d 'stop_reason) (set-box! (anth-acc-stop-reason acc) (jsref d 'stop_reason)))
     (define u (jsref data 'usage (hasheq)))
     (when (jsref u 'output_tokens) (set-box! (anth-acc-out-tokens acc) (jsref u 'output_tokens)))]
    [("error")
     (define e (jsref data 'error (hasheq)))
     (error 'anthropic f"{(jsref e 'type "error")}: {(jsref e 'message "unknown")}")]
    [else (void)])                          ; ping / content_block_stop / message_stop
  out
) ; end define anth-feed!

;; 收尾：拼装 assistant message。thinking 块（带签名）首位，再 text，再 tool_use。
(define (anth-finish acc)
  (define text (get-output-string (anth-acc-content acc)))
  (define think-text (get-output-string (anth-acc-think acc)))
  (define sig (get-output-string (anth-acc-sig acc)))
  (define tcs
    (for/list ([idx (in-list (sort (hash-keys (anth-acc-slots acc)) <))])
      (define slot (hash-ref (anth-acc-slots acc) idx))
      (define args-str (get-output-string (au-slot-args slot)))
      (tool-use-block (au-slot-id slot) (au-slot-name slot)
                      (with-handlers ([exn:fail? (lambda (_e) (hasheq))])
                        (if (string=? args-str "") (hasheq) (string->jsexpr args-str))))))
  (define blocks
    (append
     (if (non-empty-string? think-text)
         (list (thinking-block think-text (and (non-empty-string? sig) sig)))
         '())
     (if (non-empty-string? text) (list (text-block text)) '())
     tcs))
  (values (message 'assistant (if (null? blocks) (list (text-block "")) blocks))
          (or (unbox (anth-acc-stop-reason acc)) "stop")
          (usage (unbox (anth-acc-in-tokens acc)) (unbox (anth-acc-out-tokens acc))))
) ; end define anth-finish

;; ------------------------------------------------------------- 流式主流程

(define RETRY-DELAYS '(1 2 4))

(define (try-once! cfg msgs tool-specs ch started?)
  (define-values (code in) (post! cfg (build-anthropic-body cfg msgs tool-specs)))
  (cond
    [(and (>= code 200) (< code 300))
     (define acc (make-anth-acc))
     (sse-pump! in
       (lambda (evt-type data)
         (when data
           (set-box! started? #t)
           (for ([d (in-list (anth-feed! acc evt-type data))])
             (async-channel-put ch d)))))
     (define-values (msg stop-reason u) (anth-finish acc))
     (async-channel-put ch (evt:message (now-ms) msg))
     (async-channel-put ch (evt:turn-end (now-ms) stop-reason u))
     'ok]
    [(or (= code 429) (>= code 500))
     (log-warning f"anthropic: HTTP {code}, retryable: {(port->string in)}")
     (list 'retryable code)]
    [else (error 'anthropic f"HTTP {code}: {(port->string in)}")])
) ; end define try-once!

(define (stream-with-retry! cfg msgs tool-specs ch)
  (define started? (box #f))
  (let loop ([delays RETRY-DELAYS])
    (define r
      (with-handlers ([exn:fail:network?
                       (lambda (e)
                         (log-warning f"anthropic: network error: {(exn-message e)}")
                         (list 'retryable 'network))])
        (try-once! cfg msgs tool-specs ch started?)))
    (cond
      [(eq? r 'ok) (void)]
      [(unbox started?) (error 'anthropic f"stream interrupted mid-flight ({(cadr r)})")]
      [(null? delays) (error 'anthropic f"retries exhausted ({(cadr r)})")]
      [else (sleep (car delays)) (loop (cdr delays))]))
) ; end define stream-with-retry!

;; ---------------------------------------------------------------- 构造器

(define (make-anthropic-provider cfg)
  (define current-cust (box #f))
  (provider
   f"anthropic:{(config-model cfg)}"
   (lambda (msgs tool-specs)
     (define ecfg (or (current-config) cfg))    ; 运行时读 live config
     (define ch (make-async-channel))
     (define cust (make-custodian))
     (set-box! current-cust cust)
     (parameterize ([current-custodian cust])
       (thread
        (lambda ()
          (with-handlers ([exn:fail? (lambda (e) (async-channel-put ch (evt:error (now-ms) e #f)))])
            (stream-with-retry! ecfg msgs tool-specs ch)))))
     ch)
   (lambda ()
     (define c (unbox current-cust))
     (when c (custodian-shutdown-all c))))
) ; end define make-anthropic-provider

;; ---------------------------------------------------------------- provide

(provide
 make-anthropic-provider
 message->anthropic
 build-anthropic-body
 openai-spec->anthropic-tool
) ; end provide
