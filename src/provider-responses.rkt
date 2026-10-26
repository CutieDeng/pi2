#lang tstring racket
;; provider-responses.rkt — OpenAI Responses API（POST {endpoint}/responses，流式 SSE）客户端。
;; 三种调用格式之一（Chat Completions 旧 / Responses 新 / Anthropic Messages）；用哪一种由 wire.rkt 自动决定，用户不感知。
;;
;; 映射：
;;   system prompt → instructions；用户 / 助手文本 → {role, content}；工具调用 → {type:"function_call", call_id, name, arguments}；
;;   工具结果 → {type:"function_call_output", call_id, output}；thinking 块不发（Responses 的推理项要加密内容，store:false 时不可引用）。
;;   工具说明从 Chat 形状 {type, function:{name, description, parameters}} 拍平成 {type:"function", name, description, parameters}。
;;   推理档：off 不发；low/medium/high 原样；max → high。不发 temperature（推理模型拒收；默认 1）。store:false（不在服务端留存）。
;; 事件：response.output_text.delta → 文字；response.reasoning_summary_text.delta / response.reasoning_text.delta → thinking；
;;   response.output_item.added(function_call) + response.function_call_arguments.delta → 工具调用（参数流式）；
;;   response.completed → 用量；response.incomplete → stop reason "length"；response.failed / error → 错误。

(require
 json
 racket/async-channel
 racket/string
 racket/list
 (file "model.rkt")
 (file "stream.rkt")                    ; sse-pump!
 (file "provider.rkt")                  ; provider 结构、post!
 (file "media.rkt")                     ; 消息里的媒体：按模型能力投影
) ; end require

;; ---------------------------------------------------------------- 请求

(define (message->input-items m)
  (define role (message-role m))
  (define bs (message-blocks m))
  (cond
    [(ormap tool-result-block? bs)
     (for/list ([b (in-list bs)] #:when (tool-result-block? b))
       (define c (tool-result-block-content b))
       (hasheq 'type "function_call_output" 'call_id (tool-result-block-tool-use-id b)
               'output (if (string? c) c (jsexpr->string c))))]
    [(eq? role 'assistant)
     (define text (string-join (for/list ([b (in-list bs)] #:when (text-block? b)) (text-block-text b)) ""))
     (append (if (string=? (string-trim text) "") '() (list (hasheq 'role "assistant" 'content text)))
             (for/list ([b (in-list bs)] #:when (tool-use-block? b))
               (hasheq 'type "function_call" 'call_id (tool-use-block-id b) 'name (tool-use-block-name b)
                       'arguments (jsexpr->string (tool-use-block-input b)))))]
    [(has-media? m)
     (define-values (imgs phs) (split-media bs (let ([c (current-config)]) (and c (config-model c)))))
     (define text (string-join (filter (lambda (s) (not (string=? s ""))) (cons (message-text m) phs)) "\n"))
     (list (hasheq 'role (symbol->string role)
                   'content (if (null? imgs)
                                text
                                (append (if (string=? text "") '() (list (hasheq 'type "input_text" 'text text)))
                                        (for/list ([pr (in-list imgs)])
                                          (hasheq 'type "input_image" 'image_url (format "data:~a;base64,~a" (media-block-media-type (car pr)) (cdr pr))))))))]
    [else (list (hasheq 'role (symbol->string role) 'content (message-text m)))])
) ; end define message->input-items

;; Chat 形状的工具说明 → Responses 形状
(define (chat-spec->responses-tool spec)
  (define f (hash-ref spec 'function spec))
  (hasheq 'type "function" 'name (hash-ref f 'name "") 'description (hash-ref f 'description "")
          'parameters (hash-ref f 'parameters (hasheq 'type "object" 'properties (hasheq))))
) ; end define chat-spec->responses-tool

(define (build-responses-body cfg msgs tool-specs)
  (define eff (effective-reasoning-effort (config-model cfg)))
  (define base
    (hasheq 'model (config-model cfg)
            'input (append-map message->input-items msgs)
            'max_output_tokens (config-max-tokens cfg)
            'stream #t
            'store #f))
  (define b1 (if (config-system-prompt cfg) (hash-set base 'instructions (config-system-prompt cfg)) base))
  (define b2 (if (eq? eff 'off) b1 (hash-set b1 'reasoning (hasheq 'effort (symbol->string (if (eq? eff 'max) 'high eff))))))
  (if (pair? tool-specs) (hash-set b2 'tools (map chat-spec->responses-tool tool-specs)) b2)
) ; end define build-responses-body

;; ---------------------------------------------------------------- 流

;; 累积器：文字、推理摘要、函数调用（按 item id）、用量、结束原因
(struct racc ([text #:mutable] [thinking #:mutable] calls [order #:mutable] [usage #:mutable] [stop #:mutable]))
(define (make-racc) (racc "" "" (make-hash) '() (usage 0 0) "stop"))

;; 一个 SSE 事件 → 要发出的 evt 表（文字 / thinking / 工具参数增量）
(define (racc-feed! a j)
  (define type (hash-ref j 'type ""))
  (case type
    [("response.output_text.delta")
     (define d (hash-ref j 'delta ""))
     (set-racc-text! a (string-append (racc-text a) d))
     (list (evt:delta (now-ms) 'text d))]
    [("response.reasoning_summary_text.delta" "response.reasoning_text.delta")
     (define d (hash-ref j 'delta ""))
     (set-racc-thinking! a (string-append (racc-thinking a) d))
     (list (evt:delta (now-ms) 'thinking d))]
    [("response.output_item.added")
     (define item (hash-ref j 'item (hasheq)))
     (when (equal? (hash-ref item 'type #f) "function_call")
       (define id (hash-ref item 'id (hash-ref item 'call_id "")))
       (hash-set! (racc-calls a) id (vector (hash-ref item 'call_id id) (hash-ref item 'name "") (hash-ref item 'arguments "")))
       (set-racc-order! a (append (racc-order a) (list id))))
     '()]
    [("response.function_call_arguments.delta")
     (define id (hash-ref j 'item_id ""))
     (define v (hash-ref (racc-calls a) id #f))
     (define d (hash-ref j 'delta ""))
     (when v (vector-set! v 2 (string-append (vector-ref v 2) d)))
     (list (evt:delta (now-ms) 'tool-json d))]
    [("response.output_item.done")
     (define item (hash-ref j 'item (hasheq)))
     (when (equal? (hash-ref item 'type #f) "function_call")
       (define id (hash-ref item 'id (hash-ref item 'call_id "")))
       (define v (hash-ref (racc-calls a) id #f))
       (if v
           (vector-set! v 2 (hash-ref item 'arguments (vector-ref v 2)))
           (begin (hash-set! (racc-calls a) id (vector (hash-ref item 'call_id id) (hash-ref item 'name "") (hash-ref item 'arguments "")))
                  (set-racc-order! a (append (racc-order a) (list id))))))
     '()]
    [("response.completed" "response.incomplete")
     (define r (hash-ref j 'response (hasheq)))
     (define u (hash-ref r 'usage #f))
     (when (hash? u) (set-racc-usage! a (usage (hash-ref u 'input_tokens 0) (hash-ref u 'output_tokens 0))))
     (when (equal? type "response.incomplete") (set-racc-stop! a "length"))
     '()]
    [("response.failed")
     (define err (hash-ref (hash-ref j 'response (hasheq)) 'error (hasheq)))
     (define msg (if (hash? err) (hash-ref err 'message "?") "?"))
     (error 'responses f"response failed: {msg}")]
    [("error")
     (define msg (hash-ref j 'message (jsexpr->string j)))
     (error 'responses f"error: {msg}")]
    [else '()])
) ; end define racc-feed!

(define (racc-finish a)
  (define calls (for/list ([id (in-list (racc-order a))])
                  (define v (hash-ref (racc-calls a) id))
                  (tool-use-block (vector-ref v 0) (vector-ref v 1)
                                  (with-handlers ([exn:fail? (lambda (_e) (hasheq '_raw (vector-ref v 2)))])
                                    (if (string=? (string-trim (vector-ref v 2)) "") (hasheq) (string->jsexpr (vector-ref v 2)))))))
  (define blocks (append (if (string=? (racc-thinking a) "") '() (list (thinking-block (racc-thinking a) #f)))
                         (if (string=? (racc-text a) "") '() (list (text-block (racc-text a))))
                         calls))
  (values (message 'assistant blocks) (if (pair? calls) "tool_calls" (racc-stop a)) (racc-usage a))
) ; end define racc-finish

(define RETRY-DELAYS '(1 2 4))

(define (try-once! cfg msgs tool-specs ch started?)
  (define-values (code _h in) (post! cfg "/responses" (build-responses-body cfg msgs tool-specs)))
  (cond
    [(<= 200 code 299)
     (define a (make-racc))
     (sse-pump! in (lambda (_type data)
                     (when (hash? data)
                       (set-box! started? #t)
                       (for ([e (in-list (racc-feed! a data))]) (async-channel-put ch e)))))
     (define-values (msg stop u) (racc-finish a))
     (async-channel-put ch (evt:message (now-ms) msg))
     (async-channel-put ch (evt:turn-end (now-ms) stop u))
     'ok]
    [(or (= code 429) (>= code 500)) (log-warning f"responses: HTTP {code}: {(port->string in)}") (list 'retryable code)]
    [else (error 'provider f"HTTP {code}: {(port->string in)}")])
) ; end define try-once!

(define (stream-with-retry! cfg msgs tool-specs ch)
  (define started? (box #f))
  (let loop ([delays RETRY-DELAYS])
    (define r (with-handlers ([exn:fail:network? (lambda (e) (log-warning f"responses: network error: {(exn-message e)}") (list 'retryable 'network))])
                (try-once! cfg msgs tool-specs ch started?)))
    (cond [(eq? r 'ok) (void)]
          [(unbox started?) (error 'provider f"stream interrupted mid-flight ({(cadr r)})")]
          [(null? delays) (error 'provider f"retries exhausted ({(cadr r)})")]
          [else (sleep (car delays)) (loop (cdr delays))]))
) ; end define stream-with-retry!

(define (make-responses-provider cfg)
  (define streams (make-stream-set))
  (provider
   f"responses:{(config-model cfg)}"
   (lambda (msgs tool-specs)
     (define ecfg (or (current-config) cfg))
     (define ch (make-async-channel))
     (stream-set-spawn! streams
       (lambda ()
         (with-handlers ([exn:fail? (lambda (e) (async-channel-put ch (evt:error (now-ms) e #f)))])
           (stream-with-retry! ecfg msgs tool-specs ch))))
     ch)
   (lambda () (stream-set-cancel-all! streams)))
) ; end define make-responses-provider

(provide make-responses-provider build-responses-body message->input-items chat-spec->responses-tool)
