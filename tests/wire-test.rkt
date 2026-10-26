#lang tstring racket
;; wire-test.rkt — Responses API 客户端 + 调用格式自动适配（本地假 HTTP 服务器，不碰网络）

(require
 rackunit
 json
 racket/async-channel
 racket/tcp
 racket/file
 (file "../src/model.rkt")
 (file "../src/provider.rkt")
 (file "../src/provider-responses.rkt")
 (file "../src/wire.rkt")
) ; end require

(define cfg-dir (make-temporary-file "pi2-wire-~a" 'directory))
(void (putenv "PI_CONFIG_HOME" (path->string cfg-dir)))
(reload-wire-cache!)

;; ---------------------------------------------------------------- 假服务器：按路径回放 (状态 . 正文)
(define requests '())                                   ; (路径 . 请求体 jsexpr)
(define routes (make-hash))                             ; 路径 → (cons 状态 正文)

(define (read-request in)
  (define line (read-line in 'return-linefeed))
  (define path (cadr (regexp-match #rx"^[A-Z]+ ([^ ]+)" line)))
  (define len (let loop ([n 0])
                (define h (read-line in 'return-linefeed))
                (cond [(or (eof-object? h) (string=? h "")) n]
                      [(regexp-match #rx"(?i:^content-length: *([0-9]+))" h) => (lambda (m) (loop (string->number (cadr m))))]
                      [else (loop n)])))
  (values path (if (> len 0) (bytes->jsexpr (read-bytes len in)) #f)))

(define listener (tcp-listen 0 16 #t "127.0.0.1"))
(define port (let-values ([(_a p _b _c) (tcp-addresses listener #t)]) p))
(void (thread (lambda ()
                (let loop ()
                  (define-values (in out) (tcp-accept listener))
                  (thread (lambda ()
                            (define-values (path body) (read-request in))
                            (set! requests (append requests (list (cons path body))))
                            (define r (hash-ref routes path (cons 404 "{\"error\":\"no route\"}")))
                            (fprintf out "HTTP/1.1 ~a X\r\nContent-Type: text/event-stream\r\nConnection: close\r\n\r\n~a" (car r) (cdr r))
                            (close-output-port out) (close-input-port in)))
                  (loop)))))

(define ENDPOINT (format "http://127.0.0.1:~a/v1" port))
(define (sse . events) (apply string-append (for/list ([e events]) (format "data: ~a\n\n" (jsexpr->string e)))))

(define RESPONSES-OK
  (sse (hasheq 'type "response.output_text.delta" 'delta "Hel")
       (hasheq 'type "response.output_text.delta" 'delta "lo")
       (hasheq 'type "response.output_item.added" 'item (hasheq 'type "function_call" 'id "fc_1" 'call_id "call_1" 'name "bash" 'arguments ""))
       (hasheq 'type "response.function_call_arguments.delta" 'item_id "fc_1" 'delta "{\"command\":")
       (hasheq 'type "response.function_call_arguments.delta" 'item_id "fc_1" 'delta "\"ls\"}")
       (hasheq 'type "response.output_item.done" 'item (hasheq 'type "function_call" 'id "fc_1" 'call_id "call_1" 'name "bash" 'arguments "{\"command\":\"ls\"}"))
       (hasheq 'type "response.completed" 'response (hasheq 'status "completed" 'usage (hasheq 'input_tokens 11 'output_tokens 7)))))

(define CHAT-OK
  (string-append (sse (hasheq 'choices (list (hasheq 'delta (hasheq 'content "from chat")))))
                 (sse (hasheq 'choices (list (hasheq 'delta (hasheq) 'finish_reason "stop")) 'usage (hasheq 'prompt_tokens 3 'completion_tokens 2)))
                 "data: [DONE]\n\n"))

(define (collect p msgs tools cfg)
  (define ch (parameterize ([current-config cfg]) (provider-stream! p msgs tools)))
  (let loop ([acc '()])
    (define e (sync/timeout 10 ch))
    (cond [(not e) (error 'collect "timeout")]
          [(evt:turn-end? e) (reverse (cons e acc))]
          [(evt:error? e) (reverse (cons e acc))]
          [else (loop (cons e acc))])))

(define CFG (struct-copy config (default-config) [endpoint ENDPOINT] [model "gpt-x"] [system-prompt "be brief"] [api-key "k"]))
(define TOOLS (list (hasheq 'type "function" 'function (hasheq 'name "bash" 'description "run" 'parameters (hasheq 'type "object" 'properties (hasheq))))))

(test-case "Responses client: request shape, streamed text + function call arguments, usage, stop reason"
  (set! requests '())
  (hash-set! routes "/v1/responses" (cons 200 RESPONSES-OK))
  (define msgs (list (text-msg 'user "list files")
                     (message 'assistant (list (text-block "ok") (tool-use-block "call_0" "bash" (hasheq 'command "pwd"))))
                     (message 'user (list (tool-result-block "call_0" "/tmp" #f)))))
  (define evs (parameterize ([current-reasoning-effort 'max]) (collect (make-responses-provider CFG) msgs TOOLS CFG)))
  (define body (cdr (last requests)))
  (check-equal? (car (last requests)) "/v1/responses")
  (check-equal? (hash-ref body 'instructions) "be brief")
  (check-equal? (hash-ref body 'store) #f)
  (check-equal? (hash-ref (hash-ref body 'reasoning) 'effort) "high")              ; max → high
  (check-false (hash-has-key? body 'temperature))
  (check-equal? (map (lambda (i) (or (hash-ref i 'type #f) (hash-ref i 'role))) (hash-ref body 'input))
                '("user" "assistant" "function_call" "function_call_output"))
  (check-equal? (hash-ref (car (hash-ref body 'tools)) 'name) "bash")               ; 拍平的工具说明
  (define msg (evt:message-msg (findf evt:message? evs)))
  (check-equal? (message-text msg) "Hello")
  (define tu (findf tool-use-block? (message-blocks msg)))
  (check-equal? (list (tool-use-block-id tu) (tool-use-block-name tu) (tool-use-block-input tu)) (list "call_1" "bash" (hasheq 'command "ls")))
  (define end (last evs))
  (check-equal? (evt:turn-end-stop-reason end) "tool_calls")
  (check-equal? (usage-input-tokens (evt:turn-end-usage end)) 11))

(test-case "auto: a responses-only model on /chat/completions switches to /responses, and the choice is cached"
  (reload-wire-cache!)
  (set! requests '())
  (hash-set! routes "/v1/chat/completions"
             (cons 400 "{\"error\":{\"message\":\"This model is only supported in v1/responses and not in v1/chat/completions.\"}}"))
  (hash-set! routes "/v1/responses" (cons 200 RESPONSES-OK))
  (define p (make-openai-auto-provider CFG))
  (define evs (collect p (list (text-msg 'user "hi")) '() CFG))
  (check-true (evt:turn-end? (last evs)))
  (check-false (ormap evt:error? evs))                                              ; 调用方看不到格式切换
  (check-equal? (map car requests) '("/v1/chat/completions" "/v1/responses"))
  (check-equal? (wire-for ENDPOINT "gpt-x") 'responses)
  (set! requests '())
  (collect p (list (text-msg 'user "again")) '() CFG)
  (check-equal? (map car requests) '("/v1/responses"))                             ; 记住了：直接走新格式
  (reload-wire-cache!)
  (check-equal? (wire-for ENDPOINT "gpt-x") 'responses))                           ; 落盘

(test-case "auto: an endpoint without /responses (404) falls back to chat completions"
  (reload-wire-cache!)
  (remember-wire! ENDPOINT "local-model" 'responses)
  (set! requests '())
  (hash-remove! routes "/v1/responses")
  (hash-set! routes "/v1/chat/completions" (cons 200 CHAT-OK))
  (define c2 (struct-copy config CFG [model "local-model"]))
  (define evs (collect (make-openai-auto-provider c2) (list (text-msg 'user "hi")) '() c2))
  (check-equal? (message-text (evt:message-msg (findf evt:message? evs))) "from chat")
  (check-equal? (map car requests) '("/v1/responses" "/v1/chat/completions"))
  (check-equal? (wire-for ENDPOINT "local-model") 'chat))

(test-case "defaults and mismatch detection"
  (check-equal? (default-wire "https://api.openai.com/v1") 'responses)
  (check-equal? (default-wire "http://localhost:1234/v1") 'chat)
  (check-equal? (wire-mismatch "provider: HTTP 404: not found" 'responses) 'chat)
  (check-false (wire-mismatch "provider: HTTP 429: rate limit" 'chat))
  (check-false (wire-mismatch "provider: HTTP 401: invalid api key" 'responses)))

(tcp-close listener)
(delete-directory/files cfg-dir)
