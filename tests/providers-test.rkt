#lang tstring racket
;; providers-test.rkt — 内置供应商档案 + Anthropic 线路转换（离线，无网络）。

(require
 rackunit
 (file "../src/model.rkt")
 (file "../src/tool.rkt")
 (file "../src/plugin.rkt")
 (file "../src/providers.rkt")
 (file "../src/provider-anthropic.rkt")
) ; end require

;; ------------------------------------------------------------ 档案注册

(test-case "register-builtin-providers! exposes all five profiles"
  (define host (make-plugin-host))
  (register-builtin-providers! host)
  (define avail (host-available-providers host))
  (for ([name (in-list '("lmstudio" "openai" "anthropic" "gemini" "grok"))])
    (check-not-false (member name avail) (format "~a 应可选" name)))
  (check-equal? (host-current-provider host) "lmstudio")        ; 默认本地
) ; end test-case

(test-case "host-set-provider! switches among builtins; unknown rejected"
  (define host (make-plugin-host))
  (register-builtin-providers! host)
  (check-true (host-set-provider! host "anthropic"))
  (check-equal? (host-current-provider host) "anthropic")
  (check-true (host-set-provider! host "grok"))
  (check-false (host-set-provider! host "nope"))
  (check-equal? (host-current-provider host) "grok")            ; 未知名不改
) ; end test-case

;; ------------------------------------------------------------ 档案应用到 config

(test-case "apply-provider-profile rewrites endpoint/model/api-key from env"
  (putenv "OPENAI_API_KEY" "sk-test-123")
  (define c (apply-provider-profile (default-config) "openai"))
  (check-equal? (config-endpoint c) "https://api.openai.com/v1")
  (check-equal? (config-model c) "gpt-5")
  (check-equal? (config-api-key c) "sk-test-123")
) ; end test-case

(test-case "apply-provider-profile: missing key env → api-key #f; unknown name → unchanged"
  (define c (apply-provider-profile (default-config) "gemini"))
  (check-equal? (config-endpoint c) "https://generativelanguage.googleapis.com/v1beta/openai")
  ;; GEMINI_API_KEY 未设 → #f
  (check-false (config-api-key c))
  (define c2 (apply-provider-profile (default-config) "not-a-provider"))
  (check-equal? (config-endpoint c2) (config-endpoint (default-config)))  ; 原样
) ; end test-case

(test-case "builtin-provider-name? / lmstudio has no key env"
  (check-true (builtin-provider-name? "anthropic"))
  (check-false (builtin-provider-name? "echollm"))
  (check-false (provider-profile-key-env-of "lmstudio"))
  (check-equal? (provider-profile-key-env-of "anthropic") "ANTHROPIC_API_KEY")
) ; end test-case

;; ------------------------------------------------------------ Anthropic 线路转换

(test-case "message->anthropic: tool-result user message → tool_result blocks"
  (define m (message 'user (list (tool-result-block "t1" "42" #f))))
  (define a (message->anthropic m))
  (check-equal? (hash-ref a 'role) "user")
  (define blk (car (hash-ref a 'content)))
  (check-equal? (hash-ref blk 'type) "tool_result")
  (check-equal? (hash-ref blk 'tool_use_id) "t1")
  (check-equal? (hash-ref blk 'content) "42")
  (check-false (hash-ref blk 'is_error))
) ; end test-case

(test-case "message->anthropic: assistant text + tool_use → content blocks"
  (define m (message 'assistant
                     (list (text-block "let me run it")
                           (tool-use-block "u1" "bash" (hasheq 'command "ls")))))
  (define a (message->anthropic m))
  (check-equal? (hash-ref a 'role) "assistant")
  (define content (hash-ref a 'content))
  (check-equal? (length content) 2)
  (check-equal? (hash-ref (first content) 'type) "text")
  (check-equal? (hash-ref (second content) 'type) "tool_use")
  (check-equal? (hash-ref (second content) 'id) "u1")
  (check-equal? (hash-ref (second content) 'name) "bash")
) ; end test-case

(test-case "message->anthropic: empty assistant → #f (skipped)"
  (check-false (message->anthropic (message 'assistant '())))
) ; end test-case

(test-case "openai-spec->anthropic-tool maps to name/description/input_schema"
  (define spec (function-spec "bash" "run a command"
                              (hasheq 'command (hasheq 'type "string")) '("command")))
  (define t (openai-spec->anthropic-tool spec))
  (check-equal? (hash-ref t 'name) "bash")
  (check-equal? (hash-ref t 'description) "run a command")
  (check-true (hash? (hash-ref t 'input_schema)))
  (check-equal? (hash-ref (hash-ref t 'input_schema) 'type) "object")
) ; end test-case

(test-case "build-anthropic-body: system + tools + stream, empty assistants filtered"
  (define cfg (struct-copy config (default-config)
                           [system-prompt "you are helpful"] [model "claude-sonnet-5"]))
  (define msgs (list (text-msg 'user "hi")
                     (message 'assistant '())))                 ; 空 assistant 应被过滤
  (define spec (function-spec "bash" "run" (hasheq) '()))
  (define body (build-anthropic-body cfg msgs (list spec)))
  ;; system 现为带缓存断点的文本块数组（建议①）
  (define sys (hash-ref body 'system))
  (check-true (list? sys))
  (check-equal? (hash-ref (car sys) 'text) "you are helpful")
  (check-equal? (hash-ref (car sys) 'cache_control) (hasheq 'type "ephemeral"))
  (check-equal? (hash-ref body 'model) "claude-sonnet-5")
  (check-true (hash-ref body 'stream))
  (check-equal? (length (hash-ref body 'messages)) 1)          ; 仅 user，空 assistant 滤除
  (check-equal? (length (hash-ref body 'tools)) 1)
) ; end test-case

(test-case "prompt caching: cache_control on tools tail and last message block (建议①)"
  (define cfg (struct-copy config (default-config) [system-prompt "sys"]))
  (define specs (list (function-spec "a" "" (hasheq) '())
                      (function-spec "b" "" (hasheq) '())))
  (define body (build-anthropic-body cfg (list (text-msg 'user "hello")) specs))
  ;; tools 末块打断点（缓存 system+tools 静态前缀）
  (define tools (hash-ref body 'tools))
  (check-equal? (hash-ref (last tools) 'cache_control) (hasheq 'type "ephemeral"))
  (check-false (hash-ref (first tools) 'cache_control #f))     ; 只末块
  ;; 最后一条消息的末 content block 打断点（缓存对话前缀，助多步复用）
  (define lastm (last (hash-ref body 'messages)))
  (define lastblk (last (hash-ref lastm 'content)))
  (check-equal? (hash-ref lastblk 'text) "hello")
  (check-equal? (hash-ref lastblk 'cache_control) (hasheq 'type "ephemeral"))
) ; end test-case

(test-case "registry-specs is name-sorted (byte-stable tools prefix, 建议②)"
  (define reg (make-registry '()))
  ;; 乱序插入，输出应按名称升序，稳定可缓存
  (for ([nm (in-list '("zebra" "alpha" "mid"))])
    (registry-add! reg (make-simple-tool #:name nm #:desc "" #:run (lambda (_in _ctx) (ok-outcome "x")))))
  (define names (for/list ([s (in-list (registry-specs reg))])
                  (hash-ref (hash-ref s 'function) 'name)))
  (check-equal? names '("alpha" "mid" "zebra"))
) ; end test-case

(displayln "providers-test: all passed")
