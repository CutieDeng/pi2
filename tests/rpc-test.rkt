#lang tstring racket
;; rpc-test.rkt — 无头 JSONL 模式（离线）：以字符串端口喂 stdin、抓 stdout，
;; mock provider 确定性回复，断言 NDJSON 事件序列。

(require
 rackunit
 json
 racket/string
 racket/async-channel
 racket/file
 (file "../src/model.rkt")
 (file "../src/event.rkt")
 (file "../src/provider.rkt")
 (file "../src/tool.rkt")
 (file "../src/permission.rkt")
 (file "../src/loop.rkt")
 (file "../src/session.rkt")
 (file "../src/plugin.rkt")
 (file "../src/providers.rkt")
 (file "../src/auto.rkt")
 (file "../src/rpc.rkt")
 (file "../src/tools/builtin.rkt")
) ; end require

(define tmpdir (make-temporary-file "pi2-rpctest-~a" 'directory))
;; 隔离凭据目录：add_key 等只写临时 store，绝不触碰真实 ~/.config/pi++。
(putenv "PI_CONFIG_HOME" (path->string (make-temporary-file "pi2-rpccfg-~a" 'directory)))

;; mock provider：吐一段 text（delta + message + turn-end），无工具。
(define (mock-text-provider text)
  (provider "mock"
    (lambda (_msgs _tools)
      (define ch (make-async-channel))
      (thread (lambda ()
        (async-channel-put ch (evt:delta (now-ms) 'text text))
        (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant text)))
        (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 3 2)))))
      ch)
    void)
) ; end define mock-text-provider

;; 把请求列表（hasheq）作为 NDJSON 喂给 run-rpc!，返回解析出的 stdout 事件列表。
(define (drive requests #:provider [prov (mock-text-provider "hello")])
  (define cfg (struct-copy config (default-config)
                           [workdir (path->string tmpdir)] [permission-mode 'yolo]))
  (define host (make-plugin-host))
  (register-builtin-providers! host)
  (define d (make-deps #:provider prov #:registry (make-registry '())
                       #:bus (make-bus) #:policy (make-policy cfg) #:plugin-host host))
  (define sess (session-open! (fresh-session-path (path->string tmpdir)) cfg))
  (define in (open-input-string (string-join (map jsexpr->string requests) "\n")))
  (define out (open-output-string))
  (parameterize ([current-input-port in] [current-output-port out])
    (run-rpc! d (make-initial-state cfg) sess #:plugin-host host))
  (for/list ([ln (in-list (string-split (get-output-string out) "\n"))]
             #:when (non-empty-string? (string-trim ln)))
    (string->jsexpr ln))
) ; end define drive

(define (types evs) (map (lambda (j) (hash-ref j 'type #f)) evs))
(define (find-type evs t) (findf (lambda (j) (equal? (hash-ref j 'type #f) t)) evs))

;; ------------------------------------------------------------ 基本轮 + 查询

(test-case "prompt streams delta/message/turn_end then turn_complete; ready first; bye last"
  (define evs (drive (list (hasheq 'type "prompt" 'text "hi")
                           (hasheq 'type "state")
                           (hasheq 'type "shutdown"))))
  (define ts (types evs))
  (check-equal? (car ts) "ready")                       ; 首个必是 ready
  (check-equal? (last ts) "bye")                        ; 末个必是 bye
  (check-true (and (member "delta" ts) #t))
  (check-true (and (member "message" ts) #t))
  (check-true (and (member "turn_end" ts) #t))
  (check-true (and (member "turn_complete" ts) #t))
  ;; message 内容
  (define msg (find-type evs "message"))
  (check-equal? (hash-ref msg 'role) "assistant")
  (check-equal? (hash-ref msg 'text) "hello")
  ;; turn_complete 计 1 轮
  (check-equal? (hash-ref (find-type evs "turn_complete") 'turn) 1)
  ;; state 反映模型/供应商/用量
  (define st (find-type evs "state"))
  (check-equal? (hash-ref st 'provider) "lmstudio")
  (check-equal? (hash-ref st 'messages) 2)              ; user + assistant
  (check-equal? (hash-ref (hash-ref st 'usage) 'output) 2)
  (check-true (real? (hash-ref st 'cost_usd)))          ; 本地 gemma → 0.0（记费字段在场）
  (check-true (real? (hash-ref (find-type evs "turn_complete") 'cost_usd)))
) ; end test-case

;; ------------------------------------------------------------ 运行时切换

(test-case "set_provider anthropic rewrites model; set_model acknowledged"
  (define evs (drive (list (hasheq 'type "set_provider" 'name "anthropic")
                           (hasheq 'type "set_model" 'model "claude-opus-4-8")
                           (hasheq 'type "state")
                           (hasheq 'type "shutdown"))))
  (define sp (find-type evs "ok"))                       ; 第一个 ok = set_provider
  (check-equal? (hash-ref sp 'for) "set_provider")
  (check-equal? (hash-ref sp 'provider) "anthropic")
  (check-equal? (hash-ref sp 'model) "claude-sonnet-5") ; 档案默认 model
  ;; set_model 之后 state 的 model 被覆盖
  (define st (find-type evs "state"))
  (check-equal? (hash-ref st 'model) "claude-opus-4-8")
  (check-equal? (hash-ref st 'provider) "anthropic")
) ; end test-case

;; ------------------------------------------------------------ 错误处理

(test-case "unknown request type and unknown provider yield error events"
  (define evs (drive (list (hasheq 'type "frobnicate")
                           (hasheq 'type "set_provider" 'name "nope")
                           (hasheq 'type "shutdown"))))
  (define errs (filter (lambda (j) (equal? (hash-ref j 'type #f) "error")) evs))
  (check-equal? (length errs) 2)
  (check-true (string-contains? (hash-ref (first errs) 'message) "unknown request type"))
  (check-true (string-contains? (hash-ref (second errs) 'message) "unknown provider"))
) ; end test-case

;; ------------------------------------------------------------ 权限往返

(test-case "normal mode: dangerous tool triggers permission_request; 'yes' proceeds"
  ;; provider 先发一个 bash 工具调用，再（下一轮）纯文本收尾。
  (define script (box (list
    (message 'assistant (list (tool-use-block "c1" "bash" (hasheq 'command "echo hi"))))
    (text-msg 'assistant "done"))))
  (define scripted
    (provider "scripted"
      (lambda (_msgs _tools)
        (define ch (make-async-channel))
        (define m (car (unbox script)))
        (set-box! script (cdr (unbox script)))
        (thread (lambda ()
          (async-channel-put ch (evt:message (now-ms) m))
          (async-channel-put ch (evt:turn-end (now-ms)
                                              (if (null? (message-tool-uses m)) "stop" "tool_calls")
                                              (usage 1 1)))))
        ch)
      void))
  ;; 用真实 bash 工具 + normal 模式 → bash 属 dangerous → ask
  (define cfg (struct-copy config (default-config)
                           [workdir (path->string tmpdir)] [permission-mode 'normal]))
  (define host (make-plugin-host))
  (register-builtin-providers! host)
  (define d (make-deps #:provider scripted #:registry (make-registry (builtin-tools cfg))
                       #:bus (make-bus) #:policy (make-policy cfg) #:plugin-host host))
  (define sess (session-open! (fresh-session-path (path->string tmpdir)) cfg))
  (define in (open-input-string (string-join
    (list (jsexpr->string (hasheq 'type "prompt" 'text "run it"))
          (jsexpr->string (hasheq 'type "permission" 'decision "yes"))
          (jsexpr->string (hasheq 'type "shutdown"))) "\n")))
  (define out (open-output-string))
  (parameterize ([current-input-port in] [current-output-port out])
    (run-rpc! d (make-initial-state cfg) sess #:plugin-host host))
  (define evs (for/list ([ln (in-list (string-split (get-output-string out) "\n"))]
                         #:when (non-empty-string? (string-trim ln)))
                (string->jsexpr ln)))
  (define ts (map (lambda (j) (hash-ref j 'type #f)) evs))
  (check-true (and (member "permission_request" ts) #t))   ; 发起询问
  (check-true (and (member "tool_end" ts) #t))             ; 授权后工具执行
  (define te (findf (lambda (j) (equal? (hash-ref j 'type #f) "tool_end")) evs))
  (check-false (hash-ref te 'is_error))                    ; echo 成功
) ; end test-case

(test-case "set_reasoning updates global effort; state reflects it (max ok)"
  (define evs (drive (list (hasheq 'type "set_reasoning" 'level "max")
                           (hasheq 'type "state")
                           (hasheq 'type "shutdown"))))
  (check-equal? (hash-ref (find-type evs "ok") 'for) "set_reasoning")
  (check-equal? (hash-ref (find-type evs "state") 'reasoning) "max")
  (set-reasoning-effort! 'off)                 ; 复位，防污染同文件其它用例
) ; end test-case

(test-case "set_auto toggles; state carries auto flag"
  (define evs (drive (list (hasheq 'type "set_auto" 'on #f)
                           (hasheq 'type "state")
                           (hasheq 'type "shutdown"))))
  (check-equal? (hash-ref (find-type evs "ok") 'for) "set_auto")
  (check-equal? (hash-ref (find-type evs "state") 'auto) #f)
  (set-auto-mode! #t)                          ; 复位默认
) ; end test-case

(test-case "add_key stores instance token; set_provider then resolves it"
  (define evs (drive (list (hasheq 'type "add_key" 'base "deepseek" 'label "work" 'token "sk-rpc-work")
                           (hasheq 'type "set_provider" 'name "deepseek[work]")
                           (hasheq 'type "state")
                           (hasheq 'type "shutdown"))))
  (define ok1 (findf (lambda (j) (and (equal? (hash-ref j 'type #f) "ok")
                                      (equal? (hash-ref j 'for #f) "add_key"))) evs))
  (check-equal? (hash-ref ok1 'provider) "deepseek[work]")
  (check-equal? (hash-ref (find-type evs "state") 'provider) "deepseek[work]")
  (check-equal? (hash-ref (find-type evs "state") 'model) "deepseek-v4-flash")
) ; end test-case

(test-case "set_escalate toggles; state carries escalate flag"
  (define evs (drive (list (hasheq 'type "set_escalate" 'on #f)
                           (hasheq 'type "state")
                           (hasheq 'type "shutdown"))))
  (check-equal? (hash-ref (find-type evs "ok") 'for) "set_escalate")
  (check-equal? (hash-ref (find-type evs "state") 'escalate) #f)
  (void (drive (list (hasheq 'type "set_escalate" 'on #t) (hasheq 'type "shutdown"))))  ; 复位
) ; end test-case

(test-case "set_fallback sets the on-error chain; state carries it; [] clears"
  (define evs (drive (list (hasheq 'type "set_fallback" 'chain (list "anthropic" "deepseek-v4-flash"))
                           (hasheq 'type "state")
                           (hasheq 'type "shutdown"))))
  (check-equal? (hash-ref (find-type evs "ok") 'for) "set_fallback")
  (check-equal? (hash-ref (find-type evs "state") 'fallback) (list "anthropic" "deepseek-v4-flash"))
  ;; 非法 chain → error；清空复位
  (define evs2 (drive (list (hasheq 'type "set_fallback" 'chain "not-a-list")
                            (hasheq 'type "shutdown"))))
  (check-true (string-contains? (hash-ref (find-type evs2 "error") 'message) "set_fallback"))
  (void (drive (list (hasheq 'type "set_fallback" 'chain '()) (hasheq 'type "shutdown"))))
) ; end test-case

(delete-directory/files tmpdir)
(displayln "rpc-test: all passed")
