#lang tstring racket
;; rpc.rkt — 无头 JSONL 模式（--rpc）：供 IDE / 编排器以进程管道驱动 pi++。
;;
;; 设计原则（慎重、不干扰内核）：
;;   * 完全复用内核——bus 事件 + run-turn! + session；不改 loop/model/provider 一行。
;;   * 本模块只是又一个「事件汇 + 请求驱动器」，与交互式 repl、一次性 -p 并列。
;;   * 单线程、请求→流式→响应：一条 prompt 跑完整轮再读下一条请求。无中途取消（v1）。
;;
;; 协议（NDJSON，各方向每行一个 JSON 对象）：
;;   → 请求 (stdin)：
;;       {"type":"prompt","text":"..."}          跑一个用户轮
;;       {"type":"set_model","model":"..."}       切模型
;;       {"type":"set_provider","name":"..."}     切供应商（含实例名 base[label]；重写 endpoint/key/model）
;;       {"type":"set_reasoning","level":"off|low|medium|high|max"}  设推理强度
;;       {"type":"set_auto","on":true|false}       开/关 Auto 模式（DeepSeek 按任务切 flash/pro）
;;       {"type":"set_escalate","on":true|false}    开/关失败驱动模型升级（DeepSeek: flash→pro→max）
;;       {"type":"add_key","base":"deepseek","label":"work","token":"sk-..."}  存实例 token
;;       {"type":"set_fallback","chain":["anthropic","deepseek-v4-flash"]}     设 on-error 回退链（[] 清空）
;;       {"type":"goal","goal":"...","until":["python3 -m unittest"],"max_turns":20,"budget":0.5}  headless Goal 模式
;;         → 流式 goal_start / (turn 事件) / goal_status / goal_end
;;       {"type":"state"}                          查询模型/供应商/轮次/用量
;;       {"type":"history"}                        导出当前历史（role/text）
;;       {"type":"permission","decision":"yes|no|always|no-reason","reason":"..."}
;;                                                  应答 permission_request（仅轮内）
;;       {"type":"shutdown"}                       优雅退出
;;   ← 事件/响应 (stdout)：
;;       ready / delta / tool_start / tool_end / message / turn_end / error
;;       / permission_request / turn_complete / ok / state / history / bye
;;
;; 权限：yolo 模式 run-turn! 不询问；normal/strict 遇 ask 时发 permission_request 并
;;   阻塞读取下一行 permission 响应（客户端须在该轮内应答）。

(require
 json
 racket/string
 racket/pvector
 (file "model.rkt")
 (file "event.rkt")
 (file "tool.rkt")
 (file "provider.rkt")
 (file "loop.rkt")
 (file "session.rkt")
 (file "plugin.rkt")
 (file "providers.rkt")
 (file "credentials.rkt")               ; 实例 token 写入
 (file "pricing.rkt")                   ; 记费：估算 token 开销
 (file "auto.rkt")                      ; Auto 模式
 (file "escalate.rkt")                  ; 自适应：失败驱动模型升级梯
 (file "retry.rkt")                     ; 增强式回退：回退链读写
 (file "goal.rkt")                      ; Goal 模式：headless 驱动
) ; end require

;; ---------------------------------------------------------------- 事件 → JSON

(define (usage->jsexpr u)
  (hasheq 'input (usage-input-tokens u) 'output (usage-output-tokens u))
) ; end define usage->jsexpr

;; 估算美元开销 → jsexpr（未知模型 → 'null，客户端据此显示 n/a）。
(define (cost->jsexpr model u)
  (define c (estimate-cost model u))
  (if c c 'null)
) ; end define cost->jsexpr

(define (tool-use->jsexpr b)
  (hasheq 'id (tool-use-block-id b) 'name (tool-use-block-name b)
          'input (tool-use-block-input b))
) ; end define tool-use->jsexpr

;; bus 事件 → jsexpr（#f = 跳过）。仅 assistant message 外发（user 工具回填由 tool_end 覆盖）。
(define (event->jsexpr e)
  (cond
    [(evt:delta? e)
     (hasheq 'type "delta" 'kind (symbol->string (evt:delta-kind e)) 'text (evt:delta-text e))]
    [(evt:tool-start? e)
     (define b (evt:tool-start-block e))
     (hasheq 'type "tool_start" 'id (tool-use-block-id b) 'name (tool-use-block-name b)
             'input (tool-use-block-input b))]
    [(evt:tool-end? e)
     (define b (evt:tool-end-block e))
     (define r (evt:tool-end-result e))
     (hasheq 'type "tool_end" 'id (tool-use-block-id b) 'name (tool-use-block-name b)
             'is_error (tool-outcome-is-error? r) 'ms (evt:tool-end-ms e)
             'content (tool-outcome-content r))]
    [(evt:message? e)
     (define m (evt:message-msg e))
     (if (eq? (message-role m) 'assistant)
         (hasheq 'type "message" 'role "assistant" 'text (message-text m)
                 'tool_uses (map tool-use->jsexpr (message-tool-uses m)))
         #f)]
    [(evt:turn-end? e)
     (hasheq 'type "turn_end" 'stop_reason (or (evt:turn-end-stop-reason e) "stop")
             'usage (usage->jsexpr (evt:turn-end-usage e)))]
    [(evt:error? e)
     (hasheq 'type "error" 'message (exn-message (evt:error-exn e))
             'recoverable (evt:error-recoverable? e))]
    [else #f]
  ) ; end cond
) ; end define event->jsexpr

;; ---------------------------------------------------------------- 小工具

(define (jget h k [d #f]) (if (hash? h) (hash-ref h k d) d))

(define (parse-req line)
  (with-handlers ([exn:fail? (lambda (_e) #f)]) (string->jsexpr line))
) ; end define parse-req

;; 轻量持久化（不依赖 repl/TUI）：落盘本轮新增的 history 消息与用量增量。
(define (persist! sess st-before st-after)
  (define before (pvector-length (agent-state-history st-before)))
  (define hist (agent-state-history st-after))
  (for ([i (in-range before (pvector-length hist))])
    (session-append-msg! sess (pvector-ref hist i)))
  (define ub (agent-state-token-usage st-before))
  (define ua (agent-state-token-usage st-after))
  (define du (usage (- (usage-input-tokens ua) (usage-input-tokens ub))
                    (- (usage-output-tokens ua) (usage-output-tokens ub))))
  (unless (equal? du usage-zero) (session-append-usage! sess du))
) ; end define persist!

;; ---------------------------------------------------------------- 服务器

;; run-rpc! : deps agent-state session -> void（复用内核；返回时会话已关闭）
(define (run-rpc! d st0 sess #:plugin-host host)
  ;; 写一行 JSON；带锁，因 bus 在独立消费线程上回调 emit。
  (define out-lock (make-semaphore 1))
  (define (emit! j)
    (call-with-semaphore out-lock
      (lambda () (write-json j) (newline) (flush-output))))
  ;; 权限询问：发 request，阻塞读下一行 permission 响应（忽略其它行）。
  (define (rpc-asker prompt)
    (emit! (hasheq 'type "permission_request" 'prompt prompt))
    (let loop ()
      (define line (read-line (current-input-port) 'any))
      (cond
        [(eof-object? line) 'no]
        [else
         (define r (parse-req line))
         (cond
           [(and (hash? r) (equal? (jget r 'type) "permission"))
            (case (jget r 'decision "no")
              [("yes") 'yes]
              [("always") 'always]
              [("no-reason") (cons 'no (jget r 'reason ""))]
              [else 'no])]
           [else (loop)])])))   ; 非 permission 行：忽略，继续等
  ;; 用 RPC asker 覆盖 deps 的交互式 asker（不改内核，仅 struct-copy）。
  (define d* (struct-copy deps d [asker rpc-asker]))
  (define unsub
    (bus-subscribe! (deps-bus d)
      (lambda (e) (define j (event->jsexpr e)) (when j (emit! j)))))
  (define (shutdown!) (bus-drain! (deps-bus d)) (unsub) (session-close! sess))
  (emit! (hasheq 'type "ready"
                 'model (config-model (agent-state-config st0))
                 'provider (host-current-provider host)))
  (let loop ([st st0])
    (define line (read-line (current-input-port) 'any))
    (cond
      [(eof-object? line) (shutdown!)]
      [(string=? (string-trim line) "") (loop st)]
      [else
       (define req (parse-req line))
       (define type (and (hash? req) (jget req 'type)))
       (case type
         [("prompt")
          (define text (jget req 'text ""))
          ;; Auto 模式（仅 DeepSeek 生效）：按任务改 model + 推理档。
          (define-values (st1 auto-dec) (maybe-apply-auto st (if (string? text) text "") host))
          (when auto-dec
            (emit! (hasheq 'type "auto" 'model (car auto-dec) 'reasoning (symbol->string (cdr auto-dec)))))
          (define st*
            (with-handlers ([exn:fail?
                             (lambda (e)
                               (emit! (hasheq 'type "error" 'message (exn-message e)))
                               st1)])
              (run-turn! st1 (text-msg 'user (if (string? text) text "")) d*)))
          (bus-drain! (deps-bus d))
          (persist! sess st1 st*)
          (emit! (hasheq 'type "turn_complete" 'turn (agent-state-turn-count st*)
                         'usage (usage->jsexpr (agent-state-token-usage st*))
                         'cost_usd (cost->jsexpr (config-model (agent-state-config st*))
                                                 (agent-state-token-usage st*))))
          (loop st*)]
         [("goal")
          ;; headless Goal 模式:自主多轮直到 until 全过/轮数耗尽/预算耗尽。turn 流式事件仍走 bus,
          ;; 另加 goal_start / goal_status(驱动状态行) / goal_end。
          (define goal (jget req 'goal))
          (define until (jget req 'until))
          (define mt (jget req 'max_turns 20))
          (define budget (jget req 'budget))
          (cond
            [(not (and (string? goal) (list? until) (pair? until) (andmap string? until)))
             (emit! (hasheq 'type "error" 'message "goal requires string 'goal and non-empty string list 'until"))
             (loop st)]
            [else
             (define max-turns (if (exact-positive-integer? mt) mt 20))
             (emit! (hasheq 'type "goal_start" 'goal goal 'until until 'max_turns max-turns))
             (define (strip s) (regexp-replace* #rx"\e\\[[0-9;]*m" s ""))
             (define st*
               (with-handlers ([exn:fail? (lambda (e) (emit! (hasheq 'type "error" 'message (exn-message e))) st)])
                 (run-goal! d* st sess goal until max-turns host
                            #:budget (and (real? budget) (> budget 0) budget)
                            #:emit (lambda (s) (emit! (hasheq 'type "goal_status" 'text (strip s)))))))
             (emit! (hasheq 'type "goal_end"
                            'model (config-model (agent-state-config st*))
                            'turn (agent-state-turn-count st*)
                            'usage (usage->jsexpr (agent-state-token-usage st*))
                            'cost_usd (cost->jsexpr (config-model (agent-state-config st*))
                                                    (agent-state-token-usage st*))))
             (loop st*)])]
         [("set_model")
          (define m (jget req 'model))
          (cond
            [(string? m)
             (emit! (hasheq 'type "ok" 'for "set_model" 'model m))
             (loop (struct-copy agent-state st
                                [config (struct-copy config (agent-state-config st) [model m])]))]
            [else (emit! (hasheq 'type "error" 'message "set_model requires string model")) (loop st)])]
         [("set_provider")
          (define name (jget req 'name))
          (cond
            [(not (and (string? name) (host-set-provider! host name)))
             (emit! (hasheq 'type "error" 'message f"unknown provider: {name}")) (loop st)]
            [(builtin-provider-instance? name)
             (define c* (apply-provider-profile (agent-state-config st) name))
             (emit! (hasheq 'type "ok" 'for "set_provider" 'provider name 'model (config-model c*)))
             (loop (struct-copy agent-state st [config c*]))]
            [else
             (emit! (hasheq 'type "ok" 'for "set_provider" 'provider name))
             (loop st)])]
         [("set_auto")
          (define on? (jget req 'on))
          (cond
            [(boolean? on?)
             (set-auto-mode! on?)
             (emit! (hasheq 'type "ok" 'for "set_auto" 'auto on?)) (loop st)]
            [else (emit! (hasheq 'type "error" 'message "set_auto requires boolean 'on")) (loop st)])]
         [("add_key")
          ;; 存一条实例 token：{base, label?, token}。若为当前实例则刷新 config 的 api-key。
          (define base (jget req 'base))
          (define label (let ([l (jget req 'label)]) (if (string? l) l "default")))
          (define tok (jget req 'token))
          (cond
            [(not (and (string? base) (builtin-provider-name? base) (string? tok) (> (string-length tok) 0)))
             (emit! (hasheq 'type "error" 'message "add_key requires builtin base + non-empty token")) (loop st)]
            [else
             (store-instance-key! base label tok)
             (define disp (instance-display base label))
             (emit! (hasheq 'type "ok" 'for "add_key" 'provider disp))
             (if (string=? (host-current-provider host) disp)
                 (loop (struct-copy agent-state st [config (apply-provider-profile (agent-state-config st) disp)]))
                 (loop st))])]
         [("set_reasoning")
          (define lvl (jget req 'level))
          (cond
            [(and (string? lvl) (valid-reasoning-effort? (string->symbol lvl)))
             (set-reasoning-effort! (string->symbol lvl))
             (emit! (hasheq 'type "ok" 'for "set_reasoning" 'level lvl))
             (loop st)]
            [else
             (emit! (hasheq 'type "error" 'message "set_reasoning requires off|low|medium|high"))
             (loop st)])]
         [("set_escalate")
          ;; {"type":"set_escalate","on":true|false} 开/关失败驱动模型升级
          (define on? (jget req 'on))
          (cond
            [(boolean? on?)
             (set-escalate! on?)
             (emit! (hasheq 'type "ok" 'for "set_escalate" 'escalate on?)) (loop st)]
            [else (emit! (hasheq 'type "error" 'message "set_escalate requires boolean 'on")) (loop st)])]
         [("set_fallback")
          ;; {"type":"set_fallback","chain":["anthropic","deepseek-v4-flash"]}；[] 清空。
          (define ch (jget req 'chain))
          (cond
            [(and (list? ch) (andmap string? ch))
             (set-fallback-chain! ch)
             (emit! (hasheq 'type "ok" 'for "set_fallback" 'chain (fallback-chain)))
             (loop st)]
            [else
             (emit! (hasheq 'type "error" 'message "set_fallback requires string list 'chain"))
             (loop st)])]
         [("state")
          (define c (agent-state-config st))
          (emit! (hasheq 'type "state" 'model (config-model c)
                         'provider (host-current-provider host)
                         'reasoning (symbol->string (current-reasoning-effort))
                         'auto (auto-mode-on?)
                         'escalate (escalate-on?)
                         'fallback (fallback-chain)
                         'turn (agent-state-turn-count st)
                         'messages (pvector-length (agent-state-history st))
                         'usage (usage->jsexpr (agent-state-token-usage st))
                         'cost_usd (cost->jsexpr (config-model c)
                                                 (agent-state-token-usage st))))
          (loop st)]
         [("history")
          (emit! (hasheq 'type "history" 'messages
                         (for/list ([m (in-pvector (agent-state-history st))])
                           (hasheq 'role (symbol->string (message-role m))
                                   'text (message-text m)))))
          (loop st)]
         [("shutdown") (emit! (hasheq 'type "bye")) (shutdown!)]
         [else
          (emit! (hasheq 'type "error" 'message f"unknown request type: {type}"))
          (loop st)])])))
;; end define run-rpc!

;; ---------------------------------------------------------------- provide

(provide
 run-rpc!
 event->jsexpr
) ; end provide
