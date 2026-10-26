#lang tstring racket
;; subagent.rkt — spawn_agent 工具（design.md §4.10）
;; 用受限 config + 独立空 history 递归调 run-turn!；深度硬限制为 1。
;; host 参数（跨主机子 agent）：在主机表（hosts.rkt）里那台上起一次性的 pi2 内核跑这个任务（remote-agent.rkt），凭证缺省本机签发。

(require
 racket/string
 racket/pvector
 (file "model.rkt")
 (file "event.rkt")
 (file "provider.rkt")
 (file "tool.rkt")
 (file "permission.rkt")
 (file "loop.rkt")
 (file "aliases.rkt")                   ; 模型别名表
 (file "route.rkt")                     ; current-model-override（走路由池时：帐号按亲和，只换模型）
 (file "hosts.rkt")                     ; 主机表（跨主机子 agent）
 (file "remote-agent.rkt")
 (file "budget.rkt")                    ; 远端花的是本机的 key 时记进本机账
) ; end require

;; 模型别名（aliases.rkt）：父代理用别名或模型名给子 agent 选模型；帐号 / 端点始终跟父会话（亲和用户帐号）。

(define SUB-SYSTEM
  (string-join
   '("You are a sub-agent spawned to complete one focused task."
     "Use tools to accomplish it, then give a concise final answer."
     "You cannot spawn further sub-agents."
    ) ; end list
   " "
  ) ; end string-join
) ; end define SUB-SYSTEM

;; spawn_agent 工具：
;;   sub-provider : 子 agent 用的 provider
;;   sub-registry : 子 agent 的工具集（不含 spawn_agent，防止再派生）
(struct spawn-agent-tool
  (sub-provider
   sub-registry
   max-calls
   sub-max-tokens       ; 子 agent 输出上限（收紧，建议③）
   sub-context-budget   ; 子 agent 发送窗口预算（收紧，建议③）
  ) ; end fields
  #:methods gen:tool
  [(define (tool-name _t) "spawn_agent")
   (define (tool-permission-level _t) 'dangerous)
   (define (tool-spec _t)
     ;; 每次请求重算（别名表可能改了）
     (define aliases (model-aliases))
     (define hosts (filter (lambda (h) (or (hash-ref h 'ssh #f) (hash-ref h 'command #f))) (load-hosts)))
     (function-spec "spawn_agent"
                    "Spawn an independent sub-agent with a clean context to complete a focused sub-task. Returns the sub-agent's final answer. The sub-agent has its own tools but cannot spawn further agents."
                    (hasheq 'task (hasheq 'type "string"
                                          'description "The sub-task description / prompt for the sub-agent")
                            'model (hasheq 'type "string"
                                           'description (string-append
                                                         "Optional model for the sub-agent (same account as you). Default: your own model."
                                                         (if (null? aliases) " Give a model id of the current provider."
                                                             (string-append " Aliases: "
                                                                            (string-join (for/list ([a (in-list aliases)]) (format "~a = ~a" (car a) (cdr a))) ", ")
                                                                            "; a model id also works."))))
                            'host (hasheq 'type "string"
                                          'description (if (null? hosts)
                                                           "Optional host to run the sub-agent on (none configured: it runs here)."
                                                           (string-append "Optional: run the sub-agent on another machine (its own files and tools there; "
                                                                          "model calls still use your account). Hosts: "
                                                                          (string-join (for/list ([h (in-list hosts)])
                                                                                         (format "~a~a" (hash-ref h 'id)
                                                                                                 (let ([w (hash-ref h 'workdir #f)]) (if w (format " (workdir ~a)" w) ""))))
                                                                                       ", ")
                                                                          ". Omit to run here.")))
                            'workdir (hasheq 'type "string" 'description "Optional working directory on that host (with host)")
                    ) ; end hasheq
                    (list "task")
     ) ; end function-spec
   ) ; end define tool-spec
   (define (tool-run t input ctx)
     (define task (input-str input 'task))
     (define host (let ([h (input-str input 'host)]) (and h (not (string=? (string-trim h) "")) (string-trim h))))
     (cond
       [(not task) (err-outcome "missing required parameter: task")]
       [host (run-on-host host task input ctx)]
       [else
        (define parent-cfg (tool-ctx-config ctx))
        (define model-arg (let ([m (input-str input 'model)]) (and m (not (string=? (string-trim m) "")) (resolve-model-alias (string-trim m)))))
        (define sub-cfg
          (struct-copy config parent-cfg
                       [model (or model-arg (config-model parent-cfg))]
                       [system-prompt SUB-SYSTEM]
                       [turn-max-calls (spawn-agent-tool-max-calls t)]
                       ;; 收紧上下文（建议③）：取与父的较小值，绝不超过父。
                       [max-tokens (min (config-max-tokens parent-cfg)
                                        (spawn-agent-tool-sub-max-tokens t))]
                       [context-budget (min (config-context-budget parent-cfg)
                                            (spawn-agent-tool-sub-context-budget t))]
                       [permission-mode 'yolo]     ; 子 agent 在受限工具集内自主执行
          ) ; end struct-copy
        ) ; end define sub-cfg
        ;; 子 agent 事件转发到父 bus（渲染时可加缩进标记），跑在独立 custodian
        (define sub-bus (make-bus))
        (define unsub
          (bus-subscribe! sub-bus
            (lambda (e) ((tool-ctx-publish! ctx) e))
          ) ; end bus-subscribe!
        ) ; end define unsub
        (define sub-d
          (make-deps #:provider (spawn-agent-tool-sub-provider t)
                     #:registry (spawn-agent-tool-sub-registry t)
                     #:bus sub-bus
                     #:policy (make-policy sub-cfg)
          ) ; end make-deps
        ) ; end define sub-d
        (define cust (make-custodian))
        (define result
          (parameterize ([current-custodian cust])
            (with-handlers ([exn:fail?
                             (lambda (ex)
                               (err-outcome f"sub-agent failed: {(exn-message ex)}")
                             ) ; end lambda
                            ]) ; end handlers
              ;; 子 agent 的记录：不进父会话的历史（模型上下文），而是作为 sub 记录挂在父工具调用下（界面嵌套显示；重放跳过）
              (define parent-sink (current-record-sink))
              (define parent-call (current-tool-call-id))
              (define sub-sink (and parent-sink parent-call
                                    (lambda (kind payload) (parent-sink 'sub (hasheq 'parent parent-call 'kind kind 'data payload)))))
              (define final-st
                (parameterize ([current-model-override model-arg]             ; 走路由池时也换成这个模型
                               [current-record-sink sub-sink])
                  (run-turn! (make-initial-state sub-cfg) (text-msg 'user task) sub-d))
              ) ; end define final-st
              (define hist (agent-state-history final-st))
              (define final-msg (pvector-ref hist (sub1 (pvector-length hist))))
              (add-extra-usage! (agent-state-token-usage final-st))   ; 子 agent 的用量并进父会话
              (ok-outcome (message-text final-msg)
                          #:display f"spawn_agent ({(config-model sub-cfg)}) → {(usage-output-tokens (agent-state-token-usage final-st))} out-tokens"
              ) ; end ok-outcome
            ) ; end with-handlers
          ) ; end parameterize
        ) ; end define result
        (bus-drain! sub-bus)
        (unsub)
        (custodian-shutdown-all cust)
        result
       ] ; end else
     ) ; end cond
   ) ; end define tool-run
  ] ; end methods
) ; end struct spawn-agent-tool

;; 跨主机：主机表里那台上起一次性内核跑任务；用量并进父会话；花的是本机的 key（proxy / share）就记进本机账
(define (run-on-host host task input ctx)
  (define entry (host-entry host))
  (cond
    [(not entry) (err-outcome (format "unknown host ~a (hosts: ~a)" host (string-join (map (lambda (h) (hash-ref h 'id)) (load-hosts)) ", ")))]
    [else
     (define parent-cfg (tool-ctx-config ctx))
     (define model-arg (let ([m (input-str input 'model)]) (and m (not (string=? (string-trim m) "")) (resolve-model-alias (string-trim m)))))
     ;; 本轮还剩的费用额度给转发当上限（没设每轮上限 → 不限，由远端 / 每天上限管）
     (define remaining (let ([tc (turn-cap)] [tb (current-turn-budget)]) (and tc tb (max 0.01 (- tc (vector-ref tb 0))))))
     (define-values (text u outcome detail)
       (with-handlers ([exn:fail? (lambda (e) (values (exn-message e) (usage 0 0) 'error (hasheq)))])
         (run-remote-agent entry task #:parent-config parent-cfg #:model model-arg #:workdir (input-str input 'workdir)
                           #:budget remaining
                           #:progress (lambda (s) ((tool-ctx-publish! ctx) (evt:delta (now-ms) 'text (format "\n[~a]\n" s)))))))
     (add-extra-usage! u)
     ;; 决定 4：远端子 agent 的过程拉回一份副本存本机（远端会话投影成的条目，作为 sub 记录挂在父调用下）
     (let ([sink (current-record-sink)] [parent (current-tool-call-id)] [copy (hash-ref detail 'items '())])
       (when (and sink parent (list? copy))
         (for ([it (in-list copy)] #:when (hash? it))
           (sink 'sub (hasheq 'parent parent 'host host 'item (hash-remove it 'type))))))
     (when (and (member (hash-ref entry 'credential "proxy") '("proxy" "share")) (memq outcome '(ok error)))
       (record-spend! (or model-arg (config-model parent-cfg)) u))
     (define tools (hash-ref detail 'remote_tools '()))
     (define summary (format "spawn_agent @~a (~a) → ~a out-tokens~a~a" host (or model-arg (config-model parent-cfg)) (usage-output-tokens u)
                             (if (null? tools) "" (format " · remote tools: ~a" (string-join (remove-duplicates tools) ", ")))
                             (let ([p (hash-ref detail 'proxy #f)]) (if (hash? p) (format " · key proxy: ~a requests" (hash-ref p 'requests 0)) ""))))
     (case outcome
       [(ok) (ok-outcome text #:display summary)]
       [else (err-outcome (format "sub-agent on ~a failed: ~a~a" host text
                                  (let ([e (hash-ref detail 'stderr '())]) (if (null? e) "" (format "\n(stderr: ~a)" (string-join e " | "))))))])])
) ; end define run-on-host

;; 构造 spawn_agent 工具。sub-tools 是子 agent 可用的工具列表（会剔除任何 spawn_agent）。
;; #:max-tokens / #:context-budget 收紧子 agent 上下文（建议③；对父取 min，不超父）。
(define (make-spawn-agent-tool #:provider provider #:sub-tools sub-tools
                               #:max-calls [max-calls 12]
                               #:max-tokens [max-tokens 1024]
                               #:context-budget [context-budget 4096])
  (define clean-tools
    (filter (lambda (t) (not (string=? (tool-name t) "spawn_agent"))) sub-tools)
  ) ; end define clean-tools
  (spawn-agent-tool provider (make-registry clean-tools) max-calls max-tokens context-budget)
) ; end define make-spawn-agent-tool

(provide
 make-spawn-agent-tool
) ; end provide
