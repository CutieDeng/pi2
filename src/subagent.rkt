#lang tstring racket
;; subagent.rkt — spawn_agent 工具（design.md §4.10）
;; 用受限 config + 独立空 history 递归调 run-turn!；深度硬限制为 1。

(require
 racket/string
 racket/pvector
 (file "model.rkt")
 (file "event.rkt")
 (file "provider.rkt")
 (file "tool.rkt")
 (file "permission.rkt")
 (file "loop.rkt")
) ; end require

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
  ) ; end fields
  #:methods gen:tool
  [(define (tool-name _t) "spawn_agent")
   (define (tool-permission-level _t) 'dangerous)
   (define (tool-spec _t)
     (function-spec "spawn_agent"
                    "Spawn an independent sub-agent with a clean context to complete a focused sub-task. Returns the sub-agent's final answer. The sub-agent has its own tools but cannot spawn further agents."
                    (hasheq 'task (hasheq 'type "string"
                                          'description "The sub-task description / prompt for the sub-agent")
                    ) ; end hasheq
                    (list "task")
     ) ; end function-spec
   ) ; end define tool-spec
   (define (tool-run t input ctx)
     (define task (input-str input 'task))
     (cond
       [(not task) (err-outcome "missing required parameter: task")]
       [else
        (define parent-cfg (tool-ctx-config ctx))
        (define sub-cfg
          (struct-copy config parent-cfg
                       [system-prompt SUB-SYSTEM]
                       [turn-max-calls (spawn-agent-tool-max-calls t)]
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
              (define final-st
                (run-turn! (make-initial-state sub-cfg) (text-msg 'user task) sub-d)
              ) ; end define final-st
              (define hist (agent-state-history final-st))
              (define final-msg (pvector-ref hist (sub1 (pvector-length hist))))
              (ok-outcome (message-text final-msg)
                          #:display f"spawn_agent → {(usage-output-tokens (agent-state-token-usage final-st))} out-tokens"
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

;; 构造 spawn_agent 工具。sub-tools 是子 agent 可用的工具列表（会剔除任何 spawn_agent）。
(define (make-spawn-agent-tool #:provider provider #:sub-tools sub-tools
                               #:max-calls [max-calls 12])
  (define clean-tools
    (filter (lambda (t) (not (string=? (tool-name t) "spawn_agent"))) sub-tools)
  ) ; end define clean-tools
  (spawn-agent-tool provider (make-registry clean-tools) max-calls)
) ; end define make-spawn-agent-tool

(provide
 make-spawn-agent-tool
) ; end provide
