#lang tstring racket
;; loop.rkt — agent 主循环（design.md §4.4 / §5.4）
;; 全项目最薄也最核心的一层：组合 context / provider / tool / permission。

(require
 racket/list
 racket/string
 (file "model.rkt")
 (file "event.rkt")
 (file "provider.rkt")
 (file "tool.rkt")
 (file "permission.rkt")
 (file "context.rkt")
 (file "plugin.rkt")
) ; end require

;; 依赖包：一次装配，处处传递
(struct deps
  (provider     ; provider
   registry     ; registry
   bus          ; bus
   policy       ; permission-policy
   asker        ; (-> string (or/c 'yes 'no 'always))
   est-cache    ; context 估算 memo
   pre-tool-hook ; (-> tool-use-block (or/c #f string)) — #f 放行，字符串=拦截原因
   plugin-host  ; (or/c #f plugin-host) — 插件变换型钩子（tool-call/result/before-turn/context）
  ) ; end fields
) ; end struct deps

(define (make-deps #:provider p #:registry reg #:bus bus
                   #:policy policy #:asker [asker (lambda (_q) 'no)]
                   #:pre-tool-hook [pre-tool-hook (lambda (_b) #f)]
                   #:plugin-host [plugin-host #f])
  (deps p reg bus policy asker (make-estimate-cache) pre-tool-hook plugin-host)
) ; end define make-deps

;; ------------------------------------------------- 流收集：单点消费双路分发

;; 消费 provider 通道：delta 转发到 bus 渲染，收尾返回完整消息
(define (stream-and-collect! d window)
  (define ch
    (provider-stream! (deps-provider d) window (registry-specs (deps-registry d)))
  ) ; end define ch
  (define bus (deps-bus d))
  (let loop ([msg #f])
    (define e (sync ch))
    (cond
      [(evt:delta? e)
       (bus-publish! bus e)
       (loop msg)
      ] ; end delta case
      [(evt:message? e)
       (bus-publish! bus e)
       (loop (evt:message-msg e))
      ] ; end message case
      [(evt:turn-end? e)
       (bus-publish! bus e)
       (values msg (evt:turn-end-stop-reason e) (evt:turn-end-usage e))
      ] ; end turn-end case
      [(evt:error? e)
       (bus-publish! bus e)
       (raise (evt:error-exn e))
      ] ; end error case
      [else (loop msg)]
    ) ; end cond
  ) ; end let loop
) ; end define stream-and-collect!

;; ------------------------------------------------------------ 工具执行

;; 串行执行一条 assistant 消息中的全部工具调用，返回 tool-result-block 列表。
;; 工具异常/未知工具/权限拒绝一律转为 error tool-result，loop 永不因工具崩溃中断。
(define (execute-calls! calls d)
  (define bus (deps-bus d))
  (for/list ([call (in-list calls)])
    (bus-publish! bus (evt:tool-start (now-ms) call))
    (define t0 (now-ms))
    (define result (execute-one-call call d))
    (bus-publish! bus (evt:tool-end (now-ms) call result (- (now-ms) t0)))
    (tool-result-block (tool-use-block-id call)
                       (tool-outcome-content result)
                       (tool-outcome-is-error? result)
    ) ; end tool-result-block
  ) ; end for/list
) ; end define execute-calls!

(define (execute-one-call call d)
  (define name (tool-use-block-name call))
  (define input (tool-use-block-input call))
  (define t (registry-lookup (deps-registry d) name))
  (cond
    [(not t)
     (define avail
       (string-join (map tool-name (registry-tools (deps-registry d))) ", ")
     ) ; end define avail
     (err-outcome f"unknown tool: {name}. Available tools: {avail}")
    ] ; end unknown case
    [((deps-pre-tool-hook d) call)
     => (lambda (reason)
          (err-outcome f"tool `{name}` blocked by hook: {reason}")
        ) ; end lambda
    ] ; end hook-block case
    [else
     ;; 插件 on-tool-call 钩子：可拦截或改写参数。
     (define host (deps-plugin-host d))
     (define-values (decision input*)
       (if host (run-tool-call-hooks host name input) (values 'allow input)))
     (cond
       [(pair? decision)                          ; (cons 'deny reason) 插件拦截
        (err-outcome f"tool `{name}` blocked by plugin: {(cdr decision)}")]
       [else
        (define perm (permission-check (deps-policy d) t input* (deps-asker d)))
        (cond
          [(not (eq? perm 'allow))
           ;; 拒绝：把用户理由（若有）回传给模型，强调不要重试、据此调整。
           (define reason (and (pair? perm) (cdr perm)))
           (err-outcome
            (if reason
                f"User denied permission for tool `{name}`. The user's reason: {reason}. Respect this — do not retry the same call; adjust your approach accordingly (or ask the user how to proceed)."
                f"User denied permission for tool `{name}`. Do not retry the same call; consider a different approach or ask the user how to proceed."))
          ] ; end deny case
          [else
           (define outcome
             (with-handlers ([exn:fail?
                              (lambda (e)
                                (err-outcome f"tool `{name}` raised: {(exn-message e)}")
                              ) ; end lambda
                             ]) ; end handlers
               (define cfg (current-loop-config))
               (tool-run t input*
                         (tool-ctx (config-workdir cfg)
                                   (lambda (e) (bus-publish! (deps-bus d) e))
                                   cfg
                         ) ; end tool-ctx
               ) ; end tool-run
             ) ; end with-handlers
           ) ; end define outcome
           ;; 插件 on-tool-result 钩子：可改写结果。
           (if host (run-tool-result-hooks host name input* outcome) outcome)
          ] ; end allow case
        ) ; end cond
       ] ; end not-blocked
     ) ; end cond
    ] ; end else
  ) ; end cond
) ; end define execute-one-call

;; 工具执行与 provider 都需 config；统一用 model.rkt 的 current-config parameter（run-turn! 每轮设置）。
(define current-loop-config current-config)

;; 历史估算 > 1.5×预算时触发一次永久压缩，并把压缩事件报到 bus
(define (maybe-compact st d)
  (define cfg (agent-state-config st))
  (define est (history-tokens (agent-state-history st) (deps-est-cache d)))
  (cond
    [(> est (* 1.5 (config-context-budget cfg)))
     (bus-publish! (deps-bus d)
                   (evt:delta (now-ms) 'text f"\n[compacting context: ~{est} tokens]\n"))
     (with-handlers ([exn:fail? (lambda (_e) st)])   ; 压缩失败不致命，退回原状态
       (compact! st (deps-provider d))
     ) ; end with-handlers
    ] ; end compact case
    [else st]
  ) ; end cond
) ; end define maybe-compact

;; ------------------------------------------------------------- 主循环

;; 驱动一个完整「用户轮」：user-msg 进 → 模型/工具交替 → 终止性回答。
;; 返回新 agent-state。
(define (run-turn! st user-msg d)
  (define cfg (agent-state-config st))
  (define host (deps-plugin-host d))
  (when host (host-set-session! host st))         ; 供插件 ctx.session 读取
  ;; 级别3：历史远超预算时先永久压缩（context-fit 只做透传/中段淘汰）
  (define st-c (maybe-compact st d))
  ;; 插件 before-turn 钩子：在 user-msg 后注入上下文消息。
  (define st-injected
    (if host
        (for/fold ([s (state-append st-c user-msg)]) ([m (in-list (run-before-turn-hooks host st-c))])
          (state-append s m))
        (state-append st-c user-msg)))
  (parameterize ([current-loop-config cfg])
    (let step ([st st-injected] [ncalls 0])
      (define window0 (context-fit (agent-state-history st) cfg
                                   #:cache (deps-est-cache d)
                      ) ; end context-fit
      ) ; end define window0
      ;; 插件 on-context 钩子：改写发送窗口。
      (define window (if host (run-context-hooks host window0) window0))
      (define-values (asst-msg _stop u) (stream-and-collect! d window))
      (define st*
        (state-add-usage (state-append st asst-msg) u)
      ) ; end define st*
      (define calls (message-tool-uses asst-msg))
      (cond
        [(null? calls)
         (struct-copy agent-state st*
                      [turn-count (add1 (agent-state-turn-count st*))]
         ) ; end struct-copy
        ] ; end terminal case
        [(> (+ ncalls (length calls)) (config-turn-max-calls cfg))
         ;; 预算超限：注入提示让模型收尾，而非硬掐断
         (define notice
           (message 'user
                    (append
                     (for/list ([c (in-list calls)])
                       (tool-result-block (tool-use-block-id c)
                                          "tool budget for this turn exhausted; do not call more tools"
                                          #t
                       ) ; end tool-result-block
                     ) ; end for/list
                     (list (text-block "Tool budget exhausted. Summarize and answer now without more tool calls."))
                    ) ; end append
           ) ; end message
         ) ; end define notice
         (define-values (final-msg _s u2)
           (stream-and-collect! d
                                (context-fit (agent-state-history (state-append st* notice)) cfg
                                             #:cache (deps-est-cache d)
                                ) ; end context-fit
           ) ; end stream-and-collect!
         ) ; end define-values
         (define st**
           (state-add-usage (state-append (state-append st* notice) final-msg) u2)
         ) ; end define st**
         (struct-copy agent-state st**
                      [turn-count (add1 (agent-state-turn-count st**))]
         ) ; end struct-copy
        ] ; end budget case
        [else
         (define results (execute-calls! calls d))
         (step (state-append st* (message 'user results))
               (+ ncalls (length calls))
         ) ; end step
        ] ; end tool case
      ) ; end cond
    ) ; end let step
  ) ; end parameterize
) ; end define run-turn!

;; ---------------------------------------------------------------- provide

(provide
 (struct-out deps)
 make-deps
 run-turn!
 stream-and-collect!
 execute-calls!
) ; end provide
