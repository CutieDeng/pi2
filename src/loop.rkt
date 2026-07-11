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

;; 一条 assistant 消息里可能含多个工具调用。执行分两相：
;;   预检（preflight）——串行，因权限询问是交互式的，多问并发会撞终端；
;;   执行（run）——当整批都是 read-only（无副作用、无文件竞态）时并发跑，否则按序。
;; 无论并发与否，结果都按原始下标归位，tool-start/tool-end 按序发布，输出确定。
;; 工具异常/未知工具/权限拒绝一律转为 error tool-result，loop 永不因工具崩溃中断。

;; 预检一条调用（串行）。返回 plan：
;;   (list 'done outcome)            — 已决议（未知/钩子拦截/插件拒/权限拒），无副作用
;;   (list 'run t input* parallel?)  — 待执行；parallel? = 该工具 read-only 可并发
(define (preflight-call call d)
  (define name (tool-use-block-name call))
  (define input (tool-use-block-input call))
  (define t (registry-lookup (deps-registry d) name))
  (cond
    [(not t)
     (define avail
       (string-join (map tool-name (registry-tools (deps-registry d))) ", ")
     ) ; end define avail
     (list 'done (err-outcome f"unknown tool: {name}. Available tools: {avail}"))
    ] ; end unknown case
    [((deps-pre-tool-hook d) call)
     => (lambda (reason)
          (list 'done (err-outcome f"tool `{name}` blocked by hook: {reason}"))
        ) ; end lambda
    ] ; end hook-block case
    [else
     ;; 插件 on-tool-call 钩子：可拦截或改写参数。
     (define host (deps-plugin-host d))
     (define-values (decision input*)
       (if host (run-tool-call-hooks host name input) (values 'allow input)))
     (cond
       [(pair? decision)                          ; (cons 'deny reason) 插件拦截
        (list 'done (err-outcome f"tool `{name}` blocked by plugin: {(cdr decision)}"))]
       [else
        (define perm (permission-check (deps-policy d) t input* (deps-asker d)))
        (cond
          [(not (eq? perm 'allow))
           ;; 拒绝：把用户理由（若有）回传给模型，强调不要重试、据此调整。
           (define reason (and (pair? perm) (cdr perm)))
           (list 'done
             (err-outcome
              (if reason
                  f"User denied permission for tool `{name}`. The user's reason: {reason}. Respect this — do not retry the same call; adjust your approach accordingly (or ask the user how to proceed)."
                  f"User denied permission for tool `{name}`. Do not retry the same call; consider a different approach or ask the user how to proceed.")))
          ] ; end deny case
          [else
           (list 'run t input* (eq? (tool-permission-level t) 'read-only))
          ] ; end allow case
        ) ; end cond
       ] ; end not-blocked
     ) ; end cond
    ] ; end else
  ) ; end cond
) ; end define preflight-call

;; 执行一个 'run plan（可能在 worker 线程里跑）。异常兜底为 error outcome。
(define (run-plan t name input* d)
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
  (define host (deps-plugin-host d))
  (if host (run-tool-result-hooks host name input* outcome) outcome)
) ; end define run-plan

;; plan 是否无副作用/可并发：已决议的 done，或 read-only 的 run。
(define (plan-parallel-ok? p)
  (or (eq? (car p) 'done)
      (and (eq? (car p) 'run) (cadddr p))
  ) ; end or
) ; end define plan-parallel-ok?

(define (execute-calls! calls d)
  (define bus (deps-bus d))
  ;; 阶段一：串行预检（交互式权限询问不可并发）。
  (define plans (for/list ([call (in-list calls)]) (preflight-call call d)))
  (define n (length calls))
  (define outcomes (make-vector n #f))
  (define times (make-vector n 0))
  ;; 计算一个 plan 的 outcome（含计时），写回下标 i。
  (define (exec! i call plan)
    (define t0 (now-ms))
    (define oc
      (if (eq? (car plan) 'done)
          (cadr plan)
          (run-plan (cadr plan) (tool-use-block-name call) (caddr plan) d)
      ) ; end if
    ) ; end define oc
    (vector-set! outcomes i oc)
    (vector-set! times i (- (now-ms) t0))
  ) ; end define exec!
  ;; tool-start 按原始顺序发布（阶段二前）。
  (for ([call (in-list calls)]) (bus-publish! bus (evt:tool-start (now-ms) call)))
  ;; 阶段二：整批 read-only → 并发；否则按序（避免读/写文件竞态）。
  (cond
    [(and (> n 1) (andmap plan-parallel-ok? plans))
     (define ts
       (for/list ([call (in-list calls)] [plan (in-list plans)] [i (in-naturals)])
         (thread (lambda () (exec! i call plan)))
       ) ; end for/list
     ) ; end define ts
     (for-each thread-wait ts)
    ] ; end parallel case
    [else
     (for ([call (in-list calls)] [plan (in-list plans)] [i (in-naturals)])
       (exec! i call plan)
     ) ; end for
    ] ; end serial case
  ) ; end cond
  ;; 阶段三：按序发布 tool-end 并构造 result blocks。
  (for/list ([call (in-list calls)] [i (in-naturals)])
    (define oc (vector-ref outcomes i))
    (bus-publish! bus (evt:tool-end (now-ms) call oc (vector-ref times i)))
    (tool-result-block (tool-use-block-id call)
                       (tool-outcome-content oc)
                       (tool-outcome-is-error? oc)
    ) ; end tool-result-block
  ) ; end for/list
) ; end define execute-calls!

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
