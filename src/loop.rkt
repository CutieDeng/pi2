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
 (file "retry.rkt")                      ; 增强式回退：失败分类/决策/回退目标应用
 (file "escalate.rkt")                   ; 自适应：失败驱动的模型升级梯
 (file "dsv4b.rkt")                      ; dsv4-b：Minimal 锚定档案的两阶段促迁
 (file "secrets.rkt")                    ; 工具输出里的密钥原值 → 句柄
 (file "budget.rkt")                     ; 费用软上限：请求前检查、请求后记账
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
  (parameterize ([current-asker (deps-asker d)])          ; 工具里（远端子 agent）转发权限询问用
    (execute-calls/asker! calls d))
) ; end define execute-calls!

(define (execute-calls/asker! calls d)
  (define bus (deps-bus d))
  ;; 阶段一：串行预检（交互式权限询问不可并发）。
  (define plans (for/list ([call (in-list calls)]) (preflight-call call d)))
  (define n (length calls))
  (define outcomes (make-vector n #f))
  (define times (make-vector n 0))
  ;; 计算一个 plan 的 outcome（含计时），写回下标 i。
  (define progress (current-call-progress))
  (define (exec! i call plan)
    (define t0 (now-ms))
    (when progress (hash-set! progress (tool-use-block-id call) (cons 'running t0)))
    (define oc
      (if (eq? (car plan) 'done)
          (cadr plan)
          (parameterize ([current-tool-call-id (tool-use-block-id call)])   ; 子 agent 的记录挂在这个调用下（P5）
            (run-plan (cadr plan) (tool-use-block-name call) (caddr plan) d))
      ) ; end if
    ) ; end define oc
    (vector-set! outcomes i oc)
    (vector-set! times i (- (now-ms) t0))
    (when progress (hash-set! progress (tool-use-block-id call) (cons 'done oc)))
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
     ;; 被中断时把 break 转给工作线程（否则它们在后台继续跑）
     (with-handlers ([exn:break? (lambda (e) (for-each break-thread ts) (raise e))])
       (for-each thread-wait ts))
    ] ; end parallel case
    [else
     (for ([call (in-list calls)] [plan (in-list plans)] [i (in-naturals)])
       (exec! i call plan)
     ) ; end for
    ] ; end serial case
  ) ; end cond
  ;; 阶段三：按序发布 tool-end 并构造 result blocks。
  ;; 工具输出里出现的已知密钥原值 → ⟦secret:句柄⟧（给模型、界面、会话文件、操作日志的都是替换后的）
  (for/list ([call (in-list calls)] [i (in-naturals)])
    (define oc (let ([o (vector-ref outcomes i)])
                 (if (string? (tool-outcome-content o)) (struct-copy tool-outcome o [content (redact-secrets (tool-outcome-content o))]) o)))
    (bus-publish! bus (evt:tool-end (now-ms) call oc (vector-ref times i)))
    ;; 会话记录：工具的耗时、是否出错、给用户看的摘要（结果正文在随后的工具结果消息里）
    (record! 'tool (hasheq 'id (tool-use-block-id call) 'name (tool-use-block-name call) 'ms (vector-ref times i)
                           'error (and (tool-outcome-is-error? oc) #t) 'display (or (tool-outcome-display oc) 'null)
                           'diff (or (tool-outcome-diff oc) 'null)                 ; 改文件的工具：diff（界面显示，不回填模型）
                           'source ((current-tool-source) (tool-use-block-name call))))
    (tool-result-block (tool-use-block-id call)
                       (tool-outcome-content oc)
                       (tool-outcome-is-error? oc)
    ) ; end tool-result-block
  ) ; end for/list
) ; end define execute-calls/asker!

;; ---------------------------------------------------------------- 中断检查点
;; 中断语义（参照 Codex：openai/codex codex-rs/core 的 record_completed_response_item / ensure_call_outputs_present /
;; TurnAborted）：保留本轮已完成的步骤，丢弃流式到一半的文本，没出结果的工具调用补一条 aborted 结果，
;; 最后追加模型可见的 <turn_aborted> 标记，防止模型下一轮重复已执行过的副作用。
;; 调用方 parameterize current-turn-checkpoint 为一个 box 才会记录（TUI 不设，行为不变）。
;; box 内容：(vector 状态 未出结果的调用表 调用进度)；调用进度 hash：id → (cons 'running t0) | (cons 'done outcome)

(define current-turn-checkpoint (make-parameter #f))
(define current-call-progress (make-parameter #f))
;; 工具来源（会话记录 / 界面标记）：工具名 → "builtin" | "plugin:名" | "mcp:服务器" | "remote:主机"；前端按插件表设置
(define current-tool-source (make-parameter (lambda (_name) "builtin")))
;; 正在跑的工具调用的 id（工具里起的子 agent 把自己的记录挂在它下面）
(define current-tool-call-id (make-parameter #f))

;; 工具执行期间额外产生的用量（子 agent 的模型调用）：一步工具执行一个 box，工具（含并行线程里的）往里加，
;; 执行完并进父会话的用量。#f = 不收集。
(define current-extra-usage (make-parameter #f))
(define extra-usage-sema (make-semaphore 1))
(define (add-extra-usage! u)
  (define b (current-extra-usage))
  (when (and b (usage? u))
    (call-with-semaphore extra-usage-sema
      (lambda () (set-box! b (usage (+ (usage-input-tokens (unbox b)) (usage-input-tokens u))
                                    (+ (usage-output-tokens (unbox b)) (usage-output-tokens u))))))))

(define (checkpoint! st calls progress)
  (define cp (current-turn-checkpoint))
  (when cp (set-box! cp (vector st calls progress)))
) ; end define checkpoint!

(define TURN-ABORTED-MARKER
  (string-append "<turn_aborted>\nThe user interrupted the previous turn on purpose. "
                 "If any tools/commands were aborted, they may have partially executed.\n</turn_aborted>"))

;; 由检查点构造中断后的状态；没有检查点 → #f（调用方回退到轮前状态）
(define BUDGET-STOP-MARKER
  (string-append "<turn_aborted>\nThe previous turn was stopped because the spending cap set by the user was reached. "
                 "If any tools/commands were aborted, they may have partially executed.\n</turn_aborted>"))

(define (interrupted-turn-state cp-box [marker TURN-ABORTED-MARKER])
  (define v (and cp-box (unbox cp-box)))
  (cond
    [(not v) #f]
    [else
     (define st (vector-ref v 0))
     (define calls (vector-ref v 1))
     (define progress (vector-ref v 2))
     (define t-now (now-ms))
     (define st1
       (if (null? calls)
           st
           (state-append
            st
            (message 'user
                     (for/list ([c (in-list calls)])
                       (define id (tool-use-block-id c))
                       (define pr (and progress (hash-ref progress id #f)))
                       (cond
                         [(and pr (eq? (car pr) 'done))
                          (tool-result-block id (tool-outcome-content (cdr pr)) (tool-outcome-is-error? (cdr pr)))]
                         [(and pr (eq? (car pr) 'running))
                          (tool-result-block id (format "aborted by user after ~as" (/ (round (/ (- t-now (cdr pr)) 100.0)) 10.0)) #t)]
                         [else (tool-result-block id "aborted before dispatch" #t)]))))))
     (define st2 (state-append st1 (message 'user (list (text-block marker)))))
     (struct-copy agent-state st2 [turn-count (add1 (agent-state-turn-count st2))])])
) ; end define interrupted-turn-state

;; 工具执行与 provider 都需 config；统一用 model.rkt 的 current-config parameter（run-turn! 每轮设置）。
(define current-loop-config current-config)

;; 轮开始前的永久压缩（context.rkt 的三级）：估算 > PRUNE-AT × 预算 → 先删旧工具输出（不调模型）；仍 > COMPACT-AT × 预算 → 总结。
;; 总结失败不吞：会话里记一条 compact_failed 提示、流里报一句；退回删过工具输出的状态（发送窗口仍由 context-fit 兜底）
(define (compact-failed! d e)
  (define msg (format "context compaction failed: ~a" (exn-message e)))
  (record! 'note (hasheq 'level "warn" 'code "compact_failed" 'text msg))
  (bus-publish! (deps-bus d) (evt:delta (now-ms) 'text (string-append "\n[" msg "]\n"))))
(define (maybe-compact st d)
  (define cfg (agent-state-config st))
  (define budget (config-context-budget cfg))
  (define est (history-tokens (agent-state-history st) (deps-est-cache d)))
  (cond
    [(<= est (* PRUNE-AT budget)) st]
    [else
     (define-values (st1 n saved) (prune! st))
     (when (> n 0)
       (bus-publish! (deps-bus d) (evt:delta (now-ms) 'text f"\n[trimmed {n} old tool outputs (~{saved} characters) to save context]\n")))
     (define est1 (history-tokens (agent-state-history st1) (deps-est-cache d)))
     (cond
       [(<= est1 (* COMPACT-AT budget)) st1]
       [else
        (bus-publish! (deps-bus d) (evt:delta (now-ms) 'text f"\n[compacting context: ~{est1} tokens]\n"))
        (with-handlers ([exn:fail? (lambda (e) (compact-failed! d e) st1)])
          (compact! st1 (deps-provider d)))])]))

;; ------------------------------------------------ 增强式回退：带恢复的流收集

;; 环绕 stream-and-collect! 的重试外壳（策略见 retry.rkt）。一次流失败时按决策动态重算：
;;   compact  —— compact! 压缩历史后重发（超限）
;;   retry    —— 指数退避后原样重发（瞬时/限流）
;;   fallback —— 切到回退链的下一个 provider/模型后重发（鉴权/额度）
;;   fail     —— 放弃，抛原异常（沿用旧行为，向用户报错）
;; 每次尝试都按「当时的 st」重算发送窗口（压缩/降级会改 history/config），并以 live config
;; parameterize provider 读取的 current-config，故降级换模型即时生效。
;; extra-msgs：仅用于构造发送窗口的追加消息（budget 收尾的 notice），不并入返回 st。
;; 返回 (values asst-msg stop-reason usage st*)；st* 为「成功那次尝试所用的 st」（可能已压缩/降级）。
(define (stream-with-recovery! st0 d host #:extra [extra-msgs '()])
  (define bus (deps-bus d))
  (define (note s) (bus-publish! bus (evt:delta (now-ms) 'text f"\n[{s}]\n")))
  (let retry ([st st0] [attempt 0] [fb-idx 0] [compacted? #f])
    (define cfg (agent-state-config st))
    (define hist
      (if (null? extra-msgs)
          (agent-state-history st)
          (agent-state-history (for/fold ([s st]) ([m (in-list extra-msgs)]) (state-append s m)))))
    (define window0 (context-fit hist cfg #:cache (deps-est-cache d)))
    (define window (if host (run-context-hooks host window0) window0))
    ;; 费用软上限：超了先问（本轮继续 / 今天不再询问 / 停止）；停止 → exn:fail:budget 向上抛（不进恢复逻辑）
    (budget-gate! (lambda (text input)
                    (define a (parameterize ([current-permission-request (hasheq 'tool 'budget 'level 'spend 'input input)])
                                ((deps-asker d) text)))
                    (if (memq a '(yes always)) a 'no)))
    (define result
      (with-handlers ([exn:fail? (lambda (e) (list 'err e))])
        (parameterize ([current-config cfg])
          (call-with-values (lambda () (stream-and-collect! d window))
                            (lambda vs (cons 'ok vs))))))
    (cond
      [(eq? (car result) 'ok)
       (let* ([u (list-ref (cdr result) 2)] [usd (record-spend! (config-model cfg) u)])
         ;; 会话记录：每次模型请求的模型、用量、估算花费
         (when (usage? u)
           (record! 'req (hasheq 'model (config-model cfg) 'input (usage-input-tokens u) 'output (usage-output-tokens u) 'cost usd))))
       (apply values (append (cdr result) (list st)))]
      [else
       (define e (cadr result))
       (define dec (decide-recovery host e attempt compacted? fb-idx))
       (case (recovery-action dec)
         [(compact)
          (note f"recover: context overflow → compacting & retrying")
          (retry (with-handlers ([exn:fail? (lambda (e) (compact-failed! d e) st)])
                   (let-values ([(st1 _n _s) (prune! st)]) (compact! st1 (deps-provider d))))
                 (add1 attempt) fb-idx #t)]
         [(retry)
          (define ms (backoff-delay attempt))
          (note f"recover: {(err-category e)} → retry #{(add1 attempt)} after {ms}ms")
          (when (> ms 0) (sleep (/ ms 1000.0)))
          (retry st (add1 attempt) fb-idx compacted?)]
         [(fallback)
          (define tgt (recovery-target dec))
          (note f"recover: {(err-category e)} → fallback to {(fallback-target-desc tgt)}")
          (retry (apply-fallback-target st host tgt) (add1 attempt) (add1 fb-idx) compacted?)]
         [else (raise e)])]
    ) ; end cond
  ) ; end let retry
) ; end define stream-with-recovery!

;; ------------------------------------------------------------- 主循环

;; 驱动一个完整「用户轮」：user-msg 进 → 模型/工具交替 → 终止性回答。
;; 返回新 agent-state。
(define (run-turn! st user-msg d)
  ;; 本轮的费用账：最外层的轮建一个，子 agent（在工具线程里调 run-turn!）沿用父轮的
  (define top? (not (current-turn-budget)))
  (parameterize ([current-turn-budget (or (current-turn-budget) (make-turn-budget))])
    ;; 会话记录：最外层的轮记「开始」、正常结束记「结束」；出错（调用方都会退回轮前状态）记「撤回」再抛出。中断 / 费用停止由调用方决定保留什么
    (when top? (record! 'turn 'begin))
    (with-handlers ([(lambda (e) (and top? (exn:fail? e) (not (exn:fail:budget? e))))
                     (lambda (e) (record! 'turn 'revert) (raise e))])
      (begin0 (run-turn/budget! st user-msg d)
              (when top? (record! 'turn 'end)))))
) ; end define run-turn!

;; 追加进历史并记录（消息在加进历史的那一刻落盘：之后的压缩、中断都不会让它漏记）
(define (append/rec st m) (record! 'msg m) (state-append st m))

(define (run-turn/budget! st user-msg d)
  (define host (deps-plugin-host d))
  (when host (host-set-session! host st))         ; 供插件 ctx.session 读取
  ;; 级别3：历史远超预算时先永久压缩（context-fit 只做透传/中段淘汰）
  (define st-c (maybe-compact st d))
  ;; 插件 before-turn 钩子：在 user-msg 后注入上下文消息。
  (define st-injected
    (if host
        (for/fold ([s (append/rec st-c user-msg)]) ([m (in-list (run-before-turn-hooks host st-c))])
          (append/rec s m))
        (append/rec st-c user-msg)))
  (let step ([st st-injected] [ncalls 0]
             [fails 0]                                   ; 连续失败轮计数（自适应升级用）
             [rung (ladder-rung-of (config-model (agent-state-config st-injected)))]
             [mt #f]                                     ; 本轮升过的 max_tokens（输出被截断后；#f = 用 config 的）
             [truncs 0])                                 ; 本轮输出被截断的次数
    (checkpoint! st '() #f)                              ; 一致点：用户消息 / 上一步工具结果已追加
    ;; 带恢复地取一段 assistant 输出；st1 = 成功那次所用状态（可能被压缩/降级过）。
    ;; 升过的 max_tokens 只用于本轮的请求：发送时换进去，拿回来的状态换回 config 原值（不进会话的 config）。
    (define mt0 (config-max-tokens (agent-state-config st)))
    (define-values (asst-msg stop u st1r) (stream-with-recovery! (if mt (with-max-tokens st mt) st) d host))
    (define st1 (if mt (with-max-tokens st1r mt0) st1r))
    (define cfg* (agent-state-config st1))          ; live config（回退可能已改）
    (define st*
      (state-add-usage (append/rec st1 asst-msg) u)
    ) ; end define st*
    (define calls (message-tool-uses asst-msg))
    (cond
      [(and (output-truncated? stop) (< truncs MAX-TRUNCATIONS))
       ;; 输出撞上 max_tokens（实测 deepseek-v4-flash：思考就占满 4096，回复只有思考块，没有工具调用也没有文字——
       ;; 不处理的话这里会被当成「答完了」而结束整轮）。先把上限 ×4 重发同一请求（残段丢掉）；已到 OUTPUT-CAP →
       ;; 残段的思考与文字留下（DeepSeek：请求带 tools 时之前各轮的思考都要回传，否则 400）、工具调用丢掉（参数可能是截断的）
       ;; + 一句「接着做、思考从简」。
       (define cur (or mt mt0))
       (define st-u (state-add-usage st1 u))
       (cond
         [(< cur OUTPUT-CAP)
          (define new (min OUTPUT-CAP (* 4 cur)))
          (truncated-note! d f"output cut off at max_tokens {cur}; retrying with {new}")
          (step st-u ncalls fails rung new (add1 truncs))]
         [else
          (define bs (message-blocks asst-msg))
          (define texts (filter text-block? bs))
          (define partial (message 'assistant (append (filter thinking-block? bs)
                                                      (if (null? texts) (list (text-block "(reply cut off at the output limit)")) texts))))
          (truncated-note! d f"output cut off at max_tokens {cur}; asking the model to continue")
          (step (append/rec (append/rec st-u partial) (text-msg 'user TRUNCATED-CONTINUE)) ncalls fails rung mt (add1 truncs))])
      ] ; end truncated case
      [(null? calls)
       ;; dsv4-b：'either 模式下首条纯文本回复亦促迁（'tool-call 默认下此处 no-op）。
       (define-values (st-p pnote) (dsv4b-maybe-promote! host st* 'assistant-message))
       (when pnote
         (bus-publish! (deps-bus d) (evt:delta (now-ms) 'text f"\n[{pnote}]\n")))
       (struct-copy agent-state st-p
                    [turn-count (add1 (agent-state-turn-count st-p))]
       ) ; end struct-copy
      ] ; end terminal case
      [(> (+ ncalls (length calls)) (config-turn-max-calls cfg*))
       ;; 预算超限：注入提示让模型收尾，而非硬掐断（收尾流同样带恢复）。
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
       (checkpoint! (state-append st* notice) '() #f)     ; 收尾流之前：预算超限提示已构成合法结果
       (define-values (final-msg _s u2 st2)
         (stream-with-recovery! st* d host #:extra (list notice))
       ) ; end define-values
       (define st**
         (state-add-usage (append/rec (append/rec st2 notice) final-msg) u2)
       ) ; end define st**
       (struct-copy agent-state st**
                    [turn-count (add1 (agent-state-turn-count st**))]
       ) ; end struct-copy
      ] ; end budget case
      [else
       (define progress (make-hash))
       (checkpoint! st* calls progress)                   ; 一致点：assistant 消息完整，工具调用待结果
       (define extra (box (usage 0 0)))                   ; 子 agent 等工具里的模型用量
       (define results (parameterize ([current-loop-config cfg*] [current-call-progress progress] [current-extra-usage extra])
                         (execute-calls! calls d)))
       ;; 自适应：本轮工具多数失败即计一次「失败轮」；连续达阈值且可升 → climb 到更强模型。
       (define failed? (round-failed? results))
       (define fails* (if failed? (add1 fails) 0))
       (define-values (st-next rung* esc)
         (if (and failed? (>= fails* (escalate-threshold)) (escalation-active? host))
             (escalate-step st* host rung)
             (values st* rung #f)))
       (when esc
         (bus-publish! (deps-bus d)
                       (evt:delta (now-ms) 'text
                                  f"\n[adaptive: repeated failures → escalate to {(car esc)} · thinking {(cdr esc)}]\n")))
       ;; dsv4-b：首个 durable 工具调用（成败均算）后促迁——registry 灌全量 +
       ;; system-prompt 追加解锁段；本 turn 的下一次请求即见完整目录（anchored 语义）。
       (define-values (st-p pnote) (dsv4b-maybe-promote! host st-next 'tool-call))
       (when pnote
         (bus-publish! (deps-bus d) (evt:delta (now-ms) 'text f"\n[{pnote}]\n")))
       (step (state-add-usage (append/rec st-p (message 'user results)) (unbox extra))
             (+ ncalls (length calls))
             (if esc 0 fails*)                 ; 升级后重置计数，给新模型一个干净的机会
             rung*
             mt truncs
       ) ; end step
      ] ; end tool case
    ) ; end cond
  ) ; end let step
) ; end define run-turn/budget!

;; 输出被 max_tokens 截断：anthropic 线路 "max_tokens"，openai / responses 线路 "length"。
(define (output-truncated? stop) (and (member stop '("max_tokens" "length")) #t))
(define OUTPUT-CAP 32768)                                ; 截断后升 max_tokens 的上限
(define MAX-TRUNCATIONS 3)                               ; 一轮里最多处理几次截断，之后照旧按终止处理
(define TRUNCATED-CONTINUE
  "Your previous reply was cut off: it hit the output token limit before you made a tool call or finished the answer. Do not restart the analysis or recap. Keep reasoning short: make the next tool call now, or give the answer.")
(define (with-max-tokens st n)
  (define cfg (agent-state-config st))
  (if (eqv? (config-max-tokens cfg) n) st (struct-copy agent-state st [config (struct-copy config cfg [max-tokens n])])))
;; 流里报一句、会话里记一条 output_truncated（评测按它数截断次数）
(define (truncated-note! d text)
  (record! 'note (hasheq 'level "warn" 'code "output_truncated" 'text text))
  (bus-publish! (deps-bus d) (evt:delta (now-ms) 'text f"\n[{text}]\n")))

;; 一轮工具结果是否算「失败轮」：至少一半（向上取整）的调用报错。单调用报错即算失败。
(define (round-failed? results)
  (define n (length results))
  (define e (count tool-result-block-is-error? results))
  (and (positive? e) (>= e (quotient (add1 n) 2)))
) ; end define round-failed?

;; ---------------------------------------------------------------- provide

(provide
 (struct-out deps)
 make-deps
 run-turn!
 current-turn-checkpoint
 interrupted-turn-state
 TURN-ABORTED-MARKER
 BUDGET-STOP-MARKER
 current-tool-source current-tool-call-id
 stream-and-collect!
 execute-calls!
 current-extra-usage
 add-extra-usage!
) ; end provide
