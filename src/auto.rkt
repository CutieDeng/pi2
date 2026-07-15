#lang tstring racket
;; auto.rkt — Auto 模式：为 DeepSeek 按任务在「轻/重」模型间自动切换（非侵入）。
;;
;; 设计（据用户答复）：
;;   * 触发面仅限 deepseek 实例——「选 deepseek 即自动开」。其它 provider 一律不受影响。
;;   * 本地启发式（零额外 LLM 调用、零延迟）：
;;       - 简单/短任务      → light 模型（deepseek-v4-flash），thinking off；
;;       - 复杂/含代码/长任务 → pro   模型（deepseek-v4-pro），thinking max；
;;       - 拿不准 → 默认 pro + max（即用户说的「默认任务均用 -pro + thinking max」）。
;;   * 模型名可覆盖（端点相关）：env PI_AUTO_PRO / PI_AUTO_LIGHT 或 {config-home}/auto.rktd。
;;   * 完全在「turn 之前」的层做：改 config-model + 设推理 box，内核 loop/provider 不动。
;;     故对 repl / rpc / 一次性 -p 都以一行 maybe-apply-auto 接入。

(require
 racket/string
 (file "model.rkt")
 (file "plugin.rkt")                    ; host-current-provider / provider-base-name
 (file "credentials.rkt")               ; config-home（读 auto.rktd 覆盖）
) ; end require

;; ---------------------------------------------------------------- 开关（进程级 box）

;; 'on 默认；仅当 current provider base = "deepseek" 时才实际生效（见 maybe-apply-auto）。
(define auto-mode-box (box 'on))
(define (auto-mode-on?) (eq? (unbox auto-mode-box) 'on))
(define (set-auto-mode! on?) (set-box! auto-mode-box (if on? 'on 'off)))

;; ---------------------------------------------------------------- 模型名（可覆盖）

(define DEFAULT-PRO   "deepseek-v4-pro")
(define DEFAULT-LIGHT "deepseek-v4-flash")

;; {config-home}/auto.rktd：(hash 'pro "…" 'light "…")，缺/损忽略。
(define (auto-overrides)
  (define path (build-path (config-home) "auto.rktd"))
  (if (file-exists? path)
      (with-handlers ([exn:fail? (lambda (_e) (hash))])
        (let ([v (call-with-input-file path read)]) (if (hash? v) v (hash))))
      (hash)))

(define (env-or name dflt) (let ([v (getenv name)]) (if (and v (non-empty-string? v)) v dflt)))

(define (pro-model)
  (env-or "PI_AUTO_PRO" (let ([v (hash-ref (auto-overrides) 'pro #f)]) (if (string? v) v DEFAULT-PRO))))
(define (light-model)
  (env-or "PI_AUTO_LIGHT" (let ([v (hash-ref (auto-overrides) 'light #f)]) (if (string? v) v DEFAULT-LIGHT))))

;; ---------------------------------------------------------------- 启发式分类

;; 复杂信号：代码围栏、长文本，或含「工程/推理」意图关键词（中英）。
(define HARD-KEYWORDS
  '("implement" "debug" "refactor" "analyze" "analyse" "design" "optimize" "optimise"
    "prove" "explain" "why" "fix" "diagnose" "architect" "plan" "trace" "derive"
    "实现" "调试" "重构" "分析" "设计" "优化" "证明" "解释" "为什么" "修复" "规划"
    "排查" "推导" "算法" "复杂"))

;; 'light | 'pro。默认偏 pro（拿不准就用强模型 + max）。
(define (classify-task text)
  (define t (string-downcase (string-trim (if (string? text) text ""))))
  (cond
    [(string-contains? t "```") 'pro]                         ; 代码块 → 重
    [(> (string-length t) 280) 'pro]                          ; 长任务 → 重
    [(for/or ([k (in-list HARD-KEYWORDS)]) (string-contains? t k)) 'pro]  ; 意图关键词 → 重
    [(< (string-length t) 60) 'light]                         ; 短小闲问 → 轻
    [else 'pro]                                               ; 其余默认 → 重
  ) ; end cond
) ; end define classify-task

;; 分类 → (values 模型名 推理档)。pro → thinking max；light(flash) → off。
(define (auto-decide text)
  (case (classify-task text)
    [(light) (values (light-model) 'off)]
    [else    (values (pro-model)   'max)])
) ; end define auto-decide

;; ---------------------------------------------------------------- 接入点

;; 是否应对当前 provider 生效：auto on 且 base = deepseek。
(define (auto-active? host)
  (and (auto-mode-on?)
       (string=? (provider-base-name (host-current-provider host)) "deepseek")))

;; turn 之前调用：命中则回传「已改 model 的 st*」并把推理 box 设好；否则原样返回 st。
;; 返回 (values st* decision)；decision = (cons model effort) 或 #f（未生效）。
(define (maybe-apply-auto st text host)
  (cond
    [(not (auto-active? host)) (values st #f)]
    [else
     (define-values (model eff) (auto-decide text))
     (set-reasoning-effort! eff)
     (values (struct-copy agent-state st
                          [config (struct-copy config (agent-state-config st) [model model])])
             (cons model eff))]
  ) ; end cond
) ; end define maybe-apply-auto

(provide
 auto-mode-on? set-auto-mode!
 pro-model light-model
 classify-task auto-decide
 auto-active? maybe-apply-auto
) ; end provide
