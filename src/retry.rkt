#lang tstring racket
;; retry.rkt — 增强式 hook 回退 / 自动动态重算（据用户答复：四种行为全要）。
;;
;; 目标：一轮请求失败（provider 抛异常）时，不直接把错误抛给用户，而是按策略「动态重算」：
;;   1. 失败自动重试（retry）    —— 瞬时错误（超时/5xx/429/过载）指数退避后重发。
;;   2. 超限自动压缩重试（compact）—— 上下文/token 超限 → compact! 后重算本轮。
;;   3. 自动降级切换（fallback） —— 鉴权/额度类错误 → 按回退链切到备用 provider/模型再重算。
;;   4. 可编程 on-error hook      —— 插件注册 'error-recover 钩子，覆盖内置决策（见 plugin.rkt）。
;;
;; 非侵入：本模块只提供「分类 + 决策 + 回退目标应用」的纯策略；真正的重试循环由
;; loop.rkt 环绕 stream-and-collect! 实现（stream-and-collect! 在 loop.rkt，故此处不 require loop，
;; 避免环形依赖）。所有可调项走进程级 box（config 为 prefab，不能加字段）。

(require
 racket/string
 (file "model.rkt")                     ; agent-state / config / struct-copy
 (file "plugin.rkt")                    ; run-error-hooks / host-set-provider! / apply…（下）
 (file "providers.rkt")                 ; apply-provider-profile / builtin-provider-instance?
) ; end require

;; ---------------------------------------------------------------- 错误分类

;; 把 exn（或其消息串）归入一个恢复类别。云端错误多以 HTTP 状态码 + 文案回传，
;; 故按状态码与关键词双路匹配，尽量稳健（大小写无关）。
;;   overflow  上下文/token 超限         → 压缩重试
;;   rate      限流/过载（429/529/503?）  → 退避重试
;;   transient 瞬时网络/服务端（超时/5xx）→ 退避重试
;;   auth      鉴权失败（401/403/密钥）    → 降级切换
;;   quota     额度/计费/欠费             → 降级切换
;;   other     其它（默认不重试）
(define (err-category e)
  (define msg (string-downcase (if (exn? e) (exn-message e) (format "~a" e))))
  (define (has? . subs) (for/or ([s (in-list subs)]) (string-contains? msg s)))
  (cond
    ;; 超限：文案很明确，优先判（有的服务端用 400 带 context_length_exceeded）。
    [(has? "context length" "context_length" "maximum context" "too many tokens"
           "prompt is too long" "reduce the length" "input is too long"
           "exceeds the maximum" "context window" "max_tokens" "token limit")
     'overflow]
    ;; 限流 / 过载。
    [(has? "429" "rate limit" "rate_limit" "too many requests" "overloaded"
           "over capacity" "529" "quota exceeded per")
     'rate]
    ;; 鉴权。
    [(has? "401" "403" "invalid api key" "invalid_api_key" "authentication"
           "unauthorized" "forbidden" "no token" "api key not" "permission denied")
     'auth]
    ;; 额度 / 计费。
    [(has? "quota" "insufficient" "billing" "credit" "payment" "balance" "402")
     'quota]
    ;; 瞬时：网络/超时/5xx。
    [(has? "timeout" "timed out" "connection" "reset" "refused" "temporarily"
           "500" "502" "503" "504" "eof" "broken pipe" "unavailable")
     'transient]
    [else 'other]
  ) ; end cond
) ; end define err-category

;; ---------------------------------------------------------------- 决策

;; 恢复决策：action ∈ 'retry | 'compact | 'fallback | 'fail；target 仅 fallback 用（目标名）。
(struct recovery (action target) #:transparent)

;; 可调项（进程级）：瞬时重试上限、退避基数（毫秒；测试可设 0 免真 sleep）。
(define retry-max-transient (box 3))
(define retry-backoff-ms    (box 400))

;; 第 attempt 次（0 起）退避毫秒：base·2^attempt，封顶 8s。base=0 → 0（测试用）。
(define (backoff-delay attempt)
  (define base (unbox retry-backoff-ms))
  (if (<= base 0) 0 (min 8000 (* base (expt 2 attempt))))
) ; end define backoff-delay

;; 内置默认策略。compacted? 表示本轮是否已压缩过（避免压缩后仍超限时死循环）。
;; fb-idx 为已用回退次数（回退链游标）。
(define (default-recovery cat attempt compacted? fb-idx)
  (case cat
    [(overflow) (if compacted? (recovery 'fail #f) (recovery 'compact #f))]
    [(rate transient)
     (if (< attempt (unbox retry-max-transient)) (recovery 'retry #f) (recovery 'fail #f))]
    [(auth quota)
     (define chain (fallback-chain))
     (if (< fb-idx (length chain)) (recovery 'fallback (list-ref chain fb-idx)) (recovery 'fail #f))]
    [else (recovery 'fail #f)]
  ) ; end case
) ; end define default-recovery

;; 把插件钩子的原始返回（symbol | (cons 'fallback target) | #f）规整成 recovery，或 #f（放行默认）。
(define (hook-result->recovery r fb-idx)
  (cond
    [(eq? r 'retry)    (recovery 'retry #f)]
    [(eq? r 'compact)  (recovery 'compact #f)]
    [(eq? r 'fail)     (recovery 'fail #f)]
    [(eq? r 'fallback)
     (define chain (fallback-chain))
     (if (< fb-idx (length chain)) (recovery 'fallback (list-ref chain fb-idx)) (recovery 'fail #f))]
    [(and (pair? r) (eq? (car r) 'fallback) (string? (cdr r))) (recovery 'fallback (cdr r))]
    [else #f]
  ) ; end cond
) ; end define hook-result->recovery

;; 综合决策：先问插件 on-error 钩子（可覆盖），否则用内置默认。
(define (decide-recovery host e attempt compacted? fb-idx)
  (define cat (err-category e))
  (define hooked
    (and host
         (let ([r (run-error-hooks host cat (if (exn? e) (exn-message e) (format "~a" e)) attempt)])
           (and r (hook-result->recovery r fb-idx)))))
  (or hooked (default-recovery cat attempt compacted? fb-idx))
) ; end define decide-recovery

;; ---------------------------------------------------------------- 回退链

;; 回退链：一串「provider[label] 或 model」目标，降级时依次切过去。进程级 box。
;; 初值取自 env PI_FALLBACK（逗号分隔），可用 /fallback 运行时改。
(define (env-fallback)
  (define v (getenv "PI_FALLBACK"))
  (if (and v (non-empty-string? (string-trim v)))
      (filter non-empty-string? (map string-trim (string-split v ",")))
      '()))
(define fallback-chain-box (box (env-fallback)))
(define (fallback-chain) (unbox fallback-chain-box))
(define (set-fallback-chain! lst)
  (set-box! fallback-chain-box (filter non-empty-string? (map string-trim lst))))

;; 把回退目标 target 应用到 st：
;;   * 若是内置 provider 实例名（"deepseek[work]" / "anthropic"）→ 切 host 选用名 + 写档案进 config；
;;   * 否则视作模型名 → 仅改 config-model（同 provider 内降级，如 pro→flash）。
;; 返回新的 agent-state（config 已更新）。
(define (apply-fallback-target st host target)
  (cond
    [(and host (builtin-provider-instance? target))
     (when host (host-set-provider! host target))
     (struct-copy agent-state st [config (apply-provider-profile (agent-state-config st) target)])]
    [else
     (struct-copy agent-state st
                  [config (struct-copy config (agent-state-config st) [model target])])]
  ) ; end cond
) ; end define apply-fallback-target

;; 目标的人读描述（供 UI 回显）。
(define (fallback-target-desc target)
  (if (builtin-provider-instance? target) f"provider {target}" f"model {target}"))

;; ---------------------------------------------------------------- provide

(provide
 err-category
 (struct-out recovery)
 default-recovery decide-recovery hook-result->recovery
 retry-max-transient retry-backoff-ms backoff-delay
 fallback-chain set-fallback-chain! env-fallback
 apply-fallback-target fallback-target-desc
) ; end provide
