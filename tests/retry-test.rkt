#lang tstring racket
;; retry-test.rkt — 增强式回退 / 动态重算（retry.rkt + loop.rkt 集成）离线单测。
;; 覆盖：错误分类、内置决策、钩子覆盖、回退链读写与目标应用，以及经 run-turn! 的真·重算集成。

(require
 rackunit
 racket/async-channel
 racket/file
 racket/pvector
 (file "../src/model.rkt")
 (file "../src/event.rkt")
 (file "../src/provider.rkt")
 (file "../src/tool.rkt")
 (file "../src/permission.rkt")
 (file "../src/plugin.rkt")
 (file "../src/providers.rkt")
 (file "../src/loop.rkt")
 (file "../src/retry.rkt")
 (file "../src/tools/builtin.rkt")
) ; end require

(define (err s) (make-exn:fail s (current-continuation-marks)))

;; ---------------------------------------------------------------- 错误分类

(test-case "err-category：按状态码/关键词归类（大小写无关）"
  (check-eq? (err-category (err "HTTP 429 Too Many Requests")) 'rate)
  (check-eq? (err-category (err "connection timed out after 30s")) 'transient)
  (check-eq? (err-category (err "server returned 503 Service Unavailable")) 'transient)
  (check-eq? (err-category (err "401 Unauthorized: invalid api key")) 'auth)
  (check-eq? (err-category (err "403 permission denied")) 'auth)
  (check-eq? (err-category (err "Insufficient balance / billing quota")) 'quota)
  (check-eq? (err-category (err "This model's maximum context length is 8192 tokens")) 'overflow)
  (check-eq? (err-category (err "prompt is too long")) 'overflow)
  (check-eq? (err-category (err "something totally unrecognized")) 'other)
) ; end test-case

;; ---------------------------------------------------------------- 内置决策

(test-case "default-recovery：各类别的默认动作"
  ;; overflow：未压缩→compact；已压缩→fail（避免死循环）
  (check-eq? (recovery-action (default-recovery 'overflow 0 #f 0)) 'compact)
  (check-eq? (recovery-action (default-recovery 'overflow 1 #t 0)) 'fail)
  ;; transient/rate：未达上限→retry；达上限→fail
  (set-box! retry-max-transient 3)
  (check-eq? (recovery-action (default-recovery 'transient 0 #f 0)) 'retry)
  (check-eq? (recovery-action (default-recovery 'rate 2 #f 0)) 'retry)
  (check-eq? (recovery-action (default-recovery 'transient 3 #f 0)) 'fail)
  ;; auth/quota：有回退链→fallback（取当前游标目标）；空链→fail
  (set-fallback-chain! '())
  (check-eq? (recovery-action (default-recovery 'auth 0 #f 0)) 'fail)
  (set-fallback-chain! '("anthropic" "deepseek-v4-flash"))
  (check-equal? (let ([r (default-recovery 'auth 0 #f 0)]) (list (recovery-action r) (recovery-target r)))
                (list 'fallback "anthropic"))
  (check-equal? (recovery-target (default-recovery 'quota 1 #f 1)) "deepseek-v4-flash")
  (check-eq? (recovery-action (default-recovery 'auth 5 #f 2)) 'fail)   ; 游标越界→fail
  ;; other：从不重试
  (check-eq? (recovery-action (default-recovery 'other 0 #f 0)) 'fail)
  (set-fallback-chain! '())
) ; end test-case

;; ---------------------------------------------------------------- 钩子归一化

(test-case "hook-result->recovery：符号/对/无效值归一"
  (set-fallback-chain! '("gpt-5"))
  (check-eq? (recovery-action (hook-result->recovery 'retry 0)) 'retry)
  (check-eq? (recovery-action (hook-result->recovery 'compact 0)) 'compact)
  (check-eq? (recovery-action (hook-result->recovery 'fail 0)) 'fail)
  (check-equal? (recovery-target (hook-result->recovery 'fallback 0)) "gpt-5")     ; 用回退链
  (check-equal? (recovery-target (hook-result->recovery (cons 'fallback "claude-opus") 0)) "claude-opus") ; 指定目标
  (check-false (hook-result->recovery #f 0))
  (check-false (hook-result->recovery 'bogus 0))
  (set-fallback-chain! '())
) ; end test-case

;; ---------------------------------------------------------------- 退避

(test-case "backoff-delay：base=0 免真 sleep；否则指数封顶 8s"
  (set-box! retry-backoff-ms 0)
  (check-= (backoff-delay 0) 0 0)
  (check-= (backoff-delay 5) 0 0)
  (set-box! retry-backoff-ms 100)
  (check-= (backoff-delay 0) 100 0)
  (check-= (backoff-delay 1) 200 0)
  (check-= (backoff-delay 3) 800 0)
  (check-= (backoff-delay 20) 8000 0)                 ; 封顶
  (set-box! retry-backoff-ms 0)                        ; 复位（后续集成测试不真等待）
) ; end test-case

;; ---------------------------------------------------------------- 回退链读写

(test-case "fallback-chain：set 去空白、clear 清空"
  (set-fallback-chain! '("anthropic" "  deepseek-v4-flash  " ""))
  (check-equal? (fallback-chain) '("anthropic" "deepseek-v4-flash"))
  (set-fallback-chain! '())
  (check-equal? (fallback-chain) '())
) ; end test-case

;; ---------------------------------------------------------------- 回退目标应用

(test-case "apply-fallback-target：非内置名→改模型；内置实例名(有 host)→切档案"
  (define cfg (struct-copy config (default-config) [model "orig"]))
  (define st (make-initial-state cfg))
  ;; 无 host + 模型名：仅改 config-model（同 provider 内降级）
  (define st1 (apply-fallback-target st #f "cheaper-model"))
  (check-equal? (config-model (agent-state-config st1)) "cheaper-model")
  ;; 内置 provider 实例名：切 host 选用名 + 写档案（endpoint/model 变为该 provider）
  (define host (make-plugin-host))
  (register-builtin-providers! host)
  (define st2 (apply-fallback-target st host "anthropic"))
  (check-equal? (host-current-provider host) "anthropic")
  (check-equal? (config-model (agent-state-config st2)) "claude-sonnet-5")
) ; end test-case

;; ---------------------------------------------------------------- 插件 on-error 钩子覆盖

(define (host-with-error-hook handler)
  (define host (make-plugin-host))
  (hash-set! (plugin-host-hooks host) 'error-recover (list handler))
  host)

(test-case "decide-recovery：插件 error-recover 钩子覆盖内置默认"
  (set-box! retry-max-transient 3)
  ;; 默认下 transient→retry；钩子强制 fail → 应得 fail
  (define h-fail (host-with-error-hook (lambda (cat msg attempt) 'fail)))
  (check-eq? (recovery-action (decide-recovery h-fail (err "connection reset") 0 #f 0)) 'fail)
  ;; 钩子返回 #f → 放行内置默认（transient→retry）
  (define h-defer (host-with-error-hook (lambda (cat msg attempt) #f)))
  (check-eq? (recovery-action (decide-recovery h-defer (err "connection reset") 0 #f 0)) 'retry)
  ;; 钩子指定回退目标
  (define h-fb (host-with-error-hook (lambda (cat msg attempt) (cons 'fallback "grok"))))
  (check-equal? (recovery-target (decide-recovery h-fb (err "boom") 0 #f 0)) "grok")
  ;; 钩子抛异常 → 视作 #f，绝不打断（回落默认；other→fail）
  (define h-throw (host-with-error-hook (lambda (cat msg attempt) (error "hook bug"))))
  (check-eq? (recovery-action (decide-recovery h-throw (err "weird") 0 #f 0)) 'fail)
) ; end test-case

;; ---------------------------------------------------------------- run-turn! 集成：真·重算

(define tmpdir (make-temporary-file "pi2-retrytest-~a" 'directory))

(define (test-cfg)
  (struct-copy config (default-config) [workdir (path->string tmpdir)] [permission-mode 'yolo]))

;; flaky provider：前 (length fail-msgs) 次调用吐 evt:error（按序），其后成功返回 ok-msg。
(define (make-flaky-provider fail-msgs ok-msg count-box)
  (provider "flaky"
    (lambda (_m _t)
      (define ch (make-async-channel))
      (define n (unbox count-box))
      (set-box! count-box (add1 n))
      (thread
       (lambda ()
         (cond
           [(< n (length fail-msgs))
            (async-channel-put ch (evt:error (now-ms) (err (list-ref fail-msgs n)) #f))]
           [else
            (async-channel-put ch (evt:message (now-ms) ok-msg))
            (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 10 5)))])))
      ch)
    void))

(define (flaky-deps fail-msgs ok-msg count-box)
  (define cfg (test-cfg))
  (make-deps #:provider (make-flaky-provider fail-msgs ok-msg count-box)
             #:registry (make-registry (builtin-tools cfg))
             #:bus (make-bus)
             #:policy (make-policy cfg)))

(test-case "run-turn!：瞬时错误自动退避重试后成功（非侵入，历史干净）"
  (set-box! retry-backoff-ms 0)
  (set-box! retry-max-transient 3)
  (define cnt (box 0))
  (define d (flaky-deps (list "connection timed out" "503 unavailable")
                        (text-msg 'assistant "recovered answer") cnt))
  (define st (run-turn! (make-initial-state (test-cfg)) (text-msg 'user "hello") d))
  (check-equal? (unbox cnt) 3)                                 ; 2 次失败 + 1 次成功
  (check-equal? (pvector-length (agent-state-history st)) 2)    ; user + assistant，无残留
  (check-equal? (agent-state-turn-count st) 1)
  (check-equal? (message-text (pvector-ref (agent-state-history st) 1)) "recovered answer")
) ; end test-case

(test-case "run-turn!：瞬时错误超过上限则抛出（沿用旧的报错行为）"
  (set-box! retry-backoff-ms 0)
  (set-box! retry-max-transient 1)                             ; attempt0 retry；attempt1 fail
  (define cnt (box 0))
  (define d (flaky-deps (list "connection reset" "connection reset")
                        (text-msg 'assistant "unreached") cnt))
  (check-exn exn:fail? (lambda () (run-turn! (make-initial-state (test-cfg)) (text-msg 'user "x") d)))
  (check-equal? (unbox cnt) 2)                                 ; 试到上限即停
  (set-box! retry-max-transient 3)
) ; end test-case

(test-case "run-turn!：鉴权失败按回退链降级到备用模型后成功"
  (set-box! retry-backoff-ms 0)
  (set-fallback-chain! '("backup-model"))
  (define cnt (box 0))
  (define d (flaky-deps (list "401 invalid api key")
                        (text-msg 'assistant "answer via fallback") cnt))
  (define st (run-turn! (make-initial-state (test-cfg)) (text-msg 'user "x") d))
  (check-equal? (unbox cnt) 2)                                 ; 失败 1 次 + 降级后成功
  (check-equal? (config-model (agent-state-config st)) "backup-model")   ; 已切到回退目标
  (check-equal? (message-text (pvector-ref (agent-state-history st) 1)) "answer via fallback")
  (set-fallback-chain! '())
) ; end test-case
