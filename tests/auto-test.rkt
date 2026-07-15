#lang tstring racket
;; auto-test.rkt — Auto 模式（离线）：任务分类、模型/推理决策、仅 DeepSeek 生效的门控。

(require
 rackunit
 (file "../src/model.rkt")
 (file "../src/plugin.rkt")
 (file "../src/providers.rkt")
 (file "../src/auto.rkt")
) ; end require

;; 隔离：清掉可能的模型名覆盖 env，跑默认名。
(for ([n '("PI_AUTO_PRO" "PI_AUTO_LIGHT" "PI_CONFIG_HOME")]) (putenv n ""))

(test-case "classify-task: short/simple → light; complex signals → pro"
  (check-equal? (classify-task "hi there") 'light)               ; 短 → 轻
  (check-equal? (classify-task "谢谢") 'light)
  (check-equal? (classify-task "implement a red-black tree delete") 'pro)   ; 关键词
  (check-equal? (classify-task "帮我重构这个模块") 'pro)          ; 中文关键词 重构
  (check-equal? (classify-task "```\ncode\n```") 'pro)           ; 代码围栏
  (check-equal? (classify-task (make-string 300 #\x)) 'pro)      ; 长文本
  ;; 中等长度、无关键词 → 默认偏 pro
  (check-equal? (classify-task "please tell me the capital city of that european country over there") 'pro)
) ; end test-case

(test-case "auto-decide: pro → (pro-model, max); light → (light-model, off)"
  (define-values (m1 e1) (auto-decide "implement quicksort"))
  (check-equal? m1 (pro-model))
  (check-equal? e1 'max)
  (define-values (m2 e2) (auto-decide "hi"))
  (check-equal? m2 (light-model))
  (check-equal? e2 'off)
) ; end test-case

(test-case "default model names"
  (check-equal? (pro-model) "deepseek-v4-pro")
  (check-equal? (light-model) "deepseek-v4-flash")
) ; end test-case

(test-case "maybe-apply-auto only fires when provider base is deepseek"
  (set-auto-mode! #t)
  (define host (make-plugin-host))
  (register-builtin-providers! host)
  (define st (make-initial-state (default-config)))
  ;; provider = lmstudio → 不生效
  (host-set-provider! host "lmstudio")
  (define-values (st-a dec-a) (maybe-apply-auto st "implement quicksort" host))
  (check-false dec-a)
  (check-eq? st-a st)
  ;; provider = deepseek → 生效，改 model 为 pro，推理档 max
  (host-set-provider! host "deepseek")
  (set-reasoning-effort! 'off)
  (define-values (st-b dec-b) (maybe-apply-auto st "implement quicksort" host))
  (check-equal? (car dec-b) "deepseek-v4-pro")
  (check-equal? (cdr dec-b) 'max)
  (check-equal? (config-model (agent-state-config st-b)) "deepseek-v4-pro")
  (check-equal? (current-reasoning-effort) 'max)
  ;; 实例名 deepseek[work] 亦按 base 生效，轻任务 → flash + off
  (host-set-provider! host "deepseek[work]")
  (define-values (st-c dec-c) (maybe-apply-auto st "hi" host))
  (check-equal? (car dec-c) "deepseek-v4-flash")
  (check-equal? (cdr dec-c) 'off)
) ; end test-case

(test-case "auto off → never fires even on deepseek"
  (set-auto-mode! #f)
  (define host (make-plugin-host))
  (register-builtin-providers! host)
  (host-set-provider! host "deepseek")
  (define st (make-initial-state (default-config)))
  (define-values (st-a dec-a) (maybe-apply-auto st "implement quicksort" host))
  (check-false dec-a)
  (set-auto-mode! #t)                        ; 复位默认
) ; end test-case

(set-reasoning-effort! 'off)                 ; 复位，防污染同进程其它用例
(displayln "auto-test: all passed")
