#lang racket/base
;; tests/gui/model-picker-test.rkt — 模型 / 推理档选择组件（单个组件 + 假 text-layout）

(require rackunit
         "../../src/gui/compute/drawlist.rkt" "../../src/gui/compute/interfaces.rkt"
         "../../src/gui/components/model-picker.rkt" "fakes.rkt")

(define (new-mp) (instantiate-with model-picker-component model-picker))
(define OPTIONS
  (hasheq 'type "providers" 'current "lmstudio" 'model "gemma" 'reasoning "off"
          'reasoning_levels '("off" "low" "medium" "high" "max")
          'providers (list (hasheq 'name "lmstudio" 'models '("gemma") 'has_key #t)
                           (hasheq 'name "deepseek" 'models '("deepseek-v4-flash" "deepseek-v4-pro") 'has_key #f))))

(define (render s) (define dl (make-drawlist)) (values dl (s 'render! dl 20.0 700.0 600.0 720.0 2.0)))

(test-case "no state → no chip; state merges partial updates"
  (define s (new-mp))
  (define-values (_dl w0) (render s))
  (check-equal? w0 0.0)
  (check-false (s 'hit 25.0 705.0))
  (s 'set-state! (hasheq 'provider "lmstudio" 'model "gemma"))
  (s 'set-state! (hasheq 'reasoning "high"))                                       ; 只带一项：其余保留
  (define-values (_dl2 w1) (render s))
  (check-true (> w1 0.0))
  (define c (s 'hit-center 'toggle #t))
  (check-equal? (s 'hit (car c) (cdr c)) '(toggle))
  (check-true (s 'open?)))

(test-case "menu: provider / model / reasoning rows hit to actions; the menu stays open; outside click closes"
  (define s (new-mp))
  (s 'set-options! OPTIONS)
  (s 'open!)
  (render s)
  (define p (s 'hit-center 'provider "deepseek"))
  (check-true (< (cdr p) 700.0))                                                    ; 菜单在芯片上方
  (check-equal? (s 'hit (car p) (cdr p)) '(provider "deepseek"))
  (check-true (s 'open?))                                                           ; 选了不关
  (render s)
  (define r (s 'hit-center 'reasoning "high"))
  (check-equal? (s 'hit (car r) (cdr r)) '(reasoning "high"))
  (check-true (s 'open?))
  (render s)
  (define m (s 'hit-center 'model "gemma"))
  (check-equal? (s 'hit (car m) (cdr m)) '(model "gemma"))
  (check-true (s 'open?))
  (define c (s 'hit-center 'toggle #t))
  (check-equal? (s 'hit (car c) (cdr c)) '(toggle))                                 ; 再点芯片：关
  (check-false (s 'open?))
  (s 'open!) (render s)
  (check-equal? (s 'hit 590.0 10.0) '(close))                                      ; 菜单外：关
  (check-false (s 'open?))
  (check-false (s 'hit 590.0 10.0)))                                                ; 关着：不消化点击

(test-case "accounts of one provider are grouped under it; a single default account is one row"
  (define s (new-mp))
  (s 'set-options! (hasheq 'current "deepseek[work]" 'model "deepseek-v4-flash" 'reasoning "off"
                           'providers (list (hasheq 'name "lmstudio" 'models '("gemma") 'has_key #t)
                                            (hasheq 'name "deepseek" 'models '("deepseek-v4-flash") 'has_key #t)
                                            (hasheq 'name "deepseek[work]" 'models '("deepseek-v4-flash") 'has_key #f))))
  (s 'open!) (render s)
  (check-true (pair? (s 'hit-center 'provider "lmstudio")))
  (check-true (pair? (s 'hit-center 'provider "deepseek")))                         ; 默认帐号
  (define w (s 'hit-center 'provider "deepseek[work]"))
  (check-equal? (s 'hit (car w) (cdr w)) '(provider "deepseek[work]"))
  (check-true (< (cdr (s 'hit-center 'provider "deepseek")) (cdr w))))              ; 帐号按出现顺序排在组头下

(test-case "models listed are the current provider's; the current model is kept even if not in the list"
  (define s (new-mp))
  (s 'set-options! OPTIONS)
  (s 'set-state! (hasheq 'provider "deepseek" 'model "deepseek-chat"))
  (s 'open!) (render s)
  (check-true (pair? (s 'hit-center 'model "deepseek-chat")))
  (check-true (pair? (s 'hit-center 'model "deepseek-v4-pro")))
  (check-false (s 'hit-center 'model "gemma")))

(test-case "hover highlights rows only while open"
  (define s (new-mp))
  (s 'set-options! OPTIONS)
  (render s)
  (check-false (s 'hover! 25.0 705.0))
  (s 'open!) (render s)
  (define p (s 'hit-center 'provider "lmstudio"))
  (check-true (s 'hover! (car p) (cdr p)))
  (check-false (s 'hover! (car p) (cdr p))))

(test-case "long model lists: menu height is capped (<= 360pt), the current model is scrolled into view, wheel scrolls the pane under the pointer, levels stay pinned"
  (define s (new-mp))
  (define many (for/list ([i 40]) (format "model-~a" i)))
  (s 'set-options! (hasheq 'current "lmstudio" 'model "model-30" 'reasoning "off" 'reasoning_levels '("off" "high")
                           'providers (list (hasheq 'name "lmstudio" 'models many 'loaded '("model-30") 'has_key #t))))
  (s 'open!)
  (render s)                                                     ; 芯片在 y=700：菜单最高到 y=8
  (define c30 (s 'hit-center 'model "model-30"))
  (check-true (and c30 (< 8.0 (cdr c30) 700.0)))                 ; 打开时选中的模型在视区里
  (check-false (s 'hit-center 'model "model-0"))                 ; 顶部的行被滚出视区：没有命中区
  (define lv (s 'hit-center 'reasoning "high"))
  (check-true (pair? lv))                                        ; 推理档固定在底部
  (check-false (s 'scroll! 10.0 10.0 100000.0))                  ; 指针不在菜单上：不处理（交给下面的区域）
  (check-true (s 'scroll! (car c30) (cdr c30) 100000.0))          ; 指针在模型栏：滚这一栏到顶
  (render s)
  (check-true (pair? (s 'hit-center 'provider "lmstudio")))
  (check-true (> (cdr (s 'hit-center 'provider "lmstudio")) (- 700.0 6.0 360.0)))   ; 高度上限：不再顶到窗口顶部
  (check-false (s 'hit-center 'model "model-30"))
  (check-equal? (s 'hit-center 'reasoning "high") lv)            ; 滚动不影响底部
  (define c0 (s 'hit-center 'model "model-0"))
  (check-equal? (s 'hit (car c0) (cdr c0)) '(model "model-0")))

(test-case "routing pool: listed with an unavailable count; when current, the right pane lists members with state; member click pins"
  (define s (new-mp))
  (define pool (hasheq 'name "pool" 'current 1 'unavailable 1
                       'members (list (hasheq 'instance "deepseek[work]" 'model "deepseek-v4-pro" 'state "cooling" 'seconds_left 120 'available #f)
                                      (hasheq 'instance "anthropic" 'model "claude-sonnet-5" 'state "ok" 'seconds_left 'null 'available #t))))
  (s 'set-options! (hasheq 'current "@pool" 'model "claude-sonnet-5" 'reasoning "off" 'routes (list pool)
                           'providers (list (hasheq 'name "lmstudio" 'models '("gemma") 'has_key #t))))
  (s 'open!) (render s)
  (check-true (pair? (s 'hit-center 'provider "@pool")))                         ; 左栏：池
  (check-true (pair? (s 'hit-center 'provider "lmstudio")))
  (check-false (s 'hit-center 'model "gemma"))                                    ; 右栏是成员，不是模型
  (define m0 (s 'hit-center 'member 0))
  (check-equal? (s 'hit (car m0) (cdr m0)) '(member 0))                          ; 点成员 = 手动固定
  ;; route_status：fa 恢复 → 不可用数归零
  (s 'set-route! (hash-set* pool 'unavailable 0 'current 0))
  (render s)
  (check-true (pair? (s 'hit-center 'member 1))))

(test-case "adaptation rules: chips in the footer; click toggles (returns the new state); the menu stays open"
  (define s (new-mp))
  (s 'set-options! (hash-set OPTIONS 'adaptations (list (hasheq 'name "qwen3-no-think" 'enabled #t 'source "builtin")
                                                        (hasheq 'name "my-rule" 'enabled #f 'source "user"))))
  (s 'open!) (render s)
  (define a (s 'hit-center 'adapt (vector "qwen3-no-think" #t)))
  (check-equal? (s 'hit (car a) (cdr a)) '(adapt "qwen3-no-think" #f))           ; 开着 → 关
  (define b (s 'hit-center 'adapt (vector "my-rule" #f)))
  (check-equal? (s 'hit (car b) (cdr b)) '(adapt "my-rule" #t))
  (check-true (s 'open?))
  (s 'set-state! (hasheq 'adaptations '()))                                        ; state 事件：当前模型没有规则
  (render s)
  (check-false (s 'hit-center 'adapt (vector "my-rule" #f))))
