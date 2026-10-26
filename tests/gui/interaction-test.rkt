#lang racket/base
;; interaction-test.rkt — GUI 元数据平面 3：意图词汇、平台交互配置、元素声明的可达性检查，与识别器（gesture.rkt）。纯数据，不开窗口。

(require rackunit racket/list "../../src/gui/compute/interaction.rkt" "../../src/gui/compute/gesture.rkt")

(test-case "the current element declarations are reachable on every platform profile"
  (check-equal? (interaction-problems) '()))

(test-case "profiles: macOS is verified, Windows is a documented draft; Windows offers more mouse intents"
  (check-eq? (profile-status 'macos) 'verified)
  (check-eq? (profile-status 'windows) 'documented)
  (check-false (reachable? 'macos 'auxiliary))
  (check-true (reachable? 'windows 'auxiliary))
  (check-true (reachable? 'windows 'navigate))
  ;; 平台加强：中键 / 侧键的意图只在 Windows 出现在元素上
  (check-false (memq 'auxiliary (element-intents-on 'sidebar-row 'macos)))
  (check-not-false (memq 'auxiliary (element-intents-on 'sidebar-row 'windows)))
  (check-not-false (memq 'navigate (element-intents-on 'transcript 'windows))))

(test-case "the lint catches an intent with no path and no reachable alternative"
  (define d '(swipe-row archive))                         ; 没有 alternatives
  (check-false (decl-optional? d))
  (check-equal? (decl-opt '(activate-double rename #:alternatives (context keyboard)) '#:alternatives) '(context keyboard))
  (check-true (decl-optional? '(auxiliary info #:optional))))

(test-case "recognizer: press → intent (same rules as before the refactor)"
  (check-eq? (press-intent 0 '() 1) 'activate)
  (check-eq? (press-intent 0 '() 2) 'activate-double)
  (check-eq? (press-intent 1 '() 1) 'context)
  (check-eq? (press-intent 0 '(ctrl) 1) 'context "macOS ctrl-click")
  (check-eq? (press-intent 2 '() 1) 'auxiliary)
  (check-eq? (press-intent 3 '() 1) 'navigate-back)
  (check-eq? (press-intent 4 '() 1) 'navigate-forward))

(test-case "recognizer: wheel steps, swipe and drag thresholds come from the profile constants"
  (define c (consts-for 'macos))
  (check-equal? (scroll-points c #f 2.0) 40.0)
  (check-equal? (scroll-points c #t 2.0) 2.0)
  ;; 横扫：began 记行 → 横向累计超过 6 且占优 → 开始
  (define-values (c1 r1) (swipe-step c #f 'began #t "row-a" 0.0 0.0))
  (check-equal? (list c1 r1) (list (vector "row-a" 0.0 0.0) 'scroll))
  (define-values (c2 r2) (swipe-step c c1 'changed #t #f 4.0 1.0))
  (check-eq? r2 'pending)
  (define-values (c3 r3) (swipe-step c c2 'changed #t #f 4.0 0.0))
  (check-equal? (list c3 r3) (list #f (list 'swipe "row-a" 8.0)))
  ;; 纵向先超过 6 → 普通滚动
  (define-values (d2 s2) (swipe-step c (vector "row-a" 0.0 0.0) 'changed #t #f 1.0 7.0))
  (check-equal? (list d2 s2) (list #f 'scroll))
  ;; 不在可横扫的行上（row #f）：横向再多也不横扫
  (define-values (e2 t2) (swipe-step c (vector #f 0.0 0.0) 'changed #t #f 20.0 0.0))
  (check-eq? t2 'pending)
  ;; 拖：曼哈顿距离 > 6
  (check-false (drag-started? c 10.0 10.0 13.0 13.0))
  (check-true (drag-started? c 10.0 10.0 14.0 13.0)))
