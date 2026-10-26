#lang racket/base
;; interaction.rkt — GUI 元数据平面 3：交互（纯数据 + 检查）。
;;   意图（intent）：与输入设备无关的「用户想做什么」——点开、右键菜单、滚动、横扫一行、拖、悬停、缩放、中键、后退 / 前进。
;;   交互配置（profile）：每个平台的原生输入 → 意图的途径表，外加识别用的常数（阈值）。macOS 是实测过的；Windows 是按惯例写的草案
;;     （status documented：Windows 组接手后按实测改）。Windows 鼠标能做的更多（中键、侧键、Ctrl+滚轮）——这些在 Windows 配置里有、macOS 没有。
;;   元素（element）：界面里每种可交互的东西声明它接受哪些意图、各做什么；
;;     required 意图必须在每个平台配置里都能到达，否则要声明 alternatives（同一动作的别的到达方式，如右键菜单里的同名项、悬停按钮、单键）；
;;     optional 意图是平台加强（只在有途径的平台出现，不要求处处都有）。
;;   interaction-problems：上面这条规则的检查（不需要窗口，Windows 组改配置时同样能跑）。

(require racket/list racket/string)

;; ---------------------------------------------------------------- 意图
(define INTENTS
  '((activate "primary click on an item / Return on the keyboard-focused item")
    (activate-double "double click")
    (context "open the item's context menu")
    (scroll "scroll the content under the pointer")
    (swipe-row "horizontal swipe on a list row")
    (drag "press, move, release")
    (hover "pointer over an item (highlight, reveal buttons, cursor shape)")
    (zoom "change the UI scale")
    (auxiliary "middle click")
    (navigate "back / forward")
    (keyboard "single-key operation on the keyboard-focused item")))
(define INTENT-NAMES (map car INTENTS))

;; ---------------------------------------------------------------- 平台交互配置
;; (平台 (status …) (constants (名字 值 说明) …) (意图 途径 …) …)；途径是描述性 datum（给人和 Windows 组看的），检查只看「有没有」
(define PROFILES
  '((macos
     (status verified)
     (constants
      (wheel-line-points 20.0 "a non-precise wheel step (classic mouse) scrolls this many points")
      (swipe-start 6.0 "horizontal travel (points) before a precise horizontal scroll over a row becomes a swipe")
      (swipe-dominance 1.5 "a swipe needs |dx| > dominance × |dy|")
      (swipe-cancel 6.0 "vertical travel (points) that turns the gesture into an ordinary scroll")
      (drag-start 6.0 "pointer travel (points) after press before a row drag starts")
      (double-click "system" "click counting comes from the system (NSEvent clickCount)"))
     (activate (mouse left click) (key return on-focused-item))
     (activate-double (mouse left double-click))
     (context (mouse right click) (mouse ctrl+left click) (trackpad two-finger-tap "delivered as a right click"))
     (scroll (trackpad precise-pixels with-phases momentum) (wheel lines))
     (swipe-row (trackpad precise horizontal-scroll over-row))
     (drag (mouse left press-move-release))
     (hover (pointer move))
     (zoom (key (primary equal)) (key (primary minus)) (key (primary digit-0)))
     (keyboard (key (primary shift e) enters-sidebar-keyboard-mode)))
    (windows
     (status documented)
     (constants
      (wheel-line-points 20.0 "WHEEL_DELTA (120) = one line; keep the same points per line as macOS")
      (swipe-start 6.0 "precision touchpad horizontal scroll (WM_MOUSEHWHEEL / DirectManipulation): same rule as macOS, unverified")
      (swipe-dominance 1.5 "same as macOS, unverified")
      (swipe-cancel 6.0 "same as macOS, unverified")
      (drag-start "system" "GetSystemMetrics(SM_CXDRAG / SM_CYDRAG), typically 4 px")
      (double-click "system" "WM_LBUTTONDBLCLK with CS_DBLCLKS, or GetDoubleClickTime + SM_CXDOUBLECLK"))
     (activate (mouse left click) (key return on-focused-item))
     (activate-double (mouse left double-click))
     (context (mouse right click) (key (shift f10)) (key menu-key "VK_APPS"))
     (scroll (wheel lines WM_MOUSEWHEEL) (wheel shift+wheel horizontal) (tilt-wheel WM_MOUSEHWHEEL) (touchpad precise "precision touchpad"))
     (swipe-row (touchpad precise horizontal-scroll over-row "precision touchpad only; mice have no swipe"))
     (drag (mouse left press-move-release))
     (hover (pointer move))
     (zoom (key (primary equal)) (key (primary minus)) (key (primary digit-0)) (wheel ctrl+wheel))
     (auxiliary (mouse middle click))
     (navigate (mouse xbutton1 back) (mouse xbutton2 forward))
     (keyboard (key (primary shift e) enters-sidebar-keyboard-mode)))))

(define (profile-of platform) (cond [(assq platform PROFILES) => cdr] [else (error 'profile-of "no interaction profile ~s" platform)]))
(define (profile-paths platform intent) (cond [(assq intent (profile-of platform)) => cdr] [else '()]))
(define (profile-constant platform name)
  (define cs (cond [(assq 'constants (profile-of platform)) => cdr] [else '()]))
  (cond [(assq name cs) => cadr] [else (error 'profile-constant "~s has no constant ~s" platform name)]))
(define (profile-status platform) (cadr (assq 'status (profile-of platform))))
(define PLATFORMS (map car PROFILES))

;; 一个平台缺某个意图时，「不需要输入设备」的另一种做法是否到达——只有键盘意图恒可用（每个平台都有键盘）
(define (reachable? platform intent) (pair? (profile-paths platform intent)))

;; ---------------------------------------------------------------- 元素：界面里可交互的东西
;; (元素 (意图 动作 [#:optional] [#:alternatives (意图 …)] [#:note "…"]) …)
;; 动作是给人看的名字（对应 app.rkt 里的处理）；alternatives 是同一动作的其他意图（那些意图的元素声明里应当也有同名动作）
(define ELEMENTS
  '((sidebar-row
     (activate resume-session)
     (activate-double rename #:alternatives (context keyboard) #:note "row menu: Rename; keyboard: R")
     (context row-menu)
     (swipe-row pin-or-archive-or-delete #:alternatives (context keyboard) #:note "row menu: Pin / Archive / Delete; keyboard: P E ⌫; hover icons")
     (drag drop-on-group #:alternatives (context) #:note "row menu: Pin / Archive / link to project")
     (hover reveal-row-buttons)
     (keyboard pin-archive-rename-info-delete)
     (auxiliary row-info #:optional #:note "Windows: middle click opens the info panel (platform enhancement, not built yet)"))
    (sidebar-divider
     (drag resize-sidebar)
     (hover resize-cursor))
    (transcript
     (scroll scroll-history)
     (drag select-text)
     (activate tool-card-or-permission-or-copy-or-code-toggle)
     (navigate previous-or-next-session #:optional #:note "Windows: mouse side buttons (platform enhancement, not built yet)"))
    (input-box
     (activate place-caret)
     (drag select-text)
     (scroll scroll-input))
    (model-picker
     (activate open-or-choose)
     (scroll scroll-column)
     (hover highlight-row))
    (window
     (zoom change-ui-scale))))

(define (element-intents el) (cond [(assq el ELEMENTS) => cdr] [else '()]))
(define (decl-intent d) (car d))
(define (decl-action d) (cadr d))
(define (decl-opt d key) (let loop ([xs (cddr d)]) (cond [(null? xs) #f] [(eq? (car xs) key) (if (and (pair? (cdr xs)) (not (keyword? (cadr xs)))) (cadr xs) #t)] [else (loop (cdr xs))])))
(define (decl-optional? d) (and (memq '#:optional (cddr d)) #t))

;; 检查：词汇、配置形状、每个元素的每个 required 意图在每个平台都能到达（或经 alternatives 到达）
(define (interaction-problems)
  (append
   (for*/list ([p (in-list PROFILES)] [entry (in-list (cdr p))]
               #:unless (memq (car entry) (append '(status constants) INTENT-NAMES)))
     (format "profile ~a: unknown intent ~s" (car p) (car entry)))
   (for*/list ([el (in-list ELEMENTS)] [d (in-list (cdr el))] #:unless (memq (decl-intent d) INTENT-NAMES))
     (format "element ~a: unknown intent ~s" (car el) (decl-intent d)))
   (for*/list ([el (in-list ELEMENTS)] [d (in-list (cdr el))] #:unless (decl-optional? d)
               [platform (in-list PLATFORMS)]
               #:unless (reachable? platform (decl-intent d))
               #:unless (let ([alts (decl-opt d '#:alternatives)])
                          (and (list? alts) (pair? alts) (ormap (lambda (i) (reachable? platform i)) alts))))
     (format "element ~a: ~a (~a) is not reachable on ~a and declares no reachable alternative"
             (car el) (decl-intent d) (decl-action d) platform))))

;; 某平台上一个元素实际有哪些意图（optional 的只在有途径时算）——给 Windows 组看「这个平台要实现什么」
(define (element-intents-on el platform)
  (for/list ([d (in-list (element-intents el))]
             #:when (or (not (decl-optional? d)) (reachable? platform (decl-intent d))))
    (decl-intent d)))

(provide INTENTS INTENT-NAMES PROFILES PLATFORMS profile-paths profile-constant profile-status reachable?
         ELEMENTS element-intents element-intents-on decl-intent decl-action decl-opt decl-optional? interaction-problems)
