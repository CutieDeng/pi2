#lang racket/base
;; platform.rkt — GUI 元数据平面 2：平台能力与平台请求（纯数据 + 纯函数，各平台共用）。
;;
;; 能力（capability）：平台报告自己能做什么（window-port 'caps → hash，键取自 CAPABILITIES）；拼装层只按能力分支，不按平台名分支。
;;   每项给出含义与「没有时怎么办」（fallback）——Windows 组照表实现或声明没有。
;; 平台状态（platform state）：拼装层每帧算出「窗口应当处在的状态」（hash，键取自 STATE-KEYS），
;;   与上一帧比较得出请求（request）表，交给 window-port 'apply! 执行；请求也记进操作日志（kind 'plat）——
;;   这样平台这一侧的行为是数据：录制的请求流可以喂给另一个平台的后端做一致性检查。

(require racket/list)

;; ---------------------------------------------------------------- 能力
;; (名字 值的形状 含义 没有时)
(define CAPABILITIES
  '((glass boolean "translucent material behind the sidebar (macOS: NSGlassEffectView; Windows 11: Mica / Acrylic backdrop)"
           "opaque sidebar; the 'glass' material setting is not offered")
    (transparent-window boolean "window areas that are not drawn show the desktop"
                        "the 'desktop' material setting is not offered")
    (ime symbol "input method model: marked-text (in-place composition with a caret rect for the candidate window) | none"
         "none: committed text only, no composition shown in the input box")
    (capture symbol "screenshot of the app's own window content for the automatic visual checks: self | none"
             "none: visual checks fall back to the drawlist (no compositor result)")
    (native-input-sim boolean "smoke tests can inject input through the native path (macOS: NSEvent / CGEvent; Windows: SendInput)"
                      "smoke tests inject logical events directly (not the native path)")
    (precise-scroll boolean "pixel-precise scroll deltas with phases and momentum (trackpad / precision touchpad)"
                    "line-based wheel steps; scrolling is animated by the app")
    (pinch boolean "pinch / magnify gesture events" "zoom by keyboard and primary+wheel only")
    (mouse-buttons integer "number of mouse buttons delivered (3 = left right middle; 5 = with back / forward)"
                   "features bound to extra buttons need another binding (the interaction lint checks this)")))
(define CAPABILITY-NAMES (map car CAPABILITIES))

;; 能力记录的检查：键都认得、值的形状对
(define (caps-problems caps)
  (for/list ([(k v) (in-hash caps)]
             #:unless (let ([spec (assq k CAPABILITIES)])
                        (and spec (case (cadr spec)
                                    [(boolean) (boolean? v)]
                                    [(symbol) (symbol? v)]
                                    [(integer) (exact-nonnegative-integer? v)]
                                    [else #f]))))
    (format "capability ~s: ~s" k v)))
(define (cap caps name [default #f]) (hash-ref caps name default))

;; ---------------------------------------------------------------- 平台状态与请求
;; (键 缺省值 含义)
(define STATE-KEYS
  '((text-input #t "keys go through the input method (#f: raw key events only — sidebar keyboard mode)")
    (material opaque "opaque | transparent: transparent = the window and the main layer let the desktop / glass show through where nothing is drawn")
    (glass-rect #f "#f | (vector x y w h radius) in window points: where the glass panel sits")
    (caret #f "#f | (vector x y w h index): input box caret in window points (candidate window position)")
    (cursor arrow "arrow | resize-h | text | pointer")
    (content-size #f "#f | (cons w h): requested content size in points (the app resizes its window)")))
(define STATE-DEFAULTS (for/hasheq ([k (in-list STATE-KEYS)]) (values (car k) (cadr k))))

(struct req (key value) #:prefab)                    ; 一条请求：把 key 设成 value

;; 上一帧已执行的状态 + 这一帧想要的状态 → 请求表（只含变了的键，按 STATE-KEYS 的次序）；不认识的键报错
(define (state-diff applied wanted)
  (for ([k (in-hash-keys wanted)]) (unless (assq k STATE-KEYS) (error 'state-diff "unknown platform state key ~s" k)))
  (for/list ([spec (in-list STATE-KEYS)]
             #:when (hash-has-key? wanted (car spec))
             #:unless (equal? (hash-ref applied (car spec) (cadr spec)) (hash-ref wanted (car spec))))
    (req (car spec) (hash-ref wanted (car spec)))))

;; 请求作用到已执行的状态上（拼装层记下「平台现在是什么样」）
(define (state-apply applied reqs)
  (for/fold ([s applied]) ([r (in-list reqs)]) (hash-set s (req-key r) (req-value r))))

(provide CAPABILITIES CAPABILITY-NAMES caps-problems cap
         STATE-KEYS STATE-DEFAULTS (struct-out req) state-diff state-apply)
