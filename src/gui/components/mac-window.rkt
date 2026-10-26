#lang racket/base
;; appkit.rkt — 窗口、事件泵、NSEvent → core/event 翻译（只在 OS 主线程调用，P1）。
;; 主循环由 Racket 持有：pump-events! 非阻塞取完 NSEvent 并分发，返回本轮翻译出的事件。
;; ObjC 子类在 app-init! 时才注册（模块被其它 place 加载时不会重复注册，P2）。

(require ffi/unsafe racket/file "../compute/mac-objc.rkt" "../compute/mac-cf.rkt" "../compute/mac-geom.rkt" "../compute/event.rkt" "../compute/mac-capture.rkt"
         "../compute/mac-keys.rkt"                    ; 原生 → 逻辑：键、修饰键、编辑命令（GUI 元数据平面的 macOS 对照表）
         "../compute/platform.rkt"                    ; 能力记录、平台请求
         "../compute/hub.rkt" "../compute/interfaces.rkt")

(define _libappkit (ffi-lib "/System/Library/Frameworks/AppKit.framework/AppKit"))
(define libfoundation (ffi-lib "/System/Library/Frameworks/Foundation.framework/Foundation"))
(define NSDefaultRunLoopMode (get-ffi-obj 'NSDefaultRunLoopMode libfoundation _pointer))

(define ACTIVATION-REGULAR 0)
(define STYLE-TITLED+CLOSABLE+MINI+RESIZABLE 15)
(define BACKING-BUFFERED 2)
(define EVENT-MASK-ANY #xFFFFFFFFFFFFFFFF)

;; ---------------------------------------------------------------- 消息

(define-msg m-shared-application "sharedApplication" _id ())
(define-msg m-set-activation-policy "setActivationPolicy:" _BOOL ([p _int64]))
(define-msg m-finish-launching "finishLaunching" _void ())
(define-msg m-activate-ignoring "activateIgnoringOtherApps:" _void ([b _BOOL]))
(define-msg m-next-event "nextEventMatchingMask:untilDate:inMode:dequeue:" _id
  ([mask _uint64] [date _id] [mode _id] [dequeue _BOOL]))
(define-msg m-send-event "sendEvent:" _void ([e _id]))
(define-msg m-set-main-menu "setMainMenu:" _void ([m _id]))
(define-msg m-distant-past "distantPast" _id ())

(define-msg m-init-window "initWithContentRect:styleMask:backing:defer:" _id
  ([r _CGRect] [style _uint64] [backing _uint64] [defer _BOOL]))
(define-msg m-set-title "setTitle:" _void ([s _id]))
(define-msg m-set-content-view "setContentView:" _void ([v _id]))
(define-msg m-set-delegate "setDelegate:" _void ([d _id]))
(define-msg m-set-accepts-mouse-moved "setAcceptsMouseMovedEvents:" _void ([b _BOOL]))
(define-msg m-set-released-when-closed "setReleasedWhenClosed:" _void ([b _BOOL]))
(define-msg m-make-key-and-order-front "makeKeyAndOrderFront:" _void ([s _id]))
(define-msg m-make-first-responder "makeFirstResponder:" _BOOL ([r _id]))
(define-msg m-center "center" _void ())
(define-msg m-close "close" _void ())
(define-msg m-backing-scale "backingScaleFactor" _double ())
(define-msg m-set-content-size "setContentSize:" _void ([s _CGSize]))

(define-msg m-init-with-frame "initWithFrame:" _id ([r _CGRect]))
(define-msg m-set-layer "setLayer:" _void ([l _id]))
(define-msg m-set-wants-layer "setWantsLayer:" _void ([b _BOOL]))
(define-msg m-bounds "bounds" _CGRect ())
(define-msg m-convert-point "convertPoint:fromView:" _CGPoint ([p _CGPoint] [v _id]))

(define-msg m-location-in-window "locationInWindow" _CGPoint ())
(define-msg m-modifier-flags "modifierFlags" _uint64 ())
(define-msg m-key-code "keyCode" _uint16 ())
(define-msg m-characters "characters" _id ())
(define-msg m-characters-ignoring "charactersIgnoringModifiers" _id ())

;; 平台请求（platform.rkt req）→ 窗口操作。值的形状见 platform.rkt STATE-KEYS
(define (apply-request! win r)
  (define v (req-value r))
  (case (req-key r)
    [(text-input) (set-box! text-input-on (and v #t))]
    [(material) (window-set-transparent! win (eq? v 'transparent))]
    [(glass-rect) (window-set-glass-rect! win v)]
    [(caret) (when (vector? v) (window-set-text-input-state! win (vector (vector-ref v 0) (vector-ref v 1) (vector-ref v 2) (vector-ref v 3)) (vector-ref v 4)))]
    [(cursor) (set-cursor! v)]
    [(content-size) (when (pair? v) (window-set-content-size! win (car v) (cdr v)))]
    [else (void)]))
(define-msg m-is-repeat "isARepeat" _BOOL ())
(define-msg m-click-count "clickCount" _int64 ())
(define-msg m-scrolling-dx "scrollingDeltaX" _double ())
(define-msg m-scrolling-dy "scrollingDeltaY" _double ())
(define-msg m-precise? "hasPreciseScrollingDeltas" _BOOL ())
(define-msg m-phase "phase" _uint64 ())
(define-msg m-momentum-phase "momentumPhase" _uint64 ())

(define-msg m-init-with-title "initWithTitle:" _id ([t _id]))
(define-msg m-add-item "addItem:" _void ([i _id]))
(define-msg m-init-menu-item "initWithTitle:action:keyEquivalent:" _id ([t _id] [a _SEL] [k _id]))
(define-msg m-set-submenu "setSubmenu:" _void ([m _id]))
(define-msg m-set-target "setTarget:" _void ([t _id]))

;; 文字输入（NSTextInputClient 相关）
(define-msg m-input-context "inputContext" _id ())
(define-msg m-handle-event "handleEvent:" _BOOL ([e _id]))
(define-msg m-invalidate-char-coords "invalidateCharacterCoordinates" _void ())
(define-msg m-discard-marked "discardMarkedText" _void ())
(define-msg m-is-kind-of "isKindOfClass:" _BOOL ([c _id]))
(define-msg m-string "string" _id ())
(define-msg m-array "array" _id ())
(define-msg m-convert-rect-to-view "convertRect:toView:" _CGRect ([r _CGRect] [v _id]))
(define-msg m-convert-rect-to-screen "convertRectToScreen:" _CGRect ([r _CGRect]))
(define-msg m-separator-item "separatorItem" _id ())
(define-msg m-set-markedtext "setMarkedText:selectedRange:replacementRange:" _void ([s _id] [sel _NSRange] [rep _NSRange]))
(define-msg m-insert-text "insertText:replacementRange:" _void ([s _id] [rep _NSRange]))
(define-msg m-first-rect "firstRectForCharacterRange:actualRange:" _CGRect ([r _NSRange] [actual _pointer]))
(define-msg m-has-marked "hasMarkedText" _BOOL ())

;; 剪贴板
(define-msg m-general-pasteboard "generalPasteboard" _id ())
(define-msg m-unique-pasteboard "pasteboardWithUniqueName" _id ())
(define-msg m-string-for-type "stringForType:" _id ([t _id]))
(define-msg m-clear-contents "clearContents" _int64 ())
(define-msg m-set-string-for-type "setString:forType:" _BOOL ([s _id] [t _id]))
(define-msg m-release-globally "releaseGlobally" _void ())
(define NSPasteboardTypeString (get-ffi-obj 'NSPasteboardTypeString _libappkit _pointer))
(define-msg m-pasteboard-items "pasteboardItems" _id ())
(define-msg m-data-for-type "dataForType:" _id ([t _id]))
(define-msg m-ns-count "count" _uint64 ())
(define-msg m-object-at-index "objectAtIndex:" _id ([i _uint64]))
(define-msg m-data-bytes "bytes" _pointer ())
(define-msg m-data-length "length" _uint64 ())
(define-msg m-dragging-pasteboard "draggingPasteboard" _id ())
(define-msg m-register-dragged "registerForDraggedTypes:" _void ([types _id]))
(define-msg m-array-with-object "arrayWithObject:" _id ([o _id]))

;; ---------------------------------------------------------------- 事件队列 / 窗口表

(define queue '())                       ; 逆序
(define (push! e) (set! queue (cons e queue)))

(define (key-of p) (cast p _pointer _uintptr))

;; 窗口状态。view / delegate 回调通过 self 指针找回所属窗口。
;; IME 镜像：AppKit 同步查询（hasMarkedText / markedRange / firstRectForCharacterRange:…）要立即得到答案，
;; 而 app 异步处理事件，所以平台层在这里保存最近一次的组字长度与光标矩形（app 每帧经
;; window-set-text-input-state! 更新）。
(struct window (ns view delegate [w #:mutable] [h #:mutable] [scale #:mutable] [on-live-resize #:mutable]
                [caret-rect #:mutable]                  ; (vector x y w h)，内容区 point，左上原点
                [caret-index #:mutable]                 ; 光标在文档中的位置（码点）
                [marked-len16 #:mutable]                ; 组字串 UTF-16 长度，0 = 无
                [marked-sel16 #:mutable]
                [glass #:mutable] [overlay #:mutable]))   ; 液态玻璃面板（NSGlassEffectView）与它上面的透明 Metal 视图；没有 = #f
(define by-object (make-hash))           ; key-of(view|delegate) → window
(define overlay-views (make-hash))       ; key-of(overlay view) → #t：它不做第一响应者（键盘留给主视图）

(define (decode-mods flags) (mac-logical-mods flags))   ; 逻辑修饰键：⌘ → primary（mac-keys.rkt）

(define (decode-phase p momentum)
  (cond
    [(not (zero? momentum)) 'momentum]
    [(not (zero? (bitwise-and p 1))) 'began]
    [(not (zero? (bitwise-and p 4))) 'changed]
    [(not (zero? (bitwise-and p 24))) 'ended]       ; ended | cancelled
    [else 'none])
) ; end define decode-phase

;; 内容区尺寸 / scale 重新读取；变化时入队 ev:resize 并调用 live-resize 钩子（P5：嵌套 run loop 内也会回调）。
(define (refresh-size! win)
  (define b (m-bounds (window-view win)))
  (define w (rect-w b))
  (define h (rect-h b))
  (define s (m-backing-scale (window-ns win)))
  (unless (and (= w (window-w win)) (= h (window-h win)) (= s (window-scale win)))
    (set-window-w! win w)
    (set-window-h! win h)
    (set-window-scale! win s)
    (push! (ev:resize w h s))
    (define hook (window-on-live-resize win))
    (when hook (hook w h s)))
) ; end define refresh-size!

;; ---------------------------------------------------------------- 文字输入辅助

(define (ns->string o)                                   ; NSString 或 NSAttributedString → Racket 字符串
  (cond [(not o) ""]
        [(m-is-kind-of o (cls "NSAttributedString")) (cfstring->string (m-string o))]
        [else (cfstring->string o)])
) ; end define ns->string

(define (utf16-length s) (for/sum ([c (in-string s)]) (if (> (char->integer c) #xFFFF) 2 1)))

(define (utf16->cp s u)                                  ; UTF-16 下标 → 码点下标
  (let loop ([i 0] [acc 0])
    (cond [(or (>= acc u) (>= i (string-length s))) i]
          [else (loop (add1 i) (+ acc (if (> (char->integer (string-ref s i)) #xFFFF) 2 1)))]))
) ; end define utf16->cp

(define (with-window/default self dflt proc)
  (define win (hash-ref by-object (key-of self) #f))
  (if win (proc win) dflt)
) ; end define with-window/default

;; 光标矩形（内容区，左上原点）→ 屏幕坐标（左下原点），供输入法定位候选框
(define (caret-screen-rect win)
  (define c (window-caret-rect win))
  (define r (m-convert-rect-to-view (window-view win)
                                    (rect (vector-ref c 0) (vector-ref c 1) (vector-ref c 2) (vector-ref c 3)) #f))
  (m-convert-rect-to-screen (window-ns win) r)
) ; end define caret-screen-rect

;; ---------------------------------------------------------------- ObjC 子类（Racket 实现）

(define (mouse-handler kind button)
  (lambda (self ev)
    (define p (m-convert-point self (m-location-in-window ev) #f))   ; isFlipped → 左上原点
    (push! (ev:mouse kind (CGPoint-x p) (CGPoint-y p) button
                     (decode-mods (m-modifier-flags ev)) (m-click-count ev))))
) ; end define mouse-handler

;; 按键：先入队原始 ev:key（app 用来看修饰键 / Cmd 快捷键），再交给输入上下文
;; （NSTextInputContext → 输入法 → insertText: / setMarkedText: / doCommandBySelector:）。
;; 带 Cmd 的组合键不进输入上下文（菜单没接住的快捷键只作为 ev:key 交给 app）。
;; text-input-on：#f 时按键不进输入上下文（侧栏有键盘焦点时：单键操作，不能让输入法把字母吃成候选）
(define text-input-on (box #t))
(define (key-handler down?)
  (lambda (self ev)
    (define mods (decode-mods (m-modifier-flags ev)))
    (define code (m-key-code ev))
    (push! (ev:key down? code (cfstring->string (m-characters ev)) mods (m-is-repeat ev)
                   (mac-logical-key code (cfstring->string (m-characters-ignoring ev)))))
    (when (and down? (not (memq 'primary mods)) (unbox text-input-on))
      (define ctx (m-input-context self))
      (when ctx (m-handle-event ctx ev))))
) ; end define key-handler

(define (edit-action name)
  (lambda (self sender) (push! (ev:command (mac-command name))))
) ; end define edit-action

(define (with-window self proc)
  (define win (hash-ref by-object (key-of self) #f))
  (when win (proc win))
) ; end define with-window

(define classes #f)                      ; (vector view-class delegate-class target-class offscreen-window-class)

(define (register-classes!)
  (define view-class
    (define-objc-subclass "PiView" "NSView"
      (list (objc-method "acceptsFirstResponder" _BOOL '() (lambda (self) (not (hash-ref overlay-views (key-of self) #f))))
            (objc-method "isFlipped" _BOOL '() (lambda (self) #t))
            ;; 窗口不在前台时第一次点击也交给 view（否则只用来激活窗口，权限按钮要点两次）
            (objc-method "acceptsFirstMouse:" _BOOL (list _id) (lambda (self ev) #t))
            (objc-method "keyDown:" _void (list _id) (key-handler #t))
            (objc-method "keyUp:" _void (list _id) (key-handler #f))
            (objc-method "mouseDown:" _void (list _id) (mouse-handler 'down 0))
            (objc-method "mouseUp:" _void (list _id) (mouse-handler 'up 0))
            (objc-method "mouseDragged:" _void (list _id) (mouse-handler 'drag 0))
            (objc-method "mouseMoved:" _void (list _id) (mouse-handler 'move 0))
            (objc-method "rightMouseDown:" _void (list _id) (mouse-handler 'down 1))
            (objc-method "rightMouseUp:" _void (list _id) (mouse-handler 'up 1))
            (objc-method "scrollWheel:" _void (list _id)
                         (lambda (self ev)
                           (define p (m-convert-point self (m-location-in-window ev) #f))   ; 指针位置：滚轮按区域分派
                           (push! (ev:scroll (m-scrolling-dx ev) (m-scrolling-dy ev) (m-precise? ev)
                                             (decode-phase (m-phase ev) (m-momentum-phase ev))
                                             (CGPoint-x p) (CGPoint-y p)))))
            ;; Edit 菜单（target = nil → 响应链 → 本 view）
            (objc-method "undo:" _void (list _id) (edit-action 'undo:))
            (objc-method "redo:" _void (list _id) (edit-action 'redo:))
            (objc-method "cut:" _void (list _id) (edit-action 'cut:))
            (objc-method "copy:" _void (list _id) (edit-action 'copy:))
            (objc-method "selectAll:" _void (list _id) (edit-action 'selectAll:))
            ;; 粘贴：剪贴板里有文件（Finder 里复制的）或图片 → 附件；否则文字
            (objc-method "paste:" _void (list _id)
                         (lambda (self sender)
                           (define items (pasteboard-attachments (pasteboard)))
                           (push! (if (pair? items) (ev:attach items) (ev:paste (clipboard-get-string))))))
            ;; 拖进窗口的文件 → 附件（NSDraggingDestination）
            (objc-method "draggingEntered:" _uint64 (list _id)
                         (lambda (self sender) (if (pair? (pasteboard-file-paths (m-dragging-pasteboard sender))) 1 0)))   ; 1 = NSDragOperationCopy
            (objc-method "performDragOperation:" _BOOL (list _id)
                         (lambda (self sender)
                           (define items (pasteboard-attachments (m-dragging-pasteboard sender)))
                           (and (pair? items) (begin (push! (ev:attach items)) #t))))
            ;; ---- NSTextInputClient（P4）
            (objc-method "insertText:replacementRange:" _void (list _id _NSRange)
                         (lambda (self s rep)
                           (with-window/default self (void) (lambda (win) (set-window-marked-len16! win 0)))
                           (push! (ev:text (ns->string s)))))
            (objc-method "setMarkedText:selectedRange:replacementRange:" _void (list _id _NSRange _NSRange)
                         (lambda (self s sel rep)
                           (define str (ns->string s))
                           (with-window/default self (void)
                             (lambda (win)
                               (set-window-marked-len16! win (utf16-length str))
                               (set-window-marked-sel16! win (NSRange-location sel))))
                           (push! (ev:marked str (utf16->cp str (NSRange-location sel)) 0))))
            (objc-method "unmarkText" _void '()
                         (lambda (self)
                           (with-window/default self (void) (lambda (win) (set-window-marked-len16! win 0)))
                           (push! (ev:marked "" 0 0))))
            (objc-method "hasMarkedText" _BOOL '()
                         (lambda (self) (with-window/default self #f (lambda (win) (> (window-marked-len16 win) 0)))))
            (objc-method "markedRange" _NSRange '()
                         (lambda (self)
                           (with-window/default self (make-NSRange NSNotFound 0)
                             (lambda (win)
                               (if (> (window-marked-len16 win) 0)
                                   (make-NSRange (window-caret-index win) (window-marked-len16 win))
                                   (make-NSRange NSNotFound 0)))))
                         #:default (make-NSRange NSNotFound 0))
            (objc-method "selectedRange" _NSRange '()
                         (lambda (self)
                           (with-window/default self (make-NSRange 0 0)
                             (lambda (win) (make-NSRange (+ (window-caret-index win) (window-marked-sel16 win)) 0))))
                         #:default (make-NSRange 0 0))
            (objc-method "validAttributesForMarkedText" _id '() (lambda (self) (m-array (cls "NSArray"))))
            (objc-method "attributedSubstringForProposedRange:actualRange:" _id (list _NSRange _pointer)
                         (lambda (self r actual) #f))
            (objc-method "firstRectForCharacterRange:actualRange:" _CGRect (list _NSRange _pointer)
                         (lambda (self r actual)
                           (with-window/default self (rect 0.0 0.0 0.0 0.0) caret-screen-rect))
                         #:default (rect 0.0 0.0 0.0 0.0))
            (objc-method "characterIndexForPoint:" _uint64 (list _CGPoint) (lambda (self p) NSNotFound))
            (objc-method "doCommandBySelector:" _void (list _SEL)
                         (lambda (self s) (push! (ev:command (mac-command (string->symbol (sel-name s))))))))
      #:protocols '("NSTextInputClient")))
  (define delegate-class
    (define-objc-subclass "PiWindowDelegate" "NSObject"
      (list (objc-method "windowShouldClose:" _BOOL (list _id)
                         (lambda (self sender) (push! (ev:close)) #f))   ; 是否关窗由 app 决定
            (objc-method "windowDidResize:" _void (list _id)
                         (lambda (self n) (with-window self refresh-size!)))
            (objc-method "windowDidChangeBackingProperties:" _void (list _id)
                         (lambda (self n) (with-window self refresh-size!)))
            (objc-method "windowDidBecomeKey:" _void (list _id) (lambda (self n) (push! (ev:focus #t))))
            (objc-method "windowDidResignKey:" _void (list _id) (lambda (self n) (push! (ev:focus #f)))))
      #:protocols '("NSWindowDelegate")))
  (define target-class
    (define-objc-subclass "PiAppTarget" "NSObject"
      (list (objc-method "piQuit:" _void (list _id) (lambda (self sender) (push! (ev:close)))))))
  ;; 后台窗口模式用的窗口类：不把窗口拉回屏幕内（NSWindow 缺省会让带标题栏的窗口至少留一条在屏幕上——实测放到 -30000 被拉回到 x=-920，右边 40 点还在屏幕里）
  (define offscreen-window-class
    (define-objc-subclass "PiOffscreenWindow" "NSWindow"
      (list (objc-method "constrainFrameRect:toScreen:" _CGRect (list _CGRect _id) (lambda (self r screen) r)
                         #:default (rect -30000.0 -30000.0 960.0 680.0)))))
  (vector view-class delegate-class target-class offscreen-window-class)
) ; end define register-classes!

;; ---------------------------------------------------------------- 应用

(define app #f)
(define distant-past #f)
(define app-target #f)

(define (make-main-menu!)
  (define bar (alloc-init "NSMenu"))
  (define app-item (alloc-init "NSMenuItem"))
  (m-add-item bar app-item)
  (define app-menu (alloc-init "NSMenu"))
  (define quit
    (call-with-cfstring "Quit pi2"
      (lambda (title)
        (call-with-cfstring "q"
          (lambda (key) (m-init-menu-item (msg-alloc (cls "NSMenuItem")) title (sel "piQuit:") key))))))
  (m-set-target quit app-target)
  (m-add-item app-menu quit)
  (m-set-submenu app-item app-menu)
  ;; Edit 菜单：target 为 nil，动作沿响应链送到第一响应者（PiView）
  (define edit-item (alloc-init "NSMenuItem"))
  (m-add-item bar edit-item)
  (define edit-menu (call-with-cfstring "Edit" (lambda (t) (m-init-with-title (msg-alloc (cls "NSMenu")) t))))
  (for ([spec (in-list '(("Undo" "undo:" "z") ("Redo" "redo:" "Z") #f
                         ("Cut" "cut:" "x") ("Copy" "copy:" "c") ("Paste" "paste:" "v") ("Select All" "selectAll:" "a")))])
    (m-add-item edit-menu
                (if spec
                    (call-with-cfstring (car spec)
                      (lambda (title)
                        (call-with-cfstring (caddr spec)
                          (lambda (key) (m-init-menu-item (msg-alloc (cls "NSMenuItem")) title (sel (cadr spec)) key)))))
                    (m-separator-item (cls "NSMenuItem")))))
  (m-set-submenu edit-item edit-menu)
  (m-set-main-menu app bar)
) ; end define make-main-menu!

(define (app-init!)
  (assert-main-thread! 'app-init!)
  (unless app
    (set! classes (register-classes!))
    (set! app (m-shared-application (cls "NSApplication")))
    (m-set-activation-policy app (if (offscreen?) 1 ACTIVATION-REGULAR))   ; 后台窗口模式：Accessory（不进 Dock、不被激活、不抢焦点）
    (m-finish-launching app)
    (set! distant-past (retain! (m-distant-past (cls "NSDate"))))
    (set! app-target (msg-init (msg-alloc (vector-ref classes 2))))
    (make-main-menu!))
) ; end define app-init!

;; layer：可选的 CALayer（CAMetalLayer），设为 layer-hosting view 的 layer。
;; overlay：可选的第二张透明 CAMetalLayer → 液态玻璃侧栏：内容区 = 容器视图，里面从下到上：主视图（layer）、
;;   玻璃面板（NSGlassEffectView，macOS 26；没有就用 NSVisualEffectView）、透明的覆盖视图（overlay，画侧栏）。
;;   覆盖视图与主视图同大、同坐标（都 flipped、原点在内容区左上），收鼠标事件但不做第一响应者；玻璃面板的位置由 set-glass-rect! 给。
(define-msg m-set-autoresizing "setAutoresizingMask:" _void ([m _uint64]))
(define-msg m-set-hidden "setHidden:" _void ([b _BOOL]))
(define-msg m-set-view-frame "setFrame:" _void ([r _CGRect]))
(define-msg m-set-corner-radius "setCornerRadius:" _void ([r _double]))
(define-msg m-add-subview "addSubview:" _void ([v _id]))
(define AUTORESIZE-WH 18)                ; NSViewWidthSizable | NSViewHeightSizable
(define (window-open! title w h #:layer [layer #f] #:overlay [overlay-layer #f])
  (app-init!)
  (define ns (m-init-window (msg-alloc (if (offscreen?) (vector-ref classes 3) (cls "NSWindow"))) (rect 0.0 0.0 w h)
                            STYLE-TITLED+CLOSABLE+MINI+RESIZABLE BACKING-BUFFERED #f))
  (m-set-released-when-closed ns #f)
  (call-with-cfstring title (lambda (t) (m-set-title ns t)))
  (define view (m-init-with-frame (msg-alloc (vector-ref classes 0)) (rect 0.0 0.0 w h)))
  (when layer
    (m-set-layer view layer)                 ; 先 setLayer 再 setWantsLayer → layer-hosting
    (m-set-wants-layer view #t))
  (define-values (glass overlay)
    (cond
      [overlay-layer
       (define container (m-init-with-frame (msg-alloc (cls "NSView")) (rect 0.0 0.0 w h)))
       (m-set-wants-layer container #t)
       (m-set-autoresizing view AUTORESIZE-WH)
       (m-add-subview container view)
       (define g (m-init-with-frame (msg-alloc (or (cls "NSGlassEffectView") (cls "NSVisualEffectView"))) (rect 0.0 0.0 10.0 10.0)))
       (when (cls "NSGlassEffectView") (m-set-corner-radius g 16.0))
       (m-set-hidden g #t)
       (m-add-subview container g)
       (define ov (m-init-with-frame (msg-alloc (vector-ref classes 0)) (rect 0.0 0.0 w h)))
       (m-set-layer ov overlay-layer)
       (m-set-wants-layer ov #t)
       (m-set-autoresizing ov AUTORESIZE-WH)
       (m-set-hidden ov #t)
       (m-add-subview container ov)
       (hash-set! overlay-views (key-of ov) #t)
       (m-set-content-view ns container)
       (values g ov)]
      [else (m-set-content-view ns view) (values #f #f)]))
  (for ([v (list view overlay)] #:when v)                                    ; 接受拖进来的文件（覆盖视图在上面：它也要收）
    (call-with-cfstring "public.file-url" (lambda (t) (m-register-dragged v (m-array-with-object (cls "NSArray") t)))))
  (define delegate (msg-init (msg-alloc (vector-ref classes 1))))
  (define win (window ns view delegate 0.0 0.0 0.0 #f (vector 0.0 0.0 1.0 16.0) 0 0 0 glass overlay))
  (hash-set! by-object (key-of view) win)
  (when overlay (hash-set! by-object (key-of overlay) win))
  (hash-set! by-object (key-of delegate) win)
  (m-set-delegate ns delegate)
  (m-set-accepts-mouse-moved ns #t)
  (m-center ns)
  (m-make-key-and-order-front ns #f)
  (when (offscreen?) (m-set-frame-origin ns (make-CGPoint -30000.0 -30000.0)))   ; 后台窗口模式：放到所有屏幕外面（真实鼠标碰不到、看不见）；要在 orderFront 之后（之前设会被拉回屏幕内，实测）
  (m-make-first-responder ns view)
  (unless (offscreen?) (m-activate-ignoring app #t))
  (refresh-size! win)
  win
) ; end define window-open!

;; 后台窗口模式（PI2_OFFSCREEN=1；实验，2026-10-25）：窗口流程跑的时候不出现在屏幕上、不接真实键鼠（输入全靠合成事件 sendEvent:）
(define (offscreen?) (and (getenv "PI2_OFFSCREEN") #t))
(define-msg m-set-frame-origin "setFrameOrigin:" _void ([p _CGPoint]))
(define (window-frame-origin win) (let ([r (m-frame (window-ns win))]) (values (CGPoint-x (CGRect-origin r)) (CGPoint-y (CGRect-origin r)))))

(define (window-size win) (values (window-w win) (window-h win) (window-scale win)))

;; 窗口透明：内容层没画的地方透出桌面
(define-msg m-win-set-opaque "setOpaque:" _void ([b _BOOL]))
(define-msg m-win-set-bg "setBackgroundColor:" _void ([c _id]))
(define-msg m-color-clear "clearColor" _id ())
(define-msg m-color-window-bg "windowBackgroundColor" _id ())
(define-msg m-set-ignores-mouse "setIgnoresMouseEvents:" _void ([b _BOOL]))
(define (window-set-transparent! win on?)
  (m-win-set-opaque (window-ns win) (not on?))
  ;; 透明窗口里全透明的像素缺省把点击漏给下面的窗口；显式设 NO → 整个窗口都收点击（推断：未用真实点击验证，没有辅助功能权限）
  (m-set-ignores-mouse (window-ns win) #f)
  (m-win-set-bg (window-ns win) (if on? (m-color-clear (cls "NSColor")) (m-color-window-bg (cls "NSColor")))))

;; 玻璃面板的位置（内容区 point，左上原点）；#f = 不用玻璃（面板与覆盖视图都隐藏，鼠标落到主视图）
(define (window-set-glass-rect! win r)
  (define g (window-glass win))
  (when g
    (cond
      [r (define h (window-h win))
         (m-set-view-frame g (rect (vector-ref r 0) (- h (vector-ref r 1) (vector-ref r 3)) (vector-ref r 2) (vector-ref r 3)))   ; 容器不翻转：y 从下往上
         (when (and (> (vector-length r) 4) (cls "NSGlassEffectView")) (m-set-corner-radius g (vector-ref r 4)))
         (m-set-hidden g #f) (m-set-hidden (window-overlay win) #f)]
      [else (m-set-hidden g #t) (m-set-hidden (window-overlay win) #t)]))
) ; end define window-set-glass-rect!

;; ---------------------------------------------------------------- 外观与显示效果的自动检查
;; 界面只有暗色：窗口强制 DarkAqua（标题栏、液态玻璃都按暗色画）。系统是亮色外观时不强制的话，玻璃是浅色的、
;; 侧栏的浅色字压在上面（实测对比度 1.5，见 design-gui.md §28）
(define-msg m-set-appearance "setAppearance:" _void ([a _id]))
(define-msg m-appearance-named "appearanceNamed:" _id ([n _id]))
(define (window-set-dark! win)
  (call-with-cfstring "NSAppearanceNameDarkAqua"
    (lambda (n) (m-set-appearance (window-ns win) (m-appearance-named (cls "NSAppearance") n)))))

(define-msg m-frame "frame" _CGRect ())
(define-msg m-content-rect-for-frame "contentRectForFrameRect:" _CGRect ([r _CGRect]))
(define-msg m-screens "screens" _id ())
(define-msg m-ca-flush "flush" _void ())
;; 内容区在全局显示坐标里的位置（左上原点，point）
(define (window-content-screen-rect win)
  (define cr (m-content-rect-for-frame (window-ns win) (m-frame (window-ns win))))
  (define sh (CGSize-h (CGRect-size (m-frame (m-object-at-index (m-screens (cls "NSScreen")) 0)))))   ; 主屏高（全局坐标的原点在主屏左上）
  (define o (CGRect-origin cr)) (define s (CGRect-size cr))
  (vector (CGPoint-x o) (- sh (+ (CGPoint-y o) (CGSize-h s))) (CGSize-w s) (CGSize-h s)))
;; 截内容区（系统合成之后）→ (vector 宽 高 RGBA 每 point 的像素数) 或 #f。
;;   hide-overlay?：先隐藏覆盖层（侧栏的字与高亮）→ 得到玻璃本身的像素；below?：含窗口下面的（透明窗口后面的东西）
(define-msg m-ca-begin "begin" _void ())
(define-msg m-ca-commit "commit" _void ())
(define-msg m-ca-disable-actions "setDisableActions:" _void ([b _BOOL]))
(define (window-capture win hide-overlay? below?)
  (define ov (window-overlay win))
  ;; 显隐不要隐式动画（缺省淡入淡出 0.25 s：截到一半淡出的字，背景就量错了）；之后等 WindowServer 合成一帧
  (define (set-hidden-now! on?)
    (define ca (cls "CATransaction"))
    (m-ca-begin ca) (m-ca-disable-actions ca #t) (m-set-hidden ov on?) (m-ca-commit ca) (m-ca-flush ca) (sleep 0.12))
  (when (and hide-overlay? ov) (set-hidden-now! #t))
  (define r (window-content-screen-rect win))
  (define-values (pw ph bs) (capture-window (m-window-number (window-ns win)) #:rect r #:below? below?))
  (when (and hide-overlay? ov) (set-hidden-now! #f))
  (and bs (vector pw ph bs (/ pw (vector-ref r 2)))))

(define (window-set-on-live-resize! win proc) (set-window-on-live-resize! win proc))

(define (window-set-content-size! win w h)
  (m-set-content-size (window-ns win) (make-CGSize w h))
) ; end define window-set-content-size!

(define (window-close! win)
  (m-set-delegate (window-ns win) #f)
  (m-close (window-ns win))
  (hash-remove! by-object (key-of (window-view win)))
  (hash-remove! by-object (key-of (window-delegate win)))
  (when (window-overlay win)
    (hash-remove! by-object (key-of (window-overlay win)))
    (hash-remove! overlay-views (key-of (window-overlay win)))
    (release! (window-overlay win)) (release! (window-glass win)))
  (release! (window-delegate win))
  (release! (window-view win))
  (release! (window-ns win))
) ; end define window-close!

(define-msg m-date-since-now "dateWithTimeIntervalSinceNow:" _id ([secs _double]))

;; 取完当前所有 NSEvent 并分发，返回本轮翻译出的 core 事件（按发生顺序）。
;; wait-ms > 0：第一次取事件时在 run loop 里最多等这么久（等待期间 CFRunLoop 在跑，
;; 输入法等经 mach port 回包的系统组件才能被服务）；0 = 不等待。
(define (pump-events! #:wait-ms [wait-ms 0])
  (call-with-autorelease-pool
   (lambda ()
     (let loop ([until (if (> wait-ms 0) (m-date-since-now (cls "NSDate") (/ wait-ms 1000.0)) distant-past)])
       (define e (m-next-event app EVENT-MASK-ANY until NSDefaultRunLoopMode #t))
       (when e
         (m-send-event app e)
         (loop distant-past)))))
  (begin0 (reverse queue) (set! queue '()))
) ; end define pump-events!

;; ---------------------------------------------------------------- 光标（侧栏分隔线上：左右拖动）

(define-msg m-cursor-set "set" _void ())
(define (set-cursor! kind)
  (call-with-autorelease-pool
   (lambda () (m-cursor-set (m-class-msg (cls "NSCursor") (sel (case kind [(resize-h) "resizeLeftRightCursor"] [else "arrowCursor"])))))))
(define-msg m-class-msg "performSelector:" _id ([s _pointer]))

;; ---------------------------------------------------------------- 合成事件（冒烟测试用）

;; 把合成 NSEvent 投进本应用自己的事件队列（postEvent:atStart:），走与真实输入相同的
;; pump → sendEvent: → 菜单 / view → 翻译 路径。不需要辅助功能权限（不经 CGEventPost）。
(define-msg m-window-number "windowNumber" _int64 ())
(define-msg m-mouse-event
  "mouseEventWithType:location:modifierFlags:timestamp:windowNumber:context:eventNumber:clickCount:pressure:" _id
  ([type _uint64] [loc _CGPoint] [flags _uint64] [ts _double] [wn _int64] [ctx _id] [num _int64]
   [clicks _int64] [pressure _float]))
(define-msg m-key-event
  "keyEventWithType:location:modifierFlags:timestamp:windowNumber:context:characters:charactersIgnoringModifiers:isARepeat:keyCode:" _id
  ([type _uint64] [loc _CGPoint] [flags _uint64] [ts _double] [wn _int64] [ctx _id]
   [chars _id] [chars-raw _id] [repeat _BOOL] [code _uint16]))
(define-msg m-post-event "postEvent:atStart:" _void ([e _id] [at-start _BOOL]))

(define MOUSE-TYPES (hasheq 'down 1 'up 2 'move 5 'drag 6))
(define (encode-mods mods) (mac-mod-flags mods))     ; 逻辑修饰键 → NSEventModifierFlags

;; 合成鼠标事件直接进本组件的事件队列（x y：内容区 point，左上原点）。
;; 不经 NSEvent：应用不在前台时，postEvent: 的鼠标事件坐标会被 AppKit 换算到别处（实测偏差约 (-46, -17) pt），
;; 冒烟结果随焦点而变。真实点击仍走 AppKit（mouseDown: → ev:mouse），该路径在应用前台时已验证。
(define (window-post-mouse! win kind x y #:mods [mods '()])
  (push! (ev:mouse kind x y 0 mods 1))
) ; end define window-post-mouse!

;; 真实路径的鼠标事件（测试用）：造 NSEvent 交给窗口的 sendEvent: → 命中测试 → 视图的 mouseDown: / rightMouseDown: …（含玻璃上面的覆盖视图）。
;; x y：内容区 point，左上原点。不经 CGEventPost（不需要辅助功能权限），不动真实光标
(define (window-post-mouse-ns! win kind x y #:button [button 0] #:mods [mods '()] #:clicks [clicks 1])
  (define type (case kind [(down) (if (= button 1) 3 1)] [(up) (if (= button 1) 4 2)] [(drag) (if (= button 1) 7 6)] [else 5]))
  (call-with-autorelease-pool
   (lambda ()
     (define ev (m-mouse-event (cls "NSEvent") type (make-CGPoint x (- (window-h win) y)) (encode-mods mods) 0.0
                               (m-window-number (window-ns win)) #f 0 clicks (if (memq kind '(down drag)) 1.0 0.0)))
     (m-send-event (window-ns win) ev)))
) ; end define window-post-mouse-ns!

(define-msg m-perform-selector "performSelector:withObject:" _id ([s _pointer] [o _id]))
;; 菜单里的编辑快捷键 → view 上的动作（应用不在前台时菜单的动作没有 key 窗口可去）
(define (edit-selector chars mods)
  (and (memq 'primary mods)
       (case (string-downcase chars)
         [("a") "selectAll:"] [("c") "copy:"] [("x") "cut:"] [("v") "paste:"]
         [("z") (if (memq 'shift mods) "redo:" "undo:")]
         [else #f])))

;; 窗口是 key 窗口：投进本应用队列（与真实按键同一路径）。不是（别的应用在前台）：投进队列的按键没有 key 窗口可去、被丢掉，
;; 冒烟结果随焦点而变 → 编辑快捷键（⌘A ⌘C ⌘X ⌘V ⌘Z）直接调 view 上的动作，其余交给本窗口 sendEvent:（→ view keyDown: → 输入法 / 命令）。不抢焦点。
(define (window-post-key! win chars code #:mods [mods '()])
  (call-with-autorelease-pool
   (lambda ()
     (call-with-cfstring chars
       (lambda (s)
         (define ev (m-key-event (cocoa-class "NSEvent") 10 (make-CGPoint 0.0 0.0) (encode-mods mods) 0.0
                                 (m-window-number (window-ns win)) #f s s #f code))
         (cond
           [(m-is-key-window (window-ns win)) (m-post-event app ev #f)]
           [(edit-selector chars mods) => (lambda (name) (m-perform-selector (window-view win) (sel name) #f))]
           [(and (memq 'primary mods) (equal? (string-downcase chars) "q")) (push! (ev:close))]       ; 菜单的「退出」（piQuit:）
           [else (m-send-event (window-ns win) ev)])))))
) ; end define window-post-key!

(define (cocoa-class name) (cls name))

;; 用真正的 CGEvent 构造按键（CGEventCreateKeyboardEvent，C 接口），包成 NSEvent 投进本应用队列。
;; 与 keyEventWithType: 的区别：事件背后有 CGEvent（时间戳、来源等字段齐全）。仍不经 CGEventPost，不需要辅助功能权限。
(define libcg-app (ffi-lib "/System/Library/Frameworks/CoreGraphics.framework/CoreGraphics"))
(define CGEventCreateKeyboardEvent
  (get-ffi-obj 'CGEventCreateKeyboardEvent libcg-app (_fun _pointer _uint16 _stdbool -> _pointer)))
(define CGEventSetFlags (get-ffi-obj 'CGEventSetFlags libcg-app (_fun _pointer _uint64 -> _void)))
(define-msg m-event-with-cgevent "eventWithCGEvent:" _id ([e _pointer]))
(define (window-post-key-cg! win code #:mods [mods '()])
  (call-with-autorelease-pool
   (lambda ()
     (for ([down? (in-list '(#t #f))])
       (define ce (CGEventCreateKeyboardEvent #f code down?))
       (unless (null? mods) (CGEventSetFlags ce (encode-mods mods)))
       (m-post-event app (m-event-with-cgevent (cls "NSEvent") ce) #f)
       (cf-release! ce))))
) ; end define window-post-key-cg!

;; ---------------------------------------------------------------- 剪贴板

;; ---------------------------------------------------------------- 附件（P4）：剪贴板 / 拖放里的文件与图片

(define ATTACH-MAX-BYTES (* 32 1024 1024))             ; 单个附件上限（与内核一条消息的媒体上限相同）
(define IMAGE-EXT '("png" "jpg" "jpeg" "gif" "heic" "heif" "webp" "tif" "tiff" "bmp"))
(define AUDIO-EXT '(("m4a" . "audio/mp4") ("mp3" . "audio/mpeg") ("wav" . "audio/wav") ("aif" . "audio/aiff") ("aiff" . "audio/aiff")
                    ("caf" . "audio/x-caf") ("flac" . "audio/flac") ("ogg" . "audio/ogg") ("aac" . "audio/aac")))

(define (ext-of name) (let ([m (regexp-match #rx"[.]([^./]+)$" name)]) (and m (string-downcase (cadr m)))))

;; 文件名 → (values kind 类型)
(define (media-type-of name)
  (define e (ext-of name))
  (cond [(member e IMAGE-EXT) (values 'image (case e [("jpg" "jpeg") "image/jpeg"] [("tif" "tiff") "image/tiff"] [("heif") "image/heic"] [else (string-append "image/" e)]))]
        [(assoc e AUDIO-EXT) => (lambda (pr) (values 'audio (cdr pr)))]
        [(member e '("txt" "md" "log" "csv")) (values 'file "text/plain")]
        [(equal? e "pdf") (values 'file "application/pdf")]
        [(equal? e "json") (values 'file "application/json")]
        [else (values 'file "application/octet-stream")]))

(define (percent-decode s)
  (bytes->string/utf-8 (regexp-replace* #rx#"%([0-9A-Fa-f][0-9A-Fa-f])" (string->bytes/utf-8 s)
                                        (lambda (_all hex) (bytes (string->number (bytes->string/utf-8 hex) 16))))
                       #\?))

;; 剪贴板 / 拖放板里的文件路径（public.file-url → 本地路径）
(define (pasteboard-file-paths pb)
  (call-with-autorelease-pool
   (lambda ()
     (define items (and pb (m-pasteboard-items pb)))
     (if (not items) '()
         (call-with-cfstring "public.file-url"
           (lambda (t)
             (for*/list ([i (in-range (m-ns-count items))]
                         [u (in-value (let ([s (m-string-for-type (m-object-at-index items i) t)]) (and s (cfstring->string s))))]
                         #:when (and u (regexp-match? #rx"^file://" u)))
               (percent-decode (regexp-replace #rx"^file://(localhost)?" u "")))))))))

(define (ns-data->bytes d)
  (define n (m-data-length d))
  (define out (make-bytes n))
  (memcpy out (m-data-bytes d) n)
  out)

;; → 附件表（ev:attach 的 items）：文件优先；没有文件但有 PNG 图片数据（截图到剪贴板）→ 一张图片
(define (pasteboard-attachments pb)
  (define files
    (for/list ([p (in-list (pasteboard-file-paths pb))]
               #:when (and (file-exists? p) (<= (file-size p) ATTACH-MAX-BYTES)))
      (define name (let-values ([(_b n _d) (split-path p)]) (path->string n)))
      (define-values (kind mt) (media-type-of name))
      (define bs (file->bytes p))
      (vector kind mt name bs (bytes-length bs) #f)))
  (cond
    [(pair? files) files]
    [else
     (call-with-autorelease-pool
      (lambda ()
        (call-with-cfstring "public.png"
          (lambda (t)
            (define d (and pb (m-data-for-type pb t)))
            (if (and d (> (m-data-length d) 0) (<= (m-data-length d) ATTACH-MAX-BYTES))
                (let ([bs (ns-data->bytes d)]) (list (vector 'image "image/png" "pasted-image.png" bs (bytes-length bs) #f)))
                '())))))]))

;; 测试可换成私有剪贴板（pasteboardWithUniqueName），不碰用户的系统剪贴板。
(define active-pasteboard #f)

(define (pasteboard) (or active-pasteboard (m-general-pasteboard (cls "NSPasteboard"))))

(define (clipboard-get-string)
  (call-with-autorelease-pool (lambda () (let ([s (m-string-for-type (pasteboard) NSPasteboardTypeString)]) (and s (cfstring->string s))))))

(define (clipboard-set-string! s)
  (call-with-autorelease-pool
   (lambda ()
     (define pb (pasteboard))
     (m-clear-contents pb)
     (call-with-cfstring s (lambda (cs) (m-set-string-for-type pb cs NSPasteboardTypeString)))))
) ; end define clipboard-set-string!

(define (call-with-private-pasteboard thunk)
  (define pb (retain! (m-unique-pasteboard (cls "NSPasteboard"))))
  (dynamic-wind
   (lambda () (set! active-pasteboard pb))
   thunk
   (lambda () (set! active-pasteboard #f) (m-release-globally pb) (release! pb)))
) ; end define call-with-private-pasteboard

;; ---------------------------------------------------------------- 文字输入状态

;; app 每帧调用：光标矩形（内容区 point，左上原点）与光标下标。组字中光标移动时通知输入法重新定位候选框。
(define (window-set-text-input-state! win caret-rect caret-index)
  (define changed? (not (equal? caret-rect (window-caret-rect win))))
  (set-window-caret-rect! win caret-rect)
  (set-window-caret-index! win caret-index)
  (when (and changed? (> (window-marked-len16 win) 0))
    (define ctx (m-input-context (window-view win)))
    (when ctx (m-invalidate-char-coords ctx)))
) ; end define window-set-text-input-state!

;; app 放弃组字（如点击别处）
(define (window-discard-marked-text! win)
  (set-window-marked-len16! win 0)
  (define ctx (m-input-context (window-view win)))
  (when ctx (m-discard-marked ctx))
) ; end define window-discard-marked-text!

;; 测试：直接按输入法的调用顺序驱动 NSTextInputClient 方法（不经输入法进程）。
;; steps：(list 'mark 串 选中UTF16下标) | (list 'insert 串) | (list 'unmark)
(define (window-simulate-ime! win steps)
  (define v (window-view win))
  (call-with-autorelease-pool
   (lambda ()
     (for ([st (in-list steps)])
       (case (car st)
         [(mark) (call-with-cfstring (cadr st)
                   (lambda (s) (m-set-markedtext v s (make-NSRange (caddr st) 0) (make-NSRange NSNotFound 0))))]
         [(insert) (call-with-cfstring (cadr st) (lambda (s) (m-insert-text v s (make-NSRange NSNotFound 0))))]
         [(unmark) ((msg-sender _void '()) v (sel "unmarkText"))]))))
) ; end define window-simulate-ime!

;; 测试：经 NSTextInputClient 查询候选框应出现的位置（屏幕坐标）与 hasMarkedText
;; 诊断：应用是否为前台活跃应用、输入上下文认为的当前输入源
(define-msg m-is-active "isActive" _BOOL ())
(define-msg m-is-key-window "isKeyWindow" _BOOL ())
(define-msg m-selected-input-source "selectedKeyboardInputSource" _id ())
(define-msg m-current-input-context "currentInputContext" _id ())
(define (ime-diagnostics win)
  (call-with-autorelease-pool
   (lambda ()
     (define ctx (m-input-context (window-view win)))
     (define cur (m-current-input-context (cls "NSTextInputContext")))
     (list (cons 'app-active (m-is-active app))
           (cons 'is-current (and ctx cur (ptr-equal? ctx cur)))
           (cons 'context (and ctx #t))
           (cons 'context-source (and ctx (let ([s (m-selected-input-source ctx)]) (and s (cfstring->string s)))))))))

(define (window-ime-query win)
  (define r (m-first-rect (window-view win) (make-NSRange 0 0) #f))
  (values (vector (CGPoint-x (CGRect-origin r)) (CGPoint-y (CGRect-origin r)) (rect-w r) (rect-h r))
          (m-has-marked (window-view win)))
) ; end define window-ime-query

;; ---------------------------------------------------------------- 组件

;; needs gpu-port（取 CAMetalLayer 挂到窗口上）→ provides window-port、clipboard、window-input。
;; config：'title 'width 'height
(define window-component
  (component 'window (list gpu-port) (list window-port clipboard window-input)
             (lambda (use config)
               (define glass? (and (hash-ref config 'glass? #f) (or (cls "NSGlassEffectView") (cls "NSVisualEffectView")) #t))
               (define win (window-open! (hash-ref config 'title "pi2") (hash-ref config 'width 960.0) (hash-ref config 'height 680.0)
                                         #:layer ((use gpu-port) 'surface)                  ; macOS：CAMetalLayer
                                         #:overlay (and glass? ((use gpu-port) 'overlay-surface))))
               (when (hash-ref config 'dark? #f) (window-set-dark! win))                ; 界面只有暗色：标题栏、玻璃也按暗色
               (list (implement window-port
                                'size (lambda () (window-size win))
                                'pump! (lambda () (pump-events!))
                                ;; 能力：照实报（没实现的就是 #f——中键 / 侧键没有 otherMouseDown:、捏合没有 magnifyWithEvent:）
                                'caps (lambda () (hasheq 'glass (and (window-glass win) #t) 'transparent-window #t 'ime 'marked-text 'capture 'self
                                                         'native-input-sim #t 'precise-scroll #t 'pinch #f 'mouse-buttons 2))
                                'apply! (lambda (reqs) (for ([r (in-list reqs)]) (apply-request! win r)))
                                'discard-marked! (lambda () (window-discard-marked-text! win))
                                'on-live-resize! (lambda (proc) (window-set-on-live-resize! win proc))
                                'close! (lambda () (window-close! win))
                                'capture (lambda (hide-overlay? below?) (window-capture win hide-overlay? below?)))
                     (implement clipboard
                                'get (lambda () (clipboard-get-string))
                                'set! (lambda (s) (clipboard-set-string! s))
                                'call-with-private (lambda (thunk) (call-with-private-pasteboard thunk)))
                     (implement window-input
                                'post-mouse! (lambda (kind x y [mods '()]) (window-post-mouse! win kind x y #:mods mods))
                                'post-mouse-ns! (lambda (kind x y [button 0] [mods '()] [clicks 1]) (window-post-mouse-ns! win kind x y #:button button #:mods mods #:clicks clicks))
                                'post-scroll! (lambda (x y dy [dx 0.0] [phase 'none]) (push! (ev:scroll dx dy #t phase x y)))   ; 合成滚轮 / 触控板手势：直接进队列（同 post-mouse!）
                                ;; 合成按键用逻辑键（keys.rkt）：冒烟脚本与平台无关，Windows 后端照同一套脚本跑
                                'post-key! (lambda (key [mods '()])
                                             (define-values (chars code _m) (mac-synthetic-key key mods))
                                             (window-post-key! win chars code #:mods mods))
                                'post-key-cg! (lambda (key [mods '()])
                                                (define-values (_c code _m) (mac-synthetic-key key mods))
                                                (window-post-key-cg! win code #:mods mods))
                                'post-chars! (lambda (s) (for ([c (in-string s)]) (window-post-key! win (string c) 0)))   ; 打字：逐字合成（键码不相干，进输入上下文）
                                'post-paste! (lambda () (push! (ev:paste (clipboard-get-string))))
                                'post-attach! (lambda (items) (push! (ev:attach items)))
                                'key? (lambda () (m-is-key-window (window-ns win)))
                                'simulate-ime! (lambda (steps) (window-simulate-ime! win steps))
                                'ime-query (lambda () (window-ime-query win))
                                'ime-diagnostics (lambda () (ime-diagnostics win)))))))

(provide window-component)
