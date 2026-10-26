#lang racket/base
;; text-system.rkt — 文字系统组件：文字后端 + glyph atlas + shaping 缓存 + 字体表。
;; 与平台无关：后端由拼装层注入（macOS 为 compute/mac-coretext.rkt）。拼装层把它包成 rich-ctx
;; （计算层定义的闭包契约）交给输入框、对话区。

(require "../compute/text-backend.rkt" "../compute/atlas.rkt" "../compute/layout.rkt" "../compute/drawlist.rkt"
         "../compute/rich.rkt" "../compute/hub.rkt" "../compute/interfaces.rkt" "../compute/i18n.rkt" "../compute/tokens.rkt")

;; fonts：键 → (cons 字体角色 字号)——字号表是设计参数（compute/tokens.rkt TYPE-SCALE，各平台共用）；角色 → 实际字体由平台的文字后端决定
(define DEFAULT-FONTS TYPE-SCALE)

(struct text-system (backend atlas cache fonts))

(define (make-text-system backend #:fonts [specs DEFAULT-FONTS])
  (define font (text-backend-font backend))
  (text-system backend (make-atlas backend) (make-hash)
               (for/hasheq ([(k v) (in-hash specs)]) (values k (font (car v) (cdr v)))))
) ; end define make-text-system

(define (text-system-font t k) (hash-ref (text-system-fonts t) k))

;; shaping 与宽度无关，按 (串 . 字体) 缓存（流式输出会产生大量一次性的串：超过上限整表清空）；折行每次重算
(define SHAPE-CACHE-MAX 20000)
(define (cached-shape t)
  (define be (text-system-backend t))
  (define cache (text-system-cache t))
  (lambda (s f)
    (when (> (hash-count cache) SHAPE-CACHE-MAX) (hash-clear! cache))
    (hash-ref! cache (cons s f) (lambda () ((text-backend-shape be) s f))))
) ; end define cached-shape

;; Unicode 显示策略（design-gui.md §30）：所有界面文字都经它；标记文字用小字排（不再套策略）
(define (mark-label-layout ts)                         ; ts：文字系统（t 是界面文字的翻译函数）
  (lambda (kind label)
    (define shown (if (eq? kind 'tag) (t 'uni.hidden (if (> (string-length label) 60) (string-append (substring label 0 60) "…") label)) label))
    (layout-text (text-system-backend ts) shown (text-system-font ts 'small) 100000.0 #:shape (cached-shape ts))))

(define (text-system-layout t str font-key width [box-width #f])
  (layout-text (text-system-backend t) str (text-system-font t font-key) width #:box-width box-width
               #:shape (cached-shape t) #:unicode (mark-label-layout t))
) ; end define text-system-layout

(define (text-system-layout-runs t runs base-key width)
  (layout-runs (text-system-backend t)
               (for/list ([r (in-list runs)]) (cons (car r) (text-system-font t (cdr r))))
               width #:base-font (text-system-font t base-key) #:shape (cached-shape t) #:unicode (mark-label-layout t))
) ; end define text-system-layout-runs

;; 交给其它组件的契约（工具层 rich-ctx：排版闭包 + atlas）
(define (text-system-rich t)
  (rich-ctx (lambda (s k w bw) (text-system-layout t s k w bw)) (text-system-atlas t)
            (lambda (runs base w) (text-system-layout-runs t runs base w)))
) ; end define text-system-rich

;; 构建一帧：build 往 dl 里写实例；atlas 装满则整页 reset 后重建一次。返回实例数。
(define (text-system-build-frame! t dl build)
  (let retry ([attempt 0])
    (drawlist-clear! dl)
    (build)
    (when (and (atlas-full? (text-system-atlas t)) (zero? attempt))
      (atlas-reset! (text-system-atlas t))
      (retry 1)))
  (drawlist-count dl)
) ; end define text-system-build-frame!

;; 取走 atlas 脏区：→ #f | (cons atlas位图 (vector x y w h))，由拼装层交给渲染组件上传
(define (text-system-take-dirty! t)
  (define d (atlas-take-dirty! (text-system-atlas t)))
  (and d (cons (atlas-buf (text-system-atlas t)) d))
) ; end define text-system-take-dirty!

(define (text-system-glyph-count t) (atlas-glyph-count (text-system-atlas t)))

;; ---------------------------------------------------------------- 组件

;; needs font-backend → provides text-layout。config：'fonts（可选，键 → (cons 族 字号)）
(define text-system-component
  (component 'text-system (list font-backend) (list text-layout)
             (lambda (use config)
               (define fb (use font-backend))
               (define t (make-text-system (text-backend (lambda (f s) (fb 'font f s)) (lambda (s f) (fb 'shape s f))
                                                         (lambda (f) (fb 'metrics f)) (lambda (f g sc sp) (fb 'rasterize f g sc sp)))
                                           #:fonts (hash-ref config 'fonts DEFAULT-FONTS)))
               (list (implement text-layout
                                'layout (lambda (s k w [bw #f]) (text-system-layout t s k w bw))
                                'rich (lambda () (text-system-rich t))
                                'atlas (lambda () (text-system-atlas t))
                                'build-frame! (lambda (dl thunk) (text-system-build-frame! t dl thunk))
                                'take-dirty! (lambda () (text-system-take-dirty! t))
                                'glyph-count (lambda () (text-system-glyph-count t)))))))

(provide text-system-component)
