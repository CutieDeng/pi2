#lang racket/base
;; contrast.rkt — 显示效果的自动检查（用户 2026-10-25：「界面的对比度、显示的亮度、各颜色之间是否有冲突、前后景颜色是否接近难以区分，
;; 都是测试需要自动验证的，不能依赖用户反馈」）。纯计算，不碰 GPU / 窗口。
;;
;; 颜色：sRGB 0..1；亮度与对比度按 WCAG 2.x（相对亮度 L = 0.2126 R + 0.7152 G + 0.0722 B，线性化后；对比度 (L1 + 0.05) / (L2 + 0.05)）。
;; 文字段：drawlist 里连续的字形实例（同色、同一行、相邻）合成一段；段的背景 = 按绘制顺序、在字形之前画的、盖住段中心的矩形依次合成
;;   （非预乘 alpha 的 over，与渲染器的混合一致：sRGB 值上做），最底下是清屏色或调用方给的背景（例如截图里玻璃的像素）。
;;   段之后画的不透明矩形盖住段中心 → 段看不见，不检查。背景里有图片（kind 3）→ 背景不确定，标出来不检查。

(require racket/list racket/math "drawlist.rkt")

;; ---------------------------------------------------------------- WCAG

(define (channel->linear c) (if (<= c 0.04045) (/ c 12.92) (expt (/ (+ c 0.055) 1.055) 2.4)))
(define (relative-luminance rgb)
  (+ (* 0.2126 (channel->linear (car rgb))) (* 0.7152 (channel->linear (cadr rgb))) (* 0.0722 (channel->linear (caddr rgb)))))
(define (contrast-ratio a b)
  (define la (relative-luminance a)) (define lb (relative-luminance b))
  (/ (+ (max la lb) 0.05) (+ (min la lb) 0.05)))
;; 非预乘的 over：src（r g b a）盖在 dst（r g b）上 → r g b
(define (over src dst)
  (define a (cadddr src))
  (list (+ (* a (car src)) (* (- 1 a) (car dst)))
        (+ (* a (cadr src)) (* (- 1 a) (cadr dst)))
        (+ (* a (caddr src)) (* (- 1 a) (caddr dst)))))

;; ---------------------------------------------------------------- drawlist → 实例表

;; (vector 下标 kind x y w h (r g b a) radius 裁剪矩形或#f)
(define (instances dl)
  (define segs (drawlist-segments dl))
  (for*/list ([seg (in-list segs)] [i (in-range (vector-ref seg 0) (+ (vector-ref seg 0) (vector-ref seg 1)))])
    (define f (drawlist-ref dl i))
    (vector i (inexact->exact (round (list-ref f 13))) (list-ref f 0) (list-ref f 1) (list-ref f 2) (list-ref f 3)
            (list (list-ref f 4) (list-ref f 5) (list-ref f 6) (list-ref f 7)) (list-ref f 12) (vector-ref seg 2))))

(define (in-clip? clip px py)
  (or (not clip) (and (<= (vector-ref clip 0) px (+ (vector-ref clip 0) (vector-ref clip 2)))
                      (<= (vector-ref clip 1) py (+ (vector-ref clip 1) (vector-ref clip 3))))))
(define (covers? inst px py)
  (define x (vector-ref inst 2)) (define y (vector-ref inst 3)) (define w (vector-ref inst 4)) (define h (vector-ref inst 5))
  (define r (min (vector-ref inst 7) (/ w 2) (/ h 2)))
  (define in-shape?                                                        ; 圆角矩形以内
    (or (<= r 0.0)
        (let ([dx (max 0.0 (- (abs (- px (+ x (/ w 2)))) (- (/ w 2) r)))]
              [dy (max 0.0 (- (abs (- py (+ y (/ h 2)))) (- (/ h 2) r)))])
          (<= (+ (* dx dx) (* dy dy)) (* r r)))))
  (and (<= x px (+ x w)) (<= y py (+ y h)) (in-clip? (vector-ref inst 8) px py)
       (if (= (vector-ref inst 1) KIND-HOLE) (not in-shape?) in-shape?)))   ; 挖洞：外框内、圆角外

;; ---------------------------------------------------------------- 文字段

;; → (listof (vector 首实例下标 末实例下标 x y w h (r g b a) color-glyph?))
(define (text-runs insts)
  (define glyphs (filter (lambda (v) (memv (vector-ref v 1) (list KIND-MASK-GLYPH KIND-COLOR-GLYPH))) insts))
  (define (same-run? run g)
    (define ry (vector-ref run 3)) (define rh (vector-ref run 5))
    (define gx (vector-ref g 2)) (define gy (vector-ref g 3)) (define gh (vector-ref g 5))
    (and (equal? (vector-ref run 6) (vector-ref g 6))
         (= (vector-ref run 7) (if (= (vector-ref g 1) KIND-COLOR-GLYPH) 1 0))
         (< (abs (- (+ ry (/ rh 2)) (+ gy (/ gh 2)))) (max 2.0 (* 0.35 (max rh gh))))      ; 同一行（中线接近）
         (<= (- (vector-ref run 2) 1.0) gx (+ (vector-ref run 2) (vector-ref run 4) (* 1.5 (max rh gh))))))   ; 紧挨着往右，或落在段里（组合符叠在底字上）
  (define (start g) (vector (vector-ref g 0) (vector-ref g 0) (vector-ref g 2) (vector-ref g 3) (vector-ref g 4) (vector-ref g 5)
                            (vector-ref g 6) (if (= (vector-ref g 1) KIND-COLOR-GLYPH) 1 0)))
  (define (extend run g)
    (define x0 (min (vector-ref run 2) (vector-ref g 2))) (define y0 (min (vector-ref run 3) (vector-ref g 3)))
    (define x1 (max (+ (vector-ref run 2) (vector-ref run 4)) (+ (vector-ref g 2) (vector-ref g 4))))
    (define y1 (max (+ (vector-ref run 3) (vector-ref run 5)) (+ (vector-ref g 3) (vector-ref g 5))))
    (vector (vector-ref run 0) (vector-ref g 0) x0 y0 (- x1 x0) (- y1 y0) (vector-ref run 6) (vector-ref run 7)))
  (define runs
    (for/fold ([acc '()]) ([g (in-list glyphs)])
      (if (and (pair? acc) (same-run? (car acc) g)) (cons (extend (car acc) g) (cdr acc)) (cons (start g) acc))))
  (for/list ([r (in-list (reverse runs))])
    (vector (vector-ref r 0) (vector-ref r 1) (vector-ref r 2) (vector-ref r 3) (vector-ref r 4) (vector-ref r 5) (vector-ref r 6) (= 1 (vector-ref r 7)))))

;; ---------------------------------------------------------------- 检查

;; 矩形与它的裁剪的交（看得见的部分）的中心 → (cons x y)；完全裁掉 → #f。
;; 取样点用它：滚出视口一半的卡片，中心在裁剪外，那里什么都没画（实际踩到过：把卡片的底比到了对话区的底上）
(define (visible-center x y w h clip)
  (define-values (x0 y0 x1 y1)
    (if clip
        (values (max x (vector-ref clip 0)) (max y (vector-ref clip 1))
                (min (+ x w) (+ (vector-ref clip 0) (vector-ref clip 2))) (min (+ y h) (+ (vector-ref clip 1) (vector-ref clip 3))))
        (values x y (+ x w) (+ y h))))
  (and (> x1 x0) (> y1 y0) (cons (/ (+ x0 x1) 2) (/ (+ y0 y1) 2))))

;; 一段文字的背景：取样点下、段之前画的矩形依次合成在 base 上。→ (values rgb 盖在图片上? 被后画的盖住?)
;; base：(px py) → rgb（清屏色或截图里的像素）
;; 「盖住」：后画的不透明（≥ 0.9）矩形 / 图片；或后画的大块（≥ 整帧一半）半透明（≥ 0.3）遮罩——模态页（设置页）把背后的窗口压暗，
;; 背后的字本来就不是给人读的（原来没算这种：设置页开着时侧栏的字被判成不合格）
(define overlay-area (make-parameter +inf.0))
(define (dims? v) (and (= (vector-ref v 1) KIND-RECT) (>= (cadddr (vector-ref v 6)) 0.3)
                       (>= (* (vector-ref v 4) (vector-ref v 5)) (* 0.5 (overlay-area)))))
(define (run-background insts run base pt)
  (define px (car pt))
  (define py (cdr pt))
  (define first-i (vector-ref run 0)) (define last-i (vector-ref run 1))
  (for/fold ([bg (base px py)] [img? #f] [hidden? #f]) ([v (in-list insts)])
    (define i (vector-ref v 0)) (define kind (vector-ref v 1))
    (cond
      [(not (covers? v px py)) (values bg img? hidden?)]
      [(< i first-i)
       (cond [(memv kind (list KIND-RECT KIND-HOLE)) (values (over (vector-ref v 6) bg) img? hidden?)]
             [(= kind KIND-IMAGE) (values bg #t hidden?)]
             [else (values bg img? hidden?)])]                                   ; 别的字形：不算背景
      [(and (> i last-i) (memv kind (list KIND-RECT KIND-IMAGE)) (>= (cadddr (vector-ref v 6)) 0.9)) (values bg img? #t)]
      [(and (> i last-i) (dims? v)) (values bg img? #t)]
      [else (values bg img? hidden?)])))

;; 整帧：→ (listof hasheq)：每段 'x 'y 'w 'h 'fg 'bg 'ratio 'status（'ok 'low 'image 'hidden 'emoji 'faint）
;;   min-ratio：要求的对比度（缺省 4.5，WCAG AA 正文）；alpha < faint-alpha 的字（淡出、装饰）不检查
(define (contrast-report dl #:clear [clear '(0.09 0.09 0.11)] #:base [base #f] #:min-ratio [min-ratio 4.5] #:faint-alpha [faint 0.6]
                         #:frame-area [area +inf.0])
  (parameterize ([overlay-area area]) (contrast-report* dl clear base min-ratio faint)))
(define (contrast-report* dl clear base min-ratio faint)
  (define insts (instances dl))
  (define by-index (for/hasheqv ([v (in-list insts)]) (values (vector-ref v 0) v)))
  (define base-fn (or base (lambda (_x _y) (take clear 3))))
  (for/list ([run (in-list (text-runs insts))]
             #:when (visible-center (vector-ref run 2) (vector-ref run 3) (vector-ref run 4) (vector-ref run 5)
                                    (vector-ref (hash-ref by-index (vector-ref run 0)) 8)))   ; 整段被裁掉：看不见，不判
    (define pt (visible-center (vector-ref run 2) (vector-ref run 3) (vector-ref run 4) (vector-ref run 5)
                               (vector-ref (hash-ref by-index (vector-ref run 0)) 8)))
    (define-values (bg img? hidden?) (run-background insts run base-fn pt))
    (define clip (vector-ref (hash-ref by-index (vector-ref run 0)) 8))
    (define vis (let ([x (vector-ref run 2)] [y (vector-ref run 3)] [w (vector-ref run 4)] [h (vector-ref run 5)])   ; 看得见的框（与裁剪相交）
                  (if clip
                      (let ([x0 (max x (vector-ref clip 0))] [y0 (max y (vector-ref clip 1))]
                            [x1 (min (+ x w) (+ (vector-ref clip 0) (vector-ref clip 2)))] [y1 (min (+ y h) (+ (vector-ref clip 1) (vector-ref clip 3)))])
                        (vector x0 y0 (max 0.0 (- x1 x0)) (max 0.0 (- y1 y0))))
                      (vector x y w h))))
    (define fgc (vector-ref run 6))
    (define fg (over fgc bg))                                                    ; 字的颜色带 alpha：合成到背景上看
    (define ratio (contrast-ratio fg bg))
    (hasheq 'x (vector-ref run 2) 'y (vector-ref run 3) 'w (vector-ref run 4) 'h (vector-ref run 5) 'vis vis
            'fg fgc 'bg bg 'ratio ratio
            'status (cond [hidden? 'hidden] [(vector-ref run 7) 'emoji] [img? 'image] [(< (cadddr fgc) faint) 'faint]
                          [(< ratio min-ratio) 'low] [else 'ok]))))

;; 底的函数：dl 里的矩形（不含字形）按顺序合成在 base 上（例如玻璃截图 + 侧栏铺的暗色与高亮）
(define (layered-base dl base)
  (define rects (filter (lambda (v) (memv (vector-ref v 1) (list KIND-RECT KIND-HOLE))) (instances dl)))
  (lambda (x y) (for/fold ([bg (base x y)]) ([v (in-list rects)] #:when (covers? v x y)) (over (vector-ref v 6) bg))))

;; 区域的亮度（截图 / 背景函数上取样，取中位数：字与小图标的像素不左右结果）：亮度是否和暗色主题一致
(define (region-luminance base x y w h #:step [step 6.0])
  (define ls (sort (for*/list ([yy (in-range y (+ y h) step)] [xx (in-range x (+ x w) step)]) (relative-luminance (base xx yy))) <))
  (if (null? ls) 0.0 (list-ref ls (quotient (length ls) 2))))

;; 非文字的填充（按钮、选中 / 悬停的行、芯片、卡片）与它下面的背景的对比度：前后景是否接近、难以区分。
;; 只看：宽高都 ≥ min-size、不透明度 ≥ 0.3、面积不到 frame-area × max-frac（整块的底色不算）、中心没被后画的不透明矩形盖住的矩形。
;; → (listof hasheq)：'x 'y 'w 'h 'color 'bg 'ratio 'status（'ok 'low 'hidden 'image）
(define (fill-report dl #:clear [clear '(0.09 0.09 0.11)] #:base [base #f] #:frame-area area #:min-ratio [min-ratio 1.2]
                     #:min-size [ms 14.0] #:max-frac [frac 0.2])
  (define insts (instances dl))
  (define base-fn (or base (lambda (_x _y) (take clear 3))))
  ;; 带边框的卡片：一个矩形紧跟着一个内缩 ≤ 3 point 的矩形（边框环 + 主体）。按一张卡片判：环或主体，有一个和下面的背景拉得开就行；
  ;; 内层不再单独判（它下面是自己的边框）
  (define vec (list->vector insts))
  (define (inner-of i)
    (and (< (add1 i) (vector-length vec))
         (let ([v (vector-ref vec i)] [u (vector-ref vec (add1 i))])
           (and (= (vector-ref u 1) KIND-RECT) (= (vector-ref u 0) (add1 (vector-ref v 0)))
                (let ([dl (- (vector-ref u 2) (vector-ref v 2))] [dt (- (vector-ref u 3) (vector-ref v 3))]
                      [dr (- (+ (vector-ref v 2) (vector-ref v 4)) (+ (vector-ref u 2) (vector-ref u 4)))]
                      [db (- (+ (vector-ref v 3) (vector-ref v 5)) (+ (vector-ref u 3) (vector-ref u 5)))])
                  (and (<= 0.0 dl 3.0) (<= 0.0 dt 3.0) (<= 0.0 dr 3.0) (<= 0.0 db 3.0) (> (+ dl dt dr db) 0.0)))
                u))))
  (define inner-indices (for/list ([i (in-range (vector-length vec))] #:when (inner-of i)) (vector-ref (vector-ref vec (add1 i)) 0)))
  (for/list ([v (in-list insts)] [i (in-naturals)]
             #:unless (memv (vector-ref v 0) inner-indices)
             #:when (and (= (vector-ref v 1) KIND-RECT) (>= (vector-ref v 4) ms) (>= (vector-ref v 5) ms)
                         (>= (cadddr (vector-ref v 6)) 0.3) (< (* (vector-ref v 4) (vector-ref v 5)) (* frac area))
                         (visible-center (vector-ref v 2) (vector-ref v 3) (vector-ref v 4) (vector-ref v 5) (vector-ref v 8))))   ; 整块被裁掉：不判
    (define pt (visible-center (vector-ref v 2) (vector-ref v 3) (vector-ref v 4) (vector-ref v 5) (vector-ref v 8)))
    (define px (car pt)) (define py (cdr pt))
    (define-values (bg img? hidden?)
      (for/fold ([bg (base-fn px py)] [img? #f] [hidden? #f]) ([u (in-list insts)] #:when (covers? u px py))
        (define i (vector-ref u 0)) (define kind (vector-ref u 1))
        (cond [(< i (vector-ref v 0))
               (cond [(memv kind (list KIND-RECT KIND-HOLE)) (values (over (vector-ref u 6) bg) img? hidden?)]
                     [(= kind KIND-IMAGE) (values bg #t hidden?)]
                     [else (values bg img? hidden?)])]
              [(and (> i (vector-ref v 0)) (memv kind (list KIND-RECT KIND-IMAGE)) (>= (cadddr (vector-ref u 6)) 0.9)
                    (>= (vector-ref u 4) (vector-ref v 4)) (>= (vector-ref u 5) (vector-ref v 5)))   ; 整个盖住（菜单、设置页）
               (values bg img? #t)]
              [(and (> i (vector-ref v 0)) (= kind KIND-RECT) (>= (cadddr (vector-ref u 6)) 0.3) (>= (* (vector-ref u 4) (vector-ref u 5)) (* 0.5 area)))
               (values bg img? #t)]                                            ; 模态遮罩压暗的背后
              [else (values bg img? hidden?)])))
    (define ring (over (vector-ref v 6) bg))
    (define inner (inner-of i))
    (define ratio (if inner
                      (max (contrast-ratio ring bg) (contrast-ratio (over (vector-ref inner 6) ring) bg))
                      (contrast-ratio ring bg)))
    (hasheq 'x (vector-ref v 2) 'y (vector-ref v 3) 'w (vector-ref v 4) 'h (vector-ref v 5) 'color (vector-ref v 6) 'bg bg 'ratio ratio
            'status (cond [hidden? 'hidden] [img? 'image] [(< ratio min-ratio) 'low] [else 'ok]))))

;; 字叠字：两段看得见的字（contrast-report 的结果里不是 hidden 的）框相交超过较小一段的 30% → 叠在一起了（布局缺陷：
;; 比如设置页的分节压到「关闭（Esc）」上）。→ (listof (cons 段a 段b))
(define (text-overlaps report)
  (define vis (list->vector (filter (lambda (r) (not (eq? (hash-ref r 'status) 'hidden))) report)))
  (define n (vector-length vis))
  (define (box r)                                           ; 看得见的框（滚到裁剪外的半行不算叠）
    (define v (hash-ref r 'vis (vector (hash-ref r 'x) (hash-ref r 'y) (hash-ref r 'w) (hash-ref r 'h))))
    (values (vector-ref v 0) (vector-ref v 1) (+ (vector-ref v 0) (vector-ref v 2)) (+ (vector-ref v 1) (vector-ref v 3))))
  (for*/list ([i (in-range n)] [j (in-range (add1 i) n)]
              #:when (let-values ([(ax0 ay0 ax1 ay1) (box (vector-ref vis i))] [(bx0 by0 bx1 by1) (box (vector-ref vis j))])
                       (define iw (- (min ax1 bx1) (max ax0 bx0)))
                       (define ih (- (min ay1 by1) (max ay0 by0)))
                       (and (> iw 0) (> ih 0)
                            (> (* iw ih) (* 0.3 (min (* (- ax1 ax0) (- ay1 ay0)) (* (- bx1 bx0) (- by1 by0))))))))
    (cons (vector-ref vis i) (vector-ref vis j))))

(define (round2 x) (/ (round (* 100 x)) 100.0))

(provide relative-luminance contrast-ratio over instances text-runs contrast-report fill-report region-luminance layered-base text-overlaps round2)
