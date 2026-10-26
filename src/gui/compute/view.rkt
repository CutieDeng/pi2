#lang racket/base
;; view.rkt — 视图平面（GUI 元数据平面 4）：元素树（数据）→ 约束布局 → 图元 → drawlist + 命中区 + 区域。
;;   组件描述「界面长什么样」（元素树，纯数据），不再一边算坐标一边画、一边登记命中区；坐标、裁剪、命中都由这里按布局推出。
;;   与平台无关：文字量度经 rich-ctx（文字系统组件给的闭包），输出是 drawlist（平台渲染器只认 drawlist 的五种图元，见 drawlist.rkt）。
;;
;; 元素：(el 种类 属性 子元素)，属性是 hasheq。构造：ui-box ui-text ui-scroll ui-spacer（ui- 前缀：不遮住 racket 的 box）。
;;   box    容器。'dir row | column | stack（叠放）；'w 'h 固定尺寸；'min-w 'max-w 'min-h 'max-h；'grow 主轴上分剩余空间的份数；
;;          'pad 内边距（数，或 (vector 上 右 下 左)）；'gap 子元素间距；'align 交叉轴 start | center | end | stretch（缺省 stretch）；
;;          'fill 底色 (r g b a)；'stroke 描边色（画成外扩 1 point 的底）；'radius 圆角；'hit (cons 种类 载荷)：这块是一个命中区；
;;          'id 符号：布局后这块的矩形记进区域表（滚轮路由、冒烟定位）；'overflow hide：放不下的子元素不画（主轴方向）
;;   text   'text 字符串；'font 字体键；'color；一行、放不下截断加 …（缺省）或 'wrap? #t 折行（构造参数 #:fit? #f）；'align start | center | end（行内对齐）
;;   scroll 纵向滚动的一栏：'id（必需）、'offset 偏移（调用方保存，这里夹到合法范围后报告回去）、'bar 滚动条颜色；子元素纵向排列，裁剪到视区
;;   spacer 'w / 'h / 'grow 的空白
;;   custom 自画的叶子（编辑器、Markdown 块、diff 行……这些本身就是图元）：'measure (宽 → 高)、'paint (dl x y w h scale → 命中区表)
;; row 的 'wrap #t：一行放不下就换行（子元素按自然宽，行间距 = 'gap）——一组按钮 / 标签
;; 通用：'dx 'dy 平移（横扫中的行）；stack 的子元素：宽高取自己的 'w 'h（没有就占满），按父元素 'align（横向）/ 'valign（纵向）放，
;;   'at (vector dx dy) 时放在父元素内容区的这个位置（锚在点上的菜单）；ui-text 的 'strike? 画删除线
;; 结果：render-view! → (values 命中区表 区域表 滚动表)
;;   命中区 (vector 种类 载荷 x y w h)（按绘制顺序，后画的在上；被滚动栏裁剪过）；区域 id → (vector x y w h)；
;;   滚动 id → (vector 夹好的偏移 内容高 视区高)

(require racket/list "drawlist.rkt" "paint.rkt" "rich.rkt" "layout.rkt")

(struct el (kind props kids) #:prefab)
(define (prop e k [d #f]) (hash-ref (el-props e) k d))

;; 构造（关键字参数直接成为属性；值为 #f 的不存——所以「关」的开关要存成反义的 #t，如 'wrap?）
(define (props->hash kvs) (for/hasheq ([kv (in-list kvs)] #:when (cdr kv)) (values (car kv) (cdr kv))))
(define (ui-box #:dir [dir 'column] #:w [w #f] #:h [h #f] #:min-w [min-w #f] #:max-w [max-w #f] #:min-h [min-h #f] #:max-h [max-h #f]
             #:grow [grow #f] #:pad [pad #f] #:gap [gap #f] #:align [align #f] #:fill [fill #f] #:stroke [stroke #f] #:radius [radius #f]
             #:hit [hit #f] #:id [id #f] #:overflow [overflow #f] #:valign [valign #f] #:at [at #f] #:dx [dx #f] #:dy [dy #f] #:wrap [wrap #f] . kids)
  (el 'box (props->hash (list (cons 'dir dir) (cons 'w w) (cons 'h h) (cons 'min-w min-w) (cons 'max-w max-w) (cons 'min-h min-h)
                              (cons 'max-h max-h) (cons 'grow grow) (cons 'pad pad) (cons 'gap gap) (cons 'align align) (cons 'fill fill)
                              (cons 'stroke stroke) (cons 'radius radius) (cons 'hit hit) (cons 'id id) (cons 'overflow overflow)
                              (cons 'valign valign) (cons 'at at) (cons 'dx dx) (cons 'dy dy) (cons 'wrap wrap)))
      (filter values kids)))
(define (ui-text str #:font [font 'body] #:color [color '(0.9 0.9 0.9 1.0)] #:fit? [fit? #t] #:align [align #f] #:w [w #f] #:grow [grow #f]
                 #:strike? [strike? #f])
  (el 'text (props->hash (list (cons 'text str) (cons 'font font) (cons 'color color) (cons 'wrap? (not fit?)) (cons 'align align)
                               (cons 'w w) (cons 'grow grow) (cons 'strike? strike?)))
      '()))
(define (ui-custom #:measure measure #:paint paint #:w [w #f] #:grow [grow #f] #:hit [hit #f])
  (el 'custom (props->hash (list (cons 'measure measure) (cons 'paint paint) (cons 'w w) (cons 'grow grow) (cons 'hit hit))) '()))
(define (ui-scroll id #:offset [offset 0.0] #:grow [grow 1] #:h [h #f] #:bar [bar #f] #:gap [gap #f] . kids)
  (el 'scroll (props->hash (list (cons 'id id) (cons 'offset offset) (cons 'grow grow) (cons 'h h) (cons 'bar bar) (cons 'gap gap)))
      (filter values kids)))
(define (ui-spacer #:w [w #f] #:h [h #f] #:grow [grow #f])
  (el 'spacer (props->hash (list (cons 'w w) (cons 'h h) (cons 'grow grow))) '()))

;; ---------------------------------------------------------------- 量度
;; 排文字时多给的宽：自然宽经过加减内边距后可能比原值小 1 ulp，按它排会把最后一个字挤到下一行（折行）或截成 …（一行）
(define WRAP-SLACK 0.01)
(define (pad4 e)
  (define p (prop e 'pad 0.0))
  (if (vector? p) p (vector p p p p)))
(define (clamp v lo hi) (max (or lo -inf.0) (min (or hi +inf.0) v)))

;; 一次 render-view! 里的量度缓存：元素（eq）→ ((可用宽 宽 高) …)。布局时父元素量过的子元素在放置时还要再量一次，
;; 嵌套越深重复越多；自画的叶子（Markdown、提交行）量一次就是一次完整排版。元素树每帧新建，缓存随这一次绘制丢弃。
(define current-measure-memo (make-parameter #f))

;; 自然尺寸：给定可用宽 → (values w h)
(define (measure lay e avail-w)
  (define memo (current-measure-memo))
  (define hit (and memo (assv avail-w (hash-ref memo e '()))))
  (cond
    [hit (values (cadr hit) (caddr hit))]
    [else
     (define-values (w h) (measure* lay e avail-w))
     (when memo (hash-set! memo e (cons (list avail-w w h) (hash-ref memo e '()))))
     (values w h)]))

(define (measure* lay e avail-w)
  (case (el-kind e)
    [(text)
     (define tb (lay (prop e 'text "") (prop e 'font 'body) (if (not (prop e 'wrap?)) 100000.0 (+ WRAP-SLACK (max 1.0 (or (prop e 'w) avail-w)))) #f))
     (values (or (prop e 'w) (min avail-w (text-block-width tb))) (text-block-height tb))]
    [(spacer) (values (or (prop e 'w) 0.0) (or (prop e 'h) 0.0))]
    [(custom) (values (or (prop e 'w) avail-w) ((prop e 'measure) (or (prop e 'w) avail-w)))]
    [(scroll)
     (define-values (cw ch) (stack-measure lay (el-kids e) avail-w 'column (prop e 'gap 0.0)))
     (values avail-w (or (prop e 'h) ch))]
    [(box)
     (define p (pad4 e))
     (define inner-w (- (or (prop e 'w) avail-w) (vector-ref p 1) (vector-ref p 3)))
     (define-values (cw ch) (if (prop e 'wrap)
                                (let ([lines (wrap-lines lay (el-kids e) inner-w (prop e 'gap 0.0))])
                                  (values inner-w (+ (for/sum ([ln lines]) (car ln)) (* (prop e 'gap 0.0) (max 0 (sub1 (length lines)))))))
                                (stack-measure lay (el-kids e) inner-w (prop e 'dir 'column) (prop e 'gap 0.0))))
     (values (clamp (or (prop e 'w) (+ cw (vector-ref p 1) (vector-ref p 3))) (prop e 'min-w) (prop e 'max-w))
             (clamp (or (prop e 'h) (+ ch (vector-ref p 0) (vector-ref p 2))) (prop e 'min-h) (prop e 'max-h)))]))

(define (stack-measure lay kids avail-w dir gap)
  (define sizes (for/list ([k (in-list kids)]) (call-with-values (lambda () (measure lay k avail-w)) cons)))
  (define n (length sizes))
  (case dir
    [(row)
     ;; 高：grow 的子元素按分到的宽量（会折行的文字放在按钮旁边时，按整行宽量会量矮）
     (define grows (for/list ([k (in-list kids)]) (or (prop k 'grow) 0)))
     (define total-grow (apply + grows))
     (define free (max 0.0 (- avail-w (for/sum ([s sizes] [g grows]) (if (> g 0) 0.0 (car s))) (* gap (max 0 (sub1 n))))))
     (values (+ (for/sum ([s sizes]) (car s)) (* gap (max 0 (sub1 n))))
             (for/fold ([m 0.0]) ([k (in-list kids)] [s (in-list sizes)] [g (in-list grows)])
               (max m (if (> g 0) (let-values ([(_w kh) (measure lay k (* free (/ g total-grow)))]) kh) (cdr s)))))]
    [(stack) (values (for/fold ([m 0.0]) ([s sizes]) (max m (car s))) (for/fold ([m 0.0]) ([s sizes]) (max m (cdr s))))]
    [else (values (for/fold ([m 0.0]) ([s sizes]) (max m (car s))) (+ (for/sum ([s sizes]) (cdr s)) (* gap (max 0 (sub1 n)))))]))

;; 换行排：→ (listof (cons 行高 (listof (vector 子元素 x偏移 宽 高))))
(define (wrap-lines lay kids avail-w gap)
  (define-values (lines cur cur-h x)
    (for/fold ([lines '()] [cur '()] [cur-h 0.0] [x 0.0]) ([k (in-list kids)])
      (define-values (kw kh) (measure lay k avail-w))
      (if (and (pair? cur) (> (+ x kw) avail-w))
          (values (cons (cons cur-h (reverse cur)) lines) (list (vector k 0.0 kw kh)) kh (+ kw gap))
          (values lines (cons (vector k x kw kh) cur) (max cur-h kh) (+ x kw gap)))))
  (reverse (if (pair? cur) (cons (cons cur-h (reverse cur)) lines) lines)))

;; ---------------------------------------------------------------- 布局 + 绘制
;; 一次走完：给每个元素定矩形，同时画、登记命中区 / 区域 / 滚动。clip：当前裁剪矩形或 #f（命中区按它裁）
(define (render-view! rc dl root x y w h scale #:clip [clip0 #f])   ; clip0：调用方已设的裁剪（结束时恢复成它）
  (define lay (rich-ctx-layout rc))
  (define atlas (rich-ctx-atlas rc))
  (define hits '()) (define regions (make-hasheq)) (define scrolls (make-hasheq))
  (define (clip-rect r clip)
    (cond [(not clip) r]
          [else (define x0 (max (vector-ref r 0) (vector-ref clip 0))) (define y0 (max (vector-ref r 1) (vector-ref clip 1)))
                (define x1 (min (+ (vector-ref r 0) (vector-ref r 2)) (+ (vector-ref clip 0) (vector-ref clip 2))))
                (define y1 (min (+ (vector-ref r 1) (vector-ref r 3)) (+ (vector-ref clip 1) (vector-ref clip 3))))
                (and (> x1 x0) (> y1 y0) (vector x0 y0 (- x1 x0) (- y1 y0)))]))
  (define (hit! e ex ey ew eh clip)
    (define hv (prop e 'hit))
    (when hv
      (define r (clip-rect (vector ex ey ew eh) clip))
      (when r (set! hits (cons (vector (car hv) (cdr hv) (vector-ref r 0) (vector-ref r 1) (vector-ref r 2) (vector-ref r 3)) hits)))))
  (define (rect! color rx ry rw rh radius)
    (drawlist-rect! dl rx ry rw rh (list-ref color 0) (list-ref color 1) (list-ref color 2) (if (> (length color) 3) (list-ref color 3) 1.0)
                    #:radius (or radius 0.0)))
  (define (place! e ex0 ey0 ew eh clip)
    (define ex (+ ex0 (prop e 'dx 0.0)))
    (define ey (+ ey0 (prop e 'dy 0.0)))
    (case (el-kind e)
      [(text)
       (define tb (if (not (prop e 'wrap?))
                      (fit-line lay (prop e 'text "") (prop e 'font 'body) (+ WRAP-SLACK (max 1.0 ew)))
                      (lay (prop e 'text "") (prop e 'font 'body) (+ WRAP-SLACK (max 1.0 ew)) #f)))
       (define tx (case (prop e 'align 'start)
                    [(center) (+ ex (/ (- ew (text-block-width tb)) 2))]
                    [(end) (- (+ ex ew) (text-block-width tb))]
                    [else ex]))
       (define ty (+ ey (/ (- eh (text-block-height tb)) 2)))
       (paint-text! dl atlas tb tx ty (prop e 'color) #:scale scale)
       (when (prop e 'strike?)
         (define c (prop e 'color))
         (rect! (list (list-ref c 0) (list-ref c 1) (list-ref c 2) (if (> (length c) 3) (list-ref c 3) 1.0))
                tx (+ ty (/ (text-block-line-height tb) 2)) (text-block-width tb) 1.0 #f))]
      [(spacer) (void)]
      [(custom)
       (hit! e ex ey ew eh clip)
       (for ([h (in-list ((prop e 'paint) dl ex ey ew eh scale))])
         (define r (clip-rect (vector (vector-ref h 2) (vector-ref h 3) (vector-ref h 4) (vector-ref h 5)) clip))
         (when r (set! hits (cons (vector (vector-ref h 0) (vector-ref h 1) (vector-ref r 0) (vector-ref r 1) (vector-ref r 2) (vector-ref r 3)) hits))))]
      [(scroll)
       (define id (prop e 'id))
       (define gap (prop e 'gap 0.0))
       (define sizes (for/list ([k (in-list (el-kids e))]) (let-values ([(kw kh) (measure lay k ew)]) kh)))
       (define content-h (+ (for/sum ([s sizes]) s) (* gap (max 0 (sub1 (length sizes))))))
       (define smax (max 0.0 (- content-h eh)))
       (define off (max 0.0 (min smax (prop e 'offset 0.0))))
       (hash-set! scrolls id (vector off content-h eh))
       (hash-set! regions id (vector ex ey ew eh))
       (define vclip (clip-rect (vector ex ey ew eh) clip))
       (drawlist-set-clip! dl vclip)
       (for/fold ([ky (- ey off)]) ([k (in-list (el-kids e))] [kh (in-list sizes)])
         (when (and vclip (< ky (+ ey eh)) (> (+ ky kh) ey)) (place! k ex ky ew kh vclip))
         (+ ky kh gap))
       (drawlist-set-clip! dl clip)
       (when (and (> smax 0.0) (prop e 'bar))
         (define th (max 20.0 (* eh (/ eh content-h))))
         (rect! (prop e 'bar) (+ ex ew -4.0) (+ ey (* (- eh th) (/ off smax))) 3.0 th 1.5))]
      [(box)
       (when (prop e 'stroke) (rect! (prop e 'stroke) (- ex 1.0) (- ey 1.0) (+ ew 2.0) (+ eh 2.0) (and (prop e 'radius) (+ 1.0 (prop e 'radius)))))
       (when (prop e 'fill) (rect! (prop e 'fill) ex ey ew eh (prop e 'radius)))
       (hit! e ex ey ew eh clip)
       (when (prop e 'id) (hash-set! regions (prop e 'id) (vector ex ey ew eh)))
       (define p (pad4 e))
       (define ix (+ ex (vector-ref p 3))) (define iy (+ ey (vector-ref p 0)))
       (define iw (max 0.0 (- ew (vector-ref p 1) (vector-ref p 3)))) (define ih (max 0.0 (- eh (vector-ref p 0) (vector-ref p 2))))
       (arrange-kids! e (el-kids e) ix iy iw ih clip)]))
  ;; 子元素在主轴上排：固定 / 自然尺寸 + 剩余空间按 grow 分；交叉轴 stretch（缺省）或按 align
  (define (arrange-kids! e kids ix iy iw ih clip)
    (define dir (prop e 'dir 'column))
    (define gap (prop e 'gap 0.0))
    (define align (prop e 'align 'stretch))
    (define nat (for/list ([k (in-list kids)]) (call-with-values (lambda () (measure lay k (if (eq? dir 'row) iw iw))) cons)))
    (case (if (and (eq? dir 'row) (prop e 'wrap)) 'wrap dir)
      [(wrap)
       (for/fold ([ly iy]) ([ln (in-list (wrap-lines lay kids iw gap))])
         (for ([c (in-list (cdr ln))]) (place! (vector-ref c 0) (+ ix (vector-ref c 1)) ly (vector-ref c 2) (car ln) clip))
         (+ ly (car ln) gap))]
      [(stack)
       (for ([k (in-list kids)])
         (define at (prop k 'at))
         (define kw (or (prop k 'w)
                        (if at (let-values ([(mw mh) (measure lay k iw)]) mw) (clamp iw (prop k 'min-w) (prop k 'max-w)))))   ; 锚在点上、没给宽 → 自然宽
         (define kh (or (prop k 'h) (if at (let-values ([(mw mh) (measure lay k kw)]) mh) (clamp ih (prop k 'min-h) (prop k 'max-h)))))
         (define kx (cond [at (+ ix (vector-ref at 0))]
                          [else (+ ix (case align [(center) (/ (- iw kw) 2)] [(end) (- iw kw)] [else 0.0]))]))
         (define ky (cond [at (+ iy (vector-ref at 1))]
                          [else (+ iy (case (prop e 'valign 'start) [(center) (/ (- ih kh) 2)] [(end) (- ih kh)] [else 0.0]))]))
         (place! k kx ky kw kh clip))]
      [else
       (define main (if (eq? dir 'row) iw ih))
       (define fixed (for/list ([k (in-list kids)] [s (in-list nat)]) (if (prop k 'grow) 0.0 (if (eq? dir 'row) (car s) (cdr s)))))
       (define grows (for/list ([k (in-list kids)]) (or (prop k 'grow) 0)))
       (define total-grow (apply + grows))
       (define free (max 0.0 (- main (apply + fixed) (* gap (max 0 (sub1 (length kids)))))))
       (for/fold ([pos (if (eq? dir 'row) ix iy)]) ([k (in-list kids)] [s (in-list nat)] [fx (in-list fixed)] [g (in-list grows)])
         (define len (if (> g 0) (* free (/ g total-grow)) fx))
         (define over? (and (eq? (prop e 'overflow) 'hide) (> (+ pos len) (+ (if (eq? dir 'row) ix iy) main 0.5))))
         (unless over?
           (define cross-nat (cond [(not (eq? dir 'row)) (car s)]
                                   [(> g 0) (let-values ([(_w kh) (measure lay k len)]) kh)]   ; 按分到的宽量高
                                   [else (cdr s)]))
           (define cross (if (eq? dir 'row) ih iw))
           (define-values (co cl) (case align
                                    [(start) (values 0.0 cross-nat)]
                                    [(center) (values (/ (- cross cross-nat) 2) cross-nat)]
                                    [(end) (values (- cross cross-nat) cross-nat)]
                                    [else (values 0.0 cross)]))
           (if (eq? dir 'row)
               (place! k pos (+ iy co) len cl clip)
               (place! k (+ ix co) pos cl len clip)))
         (+ pos len gap))]))
  (parameterize ([current-measure-memo (make-hasheq)]) (place! root x y w h clip0))
  (drawlist-set-clip! dl clip0)
  (values (reverse hits) regions scrolls))

;; 命中：最上面（最后画）的那一块
(define (view-hit hits x y)
  (for/last ([h (in-list hits)]
             #:when (and (<= (vector-ref h 2) x (+ (vector-ref h 2) (vector-ref h 4)))
                         (<= (vector-ref h 3) y (+ (vector-ref h 3) (vector-ref h 5)))))
    h))
(define (in-region? regions id x y)
  (define r (hash-ref regions id #f))
  (and r (<= (vector-ref r 0) x (+ (vector-ref r 0) (vector-ref r 2))) (<= (vector-ref r 1) y (+ (vector-ref r 1) (vector-ref r 3)))))

(provide (struct-out el) prop ui-box ui-text ui-scroll ui-spacer ui-custom measure render-view! view-hit in-region?)
