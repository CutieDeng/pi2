#lang racket/base
;; input.rkt — 输入框视图（L4）：编辑器模型 → 绘制（文字、chip、选区、组字下划线、光标、占位提示）
;; + 折叠预览面板 + 命中测试 + 上下移动。底边锚定，高度随内容在 1..MAX-LINES 行之间伸缩，超出按整行滚动
;; （整行滚动 → 只画可见行即可，不需要像素级裁剪）。

(require racket/list racket/string "../compute/drawlist.rkt" "../compute/palette.rkt" "../compute/layout.rkt" "../compute/linebreak.rkt"
         "../compute/fold.rkt" "../compute/editor.rkt" "../compute/rich.rkt" "../compute/hub.rkt" "../compute/interfaces.rkt"
         "../compute/i18n.rkt")

(define MAX-LINES 8)
(define PAD 12.0)
(define PEEK-LINES 14)
(define PEEK-CHARS 6000)                       ; 预览只排前这么多字符（大段内容不全量排版）
;; 占位提示：按当前语言取（不能在模块加载时求值）
(define (placeholder) (t 'input.placeholder))


;; 最近一次绘制的几何信息（命中测试 / IME 光标矩形用）
(struct geom (box-x box-y box-w box-h text-x text-y tb first-line visible display caret-rect chip-rects peek-buttons)
  #:transparent)

;; pin：滚轮手动滚过时记下 (cons 光标 正文)，两者都没变就保持手动的滚动位置；acc：滚轮像素余量（按整行滚）
(struct input-view (editor rich [scroll #:mutable] [geom #:mutable] [pin #:mutable] [acc #:mutable]))

(define (make-input-view editor rc) (input-view editor rc 0 #f #f 0.0))

(define (editor-folds-in-order e)
  (for/list ([id (in-list (editor-chips e))]) (editor-fold e id))
) ; end define editor-folds-in-order

;; ---------------------------------------------------------------- 预览面板

;; 返回 (values 面板高度 按钮列表)；按钮：(vector 'unfold|'remove|'confirm fold-id x y w h)（'confirm：待选密钥「作为密钥」）
(define (paint-peek! dl rc f x bottom w scale)
  (define lay (rich-ctx-layout rc))
  (define inner (- w (* 2 PAD)))
  (define header (lay (fold-summary f) 'small inner #f))
  (define secret? (secret-fold? f))
  ;; 密钥：不显示原值，只说明它去了哪里
  (define body-src (cond [secret? (t 'input.secret-hidden (fold-preview f))]
                         [else (let ([c (fold-content f)]) (if (> (string-length c) PEEK-CHARS) (substring c 0 PEEK-CHARS) c))]))
  (define body (lay body-src (if secret? 'small 'mono) inner #f))
  (define shown (if secret? (length (text-block-lines body)) (min PEEK-LINES (length (text-block-lines body)))))
  (define more (if secret? 0
                   (- (fold-lines f) (for/sum ([l (in-list (take (text-block-lines body) shown))]
                                               #:when (or (= (line-end l) (string-length body-src))
                                                          (char=? (string-ref body-src (line-end l)) #\newline)))
                                       1))))
  (define footer (and (> more 0) (lay (t 'input.peek-more more (fold-lines f)) 'small inner #f)))
  (define hh (+ (text-block-height header) 8.0))
  (define bh (* shown (text-block-line-height body)))
  (define fh (if footer (+ 6.0 (text-block-height footer)) 0.0))
  (define h (+ PAD hh bh fh PAD))
  (define y (- bottom h))
  (drawlist-rect! dl x y w h 0.24 0.25 0.30 1.0 #:radius 10.0)
  (drawlist-rect! dl (+ x 1) (+ y 1) (- w 2) (- h 2) 0.11 0.11 0.13 1.0 #:radius 9.0)
  (rich-paint! dl rc header '#() '() (+ x PAD) (+ y PAD) DIM scale)
  ;; 右上角两个按钮
  (define buttons
    (for/fold ([bx (+ x w (- PAD))] [acc '()] #:result acc)
              ([b (in-list (append (list (list 'remove (t 'input.remove)) (list 'unfold (t 'input.to-text)))
                                   (if (eq? (fold-kind f) 'secret-pending) (list (list 'confirm (t 'input.as-secret))) '())))])
      (define ltb (lay (cadr b) 'small 1000.0 #f))
      (define bw (+ (text-block-width ltb) 16.0))
      (define bh* (+ (text-block-line-height ltb) 4.0))
      (define x0 (- bx bw))
      (define y0 (+ y PAD -2.0))
      (drawlist-rect! dl x0 y0 bw bh* 0.22 0.23 0.28 1.0 #:radius 5.0)
      (rich-paint! dl rc ltb '#() '() (+ x0 8.0) (+ y0 2.0) TEXT scale)
      (values (- x0 8.0) (cons (vector (car b) (fold-id f) x0 y0 bw bh*) acc))))
  (rich-paint! dl rc body '#() '() (+ x PAD) (+ y PAD hh) '(0.80 0.82 0.85 1.0) scale #:line-count shown)
  (when footer (rich-paint! dl rc footer '#() '() (+ x PAD) (+ y PAD hh bh 6.0) DIM scale))
  (values h buttons)
) ; end define paint-peek!

;; ---------------------------------------------------------------- 输入框

;; 底边锚定在 bottom。返回输入框（含预览面板）顶边 y。
(define (input-render! iv dl x bottom w scale #:focused? [focused? #t] #:caret-on? [caret-on? #t])
  (define e (input-view-editor iv))
  (define rc (input-view-rich iv))
  (define-values (display caret marked) (editor-display e))
  (define folds (editor-folds-in-order e))
  (define text-w (- w (* 2 PAD)))
  (define empty? (string=? display ""))
  (define-values (tb labels)
    (rich-layout rc (if empty? (placeholder) display) (if empty? '() folds) text-w 'body
                 #:expanded (editor-expanded e)))
  (define lh (text-block-line-height tb))
  (define n (length (text-block-lines tb)))
  (define visible (min n MAX-LINES))
  (define-values (caret-line caret-x) (text-block-caret tb (if empty? 0 caret)))
  ;; 整行滚动，保证光标行可见；滚轮手动滚过且光标 / 正文都没动 → 保持手动位置
  (define pinned? (equal? (input-view-pin iv) (cons caret display)))
  (unless pinned? (set-input-view-pin! iv #f))
  (define scroll
    (let ([s (input-view-scroll iv)])
      (cond [pinned? (max 0 (min s (- n visible)))]
            [(< caret-line s) caret-line]
            [(>= caret-line (+ s visible)) (- caret-line visible -1)]
            [else (max 0 (min s (- n visible)))])))
  (set-input-view-scroll! iv scroll)
  (define box-h (+ (* visible lh) (* 2 PAD)))
  (define box-y (- bottom box-h))
  (define tx (+ x PAD))
  (define ty (+ box-y PAD))
  ;; 框：描边 + 底
  (define-values (br bg bb) (if focused? (values 0.33 0.45 0.70) (values 0.24 0.25 0.30)))
  (drawlist-rect! dl x box-y w box-h br bg bb 1.0 #:radius 12.0)
  (drawlist-rect! dl (+ x 1) (+ box-y 1) (- w 2) (- box-h 2) 0.13 0.13 0.16 1.0 #:radius 11.0)
  (define (line-y k) (+ ty (* (- k scroll) lh)))
  (define (visible-line? k) (and (>= k scroll) (< k (+ scroll visible))))
  ;; 选区
  (unless (or empty? marked)
    (define-values (a b) (editor-selection e))
    (when (< a b)
      (define-values (ka xa) (text-block-caret tb a))
      (define-values (kb xb) (text-block-caret tb b))
      (for ([k (in-range ka (add1 kb))] #:when (visible-line? k))
        (define ln (list-ref (text-block-lines tb) k))
        (define x0 (if (= k ka) xa 0.0))
        (define x1 (if (= k kb) xb (+ (line-end-x ln) 6.0)))
        (drawlist-rect! dl (+ tx x0) (line-y k) (max 2.0 (- x1 x0)) lh 0.22 0.36 0.62 0.85))))
  ;; 文字与 chip
  (define chip-rects
    (rich-paint! dl rc tb labels (if empty? '() folds) tx ty (if empty? DIM TEXT) scale
                 #:first-line scroll #:line-count visible #:expanded (editor-expanded e)))
  ;; 组字下划线
  (when marked
    (define-values (ka xa) (text-block-caret tb (car marked)))
    (define-values (kb xb) (text-block-caret tb (cdr marked)))
    (for ([k (in-range ka (add1 kb))] #:when (visible-line? k))
      (define ln (list-ref (text-block-lines tb) k))
      (define x0 (if (= k ka) xa 0.0))
      (define x1 (if (= k kb) xb (line-end-x ln)))
      (drawlist-rect! dl (+ tx x0) (+ (line-y k) (text-block-baseline tb) 3.0) (max 1.0 (- x1 x0)) 1.5
                      0.85 0.86 0.90 1.0)))
  ;; 光标
  (define caret-rect (vector (+ tx caret-x) (+ (line-y caret-line) 2.0) 2.0 (- lh 4.0)))
  (when (and focused? caret-on? (visible-line? caret-line))
    (drawlist-rect! dl (vector-ref caret-rect 0) (vector-ref caret-rect 1) 1.5 (vector-ref caret-rect 3)
                    0.55 0.70 1.0 1.0))
  ;; 预览面板（在框上方）
  (define-values (peek-h buttons)
    (let ([id (editor-expanded e)])
      (if (and id (memv id (editor-chips e)))
          (paint-peek! dl rc (editor-fold e id) x (- box-y 8.0) w scale)
          (values 0.0 '()))))
  (set-input-view-geom! iv (geom x box-y w box-h tx ty tb scroll visible (if empty? "" display)
                                 caret-rect chip-rects buttons))
  (- box-y (if (> peek-h 0) (+ peek-h 8.0) 0.0))
) ; end define input-render!

;; ---------------------------------------------------------------- 命中测试 / 移动

(define (in-rect? x y rx ry rw rh) (and (<= rx x (+ rx rw)) (<= ry y (+ ry rh))))

;; → (list 'button 'unfold|'remove id) | (list 'chip id) | (list 'text idx) | #f
(define (input-hit iv x y)
  (define g (input-view-geom iv))
  (cond
    [(not g) #f]
    [(for/first ([b (in-list (geom-peek-buttons g))] #:when (in-rect? x y (vector-ref b 2) (vector-ref b 3) (vector-ref b 4) (vector-ref b 5))) b)
     => (lambda (b) (list 'button (vector-ref b 0) (vector-ref b 1)))]
    [(for/first ([c (in-list (geom-chip-rects g))] #:when (in-rect? x y (vector-ref c 1) (vector-ref c 2) (vector-ref c 3) (vector-ref c 4))) c)
     => (lambda (c) (list 'chip (vector-ref c 0)))]
    ;; 空输入框：画的是占位提示，tb 不是正文的排版 → 只能落在位置 0（曾因此越界崩溃）
    [(and (in-rect? x y (geom-box-x g) (geom-box-y g) (geom-box-w g) (geom-box-h g)) (string=? (geom-display g) ""))
     (list 'text 0)]
    [(in-rect? x y (geom-box-x g) (geom-box-y g) (geom-box-w g) (geom-box-h g))
     (define tb (geom-tb g))
     (define starts (grapheme-starts (geom-display g)))
     (define n (string-length (geom-display g)))
     (list 'text (min n (text-block-index-at tb (- x (geom-text-x g))
                                             (+ (- y (geom-text-y g)) (* (geom-first-line g) (text-block-line-height tb)))
                                             #:ok? (lambda (i) (and (< i (vector-length starts)) (vector-ref starts i))))))]
    [else #f])
) ; end define input-hit

;; 滚轮：指针在输入框上才处理（返回 #t，下面的对话区不跟着滚）；内容不超过可见行数时什么也不做
(define (input-scroll! iv x y dy)
  (define g (input-view-geom iv))
  (cond
    [(not (and g (in-rect? x y (geom-box-x g) (geom-box-y g) (geom-box-w g) (geom-box-h g)))) #f]
    [else
     (define tb (geom-tb g))
     (define n (length (text-block-lines tb)))
     (when (> n (geom-visible g))
       (define lh (text-block-line-height tb))
       (define acc (+ (input-view-acc iv) dy))
       (define steps (truncate (/ acc lh)))
       (set-input-view-acc! iv (- acc (* steps lh)))
       (unless (zero? steps)
         (define-values (display caret _marked) (editor-display (input-view-editor iv)))
         (set-input-view-scroll! iv (max 0 (min (- n (geom-visible g)) (- (input-view-scroll iv) (inexact->exact steps)))))
         (set-input-view-pin! iv (cons caret display))))
     #t])
) ; end define input-scroll!

;; 上下移动一行（按视觉行，保持 x）。到顶 / 到底返回 'top / 'bottom（调用方翻历史）。
(define (input-move-vertical! iv dir #:extend? [extend? #f])
  (define g (input-view-geom iv))
  (define e (input-view-editor iv))
  (cond
    [(or (not g) (string=? (geom-display g) "")) (if (< dir 0) 'top 'bottom)]
    [else
     (define tb (geom-tb g))
     (define-values (k x) (text-block-caret tb (editor-cursor e)))
     (define k* (+ k dir))
     (cond
       [(< k* 0) 'top]
       [(>= k* (length (text-block-lines tb))) 'bottom]
       [else
        (define starts (grapheme-starts (geom-display g)))
        (editor-move-to! e (text-block-index-at tb x (* (+ k* 0.5) (text-block-line-height tb))
                                                #:ok? (lambda (i) (vector-ref starts i)))
                         #:extend? extend?)
        'handled])])
) ; end define input-move-vertical!

(define (input-caret-rect iv) (and (input-view-geom iv) (geom-caret-rect (input-view-geom iv))))

;; ---------------------------------------------------------------- 组件

;; needs text-layout → provides input-box
(define input-component
  (component 'input (list text-layout) (list input-box)
             (lambda (use config)
               (define iv (make-input-view (make-editor) ((use text-layout) 'rich)))
               (list (implement input-box
                                'editor (lambda () (input-view-editor iv))
                                'render! (lambda (dl x bottom w scale focused? caret-on?)
                                           (input-render! iv dl x bottom w scale #:focused? focused? #:caret-on? caret-on?))
                                'hit (lambda (x y) (input-hit iv x y))
                                'move-vertical! (lambda (dir extend?) (input-move-vertical! iv dir #:extend? extend?))
                                'caret-rect (lambda () (input-caret-rect iv))
                                'scroll! (lambda (x y dy) (input-scroll! iv x y dy)))))))

(provide input-component editor-folds-in-order placeholder)
