#lang racket/base
;; paint.rkt — 已排版文字 → drawlist 字形实例。
;; 像素对齐：基线取整到像素；笔位置的像素小数量化为 1/4（SUBPX-BINS），位图已含该小数偏移，
;; 因此四边形与像素网格一一对应，atlas 用最近邻采样即可。

(require "text-backend.rkt" "drawlist.rkt" "atlas.rkt" "layout.rkt")

(define SUBPX-BINS 4)

;; Unicode 标记（design-gui.md §30）：种类 → (底色 . 字色)。对比度：字 ≥ 4.5、底与常见背景 ≥ 1.2（显示检查覆盖）
(define (mark-colors kind)
  (case kind
    [(tag) (cons '(0.45 0.12 0.10 1.0) '(1.0 0.88 0.85 1.0))]                 ; 隐藏文字：警告
    [(rtl) (cons '(0.22 0.24 0.30 1.0) '(0.80 0.83 0.90 1.0))]                ; 不支持的文字：中性
    [else (cons '(0.30 0.26 0.12 1.0) '(0.95 0.85 0.55 1.0))]))               ; 布局控制 / 不可见 / 组合符堆叠：琥珀
(define ODD-UNDERLINE '(0.95 0.65 0.30 1.0))                                  ; 形近字的虚线下划线

;; 文字行记录钩子（选择 / 复制用）：设置后，paint-text! 每画一行调用
;;   (sink 行文字 字符x表 行顶y 行高 软换行?)  —— 字符x表长度 = 行字符数 + 1（绝对坐标，point）；
;;   软换行? = 本行与下一行属于同一段（复制时不插换行）
(define current-text-sink (make-parameter #f))

(define (report-line! sink tb ln k x y lh lines-v)
  (define src (text-block-source tb))
  (define a (line-start ln))
  (define b (min (line-end ln) (string-length src)))
  (define n (max 0 (- b a)))
  (define xs (make-vector (add1 n) #f))
  (for ([gx (in-list (line-glyphs ln))])
    (define i (- (sglyph-cluster (car gx)) a))
    (when (and (<= 0 i) (< i n) (or (not (vector-ref xs i)) (< (cdr gx) (vector-ref xs i))))
      (vector-set! xs i (cdr gx))))
  (vector-set! xs n (line-end-x ln))
  (for/fold ([next (line-end-x ln)]) ([i (in-range (sub1 n) -1 -1)])      ; 无字形的位置（组合符等）取后继
    (define v (or (vector-ref xs i) next))
    (vector-set! xs i v)
    v)
  (define soft? (and (< (add1 k) (vector-length lines-v)) (= (line-end ln) (line-start (vector-ref lines-v (add1 k))))))
  (sink (if (> n 0) (substring src a b) "") (for/vector ([v (in-vector xs)]) (+ x v)) (+ y (* k lh)) lh soft?)
) ; end define report-line!

;; x y：文字块左上角（point）；color：(list r g b a)。返回 #f 表示 atlas 已满（调用方 reset 后重建本帧）。
;; color-at：可选，(cluster) → 颜色，多样式段落按片段取色（缺省整块同色）
(define (paint-text! dl at tb x y color #:scale scale #:color-at [color-at #f])
  (define-values (r0 g0 b0 a0) (apply values color))
  (define lh (text-block-line-height tb))
  (define sink (current-text-sink))
  (define lines-v (and sink (list->vector (text-block-lines tb))))
  (for ([ln (in-list (text-block-lines tb))] [k (in-naturals)])
    (when sink (report-line! sink tb ln k x y lh lines-v))
    (define baseline-px (round (* scale (+ y (* k lh) (text-block-baseline tb)))))
    ;; Unicode 标记：小圆角底 + 标记文字（零宽的不画）
    (for ([gx (in-list (line-glyphs ln))] #:when (eq? (sglyph-font (car gx)) 'mark))
      (define m (hash-ref (text-block-marks tb) (sglyph-cluster (car gx)) #f))
      (when (and m (vector-ref m 1))
        (define label (vector-ref m 1))
        (define c (mark-colors (vector-ref m 0)))
        (define mw (- (vector-ref m 2) 2.0))
        (define mh (+ (text-block-line-height label) 1.0))
        (define mx (+ x (cdr gx) 1.0))
        (define my (+ y (* k lh) (/ (- lh mh) 2)))
        (drawlist-rect! dl mx my mw mh (car (car c)) (cadr (car c)) (caddr (car c)) 1.0 #:radius 3.0)
        (paint-text! dl at label (+ mx 3.0) (+ my 0.5) (cdr c) #:scale scale)))
    ;; 形近字：虚线下划线（字形的前进宽度上）
    (define odd (text-block-odd tb))
    (when (pair? odd)
      (define gl (line-glyphs ln))
      (for ([gx (in-list gl)] [nx (in-list (append (map cdr (if (pair? gl) (cdr gl) '())) (list (line-end-x ln))))]
            #:when (memv (sglyph-cluster (car gx)) odd))
        (define uy (+ y (* k lh) (text-block-baseline tb) 2.0))
        (for ([dx (in-range 0.0 (max 1.0 (- nx (cdr gx))) 3.0)])
          (drawlist-rect! dl (+ x (cdr gx) dx) uy (min 1.5 (- nx (cdr gx) dx)) 1.0 (car ODD-UNDERLINE) (cadr ODD-UNDERLINE) (caddr ODD-UNDERLINE) 1.0))))
    (for ([gx (in-list (line-glyphs ln))] #:when (exact-integer? (sglyph-font (car gx))))   ; 跳过行内框（'box）与标记（'mark）
      (define sg (car gx))
      (define pen-px (* scale (+ x (cdr gx))))
      (define ipx (floor pen-px))
      (define sub (/ (floor (* SUBPX-BINS (- pen-px ipx))) SUBPX-BINS))
      (define e (atlas-glyph! at (sglyph-font sg) (sglyph-glyph sg) scale (exact->inexact sub)))
      (when e
        (define-values (r g b a) (if color-at (apply values (color-at (sglyph-cluster sg))) (values r0 g0 b0 a0)))
        (define px (+ ipx (entry-left e)))
        (define py (- baseline-px (entry-top e)))
        (drawlist-glyph! dl (/ px scale) (/ py scale) (/ (entry-w e) scale) (/ (entry-h e) scale)
                         (entry-u0 e) (entry-v0 e) (entry-u1 e) (entry-v1 e) r g b a
                         #:color-glyph? (entry-color? e)))))
  (not (atlas-full? at))
) ; end define paint-text!

(provide paint-text! SUBPX-BINS current-text-sink)
