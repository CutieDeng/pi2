#lang racket/base
;; mdview.rkt — Markdown 块的排版与绘制（计算层，S14）。同一过程既量高度（paint? #f）也绘制（paint? #t），
;; 对话区组件用它画助手回答。交互区域经 hit! 登记：
;;   (hit! 'copy 文本 x y w h)          代码块「复制」
;;   (hit! 'code-toggle 键 x y w h)     长代码块展开 / 收起（键 = key-prefix + 块路径，由调用方保存展开集合）
;; 字体键（rich-ctx 的 layout-runs）：body bold italic bold-italic code h1 h2 h3 small。

(require racket/list racket/string
         "drawlist.rkt" "layout.rkt" "paint.rkt" "rich.rkt" "markdown.rkt" "text-backend.rkt" "i18n.rkt")

(define TEXT '(0.90 0.91 0.93 1.0))
(define DIM '(0.62 0.64 0.69 1.0))
(define LINK '(0.47 0.67 1.0 1.0))
(define CODE-FG '(0.96 0.78 0.58 1.0))
(define CODE-BG '(0.20 0.20 0.24))
(define BLOCK-BG '(0.11 0.11 0.13))
(define BLOCK-HEAD '(0.15 0.15 0.18))
(define RULE '(0.28 0.29 0.33))
(define TABLE-HEAD '(0.16 0.17 0.21))
(define BUTTON '(0.24 0.25 0.30))

(define GAP 8.0)                  ; 块间距
(define LIST-GUTTER 22.0)
(define QUOTE-INDENT 14.0)
(define CODE-PAD 10.0)
(define CODE-FOLD-OVER 24)        ; 超过这么多行默认折叠
(define CODE-FOLD-SHOW 16)        ; 折叠时显示前这么多行
(define CELL-PAD 7.0)

(define (rect! dl x y w h rgb #:a [a 1.0] #:radius [r 0.0])
  (drawlist-rect! dl x y w h (car rgb) (cadr rgb) (caddr rgb) a #:radius r)
) ; end define rect!

(define (font-for styles base)
  (cond [(memq 'code styles) 'code]
        [(memq base '(h1 h2 h3)) base]
        [(and (memq 'bold styles) (memq 'italic styles)) 'bold-italic]
        [(memq 'bold styles) (if (eq? base 'italic) 'bold-italic 'bold)]
        [(memq 'italic styles) (if (eq? base 'bold) 'bold-italic 'italic)]
        [else base])
) ; end define font-for

(define (color-for styles base-color)
  (cond [(memq 'link styles) LINK]
        [(memq 'code styles) CODE-FG]
        [(memq 'strike styles) DIM]
        [else base-color])
) ; end define color-for

;; ---------------------------------------------------------------- 行内段落

;; 行内片段 → 排版；paint? 时画行内代码底色、文字、删除线。返回高度。
(define (inline-block! rc spans base dl x y w scale paint? color #:align [align 'left])
  (define sps (if (null? spans) (list (span "" '() #f)) spans))
  (define-values (tb ranges) ((rich-ctx-layout-runs rc) (for/list ([sp (in-list sps)]) (cons (span-text sp) (font-for (span-styles sp) base))) base w))
  (when paint?
    (define lh (text-block-line-height tb))
    (define vs (list->vector sps))
    (define (styles-at c) (let ([i (run-index-at ranges c)]) (if i (span-styles (vector-ref vs i)) '())))
    ;; 每行按片段切段：行内代码画底色，删除线画横线
    (define (segments ln pred)
      (let loop ([gs (line-glyphs ln)] [cur #f] [acc '()])
        (cond
          [(null? gs) (reverse (if cur (cons (cons cur (line-end-x ln)) acc) acc))]
          [else
           (define gx (car gs))
           (define in? (pred (styles-at (sglyph-cluster (car gx)))))
           (cond [(and in? (not cur)) (loop (cdr gs) (cdr gx) acc)]
                 [(and (not in?) cur) (loop (cdr gs) #f (cons (cons cur (cdr gx)) acc))]
                 [else (loop (cdr gs) cur acc)])])))
    (define (line-dx ln) (case align [(right) (- w (line-width ln))] [(center) (/ (- w (line-width ln)) 2)] [else 0.0]))
    (for ([ln (in-list (text-block-lines tb))] [k (in-naturals)])
      (for ([sg (in-list (segments ln (lambda (st) (memq 'code st))))])
        (rect! dl (+ x (line-dx ln) (car sg) -2.0) (+ y (* k lh) 1.5) (+ (- (cdr sg) (car sg)) 4.0) (- lh 3.0) CODE-BG #:radius 3.0)))
    (if (eq? align 'left)
        (paint-text! dl (rich-ctx-atlas rc) tb x y color #:scale scale
                     #:color-at (lambda (c) (color-for (styles-at c) color)))
        (for ([ln (in-list (text-block-lines tb))] [k (in-naturals)])
          (paint-text! dl (rich-ctx-atlas rc) (struct-copy text-block tb [lines (list ln)]) (+ x (line-dx ln)) (+ y (* k lh)) color
                       #:scale scale #:color-at (lambda (c) (color-for (styles-at c) color)))))
    (for ([ln (in-list (text-block-lines tb))] [k (in-naturals)])
      (for ([sg (in-list (segments ln (lambda (st) (memq 'strike st))))])
        (rect! dl (+ x (line-dx ln) (car sg)) (+ y (* k lh) (* 0.55 lh)) (- (cdr sg) (car sg)) 1.2 DIM))))
  (text-block-height tb)
) ; end define inline-block!

;; ---------------------------------------------------------------- 块

;; 返回高度。path：块路径（用于代码块折叠键）
(define (draw-blocks! rc blocks dl x y w scale paint? hit! expanded? key color path)
  (for/fold ([yy y] #:result (- yy y (if (null? blocks) 0.0 GAP)))
            ([b (in-list blocks)] [i (in-naturals)])
    (+ yy GAP (draw-block! rc b dl x yy w scale paint? hit! expanded? key color (cons i path))))
) ; end define draw-blocks!

(define (draw-block! rc b dl x y w scale paint? hit! expanded? key color path)
  (define lay (rich-ctx-layout rc))
  (cond
    [(md-para? b) (inline-block! rc (md-para-inlines b) 'body dl x y w scale paint? color)]
    [(md-heading? b)
     (define base (case (md-heading-level b) [(1) 'h1] [(2) 'h2] [(3) 'h3] [else 'bold]))
     (define top (if (equal? path '(0)) 0.0 4.0))
     (+ top (inline-block! rc (md-heading-inlines b) base dl x (+ y top) w scale paint? TEXT))]
    [(md-hr? b) (when paint? (rect! dl x (+ y 7.0) w 1.0 RULE)) 14.0]
    [(md-quote? b)
     (define h (draw-blocks! rc (md-quote-blocks b) dl (+ x QUOTE-INDENT) y (- w QUOTE-INDENT) scale paint? hit! expanded? key DIM path))
     (when paint? (rect! dl (+ x 2.0) y 3.0 h RULE #:radius 1.5))
     h]
    [(md-list? b) (draw-list! rc b dl x y w scale paint? hit! expanded? key color path)]
    [(md-code? b) (draw-code! rc b dl x y w scale paint? hit! expanded? key path)]
    [(md-table? b) (draw-table! rc b dl x y w scale paint? color)]
    [else 0.0])
) ; end define draw-block!

(define (list-depth path) (for/sum ([p (in-list path)]) (if (eq? p 'item) 1 0)))

(define (draw-list! rc b dl x y w scale paint? hit! expanded? key color path)
  (define depth (list-depth path))
  (define bullet (case (modulo depth 3) [(0) "•"] [(1) "◦"] [else "▪"]))
  (for/fold ([yy y] #:result (- yy y 2.0))
            ([item (in-list (md-list-items b))] [i (in-naturals)])
    (define marker (if (md-list-ordered? b) (format "~a." (+ i (md-list-start b))) bullet))
    (define h (max 1.0 (draw-blocks! rc item dl (+ x LIST-GUTTER) yy (- w LIST-GUTTER) scale paint? hit! expanded? key color
                                     (list* i 'item path))))
    (when paint?
      (define mtb ((rich-ctx-layout rc) marker 'body 1000.0 #f))
      (paint-text! dl (rich-ctx-atlas rc) mtb (+ x (- LIST-GUTTER (text-block-width mtb) 6.0)) yy
                   (if (md-list-ordered? b) DIM color) #:scale scale))
    (+ yy h 2.0))
) ; end define draw-list!

(define (draw-code! rc b dl x y w scale paint? hit! expanded? key path)
  (define lay (rich-ctx-layout rc))
  (define text (md-code-text b))
  (define all-lines (string-split text "\n" #:trim? #f))
  (define n (length all-lines))
  (define fold-key (cons key (reverse path)))
  (define foldable? (> n CODE-FOLD-OVER))
  (define open? (or (not foldable?) (expanded? fold-key)))
  (define shown (if open? text (string-join (take all-lines CODE-FOLD-SHOW) "\n")))
  (define head-h 24.0)
  (define body (lay (if (string=? shown "") " " shown) 'code (- w (* 2 CODE-PAD)) #f))
  (define foot-h (if foldable? 22.0 0.0))
  (define h (+ head-h CODE-PAD (text-block-height body) CODE-PAD foot-h))
  (define label (lay (let ([l (md-code-lang b)]) (if (string=? l "") "code" l)) 'small 1000.0 #f))
  (define copy-tb (lay (t 'md.copy) 'small 1000.0 #f))
  (define bw (+ (text-block-width copy-tb) 16.0))
  (define bx (- (+ x w) bw 6.0))
  (define by (+ y 3.0))
  (define bh (- head-h 6.0))
  (when paint?
    (rect! dl x y w h BLOCK-BG #:radius 8.0)
    (rect! dl x y w head-h BLOCK-HEAD #:radius 8.0)
    (rect! dl x (+ y head-h -8.0) w 8.0 BLOCK-HEAD)                     ; 头部下沿取直
    (paint-text! dl (rich-ctx-atlas rc) label (+ x CODE-PAD) (+ y (/ (- head-h (text-block-height label)) 2)) DIM #:scale scale)
    (rect! dl bx by bw bh BUTTON #:radius 5.0)
    (paint-text! dl (rich-ctx-atlas rc) copy-tb (+ bx 8.0) (+ by (/ (- bh (text-block-height copy-tb)) 2)) TEXT #:scale scale)
    (paint-text! dl (rich-ctx-atlas rc) body (+ x CODE-PAD) (+ y head-h CODE-PAD) '(0.86 0.88 0.92 1.0) #:scale scale))
  (hit! 'copy text bx by bw bh)
  (when foldable?
    (define ftb (lay (if open? (t 'md.collapse) (t 'md.expand-all n)) 'small 1000.0 #f))
    (define fy (+ y h (- foot-h)))
    (when paint?
      (rect! dl x fy w 1.0 RULE)
      (paint-text! dl (rich-ctx-atlas rc) ftb (+ x CODE-PAD) (+ fy (/ (- foot-h (text-block-height ftb)) 2)) LINK #:scale scale))
    (hit! 'code-toggle fold-key x fy w foot-h))
  h
) ; end define draw-code!

(define (draw-table! rc b dl x y w scale paint? color)
  (define lay-runs (rich-ctx-layout-runs rc))
  (define header (md-table-header b))
  (define rows (md-table-rows b))
  (define ncol (length header))
  (define (runs spans base) (for/list ([sp (in-list (if (null? spans) (list (span "" '() #f)) spans))])
                              (cons (span-text sp) (font-for (span-styles sp) base))))
  (define (natural spans base) (let-values ([(tb _r) (lay-runs (runs spans base) base 100000.0)]) (text-block-width tb)))
  (define naturals
    (for/list ([c (in-range ncol)])
      (+ (* 2 CELL-PAD) (max 24.0 (natural (list-ref header c) 'bold)
                             (for/fold ([m 0.0]) ([r (in-list rows)]) (max m (natural (list-ref r c) 'body)))))))
  (define total (apply + naturals))
  (define widths
    (if (<= total w)
        naturals
        (let* ([scaled (for/list ([nw (in-list naturals)]) (max 56.0 (* nw (/ w total))))]
               [over (- (apply + scaled) w)])
          ;; 最小宽度挤出的部分从最宽列扣
          (define widest (argmax values scaled))
          (for/list ([s (in-list scaled)]) (if (and (> over 0) (= s widest)) (max 56.0 (- s over)) s)))))
  (define table-w (min w (apply + widths)))
  (define (row-height cells base)
    (+ (* 2 CELL-PAD)
       (for/fold ([m 0.0]) ([c (in-list cells)] [cw (in-list widths)])
         (max m (let-values ([(tb _r) (lay-runs (runs c base) base (- cw (* 2 CELL-PAD)))]) (text-block-height tb))))))
  (define all-rows (cons header rows))
  (define heights (for/list ([r (in-list all-rows)] [i (in-naturals)]) (row-height r (if (zero? i) 'bold 'body))))
  (define h (apply + heights))
  (when paint?
    (rect! dl x y table-w (car heights) TABLE-HEAD #:radius 0.0)
    (for/fold ([ry y]) ([r (in-list all-rows)] [rh (in-list heights)] [i (in-naturals)])
      (for/fold ([cx x]) ([cell (in-list r)] [cw (in-list widths)] [al (in-list (md-table-aligns b))])
        (inline-block! rc cell (if (zero? i) 'bold 'body) dl (+ cx CELL-PAD) (+ ry CELL-PAD) (- cw (* 2 CELL-PAD)) scale #t
                       (if (zero? i) TEXT color) #:align al)
        (+ cx cw))
      (rect! dl x (+ ry rh -1.0) table-w 1.0 RULE)                       ; 行线
      (+ ry rh))
    (rect! dl x y table-w 1.0 RULE)
    (for/fold ([cx x]) ([cw (in-list widths)])                            ; 列线
      (rect! dl cx y 1.0 h RULE)
      (+ cx cw))
    (rect! dl (+ x table-w -1.0) y 1.0 h RULE))
  h
) ; end define draw-table!

;; ---------------------------------------------------------------- 入口

;; blocks：parse-markdown 的结果。expanded?：(键) → bool；key：本段 Markdown 的键前缀（例如对话区条目序号）。
;; hit!：(kind payload x y w h)。返回高度。
(define (md-draw! rc blocks dl x y w scale
                  #:paint? [paint? #t] #:hit! [hit! void] #:expanded? [expanded? (lambda (k) #f)] #:key [key #f]
                  #:color [color TEXT])
  (draw-blocks! rc blocks dl x y w scale paint? hit! expanded? key color '())
) ; end define md-draw!

(provide md-draw!)
