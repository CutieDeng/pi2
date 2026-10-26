#lang racket/base
;; rich.rkt — 富文本（字符串 + 折叠 chip）的排版与绘制（L4）。输入框与对话区里的用户消息共用：
;; 发出去的消息在对话区里同样显示折叠形态，原文只发给模型。
;; chip 在字符串里是 U+FFFC，排版时作为行内原子框，宽度 = 标签宽度。

(require racket/list "drawlist.rkt" "atlas.rkt" "layout.rkt" "paint.rkt"
         "fold.rkt" "text-backend.rkt")

;; layout：(str font-key width box-width-or-#f) → text-block（调用方负责 shaping 缓存）
;; layout-runs：(runs base-font-key width) → (values text-block 片段区间)；runs = (listof (cons 文本 font-key))
(struct rich-ctx (layout atlas layout-runs))

(define CHIP-PAD-X 8.0)
(define CHIP-MAX-W 340.0)
(define CHIP-GAP 3.0)                               ; chip 两侧留白（计入框宽）

(define KIND-COLORS
  (hasheq 'trace '(0.42 0.20 0.22) 'diff '(0.17 0.33 0.24) 'json '(0.17 0.28 0.37)
          'log '(0.38 0.31 0.15) 'code '(0.29 0.22 0.42) 'text '(0.27 0.28 0.32)
          'secret '(0.14 0.34 0.30) 'secret-pending '(0.48 0.38 0.10)
          'image '(0.20 0.30 0.46) 'audio '(0.30 0.24 0.44) 'file '(0.27 0.28 0.32)))   ; 附件（P4）

(define (chip-color f expanded?)
  (define c (hash-ref KIND-COLORS (fold-kind f) '(0.27 0.28 0.32)))
  (if expanded? (map (lambda (v) (* v 0.8)) c) c)                      ; 展开的变深（原来 +0.12 变亮：白字在「待选密钥」上只有 3.35）
) ; end define chip-color

;; 标签：「▸ 报错栈 #1 · Traceback (most recent… · 82 行」；超宽时逐级缩短预览
(define (chip-label rc f expanded?)
  (define mark (if expanded? "▾ " "▸ "))
  (let loop ([ns '(36 24 14 6 0)])
    (define preview (truncate-graphemes (fold-preview f) (car ns)))
    (define text (string-append mark (fold-label (struct-copy fold f [preview preview]))))
    (define tb ((rich-ctx-layout rc) text 'small 10000.0 #f))
    (if (or (null? (cdr ns)) (<= (+ (text-block-width tb) (* 2 CHIP-PAD-X)) CHIP-MAX-W))
        tb
        (loop (cdr ns))))
) ; end define chip-label

;; → (values text-block labels)；labels：vector of (cons 标签text-block chip宽)，按框序号
(define (rich-layout rc str folds width font-key #:expanded [expanded-id #f])
  (define labels
    (for/vector ([f (in-list folds)])
      (define tb (chip-label rc f (eqv? (fold-id f) expanded-id)))
      (cons tb (+ (text-block-width tb) (* 2 CHIP-PAD-X)))))
  (values ((rich-ctx-layout rc) str font-key width
                                (lambda (k) (+ (cdr (vector-ref labels k)) (* 2 CHIP-GAP))))
          labels)
) ; end define rich-layout

;; 只保留 [k0, k0+n) 行
(define (sub-block tb k0 n)
  (define ls (text-block-lines tb))
  (struct-copy text-block tb [lines (take (drop ls (min k0 (length ls))) (max 0 (min n (- (length ls) k0))))])
) ; end define sub-block

;; 绘制 [k0, k0+n) 行，第 k0 行顶在 y。返回 chip 命中矩形：(listof (vector fold-id x y w h))
(define (rich-paint! dl rc tb labels folds x y color scale
                     #:first-line [k0 0] #:line-count [n 100000] #:expanded [expanded-id #f])
  (define sb (sub-block tb k0 n))
  (define lh (text-block-line-height tb))
  (define rects
    (for*/list ([ln+k (in-list (for/list ([ln (in-list (text-block-lines sb))] [k (in-naturals)]) (cons ln k)))]
                [gx (in-list (line-glyphs (car ln+k)))]
                #:when (eq? (sglyph-font (car gx)) 'box))
      (define k (cdr ln+k))
      (define ord (sglyph-glyph (car gx)))
      (define f (list-ref folds ord))
      (define label (vector-ref labels ord))
      (define ltb (car label))
      (define cw (cdr label))
      (define ch (+ (text-block-line-height ltb) 2.0))
      (define cx (+ x (cdr gx) CHIP-GAP))
      (define cy (+ y (* k lh) (/ (- lh ch) 2)))
      (define expanded? (eqv? (fold-id f) expanded-id))
      (define-values (r g b) (apply values (chip-color f expanded?)))
      (drawlist-rect! dl cx cy cw ch r g b 1.0 #:radius 6.0)
      (paint-text! dl (rich-ctx-atlas rc) ltb (+ cx CHIP-PAD-X) (+ cy 1.0) '(0.93 0.94 0.96 1.0) #:scale scale)
      (vector (fold-id f) cx cy cw ch)))
  (paint-text! dl (rich-ctx-atlas rc) sb x y color #:scale scale)
  rects
) ; end define rich-paint!

;; 单行文字（列表标题、菜单项）：放不下 w 时按字素截断加「…」（按实际排版宽度二分，不折行）。
;; lay：rich-ctx 的 layout。→ text-block（一行）
(define (fit-line lay text font w)
  (define full (lay text font 100000.0 #f))
  (cond
    [(<= (text-block-width full) w) full]
    [else
     (define n (let loop ([lo 0] [hi (string-length text)])
                 (if (>= lo hi) lo
                     (let* ([mid (quotient (+ lo hi 1) 2)]
                            [tb (lay (truncate-graphemes text mid) font 100000.0 #f)])
                       (if (<= (text-block-width tb) w) (loop mid hi) (loop lo (sub1 mid)))))))
     (lay (truncate-graphemes text n) font 100000.0 #f)])
) ; end define fit-line

(provide (struct-out rich-ctx) rich-layout rich-paint! sub-block chip-color CHIP-GAP fit-line)
