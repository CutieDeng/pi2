#lang racket/base
;; layout.rkt — 段落排版（S3）：shaping 一次（整段一行），再按断行机会贪心折行。
;; 折行不重新 shaping：跨断点的字距 / 连字只在空白与 CJK 边界处断开，影响可忽略（推断，未量化）。
;; 只处理 LTR；RTL / 双向文本不在 G1 范围。
;; Unicode 显示策略（design-gui.md §30，#:unicode 给了才做）：布局控制 / 不可见 / 从右到左的字符排成标记框（宽度 = 标记文字宽），
;; 原字符留在源串里（下标不变：光标、选区、复制都不用改）；双向控制因此不会被 Core Text 执行；形近字的下标记在 deco 里（画虚线下划线）。

(require racket/list "text-backend.rkt" "linebreak.rkt" "unicode-policy.rkt")

;; start/end：在整串中的码点区间 [start, end)；width：去掉行尾空白后的宽度（point）；
;; end-x：含行尾空白的宽度（光标放在行尾时用）；glyphs：(listof (cons sglyph x))，x 相对行首。
;; 行内原子框（U+FFFC）在 glyphs 里是 font = 'box、glyph = 框序号（整串中第几个 U+FFFC）的 sglyph。
(struct line (start end width end-x glyphs) #:transparent)

;; lines；line-height、baseline（行框顶到基线的距离）单位 point
;; source：排版用的字符串（行的 start/end 是它的下标）——选择 / 复制按「看到的文字」取
;; deco：(hasheq 'marks (hash 下标 → (vector 种类 标记文字的text-block或#f 宽)) 'odd (下标表：形近字))；没做策略 = (hasheq)
;; （普通字段而非 #:auto：struct-copy 不支持 #:auto 字段）
(struct text-block (lines line-height baseline width source deco) #:transparent)
(define (text-block-marks tb) (hash-ref (text-block-deco tb) 'marks (hash)))
(define (text-block-odd tb) (hash-ref (text-block-deco tb) 'odd '()))

;; 策略：→ (values 显示串 标记表 deco)。ulabel：(种类 文字) → text-block（标记文字用小字排）
(define MARK-PAD 8.0)
(define (apply-unicode-policy s ulabel)
  (define-values (disp marks odd) (unicode-scan s))
  (define mh (for/hash ([m (in-list marks)])
               (define label (vector-ref m 2))                                    ; 文字怎么显示（tag 的前缀等）由 ulabel 决定（界面语言）
               (define tb (and (not (string=? label "")) (ulabel (vector-ref m 1) label)))
               (values (vector-ref m 0) (vector (vector-ref m 1) tb (if tb (+ (text-block-width tb) MARK-PAD) 0.0)))))
  (values disp mh (hasheq 'marks mh 'odd odd)))

;; 段内各字素起点的 x（未覆盖的位置向后取最近已知值；n 处 = 总宽）
(define (boundary-xs s sh starts)
  (define n (string-length s))
  (define mx (make-vector (add1 n) #f))
  (for ([g (in-vector (shaped-glyphs sh))])
    (define c (sglyph-cluster g))
    (define x (sglyph-x g))
    (when (or (not (vector-ref mx c)) (< x (vector-ref mx c))) (vector-set! mx c x)))
  ;; 字素内的最小 x 归到字素起点
  (let loop ([i n] [cur-start n])
    (when (>= i 0)
      (cond
        [(vector-ref starts i) (loop (sub1 i) i)]
        [else
         (define x (vector-ref mx i))
         (when (and x (or (not (vector-ref mx cur-start)) (< x (vector-ref mx cur-start))))
           (vector-set! mx cur-start x))
         (loop (sub1 i) cur-start)])))
  (vector-set! mx n (shaped-width sh))
  (for/fold ([next (shaped-width sh)]) ([i (in-range n -1 -1)])
    (cond [(vector-ref mx i) (vector-ref mx i)]
          [else (vector-set! mx i next) next]))
  mx
) ; end define boundary-xs

(define (trim-end s a b)
  (let loop ([b b]) (if (and (> b a) (space? (string-ref s (sub1 b)))) (loop (sub1 b)) b))
) ; end define trim-end

;; 单段（不含换行）→ (listof (cons start end))，段内下标
(define (break-paragraph s xs starts max-width)
  (define n (string-length s))
  (define (width a b) (- (vector-ref xs (trim-end s a b)) (vector-ref xs a)))
  ;; 紧急断行：[a, limit) 内能放下的最长字素前缀，至少一个字素
  (define (emergency a limit)
    (let loop ([i (add1 a)] [best #f])
      (cond
        [(> i limit) (or best limit)]
        [(not (vector-ref starts i)) (loop (add1 i) best)]
        [(<= (width a i) max-width) (loop (add1 i) i)]
        [else (or best i)])))
  (let loop ([start 0] [ops (append (break-opportunities s) (list n))] [last-ok #f] [acc '()])
    (cond
      [(null? ops) (reverse (if (< start n) (cons (cons start n) acc) acc))]
      [else
       (define o (car ops))
       (cond
         [(<= (width start o) max-width)
          (if (= o n)
              (reverse (cons (cons start n) acc))
              (loop start (cdr ops) o acc))]
         [last-ok (loop last-ok ops #f (cons (cons start last-ok) acc))]
         [else
          (define e (emergency start o))
          (loop e (if (= e o) (cdr ops) ops) #f (cons (cons start e) acc))])]))
) ; end define break-paragraph

;; 单段（已 shaping）→ 行。off：本段在整串中的起始下标（glyph cluster 换成整串下标）
(define (paragraph-lines p sh off max-width)
  (define starts (grapheme-starts p))
  (define xs (boundary-xs p sh starts))
  (define spans (if (zero? (string-length p)) (list (cons 0 0)) (break-paragraph p xs starts max-width)))
  (for/list ([sp (in-list spans)])
    (define a (car sp))
    (define b (cdr sp))
    (define x0 (vector-ref xs a))
    (line (+ off a) (+ off b) (- (vector-ref xs (trim-end p a b)) x0) (- (vector-ref xs b) x0)
          (for/list ([g (in-vector (shaped-glyphs sh))]
                     #:when (and (<= a (sglyph-cluster g)) (< (sglyph-cluster g) b)))
            (cons (sglyph (sglyph-font g) (sglyph-glyph g) (sglyph-x g) (+ off (sglyph-cluster g)))
                  (- (sglyph-x g) x0)))))
) ; end define paragraph-lines

;; 多样式段落（S14）：runs = (listof (cons 文本 字体id))，同一段里不同字体（粗体、斜体、行内代码……），不含换行。
;; 各片段用自己的字体 shaping，拼接后统一断行；行高 / 基线取 base 字体。
;; → (values text-block 片段区间)，片段区间 = vector of (vector 起 止 片段序号)，绘制时按 cluster 取色。
(define (layout-runs backend runs max-width #:base-font base
                     #:shape [shape-fn (text-backend-shape backend)]
                     #:line-spacing [spacing 1.25]
                     #:unicode [ulabel #f])
  (define m ((text-backend-metrics backend) base))
  (define asc (vector-ref m 0))
  (define desc (vector-ref m 1))
  (define lh (* spacing (+ asc desc (vector-ref m 2))))
  (define clean-runs0 (for/list ([r (in-list runs)]) (cons (regexp-replace* #rx"[\r\n]" (car r) " ") (cdr r))))
  (define text0 (apply string-append (map car clean-runs0)))
  ;; 策略：整串扫一遍；显示串按片段长度切回各片段（等长）；标记位置按整串下标
  (define-values (text mh deco)
    (if ulabel (apply-unicode-policy text0 ulabel) (values text0 (hash) (hasheq))))
  (define clean-runs (let loop ([rs clean-runs0] [a 0] [acc '()])
                       (if (null? rs) (reverse acc)
                           (let ([len (string-length (car (car rs)))])
                             (loop (cdr rs) (+ a len) (cons (cons (regexp-replace* #rx"\n" (substring text a (+ a len)) " ") (cdr (car rs))) acc))))))
  (define-values (glyphs width ranges)
    (for/fold ([gs '()] [x 0.0] [rs '()] [start 0]
               #:result (values (list->vector (reverse gs)) x (list->vector (reverse rs))))
              ([r (in-list clean-runs)] [i (in-naturals)])
      (define len (string-length (car r)))
      (define sh (cond [(zero? len) (shaped (vector) 0.0)]
                       [(for/or ([k (in-range start (+ start len))]) (hash-ref mh k #f))       ; 片段里有标记：分段 shaping
                        (shape-segments (car r) (cdr r) shape-fn #f 0 (lambda (k) (let ([m (hash-ref mh (+ start k) #f)]) (and m (vector-ref m 2)))))]
                       [else (shape-fn (car r) (cdr r))]))
      (values (for/fold ([gs gs]) ([g (in-vector (shaped-glyphs sh))])
                (cons (sglyph (sglyph-font g) (sglyph-glyph g) (+ x (sglyph-x g)) (+ start (sglyph-cluster g))) gs))
              (+ x (shaped-width sh))
              (cons (vector start (+ start len) i) rs)
              (+ start len))))
  (define ls (paragraph-lines text (shaped glyphs width) 0 max-width))
  (values (text-block ls lh (+ (/ (- lh (+ asc desc)) 2) asc)
                      (for/fold ([w 0.0]) ([l (in-list ls)]) (max w (line-width l))) text deco)
          ranges)
) ; end define layout-runs

;; 片段区间里 cluster 所属的片段序号
(define (run-index-at ranges c)
  (for/first ([r (in-vector ranges)] #:when (and (<= (vector-ref r 0) c) (< c (vector-ref r 1)))) (vector-ref r 2))
) ; end define run-index-at

;; 分段 shaping：在框处切开。框 = U+FFFC（box-width 给了时：输入框的 chip，宽 (box-width 序号)，base：本段第一个框的全局序号）
;; 或标记（mark-width：(段内下标) → #f | 宽；字形 = (sglyph 'mark 段内下标 x 段内下标)）
(define (shape-segments p font shape-fn box-width base mark-width)
  (define n (string-length p))
  (define (box-at? i) (or (and box-width (inline-box? (string-ref p i))) (and mark-width (mark-width i))))
  (let loop ([a 0] [x 0.0] [ord base] [acc '()])
    (define b (let find ([i a]) (if (or (= i n) (box-at? i)) i (find (add1 i)))))
    (define seg (if (< a b) (shape-fn (substring p a b) font) (shaped (vector) 0.0)))
    (define acc* (for/fold ([acc acc]) ([g (in-vector (shaped-glyphs seg))])
                   (cons (sglyph (sglyph-font g) (sglyph-glyph g) (+ x (sglyph-x g)) (+ a (sglyph-cluster g))) acc)))
    (define x* (+ x (shaped-width seg)))
    (cond
      [(= b n) (shaped (list->vector (reverse acc*)) x*)]
      [(and mark-width (mark-width b)) => (lambda (w) (loop (add1 b) (+ x* w) ord (cons (sglyph 'mark b x* b) acc*)))]
      [else
       (define w (box-width ord))
       (loop (add1 b) (+ x* w) (add1 ord) (cons (sglyph 'box ord x* b) acc*))]))
) ; end define shape-segments
(define (shape-with-boxes p font shape-fn box-width base) (shape-segments p font shape-fn box-width base #f))

(define (count-boxes p) (for/sum ([c (in-string p)]) (if (inline-box? c) 1 0)))

;; shape-fn：(str font) → shaped；默认用后端，调用方可传带缓存的版本。
;; box-width：(序号) → point，给了才把 U+FFFC 当行内原子框排；不给则 U+FFFC 交给字体（不推荐）。
(define (layout-text backend str font max-width
                     #:shape [shape-fn (text-backend-shape backend)]
                     #:box-width [box-width #f]
                     #:line-spacing [spacing 1.25]
                     #:unicode [ulabel #f])
  (define m ((text-backend-metrics backend) font))
  (define asc (vector-ref m 0))
  (define desc (vector-ref m 1))
  (define lead (vector-ref m 2))
  (define lh (* spacing (+ asc desc lead)))
  (define clean0 (regexp-replace* #rx"\r" str ""))
  (define-values (clean mh deco) (if ulabel (apply-unicode-policy clean0 ulabel) (values clean0 (hash) (hasheq))))
  (define paragraphs (regexp-split #rx"\n" clean))
  (define-values (lines _off _boxes)
    (for/fold ([acc '()] [off 0] [boxes 0]) ([p (in-list paragraphs)])
      (define marks? (for/or ([k (in-range off (+ off (string-length p)))]) (hash-ref mh k #f)))
      (define sh (cond
                   [(or (and box-width (> (count-boxes p) 0)) marks?)
                    (shape-segments p font shape-fn box-width boxes
                                    (and marks? (lambda (k) (let ([m (hash-ref mh (+ off k) #f)]) (and m (vector-ref m 2))))))]
                   [else (shape-fn p font)]))
      (values (append (reverse (paragraph-lines p sh off max-width)) acc)
              (+ off (string-length p) 1) (+ boxes (count-boxes p)))))
  (define ls (reverse lines))
  (text-block ls lh (+ (/ (- lh (+ asc desc)) 2) asc)
              (for/fold ([w 0.0]) ([l (in-list ls)]) (max w (line-width l))) clean deco)
) ; end define layout-text

(define (text-block-height tb) (* (length (text-block-lines tb)) (text-block-line-height tb)))

;; ---------------------------------------------------------------- 光标 / 命中

;; 行内下标 i（整串下标）处的 x（相对行首）。行首 → 0；行尾 → end-x；否则取 cluster ≥ i 的第一个字形的 x。
(define (line-x-of ln i)
  (cond
    [(<= i (line-start ln)) 0.0]
    [(>= i (line-end ln)) (line-end-x ln)]
    [else (or (for/first ([gx (in-list (line-glyphs ln))] #:when (>= (sglyph-cluster (car gx)) i)) (cdr gx))
              (line-end-x ln))])
) ; end define line-x-of

;; 下标 i 所在的行号：最后一个 start ≤ i 的行（软换行处的下标归下一行行首）
(define (text-block-line-of tb i)
  (for/fold ([k 0]) ([ln (in-list (text-block-lines tb))] [j (in-naturals)])
    (if (<= (line-start ln) i) j k))
) ; end define text-block-line-of

;; → (values 行号 x)，x 相对文字块左边
(define (text-block-caret tb i)
  (define k (text-block-line-of tb i))
  (values k (line-x-of (list-ref (text-block-lines tb) k) i))
) ; end define text-block-caret

;; 点 (x, y)（相对文字块左上）→ 最近的光标下标。ok?：下标是否可作光标位置（字素边界），默认全可。
(define (text-block-index-at tb x y #:ok? [ok? (lambda (i) #t)])
  (define lines (text-block-lines tb))
  (define k (max 0 (min (sub1 (length lines)) (inexact->exact (floor (/ y (text-block-line-height tb)))))))
  (define ln (list-ref lines k))
  (define candidates
    (cons (cons (line-start ln) 0.0)
          (append (for/list ([gx (in-list (line-glyphs ln))]
                             #:when (and (> (sglyph-cluster (car gx)) (line-start ln)) (ok? (sglyph-cluster (car gx)))))
                    (cons (sglyph-cluster (car gx)) (cdr gx)))
                  (list (cons (line-end ln) (line-end-x ln))))))
  (car (for/fold ([best (car candidates)]) ([c (in-list (cdr candidates))])
         (if (< (abs (- (cdr c) x)) (abs (- (cdr best) x))) c best)))
) ; end define text-block-index-at

(provide (struct-out line) (struct-out text-block) text-block-marks text-block-odd layout-text text-block-height layout-runs run-index-at
         line-x-of text-block-line-of text-block-caret text-block-index-at)
