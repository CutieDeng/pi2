#lang racket/base
;; view-test.rkt — 视图平面（compute/view.rkt）：元素树 → 布局 → 命中区 / 区域 / 滚动。假字体：拉丁 10pt 宽、行高 12.5。

(require rackunit racket/list "fakes.rkt" "../../src/gui/compute/rich.rkt" "../../src/gui/compute/drawlist.rkt"
         "../../src/gui/compute/view.rkt" "../../src/gui/compute/drawlist.rkt")

(define rc ((make-fake-text-layout) 'rich))
(define (run root [w 200.0] [h 100.0])
  (define dl (make-drawlist))
  (define-values (hits regions scrolls) (render-view! rc dl root 0.0 0.0 w h 2.0))
  (values hits regions scrolls dl))
(define (rect-of hits kind) (for/first ([h hits] #:when (eq? (vector-ref h 0) kind)) (vector (vector-ref h 2) (vector-ref h 3) (vector-ref h 4) (vector-ref h 5))))

(test-case "column: fixed heights stack with gap; grow takes the rest; cross axis stretches"
  (define-values (hits _r _s _dl)
    (run (ui-box #:dir 'column #:gap 4.0
              (ui-box #:h 20.0 #:hit (cons 'a 1))
              (ui-box #:grow 1 #:hit (cons 'b 2))
              (ui-box #:h 10.0 #:hit (cons 'c 3)))))
  (check-equal? (rect-of hits 'a) (vector 0.0 0.0 200.0 20.0))
  (check-equal? (rect-of hits 'b) (vector 0.0 24.0 200.0 62.0) "100 - 20 - 10 - 2 gaps")
  (check-equal? (rect-of hits 'c) (vector 0.0 90.0 200.0 10.0)))

(test-case "row: fixed widths, grow shares, padding, and alignment on the cross axis"
  (define-values (hits regions _s _dl)
    (run (ui-box #:dir 'row #:pad 10.0 #:gap 5.0 #:align 'center #:id 'bar
              (ui-box #:w 30.0 #:h 20.0 #:hit (cons 'l 0))
              (ui-box #:grow 1 #:h 20.0 #:hit (cons 'm 0))
              (ui-box #:grow 3 #:h 20.0 #:hit (cons 'r 0)))))
  (check-equal? (hash-ref regions 'bar) (vector 0.0 0.0 200.0 100.0))
  (check-equal? (rect-of hits 'l) (vector 10.0 40.0 30.0 20.0) "centred in the 80-high inner area that starts at y 10")
  (check-equal? (rect-of hits 'm) (vector 45.0 40.0 35.0 20.0) "free = 180 - 30 - 2 gaps × 5 = 140 → × 1/4")
  (check-equal? (rect-of hits 'r) (vector 85.0 40.0 105.0 20.0)))

(test-case "scroll: offset is clamped, content height reported, hits clipped to the viewport"
  (define rows (for/list ([i 10]) (ui-box #:h 20.0 #:hit (cons 'row i))))
  (define-values (hits regions scrolls _dl) (run (apply ui-scroll 'list #:offset 500.0 rows) 100.0 50.0))
  (check-equal? (hash-ref scrolls 'list) (vector 150.0 200.0 50.0) "max offset = 200 - 50")
  (check-equal? (hash-ref regions 'list) (vector 0.0 0.0 100.0 50.0))
  (check-equal? (map (lambda (h) (vector-ref h 1)) hits) '(7 8 9) "only visible rows are hits")
  (define-values (hits2 _r2 _s2 _dl2) (run (apply ui-scroll 'list #:offset 10.0 rows) 100.0 50.0))
  (check-equal? (map (lambda (h) (vector-ref h 1)) hits2) '(0 1 2))
  (check-equal? (rect-of hits2 'row) (vector 0.0 0.0 100.0 10.0) "first row is half scrolled out: clipped"))

(test-case "text fits on one line; overflow hide drops children that do not fit"
  (define-values (hits _r _s dl)
    (run (ui-box #:dir 'row #:overflow 'hide
              (ui-box #:w 120.0 #:hit (cons 'x 1))
              (ui-box #:w 120.0 #:hit (cons 'y 2)))))
  (check-equal? (map (lambda (h) (vector-ref h 0)) hits) '(x))
  (define-values (_h _r2 _s2 dl2) (run (ui-text "abcdefghijklmnopqrstuvwxyz" #:font 'small) 60.0 20.0))
  (check-true (< 0 (drawlist-count dl2) 7) "truncated to fit 60 points"))

(test-case "the topmost (last painted) hit wins; regions answer containment"
  (define-values (hits regions _s _dl)
    (run (ui-box #:dir 'stack #:id 'all
              (ui-box #:hit (cons 'under 0))
              (ui-box #:w 50.0 #:h 50.0 #:hit (cons 'over 0)))))
  (check-eq? (vector-ref (view-hit hits 10.0 10.0) 0) 'over)
  (check-true (in-region? regions 'all 150.0 90.0))
  (check-false (in-region? regions 'all 250.0 90.0)))

(test-case "wrapping rows: children that do not fit start a new line"
  (define kids (for/list ([i 5]) (ui-box #:w 60.0 #:h 20.0 #:hit (cons 'b i))))
  (define-values (hits _r _s _dl) (run (apply ui-box #:dir 'row #:wrap #t #:gap 10.0 kids) 200.0 100.0))
  (check-equal? (map (lambda (h) (list (vector-ref h 2) (vector-ref h 3))) hits)
                '((0.0 0.0) (70.0 0.0) (140.0 0.0) (0.0 30.0) (70.0 30.0)) "3 per line in 200 (60 + 10 + 60 + 10 + 60 = 200 fits; a 4th does not)")
  (define-values (mw mh) (measure (rich-ctx-layout rc) (apply ui-box #:dir 'row #:wrap #t #:gap 10.0 kids) 200.0))
  (check-equal? mh 50.0 "2 lines × 20 + 1 gap × 10"))

(test-case "row: a growing child's height is measured at the width it gets, not the whole row"
  ;; fake layout: 7 px per char, wraps by width → "aaaa aaaa aaaa" (14 chars = 98 px) in 60 px → 2+ lines
  (define row (ui-box #:dir 'row #:align 'start (ui-box #:grow 1 (ui-text "aaaa aaaa aaaa" #:fit? #f)) (ui-box #:w 60.0 #:h 10.0)))
  (define-values (_w h-narrow) (measure (rich-ctx-layout rc) row 120.0))
  (define-values (_w2 h-wide) (measure (rich-ctx-layout rc) (ui-box #:dir 'row (ui-box #:grow 1 (ui-text "aaaa aaaa aaaa" #:fit? #f))) 120.0))
  (check-true (> h-narrow h-wide) "60 px left for the text → more lines than at 120"))

(test-case "#:fit? #f texts wrap (props whose value is #f are not stored: the flag is kept as 'wrap?)"
  (define-values (_w h1) (measure (rich-ctx-layout rc) (ui-text "aaaa aaaa aaaa") 60.0))
  (define-values (_w2 h2) (measure (rich-ctx-layout rc) (ui-text "aaaa aaaa aaaa" #:fit? #f) 60.0))
  (check-true (> h2 h1)))

(test-case "a wrapping text placed at its own measured width stays on one line (pad arithmetic can lose an ulp)"
  (define lay (rich-ctx-layout rc))
  (for ([str (in-list '("ab" "保存" "abc def" "x"))])
    (define-values (w1 h1) (measure lay (ui-text str #:fit? #f) 1000.0))
    (define b (ui-box #:dir 'row #:pad (vector 4.0 9.3 4.0 9.1) (ui-text str #:fit? #f)))
    (define-values (bw bh) (measure lay b 1000.0))
    (define-values (_h _r _s dl) (run b bw bh))
    (check-equal? bh (+ h1 8.0) str)))

(test-case "a one-line text placed at its own measured width is not truncated"
  (define lay (rich-ctx-layout rc))
  (for* ([str (in-list '("ab" "abcdef" "ab cd"))] [pl (in-list '(9.3 9.1 8.7 0.1 0.3 7.7))])
    (define b (ui-box #:dir 'row #:pad (vector 4.0 pl 4.0 9.1) (ui-text str)))
    (define-values (bw bh) (measure lay b 1000.0))
    (define-values (_h _r _s dl) (run b bw bh))
    (check-equal? (drawlist-count dl) (string-length (regexp-replace* #rx" " str "")) (format "~a / ~a" str pl))))
