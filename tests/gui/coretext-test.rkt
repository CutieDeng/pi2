#lang racket/base
;; tests/gui/coretext-test.rkt — Core Text 后端（C 接口，无窗口）：回退、码点下标换算、彩色字形、空字形

(require rackunit racket/list
         "../../src/gui/compute/text-backend.rkt"
         "../../src/gui/compute/mac-coretext.rkt")

(define be (make-coretext-backend))
(define menlo ((text-backend-font be) "Menlo" 14))
(define (shape s) ((text-backend-shape be) s menlo))

(test-case "fallback: CJK and emoji get their own fonts"
  (define gs (vector->list (shaped-glyphs (shape "ab你🚀"))))
  (define names (map (lambda (g) (font-name (sglyph-font g))) gs))
  (check-equal? (first names) "Menlo-Regular")
  (check-not-equal? (third names) "Menlo-Regular")                    ; CJK 回退（本机为 PingFangSC）
  (check-true (font-color? (sglyph-font (fourth gs))))                ; emoji 字体带彩色字形特征位
  (check-false (font-color? menlo)))

(test-case "clusters are code point indices, not UTF-16 (surrogate pair before CJK)"
  (define gs (vector->list (shaped-glyphs (shape "🚀你x"))))
  (check-equal? (map sglyph-cluster gs) '(0 1 2)))

(test-case "x positions increase for LTR text; width covers the last glyph"
  (define sh (shape "hello 世界"))
  (define xs (map sglyph-x (vector->list (shaped-glyphs sh))))
  (check-equal? xs (sort xs <))
  (check-true (> (shaped-width sh) (last xs))))

(test-case "rasterize: space is empty; letter is mask; emoji is color with ink"
  (define gs (shaped-glyphs (shape "a 🚀")))
  (define r (text-backend-rasterize be))
  (check-false (r menlo (sglyph-glyph (vector-ref gs 1)) 2.0 0.0))
  (define a (r menlo (sglyph-glyph (vector-ref gs 0)) 2.0 0.5))
  (check-false (vector-ref a 5))
  (check-equal? (bytes-length (vector-ref a 4)) (* 4 (vector-ref a 0) (vector-ref a 1)))
  (define e (r (sglyph-font (vector-ref gs 2)) (sglyph-glyph (vector-ref gs 2)) 2.0 0.0))
  (check-true (vector-ref e 5))
  (check-true (for/or ([i (in-range 3 (bytes-length (vector-ref e 4)) 4)]) (> (bytes-ref (vector-ref e 4) i) 200))))

(test-case "subpixel offset shifts coverage"
  (define g (sglyph-glyph (vector-ref (shaped-glyphs (shape "l")) 0)))
  (define r (text-backend-rasterize be))
  (check-not-equal? (vector-ref (r menlo g 2.0 0.0) 4) (vector-ref (r menlo g 2.0 0.5) 4)))

(test-case "empty string"
  (check-equal? (shaped-glyphs (shape "")) (vector)))

(test-case "italic / bold-italic families resolve to italic faces"
  (define it ((text-backend-font be) "system-italic" 14))
  (define bi ((text-backend-font be) "system-bold-italic" 14))
  (check-regexp-match #rx"(?i:italic)" (font-name it))
  (check-regexp-match #rx"(?i:italic)" (font-name bi))
  (check-regexp-match #rx"(?i:bold|semibold|heavy)" (font-name bi)))
