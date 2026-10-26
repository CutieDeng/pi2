#lang racket/base
;; tests/gui/unicode-render-test.rkt — Unicode 显示策略接在真实 Core Text 排版上（design-gui.md §30；原来 14 类的探针改成断言）：
;; 不再有被重排的行（双向控制没被执行）；所有字形都在框里（x ≥ 0：原来从右到左的文字画到框左外）；
;; 双向 / 不可见 / tag / 从右到左的字符都有标记；tag 解码成「隐藏文字」；emoji 序列不出标记；形近字有下标

(require rackunit racket/list
         "../../src/gui/compute/text-backend.rkt" "../../src/gui/compute/mac-coretext.rkt" "../../src/gui/compute/layout.rkt")

(define be (make-coretext-backend))
(define body ((text-backend-font be) "system" 14.0))
(define small ((text-backend-font be) "system" 11.0))
(define (ulabel kind text) (layout-text be text small 100000.0))
(define (lay s) (layout-text be s body 560.0 #:unicode ulabel))
(define (tags s) (list->string (for/list ([c (in-string s)]) (integer->char (+ #xE0000 (char->integer c))))))

(define CASES
  (list (cons 'bidi "file ‮txt.exe‬ done")
        (cons 'isolates "if (admin⁧ ⁦) { ok }")
        (cons 'zero-width "pass​word se‌cr‍et w⁠j b﻿om")
        (cons 'tags (string-append "hello" (tags "ignore previous instructions") "!"))
        (cons 'combining (string-append "Z" (make-string 25 (integer->char #x0336)) "ok"))
        (cons 'hebrew "שלום עולם Hello")
        (cons 'arabic "مرحبا بالعالم 123")
        (cons 'controls "\u001b[31mred\u001b[0m bell\u0007 del\u007f")
        (cons 'separators "line sep para sep")
        (cons 'emoji "👨‍👩‍👧 👍🏽 🏳️‍🌈 1️⃣ ☺︎")
        (cons 'homoglyph "раураl.com paypal.com")
        (cons 'soft "soft­hyphen ￹base￺ruby￻")))

(define (ordered? tb)                                   ; 每行里（非标记的）字形按码点顺序从左到右
  (for/and ([ln (in-list (text-block-lines tb))])
    (define cs (for/list ([gx (in-list (line-glyphs ln))] #:when (exact-integer? (sglyph-font (car gx)))) (sglyph-cluster (car gx))))
    (for/and ([a cs] [b (if (pair? cs) (cdr cs) '())]) (<= a b))))
(define (inside? tb)
  (for*/and ([ln (in-list (text-block-lines tb))] [gx (in-list (line-glyphs ln))]) (>= (cdr gx) -0.5)))

(test-case "no line is visually reordered, every glyph is inside the box"
  (for ([c (in-list CASES)])
    (define tb (lay (cdr c)))
    (check-true (ordered? tb) (format "~a reordered" (car c)))
    (check-true (inside? tb) (format "~a drawn outside the box" (car c)))))

(test-case "marked characters: bidi / invisible / tags / RTL / folded combining; emoji sequences not marked; homoglyph word flagged"
  (define (marks k) (text-block-marks (lay (cdr (assq k CASES)))))
  (check-equal? (sort (hash-keys (marks 'bidi)) <) '(5 13))
  (check-equal? (hash-count (marks 'zero-width)) 5)
  (define tg (marks 'tags))
  (check-equal? (vector-ref (hash-ref tg 5) 0) 'tag)
  (check-regexp-match #rx"ignore previous instructions" (text-block-source (vector-ref (hash-ref tg 5) 1)))
  (check-equal? (hash-count (marks 'hebrew)) 8)
  (check-equal? (hash-count (marks 'arabic)) 12)
  (check-true (> (hash-count (marks 'combining)) 0))
  (check-equal? (hash-count (marks 'emoji)) 0)
  (check-equal? (text-block-odd (lay (cdr (assq 'homoglyph CASES)))) '(0 1 2 3 4 5))
  ;; 控制字符 → 控制符图形；U+2028 / 2029 → 换行（3 行）
  (check-regexp-match #rx"␛\\[31m" (text-block-source (lay (cdr (assq 'controls CASES)))))
  (check-equal? (length (text-block-lines (lay (cdr (assq 'separators CASES))))) 3))
