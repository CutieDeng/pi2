#lang racket/base
;; tests/gui/mdview-test.rkt — Markdown 块排版 / 绘制（计算层，假文字后端）：高度、命中区域、长代码块折叠

(require rackunit racket/list
         "../../src/gui/compute/drawlist.rkt" "../../src/gui/compute/markdown.rkt" "../../src/gui/compute/mdview.rkt"
         "../../src/gui/compute/interfaces.rkt" "fakes.rkt")

(define rc ((make-fake-text-layout) 'rich))

(define (draw md #:expanded [open '()] #:w [w 600.0])
  (define hits '())
  (define dl (make-drawlist))
  (define h (md-draw! rc (parse-markdown md) dl 0.0 0.0 w 2.0
                      #:hit! (lambda (kind payload x y ww hh) (set! hits (cons (list kind payload x y ww hh) hits)))
                      #:expanded? (lambda (k) (and (member k open) #t)) #:key 'k))
  (values h (reverse hits) dl))

(define LONG (string-append "```sh\n" (apply string-append (for/list ([i 40]) (format "echo ~a\n" i))) "```"))

(test-case "measure-only and paint give the same height; paint emits instances"
  (define md "# T\n\npara **b** `c`\n\n- a\n- b\n\n> q\n\n---")
  (define-values (h _hits dl) (draw md))
  (define h2 (md-draw! rc (parse-markdown md) (make-drawlist) 0.0 0.0 600.0 2.0 #:paint? #f))
  (check-= h h2 1e-9)
  (check-true (> h 0.0))
  (check-true (> (drawlist-count dl) 10)))                     ; 约 15：字形 + 行内代码底色 + 引用竖条 + 分隔线

(test-case "code block registers a copy hit with the verbatim code"
  (define-values (h hits dl) (draw "x\n\n```py\nprint('*not* emphasis')\n```"))
  (define c (assq 'copy hits))
  (check-equal? (cadr c) "print('*not* emphasis')"))

(test-case "long code block folds by default; the toggle key expands it"
  (define-values (h0 hits0 _d0) (draw LONG))
  (define tg (assq 'code-toggle hits0))
  (check-not-false tg)
  (define-values (h1 hits1 _d1) (draw LONG #:expanded (list (cadr tg))))
  (check-true (> h1 (+ h0 (* 20 12.0))))                       ; 多出 24 行（行高 12.5）
  (check-equal? (cadr (assq 'copy hits0)) (cadr (assq 'copy hits1)))   ; 折叠时复制的仍是全文
  (check-equal? (length (regexp-match* #rx"\n" (cadr (assq 'copy hits0)))) 39))

(test-case "tables: ragged rows and narrow widths do not crash; height grows when columns must wrap"
  (define md "| a | b |\n|---|---|\n| 1 | 2 | 3 |\n| only |\n")
  (define-values (h _h _d) (draw md))
  (check-true (> h 0.0))
  (define wide "| col | text |\n|---|---|\n| x | a fairly long cell of text that will need to wrap |\n")
  (define-values (hw _1 _2) (draw wide #:w 2000.0))
  (define-values (hn _3 _4) (draw wide #:w 200.0))
  (check-true (> hn hw)))

(test-case "nested lists indent deeper and add height per item"
  (define-values (h1 _a _b) (draw "- a"))
  (define-values (h3 _c _d) (draw "- a\n  - b\n    - c"))
  (check-true (> h3 (* 2.5 h1))))

(test-case "streaming: unclosed fence renders as a code block (copy hit present)"
  (define-values (h hits _d) (draw "text\n```rkt\n(define x"))
  (check-equal? (cadr (assq 'copy hits)) "(define x"))
