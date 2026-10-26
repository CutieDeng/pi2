#lang racket/base
;; tests/gui/ansi-test.rkt — 工具输出的 ANSI（design-gui.md §30）：SGR 前景 / 背景（16 / 256 / 真彩色）、复位、别的转义去掉；
;; 颜色调到暗色卡片上看得清（≥ 4.5）

(require rackunit racket/list "../../src/gui/compute/ansi.rkt" "../../src/gui/compute/contrast.rkt")

(define (parse s) (define-values (t sp) (ansi-parse s)) (list t (for/list ([v sp]) (list (vector-ref v 0) (vector-ref v 1) (and (vector-ref v 2) #t) (and (vector-ref v 3) #t)))))

(test-case "SGR colors become spans on the plain text; other escapes are removed"
  (check-equal? (parse "\e[31mred\e[0m plain \e[42;30mok\e[m!") '("red plain ok!" ((0 3 #t #f) (10 12 #t #t))))
  (check-equal? (car (parse "\e[2J\e[1;1Hclear\e]0;title\atext\e(B")) "cleartext")
  (check-equal? (parse "\e[38;5;196mx\e[39m y \e[48;2;10;20;30mz") '("x y z" ((0 1 #t #f) (4 5 #f #t))))
  (check-equal? (car (parse "no escapes")) "no escapes"))

(test-case "colors are made readable on the dark card (≥ 4.5), black turns grey"
  (define card '(0.16 0.16 0.18))
  (for ([n (in-range 256)])
    (check-true (>= (contrast-ratio (readable-fg (xterm256 n) card) card) 4.5) (format "xterm ~a" n)))
  (check-true (> (car (readable-fg '(0.0 0.0 0.0) card)) 0.4)))
