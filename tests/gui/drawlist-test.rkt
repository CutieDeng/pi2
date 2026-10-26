#lang racket/base
;; tests/gui/drawlist-test.rkt — 实例缓冲区字节布局（与 MSL struct Inst 对齐，64 字节）

(require rackunit "../../src/gui/compute/drawlist.rkt")

(test-case "layout: 16 little-endian f32 per instance"
  (define dl (make-drawlist 1))
  (drawlist-rect! dl 10 20 30 40 0.25 0.5 0.75 1.0 #:radius 6)
  (check-equal? INSTANCE-BYTES 64)
  (check-equal? (drawlist-count dl) 1)
  (check-equal? (drawlist-byte-length dl) 64)
  (check-equal? (drawlist-ref dl 0) '(10.0 20.0 30.0 40.0 0.25 0.5 0.75 1.0 0.0 0.0 0.0 0.0 6.0 0.0 0.0 0.0))
  (check-equal? (floating-point-bytes->real (subbytes (drawlist-buf dl) 0 4) #f) 10.0))

(test-case "grows past initial capacity and keeps earlier instances"
  (define dl (make-drawlist 2))
  (for ([i (in-range 1000)]) (drawlist-rect! dl i 0 1 1 1 1 1 1))
  (check-equal? (drawlist-count dl) 1000)
  (check-true (>= (bytes-length (drawlist-buf dl)) (* 1000 INSTANCE-BYTES)))
  (check-equal? (car (drawlist-ref dl 0)) 0.0)
  (check-equal? (car (drawlist-ref dl 999)) 999.0))

(test-case "clear resets count, reuses buffer"
  (define dl (make-drawlist 4))
  (drawlist-rect! dl 1 2 3 4 1 1 1 1)
  (define buf (drawlist-buf dl))
  (drawlist-clear! dl)
  (check-equal? (drawlist-count dl) 0)
  (check-eq? (drawlist-buf dl) buf))

(define (take-uv l) (list (list-ref l 8) (list-ref l 9) (list-ref l 10) (list-ref l 11)))

(test-case "glyph instances carry uv and kind; order is preserved"
  (define dl (make-drawlist))
  (drawlist-rect! dl 0 0 10 10 1 1 1 1)
  (drawlist-glyph! dl 1 2 3 4 0.125 0.25 0.5 0.75 1 0 0 1)
  (drawlist-glyph! dl 1 2 3 4 0.0 0.0 1.0 1.0 1 1 1 0.5 #:color-glyph? #t)
  (check-equal? (map (lambda (i) (list-ref (drawlist-ref dl i) 13)) '(0 1 2))
                (list (exact->inexact KIND-RECT) (exact->inexact KIND-MASK-GLYPH) (exact->inexact KIND-COLOR-GLYPH)))
  (check-equal? (take-uv (drawlist-ref dl 1)) '(0.125 0.25 0.5 0.75)))

(test-case "append: instances follow, clip points shift, src starts unclipped, dst ends unclipped"
  (define a (make-drawlist 1))
  (drawlist-set-clip! a (vector 0 0 5 5))
  (drawlist-rect! a 0 0 1 1 1 0 0 1)
  (define b (make-drawlist))
  (drawlist-rect! b 1 1 1 1 0 1 0 1)
  (drawlist-set-clip! b (vector 2 2 3 3))
  (drawlist-rect! b 2 2 1 1 0 0 1 1)
  (drawlist-append! a b)
  (drawlist-rect! a 3 3 1 1 1 1 1 1)
  (check-equal? (drawlist-count a) 4)
  (check-equal? (map (lambda (i) (car (drawlist-ref a i))) '(0 1 2 3)) '(0.0 1.0 2.0 3.0))
  (check-equal? (drawlist-segments a)
                (list (vector 0 1 (vector 0 0 5 5)) (vector 1 1 #f) (vector 2 1 (vector 2 2 3 3)) (vector 3 1 #f))))
