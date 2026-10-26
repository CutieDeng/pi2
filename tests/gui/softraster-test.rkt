#lang racket/base
;; softraster-test.rkt — 参考光栅器（compute/softraster.rkt）：不需要 GPU 的逐像素检查（任何平台都能跑）；
;; 在 macOS 上另跑 app.rkt --conformance，与 Metal 渲染器逐像素比（判据：compute/conformance.rkt）。

(require rackunit racket/runtime-path racket/port racket/system
         "../../src/gui/compute/softraster.rkt" "../../src/gui/compute/drawlist.rkt" "../../src/gui/compute/conformance.rkt")

(define CLEAR '(0.0 0.0 0.0 1.0))
(define (px img pw x y) (for/list ([k 4]) (bytes-ref img (+ (* 4 (+ x (* y pw))) k))))   ; BGRA

(test-case "opaque rect, half-transparent blend over the clear colour, pixels outside untouched"
  (define dl (make-drawlist))
  (drawlist-rect! dl 2 2 4 4 1.0 0.0 0.0 1.0)
  (drawlist-rect! dl 10 2 4 4 0.0 1.0 0.0 0.5)
  (define img (rasterize-drawlist dl 16 8 1.0 CLEAR))
  (check-equal? (px img 16 3 3) '(0 0 255 255))
  (check-equal? (px img 16 0 0) '(0 0 0 255))
  (check-equal? (px img 16 11 3) '(0 128 0 255) "0.5 green over black"))

(test-case "clip segments are scissors; the hole fills only the corners outside the rounded rect"
  (define dl (make-drawlist))
  (drawlist-set-clip! dl (vector 0.0 0.0 5.0 10.0))
  (drawlist-rect! dl 0 0 10 10 1.0 1.0 1.0 1.0)
  (drawlist-set-clip! dl #f)
  (drawlist-hole! dl 10 0 20 20 0.0 0.0 1.0 1.0 #:radius 10.0)
  (define img (rasterize-drawlist dl 40 20 1.0 CLEAR))
  (check-equal? (px img 40 2 2) '(255 255 255 255))
  (check-equal? (px img 40 7 2) '(0 0 0 255) "outside the clip")
  (check-equal? (px img 40 10 0) '(255 0 0 255) "hole: corner filled (BGRA blue)")
  (check-equal? (px img 40 20 10) '(0 0 0 255) "hole: centre untouched"))

(test-case "rounded corners are anti-aliased (partial coverage on the curve)"
  (define dl (make-drawlist))
  (drawlist-rect! dl 0 0 20 20 1.0 1.0 1.0 1.0 #:radius 8.0)
  (define img (rasterize-drawlist dl 20 20 1.0 CLEAR))
  (check-equal? (px img 20 0 0) '(0 0 0 255) "corner cut")
  (check-equal? (px img 20 10 10) '(255 255 255 255))
  (define edge (car (px img 20 2 2)))
  (check-true (< 0 edge 255) (format "partial coverage on the curve: ~a" edge)))

(test-case "the difference metric"
  (define a (bytes 0 0 0 255 10 10 10 255))
  (define b (bytes 0 0 0 255 30 10 10 255))
  (define-values (mx frac) (image-diff a b 8))
  (check-equal? mx 20)
  (check-= frac 0.5 1e-9)
  (check-true (conformance-pass? 58 0.0001))
  (check-false (conformance-pass? 80 0.0)))

(define-runtime-path APP "../../src/gui/assembly/app.rkt")
(test-case "macOS: Metal and the reference rasterizer agree on the conformance drawlists"
  (when (eq? (system-type 'os) 'macosx)
    (define out (open-output-string))
    (define ok? (parameterize ([current-output-port out] [current-error-port out])
                  (system* (find-system-path 'exec-file) (path->string APP) "--conformance")))
    (define text (get-output-string out))
    (check-true ok? text)
    (check-regexp-match #rx"CONFORMANCE ok" text)))
