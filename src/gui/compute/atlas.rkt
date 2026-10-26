#lang racket/base
;; atlas.rkt — 字形图集（S2）。CPU 侧一张 RGBA 预乘位图 + shelf 装箱 + 脏区；GPU 纹理由平台层按脏区上传。
;; 键 = (font glyph scale subpx)。装满时置 full?，调用方 atlas-reset! 后重建本帧（整页重建，不做逐个淘汰）。

(require "text-backend.rkt")

(define PAD 1)                                   ; 分配之间的空隙（像素）

(struct entry (u0 v0 u1 v1 w h left top color?) #:transparent)

(struct atlas (size buf entries rasterize
               [shelves #:mutable]               ; (listof (mcons-like vector y h x))，按 y 升序
               [dirty #:mutable]                 ; #f | (vector x0 y0 x1 y1)
               [full? #:mutable]
               [generation #:mutable]))

(define (make-atlas backend #:size [size 2048])
  (atlas size (make-bytes (* size size 4) 0) (make-hash) (text-backend-rasterize backend)
         '() #f #f 0)
) ; end define make-atlas

(define (atlas-reset! at)
  (bytes-fill! (atlas-buf at) 0)
  (hash-clear! (atlas-entries at))
  (set-atlas-shelves! at '())
  (set-atlas-full?! at #f)
  (set-atlas-generation! at (add1 (atlas-generation at)))
  (set-atlas-dirty! at (vector 0 0 (atlas-size at) (atlas-size at)))
) ; end define atlas-reset!

(define (mark-dirty! at x0 y0 x1 y1)
  (define d (atlas-dirty at))
  (set-atlas-dirty! at (if d
                           (vector (min x0 (vector-ref d 0)) (min y0 (vector-ref d 1))
                                   (max x1 (vector-ref d 2)) (max y1 (vector-ref d 3)))
                           (vector x0 y0 x1 y1)))
) ; end define mark-dirty!

;; shelf = (vector y h next-x)。选高度在 [h, 1.3h+2] 内且横向放得下的第一条；否则在底部开新 shelf。
(define (allocate! at w h)
  (define size (atlas-size at))
  (define fit
    (for/first ([s (in-list (atlas-shelves at))]
                #:when (and (<= h (vector-ref s 1)) (<= (vector-ref s 1) (+ 2 (* 1.3 h)))
                            (<= (+ (vector-ref s 2) w) size)))
      s))
  (cond
    [fit
     (define x (vector-ref fit 2))
     (vector-set! fit 2 (+ x w PAD))
     (values x (vector-ref fit 0))]
    [else
     (define top (for/fold ([t 0]) ([s (in-list (atlas-shelves at))]) (max t (+ (vector-ref s 0) (vector-ref s 1) PAD))))
     (cond
       [(or (> (+ top h) size) (> w size)) (values #f #f)]
       [else
        (set-atlas-shelves! at (append (atlas-shelves at) (list (vector top h (+ w PAD)))))
        (values 0 top)])])
) ; end define allocate!

;; → entry | #f（空字形，如空格）。装不下时置 full? 并返回 #f。
(define (atlas-glyph! at font glyph scale subpx)
  (define key (vector font glyph scale subpx))
  (define hit (hash-ref (atlas-entries at) key #f))
  (cond
    [hit (and (entry? hit) hit)]
    [else
     (define r ((atlas-rasterize at) font glyph scale subpx))
     (cond
       [(not r) (hash-set! (atlas-entries at) key 'empty) #f]
       [else
        (define w (vector-ref r 0))
        (define h (vector-ref r 1))
        (define-values (x y) (allocate! at w h))
        (cond
          [(not x) (set-atlas-full?! at #t) #f]
          [else
           (define size (atlas-size at))
           (define src (vector-ref r 4))
           (define dst (atlas-buf at))
           (for ([row (in-range h)])
             (bytes-copy! dst (* 4 (+ (* (+ y row) size) x)) src (* 4 row w) (* 4 (add1 row) w)))
           (mark-dirty! at x y (+ x w) (+ y h))
           (define e (entry (/ x size 1.0) (/ y size 1.0) (/ (+ x w) size 1.0) (/ (+ y h) size 1.0)
                            w h (vector-ref r 2) (vector-ref r 3) (vector-ref r 5)))
           (hash-set! (atlas-entries at) key e)
           e])])])
) ; end define atlas-glyph!

;; 取走脏区：→ #f | (vector x y w h)
(define (atlas-take-dirty! at)
  (define d (atlas-dirty at))
  (set-atlas-dirty! at #f)
  (and d (vector (vector-ref d 0) (vector-ref d 1)
                 (- (vector-ref d 2) (vector-ref d 0)) (- (vector-ref d 3) (vector-ref d 1))))
) ; end define atlas-take-dirty!

(define (atlas-glyph-count at)
  (for/sum ([v (in-hash-values (atlas-entries at))]) (if (entry? v) 1 0))
) ; end define atlas-glyph-count

(provide (struct-out entry) make-atlas atlas? atlas-size atlas-buf atlas-full? atlas-generation
         atlas-reset! atlas-glyph! atlas-take-dirty! atlas-glyph-count)
