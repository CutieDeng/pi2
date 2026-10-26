#lang racket/base
;; image-atlas.rkt — 图片纹理池（P4，纯数据）：把解码好的缩略图（RGBA 预乘）按行架（shelf）打包进一张 size×size 的 CPU 位图，
;; GPU 端一张纹理与之对应（components/mac-renderer.rkt 的 texture(1)），按脏区上传。
;; 放不下 → 记下「下一帧开始时清空」（同一帧里已画的不失效）；清空后看得到的图片再按需放进来。

(struct ia (size bitmap [entries #:mutable] [shelves #:mutable] [dirty #:mutable] [reset? #:mutable] [gen #:mutable]))
;; entries：键 → (vector x y w h)；shelves：(vector y 高 已用宽) 表；dirty：(vector x0 y0 x1 y1) 或 #f

(define (make-image-atlas [size 2048])
  (ia size (make-bytes (* size size 4) 0) (make-hash) '() #f #f 0))

(define (image-atlas-size a) (ia-size a))
(define (image-atlas-bitmap a) (ia-bitmap a))
(define (image-atlas-generation a) (ia-gen a))
(define (image-atlas-count a) (hash-count (ia-entries a)))

;; 帧开始：上一帧有放不下的 → 清空
(define (image-atlas-begin-frame! a)
  (when (ia-reset? a)
    (set-ia-entries! a (make-hash))
    (set-ia-shelves! a '())
    (set-ia-reset?! a #f)
    (set-ia-gen! a (add1 (ia-gen a)))))

;; 已在池里 → (vector u0 v0 u1 v1)
(define (image-atlas-lookup a key)
  (define e (hash-ref (ia-entries a) key #f))
  (and e (uv-of a e)))

(define (uv-of a e)
  (define s (exact->inexact (ia-size a)))
  (vector (/ (vector-ref e 0) s) (/ (vector-ref e 1) s)
          (/ (+ (vector-ref e 0) (vector-ref e 2)) s) (/ (+ (vector-ref e 1) (vector-ref e 3)) s)))

;; 放进来（w×h 像素，rgba 长 w*h*4）→ (vector u0 v0 u1 v1) 或 #f（放不下：下一帧开始时清空）
(define PAD 1)
(define (image-atlas-put! a key w h rgba)
  (or (image-atlas-lookup a key)
      (let ([spot (place! a (+ w PAD) (+ h PAD))])
        (cond
          [(not spot) (set-ia-reset?! a #t) #f]
          [else
           (define x (car spot)) (define y (cdr spot))
           (blit! a x y w h rgba)
           (define e (vector x y w h))
           (hash-set! (ia-entries a) key e)
           (mark-dirty! a x y (+ x w) (+ y h))
           (uv-of a e)]))))

;; 行架：先找高度够、剩余宽度够的架（取最矮的），没有就在最下面开新架
(define (place! a w h)
  (define size (ia-size a))
  (cond
    [(or (> w size) (> h size)) #f]
    [else
     (define fit (for/fold ([best #f]) ([s (in-list (ia-shelves a))])
                   (if (and (>= (vector-ref s 1) h) (<= (+ (vector-ref s 2) w) size)
                            (or (not best) (< (vector-ref s 1) (vector-ref best 1))))
                       s best)))
     (cond
       [fit (define x (vector-ref fit 2)) (vector-set! fit 2 (+ x w)) (cons x (vector-ref fit 0))]
       [else
        (define top (for/fold ([m 0]) ([s (in-list (ia-shelves a))]) (max m (+ (vector-ref s 0) (vector-ref s 1)))))
        (cond [(> (+ top h) size) #f]
              [else (set-ia-shelves! a (cons (vector top h w) (ia-shelves a))) (cons 0 top)])])]))

(define (blit! a x y w h rgba)
  (define size (ia-size a))
  (define bm (ia-bitmap a))
  (for ([row (in-range h)])
    (bytes-copy! bm (* 4 (+ (* (+ y row) size) x)) rgba (* row w 4) (* (add1 row) w 4))))

(define (mark-dirty! a x0 y0 x1 y1)
  (define d (ia-dirty a))
  (set-ia-dirty! a (if d (vector (min x0 (vector-ref d 0)) (min y0 (vector-ref d 1)) (max x1 (vector-ref d 2)) (max y1 (vector-ref d 3)))
                       (vector x0 y0 x1 y1))))

;; 取走脏区：→ (vector x y w h) 或 #f
(define (image-atlas-take-dirty! a)
  (define d (ia-dirty a))
  (set-ia-dirty! a #f)
  (and d (vector (vector-ref d 0) (vector-ref d 1) (- (vector-ref d 2) (vector-ref d 0)) (- (vector-ref d 3) (vector-ref d 1)))))

(provide make-image-atlas image-atlas-size image-atlas-bitmap image-atlas-generation image-atlas-count
         image-atlas-begin-frame! image-atlas-lookup image-atlas-put! image-atlas-take-dirty!)
