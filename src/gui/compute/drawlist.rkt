#lang racket/base
;; drawlist.rkt — 渲染实例缓冲区（L0，纯数据）。一帧的矩形与字形按绘制顺序写进同一块字节，
;; 平台层一次上传、一次实例化 draw（S10），绘制顺序即实例顺序。
;; 实例布局（与 platform/macos/metal.rkt 的 MSL `struct Inst` 一致，小端 f32，64 字节）：
;;   [0..3]   x y w h          point，原点左上
;;   [4..7]   r g b a          非预乘，0..1
;;   [8..11]  u0 v0 u1 v1      atlas 归一化纹理坐标（矩形不用）
;;   [12..15] radius kind 0 0  kind：0 圆角矩形（SDF）、1 单色字形（atlas alpha × 颜色）、2 彩色字形（atlas RGBA × a）、
;;                              3 图片（图片纹理池 RGBA 预乘 × a，线性采样，圆角裁切）
;;                              4 挖洞：填外框以内、圆角矩形以外（四个角）——主层在液态玻璃面板下面留出与玻璃同形的透明洞

(define INSTANCE-FLOATS 16)
(define INSTANCE-BYTES (* 4 INSTANCE-FLOATS))

(define KIND-RECT 0)
(define KIND-MASK-GLYPH 1)
(define KIND-COLOR-GLYPH 2)
(define KIND-IMAGE 3)
(define KIND-HOLE 4)

;; clips：裁剪切换点，逆序 (cons 起始实例下标 矩形或#f)；矩形 = (vector x y w h)，point
(struct drawlist ([buf #:mutable] [count #:mutable] [clips #:mutable]))

(define (make-drawlist [capacity 256])
  (drawlist (make-bytes (* capacity INSTANCE-BYTES) 0) 0 '())
) ; end define make-drawlist

(define (drawlist-clear! dl) (set-drawlist-count! dl 0) (set-drawlist-clips! dl '()))

;; 此后写入的实例裁剪到 rect（point，原点左上）；#f = 不裁剪。渲染时每段一次 draw + scissor。
(define (drawlist-set-clip! dl rect)
  (set-drawlist-clips! dl (cons (cons (drawlist-count dl) rect) (drawlist-clips dl)))
) ; end define drawlist-set-clip!

;; → (listof (vector 起始 数量 rect-或-#f))，按绘制顺序，空段省略
(define (drawlist-segments dl)
  (define n (drawlist-count dl))
  (define points (reverse (drawlist-clips dl)))
  (define starts (cons (cons 0 #f) points))
  (for/list ([p (in-list starts)]
             [next (in-list (append (map car points) (list n)))]
             #:when (> next (car p)))
    (vector (car p) (- next (car p)) (cdr p)))
) ; end define drawlist-segments

(define (ensure-capacity! dl n)
  (define buf (drawlist-buf dl))
  (when (> (* n INSTANCE-BYTES) (bytes-length buf))
    (define nb (make-bytes (* 2 (max (bytes-length buf) (* n INSTANCE-BYTES))) 0))
    (bytes-copy! nb 0 buf 0 (* (drawlist-count dl) INSTANCE-BYTES))
    (set-drawlist-buf! dl nb))
) ; end define ensure-capacity!

(define (put-instance! dl x y w h r g b a u0 v0 u1 v1 radius kind)
  (define i (drawlist-count dl))
  (ensure-capacity! dl (add1 i))
  (define buf (drawlist-buf dl))
  (define base (* i INSTANCE-BYTES))
  (define (f! k v) (real->floating-point-bytes v 4 #f buf (+ base (* 4 k))))
  (f! 0 x) (f! 1 y) (f! 2 w) (f! 3 h)
  (f! 4 r) (f! 5 g) (f! 6 b) (f! 7 a)
  (f! 8 u0) (f! 9 v0) (f! 10 u1) (f! 11 v1)
  (f! 12 radius) (f! 13 kind) (f! 14 0.0) (f! 15 0.0)
  (set-drawlist-count! dl (add1 i))
) ; end define put-instance!

(define (drawlist-rect! dl x y w h r g b a #:radius [radius 0.0])
  (put-instance! dl x y w h r g b a 0.0 0.0 0.0 0.0 radius KIND-RECT)
) ; end define drawlist-rect!

;; 图片：uv 为图片纹理池（compute/image-atlas.rkt）的归一化坐标；alpha 乘到像素上；radius 圆角
(define (drawlist-image! dl x y w h u0 v0 u1 v1 #:alpha [a 1.0] #:radius [radius 0.0])
  (put-instance! dl x y w h 1.0 1.0 1.0 a u0 v0 u1 v1 radius KIND-IMAGE)
) ; end define drawlist-image!

;; 字形四边形：x y w h 为 point（已对齐到像素网格），uv 为 atlas 归一化坐标。
;; 挖洞：外框 x y w h 以内、圆角 radius 的圆角矩形以外填色（外框四周另用矩形填，就得到一块只有圆角洞的底）
(define (drawlist-hole! dl x y w h r g b a #:radius radius)
  (put-instance! dl x y w h r g b a 0.0 0.0 0.0 0.0 radius KIND-HOLE)
) ; end define drawlist-hole!

(define (drawlist-glyph! dl x y w h u0 v0 u1 v1 r g b a #:color-glyph? [color? #f])
  (put-instance! dl x y w h r g b a u0 v0 u1 v1 0.0 (if color? KIND-COLOR-GLYPH KIND-MASK-GLYPH))
) ; end define drawlist-glyph!

(define (drawlist-byte-length dl) (* (drawlist-count dl) INSTANCE-BYTES))

;; 把 src 的实例接在 dst 后面（src 从不裁剪开始，它的裁剪切换点平移过来）；之后 dst 回到不裁剪
(define (drawlist-append! dst src)
  (define base (drawlist-count dst))
  (define n (drawlist-count src))
  (ensure-capacity! dst (+ base n))
  (bytes-copy! (drawlist-buf dst) (* base INSTANCE-BYTES) (drawlist-buf src) 0 (* n INSTANCE-BYTES))
  (set-drawlist-clips! dst (append (for/list ([c (in-list (drawlist-clips src))]) (cons (+ base (car c)) (cdr c)))
                                   (cons (cons base #f) (drawlist-clips dst))))
  (set-drawlist-count! dst (+ base n))
  (drawlist-set-clip! dst #f)
) ; end define drawlist-append!

;; 读回第 i 个实例（测试 / 调试用）
(define (drawlist-ref dl i)
  (define base (* i INSTANCE-BYTES))
  (for/list ([k (in-range INSTANCE-FLOATS)])
    (floating-point-bytes->real (drawlist-buf dl) #f (+ base (* 4 k)) (+ base (* 4 k) 4)))
) ; end define drawlist-ref

(provide
 INSTANCE-BYTES KIND-RECT KIND-MASK-GLYPH KIND-COLOR-GLYPH KIND-IMAGE KIND-HOLE drawlist-image! drawlist-hole!
 make-drawlist drawlist? drawlist-buf drawlist-count
 drawlist-clear! drawlist-rect! drawlist-glyph! drawlist-byte-length drawlist-ref
 drawlist-set-clip! drawlist-segments drawlist-append!
) ; end provide
