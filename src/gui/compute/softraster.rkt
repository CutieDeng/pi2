#lang racket/base
;; softraster.rkt — drawlist 的参考光栅器（GUI 元数据平面 4：图元契约的可执行定义）。纯计算，任何平台都能跑。
;;   与 macOS 渲染器（components/mac-renderer.rkt 的 MSL）逐项同义：
;;     顶点：实例矩形 (x y w h) point × scale → 像素；片元在像素中心求值
;;     kind 0 圆角矩形：SDF d = length(max(q,0)) + min(max(qx,qy),0) − r，q = |local| − half + r，r = min(radius, half)；
;;            覆盖 = clamp(0.5 − d / fwidth(d), 0, 1)；输出预乘 (rgb·a, a)，a = color.a × 覆盖
;;     kind 1 单色字形：atlas 最近邻采样 × color（a = 纹素 a × color.a）
;;     kind 2 彩色字形：atlas 纹素（预乘）× color.a
;;     kind 3 图片：图片池双线性采样（预乘）× color.a × 圆角覆盖
;;     kind 4 挖洞：a = color.a × (1 − 覆盖)（框内、圆角外）
;;     混合：dst = src + dst × (1 − src.a)（预乘 over）；目标 BGRA8；裁剪段 → scissor = floor / ceil(裁剪矩形 × scale)
;;   fwidth 用相邻像素的前向差分（硬件按 2×2 像素块求导，结果可差一个量化级）。
;;   用途：Windows（或任何新平台）渲染器的对照——同一个 drawlist，两边出的像素按 conformance 阈值比；无 GPU 时的显示检查。

(require racket/flonum racket/math "drawlist.rkt")

(define (clampf v lo hi) (flmax lo (flmin hi v)))

;; dl → BGRA bytes（pw × ph × 4，行 0 = 顶部）。atlas：(cons RGBA bytes 边长) | #f；images：同形 | #f；clear：(r g b a)
(define (rasterize-drawlist dl pw ph scale clear #:atlas [atlas #f] #:images [images #f])
  (define n (* pw ph))
  (define fb (make-flvector (* 4 n) 0.0))
  (for ([i (in-range n)])
    (flvector-set! fb (* 4 i) (exact->inexact (list-ref clear 0))) (flvector-set! fb (+ 1 (* 4 i)) (exact->inexact (list-ref clear 1)))
    (flvector-set! fb (+ 2 (* 4 i)) (exact->inexact (list-ref clear 2))) (flvector-set! fb (+ 3 (* 4 i)) (exact->inexact (list-ref clear 3))))
  (define sc (exact->inexact scale))
  (for ([sg (in-list (drawlist-segments dl))])
    (define r (vector-ref sg 2))
    (define-values (cx0 cy0 cx1 cy1)
      (if r
          (let* ([x0 (max 0 (min pw (exact-floor (* sc (vector-ref r 0)))))]
                 [y0 (max 0 (min ph (exact-floor (* sc (vector-ref r 1)))))]
                 [x1 (max x0 (min pw (exact-ceiling (* sc (+ (vector-ref r 0) (vector-ref r 2))))))]
                 [y1 (max y0 (min ph (exact-ceiling (* sc (+ (vector-ref r 1) (vector-ref r 3))))))])
            (values x0 y0 x1 y1))
          (values 0 0 pw ph)))
    (for ([i (in-range (vector-ref sg 0) (+ (vector-ref sg 0) (vector-ref sg 1)))])
      (draw-instance! fb pw (drawlist-ref dl i) sc cx0 cy0 cx1 cy1 atlas images)))
  ;; float RGBA → BGRA8
  (define out (make-bytes (* 4 n)))
  (define (q v) (exact-round (* 255.0 (clampf v 0.0 1.0))))
  (for ([i (in-range n)])
    (define b (* 4 i))
    (bytes-set! out b (q (flvector-ref fb (+ b 2))))
    (bytes-set! out (+ b 1) (q (flvector-ref fb (+ b 1))))
    (bytes-set! out (+ b 2) (q (flvector-ref fb b)))
    (bytes-set! out (+ b 3) (q (flvector-ref fb (+ b 3)))))
  out)

;; 圆角矩形的 SDF（point）
(define (sdf lx ly hw hh rad)
  (define qx (fl+ (fl- (flabs lx) hw) rad))
  (define qy (fl+ (fl- (flabs ly) hh) rad))
  (fl+ (fl- (flsqrt (fl+ (fl* (flmax qx 0.0) (flmax qx 0.0)) (fl* (flmax qy 0.0) (flmax qy 0.0)))) rad)
       (flmin (flmax qx qy) 0.0)))

(define (texel tex u v)                                   ; 最近邻，RGBA 0..1
  (define bs (car tex)) (define size (cdr tex))
  (define tx (max 0 (min (sub1 size) (exact-floor (* u size)))))
  (define ty (max 0 (min (sub1 size) (exact-floor (* v size)))))
  (define o (* 4 (+ tx (* ty size))))
  (values (/ (bytes-ref bs o) 255.0) (/ (bytes-ref bs (+ o 1)) 255.0) (/ (bytes-ref bs (+ o 2)) 255.0) (/ (bytes-ref bs (+ o 3)) 255.0)))

(define (bilinear tex u v)
  (define size (cdr tex))
  (define fx (- (* u size) 0.5)) (define fy (- (* v size) 0.5))
  (define x0 (exact-floor fx)) (define y0 (exact-floor fy))
  (define ax (- fx x0)) (define ay (- fy y0))
  (define (at x y) (texel tex (/ (+ (max 0 (min (sub1 size) x)) 0.5) size) (/ (+ (max 0 (min (sub1 size) y)) 0.5) size)))
  (define-values (r00 g00 b00 a00) (at x0 y0)) (define-values (r10 g10 b10 a10) (at (add1 x0) y0))
  (define-values (r01 g01 b01 a01) (at x0 (add1 y0))) (define-values (r11 g11 b11 a11) (at (add1 x0) (add1 y0)))
  (define (mix a b c d) (+ (* (- 1 ax) (- 1 ay) a) (* ax (- 1 ay) b) (* (- 1 ax) ay c) (* ax ay d)))
  (values (mix r00 r10 r01 r11) (mix g00 g10 g01 g11) (mix b00 b10 b01 b11) (mix a00 a10 a01 a11)))

(define (draw-instance! fb pw inst sc cx0 cy0 cx1 cy1 atlas images)
  ;; drawlist-ref → (list x y w h r g b a u0 v0 u1 v1 radius kind …)
  (define-values (x y w h cr cg cb ca u0 v0 u1 v1 radius kind)
    (apply values (map exact->inexact (for/list ([k 14]) (list-ref inst k)))))
  (define hw (fl/ w 2.0)) (define hh (fl/ h 2.0))
  (define rad (flmin radius (flmin hw hh)))
  (define px0 (max cx0 (exact-ceiling (- (* x sc) 0.5)))) (define px1 (min cx1 (exact-ceiling (- (* (+ x w) sc) 0.5))))
  (define py0 (max cy0 (exact-ceiling (- (* y sc) 0.5)))) (define py1 (min cy1 (exact-ceiling (- (* (+ y h) sc) 0.5))))
  (define k (exact-round kind))
  (for* ([py (in-range py0 py1)] [px (in-range px0 px1)])
    (define lx (fl- (fl/ (fl+ (->fl px) 0.5) sc) (fl+ x hw)))
    (define ly (fl- (fl/ (fl+ (->fl py) 0.5) sc) (fl+ y hh)))
    (define (cover)
      (define d (sdf lx ly hw hh rad))
      (define fw (fl+ (flabs (fl- (sdf (fl+ lx (fl/ 1.0 sc)) ly hw hh rad) d)) (flabs (fl- (sdf lx (fl+ ly (fl/ 1.0 sc)) hw hh rad) d))))
      (clampf (fl- 0.5 (fl/ d (flmax fw 1e-4))) 0.0 1.0))
    (define-values (sr sg sb sa)
      (case k
        [(0) (define a (fl* ca (cover))) (values (fl* cr a) (fl* cg a) (fl* cb a) a)]
        [(4) (define a (fl* ca (fl- 1.0 (cover)))) (values (fl* cr a) (fl* cg a) (fl* cb a) a)]
        [(1 2)
         (cond
           [(not atlas) (values 0.0 0.0 0.0 0.0)]
           [else
            (define tu (fl+ u0 (fl* (fl- u1 u0) (fl+ (fl/ lx w) 0.5))))
            (define tv (fl+ v0 (fl* (fl- v1 v0) (fl+ (fl/ ly h) 0.5))))
            (define-values (tr tg tb ta) (texel atlas tu tv))
            (if (= k 1)
                (let ([a (* ta ca)]) (values (* cr a) (* cg a) (* cb a) a))
                (values (* tr ca) (* tg ca) (* tb ca) (* ta ca)))])]
        [(3)
         (cond
           [(not images) (values 0.0 0.0 0.0 0.0)]
           [else
            (define tu (fl+ u0 (fl* (fl- u1 u0) (fl+ (fl/ lx w) 0.5))))
            (define tv (fl+ v0 (fl* (fl- v1 v0) (fl+ (fl/ ly h) 0.5))))
            (define-values (tr tg tb ta) (bilinear images tu tv))
            (define m (* ca (cover)))
            (values (* tr m) (* tg m) (* tb m) (* ta m))])]
        [else (values 0.0 0.0 0.0 0.0)]))
    (define o (* 4 (+ px (* py pw))))
    (define inv (fl- 1.0 (real->double-flonum sa)))
    (flvector-set! fb o (fl+ (real->double-flonum sr) (fl* (flvector-ref fb o) inv)))
    (flvector-set! fb (+ o 1) (fl+ (real->double-flonum sg) (fl* (flvector-ref fb (+ o 1)) inv)))
    (flvector-set! fb (+ o 2) (fl+ (real->double-flonum sb) (fl* (flvector-ref fb (+ o 2)) inv)))
    (flvector-set! fb (+ o 3) (fl+ (real->double-flonum sa) (fl* (flvector-ref fb (+ o 3)) inv)))))

;; 两张 BGRA 图的差：→ (values 最大通道差 超过阈值的像素比例)
(define (image-diff a b threshold)
  (define n (quotient (bytes-length a) 4))
  (for/fold ([mx 0] [over 0] #:result (values mx (/ over (max 1 n) 1.0))) ([i (in-range n)])
    (define d (for/fold ([m 0]) ([c 4]) (max m (abs (- (bytes-ref a (+ (* 4 i) c)) (bytes-ref b (+ (* 4 i) c)))))))
    (values (max mx d) (if (> d threshold) (add1 over) over))))

(provide rasterize-drawlist image-diff)
