#lang racket/base
;; metal.rkt — GPU 后端（Metal）。一帧 = 一个实例缓冲区 + 一次实例化 draw（S10）。
;; 着色器为自写 MSL，运行时编译（P11：约 100ms）。只在有变化时渲染（P3）。

(require ffi/unsafe racket/math "../compute/mac-objc.rkt" "../compute/mac-cf.rkt" "../compute/mac-geom.rkt" "../compute/drawlist.rkt"
         "../compute/hub.rkt" "../compute/interfaces.rkt")

(define libmetal (ffi-lib "/System/Library/Frameworks/Metal.framework/Metal"))
(define _libquartz (ffi-lib "/System/Library/Frameworks/QuartzCore.framework/QuartzCore")) ; CAMetalLayer
(define MTLCreateSystemDefaultDevice (get-ffi-obj 'MTLCreateSystemDefaultDevice libmetal (_fun -> _id)))

(define-cstruct _MTLClearColor ([r _double] [g _double] [b _double] [a _double]))
(define-cstruct _MTLScissorRect ([x _uint64] [y _uint64] [w _uint64] [h _uint64]))
(define-cstruct _MTLRegion ([x _uint64] [y _uint64] [z _uint64] [w _uint64] [h _uint64] [d _uint64]))

(define PIXEL-BGRA8 80)
(define PIXEL-RGBA8 70)
(define LOAD-CLEAR 2)
(define STORE-STORE 1)
(define PRIM-TRIANGLE 3)
(define BLEND-ONE 1)
(define BLEND-ONE-MINUS-SRC-ALPHA 5)
(define USAGE-SHADER-READ 1)
(define USAGE-RENDER-TARGET 4)
(define STORAGE-SHARED 0)

;; ---------------------------------------------------------------- 消息

(define-msg m-name "name" _id ())
(define-msg m-new-queue "newCommandQueue" _id ())
(define-msg m-new-library "newLibraryWithSource:options:error:" _id ([src _id] [opts _id] [err _pointer]))
(define-msg m-new-function "newFunctionWithName:" _id ([n _id]))
(define-msg m-localized-description "localizedDescription" _id ())
(define-msg m-set-vertex-fn "setVertexFunction:" _void ([f _id]))
(define-msg m-set-fragment-fn "setFragmentFunction:" _void ([f _id]))
(define-msg m-color-attachments "colorAttachments" _id ())
(define-msg m-at-index "objectAtIndexedSubscript:" _id ([i _uint64]))
(define-msg m-set-pixel-format "setPixelFormat:" _void ([f _uint64]))
(define-msg m-set-blending "setBlendingEnabled:" _void ([on _BOOL]))
(define-msg m-set-src-rgb "setSourceRGBBlendFactor:" _void ([f _uint64]))
(define-msg m-set-dst-rgb "setDestinationRGBBlendFactor:" _void ([f _uint64]))
(define-msg m-set-src-a "setSourceAlphaBlendFactor:" _void ([f _uint64]))
(define-msg m-set-dst-a "setDestinationAlphaBlendFactor:" _void ([f _uint64]))
(define-msg m-new-pipeline "newRenderPipelineStateWithDescriptor:error:" _id ([d _id] [err _pointer]))
(define-msg m-layer "layer" _id ())
(define-msg m-set-device "setDevice:" _void ([d _id]))
(define-msg m-set-contents-scale "setContentsScale:" _void ([s _double]))
(define-msg m-set-drawable-size "setDrawableSize:" _void ([s _CGSize]))
(define-msg m-next-drawable "nextDrawable" _id ())
(define-msg m-texture "texture" _id ())
(define-msg m-render-pass-descriptor "renderPassDescriptor" _id ())
(define-msg m-set-texture "setTexture:" _void ([t _id]))
(define-msg m-set-load-action "setLoadAction:" _void ([a _uint64]))
(define-msg m-set-store-action "setStoreAction:" _void ([a _uint64]))
(define-msg m-set-clear-color "setClearColor:" _void ([c _MTLClearColor]))
(define-msg m-command-buffer "commandBuffer" _id ())
(define-msg m-render-encoder "renderCommandEncoderWithDescriptor:" _id ([d _id]))
(define-msg m-set-pipeline "setRenderPipelineState:" _void ([p _id]))
(define-msg m-set-vertex-buffer "setVertexBuffer:offset:atIndex:" _void ([b _id] [off _uint64] [i _uint64]))
(define-msg m-set-vertex-bytes "setVertexBytes:length:atIndex:" _void ([p _bytes] [n _uint64] [i _uint64]))
(define-msg m-draw-instanced "drawPrimitives:vertexStart:vertexCount:instanceCount:" _void
  ([t _uint64] [start _uint64] [n _uint64] [k _uint64]))
(define-msg m-draw-instanced-base "drawPrimitives:vertexStart:vertexCount:instanceCount:baseInstance:" _void
  ([t _uint64] [start _uint64] [n _uint64] [k _uint64] [base _uint64]))
(define-msg m-set-scissor "setScissorRect:" _void ([r _MTLScissorRect]))
(define-msg m-end-encoding "endEncoding" _void ())
(define-msg m-present "presentDrawable:" _void ([d _id]))
(define-msg m-commit "commit" _void ())
(define-msg m-wait "waitUntilCompleted" _void ())
(define-msg m-new-buffer "newBufferWithBytes:length:options:" _id ([p _bytes] [n _uint64] [o _uint64]))
(define-msg m-tex2d-desc "texture2DDescriptorWithPixelFormat:width:height:mipmapped:" _id
  ([f _uint64] [w _uint64] [h _uint64] [mip _BOOL]))
(define-msg m-set-usage "setUsage:" _void ([u _uint64]))
(define-msg m-set-storage-mode "setStorageMode:" _void ([m _uint64]))
(define-msg m-new-texture "newTextureWithDescriptor:" _id ([d _id]))
(define-msg m-replace-region "replaceRegion:mipmapLevel:withBytes:bytesPerRow:" _void
  ([r _MTLRegion] [lvl _uint64] [p _pointer] [row-bytes _uint64]))
(define-msg m-set-fragment-texture "setFragmentTexture:atIndex:" _void ([t _id] [i _uint64]))
(define-msg m-get-bytes "getBytes:bytesPerRow:fromRegion:mipmapLevel:" _void
  ([p _pointer] [row-bytes _uint64] [r _MTLRegion] [lvl _uint64]))

;; ---------------------------------------------------------------- 着色器

;; struct Inst 与 core/drawlist.rkt 的实例布局一致（64 字节）。kind：0 圆角矩形（SDF，fwidth 抗锯齿）、
;; 1 单色字形（atlas alpha × 颜色）、2 彩色字形（atlas 预乘 RGBA × 颜色 alpha）、3 图片、4 挖洞（外框内、圆角矩形外）。
;; 字形四边形与像素一一对应 → 最近邻采样。
;; 输出预乘 alpha，管线混合 = one / oneMinusSrcAlpha。
(define MSL #<<EOF
#include <metal_stdlib>
using namespace metal;
struct Inst { float4 xywh; float4 color; float4 uv; float4 params; };
struct VOut { float4 pos [[position]]; float4 color; float2 local; float2 half_size; float2 uv;
              float radius; float kind; };
vertex VOut inst_v(uint vid [[vertex_id]], uint iid [[instance_id]],
                   const device Inst* insts [[buffer(0)]], constant float2& viewport [[buffer(1)]]) {
  const float2 corners[6] = { float2(0,0), float2(1,0), float2(0,1), float2(1,0), float2(1,1), float2(0,1) };
  Inst r = insts[iid];
  float2 t = corners[vid];
  float2 p = r.xywh.xy + t * r.xywh.zw;
  VOut o;
  o.pos = float4(p.x / viewport.x * 2.0 - 1.0, 1.0 - p.y / viewport.y * 2.0, 0.0, 1.0);
  o.color = r.color;
  o.half_size = r.xywh.zw * 0.5;
  o.local = (t - 0.5) * r.xywh.zw;
  o.uv = mix(r.uv.xy, r.uv.zw, t);
  o.radius = min(r.params.x, min(o.half_size.x, o.half_size.y));
  o.kind = r.params.y;
  return o;
}
fragment float4 inst_f(VOut in [[stage_in]], texture2d<float> atlas [[texture(0)]], texture2d<float> images [[texture(1)]]) {
  constexpr sampler nearest(filter::nearest, address::clamp_to_edge);
  constexpr sampler smooth(filter::linear, address::clamp_to_edge);
  if (in.kind > 3.5) {
    float2 q = abs(in.local) - in.half_size + in.radius;
    float d = length(max(q, 0.0)) + min(max(q.x, q.y), 0.0) - in.radius;
    float a = in.color.a * (1.0 - clamp(0.5 - d / max(fwidth(d), 1e-4), 0.0, 1.0));
    return float4(in.color.rgb * a, a);
  }
  if (in.kind > 2.5) {
    float2 q = abs(in.local) - in.half_size + in.radius;
    float d = length(max(q, 0.0)) + min(max(q.x, q.y), 0.0) - in.radius;
    float cover = clamp(0.5 - d / max(fwidth(d), 1e-4), 0.0, 1.0);
    return images.sample(smooth, in.uv) * (in.color.a * cover);
  }
  if (in.kind < 0.5) {
    float2 q = abs(in.local) - in.half_size + in.radius;
    float d = length(max(q, 0.0)) + min(max(q.x, q.y), 0.0) - in.radius;
    float cover = clamp(0.5 - d / max(fwidth(d), 1e-4), 0.0, 1.0);
    float a = in.color.a * cover;
    return float4(in.color.rgb * a, a);
  }
  float4 t = atlas.sample(nearest, in.uv);
  if (in.kind < 1.5) {
    float a = t.a * in.color.a;
    return float4(in.color.rgb * a, a);
  }
  return t * in.color.a;
}
EOF
  )

;; ---------------------------------------------------------------- 设备 / 管线

(struct gpu (device queue pipeline layer uniform atlas atlas-size images images-size [overlay #:mutable]))   ; overlay：第二张透明层（液态玻璃上的侧栏）或 #f

(define (call-with-error-out who proc)
  (define errp (malloc _pointer 'raw))
  (ptr-set! errp _pointer #f)
  (define r (proc errp))
  (define e (ptr-ref errp _pointer))
  (free errp)
  (or r (error who "~a" (if e (cfstring->string (m-localized-description e)) "unknown error")))
) ; end define call-with-error-out

(define (make-pipeline device library)
  (define desc (alloc-init "MTLRenderPipelineDescriptor"))
  (call-with-cfstring "inst_v" (lambda (n) (m-set-vertex-fn desc (m-new-function library n))))
  (call-with-cfstring "inst_f" (lambda (n) (m-set-fragment-fn desc (m-new-function library n))))
  (define ca (m-at-index (m-color-attachments desc) 0))
  (m-set-pixel-format ca PIXEL-BGRA8)
  (m-set-blending ca #t)
  (m-set-src-rgb ca BLEND-ONE)
  (m-set-dst-rgb ca BLEND-ONE-MINUS-SRC-ALPHA)
  (m-set-src-a ca BLEND-ONE)
  (m-set-dst-a ca BLEND-ONE-MINUS-SRC-ALPHA)
  (begin0 (call-with-error-out 'gpu-pipeline (lambda (errp) (m-new-pipeline device desc errp)))
    (release! desc))
) ; end define make-pipeline

;; #:layer? #t → 同时建 CAMetalLayer（窗口用）；#f → 仅离屏（--selftest）。
;; atlas：RGBA8 纹理，与 render/atlas.rkt 的 CPU 位图同尺寸，按脏区上传。
(define (gpu-open #:layer? [layer? #t] #:atlas-size [atlas-size 2048])
  (call-with-autorelease-pool
   (lambda ()
     (define device (or (MTLCreateSystemDefaultDevice) (error 'gpu-open "no Metal device")))
     (define library
       (call-with-cfstring MSL
         (lambda (src) (call-with-error-out 'gpu-msl (lambda (errp) (m-new-library device src #f errp))))))
     (define pipeline (make-pipeline device library))
     (define layer
       (and layer?
            (let ([l (retain! (m-layer (cls "CAMetalLayer")))])
              (m-set-device l device)
              (m-set-pixel-format l PIXEL-BGRA8)
              l)))
     (define atlas-desc (m-tex2d-desc (cls "MTLTextureDescriptor") PIXEL-RGBA8 atlas-size atlas-size #f))
     (m-set-usage atlas-desc USAGE-SHADER-READ)
     (m-set-storage-mode atlas-desc STORAGE-SHARED)
     ;; 图片纹理池（compute/image-atlas.rkt 的 CPU 位图，同尺寸，按脏区上传）
     (define images-size 2048)
     (define images-desc (m-tex2d-desc (cls "MTLTextureDescriptor") PIXEL-RGBA8 images-size images-size #f))
     (m-set-usage images-desc USAGE-SHADER-READ)
     (m-set-storage-mode images-desc STORAGE-SHARED)
     (gpu device (m-new-queue device) pipeline layer (make-bytes 8 0)
          (m-new-texture device atlas-desc) atlas-size (m-new-texture device images-desc) images-size #f)))
) ; end define gpu-open

(define (gpu-device-name g)
  (call-with-autorelease-pool (lambda () (cfstring->string (m-name (gpu-device g)))))
) ; end define gpu-device-name

;; w h：内容区 point；scale：backing scale。drawable 按像素给。
(define (gpu-resize! g w h scale)
  (for ([l (list (gpu-layer g) (gpu-overlay g))] #:when l)
    (m-set-contents-scale l scale)
    (m-set-drawable-size l (make-CGSize (* w scale) (* h scale))))
) ; end define gpu-resize!

;; 第二张层（透明）：画在液态玻璃上面的内容（侧栏）。懒建；同一个 device、管线、纹理
(define-msg m-set-opaque "setOpaque:" _void ([b _BOOL]))
(define (gpu-overlay-layer! g)
  (or (gpu-overlay g)
      (let ([l (retain! (m-layer (cls "CAMetalLayer")))])
        (m-set-device l (gpu-device g))
        (m-set-pixel-format l PIXEL-BGRA8)
        (m-set-opaque l #f)
        (set-gpu-overlay! g l)
        l))
) ; end define gpu-overlay-layer!

;; ---------------------------------------------------------------- atlas 上传

;; bytes：CPU 侧 atlas 位图（size×size×4，RGBA 预乘）；rect：(vector x y w h) 脏区。
;; withBytes 指向脏区左上角，bytesPerRow 取整行跨度。
(define (gpu-atlas-upload! g bytes rect)
  (define size (gpu-atlas-size g))
  (unless (= (bytes-length bytes) (* size size 4))
    (error 'gpu-atlas-upload! "atlas bytes size mismatch: ~a vs ~a" (bytes-length bytes) (* size size 4)))
  (define x (vector-ref rect 0))
  (define y (vector-ref rect 1))
  (define w (vector-ref rect 2))
  (define h (vector-ref rect 3))
  (unless (and (<= 0 x) (<= 0 y) (< 0 w) (< 0 h) (<= (+ x w) size) (<= (+ y h) size))
    (error 'gpu-atlas-upload! "dirty rect out of bounds: ~a" rect))
  (m-replace-region (gpu-atlas g) (make-MTLRegion x y 0 w h 1) 0
                    (ptr-add bytes (* 4 (+ (* y size) x))) (* 4 size))
) ; end define gpu-atlas-upload!

;; 图片纹理池上传：bytes = size×size×4 RGBA 预乘；rect = (vector x y w h)
(define (gpu-images-upload! g bytes rect)
  (define size (gpu-images-size g))
  (unless (= (bytes-length bytes) (* size size 4))
    (error 'gpu-images-upload! "image atlas size mismatch: ~a vs ~a" (bytes-length bytes) (* size size 4)))
  (define x (vector-ref rect 0)) (define y (vector-ref rect 1)) (define w (vector-ref rect 2)) (define h (vector-ref rect 3))
  (unless (and (<= 0 x) (<= 0 y) (< 0 w) (< 0 h) (<= (+ x w) size) (<= (+ y h) size))
    (error 'gpu-images-upload! "dirty rect out of bounds: ~a" rect))
  (m-replace-region (gpu-images g) (make-MTLRegion x y 0 w h 1) 0 (ptr-add bytes (* 4 (+ (* y size) x))) (* 4 size))
) ; end define gpu-images-upload!

;; ---------------------------------------------------------------- 编码一帧

;; 诊断 / 测试钩子：encoder 建好之后调用 (hook encoder)，用于注入故障。默认 #f。
(define current-encode-fault-hook (make-parameter #f))

;; 返回 (values command-buffer instance-buffer-or-#f)。viewport 单位是 point。
(define (encode! g target dl clear vw vh scale)
  (define rpd (m-render-pass-descriptor (cls "MTLRenderPassDescriptor")))
  (define ca (m-at-index (m-color-attachments rpd) 0))
  (m-set-texture ca target)
  (m-set-load-action ca LOAD-CLEAR)
  (m-set-store-action ca STORE-STORE)
  (m-set-clear-color ca (apply make-MTLClearColor clear))
  (define n (drawlist-count dl))
  (define buf (and (> n 0) (m-new-buffer (gpu-device g) (drawlist-buf dl) (drawlist-byte-length dl)
                                         STORAGE-SHARED)))
  (define cb (m-command-buffer (gpu-queue g)))
  (define enc (m-render-encoder cb rpd))
  (define ok? #f)
  ;; encoder 守卫（P8）：encoder 是 autoreleased 对象，若未 endEncoding 就随 pool 释放，
  ;; Metal 在 -[_MTLCommandEncoder dealloc] 里断言失败并 abort()。任何退出路径（异常 / 续延跳出）
  ;; 都先 endEncoding；失败路径上 command buffer 不 commit，实例缓冲区在这里释放。
  (dynamic-wind
   void
   (lambda ()
     (let ([hook (current-encode-fault-hook)]) (when hook (hook enc)))
     (when buf
       (define uni (gpu-uniform g))
       (real->floating-point-bytes vw 4 #f uni 0)
       (real->floating-point-bytes vh 4 #f uni 4)
       (m-set-pipeline enc (gpu-pipeline g))
       (m-set-fragment-texture enc (gpu-atlas g) 0)
       (m-set-fragment-texture enc (gpu-images g) 1)
       (m-set-vertex-buffer enc buf 0 0)
       (m-set-vertex-bytes enc uni 8 1)
       ;; 每个裁剪段一次 draw（baseInstance 定位段起点；shader 的 instance_id 含 base）
       (define tw (exact-round (* vw scale)))
       (define th (exact-round (* vh scale)))
       (for ([sg (in-list (drawlist-segments dl))])
         (define r (vector-ref sg 2))
         (define sc
           (if r
               (let* ([x0 (max 0 (min tw (exact-floor (* scale (vector-ref r 0)))))]
                      [y0 (max 0 (min th (exact-floor (* scale (vector-ref r 1)))))]
                      [x1 (max x0 (min tw (exact-ceiling (* scale (+ (vector-ref r 0) (vector-ref r 2))))))]
                      [y1 (max y0 (min th (exact-ceiling (* scale (+ (vector-ref r 1) (vector-ref r 3))))))])
                 (make-MTLScissorRect x0 y0 (- x1 x0) (- y1 y0)))
               (make-MTLScissorRect 0 0 tw th)))
         (when (and (> (MTLScissorRect-w sc) 0) (> (MTLScissorRect-h sc) 0))
           (m-set-scissor enc sc)
           (m-draw-instanced-base enc PRIM-TRIANGLE 0 6 (vector-ref sg 1) (vector-ref sg 0)))))
     (set! ok? #t))
   (lambda ()
     (m-end-encoding enc)
     (unless ok? (release! buf))))
  (values cb buf)
) ; end define encode!

;; 渲染到窗口。拿不到 drawable 时返回 #f（不阻塞重试）。
;; 实例缓冲区在 commit 后立即 release：command buffer 持有它直到 GPU 完成（P7）。
(define (gpu-render! g dl clear vw vh #:scale [scale 1.0] #:layer [layer (gpu-layer g)])
  (parameterize-break #f                 ; exn:break 推迟到整帧编码 + commit 之后
  (call-with-autorelease-pool
   (lambda ()
     (define drawable (m-next-drawable layer))
     (cond
       [(not drawable) #f]
       [else
        (define-values (cb buf) (encode! g (m-texture drawable) dl clear vw vh scale))
        (m-present cb drawable)
        (m-commit cb)
        (release! buf)
        #t]))))
) ; end define gpu-render!

;; 离屏渲染并读回 BGRA 像素（P10：窗口画面无法截图，正确性靠这里验证）。
;; 返回 bytes，长度 pw*ph*4，行 0 = 顶部。
(define (gpu-render-offscreen g dl clear pw ph #:scale [scale 1.0])
  (parameterize-break #f
  (call-with-autorelease-pool
   (lambda ()
     (define desc (m-tex2d-desc (cls "MTLTextureDescriptor") PIXEL-BGRA8 pw ph #f))
     (m-set-usage desc USAGE-RENDER-TARGET)
     (m-set-storage-mode desc STORAGE-SHARED)
     (define tex (m-new-texture (gpu-device g) desc))
     (define-values (cb buf) (encode! g tex dl clear (/ pw scale) (/ ph scale) scale))
     (m-commit cb)
     (m-wait cb)
     (release! buf)
     (define px (malloc (* pw ph 4) 'raw))
     (m-get-bytes tex px (* pw 4) (make-MTLRegion 0 0 0 pw ph 1) 0)
     (define out (make-bytes (* pw ph 4)))
     (memcpy out px (* pw ph 4))
     (free px)
     (release! tex)
     out)))
) ; end define gpu-render-offscreen

;; ---------------------------------------------------------------- 组件

;; provides gpu-port。config：'layer?（#t 窗口用 / #f 只离屏），'atlas-size
(define renderer-component
  (component 'renderer '() (list gpu-port)
             (lambda (use config)
               (define g (gpu-open #:layer? (hash-ref config 'layer? #t) #:atlas-size (hash-ref config 'atlas-size 2048)))
               (list (implement gpu-port
                                'layer (lambda () (gpu-layer g))
                                'resize! (lambda (w h scale) (gpu-resize! g w h scale))
                                'render! (lambda (dl clear w h scale) (gpu-render! g dl clear w h #:scale scale))
                                'render-offscreen (lambda (dl clear pw ph scale) (gpu-render-offscreen g dl clear pw ph #:scale scale))
                                'upload-atlas! (lambda (bytes rect) (gpu-atlas-upload! g bytes rect))
                                'upload-images! (lambda (bytes rect) (gpu-images-upload! g bytes rect))
                                ;; 液态玻璃：透明的第二张层（侧栏画在上面），清成全透明
                                'overlay-layer (lambda () (gpu-overlay-layer! g))
                                'set-opaque! (lambda (on?) (m-set-opaque (gpu-layer g) on?))   ; 主层透明：窗口透出桌面（液态玻璃·透出桌面）
                                'render-overlay! (lambda (dl w h scale) (and (gpu-overlay g) (gpu-render! g dl '(0.0 0.0 0.0 0.0) w h #:scale scale #:layer (gpu-overlay g))))
                                'device-name (lambda () (gpu-device-name g)))))))

;; current-encode-fault-hook：故障注入（fault-test 用）
(provide renderer-component current-encode-fault-hook)
