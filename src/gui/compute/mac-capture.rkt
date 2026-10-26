#lang racket/base
;; mac-capture.rkt — 截本进程自己的窗口（系统合成之后的像素：含液态玻璃）。显示效果的自动检查用（compute/contrast.rkt）。
;; CGWindowListCreateImage：没有屏幕录制权限时，本进程的窗口与桌面背景照样截得到（别的应用的窗口被抹掉）；
;; macOS 15 起 SDK 标为过时，但 macOS 26 的 CoreGraphics 仍导出它（转到 SkyLight），实测可用（2026-10-25）。
;;   capture-window：窗口号 + 可选的屏幕矩形 → (values 宽 高 RGBA 字节（sRGB、预乘、行 0 = 顶）) 或 (values 0 0 #f)
;;     below? #f：只截这个窗口（含标题栏）；#t：截屏幕上 rect 区域里这个窗口及它下面的（透明窗口后面的东西也在）

(require ffi/unsafe "mac-geom.rkt")

(define libcg (ffi-lib "/System/Library/Frameworks/CoreGraphics.framework/CoreGraphics"))
(define CGWindowListCreateImage (get-ffi-obj 'CGWindowListCreateImage libcg (_fun _CGRect _uint32 _uint32 _uint32 -> _pointer)))
(define CGImageGetWidth (get-ffi-obj 'CGImageGetWidth libcg (_fun _pointer -> _size)))
(define CGImageGetHeight (get-ffi-obj 'CGImageGetHeight libcg (_fun _pointer -> _size)))
(define CGColorSpaceCreateWithName (get-ffi-obj 'CGColorSpaceCreateWithName libcg (_fun _pointer -> _pointer)))
(define kCGColorSpaceSRGB (get-ffi-obj 'kCGColorSpaceSRGB libcg _pointer))         ; 全局量里存的就是 CFStringRef
(define CGBitmapContextCreate (get-ffi-obj 'CGBitmapContextCreate libcg (_fun _pointer _size _size _size _size _pointer _uint32 -> _pointer)))
(define CGContextDrawImage (get-ffi-obj 'CGContextDrawImage libcg (_fun _pointer _CGRect _pointer -> _void)))
(define CGRectNull (get-ffi-obj 'CGRectNull libcg _CGRect))
(define CFRelease (get-ffi-obj 'CFRelease libcg (_fun _pointer -> _void)))

(define OPT-BELOW 2)                     ; kCGWindowListOptionOnScreenBelowWindow
(define OPT-INCLUDING 8)                 ; kCGWindowListOptionIncludingWindow
(define IMG-IGNORE-FRAMING 1)            ; kCGWindowImageBoundsIgnoreFraming（不要阴影）
(define IMG-BEST-RESOLUTION 8)           ; kCGWindowImageBestResolution（Retina：按像素）
(define ALPHA-PREMULTIPLIED-LAST 1)

;; rect：(vector x y w h)，全局显示坐标（左上原点，point）；#f = 窗口自己的边界
(define (capture-window number #:rect [r #f] #:below? [below? #f])
  (define img (CGWindowListCreateImage (if r (rect (vector-ref r 0) (vector-ref r 1) (vector-ref r 2) (vector-ref r 3)) CGRectNull)
                                       (if below? (bitwise-ior OPT-BELOW OPT-INCLUDING) OPT-INCLUDING)
                                       number (bitwise-ior IMG-IGNORE-FRAMING IMG-BEST-RESOLUTION)))
  (cond
    [(not img) (values 0 0 #f)]
    [else
     (define w (CGImageGetWidth img)) (define h (CGImageGetHeight img))
     (define buf (malloc (* w h 4) 'raw))
     (define cs (CGColorSpaceCreateWithName kCGColorSpaceSRGB))                  ; 换算到 sRGB 量（WCAG 按 sRGB）
     (define ctx (CGBitmapContextCreate buf w h 8 (* w 4) cs ALPHA-PREMULTIPLIED-LAST))
     (CGContextDrawImage ctx (rect 0.0 0.0 (exact->inexact w) (exact->inexact h)) img)
     (define bs (make-bytes (* w h 4)))
     (memcpy bs buf (* w h 4))
     (free buf) (CFRelease ctx) (CFRelease cs) (CFRelease img)
     (values w h bs)])
) ; end define capture-window

(provide capture-window)
