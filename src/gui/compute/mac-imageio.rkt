#lang racket/base
;; mac-imageio.rkt — 图片解码（ImageIO）与进程自我沙箱（sandbox_init）。只在解码子进程里用（components/mac-decoder.rkt）：
;; 不可信的图片字节不在 GUI 进程里解码（design-gui.md §7）。
;;   decode-thumbnail：字节 → 缩略图（长边不超过 max-px，按 EXIF 方向转正）→ (values 宽 高 RGBA 预乘字节 原图宽 原图高) 或 (values #f 原因 …)
;;   sandbox-self!：调 sandbox_init 进入 "pure-computation" 命名档案（不能读写文件、不能联网、不能起进程）；之后只做计算。
;;     须在用到的框架都加载完、解码过一次（ImageIO 的格式插件是惰性加载的）之后调用。

(require ffi/unsafe "mac-cf.rkt")

(define libcg (ffi-lib "/System/Library/Frameworks/CoreGraphics.framework/CoreGraphics"))
(define libio (ffi-lib "/System/Library/Frameworks/ImageIO.framework/ImageIO"))
(define libcf (ffi-lib "/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation"))
(define libsys (ffi-lib #f))

(define CFDataCreate (get-ffi-obj 'CFDataCreate libcf (_fun _pointer _bytes _long -> _pointer)))
(define CFNumberCreate (get-ffi-obj 'CFNumberCreate libcf (_fun _pointer _long _pointer -> _pointer)))
(define CFDictionaryCreate (get-ffi-obj 'CFDictionaryCreate libcf (_fun _pointer _pointer _pointer _long _pointer _pointer -> _pointer)))
;; 回调表是结构体全局量：要的是它的地址；CFStringRef / CFBooleanRef 全局量：get-ffi-obj _pointer 读出的就是对象
(define kCFTypeDictionaryKeyCallBacks (ffi-obj-ref 'kCFTypeDictionaryKeyCallBacks libcf))
(define kCFTypeDictionaryValueCallBacks (ffi-obj-ref 'kCFTypeDictionaryValueCallBacks libcf))
(define kCFBooleanTrue (get-ffi-obj 'kCFBooleanTrue libcf _pointer))
(define CFDictionaryGetValue (get-ffi-obj 'CFDictionaryGetValue libcf (_fun _pointer _pointer -> _pointer)))
(define CFNumberGetValue (get-ffi-obj 'CFNumberGetValue libcf (_fun _pointer _long _pointer -> _bool)))
(define kCFNumberLongType 10)

(define CGImageSourceCreateWithData (get-ffi-obj 'CGImageSourceCreateWithData libio (_fun _pointer _pointer -> _pointer)))
(define CGImageSourceGetCount (get-ffi-obj 'CGImageSourceGetCount libio (_fun _pointer -> _size)))
(define CGImageSourceCreateThumbnailAtIndex (get-ffi-obj 'CGImageSourceCreateThumbnailAtIndex libio (_fun _pointer _size _pointer -> _pointer)))
(define CGImageSourceCopyPropertiesAtIndex (get-ffi-obj 'CGImageSourceCopyPropertiesAtIndex libio (_fun _pointer _size _pointer -> _pointer)))
(define kThumbFromAlways (get-ffi-obj 'kCGImageSourceCreateThumbnailFromImageAlways libio _pointer))
(define kThumbMaxPx (get-ffi-obj 'kCGImageSourceThumbnailMaxPixelSize libio _pointer))
(define kThumbTransform (get-ffi-obj 'kCGImageSourceCreateThumbnailWithTransform libio _pointer))
(define kPixelWidth (get-ffi-obj 'kCGImagePropertyPixelWidth libio _pointer))
(define kPixelHeight (get-ffi-obj 'kCGImagePropertyPixelHeight libio _pointer))

(define CGImageGetWidth (get-ffi-obj 'CGImageGetWidth libcg (_fun _pointer -> _size)))
(define CGImageGetHeight (get-ffi-obj 'CGImageGetHeight libcg (_fun _pointer -> _size)))
(define CGImageRelease (get-ffi-obj 'CGImageRelease libcg (_fun _pointer -> _void)))
(define CGColorSpaceCreateDeviceRGB (get-ffi-obj 'CGColorSpaceCreateDeviceRGB libcg (_fun -> _pointer)))
(define CGBitmapContextCreate (get-ffi-obj 'CGBitmapContextCreate libcg (_fun _pointer _size _size _size _size _pointer _uint32 -> _pointer)))
(define CGContextRelease (get-ffi-obj 'CGContextRelease libcg (_fun _pointer -> _void)))
(define-cstruct _CGRect ([x _double] [y _double] [w _double] [h _double]))
(define CGContextDrawImage (get-ffi-obj 'CGContextDrawImage libcg (_fun _pointer _CGRect _pointer -> _void)))
(define BITMAP-PREMULTIPLIED-LAST 1)                    ; kCGImageAlphaPremultipliedLast：RGBA

(define device-rgb (CGColorSpaceCreateDeviceRGB))

;; CF 值的 *const 指针数组（CFDictionaryCreate 的 keys / values）
(define (ptr-array ps)
  (define a (malloc (* (length ps) (ctype-sizeof _pointer)) 'raw))
  (for ([p (in-list ps)] [i (in-naturals)]) (ptr-set! a _pointer i p))
  a)

(define (cf-number n)
  (define p (malloc (ctype-sizeof _long) 'raw))
  (ptr-set! p _long n)
  (begin0 (CFNumberCreate #f kCFNumberLongType p) (free p)))

(define (cf-number-value p)
  (and p (let ([out (malloc (ctype-sizeof _long) 'raw)])
           (begin0 (and (CFNumberGetValue p kCFNumberLongType out) (ptr-ref out _long)) (free out)))))

(define (with-source bs proc)
  (define data (CFDataCreate #f bs (bytes-length bs)))
  (define src (and data (CGImageSourceCreateWithData data #f)))
  (dynamic-wind void
                (lambda () (if (and src (> (CGImageSourceGetCount src) 0)) (proc src) #f))
                (lambda () (cf-release! src) (cf-release! data))))

;; 原图尺寸（不解码像素）：→ (values 宽 高) 或 (values #f #f)
(define (image-size bs)
  (define r (with-source bs (lambda (src)
                              (define props (CGImageSourceCopyPropertiesAtIndex src 0 #f))
                              (begin0 (and props (cons (cf-number-value (CFDictionaryGetValue props kPixelWidth))
                                                       (cf-number-value (CFDictionaryGetValue props kPixelHeight))))
                                (cf-release! props)))))
  (if (and r (car r) (cdr r)) (values (car r) (cdr r)) (values #f #f)))

;; → (values 宽 高 RGBA 原图宽 原图高)；不是图片 / 解不开 → (values #f 原因 #f #f #f)
(define (decode-thumbnail bs max-px)
  (define-values (ow oh) (image-size bs))
  (define r
    (with-source bs
      (lambda (src)
        (define n (cf-number max-px))
        (define keys (ptr-array (list kThumbFromAlways kThumbMaxPx kThumbTransform)))
        (define vals (ptr-array (list kCFBooleanTrue n kCFBooleanTrue)))
        (define opts (CFDictionaryCreate #f keys vals 3 kCFTypeDictionaryKeyCallBacks kCFTypeDictionaryValueCallBacks))
        (free keys) (free vals) (cf-release! n)
        (define img (CGImageSourceCreateThumbnailAtIndex src 0 opts))
        (cf-release! opts)
        (and img
             (let* ([w (CGImageGetWidth img)] [h (CGImageGetHeight img)]
                    [buf (malloc (* w h 4) 'raw)]
                    [ctx (CGBitmapContextCreate buf w h 8 (* w 4) device-rgb BITMAP-PREMULTIPLIED-LAST)])
               (memset buf 0 (* w h 4))
               (CGContextDrawImage ctx (make-CGRect 0.0 0.0 (exact->inexact w) (exact->inexact h)) img)
               (define out (make-bytes (* w h 4)))
               (memcpy out buf (* w h 4))
               (CGContextRelease ctx) (free buf) (CGImageRelease img)
               (list w h out))))))
  (if r (values (car r) (cadr r) (caddr r) ow oh) (values #f "not a decodable image" #f #f #f))
) ; end define decode-thumbnail

;; ---------------------------------------------------------------- 自我沙箱

(define sandbox_init (get-ffi-obj 'sandbox_init libsys (_fun _string _uint64 (err : (_ptr o _pointer)) -> (r : _int) -> (values r err))))
(define SANDBOX_NAMED 1)

;; → #t 或 raise
(define (sandbox-self! [profile "pure-computation"])
  (define-values (r err) (sandbox_init profile SANDBOX_NAMED))
  (unless (zero? r) (error 'sandbox-self! "sandbox_init failed: ~a" (if err (cast err _pointer _string) "?")))
  #t
) ; end define sandbox-self!

(provide decode-thumbnail image-size sandbox-self!)
