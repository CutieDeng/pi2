#lang racket/base
;; png.rkt — 最小 PNG 编码器（L0，纯 Racket，deflate 用 file/gzip）。
;; 用途：把离屏渲染的像素落成图片，供人 / 测试查看（P10：没有屏幕录制权限时的可视化验证）。

(require file/gzip)

(define crc-table
  (for/vector ([n (in-range 256)])
    (for/fold ([c n]) ([_ (in-range 8)])
      (if (odd? c) (bitwise-xor #xEDB88320 (arithmetic-shift c -1)) (arithmetic-shift c -1))))
) ; end define crc-table

(define (crc32 bs)
  (bitwise-xor
   #xFFFFFFFF
   (for/fold ([c #xFFFFFFFF]) ([b (in-bytes bs)])
     (bitwise-xor (vector-ref crc-table (bitwise-and (bitwise-xor c b) #xFF)) (arithmetic-shift c -8))))
) ; end define crc32

(define (adler32 bs)
  (define-values (a b)
    (for/fold ([a 1] [b 0]) ([x (in-bytes bs)])
      (define a* (modulo (+ a x) 65521))
      (values a* (modulo (+ b a*) 65521))))
  (+ (arithmetic-shift b 16) a)
) ; end define adler32

(define (u32 n) (integer->integer-bytes n 4 #f #t))

(define (chunk type data)
  (define td (bytes-append type data))
  (bytes-append (u32 (bytes-length data)) td (u32 (crc32 td)))
) ; end define chunk

;; pixels：w*h*4 字节，行 0 = 顶部；order：'bgra（Metal 读回）或 'rgba。alpha 原样写出。
(define (write-png path w h pixels #:order [order 'bgra])
  (define raw (make-bytes (* h (add1 (* w 4))) 0))      ; 每行前缀 filter 0
  (for* ([y (in-range h)] [x (in-range w)])
    (define s (* 4 (+ (* y w) x)))
    (define d (+ (* y (add1 (* w 4))) 1 (* 4 x)))
    (define-values (r g b) (if (eq? order 'bgra)
                               (values (bytes-ref pixels (+ s 2)) (bytes-ref pixels (+ s 1)) (bytes-ref pixels s))
                               (values (bytes-ref pixels s) (bytes-ref pixels (+ s 1)) (bytes-ref pixels (+ s 2)))))
    (bytes-set! raw d r)
    (bytes-set! raw (+ d 1) g)
    (bytes-set! raw (+ d 2) b)
    (bytes-set! raw (+ d 3) (bytes-ref pixels (+ s 3))))
  (define z (open-output-bytes))
  (deflate (open-input-bytes raw) z)
  (define idat (bytes-append #"\x78\x01" (get-output-bytes z) (u32 (adler32 raw))))
  (define ihdr (bytes-append (u32 w) (u32 h) (bytes 8 6 0 0 0)))   ; 8-bit RGBA
  (call-with-output-file path #:exists 'replace
    (lambda (o)
      (write-bytes #"\x89PNG\r\n\x1a\n" o)
      (write-bytes (chunk #"IHDR" ihdr) o)
      (write-bytes (chunk #"IDAT" idat) o)
      (write-bytes (chunk #"IEND" #"") o)))
  (void)
) ; end define write-png

(provide write-png crc32 adler32)
