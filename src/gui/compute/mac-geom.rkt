#lang racket/base
;; geom.rkt — CoreGraphics 几何结构体（arm64 上 NSPoint/NSSize/NSRect 即 CGPoint/CGSize/CGRect）。
;; 同时登记 ObjC 类型编码，供 define-objc-subclass 生成方法签名。

(require ffi/unsafe "mac-objc.rkt")

(define-cstruct _CGPoint ([x _double] [y _double]))
(define-cstruct _CGSize ([w _double] [h _double]))
(define-cstruct _CGRect ([origin _CGPoint] [size _CGSize]))

(define-cstruct _NSRange ([location _uint64] [length _uint64]))
(define NSNotFound #x7FFFFFFFFFFFFFFF)

(register-encoding! _NSRange "{_NSRange=QQ}")
(register-encoding! _CGPoint "{CGPoint=dd}")
(register-encoding! _CGSize "{CGSize=dd}")
(register-encoding! _CGRect "{CGRect={CGPoint=dd}{CGSize=dd}}")

(define (rect x y w h) (make-CGRect (make-CGPoint x y) (make-CGSize w h)))
(define (rect-w r) (CGSize-w (CGRect-size r)))
(define (rect-h r) (CGSize-h (CGRect-size r)))

(provide
 _CGPoint make-CGPoint CGPoint-x CGPoint-y
 _CGSize make-CGSize CGSize-w CGSize-h
 _CGRect make-CGRect CGRect-origin CGRect-size
 _NSRange make-NSRange NSRange-location NSRange-length NSNotFound
 rect rect-w rect-h
) ; end provide
