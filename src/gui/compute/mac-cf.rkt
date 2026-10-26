#lang racket/base
;; cf.rkt — CoreFoundation C 接口（零 objc）。CFString 与 NSString toll-free bridged，
;; 平台层用它把 Racket 字符串交给 AppKit / Core Text，不经 NSString 的 objc 方法。

(require ffi/unsafe)

(define libcf (ffi-lib "/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation"))

(define kCFStringEncodingUTF8 #x08000100)

(define CFStringCreateWithBytes
  (get-ffi-obj 'CFStringCreateWithBytes libcf (_fun _pointer _bytes _long _uint32 _stdbool -> _pointer)))
(define CFStringGetLength (get-ffi-obj 'CFStringGetLength libcf (_fun _pointer -> _long)))
(define CFStringGetMaximumSizeForEncoding
  (get-ffi-obj 'CFStringGetMaximumSizeForEncoding libcf (_fun _long _uint32 -> _long)))
(define CFStringGetCString
  (get-ffi-obj 'CFStringGetCString libcf (_fun _pointer _bytes _long _uint32 -> _stdbool)))
(define CFRelease (get-ffi-obj 'CFRelease libcf (_fun _pointer -> _void)))
(define CFRetain (get-ffi-obj 'CFRetain libcf (_fun _pointer -> _pointer)))

;; Racket 字符串 → CFStringRef（+1，调用方负责 cf-release!）
(define (cfstring s)
  (define b (string->bytes/utf-8 s))
  (or (CFStringCreateWithBytes #f b (bytes-length b) kCFStringEncodingUTF8 #f)
      (error 'cfstring "CFStringCreateWithBytes failed"))
) ; end define cfstring

;; CFStringRef / NSString* → Racket 字符串（不改变引用计数）
(define (cfstring->string p)
  (cond
    [(not p) #f]
    [else
     (define cap (add1 (CFStringGetMaximumSizeForEncoding (CFStringGetLength p) kCFStringEncodingUTF8)))
     (define buf (make-bytes cap 0))
     (unless (CFStringGetCString p buf cap kCFStringEncodingUTF8)
       (error 'cfstring->string "CFStringGetCString failed"))
     (define n (let loop ([i 0]) (if (zero? (bytes-ref buf i)) i (loop (add1 i)))))
     (bytes->string/utf-8 (subbytes buf 0 n) (integer->char #xFFFD))])
) ; end define cfstring->string

(define (call-with-cfstring s proc)
  (define p (cfstring s))
  (dynamic-wind void (lambda () (proc p)) (lambda () (CFRelease p)))
) ; end define call-with-cfstring

(define (cf-release! p) (when p (CFRelease p)))
(define (cf-retain! p) (CFRetain p))

;; 系统首选语言（「语言与地区」里的顺序）→ 标签表，如 '("zh-Hans-CN" "en-US")；界面语言用
(define CFLocaleCopyPreferredLanguages (get-ffi-obj 'CFLocaleCopyPreferredLanguages libcf (_fun -> _pointer)))
(define CFArrayGetCount (get-ffi-obj 'CFArrayGetCount libcf (_fun _pointer -> _long)))
(define CFArrayGetValueAtIndex (get-ffi-obj 'CFArrayGetValueAtIndex libcf (_fun _pointer _long -> _pointer)))
(define (preferred-languages)
  (define arr (CFLocaleCopyPreferredLanguages))
  (cond
    [(not arr) '()]
    [else
     (begin0 (for/list ([i (in-range (CFArrayGetCount arr))]) (cfstring->string (CFArrayGetValueAtIndex arr i)))
             (CFRelease arr))])
) ; end define preferred-languages

(provide cfstring cfstring->string call-with-cfstring cf-release! cf-retain! preferred-languages)
