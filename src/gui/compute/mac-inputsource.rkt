#lang racket/base
;; inputsource.rkt — 输入源（Text Input Sources，Carbon HIToolbox 的 C 接口，零 objc）。
;; 仅用于测试：切到真实输入法（如五笔 com.apple.inputmethod.SCIM.WBX）验证 IME 链路，结束后切回。
;; 切换输入源会影响整个用户会话的当前输入法，调用方必须在 dynamic-wind 里恢复。

(require ffi/unsafe "mac-cf.rkt")

(define carbon (ffi-lib "/System/Library/Frameworks/Carbon.framework/Carbon"))
(define libcf (ffi-lib "/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation"))

(define TISCreateInputSourceList (get-ffi-obj 'TISCreateInputSourceList carbon (_fun _pointer _stdbool -> _pointer)))
(define TISGetInputSourceProperty (get-ffi-obj 'TISGetInputSourceProperty carbon (_fun _pointer _pointer -> _pointer)))
(define TISCopyCurrentKeyboardInputSource (get-ffi-obj 'TISCopyCurrentKeyboardInputSource carbon (_fun -> _pointer)))
(define TISSelectInputSource (get-ffi-obj 'TISSelectInputSource carbon (_fun _pointer -> _int32)))
(define kTISPropertyInputSourceID (get-ffi-obj 'kTISPropertyInputSourceID carbon _pointer))
(define kTISPropertyInputSourceIsSelectCapable (get-ffi-obj 'kTISPropertyInputSourceIsSelectCapable carbon _pointer))
(define CFArrayGetCount (get-ffi-obj 'CFArrayGetCount libcf (_fun _pointer -> _long)))
(define CFArrayGetValueAtIndex (get-ffi-obj 'CFArrayGetValueAtIndex libcf (_fun _pointer _long -> _pointer)))
(define CFBooleanGetValue (get-ffi-obj 'CFBooleanGetValue libcf (_fun _pointer -> _stdbool)))

(define (source-id src) (cfstring->string (TISGetInputSourceProperty src kTISPropertyInputSourceID)))

;; 已启用且可选的输入源 id
(define (enabled-input-sources)
  (define lst (TISCreateInputSourceList #f #f))
  (begin0
    (for/list ([i (in-range (CFArrayGetCount lst))]
               #:when (let ([p (TISGetInputSourceProperty (CFArrayGetValueAtIndex lst i) kTISPropertyInputSourceIsSelectCapable)])
                        (and p (CFBooleanGetValue p))))
      (source-id (CFArrayGetValueAtIndex lst i)))
    (cf-release! lst))
) ; end define enabled-input-sources

(define (current-input-source)
  (define s (TISCopyCurrentKeyboardInputSource))
  (begin0 (source-id s) (cf-release! s))
) ; end define current-input-source

;; → #t 成功
(define (select-input-source! id)
  (define lst (TISCreateInputSourceList #f #f))
  (begin0
    (for/or ([i (in-range (CFArrayGetCount lst))])
      (define src (CFArrayGetValueAtIndex lst i))
      (and (equal? (source-id src) id) (zero? (TISSelectInputSource src))))
    (cf-release! lst))
) ; end define select-input-source!

;; 临时切到 id，thunk 结束（含异常）后切回原输入源
(define (call-with-input-source id thunk)
  (define before (current-input-source))
  (dynamic-wind
   (lambda () (unless (select-input-source! id) (error 'call-with-input-source "cannot select ~a" id)))
   thunk
   (lambda () (select-input-source! before)))
) ; end define call-with-input-source

(provide enabled-input-sources current-input-source select-input-source! call-with-input-source)
