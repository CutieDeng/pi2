#lang racket/base
;; tests/gui/objc-test.rkt — 自写 ObjC 桥 + CF 字符串（无窗口，可在任意线程跑）

(require rackunit ffi/unsafe
         "../../src/gui/compute/mac-objc.rkt"
         "../../src/gui/compute/mac-cf.rkt")

(define (fresh name) (format "~a_~a" name (random 1000000000)))

(define-msg msg-hash "hash" _uint64 ())
(define-msg msg-description "description" _id ())
(define-msg msg-kind-of? "isKindOfClass:" _BOOL ([c _id]))
(define-msg msg-retain-count "retainCount" _uint64 ())

(test-case "class lookup + typed msgSend"
  (check-true (cpointer? (cls "NSObject")))
  (check-exn #rx"no ObjC class" (lambda () (cls "PiNoSuchClass_xyz")))
  (define o (msg-new (cls "NSObject")))
  (check-true (> (msg-hash o) 0))
  (check-true (msg-kind-of? o (cls "NSObject")))
  (check-equal? (class-name-of o) "NSObject")
  (call-with-autorelease-pool
   (lambda () (check-regexp-match #rx"NSObject" (cfstring->string (msg-description o)))))
  (release! o))

(test-case "retain / release change the retain count"
  (define o (msg-new (cls "NSObject")))
  (define n0 (msg-retain-count o))
  (retain! o)
  (check-equal? (msg-retain-count o) (add1 n0))
  (release! o)
  (check-equal? (msg-retain-count o) n0)
  (release! o))

(test-case "CFString round trip keeps CJK / emoji"
  (for ([s (list "" "ascii" "pi++ 你好，世界 ✓ 🚀" (make-string 5000 #\字))])
    (check-equal? (call-with-cfstring s cfstring->string) s)))

(test-case "subclass method implemented in Racket, called through objc_msgSend"
  (define name (fresh "PiTestAdder"))
  (define c (define-objc-subclass name "NSObject"
              (list (objc-method "piAdd:to:" _int64 (list _int64 _int64)
                                 (lambda (self a b) (+ a b)))
                    (objc-method "piHalf:" _double (list _double)
                                 (lambda (self x) (/ x 2.0))))))
  (define-msg msg-add "piAdd:to:" _int64 ([a _int64] [b _int64]))
  (define-msg msg-half "piHalf:" _double ([x _double]))
  (define o (msg-init (msg-alloc c)))
  (check-equal? (class-name-of o) name)
  (check-equal? (msg-add o 40 2) 42)
  (check-equal? (msg-add o -7 -8) -15)
  (check-= (msg-half o 3.0) 1.5 1e-12)
  (check-true (msg-responds? o (sel "piAdd:to:")))
  (release! o))

(test-case "exception inside a callback returns the default value; process survives"
  (define c (define-objc-subclass (fresh "PiTestBoom") "NSObject"
              (list (objc-method "piBoom" _int64 '() (lambda (self) (error 'boom "kaboom")))
                    (objc-method "piBoomObj" _id '() (lambda (self) (raise 'not-an-exn))))))
  (define-msg msg-boom "piBoom" _int64 ())
  (define-msg msg-boom-obj "piBoomObj" _id ())
  (define o (msg-init (msg-alloc c)))
  (check-equal? (msg-boom o) 0)
  (check-equal? (car (unbox last-callback-error)) "piBoom")
  (check-false (msg-boom-obj o))
  (check-equal? (cdr (unbox last-callback-error)) 'not-an-exn)
  (release! o))

(test-case "protocol adoption"
  (define c (define-objc-subclass (fresh "PiTestCopy") "NSObject" '() #:protocols '("NSCopying")))
  (check-true (conforms-to? c "NSCopying"))
  (check-false (conforms-to? (cls "NSObject") "NSCopying"))
  (check-exn #rx"no ObjC protocol"
             (lambda () (define-objc-subclass (fresh "PiTestBadProto") "NSObject" '()
                          #:protocols '("PiNoSuchProtocol")))))

(test-case "duplicate class name is rejected (design-gui.md P2)"
  (define name (fresh "PiTestDup"))
  (define-objc-subclass name "NSObject" '())
  (check-exn #rx"already exists" (lambda () (define-objc-subclass name "NSObject" '()))))

(test-case "struct return types need an explicit default"
  (define-cstruct _Pair ([a _double] [b _double]))
  (check-exn #rx"pass #:default" (lambda () (objc-method "piPair" _Pair '() (lambda (s) #f))))
  (check-exn #rx"no ObjC type encoding"
             (lambda () (define-objc-subclass (fresh "PiTestEnc") "NSObject"
                          (list (objc-method "piPair" _Pair '() (lambda (s) #f)
                                             #:default (make-Pair 0.0 0.0)))))))

(test-case "autorelease pool returns the thunk's value"
  (check-equal? (call-with-autorelease-pool (lambda () 7)) 7))
