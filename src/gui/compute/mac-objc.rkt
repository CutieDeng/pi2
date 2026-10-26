#lang racket/base
;; objc.rkt — 自写 ObjC 桥（design-gui.md §1）
;; 只调 libobjc 的 C 函数：objc_msgSend 按方法签名 cast 成具体 C 函数类型，selector 在定义时注册。
;; 不用 ffi/unsafe/objc（实测 tell 44ns/次，本桥 6.5ns/次）。v1 只支持 arm64（无 _stret 变体，P12）。

(require ffi/unsafe "mac-cf.rkt")

(unless (eq? (system-type 'arch) 'aarch64)
  (error 'objc "only arm64 macOS is supported (x86_64 needs objc_msgSend_stret, design-gui.md P12)"))

(define libobjc (ffi-lib "/usr/lib/libobjc.A.dylib"))
(define libc (ffi-lib #f))

(define _id _pointer)          ; 任意 ObjC 对象 / Class / nil(#f)
(define _SEL _pointer)
(define _BOOL _stdbool)        ; arm64 上 BOOL 是 C bool

;; ---------------------------------------------------------------- libobjc C 函数

(define objc_getClass (get-ffi-obj 'objc_getClass libobjc (_fun _string/utf-8 -> _id)))
(define objc_getProtocol (get-ffi-obj 'objc_getProtocol libobjc (_fun _string/utf-8 -> _pointer)))
(define sel_registerName (get-ffi-obj 'sel_registerName libobjc (_fun _string/utf-8 -> _SEL)))
(define objc_allocateClassPair
  (get-ffi-obj 'objc_allocateClassPair libobjc (_fun _id _string/utf-8 _size -> _id)))
(define objc_registerClassPair (get-ffi-obj 'objc_registerClassPair libobjc (_fun _id -> _void)))
(define class_addMethod
  (get-ffi-obj 'class_addMethod libobjc (_fun _id _SEL _fpointer _string/utf-8 -> _stdbool)))
(define class_addProtocol (get-ffi-obj 'class_addProtocol libobjc (_fun _id _pointer -> _stdbool)))
(define class_conformsToProtocol
  (get-ffi-obj 'class_conformsToProtocol libobjc (_fun _id _pointer -> _stdbool)))
(define class_getName (get-ffi-obj 'class_getName libobjc (_fun _id -> _string/utf-8)))
(define object_getClass (get-ffi-obj 'object_getClass libobjc (_fun _id -> _id)))
(define objc_retain (get-ffi-obj 'objc_retain libobjc (_fun _id -> _id)))
(define objc_release (get-ffi-obj 'objc_release libobjc (_fun _id -> _void)))
(define objc_autoreleasePoolPush (get-ffi-obj 'objc_autoreleasePoolPush libobjc (_fun -> _pointer)))
(define objc_autoreleasePoolPop (get-ffi-obj 'objc_autoreleasePoolPop libobjc (_fun _pointer -> _void)))
(define sel_getName (get-ffi-obj 'sel_getName libobjc (_fun _SEL -> _string/utf-8)))
(define pthread_main_np (get-ffi-obj 'pthread_main_np libc (_fun -> _int)))
(define msgsend-addr (get-ffi-obj 'objc_msgSend libobjc _fpointer))
(define objc_setUncaughtExceptionHandler
  (get-ffi-obj 'objc_setUncaughtExceptionHandler libobjc (_fun (_fun _id -> _void) -> _pointer)))

;; ---------------------------------------------------------------- 类 / selector / 线程

(define (cls name)
  (or (objc_getClass name) (error 'cls "no ObjC class: ~a" name))
) ; end define cls

(define (sel name) (sel_registerName name))
(define (sel-name s) (sel_getName s))

(define (class-name-of obj) (class_getName (object_getClass obj)))

(define (main-thread?) (= 1 (pthread_main_np)))

;; AppKit 只许在 OS 主线程调用；违反时 AppKit 抛 NSException 直接 abort（P1/P8），所以先在 Racket 侧拦住。
(define (assert-main-thread! who)
  (unless (main-thread?)
    (error who "must run on the OS main thread (AppKit rule, design-gui.md P1/P2)"))
) ; end define assert-main-thread!

(define (retain! o) (objc_retain o))
(define (release! o) (when o (objc_release o)))

(define (call-with-autorelease-pool thunk)
  (define p (objc_autoreleasePoolPush))
  (dynamic-wind void thunk (lambda () (objc_autoreleasePoolPop p)))
) ; end define call-with-autorelease-pool

;; ---------------------------------------------------------------- 消息发送

;; 按签名缓存 objc_msgSend 的 C 函数视图。arm64 上 objc_msgSend 必须以方法的真实原型调用。
(define sender-cache (make-hash))

(define (msg-sender ret args)
  (define key (cons ret args))
  (or (hash-ref sender-cache key #f)
      (let ([f (cast msgsend-addr _fpointer (_cprocedure (list* _id _SEL args) ret))])
        (hash-set! sender-cache key f)
        f))
) ; end define msg-sender

;; 本 place 内经 define-msg 发出的消息总数（S10：验证每帧 objc 调用数为常数）。
(define msg-counter (box 0))
(define (objc-msg-count) (unbox msg-counter))

;; (define-msg name "sel:with:" ret ([a type] ...)) → (name receiver a ...)
;; selector 与 C 函数类型都在定义时确定，调用路径上没有查表。
(define-syntax-rule (define-msg name selector ret ([arg type] ...))
  (define name
    (let ([s (sel_registerName selector)]
          [f (msg-sender ret (list type ...))])
      (lambda (obj arg ...)
        (set-box! msg-counter (add1 (unbox msg-counter)))
        (f obj s arg ...))))
) ; end define-syntax-rule define-msg

(define-msg msg-alloc "alloc" _id ())
(define-msg msg-init "init" _id ())
(define-msg msg-new "new" _id ())
(define-msg msg-responds? "respondsToSelector:" _BOOL ([s _SEL]))

(define (alloc-init class-name) (msg-init (msg-alloc (cls class-name))))

;; ---------------------------------------------------------------- 类型编码

;; class_addMethod 需要 ObjC 类型编码串。基础类型内置；结构体由平台层登记（如 NSRect）。
(define encodings
  (make-hasheq
   (list (cons _void "v") (cons _pointer "@") (cons _stdbool "B")
         (cons _int8 "c") (cons _uint8 "C") (cons _int16 "s") (cons _uint16 "S")
         (cons _int32 "i") (cons _uint32 "I") (cons _int64 "q") (cons _uint64 "Q")
         (cons _long "q") (cons _ulong "Q") (cons _intptr "q") (cons _uintptr "Q") (cons _size "Q")
         (cons _float "f") (cons _double "d")))
) ; end define encodings

(define (register-encoding! ctype enc) (hash-set! encodings ctype enc))

(define (encoding-of who ctype)
  (or (hash-ref encodings ctype #f)
      (error who "no ObjC type encoding registered for ctype ~a (use register-encoding!)" ctype))
) ; end define encoding-of

(define (default-for ctype)
  (cond
    [(eq? ctype _void) (void)]
    [(eq? ctype _stdbool) #f]
    [(eq? ctype _pointer) #f]
    [(memq ctype (list _float _double)) 0.0]
    [(memq ctype (list _int8 _uint8 _int16 _uint16 _int32 _uint32 _int64 _uint64
                       _long _ulong _intptr _uintptr _size)) 0]
    [else #f])
) ; end define default-for

;; ---------------------------------------------------------------- 子类定义

;; 方法描述：selector 串、返回类型、参数类型（不含 self/_cmd）、实现 (λ (self arg ...) ...)。
;; 结构体返回值必须给 default（回调出错时返回它）。
(struct method (selector ret args proc default))

(define (objc-method selector ret args proc #:default [dflt (default-for ret)])
  (when (and (not dflt) (not (memq ret (list _void _stdbool _pointer))))
    (error 'objc-method "~a returns a struct; pass #:default" selector))
  (method selector ret args proc dflt)
) ; end define objc-method

;; IMP 必须保持可达，否则回调被 GC 后 ObjC 调到悬空指针。
(define keep-alive '())

;; 回调里的 Racket 异常不能穿过 C 栈帧（P6）：全捕获，记录，返回默认值。
(define last-callback-error (box #f))

(define (wrap-imp m)
  (define proc (method-proc m))
  (define dflt (method-default m))
  (define selector (method-selector m))
  (lambda (self _cmd . args)
    (parameterize-break #f
      (with-handlers ([(lambda (_e) #t)
                       (lambda (e)
                         (set-box! last-callback-error (cons selector e))
                         (log-error "objc callback ~a raised: ~a" selector
                                    (if (exn? e) (exn-message e) e))
                         dflt)])
        (apply proc self args))))
) ; end define wrap-imp

;; 定义并注册 ObjC 子类。同名类已存在 → 报错（不复用：旧 IMP 可能属于另一个 place，P2）。
(define (define-objc-subclass name super-name methods #:protocols [protocols '()])
  (define super (cls super-name))
  (define c (objc_allocateClassPair super name 0))
  (unless c
    (error 'define-objc-subclass
           "ObjC class ~a already exists (module instantiated twice? see design-gui.md P2)" name))
  (for ([p (in-list protocols)])
    (define proto (or (objc_getProtocol p) (error 'define-objc-subclass "no ObjC protocol: ~a" p)))
    (class_addProtocol c proto))
  (for ([m (in-list methods)])
    (define enc
      (apply string-append (encoding-of 'define-objc-subclass (method-ret m)) "@:"
             (for/list ([t (in-list (method-args m))]) (encoding-of 'define-objc-subclass t))))
    (define imp-type (_cprocedure (list* _id _SEL (method-args m)) (method-ret m)))
    (define imp (wrap-imp m))
    (set! keep-alive (cons imp keep-alive))
    (unless (class_addMethod c (sel_registerName (method-selector m)) (cast imp imp-type _fpointer) enc)
      (error 'define-objc-subclass "class_addMethod failed: ~a" (method-selector m))))
  (objc_registerClassPair c)
  c
) ; end define define-objc-subclass

;; ---------------------------------------------------------------- 崩溃诊断（P8）

;; NSException 走 C++ 式栈展开，过不了 Racket 栈帧，Racket 的 with-handlers 接不到；未捕获时
;; libc++abi 调 std::terminate → abort。这里只能在 abort 之前拿到 name / reason 做记录，不能恢复。
(define-msg msg-exception-name "name" _id ())
(define-msg msg-exception-reason "reason" _id ())
(define uncaught-reporter #f)                      ; 保持回调可达

;; proc : (name reason) -> any。在抛出异常的线程上、进程 abort 之前被调用。
(define (install-uncaught-exception-reporter! proc)
  (define (h exc)
    (with-handlers ([(lambda (_e) #t) void])
      (proc (cfstring->string (msg-exception-name exc)) (cfstring->string (msg-exception-reason exc)))))
  (set! uncaught-reporter h)
  (objc_setUncaughtExceptionHandler h)
  (void)
) ; end define install-uncaught-exception-reporter!

(define (conforms-to? class-ptr protocol-name)
  (define proto (objc_getProtocol protocol-name))
  (and proto (class_conformsToProtocol class-ptr proto))
) ; end define conforms-to?

(provide
 _id _SEL _BOOL
 cls sel sel-name class-name-of
 main-thread? assert-main-thread!
 retain! release! call-with-autorelease-pool
 msg-sender define-msg objc-msg-count
 msg-alloc msg-init msg-new msg-responds? alloc-init
 register-encoding!
 objc-method define-objc-subclass conforms-to? last-callback-error
 install-uncaught-exception-reporter!
) ; end provide
