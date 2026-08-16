#lang racket/base
;; browser/js/runtime/realm.rkt — L6 · realm 装配（原型根 + 构造器 + 门面）
;; 对标: ECMAScript 2023 §9.3（Realm）· §20.1 Object · §22.1 Array · §20.2 Function
;; 依赖: racket/base browser/js/runtime/{values,convert,function,array}
;; 说明: 一个 cell 一个 realm（§11 隔离）。此处装配 Object/Function/Array 三条原型链 +
;;       构造器 + 一批关键内建。最小但结构完整——足以看清「JS 对象世界如何在 Racket 里成形」。

(require racket/string "values.rkt" "convert.rkt" "function.rkt" "array.rkt")

(struct realm (object-proto function-proto array-proto string-proto
               object-ctor array-ctor math-obj
               [make-ordinary #:mutable]))

;; 便捷：在对象上装不可枚举方法（内建方法均 w#t e#f c#t）
(define (def-method! o name arity proc fn-proto)
  (define f (make-builtin proc fn-proto))
  (ordinary-define! f "length" (hasheq 'value (exact->inexact arity) 'w #f 'e #f 'c #t))
  (ordinary-define! f "name" (hasheq 'value name 'w #f 'e #f 'c #t))
  (ordinary-define! o name (hasheq 'value f 'w #t 'e #f 'c #t))
  f)
(define (def-value! o name v [w #t] [e #f] [c #t])
  (ordinary-define! o name (hasheq 'value v 'w w 'e e 'c c)))

(define (arg args i) (if (< i (length args)) (list-ref args i) the-undefined))

;; ---------------------------------------------------------------- 装配一个 realm
(define (make-realm)
  ;; 1. 原型根（Object.prototype 的原型是 null；其余原型链到 Object.prototype）
  (define object-proto (new-object the-null))
  (define function-proto (make-builtin (lambda (this args) the-undefined) object-proto))
  (define array-proto (new-object object-proto))
  (define string-proto (new-object object-proto))   ; 字符串原始值的方法宿主（§22.1.3）

  ;; 普通对象工厂（proto = Object.prototype）
  (define (make-ordinary) (new-object object-proto))

  ;; 2. Object.prototype 方法
  (def-method! object-proto "hasOwnProperty" 1
    (lambda (this args) (and (own-desc (to-object this) (to-property-key (arg args 0))) #t))
    function-proto)
  (def-method! object-proto "isPrototypeOf" 1
    (lambda (this args)
      (define v (arg args 0))
      (and (jso? v)
           (let loop ([p (jso-proto v)])
             (cond [(eq? p the-null) #f] [(eq? p (to-object this)) #t] [else (loop (jso-proto p))]))))
    function-proto)
  (def-method! object-proto "toString" 0
    (lambda (this args) "[object Object]") function-proto)
  (def-method! object-proto "valueOf" 0
    (lambda (this args) (to-object this)) function-proto)

  ;; 3. Array.prototype 方法
  (def-method! array-proto "push" 1
    (lambda (this args) (exact->inexact (for/last ([v (in-list args)]) (array-push! this v)))
      ) function-proto)
  (def-method! array-proto "join" 1
    (lambda (this args)
      (define sep (let ([s (arg args 0)]) (if (js-undefined? s) "," (to-js-string s))))
      (define n (len-of this))
      (apply string-append
             (for/list ([i (in-range n)])
               (define v (js-get this (number->string i)))
               (string-append (if (or (js-undefined? v) (js-null? v)) "" (to-js-string v))
                              (if (< i (sub1 n)) sep "")))))
    function-proto)
  ;; §23.1.3.36 Array.prototype.toString → 委托 join（故 ToString([1,2,3])="1,2,3"）
  (def-method! array-proto "toString" 0
    (lambda (this args) (js-call (js-get this "join") this '())) function-proto)
  ;; §23.1.3.30 Array.prototype.sort：缺省按 ToString 比较；给比较函数则用之。原地排序，返回 this。
  (def-method! array-proto "sort" 1
    (lambda (this args)
      (define cmp (arg args 0))
      (define n (len-of this))
      (define elems (for/list ([i (in-range n)]) (js-get this (number->string i))))
      (define less?
        (if (js-callable? cmp)
            (lambda (a b) (< (to-number (js-call cmp the-undefined (list a b))) 0))
            (lambda (a b) (string<? (to-js-string a) (to-js-string b)))))   ; 缺省 ToString 比较
      (for ([v (in-list (sort elems less?))] [i (in-naturals)])
        (js-define! this (number->string i) (hasheq 'value v 'w #t 'e #t 'c #t)))
      this)
    function-proto)
  (def-method! array-proto "indexOf" 1
    (lambda (this args)
      (define target (arg args 0))
      (define n (len-of this))
      (exact->inexact
       (let loop ([i 0])
         (cond [(>= i n) -1]
               [(strict-equals? (js-get this (number->string i)) target) i]
               [else (loop (add1 i))]))))
    function-proto)

  ;; 3b. String.prototype 方法（§22.1.3）。this 强制为 Racket 字符串（原始值或 String 包裹）
  (define (this-str this)
    (cond [(string? this) this]
          [(and (jso? this) (eq? (jso-exotic this) 'string)) (jso-xdata this)]
          [else (to-js-string this)]))
  (def-method! string-proto "toUpperCase" 0 (lambda (this args) (string-upcase (this-str this))) function-proto)
  (def-method! string-proto "toLowerCase" 0 (lambda (this args) (string-downcase (this-str this))) function-proto)
  (def-method! string-proto "charAt" 1
    (lambda (this args)
      (define s (this-str this)) (define i (inexact->exact (truncate (to-number (arg args 0)))))
      (if (and (>= i 0) (< i (string-length s))) (substring s i (add1 i)) ""))
    function-proto)
  (def-method! string-proto "indexOf" 1
    (lambda (this args)
      (define s (this-str this)) (define sub (to-js-string (arg args 0)))
      (define n (string-length s)) (define m (string-length sub))
      (exact->inexact
       (let loop ([i 0]) (cond [(> (+ i m) n) -1]
                               [(string=? (substring s i (+ i m)) sub) i]
                               [else (loop (add1 i))]))))
    function-proto)
  (def-method! string-proto "slice" 2
    (lambda (this args)
      (define s (this-str this)) (define n (string-length s))
      (define (norm v d) (if (js-undefined? v) d (let ([k (inexact->exact (truncate (to-number v)))])
                                                   (max 0 (min n (if (< k 0) (+ n k) k))))))
      (define a (norm (arg args 0) 0)) (define b (norm (arg args 1) n))
      (if (< a b) (substring s a b) ""))
    function-proto)
  (def-method! string-proto "split" 1
    (lambda (this args)
      (define s (this-str this)) (define sep (arg args 0))
      (define parts (cond [(js-undefined? sep) (list s)]
                          [(string=? (to-js-string sep) "") (map string (string->list s))]
                          [else (string-split s (to-js-string sep) #:trim? #f)]))
      (define a (make-array/proto array-proto))
      (for ([p (in-list parts)]) (array-push! a p)) a)
    function-proto)
  (def-method! string-proto "toString" 0 (lambda (this args) (this-str this)) function-proto)
  (def-method! string-proto "valueOf" 0 (lambda (this args) (this-str this)) function-proto)

  ;; 3c. Array 迭代方法（§23.1.3；回调 (elem index array)，再次展示闭包）
  (def-method! array-proto "forEach" 1
    (lambda (this args)
      (define f (arg args 0))
      (for ([i (in-range (len-of this))])
        (js-call f the-undefined (list (js-get this (number->string i)) (exact->inexact i) this)))
      the-undefined)
    function-proto)
  (def-method! array-proto "map" 1
    (lambda (this args)
      (define f (arg args 0)) (define r (make-array/proto array-proto))
      (for ([i (in-range (len-of this))])
        (array-push! r (js-call f the-undefined (list (js-get this (number->string i)) (exact->inexact i) this))))
      r)
    function-proto)
  (def-method! array-proto "filter" 1
    (lambda (this args)
      (define f (arg args 0)) (define r (make-array/proto array-proto))
      (for ([i (in-range (len-of this))])
        (define v (js-get this (number->string i)))
        (when (to-boolean (js-call f the-undefined (list v (exact->inexact i) this))) (array-push! r v)))
      r)
    function-proto)
  (def-method! array-proto "reduce" 2
    (lambda (this args)
      (define f (arg args 0)) (define n (len-of this))
      (define-values (acc start)
        (if (>= (length args) 2) (values (arg args 1) 0)
            (if (= n 0) (error 'reduce "reduce of empty array with no initial value")
                (values (js-get this "0") 1))))
      (let loop ([i start] [acc acc])
        (if (>= i n) acc
            (loop (add1 i) (js-call f the-undefined
                                    (list acc (js-get this (number->string i)) (exact->inexact i) this))))))
    function-proto)
  (def-method! array-proto "slice" 2
    (lambda (this args)
      (define n (len-of this))
      (define (norm v d) (if (js-undefined? v) d (let ([k (inexact->exact (truncate (to-number v)))])
                                                   (max 0 (min n (if (< k 0) (+ n k) k))))))
      (define a (norm (arg args 0) 0)) (define b (norm (arg args 1) n))
      (define r (make-array/proto array-proto))
      (for ([i (in-range a b)]) (array-push! r (js-get this (number->string i)))) r)
    function-proto)

  ;; 4. Object 构造器 + 静态方法
  (define (object-construct args)
    (define v (arg args 0))
    (if (jso? v) v (make-ordinary)))
  (define object-ctor
    (make-constructor (lambda (this args) (object-construct args)) object-construct function-proto))
  (def-value! object-ctor "prototype" object-proto #f #f #f)
  (def-value! object-proto "constructor" object-ctor)
  (def-method! object-ctor "keys" 1
    (lambda (this args)
      (define o (to-object (arg args 0)))
      (define a (make-array/proto array-proto))
      (for ([k (in-list (js-own-keys o))]
            #:when (and (string? k) (desc-enumerable? (own-desc o k))))
        (array-push! a k))
      a)
    function-proto)
  (def-method! object-ctor "getPrototypeOf" 1
    (lambda (this args) (js-get-proto (to-object (arg args 0)))) function-proto)
  (def-method! object-ctor "create" 2
    (lambda (this args)
      (define p (arg args 0))
      (unless (or (jso? p) (js-null? p)) (error 'Object.create "proto must be object or null"))
      (new-object p))
    function-proto)
  (def-method! object-ctor "defineProperty" 3
    (lambda (this args)
      (define o (arg args 0))
      (unless (jso? o) (error 'Object.defineProperty "not object"))
      (define ok (js-define! o (to-property-key (arg args 1)) (to-descriptor (arg args 2))))
      (unless ok (error 'Object.defineProperty "cannot redefine property"))
      o)
    function-proto)
  (def-method! object-ctor "getOwnPropertyNames" 1
    (lambda (this args)
      (define o (to-object (arg args 0)))
      (define a (make-array/proto array-proto))
      (for ([k (in-list (js-own-keys o))] #:when (string? k)) (array-push! a k))
      a)
    function-proto)

  ;; 5. Array 构造器
  (define (array-construct args)
    (define a (make-array/proto array-proto))
    (cond
      [(and (= 1 (length args)) (flonum? (car args)))       ; new Array(n) → 长度 n
       (js-define! a "length" (hasheq 'value (car args)))]
      [else (for ([v (in-list args)]) (array-push! a v))])
    a)
  (define array-ctor
    (make-constructor (lambda (this args) (array-construct args)) array-construct function-proto))
  (def-value! array-ctor "prototype" array-proto #f #f #f)
  (def-value! array-proto "constructor" array-ctor)
  (def-method! array-ctor "isArray" 1
    (lambda (this args) (and (jso? (arg args 0)) (eq? (jso-exotic (arg args 0)) 'array)))
    function-proto)

  ;; 6. Math 命名空间对象（§21.3）
  (define math-obj (new-object object-proto))
  (define (m1 name fn) (def-method! math-obj name 1 (lambda (this args) (fn (to-number (arg args 0)))) function-proto))
  (m1 "floor" floor) (m1 "ceil" ceiling) (m1 "abs" abs) (m1 "trunc" truncate)
  (m1 "round" (lambda (x) (floor (+ x 0.5))))          ; JS Math.round = floor(x+0.5)
  (m1 "sqrt" (lambda (x) (if (< x 0) +nan.0 (sqrt x))))
  (def-method! math-obj "pow" 2 (lambda (this args) (expt (to-number (arg args 0)) (to-number (arg args 1)))) function-proto)
  (define (mvar name init fold)
    (def-method! math-obj name 2
      (lambda (this args)
        (define ns (map to-number args))
        (cond [(null? ns) init] [(ormap (lambda (x) (eqv? x +nan.0)) ns) +nan.0] [else (foldl fold (car ns) (cdr ns))]))
      function-proto))
  (mvar "max" -inf.0 max) (mvar "min" +inf.0 min)
  (def-value! math-obj "PI" 3.141592653589793 #f #f #f) (def-value! math-obj "E" 2.718281828459045 #f #f #f)

  (define r (realm object-proto function-proto array-proto string-proto object-ctor array-ctor math-obj make-ordinary))
  r)

;; JS 描述符对象 → 内部 Desc（immutable hash）
(define (to-descriptor obj)
  (unless (jso? obj) (error 'to-descriptor "descriptor must be object"))
  (define (has k) (js-has? obj k))
  (let* ([D (hasheq)]
         [D (if (has "value") (hash-set D 'value (js-get obj "value")) D)]
         [D (if (has "get") (hash-set D 'get (js-get obj "get")) D)]
         [D (if (has "set") (hash-set D 'set (js-get obj "set")) D)]
         [D (if (has "writable") (hash-set D 'w (to-boolean (js-get obj "writable"))) D)]
         [D (if (has "enumerable") (hash-set D 'e (to-boolean (js-get obj "enumerable"))) D)]
         [D (if (has "configurable") (hash-set D 'c (to-boolean (js-get obj "configurable"))) D)])
    D))

;; instanceof §13.10.2（默认 OrdinaryHasInstance）：F.prototype 是否在 v 原型链上
(define (js-instanceof v F)
  (unless (js-callable? F) (error 'instanceof "right side not callable"))
  (define proto (js-get F "prototype"))
  (and (jso? v)
       (let loop ([p (jso-proto v)])
         (cond [(eq? p the-null) #f] [(eq? p proto) #t] [else (loop (jso-proto p))]))))

(provide (struct-out realm) make-realm js-instanceof to-descriptor def-method! def-value! arg)
