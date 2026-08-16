#lang racket/base
;; tests/browser/l6-realm-test.rkt — L6 JS 核心对象框架端到端
;; 把 values+convert+function+array+realm 串起来，验证一个可用的 JS 对象世界

(require rackunit
         (file "../../src/web/browser/js/runtime/values.rkt")
         (file "../../src/web/browser/js/runtime/convert.rkt")
         (file "../../src/web/browser/js/runtime/function.rkt")
         (file "../../src/web/browser/js/runtime/array.rkt")
         (file "../../src/web/browser/js/runtime/realm.rkt"))

(define R (make-realm))
(define (ordinary) ((realm-make-ordinary R)))
(define (data v) (hasheq 'value v 'w #t 'e #t 'c #t))

;; ------------------------------------------------------------ 原型链 + Object.prototype 方法
(test-case "普通对象继承 Object.prototype；hasOwnProperty 区分自有/继承"
  (define o (ordinary))
  (js-define! o "x" (data 1.0))
  (check-equal? (js-get o "x") 1.0)
  ;; hasOwnProperty 是从 Object.prototype 继承来的方法
  (define hop (js-get o "hasOwnProperty"))
  (check-true (js-callable? hop))
  (check-eq? (js-call hop o (list "x")) #t)         ; 自有
  (check-eq? (js-call hop o (list "hasOwnProperty")) #f)  ; 继承的不是自有
  (check-eq? (js-call hop o (list "toString")) #f))

;; ------------------------------------------------------------ Object 静态方法
(test-case "Object.create/keys/getPrototypeOf/defineProperty"
  (define proto (ordinary))
  (js-define! proto "inherited" (data 'P))
  (define create (js-get (realm-object-ctor R) "create"))
  (define o (js-call create the-undefined (list proto)))
  (check-eq? (js-get-proto o) proto)
  (check-equal? (js-get o "inherited") 'P)          ; 经原型链
  ;; defineProperty：装一个不可枚举属性
  (define defp (js-get (realm-object-ctor R) "defineProperty"))
  (define descobj (ordinary))
  (js-define! descobj "value" (data 42.0))
  (js-define! descobj "enumerable" (data #f))
  (js-call defp the-undefined (list o "hidden" descobj))
  (check-equal? (js-get o "hidden") 42.0)
  ;; Object.keys 只列自有可枚举
  (js-define! o "vis" (data 'V))
  (define keys-fn (js-get (realm-object-ctor R) "keys"))
  (define ks (js-call keys-fn the-undefined (list o)))
  (check-equal? (array->list-values ks) (list "vis")))   ; hidden 不可枚举、inherited 非自有

;; ------------------------------------------------------------ Array 奇异对象
(test-case "数组 length↔元素耦合"
  (define a (make-array/proto (realm-array-proto R)))
  (check-equal? (len-of a) 0)
  (array-push! a 'x) (array-push! a 'y) (array-push! a 'z)
  (check-equal? (len-of a) 3)                        ; push 涨 length
  (check-equal? (js-get a "1") 'y)
  ;; 写越界索引 → length 涨
  (js-define! a "5" (data 'far))
  (check-equal? (len-of a) 6)
  ;; 缩 length → 删元素
  (js-define! a "length" (hasheq 'value 2.0))
  (check-equal? (len-of a) 2)
  (check-true (js-undefined? (js-get a "2")))        ; 被删
  (check-equal? (js-get a "0") 'x))

(test-case "Array.prototype.join/indexOf/push；Array.isArray"
  (define a (make-array/proto (realm-array-proto R)))
  (array-push! a "a") (array-push! a "b") (array-push! a "c")
  (check-equal? (js-call (js-get a "join") a (list "-")) "a-b-c")
  (check-equal? (js-call (js-get a "indexOf") a (list "b")) 1.0)
  (check-equal? (js-call (js-get a "indexOf") a (list "z")) -1.0)
  (define isarr (js-get (realm-array-ctor R) "isArray"))
  (check-eq? (js-call isarr the-undefined (list a)) #t)
  (check-eq? (js-call isarr the-undefined (list (ordinary))) #f))

;; ------------------------------------------------------------ 函数 / new / instanceof
(test-case "自定义构造器 + new + 原型方法 + instanceof"
  ;; function Point(x,y){ this.x=x; this.y=y }
  (define point-proto (ordinary))
  (define Point
    (make-function
     (lambda (this args)
       (js-set! this "x" (arg args 0))
       (js-set! this "y" (arg args 1))
       the-undefined)
     #:construct? #t #:proto (realm-function-proto R)))
  (js-define! Point "prototype" (hasheq 'value point-proto 'w #t 'e #f 'c #f))
  (js-define! point-proto "constructor" (hasheq 'value Point 'w #t 'e #f 'c #t))
  ;; Point.prototype.sum = function(){ return this.x + this.y }
  (js-define! point-proto "sum"
    (data (make-function
           (lambda (this args) (+ (js-get this "x") (js-get this "y")))
           #:proto (realm-function-proto R))))
  ;; new Point(3,4)
  (define p (js-construct Point (list 3.0 4.0)))
  (check-equal? (js-get p "x") 3.0)
  (check-equal? (js-get p "y") 4.0)
  (check-eq? (js-get-proto p) point-proto)
  (check-equal? (js-call (js-get p "sum") p '()) 7.0)   ; 继承的方法，this=p
  (check-true (js-instanceof p Point))                  ; p instanceof Point
  (check-false (js-instanceof (ordinary) Point)))

;; ------------------------------------------------------------ 转换 / typeof
(test-case "typeof / ToString / ToNumber / ToBoolean / === "
  (check-equal? (js-typeof the-undefined) "undefined")
  (check-equal? (js-typeof the-null) "object")          ; 历史遗留
  (check-equal? (js-typeof 3.0) "number")
  (check-equal? (js-typeof "s") "string")
  (check-equal? (js-typeof (make-function (lambda (t a) t) #:proto (realm-function-proto R))) "function")
  (check-equal? (js-typeof (ordinary)) "object")
  (check-equal? (to-js-string 3.0) "3")                 ; 整数值无小数点
  (check-equal? (to-js-string 3.5) "3.5")
  (check-equal? (to-js-string the-null) "null")
  (check-equal? (to-number "42") 42.0)
  (check-true (eqv? (to-number "x") +nan.0))
  (check-false (to-boolean 0.0)) (check-true (to-boolean 1.0))
  (check-false (to-boolean "")) (check-true (to-boolean "a"))
  (check-true (strict-equals? 1.0 1.0))
  (check-false (strict-equals? +nan.0 +nan.0))          ; NaN !== NaN
  (check-false (strict-equals? 1.0 "1")))               ; 无隐式转换

;; ------------------------------------------------------------ realm 隔离
(test-case "两个 realm 的 Object.prototype 互不相同（§11 隔离）"
  (define R2 (make-realm))
  (check-false (eq? (realm-object-proto R) (realm-object-proto R2)))
  ;; 污染一个 realm 的原型不影响另一个
  (js-define! (realm-object-proto R) "polluted" (data 'YES))
  (check-equal? (js-get (ordinary) "polluted") 'YES)
  (check-true (js-undefined? (js-get ((realm-make-ordinary R2)) "polluted"))))
