#lang racket/base
;; browser/js/runtime/array.rkt — L6 · Array 奇异对象（design-jsobj.md §5.1）
;; 对标: ECMAScript 2023 §10.4.2（Array Exotic Objects）· §10.4.2.4 ArraySetLength
;; 依赖: racket/base browser/js/runtime/values browser/js/runtime/convert
;; 机制: 元素是普通整数索引数据属性（走 dict ikeys）；仅覆写 [[DefineOwnProperty]] 处理
;;       "length" 与索引的耦合。经 register-exotic-define! 注册（开放递归，values 不反依赖）。

(require "values.rkt" "convert.rkt")

(define (len-of o) (inexact->exact (prop-value (own-desc o "length"))))
(define (set-len! o n) (set-prop-value! (own-desc o "length") (exact->inexact n)))
(define (len-writable? o) (prop-w (own-desc o "length")))

(define (to-uint32 v)
  (define n (to-number v))
  (if (or (eqv? n +nan.0) (eqv? n +inf.0) (eqv? n -inf.0)) 0
      (modulo (inexact->exact (truncate n)) 4294967296)))

;; §10.4.2.4 ArraySetLength：定义 "length" 时的元素删除耦合
(define (array-set-length o D)
  (cond
    [(not (hash-has-key? D 'value)) (ordinary-define! o "length" D)]   ; 仅改特性
    [else
     (define new-len (to-uint32 (hash-ref D 'value)))
     (unless (= (exact->inexact new-len) (to-number (hash-ref D 'value)))
       (error 'array "Invalid array length"))
     (define old-len (len-of o))
     (cond
       [(>= new-len old-len)                                          ; 增长：仅设值
        (ordinary-define! o "length" (hash-set D 'value (exact->inexact new-len)))]
       [(not (len-writable? o)) #f]
       [else                                                          ; 收缩：从高到低删元素
        (let loop ([i (sub1 old-len)])
          (cond
            [(< i new-len) (set-len! o new-len) #t]
            [else
             (define k (number->string i))
             (cond
               [(not (own-desc o k)) (loop (sub1 i))]                 ; 稀疏空位
               [(js-delete! o k) (loop (sub1 i))]
               [else (set-len! o (add1 i)) #f])]))])]))               ; 撞不可配元素 → 停

;; §10.4.2.1 Array [[DefineOwnProperty]]
(define (array-define! o p D)
  (cond
    [(equal? p "length") (array-set-length o D)]
    [(array-index? p)
     (define idx (string->number p))
     (define old-len (len-of o))
     (cond
       [(and (>= idx old-len) (not (len-writable? o))) #f]            ; 越界且 length 只读
       [else
        (define ok (ordinary-define! o p D))
        (cond [(not ok) #f]
              [(>= idx old-len) (set-len! o (add1 idx)) #t]           ; 越界 → 涨 length
              [else #t])])]
    [else (ordinary-define! o p D)]))

(register-exotic-define! 'array array-define!)

;; 构造门面：exotic='array 的 jso + "length"(w#t e#f c#f) 数据属性
(define (make-array/proto proto)
  (define a (new-exotic 'array proto))
  (ordinary-define! a "length" (hasheq 'value 0.0 'w #t 'e #f 'c #f))
  a)
(define (array-push! a v)
  (define n (len-of a))
  (js-define! a (number->string n) (hasheq 'value v 'w #t 'e #t 'c #t))   ; 经钩子涨 length
  (len-of a))
(define (array->list-values a)
  (for/list ([i (in-range (len-of a))]) (js-get a (number->string i))))

(provide array-define! make-array/proto array-push! array->list-values len-of)
