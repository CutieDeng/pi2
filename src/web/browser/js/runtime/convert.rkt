#lang racket/base
;; browser/js/runtime/convert.rkt — L6 · 抽象操作（类型转换 + typeof + 相等）
;; 对标: ECMAScript 2023 §7.1 类型转换 · §7.2 类型比较 · §13.5.3 typeof（design-jsobj.md §7）
;; 依赖: racket/base browser/js/runtime/values

(require "values.rkt")

;; ---------------------------------------------------------------- 类型谓词
(define (js-object? v) (jso? v))
(define (js-number? v) (and (flonum? v) #t))     ; JS number 恒 flonum（含整数）
(define (js-string? v) (string? v))
(define (js-boolean? v) (boolean? v))
(define (js-symbol? v) (jsym? v))
(define (js-primitive? v) (not (jso? v)))

;; ---------------------------------------------------------------- ToBoolean §7.1.2
(define (to-boolean v)
  (cond [(boolean? v) v]
        [(js-undefined? v) #f]
        [(js-null? v) #f]
        [(flonum? v) (not (or (= v 0.0) (eqv? v +nan.0)))]  ; 0/-0/NaN → #f
        [(string? v) (> (string-length v) 0)]
        [(jsym? v) #t]
        [(jso? v) #t]                                        ; 对象恒真
        [else #t]))

;; ---------------------------------------------------------------- ToPrimitive §7.1.1
;; hint: 'number | 'string | 'default
(define (to-primitive v [hint 'default])
  (cond
    [(not (jso? v)) v]
    [else
     (define exotic-tp (js-get v sym:to-primitive))
     (cond
       [(js-callable? exotic-tp)
        (define r (js-call exotic-tp v (list (hint->string hint))))
        (if (jso? r) (error 'to-primitive "Symbol.toPrimitive returned object") r)]
       [else (ordinary-to-primitive v (if (eq? hint 'default) 'number hint))])]))
(define (hint->string h) (symbol->string h))
(define (ordinary-to-primitive v hint)
  (define order (if (eq? hint 'string) '("toString" "valueOf") '("valueOf" "toString")))
  (let loop ([ms order])
    (cond
      [(null? ms) (error 'to-primitive "cannot convert object to primitive")]
      [else
       (define m (js-get v (car ms)))
       (cond
         [(js-callable? m)
          (define r (js-call m v '()))
          (if (jso? r) (loop (cdr ms)) r)]
         [else (loop (cdr ms))])])))

;; ---------------------------------------------------------------- ToNumber §7.1.4
(define (to-number v)
  (cond
    [(flonum? v) v]
    [(boolean? v) (if v 1.0 0.0)]
    [(js-undefined? v) +nan.0]
    [(js-null? v) 0.0]
    [(string? v) (string->js-number v)]
    [(jsym? v) (error 'to-number "cannot convert symbol to number")]
    [(jso? v) (to-number (to-primitive v 'number))]
    [else +nan.0]))
(define (string->js-number s)
  (define t (string-trim-ws s))
  (cond [(string=? t "") 0.0]
        [(string=? t "Infinity") +inf.0] [(string=? t "+Infinity") +inf.0]
        [(string=? t "-Infinity") -inf.0]
        [else (let ([n (string->number t)]) (if (real? n) (exact->inexact n) +nan.0))]))
(define (string-trim-ws s)
  (list->string (reverse (drop-ws (reverse (drop-ws (string->list s)))))))
(define (drop-ws cs)
  (cond [(null? cs) '()]
        [(memv (car cs) '(#\space #\tab #\newline #\return)) (drop-ws (cdr cs))]
        [else cs]))

;; ---------------------------------------------------------------- ToString §7.1.17
(define (to-js-string v)
  (cond
    [(string? v) v]
    [(js-undefined? v) "undefined"]
    [(js-null? v) "null"]
    [(boolean? v) (if v "true" "false")]
    [(flonum? v) (number->js-string v)]
    [(jsym? v) (error 'to-string "cannot convert symbol to string")]
    [(jso? v) (to-js-string (to-primitive v 'string))]
    [else "undefined"]))
(define (number->js-string n)
  (cond [(eqv? n +nan.0) "NaN"]
        [(eqv? n +inf.0) "Infinity"] [(eqv? n -inf.0) "-Infinity"]
        [(and (= n (round n)) (< (abs n) 1e21))       ; 整数值 → 无小数点
         (number->string (inexact->exact (round n)))]
        [else (number->string n)]))

;; ---------------------------------------------------------------- ToPropertyKey §7.1.19
(define (to-property-key v)
  (cond [(jsym? v) v]
        [(string? v) v]
        [else (to-js-string v)]))    ; 数字/其它 → 规范字符串

;; ---------------------------------------------------------------- ToObject §7.1.18（最小）
;; jso 透传；null/undefined 报错；原始值包裹留待 realm 的 wrapper（§5.3）——此处仅透传/报错
(define (to-object v)
  (cond [(jso? v) v]
        [(or (js-undefined? v) (js-null? v)) (error 'to-object "cannot convert null/undefined to object")]
        [else (error 'to-object "primitive wrapping is realm-level (design-jsobj.md §5.3)")]))

;; ---------------------------------------------------------------- typeof §13.5.3
(define (js-typeof v)
  (cond [(js-undefined? v) "undefined"]
        [(js-null? v) "object"]                 ; 历史遗留：typeof null === "object"
        [(boolean? v) "boolean"]
        [(flonum? v) "number"]
        [(string? v) "string"]
        [(jsym? v) "symbol"]
        [(js-callable? v) "function"]
        [(jso? v) "object"]
        [else "undefined"]))

;; ---------------------------------------------------------------- 相等
(define (same-value-zero a b)   ; §7.2.11：同 SameValue 但 -0 == +0
  (cond [(and (flonum? a) (flonum? b) (= a 0.0) (= b 0.0)) #t]
        [else (same-value a b)]))

(define (strict-equals? a b)   ; === §7.2.15（IsStrictlyEqual）
  (cond [(and (flonum? a) (flonum? b)) (= a b)]      ; NaN≠NaN、-0===+0 均由 = 给出
        [(and (string? a) (string? b)) (string=? a b)]
        [(and (boolean? a) (boolean? b)) (eq? a b)]
        [else (eq? a b)]))                            ; null/undefined/symbol/对象按身份

(provide
 js-object? js-number? js-string? js-boolean? js-symbol? js-primitive?
 to-boolean to-primitive to-number to-js-string to-property-key to-object
 number->js-string js-typeof
 same-value-zero strict-equals?)
