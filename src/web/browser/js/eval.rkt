#lang racket/base
;; browser/js/eval.rkt — L7 · 驱动：lex → parse → compile → eval（设计 A 端到端）
;; 依赖: racket/base browser/js/{lex,parse,compile} browser/js/runtime/{values,convert,function,array,realm}
;; run-js: JS 源码字符串 → js 值。每次 run 建独立 realm（隔离对象世界）。
;; prelude: 把 __js_* 宿主函数与 realm 全局注入一个 base namespace，再 eval 编译出的 Racket。

(require "lex.rkt" "parse.rkt" "compile.rkt"
         "runtime/values.rkt" "runtime/convert.rkt"
         "runtime/function.rkt" "runtime/array.rkt" "runtime/realm.rkt")

;; #:realm 复用外部 realm（页面场景须与 document 共享）；#:globals 追加全局绑定
;; （(name . js值)，如 (cons "document" doc)）——doc 是普通 jso，故 run-js 不依赖 L8。
(define (run-js src #:realm [R0 #f] #:globals [extra '()])
  (define R (or R0 (make-realm)))
  (define fn-proto (realm-function-proto R))
  (define obj-proto (realm-object-proto R))
  (define arr-proto (realm-array-proto R))
  (define str-proto (realm-string-proto R))
  (define globals (make-hash))
  (hash-set! globals "Object" (realm-object-ctor R))
  (hash-set! globals "Array" (realm-array-ctor R))
  (hash-set! globals "Math" (realm-math-obj R))
  (for ([g (in-list extra)]) (hash-set! globals (car g) (cdr g)))

  ;; ---- __js_* 宿主前缀
  ;; §7.1.18 ToObject 的实用包裹：字符串原始值的 length/索引直取，方法经 String.prototype
  (define (g:get o k)
    (cond
      [(jso? o) (js-get o (to-property-key k))]
      [(string? o)
       (define key (to-property-key k))
       (cond [(equal? key "length") (exact->inexact (string-length o))]
             [(and (string? key) (regexp-match? #px"^[0-9]+$" key))
              (define i (string->number key))
              (if (< i (string-length o)) (substring o i (add1 i)) the-undefined)]
             [else (js-get str-proto key)])]
      [else (error 'js "cannot read property of primitive")]))
  (define (g:set! o k v) (when (jso? o) (js-set! o (to-property-key k) v)) v)
  (define (g:arg args i) (if (< i (length args)) (list-ref args i) the-undefined))
  (define (g:func proc)
    (define f (make-function proc #:construct? #t #:proto fn-proto))
    (define p (new-object obj-proto))                    ; F.prototype（可 new / instanceof）
    (ordinary-define! p "constructor" (hasheq 'value f 'w #t 'e #f 'c #t))
    (ordinary-define! f "prototype" (hasheq 'value p 'w #t 'e #f 'c #f))
    f)
  (define (g:object pairs)
    (define o (new-object obj-proto))
    (for ([kv (in-list pairs)]) (js-define! o (car kv) (hasheq 'value (cdr kv) 'w #t 'e #t 'c #t)))
    o)
  (define (g:array elems)
    (define a (make-array/proto arr-proto))
    (for ([v (in-list elems)]) (array-push! a v)) a)
  (define (g:global name) (hash-ref globals name (lambda () (error 'js "~a is not defined" name))))
  ;; typeof 不可解析引用 → "undefined"（§13.5.3），不查会报错
  (define (g:typeof-global name) (if (hash-has-key? globals name) (js-typeof (hash-ref globals name)) "undefined"))
  (define (g:global-set! name v) (hash-set! globals name v) v)
  ;; 算术/比较（JS 语义子集）
  (define (num v) (to-number v))
  (define (g:add a b)
    (define pa (to-primitive a)) (define pb (to-primitive b))
    (if (or (string? pa) (string? pb)) (string-append (to-js-string pa) (to-js-string pb))
        (+ (num pa) (num pb))))
  (define (both-str? a b) (and (string? a) (string? b)))
  (define (g:lt a b) (if (both-str? a b) (string<? a b) (< (num a) (num b))))
  (define (g:gt a b) (if (both-str? a b) (string>? a b) (> (num a) (num b))))
  (define (g:le a b) (if (both-str? a b) (string<=? a b) (<= (num a) (num b))))
  (define (g:ge a b) (if (both-str? a b) (string>=? a b) (>= (num a) (num b))))

  (define bindings
    (list (cons '__js_undef the-undefined) (cons '__js_null the-null) (cons 'this the-undefined)
          (cons '__js_get g:get) (cons '__js_set! g:set!) (cons '__js_key to-property-key)
          (cons '__js_call js-call) (cons '__js_construct js-construct)
          (cons '__js_arg g:arg) (cons '__js_func g:func)
          (cons '__js_object g:object) (cons '__js_array g:array)
          (cons '__js_global g:global) (cons '__js_global_set! g:global-set!)
          (cons '__js_add g:add)
          (cons '__js_sub (lambda (a b) (- (num a) (num b)))) (cons '__js_mul (lambda (a b) (* (num a) (num b))))
          (cons '__js_div (lambda (a b) (/ (num a) (num b)))) (cons '__js_mod (lambda (a b) (flmod (num a) (num b))))
          (cons '__js_lt g:lt) (cons '__js_gt g:gt) (cons '__js_le g:le) (cons '__js_ge g:ge)
          (cons '__js_seq strict-equals?) (cons '__js_sne (lambda (a b) (not (strict-equals? a b))))
          (cons '__js_eq strict-equals?) (cons '__js_ne (lambda (a b) (not (strict-equals? a b))))
          (cons '__js_instanceof (lambda (v F) (js-instanceof v F)))
          (cons '__js_truthy to-boolean) (cons '__js_not (lambda (v) (not (to-boolean v))))
          (cons '__js_neg (lambda (v) (- (num v)))) (cons '__js_num num) (cons '__js_typeof js-typeof)
          (cons '__js_typeof_global g:typeof-global)))

  (define ns (make-base-namespace))
  (for ([b (in-list bindings)]) (namespace-set-variable-value! (car b) (cdr b) #t ns))
  (eval (compile-program (cadr (parse src))) ns))

(define (flmod a b) (define r (- a (* b (truncate (/ a b))))) r)

;; JS 值 → 可读字符串（调试/测试）
(define (js->display v) (to-js-string v))

(provide run-js js->display)
