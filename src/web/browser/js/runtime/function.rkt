#lang racket/base
;; browser/js/runtime/function.rkt — L6 · 函数对象与 [[Construct]]
;; 对标: ECMAScript 2023 §10.2（ordinary function）· §10.2.2 [[Construct]]（design-jsobj.md §5.2）
;; 依赖: racket/base browser/js/runtime/values
;; 说明: 函数对象 = exotic='function 的 jso（callable 协议在 values）。此处补 [[Construct]]
;;       与函数创建门面。.prototype/.name/.length 等属性由 realm.rkt 装配（避免反依赖）。

(require "values.rkt")

;; OrdinaryConstruct §10.2.2：以 F.prototype 为原型建新对象 O，调 F(this=O)；
;; 返回对象则用之，否则用 O。
(define (js-construct F args)
  (unless (js-callable? F) (error 'js-construct "not a constructor"))
  (define c (callable-construct (jso-xdata F)))
  (cond
    [(not c) (error 'js-construct "not a constructor")]
    [(procedure? c) (c args)]                           ; 自定义 [[Construct]]（Object/Array…）
    [else                                               ; 'ordinary
     (define p (js-get F "prototype"))
     (define O (new-object (if (jso? p) p the-null)))   ; 原型取自 F.prototype
     (define r ((callable-call (jso-xdata F)) O args))
     (if (jso? r) r O)]))

;; 普通函数门面：proc 为 (this args→val)。#:construct? → 可 new。
(define (make-function proc #:construct? [construct? #f] #:proto [proto the-null])
  (make-callable proc (and construct? 'ordinary) proto))

;; 内建方法门面：不可构造，proto = Function.prototype（realm 传入）。
(define (make-builtin proc [fn-proto the-null])
  (make-callable proc #f fn-proto))

;; 自定义构造器门面（Object/Array 等）：construct-proc 为 (args→jso)。
(define (make-constructor call-proc construct-proc [fn-proto the-null])
  (make-callable call-proc construct-proc fn-proto))

(provide js-construct make-function make-builtin make-constructor)
