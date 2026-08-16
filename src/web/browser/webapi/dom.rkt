#lang racket/base
;; browser/webapi/dom.rkt — L8 · DOM webapi（L3 树 ↔ L6 对象的桥）
;; 对标: WHATWG DOM（Document/Element 子集）（design-jsobj.md §11 · design-chrome.md L8）
;; 依赖: racket/base + L3 dom + L4 fragment + L5 css + L6 runtime（全部向下）
;; 机制: dnode 包一层 jso（proto=Element.prototype，xdata=dnode，弱缓存保 === 稳定）；
;;   DOM 属性做成 Element.prototype 上的访问器（getter/setter 读/写 this.xdata 的 dnode）；
;;   变更经 ddoc 的 dom-*! 走（计入 mutation，供 L9 安定态）。

(require "../dom/node.rkt" "../dom/mutation.rkt" "../dom/query.rkt" "../dom/serialize.rkt"
         "../html/fragment.rkt" "../css/match.rkt"
         "../js/runtime/values.rkt" "../js/runtime/convert.rkt"
         "../js/runtime/function.rkt" "../js/runtime/array.rkt" "../js/runtime/realm.rkt")

;; (make-dom-api R ddoc) → document jso（内部装好 Element.prototype 与包裹缓存）
(define (make-dom-api R ddoc)
  (define obj-proto (realm-object-proto R))
  (define fn-proto (realm-function-proto R))
  (define arr-proto (realm-array-proto R))
  (define element-proto (new-object obj-proto))
  (define cache (make-hasheq))                 ; dnode → jso（同一节点同一包裹）

  (define (wrap n)
    (cond [(not (dnode? n)) the-null]
          [(hash-ref cache n #f) => values]
          [else (define o (new-exotic 'dom element-proto n)) (hash-set! cache n o) o]))
  (define (dn this) (jso-xdata this))          ; this 的底层 dnode

  ;; ---- 访问器（DOM 属性）
  (define (def-accessor! o name getter [setter #f])
    (define g (make-builtin getter fn-proto))
    (define D (hasheq 'get g 'e #t 'c #t))
    (js-define! o name (if setter (hash-set D 'set (make-builtin setter fn-proto)) D)))

  (def-accessor! element-proto "tagName"
    (lambda (this args) (string-upcase (symbol->string (dnode-tag (dn this))))))
  (def-accessor! element-proto "id"
    (lambda (this args) (or (node-attr (dn this) 'id #f) ""))
    (lambda (this args) (dom-set-attr! ddoc (dn this) 'id (to-js-string (arg args 0))) the-undefined))
  (def-accessor! element-proto "className"
    (lambda (this args) (or (node-attr (dn this) 'class #f) ""))
    (lambda (this args) (dom-set-attr! ddoc (dn this) 'class (to-js-string (arg args 0))) the-undefined))
  (def-accessor! element-proto "textContent"
    (lambda (this args) (dom-text-content (dn this)))
    (lambda (this args)
      (dom-replace-children! ddoc (dn this) (list (make-text (to-js-string (arg args 0))))) the-undefined))
  (def-accessor! element-proto "innerHTML"
    (lambda (this args) (apply string-append (map dom->html (node-children (dn this)))))
    (lambda (this args)
      (define nodes (parse-fragment (to-js-string (arg args 0)) (dnode-tag (dn this))))
      (dom-replace-children! ddoc (dn this) nodes) the-undefined))
  (def-accessor! element-proto "children"
    (lambda (this args)
      (define a (make-array/proto arr-proto))
      (for ([c (in-list (node-children (dn this)))] #:when (dnode? c)) (array-push! a (wrap c)))
      a))

  ;; ---- 方法
  (define (defm! o name arity proc) (def-method! o name arity proc fn-proto))
  (defm! element-proto "getAttribute" 1
    (lambda (this args) (or (node-attr (dn this) (string->symbol (to-js-string (arg args 0))) #f) the-null)))
  (defm! element-proto "setAttribute" 2
    (lambda (this args)
      (dom-set-attr! ddoc (dn this) (string->symbol (to-js-string (arg args 0))) (to-js-string (arg args 1)))
      the-undefined))
  (defm! element-proto "appendChild" 1
    (lambda (this args) (define child (arg args 0)) (dom-append! ddoc (dn this) (dn child)) child))
  (defm! element-proto "querySelector" 1
    (lambda (this args) (query1 (dn this) (to-js-string (arg args 0)) wrap)))
  (defm! element-proto "querySelectorAll" 1
    (lambda (this args) (query-all (dn this) (to-js-string (arg args 0)) wrap arr-proto)))

  ;; ---- document
  (define root (ddoc-root ddoc))
  (define document (new-object obj-proto))
  (defm! document "getElementById" 1
    (lambda (this args)
      (define id (to-js-string (arg args 0)))
      (define hits (dom-query root (lambda (n) (equal? (node-attr n 'id #f) id)) #:limit 1))
      (if (pair? hits) (wrap (car hits)) the-null)))
  (defm! document "querySelector" 1 (lambda (this args) (query1 root (to-js-string (arg args 0)) wrap)))
  (defm! document "querySelectorAll" 1 (lambda (this args) (query-all root (to-js-string (arg args 0)) wrap arr-proto)))
  (defm! document "createElement" 1
    (lambda (this args) (wrap (make-element (string->symbol (string-downcase (to-js-string (arg args 0))))))))
  (define body-el (let ([b (dom-query root (lambda (n) (eq? (dnode-tag n) 'body)) #:limit 1)])
                    (if (pair? b) (car b) root)))
  (js-define! document "body" (hasheq 'value (wrap body-el) 'w #t 'e #t 'c #t))
  (js-define! document "documentElement" (hasheq 'value (wrap root) 'w #t 'e #t 'c #t))

  (values document wrap))

(define (query1 root sel wrap)
  (define pred (compile-selector sel))
  (define hits (dom-query root pred #:limit 1))
  (if (pair? hits) (wrap (car hits)) the-null))
(define (query-all root sel wrap arr-proto)
  (define pred (compile-selector sel))
  (define a (make-array/proto arr-proto))
  (for ([n (in-list (dom-query root pred))]) (array-push! a (wrap n)))
  a)

(provide make-dom-api)
