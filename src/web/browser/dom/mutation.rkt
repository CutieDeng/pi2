#lang racket/base
;; browser/dom/mutation.rkt — L3 · 运行期变更面（计数）（design-chrome.md L3 / §2.3）
;; 职责: ddoc(树根+变更计数) + 计数版变更原语——L9 安定态的信号源
;; 不做: HTML 解析(innerHTML 字符串在 L8 接线)、结构原语本体(在 node.rkt)
;; 分工: L4 建树期用 node.rkt(不计数)；页面运行期变更一律走本模块
;; 依赖: racket/base browser/dom/node

(require racket/contract "node.rkt")

(provide
 (contract-out
  [ddoc? (-> any/c boolean?)]
  [make-document (->* (dnode?) ((or/c string? #f)) ddoc?)]
  [ddoc-root (-> ddoc? dnode?)]
  [ddoc-url (-> ddoc? (or/c string? #f))]
  [ddoc-mutations (-> ddoc? exact-nonnegative-integer?)]
  [dom-append! (-> ddoc? dnode? node? void?)]
  [dom-insert-before! (-> ddoc? dnode? node? (or/c node? #f) void?)]
  [dom-remove! (-> ddoc? node? void?)]
  [dom-replace-children! (-> ddoc? dnode? (listof node?) void?)]
  [dom-set-text! (-> ddoc? dtext? string? void?)]
  [dom-set-attr! (-> ddoc? dnode? symbol? string? void?)]
  [dom-remove-attr! (-> ddoc? dnode? symbol? void?)]
 ) ; end contract-out
) ; end provide

(struct ddoc (root url [mut-count #:mutable]))

(define (make-document root [url #f])
  (ddoc root url 0)
) ; end define make-document

(define (ddoc-mutations d) (ddoc-mut-count d))

(define (bump! d)
  (set-ddoc-mut-count! d (add1 (ddoc-mut-count d)))
) ; end define bump!

;; 计数版原语：node.rkt 同名操作 + bump

(define (dom-append! d parent child)
  (node-append! parent child) (bump! d)
) ; end define dom-append!

(define (dom-insert-before! d parent child ref)
  (node-insert-before! parent child ref) (bump! d)
) ; end define dom-insert-before!

(define (dom-remove! d n)
  (node-remove! n) (bump! d)
) ; end define dom-remove!

(define (dom-replace-children! d parent nodes)
  (node-replace-children! parent nodes) (bump! d)
) ; end define dom-replace-children!

(define (dom-set-text! d t s)
  (node-set-text! t s) (bump! d)
) ; end define dom-set-text!

(define (dom-set-attr! d n k v)
  (node-set-attr! n k v) (bump! d)
) ; end define dom-set-attr!

(define (dom-remove-attr! d n k)
  (node-remove-attr! n k) (bump! d)
) ; end define dom-remove-attr!
