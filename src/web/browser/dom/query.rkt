#lang racket/base
;; browser/dom/query.rkt — L3 · 遍历与查询（design-chrome.md L3 / §2.3）
;; 职责: 谓词查询(先序)/剪枝遍历/文本聚合——只吃匹配闭包，不认识选择器语法
;;       (选择器字符串→闭包在 L5 编译；querySelector 字符串入口在 L8 接线)
;; 不做: 选择器解析、序列化
;; 依赖: racket/base browser/dom/node

(require racket/contract "node.rkt")

(provide
 (contract-out
  [dom-query (->* (node? (-> dnode? any/c)) (#:limit (or/c exact-positive-integer? #f))
                  (listof dnode?))]
  [dom-walk (-> node? (-> node? any/c) void?)]
  [dom-text-content (-> node? string?)]
 ) ; end contract-out
) ; end provide

;; 先序收集满足 pred 的元素节点（不含文本节点——querySelector 语义）。
(define (dom-query root pred #:limit [limit #f])
  (define out '())
  (define count 0)
  (let/ec done
    (let walk ([n root])
      (when (dnode? n)
        (when (pred n)
          (set! out (cons n out))
          (set! count (add1 count))
          (when (and limit (>= count limit)) (done (void)))
        ) ; end when
        (for ([c (in-children n)]) (walk c))
      ) ; end when
    ) ; end walk
  ) ; end let/ec
  (reverse out)
) ; end define dom-query

;; 先序全节点遍历；f 返回 'skip-subtree 则不下探该子树（extract 遍历契约）。
(define (dom-walk root f)
  (let walk ([n root])
    (define r (f n))
    (unless (eq? r 'skip-subtree)
      (for ([c (in-children n)]) (walk c))
    ) ; end unless
  ) ; end walk
  (void)
) ; end define dom-walk

;; 后代 dtext 按文档序拼接（textContent 语义）。
(define (dom-text-content n)
  (define parts '())
  (dom-walk n (lambda (x)
                (when (dtext? x) (set! parts (cons (dtext-s x) parts)))
                (void)))
  (apply string-append (reverse parts))
) ; end define dom-text-content
