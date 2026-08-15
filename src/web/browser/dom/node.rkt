#lang racket/base
;; browser/dom/node.rkt — L3 · 树结构原语（design-chrome.md L3 / §2.3）
;; 职责: 结构不变量下的最小变更集（parent/children 互指、无环——构造性维护）
;; 不做: 变更计数/选择器/序列化/HTML 解析
;; 依赖: racket/base racket/pvector

(require racket/contract racket/pvector racket/sequence)

(provide
 (contract-out
  [dnode? (-> any/c boolean?)]
  [dtext? (-> any/c boolean?)]
  [node?  (-> any/c boolean?)]
  [make-element (->* (symbol?) ((listof (cons/c symbol? string?))) dnode?)]
  [make-text (-> string? dtext?)]
  [dnode-tag (-> dnode? symbol?)]
  [dtext-s (-> dtext? string?)]
  [node-parent (-> node? (or/c dnode? #f))]
  [node-append! (-> dnode? node? void?)]
  [node-insert-before! (-> dnode? node? (or/c node? #f) void?)]
  [node-remove! (-> node? void?)]
  [node-replace-children! (-> dnode? (listof node?) void?)]
  [node-set-text! (-> dtext? string? void?)]
  [node-set-attr! (-> dnode? symbol? string? void?)]
  [node-remove-attr! (-> dnode? symbol? void?)]
  [node-attr (->* (dnode? symbol?) (any/c) any/c)]
  [node-attr-names (-> dnode? (listof symbol?))]
  [in-children (-> node? sequence?)]
  [node-children (-> node? (listof node?))]
  [node-child-count (-> node? exact-nonnegative-integer?)]
  [check-tree! (-> node? void?)]
 ) ; end contract-out
) ; end provide

;; ---------------------------------------------------------------- 结构
;; 裸构造器与裸 mutator 不出层：所有可达变更都经下方原语（§2.2 规约 2）。

(struct dnode (tag [attrs #:mutable] [children #:mutable] [parent #:mutable]))
(struct dtext ([s #:mutable] [parent #:mutable]))

(define (node? v) (or (dnode? v) (dtext? v)))

(define (make-element tag [attrs '()])
  (define h (make-hasheq))
  (for ([kv (in-list attrs)])
    (hash-set! h (car kv) (cdr kv))
  ) ; end for
  (dnode tag h (pvector) #f)
) ; end define make-element

(define (make-text s) (dtext s #f))

(define (node-parent n)
  (if (dnode? n) (dnode-parent n) (dtext-parent n))
) ; end define node-parent

(define (set-parent! n p)
  (if (dnode? n) (set-dnode-parent! n p) (set-dtext-parent! n p))
) ; end define set-parent!

;; ---------------------------------------------------------------- 遍历面

(define (in-children n)
  (if (dnode? n) (in-pvector (dnode-children n)) empty-sequence)
) ; end define in-children

(define (node-children n)
  (for/list ([c (in-children n)]) c)
) ; end define node-children

(define (node-child-count n)
  (if (dnode? n) (pvector-length (dnode-children n)) 0)
) ; end define node-child-count

;; ---------------------------------------------------------------- 变更原语

(define (ancestor-of? a n)   ; a 是否 n 的祖先(含自身)
  (let loop ([x n])
    (cond [(not x) #f]
          [(eq? x a) #t]
          [else (loop (node-parent x))])
  ) ; end loop
) ; end define ancestor-of?

(define (assert-no-cycle! who parent child)
  (when (and (dnode? child) (ancestor-of? child parent))
    (error who "cycle: child is an ancestor of parent")
  ) ; end when
) ; end define assert-no-cycle!

(define (detach! n)
  (define p (node-parent n))
  (when p
    (set-dnode-children! p (for/pvector ([c (in-pvector (dnode-children p))]
                                         #:unless (eq? c n))
                             c))
    (set-parent! n #f)
  ) ; end when
) ; end define detach!

(define (node-append! parent child)
  (assert-no-cycle! 'node-append! parent child)
  (detach! child)
  (set-parent! child parent)
  (set-dnode-children! parent (pvector-cons-right (dnode-children parent) child))
) ; end define node-append!

(define (node-insert-before! parent child ref)
  (cond
    [(not ref) (node-append! parent child)]
    [else
     (unless (eq? (node-parent ref) parent)
       (error 'node-insert-before! "ref is not a child of parent")
     ) ; end unless
     (assert-no-cycle! 'node-insert-before! parent child)
     (detach! child)
     (set-parent! child parent)
     (define kids
       (for/fold ([acc '()]) ([c (in-pvector (dnode-children parent))])
         (if (eq? c ref) (cons c (cons child acc)) (cons c acc))
       ) ; end for/fold
     ) ; end define kids
     (set-dnode-children! parent (list->pvector (reverse kids)))]
  ) ; end cond
) ; end define node-insert-before!

(define (node-remove! n) (detach! n))

(define (node-replace-children! parent nodes)
  ;; 旧子全部脱父；新子各自脱旧父后挂入（一次成树，O(旧+新)）
  (for ([c (in-pvector (dnode-children parent))]) (set-parent! c #f))
  (set-dnode-children! parent (pvector))
  (for ([c (in-list nodes)])
    (node-append! parent c)
  ) ; end for
) ; end define node-replace-children!

(define (node-set-text! t s) (set-dtext-s! t s))

(define (node-set-attr! n k v) (hash-set! (dnode-attrs n) k v))
(define (node-remove-attr! n k) (hash-remove! (dnode-attrs n) k))

(define (node-attr n k [dflt #f])
  (hash-ref (dnode-attrs n) k dflt)
) ; end define node-attr

(define (node-attr-names n)
  (sort (hash-keys (dnode-attrs n)) symbol<?)
) ; end define node-attr-names

;; ---------------------------------------------------------------- 不变量验证
;; §7.1 的可运行陈述：parent/children 互指、无环（eq 判重）。dev/CI 开。

(define (check-tree! root)
  (define seen (make-hasheq))
  (let walk ([n root])
    (when (hash-ref seen n #f)
      (error 'check-tree! "node visited twice (cycle or shared subtree)")
    ) ; end when
    (hash-set! seen n #t)
    (for ([c (in-children n)])
      (unless (eq? (node-parent c) n)
        (error 'check-tree! "child's parent pointer does not point back")
      ) ; end unless
      (walk c)
    ) ; end for
  ) ; end walk
  (void)
) ; end define check-tree!
