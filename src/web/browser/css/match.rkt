#lang racket/base
;; browser/css/match.rkt — L5 · AST → 匹配闭包（design-chrome.md L5 / §2.3）
;; 职责: 选择器编译成 (dnode → bool)——编译一次反复用(页生命周期内闭包可长持)
;; 不做: 文法解析(selparse)、级联(style)
;; 依赖: racket/base browser/css/selparse browser/dom/node

(require racket/contract racket/string
         "selparse.rkt"
         "../dom/node.rkt")

(provide
 (contract-out
  [compile-selector (-> string? (-> dnode? boolean?))]
  [compile-selector-ast (-> list? (-> dnode? boolean?))]
 ) ; end contract-out
) ; end provide

;; simple → (dnode → bool)
(define (compile-simple sp)
  (case (car sp)
    [(tag)   (let ([t (cadr sp)]) (lambda (n) (eq? (dnode-tag n) t)))]
    [(id)    (let ([v (cadr sp)]) (lambda (n) (equal? (node-attr n 'id) v)))]
    [(class) (let ([v (cadr sp)])
               (lambda (n)
                 (define cls (node-attr n 'class))
                 (and cls (member v (string-split cls)) #t)))]
    [(attr)  (let ([k (cadr sp)]) (lambda (n) (and (node-attr n k) #t)))]
    [(attr=) (let ([k (cadr sp)] [v (caddr sp)])
               (lambda (n) (equal? (node-attr n k) v)))]
    [(not)   (let ([f (compile-compound (cadr sp))]) (lambda (n) (not (f n))))]
    [(pseudo) (lambda (n) #f)]   ; 伪类布局族恒假(设计既定)
    [else (error 'compile-simple "unknown simple: ~s" sp)]
  ) ; end case
) ; end define compile-simple

;; compound(空表=通配) → (dnode → bool)
(define (compile-compound cpd)
  (cond
    [(null? cpd) (lambda (n) #t)]
    [else
     (define fs (map compile-simple cpd))
     (lambda (n) (for/and ([f (in-list fs)]) (f n)))]
  ) ; end cond
) ; end define compile-compound

;; complex：右端 compound 在节点上匹配后，沿 steps(右→左)上溯。
;; descendant 需回溯：任一满足该步的祖先，其余步继续成立即可。
(define (compile-complex cx)
  (define rightmost (compile-compound (car cx)))
  (define steps
    (for/list ([st (in-list (cdr cx))])
      (cons (car st) (compile-compound (cdr st)))))
  (define (match-steps n rest)
    (cond
      [(null? rest) #t]
      [else
       (define comb (caar rest))
       (define f (cdar rest))
       (case comb
         [(child)
          (define p (node-parent n))
          (and p (f p) (match-steps p (cdr rest)))]
         [(descendant)
          (let loop ([p (node-parent n)])
            (cond [(not p) #f]
                  [(and (f p) (match-steps p (cdr rest))) #t]
                  [else (loop (node-parent p))]))]
       ) ; end case
      ] ; end else
    ) ; end cond
  ) ; end define match-steps
  (lambda (n) (and (rightmost n) (match-steps n steps)))
) ; end define compile-complex

(define (compile-selector-ast ast)
  (define alts (map compile-complex ast))
  (lambda (n) (for/or ([f (in-list alts)]) (and (f n) #t)))
) ; end define compile-selector-ast

(define (compile-selector str)
  (compile-selector-ast (parse-selector str))
) ; end define compile-selector
