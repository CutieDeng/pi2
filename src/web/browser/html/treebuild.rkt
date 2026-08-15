#lang racket/base
;; browser/html/treebuild.rkt — L4 · 树构建（design-chrome.md L4 / §2.3）
;; 职责: token 流 → L3 树。开放元素栈 + 隐式闭合表(top-only 子集) + EOF 全栈弹出
;;       + html/head/body 合成与 head 路由
;; 已知偏差(记 §7.2 manifest): 无 foster parenting、无 formatting 重建、注释节点丢弃、
;;       隐式闭合只看栈顶(深嵌套错位交给容错)
;; 规约: 建树期用 node.rkt 原语(不计数)；对任意输入永不 raise(§2.2 规约 4)
;; 依赖: racket/base browser/html/tokenizer browser/dom/{node,mutation}

(require racket/contract racket/string
         "tokenizer.rkt"
         "../dom/node.rkt"
         "../dom/mutation.rkt")

(provide
 (contract-out
  [parse-html (->* (string?) ((or/c string? #f)) ddoc?)]
  [tokens->children (-> (-> any/c) dnode? void?)]
 ) ; end contract-out
) ; end provide

(define VOID-TAGS
  '(area base br col embed hr img input link meta source track wbr))

(define HEAD-TAGS '(base link meta title style script noscript template))

(define P-CLOSERS
  '(address article aside blockquote details div dl fieldset figcaption figure
    footer form h1 h2 h3 h4 h5 h6 header hgroup hr main menu nav ol p pre
    section table ul))

;; 隐式闭合(top-only 实用子集): 栈顶为 top 时,incoming 到来是否先关 top
(define (closes-top? top incoming)
  (case top
    [(p) (memq incoming P-CLOSERS)]
    [(li) (eq? incoming 'li)]
    [(dt dd) (memq incoming '(dt dd))]
    [(tr) (memq incoming '(tr))]
    [(td th) (memq incoming '(td th tr))]
    [(thead tbody) (memq incoming '(tbody tfoot))]
    [(option) (memq incoming '(option optgroup))]
    [else #f]
  ) ; end case
) ; end define closes-top?

(define (merge-attrs! n attrs)
  (for ([kv (in-list attrs)])
    (unless (node-attr n (car kv))
      (node-set-attr! n (car kv) (cdr kv))
    ) ; end unless
  ) ; end for
) ; end define merge-attrs!

;; ---------------------------------------------------------------- 通用栈构建
;; base 恒为栈底(不可弹出)。fragment 与 body 内容共用此核心。

(define (make-builder base)
  (define stack (list base))   ; 栈顶在前
  (define (top) (car stack))
  (define (push! n) (set! stack (cons n stack)))
  (define (pop!) (when (pair? (cdr stack)) (set! stack (cdr stack))))
  (define (open! name attrs sc?)
    (let loop () (when (and (pair? (cdr stack)) (closes-top? (dnode-tag (top)) name))
                   (pop!) (loop)))
    (define n (make-element name attrs))
    (node-append! (top) n)
    (unless (or sc? (memq name VOID-TAGS)) (push! n))
    n
  ) ; end define open!
  (define (in-stack? name)   ; 栈中(不含栈底)是否有同名开放元素
    (let loop ([st stack])
      (cond [(null? (cdr st)) #f]
            [(eq? (dnode-tag (car st)) name) #t]
            [else (loop (cdr st))])
    ) ; end loop
  ) ; end define in-stack?
  (define (close! name)
    ;; 找最近同名者: 弹到它(含);找不到 → 游离关标签,忽略
    (when (in-stack? name)
      (let loop ()
        (when (pair? (cdr stack))
          (define t (dnode-tag (top)))
          (pop!)
          (unless (eq? t name) (loop))
        ) ; end when
      ) ; end loop
    ) ; end when
  ) ; end define close!
  (define (text! str)
    (unless (string=? str "")
      (node-append! (top) (make-text str))
    ) ; end unless
  ) ; end define text!
  (values open! close! text! (lambda () (cdr stack)))   ; 第四值: 栈深(除底)非空?
) ; end define make-builder

;; token 流全部灌进 base(fragment 语境——无 html/head/body 合成/路由)
(define (tokens->children next base)
  (define-values (open! close! text! _depth) (make-builder base))
  (let loop ()
    (define t (next))
    (cond
      [(tok-eof? t) (void)]
      [(tok-open? t) (open! (tok-open-name t) (tok-open-attrs t) (tok-open-self-closing? t)) (loop)]
      [(tok-close? t) (close! (tok-close-name t)) (loop)]
      [(tok-text? t) (text! (tok-text-s t)) (loop)]
      [else (loop)]   ; 注释丢弃(已知偏差)
    ) ; end cond
  ) ; end loop
) ; end define tokens->children

;; ---------------------------------------------------------------- 文档构建
;; html/head/body 恒合成;显式标签只合并属性。head 模式路由 HEAD-TAGS,
;; 首个"非 head 内容"切到 body。

(define (parse-html str [url #f])
  (define next (make-tokenizer str))
  (define root (make-element 'html))
  (define head (make-element 'head))
  (define body (make-element 'body))
  (node-append! root head)
  (node-append! root body)
  (define mode 'head)
  (define-values (h-open! h-close! h-text! h-depth) (make-builder head))
  (define-values (b-open! b-close! b-text! b-depth) (make-builder body))
  (define (in-head?) (and (eq? mode 'head) (null? (b-depth))))
  (let loop ()
    (define t (next))
    (cond
      [(tok-eof? t) (void)]
      [(tok-open? t)
       (define name (tok-open-name t))
       (define attrs (tok-open-attrs t))
       (define sc? (tok-open-self-closing? t))
       (case name
         [(html) (merge-attrs! root attrs)]
         [(head) (merge-attrs! head attrs)]
         [(body) (merge-attrs! body attrs) (set! mode 'body)]
         [else
          (cond
            ;; head 模式: head 族且不在 head 内嵌套之外 → 进 head
            [(and (in-head?) (memq name HEAD-TAGS) (null? (h-depth)))
             (h-open! name attrs sc?)]
            [(and (eq? mode 'head) (pair? (h-depth)))   ; head 内元素的子内容
             (h-open! name attrs sc?)]
            [else
             (set! mode 'body)
             (b-open! name attrs sc?)]
          ) ; end cond
         ] ; end else
       ) ; end case
       (loop)]
      [(tok-close? t)
       (define name (tok-close-name t))
       (case name
         [(html) (void)]
         [(head) (set! mode 'body)]
         [(body) (void)]
         [else (if (and (eq? mode 'head) (or (pair? (h-depth)) (memq name HEAD-TAGS)))
                   (h-close! name)
                   (b-close! name))]
       ) ; end case
       (loop)]
      [(tok-text? t)
       (define s (tok-text-s t))
       (cond
         [(and (eq? mode 'head) (pair? (h-depth))) (h-text! s)]   ; title/style 内文本
         [(and (eq? mode 'head) (string=? (string-trim s) "")) (void)]  ; head 间空白丢弃
         [(and (null? (b-depth)) (zero? (node-child-count body))
               (string=? (string-trim s) ""))
          (void)]   ; body 尚空时的间隙空白(如 </head> 与 <body> 之间)丢弃
         [else (set! mode 'body) (b-text! s)]
       ) ; end cond
       (loop)]
      [else (loop)]   ; 注释丢弃
    ) ; end cond
  ) ; end loop
  (make-document root url)
) ; end define parse-html
