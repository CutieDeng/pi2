#lang racket/base
;; browser/dom/serialize.rkt — L3 · 序列化（design-chrome.md L3 / §2.3）
;; 职责: dom->sexp/sexp->dom(往返恒等) · dom->html(差分 oracle 对比面 §7.4)
;;       · dom->markdown(extract 迁移目标,B1 先立最小面)
;; 不做: 解析(字符串→树是 L4)、查询
;; 依赖: racket/base browser/dom/{node,query}

(require racket/contract racket/string "node.rkt" "query.rkt")

(provide
 (contract-out
  [dom->sexp (-> node? any/c)]
  [sexp->dom (-> any/c node?)]
  [dom->html (-> node? string?)]
  [dom->markdown (-> node? string?)]
 ) ; end contract-out
) ; end provide

;; ---------------------------------------------------------------- sexp 往返
;; 形状：dtext → string；dnode → (tag ((k . v) ...) child ...)，attrs 按键序。

(define (dom->sexp n)
  (cond
    [(dtext? n) (dtext-s n)]
    [else
     (list* (dnode-tag n)
            (for/list ([k (in-list (node-attr-names n))])
              (cons k (node-attr n k)))
            (for/list ([c (in-children n)]) (dom->sexp c)))]
  ) ; end cond
) ; end define dom->sexp

(define (sexp->dom sx)
  (cond
    [(string? sx) (make-text sx)]
    [else
     (define n (make-element (car sx) (cadr sx)))
     (for ([csx (in-list (cddr sx))])
       (node-append! n (sexp->dom csx))
     ) ; end for
     n]
  ) ; end cond
) ; end define sexp->dom

;; ---------------------------------------------------------------- HTML

(define VOID-TAGS
  '(area base br col embed hr img input link meta source track wbr))

(define RAWTEXT-TAGS '(script style))

(define (escape-text s)
  (regexp-replace* #rx"[&<>]" s
                   (lambda (m)
                     (case (string-ref m 0)
                       [(#\&) "&amp;"] [(#\<) "&lt;"] [else "&gt;"])))
) ; end define escape-text

(define (escape-attr s)
  (regexp-replace* #rx"[&\"]" s
                   (lambda (m) (if (char=? (string-ref m 0) #\&) "&amp;" "&quot;")))
) ; end define escape-attr

(define (dom->html n)
  (define out (open-output-string))
  (let emit ([n n] [raw? #f])
    (cond
      [(dtext? n) (write-string (if raw? (dtext-s n) (escape-text (dtext-s n))) out)]
      [else
       (define tag (dnode-tag n))
       (write-string "<" out) (write-string (symbol->string tag) out)
       (for ([k (in-list (node-attr-names n))])
         (write-string " " out) (write-string (symbol->string k) out)
         (write-string "=\"" out) (write-string (escape-attr (node-attr n k)) out)
         (write-string "\"" out)
       ) ; end for
       (write-string ">" out)
       (unless (memq tag VOID-TAGS)
         (define raw* (and (memq tag RAWTEXT-TAGS) #t))
         (for ([c (in-children n)]) (emit c raw*))
         (write-string "</" out) (write-string (symbol->string tag) out)
         (write-string ">" out)
       ) ; end unless
      ] ; end else
    ) ; end cond
  ) ; end emit
  (get-output-string out)
) ; end define dom->html

;; ---------------------------------------------------------------- Markdown
;; extract 迁移目标的最小面：块级换行、标题/列表/链接/强调/代码。
;; 刻意保守——语义细化随 extract 迁移(B1 尾段)演进。

(define SKIP-TAGS '(script style head noscript template))

(define (collapse-ws s) (regexp-replace* #rx"[ \t\r\n]+" s " "))

(define (md-children n)
  (apply string-append (for/list ([c (in-children n)]) (md-node c)))
) ; end define md-children

(define (md-node n)
  (cond
    [(dtext? n) (collapse-ws (dtext-s n))]
    [else
     (define tag (dnode-tag n))
     (define (blk s) (string-append "\n\n" s "\n\n"))
     (case tag
       [(script style head noscript template) ""]
       [(h1) (blk (string-append "# " (md-children n)))]
       [(h2) (blk (string-append "## " (md-children n)))]
       [(h3) (blk (string-append "### " (md-children n)))]
       [(h4 h5 h6) (blk (string-append "#### " (md-children n)))]
       [(p div section article main header footer blockquote table tr)
        (blk (md-children n))]
       [(br) "\n"]
       [(li) (string-append "\n- " (md-children n))]
       [(ul ol) (string-append "\n" (md-children n) "\n")]
       [(a) (let ([href (node-attr n 'href)])
              (if href
                  (string-append "[" (md-children n) "](" href ")")
                  (md-children n)))]
       [(img) (let ([alt (node-attr n 'alt "")]) (if (string=? alt "") "" alt))]
       [(strong b) (string-append "**" (md-children n) "**")]
       [(em i) (string-append "*" (md-children n) "*")]
       [(code) (string-append "`" (md-children n) "`")]
       [(pre) (string-append "\n\n```\n" (dom-text-content n) "\n```\n\n")]
       [else (md-children n)]
     ) ; end case
    ] ; end else
  ) ; end cond
) ; end define md-node

(define (dom->markdown n)
  (string-trim (regexp-replace* #rx"\n{3,}" (md-node n) "\n\n"))
) ; end define dom->markdown
