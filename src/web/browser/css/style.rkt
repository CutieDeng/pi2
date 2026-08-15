#lang racket/base
;; browser/css/style.rkt — L5 · display 子集级联（design-chrome.md L5 / §2.3）
;; 职责: <style> 文本 → 规则(仅 display/visibility) · style=/hidden 属性 · 可见性判定
;; 级联近似(B1 已知简化): 无 specificity——hidden 属性 → 样式表按源序(后者胜) → 内联 style 最高
;; 不做: 布局级联(字号/流向在 L11 style2)、选择器语法(委托 selparse/match)
;; 依赖: racket/base browser/css/{selparse,match} browser/dom/node

(require racket/contract racket/string
         "selparse.rkt" "match.rkt"
         "../dom/node.rkt")

(provide
 (struct-out style)
 (contract-out
  [parse-css (-> string? list?)]
  [make-style-index (-> list? (-> dnode? style?))]
  [style-visible? (-> style? boolean?)]
 ) ; end contract-out
) ; end provide

(struct style (display visibility) #:prefab)

;; ---------------------------------------------------------------- 声明解析

(define KEPT-PROPS '(display visibility))

;; "display:none; color:red" → ((display . "none")) —— 只留 display 族
(define (parse-decls s)
  (for*/list ([piece (in-list (string-split s ";"))]
              [kv (in-value (string-split piece ":"))]
              #:when (= (length kv) 2)
              [k (in-value (string->symbol (string-downcase (string-trim (car kv)))))]
              #:when (memq k KEPT-PROPS))
    (cons k (string-downcase (string-trim (cadr kv))))
  ) ; end for*/list
) ; end define parse-decls

;; ---------------------------------------------------------------- 样式表解析
;; 实用子集: 剥注释 → 顺序读 "selector { decls }"；@ 规则整块跳过；
;; 坏选择器的规则跳过(容错——样式表一条坏不该废整表)。

(define (strip-comments s)
  (regexp-replace* #rx"/\\*.*?\\*/" s "")
) ; end define strip-comments

(define (parse-css text)
  (define s (strip-comments text))
  (define len (string-length s))
  (define (find-char from ch)
    (for/first ([k (in-range from len)] #:when (char=? (string-ref s k) ch)) k)
  ) ; end define find-char
  ;; @ 块规则: 跳到配平的 }; @ 语句规则: 跳到 ;
  (define (skip-at from)
    (define brace (find-char from #\{))
    (define semi (find-char from #\;))
    (cond
      [(and semi (or (not brace) (< semi brace))) (add1 semi)]
      [(not brace) len]
      [else
       (let loop ([k (add1 brace)] [depth 1])
         (cond [(>= k len) len]
               [(char=? (string-ref s k) #\{) (loop (add1 k) (add1 depth))]
               [(char=? (string-ref s k) #\})
                (if (= depth 1) (add1 k) (loop (add1 k) (sub1 depth)))]
               [else (loop (add1 k) depth)]))]
    ) ; end cond
  ) ; end define skip-at
  (let loop ([i 0] [rules '()])
    (cond
      [(>= i len) (reverse rules)]
      [(memv (string-ref s i) '(#\space #\tab #\newline #\return)) (loop (add1 i) rules)]
      [(char=? (string-ref s i) #\@) (loop (skip-at i) rules)]
      [else
       (define open (find-char i #\{))
       (cond
         [(not open) (reverse rules)]
         [else
          (define close (or (find-char open #\}) len))
          (define sel-str (string-trim (substring s i open)))
          (define decls (parse-decls (substring s (add1 open) close)))
          (define ast (with-handlers ([exn:fail:selector? (lambda (_) #f)])
                        (parse-selector sel-str)))
          (loop (add1 close)
                (if (and ast (pair? decls))
                    (cons (cons ast decls) rules)
                    rules))]
       ) ; end cond
      ] ; end else
    ) ; end cond
  ) ; end loop
) ; end define parse-css

;; ---------------------------------------------------------------- 级联

(define (make-style-index rules)
  (define compiled
    (for/list ([r (in-list rules)])
      (cons (compile-selector-ast (car r)) (cdr r)))
  ) ; end define compiled
  (lambda (n)
    ;; 基线: hidden 属性(HTML) → display:none
    (define disp0 (and (node-attr n 'hidden) "none"))
    ;; 样式表源序,后者胜
    (define-values (disp1 vis1)
      (for/fold ([d disp0] [v #f]) ([r (in-list compiled)])
        (if ((car r) n)
            (values (cond [(assq 'display (cdr r)) => cdr] [else d])
                    (cond [(assq 'visibility (cdr r)) => cdr] [else v]))
            (values d v))
      ) ; end for/fold
    ) ; end define-values
    ;; 内联 style 最高
    (define inline (parse-decls (or (node-attr n 'style) "")))
    (style (cond [(assq 'display inline) => cdr] [else disp1])
           (cond [(assq 'visibility inline) => cdr] [else vis1]))
  ) ; end lambda
) ; end define make-style-index

(define (style-visible? st)
  (and (not (equal? (style-display st) "none"))
       (not (member (style-visibility st) '("hidden" "collapse"))))
) ; end define style-visible?
