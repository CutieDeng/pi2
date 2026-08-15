#lang racket/base
;; tests/browser/l5-css-test.rkt — L5 css：选择器解析/匹配/display 级联

(require rackunit
         (file "../../src/web/browser/css/selparse.rkt")
         (file "../../src/web/browser/css/match.rkt")
         (file "../../src/web/browser/css/style.rkt")
         (file "../../src/web/browser/dom/node.rkt")
         (file "../../src/web/browser/dom/query.rkt")
         (file "../../src/web/browser/dom/serialize.rkt"))

;; fixture 树
(define root
  (sexp->dom
   '(html ()
      (body ()
        (div ((id . "main") (class . "content wide"))
          (p ((class . "intro")) "hi")
          (section ()
            (p ((data-x . "1")) "deep")))
        (div ((class . "sidebar"))
          (p () "aside")
          (a ((href . "http://e.org")) "link"))))))

(define (q sel) (dom-query root (compile-selector sel)))
(define (tags sel) (map dnode-tag (q sel)))

;; -------- 简单选择器
(check-equal? (length (q "p")) 3)
(check-equal? (length (q "*")) 9)
(check-equal? (tags "#main") '(div))
(check-equal? (length (q ".content")) 1)   ; 多 class 属性中的一个
(check-equal? (length (q ".wide")) 1)
(check-equal? (length (q ".nope")) 0)
(check-equal? (length (q "[href]")) 1)
(check-equal? (length (q "[data-x=1]")) 1)
(check-equal? (length (q "[data-x=\"1\"]")) 1)
(check-equal? (length (q "div.sidebar")) 1)

;; -------- 组合器
(check-equal? (length (q "div p")) 3)          ; 后代
(check-equal? (length (q "#main > p")) 1)      ; 子代(deep 的 p 不算)
(check-equal? (length (q "#main p")) 2)        ; 后代含 deep
(check-equal? (length (q "body > div > p")) 2)
(check-equal? (length (q "section > p")) 1)
(check-equal? (length (q "p, a")) 4)           ; 并联
;; 回溯: "div section p" —— section 的祖先里要再找 div
(check-equal? (length (q "div section p")) 1)

;; -------- :not 与伪类
(check-equal? (length (q "p:not(.intro)")) 2)
(check-equal? (length (q "div:not(#main)")) 1)
(check-equal? (length (q "a:hover")) 0)        ; 伪类恒假
(check-equal? (length (q "p::first-line")) 0)  ; 伪元素恒假

;; -------- 语法错误
(check-exn exn:fail:selector? (lambda () (parse-selector "")))
(check-exn exn:fail:selector? (lambda () (parse-selector "p >")))
(check-exn exn:fail:selector? (lambda () (parse-selector "[a~=b]")))
(check-exn exn:fail:selector? (lambda () (parse-selector "p ~ a")))

;; -------- style: 解析与级联
(let ()
  (define rules (parse-css "
    /* comment */
    .hidden { display: none; color: red }
    @media print { p { display: none } }
    p { visibility: hidden; }
    #main { display: block }
    bad{{selector} { display:none }
  "))
  ;; @media 整块跳过、坏选择器跳过、无 display 族声明的规则不收 → 3 条
  (check-equal? (length rules) 3)
  (define style-of (make-style-index rules))

  (define t (sexp->dom
             '(body ()
                (div ((class . "hidden")) "a")
                (div ((id . "main") (class . "hidden")) "b")   ; 源序后者胜 → block
                (p ((style . "visibility: visible")) "c")       ; 内联最高
                (p () "d")
                (span ((hidden . "")) "e")
                (span ((hidden . "") (style . "display:inline")) "f"))))
  (define (nth-el k) (list-ref (dom-query t (lambda (n) (not (eq? (dnode-tag n) 'body)))) k))

  (check-false (style-visible? (style-of (nth-el 0))))           ; .hidden → none
  (check-true (style-visible? (style-of (nth-el 1))))            ; #main 覆盖 .hidden
  (check-true (style-visible? (style-of (nth-el 2))))            ; 内联 visible 覆盖规则
  (check-false (style-visible? (style-of (nth-el 3))))           ; p → visibility hidden
  (check-false (style-visible? (style-of (nth-el 4))))           ; hidden 属性
  (check-true (style-visible? (style-of (nth-el 5))))            ; 内联覆盖 hidden 属性
) ; end let
