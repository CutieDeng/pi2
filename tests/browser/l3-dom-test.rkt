#lang racket/base
;; tests/browser/l3-dom-test.rkt — L3 dom：结构原语/计数/查询/序列化

(require rackunit
         (file "../../src/web/browser/dom/node.rkt")
         (file "../../src/web/browser/dom/mutation.rkt")
         (file "../../src/web/browser/dom/query.rkt")
         (file "../../src/web/browser/dom/serialize.rkt"))

;; -------- node.rkt：结构原语与不变量

(let ()
  (define root (make-element 'html))
  (define body (make-element 'body '((class . "main"))))
  (define p (make-element 'p))
  (define t (make-text "hello"))
  (node-append! root body)
  (node-append! body p)
  (node-append! p t)
  (check-eq? (node-parent p) body)
  (check-eq? (node-parent t) p)
  (check-equal? (node-child-count body) 1)
  (check-tree! root)

  ;; append 移动语义：再挂别处自动脱旧父
  (define aside (make-element 'aside))
  (node-append! root aside)
  (node-append! aside p)
  (check-eq? (node-parent p) aside)
  (check-equal? (node-child-count body) 0)
  (check-tree! root)

  ;; 环拒绝
  (check-exn exn:fail? (lambda () (node-append! p aside)))
  (check-exn exn:fail? (lambda () (node-append! p p)))

  ;; insert-before
  (define h1 (make-element 'h1))
  (node-insert-before! aside h1 p)
  (check-equal? (map (lambda (c) (dnode-tag c)) (node-children aside)) '(h1 p))
  (node-insert-before! aside (make-element 'footer) #f)   ; ref=#f → append
  (check-equal? (node-child-count aside) 3)
  (check-exn exn:fail? (lambda () (node-insert-before! body h1 t)))  ; ref 不是 body 的孩子…
  (check-tree! root)

  ;; remove
  (node-remove! h1)
  (check-false (node-parent h1))
  (check-equal? (node-child-count aside) 2)

  ;; attrs
  (check-equal? (node-attr body 'class) "main")
  (check-false (node-attr body 'id))
  (check-equal? (node-attr body 'id 'none) 'none)
  (node-set-attr! body 'id "top")
  (check-equal? (node-attr-names body) '(class id))
  (node-remove-attr! body 'class)
  (check-equal? (node-attr-names body) '(id))
) ; end let

;; -------- mutation.rkt：计数语义（node.rkt 不计，dom-* 计）

(let ()
  (define root (make-element 'html))
  (define doc (make-document root "http://example.org/"))
  (check-equal? (ddoc-mutations doc) 0)
  (check-equal? (ddoc-url doc) "http://example.org/")

  (define d1 (make-element 'div))
  (node-append! root d1)                    ; 建树期原语：不计数
  (check-equal? (ddoc-mutations doc) 0)

  (dom-append! doc root (make-element 'p))  ; 运行期原语：计数
  (check-equal? (ddoc-mutations doc) 1)
  (dom-set-attr! doc d1 'class "x")
  (define t (make-text "a"))
  (dom-append! doc d1 t)
  (dom-set-text! doc t "b")
  (check-equal? (ddoc-mutations doc) 4)

  ;; replace-children!（innerHTML 的 L3 半边）
  (define k1 (make-element 'span))
  (define k2 (make-text "tail"))
  (dom-replace-children! doc d1 (list k1 k2))
  (check-equal? (ddoc-mutations doc) 5)
  (check-eq? (node-parent k1) d1)
  (check-false (node-parent t))             ; 旧子已脱父
  (check-equal? (node-child-count d1) 2)
  (check-tree! root)
) ; end let

;; -------- query.rkt

(let ()
  (define root (sexp->dom
                '(html ()
                   (body ()
                     (div ((class . "a")) (p () "one") (p () "two"))
                     (div ((class . "b")) (span () "three"))))))
  (define (tag= t) (lambda (n) (eq? (dnode-tag n) t)))
  (check-equal? (length (dom-query root (tag= 'p))) 2)
  (check-equal? (length (dom-query root (tag= 'p) #:limit 1)) 1)
  (check-equal? (length (dom-query root (lambda (n) #t))) 7)  ; 仅元素节点(不含 dtext)

  ;; 剪枝遍历：跳过 class=a 的子树
  (define seen '())
  (dom-walk root
            (lambda (n)
              (cond [(and (dnode? n) (equal? (node-attr n 'class) "a")) 'skip-subtree]
                    [(dtext? n) (set! seen (cons (dtext-s n) seen)) (void)]
                    [else (void)])))
  (check-equal? (reverse seen) '("three"))

  ;; textContent 文档序
  (check-equal? (dom-text-content root) "onetwothree")
) ; end let

;; -------- serialize.rkt

(let ()
  (define sx '(div ((class . "a") (id . "x"))
                "hi & <there>"
                (br ())
                (script () "if (a < b) go()")))
  (define n (sexp->dom sx))
  ;; sexp 往返恒等
  (check-equal? (dom->sexp n) sx)
  ;; html：转义/void 元素/rawtext 不转义
  (check-equal?
   (dom->html n)
   "<div class=\"a\" id=\"x\">hi &amp; &lt;there&gt;<br><script>if (a < b) go()</script></div>")
  ;; 属性转义
  (check-equal? (dom->html (sexp->dom '(a ((href . "x?a=1&b=\"q\"")) "t")))
                "<a href=\"x?a=1&amp;b=&quot;q&quot;\">t</a>")
) ; end let

(let ()
  (define n (sexp->dom
             '(body ()
                (h1 () "Title")
                (p () "Hello " (strong () "world") ".")
                (ul () (li () "one") (li () "two"))
                (p () (a ((href . "http://e.org")) "link"))
                (script () "junk()"))))
  (define md (dom->markdown n))
  (check-true (regexp-match? #rx"# Title" md))
  (check-true (regexp-match? #rx"Hello \\*\\*world\\*\\*\\." md))
  (check-true (regexp-match? #rx"- one\n- two" md))
  (check-true (regexp-match? #rx"\\[link\\]\\(http://e.org\\)" md))
  (check-false (regexp-match? #rx"junk" md))
) ; end let
