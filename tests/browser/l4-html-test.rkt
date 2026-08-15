#lang racket/base
;; tests/browser/l4-html-test.rkt — L4 html：实体/tokenizer/树构建/片段

(require rackunit
         (file "../../src/web/browser/html/entities.rkt")
         (file "../../src/web/browser/html/treebuild.rkt")
         (file "../../src/web/browser/html/fragment.rkt")
         (file "../../src/web/browser/dom/node.rkt")
         (file "../../src/web/browser/dom/mutation.rkt")
         (file "../../src/web/browser/dom/query.rkt")
         (file "../../src/web/browser/dom/serialize.rkt"))

;; -------- 实体（全量表 + 数字引用 + legacy）
(check-equal? (decode-entities "a &amp; b") "a & b")
(check-equal? (decode-entities "&lt;p&gt;") "<p>")
(check-equal? (decode-entities "&#65;&#x42;c") "ABc")
(check-equal? (decode-entities "&AElig;") "Æ")
(check-equal? (decode-entities "&rarr;&nbsp;") "→ ")
(check-equal? (decode-entities "&amp b") "& b")          ; legacy 无分号
(check-equal? (decode-entities "&ampx") "&x")            ; legacy 最长前缀
(check-equal? (decode-entities "&notarealentity;") "&notarealentity;")
(check-equal? (decode-entities "&#0;") "�")              ; 码点守卫
(check-equal? (decode-entities "no entities") "no entities")
(check-equal? (entity-ref "amp;") "&")
(check-equal? (entity-ref "amp") "&")
(check-false (entity-ref "zzz;"))

;; -------- 整文档：结构合成与路由
(let ()
  (define doc (parse-html "<!doctype html>
<html lang=\"en\"><head><meta charset=\"utf-8\"><title>T &amp; t</title></head>
<body class=main><p>one<p>two</body></html>" "http://e.org/"))
  (define root (ddoc-root doc))
  (check-equal? (ddoc-url doc) "http://e.org/")
  (check-equal? (dnode-tag root) 'html)
  (check-equal? (node-attr root 'lang) "en")
  (check-equal? (map dnode-tag (node-children root)) '(head body))
  (define head (car (node-children root)))
  (define body (cadr (node-children root)))
  (check-equal? (node-attr body 'class) "main")
  ;; head 路由 + title rcdata 解实体
  (check-equal? (map dnode-tag (node-children head)) '(meta title))
  (check-equal? (dom-text-content (cadr (node-children head))) "T & t")
  ;; p 隐式闭合：两个平级 p
  (check-equal? (map dnode-tag (node-children body)) '(p p))
  (check-equal? (dom-text-content body) "onetwo")
  (check-tree! root)
) ; end let

;; 裸片段文档：无 html/head/body 标签也合成结构，meta 进 head，正文进 body
(let ()
  (define doc (parse-html "<meta charset=gbk><h1>Title</h1>text"))
  (define root (ddoc-root doc))
  (define head (car (node-children root)))
  (define body (cadr (node-children root)))
  (check-equal? (map dnode-tag (node-children head)) '(meta))
  (check-equal? (map dnode-tag (dom-query body (lambda (n) (eq? (dnode-tag n) 'h1)))) '(h1))
  (check-equal? (dom-text-content body) "Titletext")
) ; end let

;; -------- 容错：未闭合/错位/游离关标签/EOF
(let ()
  (define doc (parse-html "<div><ul><li>a<li>b</ul><p>c<div>d"))
  (define body (cadr (node-children (ddoc-root doc))))
  (define sx (dom->sexp body))
  (check-equal? sx
                '(body ()
                   (div ()
                     (ul () (li () "a") (li () "b"))
                     (p () "c")
                     (div () "d"))))
) ; end let

(let ()   ; 游离关标签忽略；错位关闭弹到同名
  (define doc (parse-html "<b>x</i>y</b>z"))
  (define body (cadr (node-children (ddoc-root doc))))
  (check-equal? (dom->sexp body) '(body () (b () "x" "y") "z"))
) ; end let

;; -------- 属性形态
(let ()
  (define doc (parse-html "<p id=a class=\"b c\" data-k='v' disabled DUP=1 dup=2>t"))
  (define p (car (dom-query (ddoc-root doc) (lambda (n) (eq? (dnode-tag n) 'p)))))
  (check-equal? (node-attr p 'id) "a")
  (check-equal? (node-attr p 'class) "b c")
  (check-equal? (node-attr p 'data-k) "v")
  (check-equal? (node-attr p 'disabled) "")
  (check-equal? (node-attr p 'dup) "1")     ; 重复属性首键胜(名字大小写归一)
) ; end let

;; 自闭合与 void
(let ()
  (define doc (parse-html "<div><br><img src=x.png><hr/><span/>after</div>"))
  (define dv (car (dom-query (ddoc-root doc) (lambda (n) (eq? (dnode-tag n) 'div)))))
  (check-equal? (map (lambda (c) (if (dtext? c) 'TEXT (dnode-tag c))) (node-children dv))
                '(br img hr span TEXT))
) ; end let

;; -------- rawtext：script 内 </div> 与 < 不当标签
(let ()
  (define doc (parse-html "<script>if (a<b) { x = \"</div>\"; }</script><p>ok"))
  (define root (ddoc-root doc))
  (define script (car (dom-query root (lambda (n) (eq? (dnode-tag n) 'script)))))
  (check-equal? (dom-text-content script) "if (a<b) { x = \"</div>\"; }")
  (check-equal? (length (dom-query root (lambda (n) (eq? (dnode-tag n) 'p)))) 1)
  ;; script 里的实体不解码(rawtext)
  (define doc2 (parse-html "<script>a &amp;&amp; b</script>"))
  (define s2 (car (dom-query (ddoc-root doc2) (lambda (n) (eq? (dnode-tag n) 'script)))))
  (check-equal? (dom-text-content s2) "a &amp;&amp; b")
) ; end let

;; -------- 注释丢弃、孤立 < 按文本
(let ()
  (define doc (parse-html "a <!-- no --> b"))
  (define body (cadr (node-children (ddoc-root doc))))
  (check-equal? (dom-text-content body) "a  b")
  (define doc2 (parse-html "<p>1 < 2 and 3 > 2</p>"))
  (check-equal? (dom-text-content (ddoc-root doc2)) "1 < 2 and 3 > 2")
) ; end let

;; -------- 片段解析（innerHTML 语境）
(let ()
  (define kids (parse-fragment "<li>a</li><li>b &amp; c" 'ul))
  (check-equal? (length kids) 2)
  (check-true (andmap (lambda (k) (not (node-parent k))) kids))   ; 已脱父
  (check-equal? (dom-text-content (cadr kids)) "b & c")
  ;; 接线到 mutation(L8 的用法)
  (define root (make-element 'div))
  (define doc (make-document root))
  (dom-replace-children! doc root kids)
  (check-equal? (ddoc-mutations doc) 1)
  (check-equal? (map dnode-tag (node-children root)) '(li li))
  (check-tree! root)
) ; end let

;; -------- 端到端小样：真实形态页面
(let ()
  (define doc (parse-html "
<!DOCTYPE html>
<html>
<head>
  <meta charset=\"utf-8\">
  <style>.ad { display: none }</style>
  <script src=\"app.js\"></script>
</head>
<body>
  <div class=\"ad\">buy!</div>
  <article><h1>Hello</h1><p>World &mdash; ok</p></article>
</body>
</html>"))
  (define root (ddoc-root doc))
  (define head (car (node-children root)))
  (check-equal? (map dnode-tag (node-children head)) '(meta style script))
  (check-true (regexp-match? #rx"World — ok" (dom-text-content root)))
  (check-tree! root)
) ; end let
