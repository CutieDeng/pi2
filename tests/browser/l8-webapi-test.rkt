#lang racket/base
;; tests/browser/l8-webapi-test.rkt — L8+L10 端到端：JS 脚本操作 DOM（页面水合）
;; 验证 B2 闭环：HTML → DOM → 跑脚本 → 脚本改 DOM → 序列化出水合页面

(require rackunit (file "../../src/web/browser/page.rkt"))

(define (render html script) (render-page html script))

(test-case "textContent 读写"
  (check-equal? (render "<div id=\"app\">old</div>"
                        "document.getElementById(\"app\").textContent = \"new\"")
                "<html><head></head><body><div id=\"app\">new</div></body></html>")
  ;; 读 textContent
  (check-equal? (render "<div id=\"a\">hi</div><div id=\"out\"></div>"
                        "document.getElementById(\"out\").textContent = document.getElementById(\"a\").textContent + \"!\"")
                "<html><head></head><body><div id=\"a\">hi</div><div id=\"out\">hi!</div></body></html>"))

(test-case "innerHTML 水合（片段解析）"
  (check-equal? (render "<div id=\"root\"></div>"
                        "document.getElementById(\"root\").innerHTML = \"<h1>T</h1><p>B</p>\"")
                "<html><head></head><body><div id=\"root\"><h1>T</h1><p>B</p></div></body></html>"))

(test-case "createElement + appendChild（DOM 构建 + 循环）"
  (check-equal? (render "<ul id=\"l\"></ul>"
                        (string-append "var ul=document.getElementById(\"l\");"
                                       "for(var i=1;i<=3;i++){ var li=document.createElement(\"li\");"
                                       "li.textContent=\"item \"+i; ul.appendChild(li) }"))
                "<html><head></head><body><ul id=\"l\"><li>item 1</li><li>item 2</li><li>item 3</li></ul></body></html>"))

(test-case "querySelector + setAttribute/getAttribute"
  (check-equal? (render "<a class=\"link\">x</a>"
                        "document.querySelector(\".link\").setAttribute(\"href\", \"/go\")")
                "<html><head></head><body><a class=\"link\" href=\"/go\">x</a></body></html>")
  ;; 注：序列化按键序输出属性（id < type），与输入顺序无关
  (check-equal? (render "<input type=\"text\" id=\"i\"><div id=\"o\"></div>"
                        "document.getElementById(\"o\").textContent = document.getElementById(\"i\").getAttribute(\"type\")")
                "<html><head></head><body><input id=\"i\" type=\"text\"><div id=\"o\">text</div></body></html>"))

(test-case "querySelectorAll + children + 遍历"
  ;; 统计 .item 数量，写入结果
  (check-equal? (render (string-append "<div class=\"item\">a</div><div class=\"item\">b</div>"
                                       "<div class=\"item\">c</div><div id=\"count\"></div>")
                        "document.getElementById(\"count\").textContent = \"\" + document.querySelectorAll(\".item\").length")
                (string-append "<html><head></head><body><div class=\"item\">a</div><div class=\"item\">b</div>"
                               "<div class=\"item\">c</div><div id=\"count\">3</div></body></html>")))

(test-case "tagName / id 属性"
  (check-equal? (render "<section id=\"s\"></section><div id=\"o\"></div>"
                        "document.getElementById(\"o\").textContent = document.getElementById(\"s\").tagName")
                "<html><head></head><body><section id=\"s\"></section><div id=\"o\">SECTION</div></body></html>"))

(test-case "综合：从数据渲染列表（框架式水合）"
  (check-equal?
   (render "<ul id=\"todos\"></ul>"
           (string-append
            "var data = [\"buy milk\", \"walk dog\", \"write code\"];"
            "var ul = document.getElementById(\"todos\");"
            "data.forEach(function(t){"
            "  var li = document.createElement(\"li\");"
            "  li.textContent = t;"
            "  ul.appendChild(li);"
            "});"))
   (string-append "<html><head></head><body><ul id=\"todos\">"
                  "<li>buy milk</li><li>walk dog</li><li>write code</li></ul></body></html>")))
