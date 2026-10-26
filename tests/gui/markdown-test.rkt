#lang racket/base
;; tests/gui/markdown-test.rkt — Markdown 解析（计算层）：按模型回答里常见的写法

(require rackunit racket/list "../../src/gui/compute/markdown.rkt")

(define (spans s) (for/list ([sp (parse-inlines s)]) (cons (span-text sp) (sort (span-styles sp) symbol<?))))

(test-case "inline: bold / italic / code / strike, nesting, unmatched delimiters stay literal"
  (check-equal? (spans "a **b** c") '(("a " ) ("b" bold) (" c")))
  (check-equal? (spans "**粗 *斜* 粗**") '(("粗 " bold) ("斜" bold italic) (" 粗" bold)))
  (check-equal? (spans "***both***") '(("both" bold italic)))
  (check-equal? (spans "~~gone~~ x") '(("gone" strike) (" x")))
  (check-equal? (spans "2 * 3 * 4") '(("2 * 3 * 4")))                       ; 两侧空白：不是强调
  (check-equal? (spans "a ** b") '(("a ** b"))))

(test-case "inline: code spans protect their content; snake_case is not italic; escapes"
  (check-equal? (spans "run `a*b*c` now") '(("run ") ("a*b*c" code) (" now")))
  (check-equal? (spans "``x ` y``") '(("x ` y" code)))
  (check-equal? (spans "call my_func_name here") '(("call my_func_name here")))
  (check-equal? (spans "_italic_ word") '(("italic" italic) (" word")))
  (check-equal? (spans "\\*not\\* emph") '(("*not* emph")))
  (check-equal? (spans "`\\n` stays") '(("\\n" code) (" stays")))
  (check-equal? (spans "unclosed `tick") '(("unclosed `tick"))))

(test-case "inline: links, autolinks, bare URLs (trailing punctuation excluded)"
  (define sp (parse-inlines "see [docs](https://x.dev/a) and https://y.org/p?q=1."))
  (check-equal? (map span-text sp) '("see " "docs" " and " "https://y.org/p?q=1" "."))
  (check-equal? (span-url (second sp)) "https://x.dev/a")
  (check-not-false (memq 'link (span-styles (fourth sp)))))

(test-case "blocks: heading, paragraph soft break, hr"
  (define bs (parse-markdown "# 标题\n\n第一行\n第二行\n\n---\n## 二级 ##"))
  (check-true (md-heading? (first bs)))
  (check-equal? (md-heading-level (first bs)) 1)
  (check-equal? (inline-text (md-para-inlines (second bs))) "第一行 第二行")
  (check-true (md-hr? (third bs)))
  (check-equal? (inline-text (md-heading-inlines (fourth bs))) "二级"))

(test-case "blocks: fenced code keeps text verbatim; unclosed fence (streaming) runs to the end"
  (define bs (parse-markdown "before\n```python\ndef f(x):\n    return x * 2  # **not bold**\n```\nafter"))
  (check-equal? (map (lambda (b) (cond [(md-code? b) 'code] [(md-para? b) 'para])) bs) '(para code para))
  (check-equal? (md-code-lang (second bs)) "python")
  (check-equal? (md-code-text (second bs)) "def f(x):\n    return x * 2  # **not bold**")
  (check-true (md-code-closed? (second bs)))
  (define streaming (parse-markdown "text\n```rkt\n(define x 1)\n(displayln"))
  (check-false (md-code-closed? (second streaming)))
  (check-equal? (md-code-text (second streaming)) "(define x 1)\n(displayln"))

(test-case "blocks: nested lists, item with code block, ordered start, loose items"
  (define bs (parse-markdown (string-append
                              "1. 第一步\n"
                              "   - 子项 a\n"
                              "   - 子项 **b**\n"
                              "2. 第二步：\n"
                              "   ```sh\n"
                              "   make test\n"
                              "   ```\n"
                              "\n"
                              "3. 第三步")))
  (define l (first bs))
  (check-true (md-list-ordered? l))
  (check-equal? (md-list-start l) 1)
  (check-equal? (length (md-list-items l)) 3)
  (define item1 (first (md-list-items l)))
  (check-equal? (inline-text (md-para-inlines (first item1))) "第一步")
  (check-equal? (length (md-list-items (second item1))) 2)                 ; 嵌套无序列表
  (define item2 (second (md-list-items l)))
  (check-true (md-code? (second item2)))
  (check-equal? (md-code-text (second item2)) "make test")
  (check-equal? (md-list-start (first (parse-markdown "3) c\n4) d"))) 3))

(test-case "blocks: GFM table with alignment, ragged rows padded"
  (define t (first (parse-markdown "| 方案 | 延迟 | 说明 |\n|:---|---:|:-:|\n| 同 place | **1µs** | 直调 |\n| 子进程 | 217µs |\n\nafter")))
  (check-true (md-table? t))
  (check-equal? (md-table-aligns t) '(left right center))
  (check-equal? (map inline-text (md-table-header t)) '("方案" "延迟" "说明"))
  (check-equal? (length (md-table-rows t)) 2)
  (check-equal? (map inline-text (second (md-table-rows t))) '("子进程" "217µs" ""))
  (check-equal? (span-styles (car (second (first (md-table-rows t))))) '(bold)))

(test-case "blocks: quote with nested list and lazy continuation"
  (define q (first (parse-markdown "> 注意：\n> - 一\n> - 二\n延续行")))
  (check-true (md-quote? q))
  (check-true (md-list? (second (md-quote-blocks q)))))
