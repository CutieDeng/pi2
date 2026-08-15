#lang racket/base
;; browser/html/fragment.rkt — L4 · 片段解析（design-chrome.md L4 / §2.3）
;; 职责: innerHTML 语境——字符串 → 脱父节点列表(供 L8 经 dom-replace-children! 接线)
;; 不做: html/head/body 合成(那是整文档语境)
;; 依赖: racket/base browser/html/{tokenizer,treebuild} browser/dom/node

(require racket/contract
         "tokenizer.rkt" "treebuild.rkt"
         "../dom/node.rkt")

(provide
 (contract-out
  [parse-fragment (-> string? symbol? (listof node?))]
 ) ; end contract-out
) ; end provide

(define (parse-fragment str ctx-tag)
  (define dummy (make-element ctx-tag))
  (tokens->children (make-tokenizer str) dummy)
  (define kids (node-children dummy))
  (node-replace-children! dummy '())   ; 脱父(O(n)),调用方自由挂接
  kids
) ; end define parse-fragment
