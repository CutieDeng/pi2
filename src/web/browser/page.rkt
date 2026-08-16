#lang racket/base
;; browser/page.rkt — L10 · 页面生命周期编排（design-chrome.md L10）
;; 依赖: racket/base + L4 html + L3 dom + L6 realm + L7 eval + L8 webapi（全部向下）
;; render-page: HTML + 脚本 → 运行脚本（可改 DOM）→ 序列化出「水合后」的页面。
;;   这是整个浏览器论点的最小闭环：静态抽取读不到的 JS 页面，跑一遍脚本后能读了。

(require "html/treebuild.rkt" "dom/mutation.rkt" "dom/serialize.rkt"
         "js/runtime/realm.rkt" "js/eval.rkt" "webapi/dom.rkt")

;; (render-page html script) → 运行 script 后的 DOM HTML 序列化。
;; script 里可用全局 document（getElementById/querySelector/createElement）操作页面。
(define (render-page html script)
  (define ddoc (parse-html html #f))
  (define R (make-realm))
  (define-values (document _wrap) (make-dom-api R ddoc))
  (run-js script #:realm R #:globals (list (cons "document" document)))
  (dom->html (ddoc-root ddoc)))

;; 也暴露「拿到 document 后自己驱动」的低层版本（供测试/L9 接入）
(define (make-page html)
  (define ddoc (parse-html html #f))
  (define R (make-realm))
  (define-values (document wrap) (make-dom-api R ddoc))
  (values ddoc R document wrap))

(provide render-page make-page)
