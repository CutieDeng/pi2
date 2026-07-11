#lang racket/base
;; pi-plugin-lang.rkt — 声明式插件语言（design-plugins.md M5，§2.1）
;;
;; 用法：插件文件首行 `#lang s-exp "…/src/pi-plugin-lang.rkt"`，随后写声明式表单：
;;   (deftool name #:desc "…" #:params (hasheq …) (lambda (in ctx) …))
;;   (defcommand "/cmd" #:desc "…" (lambda (args ctx) …))
;;   (on 'tool-call (lambda (name input) …))
;;   (defprovider "name" (lambda (cfg) …))
;;   (defshortcut (kchar #\g '(ctrl)) (lambda (ctx) …))
;; 自定义 #%module-begin 把整段主体包成 `(provide plugin)` 的注册函数：表单在 plugin 被调用
;; 时于 (current-plugin-api) 参数化下运行。故与既有加载器零改动（仍是 provide plugin 的受信插件）。
;; SDK（make-simple-tool 等）与 racket/base 已由本语言导出，插件通常无需 require。

(require
 (for-syntax racket/base syntax/parse)
 (file "plugin.rkt")
) ; end require

(provide
 (rename-out [pi-module-begin #%module-begin])
 (except-out (all-from-out racket/base) #%module-begin)
 deftool defcommand on defprovider defshortcut
 current-plugin-api
 ;; SDK 再导出（供插件主体直接使用，无需 require）
 make-simple-tool make-simple-provider
 ok-outcome err-outcome input-str input-int input-ref
 kchar knamed hook-block hook-replace
 (struct-out plugin-ctx)
) ; end provide

;; 传给声明式表单的当前 api（plugin 调用时参数化）
(define current-plugin-api (make-parameter #f))

;; 主体 = plugin 函数体（表单在此运行；内部 define 作局部定义，闭包可捕获）。
(define-syntax (pi-module-begin stx)
  (syntax-parse stx
    [(_ form ...)
     #'(#%module-begin
        (provide plugin)
        (define (plugin api)
          (parameterize ([current-plugin-api api]) form ... (void))))]
  ) ; end syntax-parse
) ; end define pi-module-begin

;; (deftool name #:desc d #:params p #:level l (lambda (in ctx) …))  —— 关键字可省
(define-syntax (deftool stx)
  (syntax-parse stx
    [(_ name:id
        (~optional (~seq #:desc desc:expr) #:defaults ([desc #'""]))
        (~optional (~seq #:params params:expr) #:defaults ([params #'(hasheq)]))
        (~optional (~seq #:level level:expr) #:defaults ([level #''read-only]))
        fn:expr)
     #'((plugin-api-register-tool! (current-plugin-api))
        (make-simple-tool #:name (symbol->string 'name) #:desc desc
                          #:params params #:level level #:run fn))]
  ) ; end syntax-parse
) ; end define deftool

;; (defcommand "/name" #:desc d (lambda (args ctx) …))
(define-syntax (defcommand stx)
  (syntax-parse stx
    [(_ name:expr (~optional (~seq #:desc desc:expr) #:defaults ([desc #'""])) fn:expr)
     #'((plugin-api-register-command! (current-plugin-api)) name (hasheq 'desc desc 'handler fn))]
  ) ; end syntax-parse
) ; end define defcommand

;; (on 'event (lambda …)) —— 变换/观测钩子
(define-syntax-rule (on ev fn) ((plugin-api-on! (current-plugin-api)) ev fn))

;; (defprovider "name" (lambda (cfg) …))
(define-syntax-rule (defprovider name fn) ((plugin-api-register-provider! (current-plugin-api)) name fn))

;; (defshortcut key (lambda (ctx) …)) —— key 为 kev（kchar/knamed 构造）
(define-syntax-rule (defshortcut key fn) ((plugin-api-register-shortcut! (current-plugin-api)) key fn))
