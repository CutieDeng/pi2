#lang racket/base
;; tests/browser/deps-test.rkt — §2.1 依赖白名单（机器强制的架构规则）
;; 规则1: 内核模块 #lang 必须是 racket/base（stock 纪律——不用 tstring）
;; 规则2: require 只许 racket/* 与 browser/ 内相对路径（内核永不 require pi2）
;; 规则3: 相对 require 的目标层 ≤ 本模块层（§1 严格向下依赖）
;; 规则4: require spec 只许裸 symbol/字符串（保持可机器审计）

(require rackunit racket/runtime-path racket/list racket/string racket/path)

(define-runtime-path BROWSER-ROOT "../../src/web/browser")

;; 目录 → 层号（js/runtime 特判为 6，js 其余 7）
(define DIR-LAYER
  (hash "text" 1 "cell" 2 "dom" 3 "html" 4 "css" 5
        "js" 7 "webapi" 8 "layout" 11 "view" 12 "paint" 13))
(define FILE-LAYER (hash "loop.rkt" 9 "page.rkt" 10))

(define (rel-elements p)   ; browser 根之下的路径元素(字符串列表)
  (define rel (find-relative-path (simplify-path BROWSER-ROOT)
                                  (simplify-path p)))
  (map path->string (explode-path rel))
) ; end define rel-elements

(define (path-layer p)
  (define els (rel-elements p))
  (cond
    [(and (>= (length els) 2) (string=? (first els) "js")
          (string=? (second els) "runtime"))
     6]
    [(>= (length els) 2) (hash-ref DIR-LAYER (first els)
                                   (lambda () (error 'deps "unknown dir: ~a" els)))]
    [else (hash-ref FILE-LAYER (first els)
                    (lambda () (error 'deps "unknown top file: ~a" els)))]
  ) ; end cond
) ; end define path-layer

(define (module-form f)
  (parameterize ([read-accept-reader #t] [read-accept-lang #t])
    (with-input-from-file f read)
  ) ; end parameterize
) ; end define module-form

(define (module-body form)
  ;; (module name lang body...) 且 body 可能被 #%module-begin 包一层
  (define body (cdddr form))
  (if (and (= 1 (length body)) (pair? (car body))
           (eq? (caar body) '#%module-begin))
      (cdar body)
      body)
) ; end define module-body

(define (requires-of form)
  (append*
   (for/list ([e (in-list (module-body form))]
              #:when (and (pair? e) (eq? (car e) 'require)))
     (cdr e))
  ) ; end append*
) ; end define requires-of

(define (browser-files)
  (for/list ([f (in-directory BROWSER-ROOT)]
             #:when (regexp-match? #rx"\\.rkt$" (path->string f)))
    f)
) ; end define browser-files

(for ([f (in-list (browser-files))])
  (define name (path->string f))
  (define form (module-form f))
  (test-case (string-append "deps: " name)
    ;; 规则1：#lang racket/base
    (check-equal? (caddr form) 'racket/base
                  "kernel module must be #lang racket/base (stock discipline)")
    ;; 规则2/3/4：逐条 require 检查
    (define my-layer (path-layer f))
    (define-values (dir _n _d?) (split-path f))
    (for ([spec (in-list (requires-of form))])
      (cond
        [(symbol? spec)
         (check-true (regexp-match? #rx"^racket(/|$)" (symbol->string spec))
                     (format "non-racket require: ~a" spec))]
        [(string? spec)
         (define target (simplify-path (build-path dir spec)))
         (define tstr (path->string target))
         (check-true (string-prefix? tstr (path->string (simplify-path BROWSER-ROOT)))
                     (format "require escapes browser/: ~a" spec))
         (check-true (<= (path-layer target) my-layer)
                     (format "upward layer require: ~a (L~a) → ~a (L~a)"
                             name my-layer spec (path-layer target)))]
        [else
         (fail (format "non-plain require spec (keep kernel requires plain): ~s" spec))]
      ) ; end cond
    ) ; end for
  ) ; end test-case
) ; end for
