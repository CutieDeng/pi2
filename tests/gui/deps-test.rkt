#lang racket/base
;; tests/gui/deps-test.rkt — GUI 三层扁平架构（design-gui.md §2，机器强制）
;;   compute/     计算层：无自身状态的计算与数据结构（纯逻辑、单一平台绑定），可单独测试
;;   components/  组件层：能存储信息（有状态、有生命周期）；经接口使用其它组件，不认识其它组件
;;   assembly/    拼装层：配置组件（hub.rkt assemble 自动发现接口）、建立联系、驱动对外接口
;; 规则1: 所有模块 #lang racket/base
;; 规则2: 只能向下引用：compute → compute；components → compute；assembly → 任意
;; 规则3: 组件之间不 require（组件间的调用只经 compute/interfaces.rkt 定义的接口，由拼装层装配）
;; 规则4: 平台相关代码只在平台文件（mac-* / win-*）；ffi/unsafe 只许平台文件引用；ffi/unsafe/objc 全部禁止（自写桥）；
;;        非平台的工具 / 组件不得引用平台文件（保证可无窗口测试、可移植），拼装层除外；一个平台的文件不得引用另一个平台的
;; 规则5: symbol require 只许 racket/* 与纯 Racket 库白名单
;; 规则6: 只有三个目录，目录下不再分子目录

(require rackunit racket/runtime-path racket/list racket/string racket/path)

(define-runtime-path GUI-ROOT "../../src/gui")

(define LAYERS (hash "compute" 1 "components" 2 "assembly" 3))
(define PURE-LIBS '(file/gzip json net/base64))

(define (rel-elements p)
  (map path->string (explode-path (find-relative-path (simplify-path GUI-ROOT) (simplify-path p))))
) ; end define rel-elements

(define (layer-of p) (hash-ref LAYERS (first (rel-elements p)) (lambda () (error 'deps "outside the 3 layers: ~a" p))))
(define (platform-of p) (let ([m (regexp-match #rx"^(mac|win)-" (last (rel-elements p)))]) (and m (cadr m))))
(define (mac? p) (and (platform-of p) #t))                     ; 平台文件（名字沿用：最早只有 mac-*）

(define (module-form f)
  (parameterize ([read-accept-reader #t] [read-accept-lang #t]) (with-input-from-file f read))
) ; end define module-form

(define (module-body form)
  (define body (cdddr form))
  (if (and (= 1 (length body)) (pair? (car body)) (eq? (caar body) '#%module-begin)) (cdar body) body)
) ; end define module-body

(define (requires-of form)
  (append* (for/list ([e (in-list (module-body form))] #:when (and (pair? e) (eq? (car e) 'require)))
             (cdr e)))
) ; end define requires-of

(define files
  (for/list ([f (in-directory GUI-ROOT (lambda (d) (not (regexp-match? #rx"compiled$" (path->string d)))))]
             #:when (regexp-match? #rx"\\.rkt$" (path->string f)))
    f))

(test-case "exactly three flat layer directories"
  (define dirs (for/list ([d (directory-list GUI-ROOT #:build? #t)]
                          #:when (and (directory-exists? d) (not (regexp-match? #rx"compiled$" (path->string d)))))
                 (path->string (file-name-from-path d))))
  (check-equal? (sort dirs string<?) '("assembly" "components" "compute"))
  (for ([f (in-list files)])
    (check-equal? (length (rel-elements f)) 2 (format "nested deeper than one level: ~a" f))))

(for ([f (in-list files)])
  (define name (path->string f))
  (define form (module-form f))
  (test-case (string-append "deps: " name)
    (check-equal? (caddr form) 'racket/base "gui module must be #lang racket/base")
    (define my-layer (layer-of f))
    (define-values (dir _n _d?) (split-path f))
    (for ([spec (in-list (requires-of form))])
      (cond
        [(eq? spec 'ffi/unsafe)
         (check-true (mac? f) (format "ffi/unsafe outside platform files (mac-* / win-*): ~a" name))]
        [(symbol? spec)
         (check-true (or (regexp-match? #rx"^racket(/|$)" (symbol->string spec)) (and (memq spec PURE-LIBS) #t))
                     (format "disallowed require ~a in ~a (ffi/unsafe/objc is banned; own bridge in mac-objc.rkt)" spec name))]
        [(string? spec)
         (define target (simplify-path (build-path dir spec)))
         (check-true (string-prefix? (path->string target) (path->string (simplify-path GUI-ROOT)))
                     (format "require escapes src/gui/: ~a" spec))
         (define tl (layer-of target))
         (check-true (<= tl my-layer) (format "upward require: ~a → ~a" name spec))
         (when (= my-layer 2)
           (check-false (= tl 2) (format "component requires another component: ~a → ~a (use an interface)" name spec)))
         (when (and (mac? target) (not (mac? f)) (< my-layer 3))
           (fail (format "platform-neutral module requires a platform file: ~a → ~a" name spec)))
         (when (and (mac? target) (mac? f) (not (equal? (platform-of target) (platform-of f))))
           (fail (format "one platform's file requires another platform's: ~a → ~a" name spec)))]
        ;; 编译期（宏）用的 racket/*：只允许 (for-syntax racket/…)
        [(and (pair? spec) (eq? (car spec) 'for-syntax) (andmap symbol? (cdr spec)))
         (check-true (andmap (lambda (m) (regexp-match? #rx"^racket(/|$)" (symbol->string m))) (cdr spec))
                     (format "for-syntax require other than racket/* in ~a: ~s" name spec))]
        [else (fail (format "non-plain require spec: ~s" spec))]))))
