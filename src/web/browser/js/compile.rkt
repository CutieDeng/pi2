#lang racket/base
;; browser/js/compile.rkt — L7 · JS AST → Racket 语法（设计 A 核心：JS 编译成 Racket）
;; 对标: design-jsobj.md §6 编译面契约；design-browser.md §3。
;; 依赖: racket/base racket/list racket/set
;; 产出: 一个 Racket S-表达式，在 eval.rkt 备好的 prelude namespace 里 eval。
;;   作用域分析：已声明名 → Racket 词法变量 js.<name>（JS 闭包 = Racket 闭包，白捡）；
;;   未声明名 → __js_global 全局兜底（内建 Object/Array…）。return → 逃逸续延 let/ec。
;;   宿主前缀 __js_*（get/set!/call/construct/算术/比较/truthy/object/array/func/global…）由 prelude 供。

(require racket/list racket/set)

(define current-scope (make-parameter (set)))   ; 当前词法在作用域名集合
(define (in-scope? name) (set-member? (current-scope) name))
(define (jsvar name) (string->symbol (string-append "js." name)))

;; 收集 var/函数声明名（提升）——var 是函数作用域，须**递归**进 if/while/for/block，
;; 但**不**进嵌套函数体（那有自己的作用域）。
(define (hoist-decls stmts) (append* (map hoist-stmt stmts)))
(define (hoist-stmt s)
  (case (car s)
    [(var) (map car (cadr s))]
    [(func-decl) (list (cadr s))]
    [(if) (append (hoist-stmt (caddr s)) (if (cadddr s) (hoist-stmt (cadddr s)) '()))]
    [(while) (hoist-stmt (caddr s))]
    [(for-c) (append (if (cadr s) (hoist-stmt (cadr s)) '()) (hoist-stmt (list-ref s 4)))]
    [(block) (append* (map hoist-stmt (cadr s)))]
    [else '()]))

;; 顶层程序：顶层声明名提升为 Racket 词法变量；末句为表达式语句则返回其值（REPL 语义）
(define (compile-program stmts)
  (define names (hoist-decls stmts))
  (parameterize ([current-scope (set-union (current-scope) (list->set names))])
    (define-values (init tailv)
      (if (and (pair? stmts) (eq? (car (last stmts)) 'expr-stmt))
          (values (drop-right stmts 1) (compile-expr (cadr (last stmts))))
          (values stmts '__js_undef)))
    `(let (,@(for/list ([n (in-list names)]) `[,(jsvar n) __js_undef]))
       ,@(map compile-stmt init) ,tailv)))

;; 函数：params + 内部声明进新作用域（并入外层 → 闭包捕获）
(define (compile-func params body-block)
  (define stmts (cadr body-block))
  (define names (append params (hoist-decls stmts)))
  (parameterize ([current-scope (set-union (current-scope) (list->set names))])
    `(__js_func
      (lambda (this args)
        (let/ec __js_return
          (let (,@(for/list ([p (in-list params)] [i (in-naturals)]) `[,(jsvar p) (__js_arg args ,i)])
                ,@(for/list ([h (in-list (hoist-decls stmts))]) `[,(jsvar h) __js_undef]))
            ,@(map compile-stmt stmts)
            __js_undef))))))

;; -------------------- 语句 --------------------
(define (compile-stmt s)
  (case (car s)
    [(var) `(begin ,@(for/list ([d (in-list (cadr s))] #:when (cdr d))
                       `(set! ,(jsvar (car d)) ,(compile-expr (cdr d)))) (void))]
    [(func-decl) `(set! ,(jsvar (cadr s)) ,(compile-func (caddr s) (cadddr s)))]
    [(expr-stmt) (compile-expr (cadr s))]
    [(return) `(__js_return ,(if (cadr s) (compile-expr (cadr s)) '__js_undef))]
    [(if) `(if (__js_truthy ,(compile-expr (cadr s))) ,(compile-stmt (caddr s))
               ,(if (cadddr s) (compile-stmt (cadddr s)) '(void)))]
    [(while) `(let loop () (when (__js_truthy ,(compile-expr (cadr s))) ,(compile-stmt (caddr s)) (loop)))]
    [(for-c)
     (define init (cadr s)) (define cnd (caddr s)) (define upd (cadddr s)) (define body (list-ref s 4))
     `(begin ,(if init (compile-stmt init) '(void))
             (let loop () (when ,(if cnd `(__js_truthy ,(compile-expr cnd)) #t)
                            ,(compile-stmt body)
                            ,(if upd (compile-expr upd) '(void))
                            (loop))))]
    [(block) `(let () ,@(map compile-stmt (cadr s)) (void))]
    [else (error 'compile "stmt: ~a" (car s))]))

;; -------------------- 表达式 --------------------
(define (compile-expr e)
  (case (car e)
    [(num str bool) (cadr e)]
    [(null) '__js_null] [(undef) '__js_undef] [(this) 'this]
    [(ident) (if (in-scope? (cadr e)) (jsvar (cadr e)) `(__js_global ,(cadr e)))]
    [(array) `(__js_array (list ,@(map compile-expr (cadr e))))]
    [(object) `(__js_object (list ,@(for/list ([kv (in-list (cadr e))]) `(cons ,(car kv) ,(compile-expr (cdr kv))))))]
    [(member) `(__js_get ,(compile-expr (cadr e)) ,(caddr e))]
    [(index) `(__js_get ,(compile-expr (cadr e)) (__js_key ,(compile-expr (caddr e))))]
    [(call) (compile-call (cadr e) (caddr e))]
    [(new) `(__js_construct ,(compile-expr (cadr e)) (list ,@(map compile-expr (caddr e))))]
    [(unary) (compile-unary (cadr e) (caddr e))]
    [(binary) `(,(hash-ref BINFN (cadr e)) ,(compile-expr (caddr e)) ,(compile-expr (cadddr e)))]
    [(logical) (compile-logical (cadr e) (caddr e) (cadddr e))]
    [(ternary) `(if (__js_truthy ,(compile-expr (cadr e))) ,(compile-expr (caddr e)) ,(compile-expr (cadddr e)))]
    [(assign) (compile-assign (cadr e) (caddr e))]
    [(update) (compile-update (cadr e) (caddr e) (cadddr e))]   ; op prefix? target
    [(func) (compile-func (caddr e) (cadddr e))]
    [else (error 'compile "expr: ~a" (car e))]))

;; ++/--：ToNumber(old) → old±1 → 写回；前缀返回新值、后缀返回旧值（§13.4）
(define (compile-update op prefix? target)
  (define addsub (if (string=? op "++") '__js_add '__js_sub))
  (define read (compile-expr target))
  (define write (lambda (valsym) (compile-write target valsym)))
  `(let* ([__old (__js_num ,read)] [__new (,addsub __old 1.0)])
     ,(write '__new)
     ,(if prefix? '__new '__old)))

;; 把 valsym 写进 target（ident 局部/全局 · member · index），返回写入表达式
(define (compile-write target valsym)
  (case (car target)
    [(ident) (if (in-scope? (cadr target)) `(set! ,(jsvar (cadr target)) ,valsym)
                 `(__js_global_set! ,(cadr target) ,valsym))]
    [(member) `(__js_set! ,(compile-expr (cadr target)) ,(caddr target) ,valsym)]
    [(index) `(__js_set! ,(compile-expr (cadr target)) (__js_key ,(compile-expr (caddr target))) ,valsym)]
    [else (error 'compile "bad update target")]))

(define (compile-call callee args)
  (define cargs `(list ,@(map compile-expr args)))
  (case (car callee)
    [(member) `(__js_call (__js_get ,(compile-expr (cadr callee)) ,(caddr callee)) ,(compile-expr (cadr callee)) ,cargs)]
    [(index) `(__js_call (__js_get ,(compile-expr (cadr callee)) (__js_key ,(compile-expr (caddr callee)))) ,(compile-expr (cadr callee)) ,cargs)]
    [else `(__js_call ,(compile-expr callee) __js_undef ,cargs)]))

(define (compile-assign target e)
  (define v (compile-expr e))
  (case (car target)
    [(ident) (if (in-scope? (cadr target))
                 `(let ([t ,v]) (set! ,(jsvar (cadr target)) t) t)
                 `(__js_global_set! ,(cadr target) ,v))]
    [(member) `(__js_set! ,(compile-expr (cadr target)) ,(caddr target) ,v)]
    [(index) `(__js_set! ,(compile-expr (cadr target)) (__js_key ,(compile-expr (caddr target))) ,v)]
    [else (error 'compile "bad assign target")]))

(define (compile-unary op e)
  ;; typeof 作用于不可解析引用返回 "undefined" 不报错（§13.5.3 step 1-2）：
  ;; typeof <未声明标识符> 须走 __js_typeof_global，不能先求值（否则 ReferenceError）
  (cond
    [(and (string=? op "typeof") (eq? (car e) 'ident) (not (in-scope? (cadr e))))
     `(__js_typeof_global ,(cadr e))]
    [else
     (define c (compile-expr e))
     (cond [(string=? op "!") `(__js_not ,c)] [(string=? op "-") `(__js_neg ,c)]
           [(string=? op "+") `(__js_num ,c)] [(string=? op "typeof") `(__js_typeof ,c)]
           [else (error 'compile "unary ~a" op)])]))

(define BINFN (hash "+" '__js_add "-" '__js_sub "*" '__js_mul "/" '__js_div "%" '__js_mod
                    "<" '__js_lt ">" '__js_gt "<=" '__js_le ">=" '__js_ge
                    "===" '__js_seq "!==" '__js_sne "==" '__js_eq "!=" '__js_ne
                    "instanceof" '__js_instanceof))

(define (compile-logical op l r)
  (if (string=? op "&&")
      `(let ([t ,(compile-expr l)]) (if (__js_truthy t) ,(compile-expr r) t))
      `(let ([t ,(compile-expr l)]) (if (__js_truthy t) t ,(compile-expr r)))))

(provide compile-program)
