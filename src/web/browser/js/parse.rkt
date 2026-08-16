#lang racket/base
;; browser/js/parse.rkt — L7 · JS 语法分析（实用子集 → AST）
;; 对标: ECMAScript 2023 §13–15（表达式/语句，子集）（design-jsobj.md L7）
;; 依赖: racket/base browser/js/lex
;; AST（tagged list）:
;;   表达式: (num n)(str s)(bool b)(null)(undef)(this)(ident x)(array es)(object kvs)
;;           (member o name)(index o k)(call callee args)(new callee args)
;;           (unary op e)(binary op l r)(logical op l r)(assign target e)(ternary c t e)(func name ps body)
;;   语句: (var decls)(expr-stmt e)(return e|#f)(if c then else|#f)(block stmts)
;;         (func-decl name ps body)(while c body)

(require "lex.rkt")

(define (parse str)
  (define toks (list->vector (lex str)))
  (define pos 0)
  (define (peek) (vector-ref toks pos))
  (define (kind) (vector-ref (peek) 0))
  (define (val) (vector-ref (peek) 1))
  (define (adv!) (define t (peek)) (set! pos (add1 pos)) t)
  (define (at? k v) (and (eq? (kind) k) (equal? (val) v)))
  (define (at-punc? v) (at? 'punc v))
  (define (eat-punc! v) (unless (at-punc? v) (perr (format "expected '~a'" v))) (adv!))
  (define (perr msg) (error 'parse "~a, got ~a" msg (peek)))

  ;; -------------------- 表达式（优先级爬升）--------------------
  (define BINOPS  ; 优先级：越大越紧
    (hash "||" 1 "&&" 2 "==" 7 "!=" 7 "===" 7 "!==" 7
          "<" 9 ">" 9 "<=" 9 ">=" 9 "instanceof" 9 "+" 11 "-" 11 "*" 13 "/" 13 "%" 13))
  (define (binop-here)   ; 当前 token 若是二元运算符则返回其名（含关键字 instanceof）
    (cond [(and (eq? (kind) 'punc) (hash-ref BINOPS (val) #f)) (val)]
          [(and (eq? (kind) 'name) (equal? (val) "instanceof")) "instanceof"]
          [else #f]))
  (define (logical? op) (member op '("||" "&&")))

  (define (parse-expr) (parse-assign))

  (define (parse-assign)
    (define left (parse-ternary))
    (cond
      [(at-punc? "=") (adv!) (list 'assign left (parse-assign))]
      [(or (at-punc? "+=") (at-punc? "-=") (at-punc? "*=") (at-punc? "/="))
       (define op (substring (val) 0 1)) (adv!)
       (list 'assign left (list 'binary op left (parse-assign)))]
      [else left]))

  (define (parse-ternary)
    (define c (parse-binary 1))
    (cond [(at-punc? "?") (adv!) (define t (parse-assign)) (eat-punc! ":") (list 'ternary c t (parse-assign))]
          [else c]))

  (define (parse-binary min-prec)
    (let loop ([left (parse-unary)])
      (define o (binop-here))
      (define prec (and o (hash-ref BINOPS o)))
      (cond
        [(and prec (>= prec min-prec))
         (adv!)
         (define right (parse-binary (add1 prec)))
         (loop (list (if (logical? o) 'logical 'binary) o left right))]
        [else left])))

  (define (parse-unary)
    (cond
      [(or (at-punc? "++") (at-punc? "--")) (define op (val)) (adv!) (list 'update op #t (parse-unary))]  ; 前缀
      [(or (at-punc? "!") (at-punc? "-") (at-punc? "+")) (define op (val)) (adv!) (list 'unary op (parse-unary))]
      [(at? 'kw "typeof") (adv!) (list 'unary "typeof" (parse-unary))]
      [(at? 'kw "new") (adv!)
       (define callee (parse-member-only (parse-primary)))
       (define args (if (at-punc? "(") (parse-args) '()))
       (parse-postfix (list 'new callee args))]
      [else (parse-postfix (parse-primary))]))

  ;; new 的 callee 只吃 member（不吃 call），再由外层加 args
  (define (parse-member-only e)
    (cond [(at-punc? ".") (adv!) (define nm (val)) (adv!) (parse-member-only (list 'member e nm))]
          [(at-punc? "[") (adv!) (define k (parse-expr)) (eat-punc! "]") (parse-member-only (list 'index e k))]
          [else e]))

  (define (parse-args)
    (eat-punc! "(")
    (let loop ([acc '()])
      (cond [(at-punc? ")") (adv!) (reverse acc)]
            [else (define a (parse-assign))
                  (if (at-punc? ",") (begin (adv!) (loop (cons a acc)))
                      (begin (eat-punc! ")") (reverse (cons a acc))))])))

  (define (parse-postfix e)
    (cond
      [(at-punc? ".") (adv!) (define nm (val)) (adv!) (parse-postfix (list 'member e nm))]
      [(at-punc? "[") (adv!) (define k (parse-expr)) (eat-punc! "]") (parse-postfix (list 'index e k))]
      [(at-punc? "(") (parse-postfix (list 'call e (parse-args)))]
      [(or (at-punc? "++") (at-punc? "--")) (define op (val)) (adv!) (list 'update op #f e)]  ; 后缀
      [else e]))

  (define (parse-primary)
    (case (kind)
      [(num) (list 'num (adv-val!))]
      [(str) (list 'str (adv-val!))]
      [(kw)
       (define k (adv-val!))
       (case k [("true") '(bool #t)] [("false") '(bool #f)] [("null") '(null)]
               [("undefined") '(undef)] [("this") '(this)]
               [("function") (parse-func #f)]
               [else (perr (format "unexpected keyword ~a" k))])]
      [(name) (list 'ident (adv-val!))]
      [(punc)
       (cond
         [(at-punc? "(") (adv!) (define e (parse-expr)) (eat-punc! ")") e]
         [(at-punc? "[") (parse-array)]
         [(at-punc? "{") (parse-object)]
         [else (perr "unexpected token")])]
      [else (perr "unexpected token")]))
  (define (adv-val!) (define v (val)) (adv!) v)

  (define (parse-array)
    (eat-punc! "[")
    (let loop ([acc '()])
      (cond [(at-punc? "]") (adv!) (list 'array (reverse acc))]
            [else (define e (parse-assign))
                  (if (at-punc? ",") (begin (adv!) (loop (cons e acc)))
                      (begin (eat-punc! "]") (list 'array (reverse (cons e acc)))))])))

  (define (parse-object)
    (eat-punc! "{")
    (let loop ([acc '()])
      (cond [(at-punc? "}") (adv!) (list 'object (reverse acc))]
            [else
             (define key (case (kind) [(name kw) (adv-val!)] [(str) (adv-val!)] [(num) (number->string (adv-val!))]
                                      [else (perr "bad object key")]))
             (eat-punc! ":")
             (define v (parse-assign))
             (define acc* (cons (cons key v) acc))
             (if (at-punc? ",") (begin (adv!) (loop acc*))
                 (begin (eat-punc! "}") (list 'object (reverse acc*))))])))

  (define (parse-func name-default)
    ;; 已消费 'function；可有名字
    (define name (if (eq? (kind) 'name) (adv-val!) name-default))
    (define params (parse-params))
    (define body (parse-block))
    (list 'func name params body))
  (define (parse-params)
    (eat-punc! "(")
    (let loop ([acc '()])
      (cond [(at-punc? ")") (adv!) (reverse acc)]
            [else (define p (adv-val!))
                  (if (at-punc? ",") (begin (adv!) (loop (cons p acc)))
                      (begin (eat-punc! ")") (reverse (cons p acc))))])))

  ;; -------------------- 语句 --------------------
  (define (parse-block)
    (eat-punc! "{")
    (let loop ([acc '()])
      (cond [(at-punc? "}") (adv!) (list 'block (reverse acc))]
            [(eq? (kind) 'eof) (perr "unterminated block")]
            [else (loop (cons (parse-stmt) acc))])))

  (define (semi!) (when (at-punc? ";") (adv!)))   ; 分号可选（简化 ASI）

  (define (parse-stmt)
    (cond
      [(or (at? 'kw "var") (at? 'kw "let") (at? 'kw "const")) (adv!) (parse-var)]
      [(at? 'kw "function") (adv!) (define f (parse-func #f)) (list 'func-decl (caddr-name f) (list-ref f 2) (list-ref f 3))]
      [(at? 'kw "return") (adv!)
       (define e (if (or (at-punc? ";") (at-punc? "}")) #f (parse-expr))) (semi!) (list 'return e)]
      [(at? 'kw "if") (adv!) (eat-punc! "(") (define c (parse-expr)) (eat-punc! ")")
       (define then (parse-stmt))
       (define else* (if (at? 'kw "else") (begin (adv!) (parse-stmt)) #f))
       (list 'if c then else*)]
      [(at? 'kw "while") (adv!) (eat-punc! "(") (define c (parse-expr)) (eat-punc! ")")
       (list 'while c (parse-stmt))]
      [(at? 'kw "for") (adv!) (parse-for)]
      [(at-punc? "{") (parse-block)]
      [(at-punc? ";") (adv!) (list 'block '())]
      [else (define e (parse-expr)) (semi!) (list 'expr-stmt e)]))

  (define (caddr-name f) (cadr f))   ; func 的名字位

  ;; C 风格 for(init; cond; update) body（不含 for-in，后续补）
  (define (parse-for)
    (eat-punc! "(")
    (define init   ; 三个分支都消费第一个 ';'
      (cond [(at-punc? ";") (adv!) #f]
            [(or (at? 'kw "var") (at? 'kw "let") (at? 'kw "const")) (adv!) (parse-var)]  ; parse-var 吃掉 ;
            [else (define e (parse-expr)) (eat-punc! ";") (list 'expr-stmt e)]))
    ;; parse-var 已消费其 ';'；expr/empty 分支在上面各自消费
    (define cond* (if (at-punc? ";") #f (parse-expr)))
    (eat-punc! ";")
    (define update (if (at-punc? ")") #f (parse-expr)))
    (eat-punc! ")")
    (list 'for-c init cond* update (parse-stmt)))

  (define (parse-var)
    (let loop ([acc '()])
      (define name (adv-val!))
      (define init (if (at-punc? "=") (begin (adv!) (parse-assign)) #f))
      (define acc* (cons (cons name init) acc))
      (cond [(at-punc? ",") (adv!) (loop acc*)]
            [else (semi!) (list 'var (reverse acc*))])))

  ;; -------------------- 程序 --------------------
  (let loop ([acc '()])
    (cond [(eq? (kind) 'eof) (list 'block (reverse acc))]
          [else (loop (cons (parse-stmt) acc))])))

(provide parse)
