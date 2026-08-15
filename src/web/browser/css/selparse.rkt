#lang racket/base
;; browser/css/selparse.rkt — L5 · 选择器文法 → AST（design-chrome.md L5 / §2.3）
;; 职责: 选择器字符串 → 纯数据 AST（不可变,§2.2 规约 5）；语法错 raise exn:fail:selector
;; 覆盖: tag * #id .class [attr] [attr=v] 后代 子代(>) 并联(,) :not(compound) 其余伪类恒假
;; 不做: 匹配求值(match.rkt)、级联(style.rkt)
;; 依赖: racket/base
;;
;; AST 形状（plain datum）:
;;   selector := (listof complex)                          ; 逗号并联
;;   complex  := (cons compound steps)                     ; steps 右→左
;;   steps    := (listof (cons (or/c 'child 'descendant) compound))
;;   compound := (listof simple)                           ; 空表 = 通配 *
;;   simple   := (tag sym)|(id str)|(class str)|(attr sym)|(attr= sym str)
;;             | (not compound)|(pseudo sym)

(require racket/contract)

(provide
 (contract-out
  [parse-selector (-> string? list?)]
  [exn:fail:selector? (-> any/c boolean?)]
 ) ; end contract-out
) ; end provide

(struct exn:fail:selector exn:fail ())

(define (sel-error fmt . args)
  (raise (exn:fail:selector (string-append "selector: " (apply format fmt args))
                            (current-continuation-marks)))
) ; end define sel-error

;; ---------------------------------------------------------------- 扫描器

(define (parse-selector str)
  (define len (string-length str))
  (define i 0)
  (define (peek) (and (< i len) (string-ref str i)))
  (define (advance!) (set! i (add1 i)))
  (define (ws? c) (memv c '(#\space #\tab #\newline #\return)))
  (define (skip-ws!)   ; → 是否跳过了至少一个空白
    (define start i)
    (let loop () (when (and (< i len) (ws? (string-ref str i))) (advance!) (loop)))
    (> i start)
  ) ; end define skip-ws!
  (define (ident-char? c)
    (or (char-alphabetic? c) (char-numeric? c) (memv c '(#\- #\_))
        (> (char->integer c) 127))
  ) ; end define ident-char?
  (define (read-ident who)
    (define start i)
    (let loop () (when (and (< i len) (ident-char? (string-ref str i))) (advance!) (loop)))
    (when (= i start) (sel-error "expected identifier after ~a at ~a" who start))
    (substring str start i)
  ) ; end define read-ident

  (define (compound-start? c)
    (or (ident-char? c) (memv c '(#\* #\# #\. #\[ #\:)))
  ) ; end define compound-start?

  ;; 平衡跳过 ( ... )，用于不支持的带参伪类 :nth-child(2n)
  (define (skip-balanced-parens!)
    (advance!)   ; 吃掉 (
    (let loop ([depth 1])
      (define c (peek))
      (cond [(not c) (sel-error "unclosed ( in pseudo-class")]
            [(char=? c #\() (advance!) (loop (add1 depth))]
            [(char=? c #\)) (advance!) (unless (= depth 1) (loop (sub1 depth)))]
            [else (advance!) (loop depth)])
    ) ; end loop
  ) ; end define skip-balanced-parens!

  (define (parse-attr)
    (advance!)   ; [
    (skip-ws!)
    (define k (string->symbol (string-downcase (read-ident "["))))
    (skip-ws!)
    (define c (peek))
    (cond
      [(eqv? c #\]) (advance!) (list 'attr k)]
      [(eqv? c #\=)
       (advance!) (skip-ws!)
       (define v
         (let ([q (peek)])
           (cond
             [(memv q '(#\" #\'))
              (advance!)
              (define start i)
              (let loop () (when (and (< i len) (not (char=? (string-ref str i) q)))
                             (advance!) (loop)))
              (unless (< i len) (sel-error "unclosed attribute value string"))
              (define v (substring str start i))
              (advance!)   ; 吃掉引号
              v]
             [else (read-ident "attribute value")])
         ) ; end let
       ) ; end define v
       (skip-ws!)
       (unless (eqv? (peek) #\]) (sel-error "expected ] at ~a" i))
       (advance!)
       (list 'attr= k v)]
      [(memv c '(#\~ #\^ #\$ #\| #\*))
       (sel-error "unsupported attribute operator ~a=" c)]
      [else (sel-error "expected ] or = in attribute selector at ~a" i)]
    ) ; end cond
  ) ; end define parse-attr

  (define (parse-pseudo)
    (advance!)   ; :
    (when (eqv? (peek) #\:) (advance!))   ; ::pseudo-element → 同伪类处理(恒假)
    (define name (string-downcase (read-ident ":")))
    (cond
      [(and (string=? name "not") (eqv? (peek) #\())
       (advance!) (skip-ws!)
       (define inner (parse-compound))
       (skip-ws!)
       (unless (eqv? (peek) #\)) (sel-error "expected ) closing :not("))
       (advance!)
       (list 'not inner)]
      [(eqv? (peek) #\()
       (skip-balanced-parens!)
       (list 'pseudo (string->symbol name))]
      [else (list 'pseudo (string->symbol name))]
    ) ; end cond
  ) ; end define parse-pseudo

  (define (parse-compound)
    (let loop ([simples '()] [any? #f])
      (define c (peek))
      (cond
        [(and c (char=? c #\*)) (advance!) (loop simples #t)]
        [(and c (char=? c #\#)) (advance!) (loop (cons (list 'id (read-ident "#")) simples) #t)]
        [(and c (char=? c #\.)) (advance!) (loop (cons (list 'class (read-ident ".")) simples) #t)]
        [(and c (char=? c #\[)) (loop (cons (parse-attr) simples) #t)]
        [(and c (char=? c #\:)) (loop (cons (parse-pseudo) simples) #t)]
        [(and c (ident-char? c))
         (define name (read-ident "tag"))
         (loop (cons (list 'tag (string->symbol (string-downcase name))) simples) #t)]
        [else
         (unless any? (sel-error "expected a simple selector at ~a" i))
         (reverse simples)]
      ) ; end cond
    ) ; end loop
  ) ; end define parse-compound

  (define (parse-complex)
    (let loop ([cur (parse-compound)] [steps '()])
      (define had-ws (skip-ws!))
      (define c (peek))
      (cond
        [(or (not c) (char=? c #\,)) (cons cur steps)]
        [(char=? c #\>)
         (advance!) (skip-ws!)
         (loop (parse-compound) (cons (cons 'child cur) steps))]
        [(and had-ws (compound-start? c))
         (loop (parse-compound) (cons (cons 'descendant cur) steps))]
        [else (sel-error "unexpected character ~a at ~a" c i)]
      ) ; end cond
    ) ; end loop
  ) ; end define parse-complex

  ;; 顶层：逗号并联
  (skip-ws!)
  (when (>= i len) (sel-error "empty selector"))
  (let loop ([alts (list (parse-complex))])
    (cond
      [(eqv? (peek) #\,)
       (advance!) (skip-ws!)
       (loop (cons (parse-complex) alts))]
      [(< i len) (sel-error "trailing garbage at ~a" i)]
      [else (reverse alts)]
    ) ; end cond
  ) ; end loop
) ; end define parse-selector
