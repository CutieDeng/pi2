#lang racket/base
;; browser/js/lex.rkt — L7 · JS 词法分析（实用子集）
;; 对标: ECMAScript 2023 §12（Lexical Grammar，子集）（design-jsobj.md L7）
;; 依赖: racket/base racket/list
;; token: (vector 'kind value)  kind ∈ num str name kw punc eof

(require racket/list)

(define KEYWORDS
  '("var" "let" "const" "function" "return" "if" "else" "while" "for"
    "new" "typeof" "delete" "true" "false" "null" "undefined" "this"))

(define (id-start? c) (or (char-alphabetic? c) (char=? c #\_) (char=? c #\$)))
(define (id-part? c)  (or (id-start? c) (char-numeric? c)))

;; 多字符标点（长优先）
(define PUNCS
  '("===" "!==" ">>>" "&&" "||" "==" "!=" "<=" ">=" "+=" "-=" "*=" "/=" "=>"
    "++" "--" "(" ")" "{" "}" "[" "]" ";" "," "." "=" "+" "-" "*" "/" "%"
    "<" ">" "!" "?" ":" "&" "|"))

(define (lex str)
  (define n (string-length str))
  (define toks '())
  (define (emit! k v) (set! toks (cons (vector k v) toks)))
  (let loop ([i 0])
    (cond
      [(>= i n) (emit! 'eof #f) (reverse toks)]
      [else
       (define c (string-ref str i))
       (cond
         [(char-whitespace? c) (loop (add1 i))]
         ;; 行注释 //
         [(and (char=? c #\/) (< (add1 i) n) (char=? (string-ref str (add1 i)) #\/))
          (let skip ([j (+ i 2)]) (if (or (>= j n) (char=? (string-ref str j) #\newline)) (loop j) (skip (add1 j))))]
         ;; 块注释 /* */
         [(and (char=? c #\/) (< (add1 i) n) (char=? (string-ref str (add1 i)) #\*))
          (let skip ([j (+ i 2)])
            (cond [(>= (add1 j) n) (loop n)]
                  [(and (char=? (string-ref str j) #\*) (char=? (string-ref str (add1 j)) #\/)) (loop (+ j 2))]
                  [else (skip (add1 j))]))]
         ;; 数字：[0-9.]+ 后可选指数 (e|E)(+|-)?digits
         [(or (char-numeric? c) (and (char=? c #\.) (< (add1 i) n) (char-numeric? (string-ref str (add1 i)))))
          (define (digits/dot j) (if (and (< j n) (or (char-numeric? (string-ref str j)) (char=? (string-ref str j) #\.)))
                                     (digits/dot (add1 j)) j))
          (define j1 (digits/dot i))
          (define j2 (if (and (< j1 n) (memv (string-ref str j1) '(#\e #\E)))
                         (let ([k (if (and (< (add1 j1) n) (memv (string-ref str (add1 j1)) '(#\+ #\-))) (+ j1 2) (add1 j1))])
                           (let dig ([m k]) (if (and (< m n) (char-numeric? (string-ref str m))) (dig (add1 m)) m)))
                         j1))
          (define v (string->number (substring str i j2)))
          (unless (real? v) (error 'lex "bad number: ~a" (substring str i j2)))
          (emit! 'num (exact->inexact v)) (loop j2)]
         ;; 字符串
         [(or (char=? c #\") (char=? c #\'))
          (let strloop ([j (add1 i)] [acc '()])
            (cond
              [(>= j n) (error 'lex "unterminated string")]
              [(char=? (string-ref str j) c) (emit! 'str (list->string (reverse acc))) (loop (add1 j))]
              [(char=? (string-ref str j) #\\)
               (define e (string-ref str (add1 j)))
               (strloop (+ j 2) (cons (case e [(#\n) #\newline] [(#\t) #\tab] [(#\r) #\return]
                                            [(#\\) #\\] [(#\") #\"] [(#\') #\'] [else e]) acc))]
              [else (strloop (add1 j) (cons (string-ref str j) acc))]))]
         ;; 标识符/关键字
         [(id-start? c)
          (let id ([j i])
            (if (and (< j n) (id-part? (string-ref str j))) (id (add1 j))
                (let ([s (substring str i j)])
                  (emit! (if (member s KEYWORDS) 'kw 'name) s) (loop j))))]
         ;; 标点（长优先）
         [else
          (define p (for/or ([pu (in-list PUNCS)])
                      (and (<= (+ i (string-length pu)) n)
                           (string=? (substring str i (+ i (string-length pu))) pu) pu)))
          (if p (begin (emit! 'punc p) (loop (+ i (string-length p))))
              (error 'lex "unexpected char: ~a" c))])])))

(provide lex KEYWORDS)
