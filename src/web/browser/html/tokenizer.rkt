#lang racket/base
;; browser/html/tokenizer.rkt — L4 · 五态 tokenizer（design-chrome.md L4 / §2.3）
;; 职责: 字符流 → token 流（data/tag/attr/comment/rawtext），拉式(可暂停天然成立)
;; 状态对应 WHATWG 13.2.5 的实用子集；§7.3 的表驱动宏化留作后续重构(语义先行)
;; 已知偏差: doctype/CDATA 按 bogus 跳过；重复属性首键胜(同规范)
;; 不做: 树构建、实体表本体(entities.rkt)
;; 依赖: racket/base browser/html/entities

(require racket/contract racket/string "entities.rkt")

(provide
 (struct-out tok-open) (struct-out tok-close) (struct-out tok-text)
 (struct-out tok-comment) (struct-out tok-eof)
 (contract-out
  [make-tokenizer (-> string? (-> any/c))]
 ) ; end contract-out
) ; end provide

;; token 族（不可变 prefab,§2.2 规约 5）
(struct tok-open (name attrs self-closing?) #:prefab)   ; attrs: 保序 alist,首键胜
(struct tok-close (name) #:prefab)
(struct tok-text (s) #:prefab)
(struct tok-comment (s) #:prefab)
(struct tok-eof () #:prefab)

(define RAWTEXT-TAGS '(script style))          ; 原文,不解实体
(define RCDATA-TAGS  '(title textarea))        ; 解实体,不解标签

(define (make-tokenizer s)
  (define len (string-length s))
  (define i 0)
  (define pending #f)   ; (cons tag-sym 'raw|'rcdata) —— 刚开的 rawtext/rcdata 元素

  (define (peek [k 0]) (and (< (+ i k) len) (string-ref s (+ i k))))
  (define (letter? c) (and c (char-alphabetic? c)))
  (define (ws? c) (memv c '(#\space #\tab #\newline #\return #\page)))

  ;; -------- rawtext/rcdata: 读到 </tag 边界 --------
  (define (read-rawtext! tag kind)
    (define rx (regexp (string-append "(?i:</" (symbol->string tag) "[ \t\n\r/>])")))
    (define m (regexp-match-positions rx s i))
    (define end (if m (caar m) len))
    (define raw (substring s i end))
    (set! i end)
    (set! pending #f)
    (if (string=? raw "")
        (next-token)
        (tok-text (if (eq? kind 'rcdata) (decode-entities raw) raw)))
  ) ; end define read-rawtext!

  ;; -------- tag name / attrs --------
  (define (read-name!)
    (define start i)
    (let loop ()
      (define c (peek))
      (when (and c (or (char-alphabetic? c) (char-numeric? c) (char=? c #\-)))
        (set! i (add1 i)) (loop))
    ) ; end loop
    (string->symbol (string-downcase (substring s start i)))
  ) ; end define read-name!

  (define (skip-ws!)
    (let loop () (when (ws? (peek)) (set! i (add1 i)) (loop)))
  ) ; end define skip-ws!

  (define (read-attr-value!)
    (define q (peek))
    (cond
      [(memv q '(#\" #\'))
       (set! i (add1 i))
       (define start i)
       (let loop () (define c (peek))
         (when (and c (not (char=? c q))) (set! i (add1 i)) (loop)))
       (define v (substring s start i))
       (when (< i len) (set! i (add1 i)))   ; 吃掉引号(EOF 容错)
       (decode-entities v)]
      [else
       (define start i)
       (let loop () (define c (peek))
         (when (and c (not (ws? c)) (not (char=? c #\>)))
           (set! i (add1 i)) (loop)))
       (decode-entities (substring s start i))]
    ) ; end cond
  ) ; end define read-attr-value!

  (define (read-attrs!)   ; → (values attrs self-closing?)
    (let loop ([attrs '()])
      (skip-ws!)
      (define c (peek))
      (cond
        [(not c) (values (reverse attrs) #f)]
        [(char=? c #\>) (set! i (add1 i)) (values (reverse attrs) #f)]
        [(and (char=? c #\/) (eqv? (peek 1) #\>))
         (set! i (+ i 2)) (values (reverse attrs) #t)]
        [(char=? c #\/) (set! i (add1 i)) (loop attrs)]   ; 游离 / 忽略
        [else
         (define start i)
         (let nloop () (define c (peek))
           (when (and c (not (ws? c)) (not (memv c '(#\= #\> #\/))))
             (set! i (add1 i)) (nloop)))
         (define k (string->symbol (string-downcase (substring s start i))))
         (skip-ws!)
         (define v
           (cond [(eqv? (peek) #\=) (set! i (add1 i)) (skip-ws!) (read-attr-value!)]
                 [else ""])
         ) ; end define v
         (loop (if (assq k attrs) attrs (cons (cons k v) attrs)))]   ; 首键胜
      ) ; end cond
    ) ; end loop
  ) ; end define read-attrs!

  (define (read-open-tag!)
    (set! i (add1 i))   ; 吃掉 <
    (define name (read-name!))
    (define-values (attrs sc?) (read-attrs!))
    (cond
      [(and (not sc?) (memq name RAWTEXT-TAGS)) (set! pending (cons name 'raw))]
      [(and (not sc?) (memq name RCDATA-TAGS))  (set! pending (cons name 'rcdata))]
      [else (void)]
    ) ; end cond
    (tok-open name attrs sc?)
  ) ; end define read-open-tag!

  (define (read-close-tag!)
    (set! i (+ i 2))   ; 吃掉 </
    (define name (read-name!))
    (let loop () (define c (peek))   ; 关标签属性丢弃(同规范)
      (when (and c (not (char=? c #\>))) (set! i (add1 i)) (loop)))
    (when (< i len) (set! i (add1 i)))
    (tok-close name)
  ) ; end define read-close-tag!

  (define (read-comment!)
    (set! i (+ i 4))   ; 吃掉 <!--
    (define m (regexp-match-positions #rx"-->" s i))
    (define end (if m (caar m) len))
    (define text (substring s i end))
    (set! i (if m (cdar m) len))
    (tok-comment text)
  ) ; end define read-comment!

  (define (skip-bogus!)   ; <!doctype …> / <![CDATA[ …]]> / <?…> → 跳到 >
    (let loop () (define c (peek))
      (cond [(not c) (void)]
            [(char=? c #\>) (set! i (add1 i))]
            [else (set! i (add1 i)) (loop)]))
    (next-token)
  ) ; end define skip-bogus!

  (define (read-text!)
    (define start i)
    (let loop ()
      (define c (peek))
      (cond
        [(not c) (void)]
        [(char=? c #\<)
         (define c1 (peek 1))
         (if (or (letter? c1) (memv c1 '(#\/ #\! #\?)))
             (void)                              ; 疑似标签,停在 <
             (begin (set! i (add1 i)) (loop)))]  ; 字面 <
        [else (set! i (add1 i)) (loop)]
      ) ; end cond
    ) ; end loop
    (tok-text (decode-entities (substring s start i)))
  ) ; end define read-text!

  (define (next-token)
    (cond
      [pending (read-rawtext! (car pending) (cdr pending))]
      [(>= i len) (tok-eof)]
      [(char=? (string-ref s i) #\<)
       (define c1 (peek 1))
       (cond
         [(and (eqv? c1 #\!) (eqv? (peek 2) #\-) (eqv? (peek 3) #\-)) (read-comment!)]
         [(memv c1 '(#\! #\?)) (set! i (+ i 2)) (skip-bogus!)]
         [(and (eqv? c1 #\/) (letter? (peek 2))) (read-close-tag!)]
         [(eqv? c1 #\/) (set! i (+ i 2)) (skip-bogus!)]
         [(letter? c1) (read-open-tag!)]
         [else (read-text!)]   ; 孤立 < 按文本
       ) ; end cond
      ] ; end <
      [else (read-text!)]
    ) ; end cond
  ) ; end define next-token

  next-token
) ; end define make-tokenizer
