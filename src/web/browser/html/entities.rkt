#lang racket/base
;; browser/html/entities.rkt — L4 · 实体解码（design-chrome.md L4 / §7.4）
;; 职责: &name; / &#n; / &#xn; / 无分号 legacy(最长匹配)——数据来自生成表(全量 2231)
;; 不做: tokenize、树构建
;; 依赖: racket/base browser/html/entities-data

(require racket/contract "entities-data.rkt")

(provide
 (contract-out
  [decode-entities (-> string? string?)]
  [entity-ref (-> string? (or/c string? #f))]
 ) ; end contract-out
) ; end provide

;; 查一个实体名（"amp;" 或 legacy "amp"）→ 替换串或 #f。测试/调试用。
(define (entity-ref name)
  (define n (string-length name))
  (if (and (> n 0) (char=? (string-ref name (sub1 n)) #\;))
      (hash-ref NAMED-ENTITIES (substring name 0 (sub1 n)) #f)
      (hash-ref LEGACY-ENTITIES name #f))
) ; end define entity-ref

(define (ident-char? c)
  (or (char-alphabetic? c) (char-numeric? c))
) ; end define ident-char?

;; 数字引用的码点守卫：0/超界/surrogate → U+FFFD（C1 remap 记为已知偏差）
(define (codepoint->string cp)
  (if (or (= cp 0) (> cp #x10FFFF) (and (>= cp #xD800) (<= cp #xDFFF)))
      "�"
      (string (integer->char cp)))
) ; end define codepoint->string

;; 在位置 i(指向 &)尝试解一个实体。
;; 返回 (values 替换串 新位置)；不成实体 → (values #f i)。
(define (try-entity s i len)
  (define j (add1 i))
  (cond
    [(>= j len) (values #f i)]
    ;; 数字引用
    [(char=? (string-ref s j) #\#)
     (define hex? (and (< (add1 j) len) (memv (string-ref s (add1 j)) '(#\x #\X))))
     (define d0 (if hex? (+ j 2) (add1 j)))
     (define (digit? c) (if hex?
                            (or (char-numeric? c)
                                (memv (char-downcase c) '(#\a #\b #\c #\d #\e #\f)))
                            (char-numeric? c)))
     (let loop ([k d0])
       (cond
         [(and (< k len) (digit? (string-ref s k))) (loop (add1 k))]
         [(= k d0) (values #f i)]     ; &# 后无数字
         [else
          (define cp (string->number (substring s d0 k) (if hex? 16 10)))
          (define k* (if (and (< k len) (char=? (string-ref s k) #\;)) (add1 k) k))
          (values (codepoint->string cp) k*)]
       ) ; end cond
     )] ; end let loop
    ;; 命名引用
    [(ident-char? (string-ref s j))
     (let loop ([k j])
       (cond
         [(and (< k len) (ident-char? (string-ref s k)) (< (- k j) 48)) (loop (add1 k))]
         [else
          (define name (substring s j k))
          (cond
            ;; 规范形态：&name;
            [(and (< k len) (char=? (string-ref s k) #\;))
             (define r (hash-ref NAMED-ENTITIES name #f))
             (if r (values r (add1 k)) (values #f i))]
            ;; legacy 形态：无分号，最长前缀匹配
            [else
             (let shrink ([l (string-length name)])
               (cond
                 [(< l 2) (values #f i)]
                 [else
                  (define r (hash-ref LEGACY-ENTITIES (substring name 0 l) #f))
                  (if r (values r (+ j l)) (shrink (sub1 l)))]
               ) ; end cond
             )] ; end shrink
          ) ; end cond
         ] ; end else
       ) ; end cond
     )] ; end let loop
    [else (values #f i)]
  ) ; end cond
) ; end define try-entity

(define (decode-entities s)
  (cond
    [(not (regexp-match? #rx"&" s)) s]      ; 快路：无 & 直通零拷贝
    [else
     (define len (string-length s))
     (define out (open-output-string))
     (let loop ([i 0])
       (cond
         [(>= i len) (get-output-string out)]
         [(char=? (string-ref s i) #\&)
          (define-values (rep i*) (try-entity s i len))
          (cond
            [rep (write-string rep out) (loop i*)]
            [else (write-char #\& out) (loop (add1 i))])]
         [else
          (define j (or (for/first ([k (in-range (add1 i) len)]
                                    #:when (char=? (string-ref s k) #\&))
                          k)
                        len))
          (write-string (substring s i j) out)
          (loop j)]
       ) ; end cond
     ) ; end loop
    ] ; end else
  ) ; end cond
) ; end define decode-entities
