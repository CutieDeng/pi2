#lang racket/base
;; unicode-policy.rkt — 显示时的 Unicode 策略（design-gui.md §30，用户决定）：只放行「普通」字符，其余画成标记、不执行。
;; 纯函数；只决定怎么显示，不改原始数据（复制出来仍是原文：标记与原字符一一对应，下标不变）。
;;
;; (unicode-scan s) → (values 显示串 标记表 形近字下标)
;;   显示串：与 s 等长；C0 控制字符（除 \t \n）与 DEL 换成控制符图形（␛ ␇ ␡…），U+2028 / 2029 换成 \n（当换行）；其余照旧
;;   标记表：(listof (vector 下标 种类 文字))；种类 ∈ bidi invisible tag rtl combining c1 vs；文字 "" = 零宽（属于前一个标记，例如 tag 串、多余组合符）
;;   形近字下标：一个「词」里混了拉丁 / 希腊 / 西里尔等几种文字系统时，这个词的全部下标（画虚线下划线）

(require racket/list racket/string)

;; ---------------------------------------------------------------- 分类

(define BIDI
  (hasheqv #x061C "ALM" #x200E "LRM" #x200F "RLM" #x202A "LRE" #x202B "RLE" #x202C "PDF" #x202D "LRO" #x202E "RLO"
           #x2066 "LRI" #x2067 "RLI" #x2068 "FSI" #x2069 "PDI"))

;; 不可见 / 格式字符（ZWJ、变体选择符另判）
(define INVISIBLE
  (hasheqv #x00AD "SHY" #x034F "CGJ" #x115F "HF" #x1160 "HF" #x17B4 "KIV" #x17B5 "KIV" #x180E "MVS"
           #x200B "ZWSP" #x200C "ZWNJ" #x2060 "WJ" #x2061 "FA" #x2062 "IT" #x2063 "IS" #x2064 "IP"
           #x206A "ISS" #x206B "ASS" #x206C "IAFS" #x206D "AAFS" #x206E "NADS" #x206F "NODS"
           #x3164 "HF" #xFEFF "BOM" #xFFA0 "HF" #xFFF9 "IAA" #xFFFA "IAS" #xFFFB "IAT"))

(define (in? n a b) (and (<= a n) (<= n b)))

;; 从右到左的文字（用户决定：不支持，显示成码点）
(define (rtl? n)
  (or (in? n #x0590 #x08FF) (in? n #xFB1D #xFDFF) (in? n #xFE70 #xFEFE) (in? n #x10800 #x10FFF) (in? n #x1E800 #x1EFFF)))

(define (tag? n) (in? n #xE0000 #xE007F))
(define (vs? n) (or (in? n #xFE00 #xFE0F) (in? n #xE0100 #xE01EF)))
(define (c1? n) (in? n #x80 #x9F))

;; emoji / 符号（变体选择符、ZWJ 只在它们旁边放行）
(define (pictographic? n)
  (or (in? n #x1F000 #x1FAFF) (in? n #x2600 #x27BF) (in? n #x2190 #x21FF) (in? n #x2300 #x23FF) (in? n #x2B00 #x2BFF)
      (in? n #x1F1E6 #x1F1FF) (in? n #x3030 #x3030) (in? n #x303D #x303D) (in? n #x3297 #x3299) (in? n #x00A9 #x00AE)
      (in? n #x2122 #x2139) (in? n #x24C2 #x24C2) (in? n #x25A0 #x25FF) (in? n #x2934 #x2935) (in? n #x20E3 #x20E3)))
(define (keycap-base? n) (or (in? n #x30 #x39) (= n #x23) (= n #x2A)))
(define (emoji-modifier? n) (in? n #x1F3FB #x1F3FF))

(define (combining? c) (memq (char-general-category c) '(mn me)))
(define MAX-COMBINING 4)                                  ; 一个基字最多带几个组合符（多的折成一个标记）

(define (hex n) (string-upcase (number->string n 16)))
(define (u+ n) (string-append "U+" (let ([h (hex n)]) (if (< (string-length h) 4) (string-append (make-string (- 4 (string-length h)) #\0) h) h))))

;; ---------------------------------------------------------------- 形近字（混合文字系统）

(define (script-of n)
  (cond [(or (in? n #x41 #x5A) (in? n #x61 #x7A) (in? n #xC0 #x24F) (in? n #x1E00 #x1EFF)) 'latin]
        [(or (in? n #x370 #x3FF) (in? n #x1F00 #x1FFF)) 'greek]
        [(or (in? n #x400 #x52F) (in? n #x2DE0 #x2DFF) (in? n #xA640 #xA69F) (in? n #x1C80 #x1C8F)) 'cyrillic]
        [(in? n #x530 #x58F) 'armenian]
        [(in? n #x13A0 #x13FF) 'cherokee]
        [else #f]))

;; 「词」= 连续的字母（加中间的组合符）；混了两种以上文字系统 → 整个词的字母下标（画虚线下划线：这个词不对劲；悬停看码点）
(define (mixed-script-indices s)
  (define n (string-length s))
  (let loop ([i 0] [out '()])
    (cond
      [(>= i n) (reverse out)]
      [(not (char-alphabetic? (string-ref s i))) (loop (add1 i) out)]
      [else
       (define j (let w ([j i]) (if (and (< j n) (or (char-alphabetic? (string-ref s j)) (combining? (string-ref s j)))) (w (add1 j)) j)))
       (define tagged (for/list ([k (in-range i j)] #:when (script-of (char->integer (string-ref s k)))) (cons k (script-of (char->integer (string-ref s k))))))
       (define counts (for/fold ([h (hasheq)]) ([p (in-list tagged)]) (hash-update h (cdr p) add1 0)))
       (cond
         [(< (hash-count counts) 2) (loop j out)]
         [else (loop j (append (reverse (for/list ([k (in-range i j)]) k)) out))])])))

;; ---------------------------------------------------------------- 扫描

(define (unicode-scan s)
  (define n (string-length s))
  (define out (string-copy s))
  (define marks '())
  (define (mark! i kind text) (set! marks (cons (vector i kind text) marks)))
  (let loop ([i 0] [comb 0])
    (when (< i n)
      (define c (string-ref s i))
      (define k (char->integer c))
      (define prev (and (> i 0) (char->integer (string-ref s (sub1 i)))))
      (define next (and (< (add1 i) n) (char->integer (string-ref s (add1 i)))))
      (cond
        [(and (< k 32) (not (memv k '(9 10)))) (string-set! out i (integer->char (+ #x2400 k))) (loop (add1 i) 0)]   ; ␀…␟
        [(= k #x7F) (string-set! out i (integer->char #x2421)) (loop (add1 i) 0)]                                  ; ␡
        [(memv k '(#x2028 #x2029)) (string-set! out i #\newline) (loop (add1 i) 0)]
        [(hash-ref BIDI k #f) => (lambda (l) (mark! i 'bidi l) (loop (add1 i) 0))]
        [(= k #x200D)                                                       ; ZWJ：两边都是 emoji / 符号才放行（emoji 序列）
         (if (and prev next (or (pictographic? prev) (emoji-modifier? prev) (vs? prev)) (pictographic? next))
             (loop (add1 i) comb)
             (begin (mark! i 'invisible "ZWJ") (loop (add1 i) 0)))]
        [(vs? k)                                                            ; 变体选择符：跟在 emoji / 符号 / keycap 底字后放行
         (if (and prev (or (pictographic? prev) (keycap-base? prev)))
             (loop (add1 i) comb)
             (begin (mark! i 'vs (if (in? k #xFE00 #xFE0F) (format "VS~a" (add1 (- k #xFE00))) "VS")) (loop (add1 i) 0)))]
        [(hash-ref INVISIBLE k #f) => (lambda (l) (mark! i 'invisible l) (loop (add1 i) 0))]
        [(tag? k)                                                           ; tag 串：解码成一条「隐藏文字」，后面的 tag 零宽
         (define j (let t ([j i]) (if (and (< j n) (tag? (char->integer (string-ref s j)))) (t (add1 j)) j)))
         (define flag? (and prev (= prev #x1F3F4) (= (char->integer (string-ref s (sub1 j))) #xE007F)))   ; 🏴 + tag + 取消 tag = 地区旗（England 等），放行
         (define hidden (list->string (for/list ([m (in-range i j)] #:when (in? (char->integer (string-ref s m)) #xE0020 #xE007E))
                                        (integer->char (- (char->integer (string-ref s m)) #xE0000)))))
         (unless flag?
           (mark! i 'tag hidden)
           (for ([m (in-range (add1 i) j)]) (mark! m 'tag "")))
         (loop j 0)]
        [(rtl? k) (mark! i 'rtl (u+ k)) (loop (add1 i) 0)]
        [(c1? k) (mark! i 'c1 (u+ k)) (loop (add1 i) 0)]
        [(combining? c)                                                     ; 组合符：前 MAX-COMBINING 个放行，多的折成一个标记
         (cond [(< comb MAX-COMBINING) (loop (add1 i) (add1 comb))]
               [else
                (define j (let t ([j i]) (if (and (< j n) (combining? (string-ref s j))) (t (add1 j)) j)))
                (mark! i 'combining (format "+~a" (- j i)))
                (for ([m (in-range (add1 i) j)]) (mark! m 'combining ""))
                (loop j 0)])]
        [else (loop (add1 i) 0)])))
  (values out (reverse marks) (mixed-script-indices s))
) ; end define unicode-scan

;; 标记的显示文字（tag 前加「隐藏：」；零宽的 ""）
(define (mark-label kind text hidden-label)
  (cond [(string=? text "") ""]
        [(eq? kind 'tag) (string-append "⚠ " hidden-label "：" (if (> (string-length text) 60) (string-append (substring text 0 60) "…") text))]
        [else text]))

(provide unicode-scan mark-label mixed-script-indices u+)
