#lang racket/base
;; linebreak.rkt — 断行机会（S3）：UAX#14 的子集 + CJK 逐字可断 + 避头尾，只落在字素边界上。
;; 与 UAX#14 的已知偏离（有意）：'/' 之后可断（agent 输出里长路径 / URL 多）；不处理数字前后缀、
;; 引号的上下文规则、RTL。下标为码点下标；「i 处可断」= 可以在 s[i] 之前换行。

(define (grapheme-starts s)            ; → (vector bool)，长度 n+1；#t = 字素起点（含末尾 n）
  (define n (string-length s))
  (define v (make-vector (add1 n) #f))
  (let loop ([i 0])
    (vector-set! v i #t)
    (when (< i n) (loop (+ i (max 1 (string-grapheme-span s i))))))
  v
) ; end define grapheme-starts

(define (in-ranges? n ranges)
  (for/or ([r (in-list ranges)]) (<= (car r) n (cdr r)))
) ; end define in-ranges?

;; 表意 / 类表意：两侧都可断（UAX#14 ID 类近似；含假名、谚文、全角形式、emoji 图形区）
(define IDEOGRAPHIC
  '((#x2E80 . #x2FFF) (#x3040 . #x30FF) (#x3100 . #x31FF) (#x3200 . #x33FF) (#x3400 . #x4DBF)
    (#x4E00 . #x9FFF) (#xA000 . #xA4CF) (#xAC00 . #xD7AF) (#xF900 . #xFAFF) (#xFE30 . #xFE4F)
    (#xFF00 . #xFFEF) (#x1F000 . #x1FAFF) (#x20000 . #x3FFFF)))

(define (ideographic? c) (in-ranges? (char->integer c) IDEOGRAPHIC))

;; U+FFFC：行内原子框（输入框里的折叠 chip），两侧都可断（UAX#14 CB 类按可断处理）
(define (inline-box? c) (char=? c (integer->char #xFFFC)))

(define IDEOGRAPHIC-SPACE (integer->char #x3000))
(define (space? c) (or (char=? c #\space) (char=? c #\tab) (char=? c IDEOGRAPHIC-SPACE)))

;; 避头：不能出现在行首（其前不可断）
(define CLOSING
  (string->list ")]},.;:!?%）］｝，．：；！？％、。」』】〉》〕〗〙〛｠・ー…‥〜々ゝゞヽヾ’”»›ぁぃぅぇぉっゃゅょゎァィゥェォッャュョヮヵヶ"))
;; 避尾：不能出现在行末（其后不可断）
(define OPENING (string->list "([{（［｛「『【〈《〔〖〘〚｟‘“«‹"))

(define (closing? c) (memv c CLOSING))
(define (opening? c) (memv c OPENING))

(define (break-allowed? s i starts)
  (define prev (string-ref s (sub1 i)))
  (define cur (string-ref s i))
  (and (vector-ref starts i)
       (not (closing? cur))
       (not (opening? prev))
       (not (space? cur))                                   ; 在空白串之后断，不在其前断
       (or (space? prev)
           (ideographic? prev)
           (ideographic? cur)
           (inline-box? prev)
           (inline-box? cur)
           (and (char=? prev #\-) (>= i 2) (char-alphabetic? (string-ref s (- i 2))) (char-alphabetic? cur))
           (char=? prev #\/)))
) ; end define break-allowed?

;; → 升序的可断位置列表（不含 0 与 n）
(define (break-opportunities s)
  (define starts (grapheme-starts s))
  (for/list ([i (in-range 1 (string-length s))] #:when (break-allowed? s i starts)) i)
) ; end define break-opportunities

(provide break-opportunities grapheme-starts ideographic? inline-box? space? closing? opening?)
