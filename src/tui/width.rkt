#lang tstring racket
;; tui/width.rkt — Unicode 显示宽度（wcwidth 等价）
;; 终端里一个码位占几列：CJK/emoji 占 2，组合符/零宽占 0，其余占 1。
;; 用于行编辑器的光标定位与重绘对齐。

;; 区间表：升序、不重叠的 (lo . hi) 闭区间列表。二分查找命中。
(define (in-ranges? code ranges)
  (let loop ([lo 0] [hi (sub1 (vector-length ranges))])
    (cond
      [(> lo hi) #f]
      [else
       (define mid (quotient (+ lo hi) 2))
       (define r (vector-ref ranges mid))
       (cond
         [(< code (car r)) (loop lo (sub1 mid))]
         [(> code (cdr r)) (loop (add1 mid) hi)]
         [else #t]
       ) ; end cond
      ] ; end else
    ) ; end cond
  ) ; end let loop
) ; end define in-ranges?

;; 零宽：组合附加符、变体选择符、零宽空格类、部分格式控制。
(define ZERO-WIDTH
  (vector
   '(#x0300 . #x036F)     ; 组合附加符
   '(#x0483 . #x0489)
   '(#x0591 . #x05BD)
   '(#x0610 . #x061A)
   '(#x064B . #x065F)     ; 阿拉伯组合符
   '(#x0670 . #x0670)
   '(#x06D6 . #x06DC)
   '(#x0900 . #x0903)     ; 天城文（部分）
   '(#x093A . #x093C)
   '(#x0941 . #x0948)
   '(#x094D . #x094D)
   '(#x0E31 . #x0E31)     ; 泰文组合
   '(#x0E34 . #x0E3A)
   '(#x0E47 . #x0E4E)
   '(#x200B . #x200F)     ; 零宽空格 / 方向标记
   '(#x2028 . #x202E)
   '(#x2060 . #x2064)     ; 词连接符等
   '(#x20D0 . #x20FF)     ; 组合用记号
   '(#xFE00 . #xFE0F)     ; 变体选择符
   '(#xFE20 . #xFE2F)     ; 组合半符
   '(#xFEFF . #xFEFF)     ; 零宽不换行空格 (BOM)
   '(#x1AB0 . #x1AFF)
   '(#x1DC0 . #x1DFF)
   '(#xE0100 . #xE01EF)   ; 变体选择符补充
  ) ; end vector
) ; end define ZERO-WIDTH

;; 双宽：East Asian Wide / Fullwidth 及 emoji。
(define WIDE
  (vector
   '(#x1100 . #x115F)     ; 谚文字母
   '(#x2E80 . #x303E)     ; CJK 部首、康熙部首、假名标点
   '(#x3041 . #x33FF)     ; 平/片假名、CJK 符号
   '(#x3400 . #x4DBF)     ; CJK 扩展 A
   '(#x4E00 . #x9FFF)     ; CJK 统一表意
   '(#xA000 . #xA4CF)     ; 彝文
   '(#xAC00 . #xD7A3)     ; 谚文音节
   '(#xF900 . #xFAFF)     ; CJK 兼容表意
   '(#xFE10 . #xFE19)     ; 竖排标点
   '(#xFE30 . #xFE6F)     ; CJK 兼容形式、小写变体
   '(#xFF00 . #xFF60)     ; 全角 ASCII
   '(#xFFE0 . #xFFE6)     ; 全角符号
   '(#x1F300 . #x1F64F)   ; emoji：符号与图形、表情
   '(#x1F680 . #x1F6FF)   ; 交通与地图符号
   '(#x1F900 . #x1FAFF)   ; 补充符号与图形
   '(#x20000 . #x2FFFD)   ; CJK 扩展 B–F
   '(#x30000 . #x3FFFD)   ; CJK 扩展 G
  ) ; end vector
) ; end define WIDE

;; 单个字符的列宽：0 | 1 | 2
(define (char-width ch)
  (define code (char->integer ch))
  (cond
    [(= code 0) 0]                          ; NUL
    [(< code #x20) 0]                        ; C0 控制符（不可打印）
    [(and (>= code #x7F) (< code #xA0)) 0]   ; DEL 与 C1 控制符
    [(in-ranges? code ZERO-WIDTH) 0]
    [(in-ranges? code WIDE) 2]
    [else 1]
  ) ; end cond
) ; end define char-width

;; 字符串总列宽
(define (string-width s)
  (for/sum ([ch (in-string s)])
    (char-width ch)
  ) ; end for/sum
) ; end define string-width

;; 前 n 个字符（char 索引）的列宽——光标列定位用
(define (string-width-upto s n)
  (for/sum ([ch (in-string s)] [i (in-naturals)] #:when (< i n))
    (char-width ch)
  ) ; end for/sum
) ; end define string-width-upto

(provide
 char-width
 string-width
 string-width-upto
) ; end provide
