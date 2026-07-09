#lang tstring racket
;; tui/lineedit.rkt — 行编辑器（design: TUI 编辑层）
;; 纯状态迁移 (ledit-apply) 与渲染 (ledit-render) 分离，故编辑逻辑完全可离线测试。
;; Unicode 感知：光标按字符移动，重绘按显示宽度定位。

(require
 (file "keys.rkt")
 (file "width.rkt")
) ; end require

(struct ledit
  (text      ; string — 当前行内容
   cursor    ; exact 0..(string-length text) — 字符索引
   history   ; (listof string) — 最新在前
   hist-idx  ; #f=编辑新行，否则 history 下标
   stash     ; 浏览历史时暂存的新行内容
   kill      ; string — kill-ring（Ctrl-K/U/W 存，Ctrl-Y 取）
  ) ; end fields
  #:prefab
) ; end struct ledit

(define (make-ledit #:history [history '()] #:text [text ""])
  (ledit text (string-length text) history #f "" "")
) ; end define make-ledit

(define (ledit-value st) (ledit-text st))

;; ---------------------------------------------------------------- 词边界

(define (word-char? ch)
  (or (char-alphabetic? ch) (char-numeric? ch))
) ; end define word-char?

;; 光标前一个词的起点（先跳非词符，再跳词符）
(define (word-start-before text cursor)
  (let* ([i (let skip-sep ([i cursor])
              (if (and (> i 0) (not (word-char? (string-ref text (sub1 i)))))
                  (skip-sep (sub1 i))
                  i
              ) ; end if
            )] ; end skip-sep
         [j (let skip-word ([i i])
              (if (and (> i 0) (word-char? (string-ref text (sub1 i))))
                  (skip-word (sub1 i))
                  i
              ) ; end if
            )]) ; end skip-word
    j
  ) ; end let*
) ; end define word-start-before

;; 光标后一个词的终点
(define (word-end-after text cursor)
  (define n (string-length text))
  (let* ([i (let skip-sep ([i cursor])
              (if (and (< i n) (not (word-char? (string-ref text i))))
                  (skip-sep (add1 i))
                  i
              ) ; end if
            )] ; end skip-sep
         [j (let skip-word ([i i])
              (if (and (< i n) (word-char? (string-ref text i)))
                  (skip-word (add1 i))
                  i
              ) ; end if
            )]) ; end skip-word
    j
  ) ; end let*
) ; end define word-end-after

;; ---------------------------------------------------------------- 编辑原语

(define (with-text st text cursor)
  (struct-copy ledit st [text text] [cursor (max 0 (min cursor (string-length text)))])
) ; end define with-text

(define (insert-str st s)
  (define t (ledit-text st))
  (define c (ledit-cursor st))
  (with-text st
             (string-append (substring t 0 c) s (substring t c))
             (+ c (string-length s))
  ) ; end with-text
) ; end define insert-str

(define (delete-range st from to)   ; 删除 [from,to)，光标落到 from
  (define t (ledit-text st))
  (define lo (max 0 (min from to)))
  (define hi (min (string-length t) (max from to)))
  (with-text st (string-append (substring t 0 lo) (substring t hi)) lo)
) ; end define delete-range

(define (kill-range st from to)     ; 删除并存入 kill-ring
  (define t (ledit-text st))
  (define lo (max 0 (min from to)))
  (define hi (min (string-length t) (max from to)))
  (define killed (substring t lo hi))
  (struct-copy ledit (delete-range st lo hi) [kill killed])
) ; end define kill-range

;; ---------------------------------------------------------------- 历史

(define (history-nav st dir)   ; dir: 'prev（更旧）| 'next（更新）
  (define hist (ledit-history st))
  (define n (length hist))
  (cond
    [(zero? n) (values st 'ignore)]
    [(eq? dir 'prev)
     (define idx (if (ledit-hist-idx st) (add1 (ledit-hist-idx st)) 0))
     (cond
       [(>= idx n) (values st 'ignore)]     ; 已到最旧
       [else
        (define stash (if (ledit-hist-idx st) (ledit-stash st) (ledit-text st)))
        (define entry (list-ref hist idx))
        (values (struct-copy ledit st
                             [text entry] [cursor (string-length entry)]
                             [hist-idx idx] [stash stash])
                'edit)
       ] ; end else
     ) ; end cond
    ] ; end prev case
    [else                                     ; next（更新）
     (cond
       [(not (ledit-hist-idx st)) (values st 'ignore)]
       [(= (ledit-hist-idx st) 0)
        ;; 回到正在编辑的新行
        (values (struct-copy ledit st
                             [text (ledit-stash st)]
                             [cursor (string-length (ledit-stash st))]
                             [hist-idx #f])
                'edit)
       ] ; end back-to-fresh case
       [else
        (define idx (sub1 (ledit-hist-idx st)))
        (define entry (list-ref hist idx))
        (values (struct-copy ledit st
                             [text entry] [cursor (string-length entry)]
                             [hist-idx idx])
                'edit)
       ] ; end else
     ) ; end cond
    ] ; end next case
  ) ; end cond
) ; end define history-nav

;; ---------------------------------------------------------------- 主迁移

;; (ledit-apply st kev) -> (values st* action)
;; action: 'edit | 'submit | 'cancel | 'eof | 'clear-screen | 'ignore
(define (ledit-apply st k)
  (define t (ledit-text st))
  (define c (ledit-cursor st))
  (define n (string-length t))
  (case (kev-kind k)
    [(eof) (values st 'eof)]
    [(char) (apply-char st k)]
    [(named) (apply-named st k)]
    [else (values st 'ignore)]
  ) ; end case
) ; end define ledit-apply

(define (apply-char st k)
  (define ch (kev-char k))
  (define c (ledit-cursor st))
  (define t (ledit-text st))
  (define n (string-length t))
  (cond
    ;; Ctrl 组合
    [(kev-ctrl? k)
     (case ch
       [(#\a) (values (struct-copy ledit st [cursor 0]) 'edit)]
       [(#\e) (values (struct-copy ledit st [cursor n]) 'edit)]
       [(#\b) (values (struct-copy ledit st [cursor (max 0 (sub1 c))]) 'edit)]
       [(#\f) (values (struct-copy ledit st [cursor (min n (add1 c))]) 'edit)]
       [(#\h) (values (if (> c 0) (delete-range st (sub1 c) c) st) 'edit)]
       [(#\d) (if (zero? n)
                  (values st 'eof)
                  (values (if (< c n) (delete-range st c (add1 c)) st) 'edit))]
       [(#\k) (values (kill-range st c n) 'edit)]
       [(#\u) (values (kill-range st 0 c) 'edit)]
       [(#\w) (values (kill-range st (word-start-before t c) c) 'edit)]
       [(#\y) (values (insert-str st (ledit-kill st)) 'edit)]
       [(#\p) (history-nav st 'prev)]
       [(#\n) (history-nav st 'next)]
       [(#\c) (values (make-ledit #:history (ledit-history st)) 'cancel)]
       [(#\l) (values st 'clear-screen)]
       [(#\g) (values st 'ignore)]
       [else (values st 'ignore)]
     ) ; end case
    ] ; end ctrl case
    ;; Alt 组合（词操作）
    [(kev-alt? k)
     (case ch
       [(#\b) (values (struct-copy ledit st [cursor (word-start-before t c)]) 'edit)]
       [(#\f) (values (struct-copy ledit st [cursor (word-end-after t c)]) 'edit)]
       [(#\d) (values (kill-range st c (word-end-after t c)) 'edit)]
       [else (values st 'ignore)]
     ) ; end case
    ] ; end alt case
    ;; 普通字符：插入
    [else (values (insert-str st (string ch)) 'edit)]
  ) ; end cond
) ; end define apply-char

(define (apply-named st k)
  (define name (kev-name k))
  (define c (ledit-cursor st))
  (define t (ledit-text st))
  (define n (string-length t))
  (define ctrl? (kev-ctrl? k))
  (case name
    [(enter) (values st 'submit)]
    [(left) (if ctrl?
                (values (struct-copy ledit st [cursor (word-start-before t c)]) 'edit)
                (values (struct-copy ledit st [cursor (max 0 (sub1 c))]) 'edit))]
    [(right) (if ctrl?
                 (values (struct-copy ledit st [cursor (word-end-after t c)]) 'edit)
                 (values (struct-copy ledit st [cursor (min n (add1 c))]) 'edit))]
    [(home) (values (struct-copy ledit st [cursor 0]) 'edit)]
    [(end) (values (struct-copy ledit st [cursor n]) 'edit)]
    [(backspace) (if (kev-alt? k)
                     (values (kill-range st (word-start-before t c) c) 'edit)
                     (values (if (> c 0) (delete-range st (sub1 c) c) st) 'edit))]
    [(delete) (values (if (< c n) (delete-range st c (add1 c)) st) 'edit)]
    [(up) (history-nav st 'prev)]
    [(down) (history-nav st 'next)]
    [(escape) (values st 'ignore)]
    [(tab) (values st 'ignore)]              ; 补全留待后续
    [else (values st 'ignore)]
  ) ; end case
) ; end define apply-named

;; ---------------------------------------------------------------- 渲染

;; 去除 ANSI CSI 序列后的显示宽度（prompt 可能含颜色码）
(define (visible-width s)
  (string-width (strip-ansi s))
) ; end define visible-width

(define (strip-ansi s)
  (regexp-replace* #rx"\e\\[[0-9;?]*[ -/]*[@-~]" s "")
) ; end define strip-ansi

;; 生成把当前行重绘到终端的 ANSI 串：回到行首、清行、写 prompt+text、定位光标。
;; 单视觉行模型（假定 prompt+text 不超过一屏宽；超宽时依赖终端自动折行，光标列可能偏移）。
(define (ledit-render st prompt)
  (define t (ledit-text st))
  (define c (ledit-cursor st))
  (define target-col (+ (visible-width prompt) (string-width-upto t c)))
  (string-append
   "\r"                                       ; 回行首
   "\e[K"                                      ; 清到行尾
   prompt
   t
   "\r"                                        ; 再回行首
   (if (> target-col 0) f"\e[{target-col}C" "")  ; 右移到光标列
  ) ; end string-append
) ; end define ledit-render

;; ---------------------------------------------------------------- provide

(provide
 (struct-out ledit)
 make-ledit
 ledit-value
 ledit-apply
 ledit-render
 visible-width
 strip-ansi
 word-start-before
 word-end-after
) ; end provide
