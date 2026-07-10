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
    ;; Shift/Alt+Enter 插入换行（多行输入）；普通 Enter 提交。
    [(enter) (if (or (kev-shift? k) (kev-alt? k))
                 (values (insert-str st "\n") 'edit)
                 (values st 'submit))]
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
    [(tab) (values st 'ignore)]              ; Tab 由 console 截获做 '/' 命令补全
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

;; 文本按 \n 切成的视觉行（保留空行）
(define (text-lines t) (regexp-split #rx"\n" t))

;; 输入占用的行数（多行输入）
(define (ledit-line-count st) (length (text-lines (ledit-text st))))

;; 光标所在行号（0 基）= 光标前的 \n 个数
(define (ledit-cursor-row st)
  (for/sum ([ch (in-string (substring (ledit-text st) 0 (ledit-cursor st)))]
            #:when (char=? ch #\newline)) 1)
) ; end define ledit-cursor-row

;; 光标定位到 (row,col)：给定行内偏移，算出行号与该行的字符偏移
(define (cursor-row/col lines c)
  (let loop ([ls lines] [idx 0] [rem c])
    (define L (string-length (car ls)))
    (if (or (null? (cdr ls)) (<= rem L))
        (values idx (min rem L))
        (loop (cdr ls) (add1 idx) (- rem L 1)))     ; -1 跳过 \n
  ) ; end let loop
) ; end define cursor-row/col

;; 生成把当前输入重绘到终端的 ANSI 串。多行感知：逐行输出，末尾把光标上移到
;; 光标所在行、右移到目标列。单行时退化为原「回行首/清行/定位列」。
;; （每行按显示宽度定位；单条逻辑行超屏宽的自动折行不额外记账，同既有假设。）
(define (ledit-render st prompt)
  (define t (ledit-text st))
  (define c (ledit-cursor st))
  (define lines (text-lines t))
  (define pw (visible-width prompt))
  ;; 逐行：第一行带 prompt，其后各行换行另起；每行 \e[K 清到行尾
  (define body
    (let loop ([ls lines] [first? #t] [acc '()])
      (cond
        [(null? ls) (apply string-append (reverse acc))]
        [first? (loop (cdr ls) #f (cons f"\r\e[K{prompt}{(car ls)}" acc))]
        [else   (loop (cdr ls) #f (cons f"\r\n\e[K{(car ls)}" acc))]
      ) ; end cond
    ) ; end let loop
  ) ; end define body
  (define-values (crow coff) (cursor-row/col lines c))
  (define col (+ (if (= crow 0) pw 0) (string-width-upto (list-ref lines crow) coff)))
  (define up (- (sub1 (length lines)) crow))    ; 光标行到末行的行差
  (string-append
   body
   (if (> up 0) f"\e[{up}A" "")                  ; 上移到光标行
   "\r"
   (if (> col 0) f"\e[{col}C" "")                ; 右移到光标列
  ) ; end string-append
) ; end define ledit-render

;; ---------------------------------------------------------------- provide

(provide
 (struct-out ledit)
 make-ledit
 ledit-value
 ledit-apply
 ledit-render
 ledit-line-count
 ledit-cursor-row
 visible-width
 strip-ansi
 word-start-before
 word-end-after
) ; end provide
