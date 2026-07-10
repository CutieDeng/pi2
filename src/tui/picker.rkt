#lang tstring racket
;; tui/picker.rkt — 全屏可选列表控件（design.md §11.9）
;; 纯状态机 (pick-step) + 共享渲染 (pick-render-lines) 分离，故可离线测试；
;; run-picker 提供独立(raw+alt)运行入口，console 亦复用同一渲染做「/resume」内嵌选择。

(require
 (file "keys.rkt")
 (file "width.rkt")
 (file "terminal.rkt")
) ; end require

;; 选择器状态：高亮项下标 idx（0 基）与项数 n
(struct pick-state (idx n) #:prefab)
(define (pick-init n) (pick-state 0 n))

;; pick-step : state kev -> (values state* result)
;; result: 'continue | 'cancel | (list 'chosen idx)
(define (pick-step st k)
  (define n (pick-state-n st))
  (define i (pick-state-idx st))
  (define (go j) (values (pick-state (max 0 (min (sub1 n) j)) n) 'continue))
  (cond
    [(zero? n) (values st 'cancel)]
    [(eq? (kev-kind k) 'eof) (values st 'cancel)]
    [(eq? (kev-kind k) 'named)
     (case (kev-name k)
       [(up) (go (sub1 i))]
       [(down) (go (add1 i))]
       [(home) (go 0)]
       [(end) (go (sub1 n))]
       [(pgup) (go (- i 10))]
       [(pgdn) (go (+ i 10))]
       [(enter) (values st (list 'chosen i))]
       [(escape) (values st 'cancel)]
       [else (values st 'continue)]
     ) ; end case
    ] ; end named
    [(eq? (kev-kind k) 'char)
     (define ch (kev-char k))
     (cond
       [(kev-ctrl? k)
        (case ch
          [(#\p) (go (sub1 i))] [(#\n) (go (add1 i))]
          [(#\c) (values st 'cancel)]
          [else (values st 'continue)])]
       [(char=? ch #\q) (values st 'cancel)]
       [(char=? ch #\k) (go (sub1 i))]        ; vim 上
       [(char=? ch #\j) (go (add1 i))]        ; vim 下
       [else (values st 'continue)]
     ) ; end cond
    ] ; end char
    [else (values st 'continue)]
  ) ; end cond
) ; end define pick-step

;; 把字符串裁剪到 w 显示列（CJK 双宽计入）
(define (clip s w)
  (let loop ([i 0] [cw 0])
    (cond
      [(>= i (string-length s)) s]
      [(> (+ cw (char-width (string-ref s i))) w) (substring s 0 i)]
      [else (loop (add1 i) (+ cw (char-width (string-ref s i))))]
    ) ; end cond
  ) ; end let loop
) ; end define clip

;; 计算可见窗口起点，使 idx 始终可见
(define (window-start idx n rows)
  (define maxstart (max 0 (- n rows)))
  (cond
    [(< idx 0) 0]
    [(< idx rows) 0]                            ; 前 rows 项：从头
    [(>= idx (- n 1)) maxstart]
    [else (min maxstart (- idx (quotient rows 2)))]
  ) ; end cond
) ; end define window-start

;; pick-render-lines : state items title render-item W H -> (listof string)（恰 H 行）
;; render-item : item -> string（单行；此处按 W 裁剪）。当前项反显 + 「› 」标记。
(define (pick-render-lines st items title render-item W H)
  (define n (pick-state-n st))
  (define idx (pick-state-idx st))
  (define rows (max 1 (- H 2)))                 ; 首行标题、末行提示
  (define start (window-start idx n rows))
  (define end (min n (+ start rows)))
  (define item-lines
    (for/list ([i (in-range start end)])
      (define sel? (= i idx))
      (define text (clip (render-item (list-ref items i)) (- W 2)))
      (if sel?
          f"\e[7m› {text}\e[0m"
          f"  {text}")
    ) ; end for/list
  ) ; end define item-lines
  ;; 补足到 rows 行
  (define padded (append item-lines (make-list (max 0 (- rows (length item-lines))) "")))
  (append
   (list f"\e[1m{(clip title W)}\e[0m")
   padded
   (list (dim f"↑/↓ 选择 · Enter 确认 · Esc 取消  ({(add1 idx)}/{n})")))
) ; end define pick-render-lines

(define (dim s) f"\e[2m{s}\e[0m")

;; ------------------------------------------------------------ 独立运行

;; run-picker : term items -> idx | #f（取消返回 #f）。独立进 raw+alt，藏光标。
(define (run-picker term items #:title [title "select"] #:render-item [render-item (lambda (x) (format "~a" x))])
  (cond
    [(null? items) #f]
    [else
     (term-raw-on! term)
     (term-write term "\e[?1049h\e[?25l")
     (dynamic-wind
      void
      (lambda ()
        (let loop ([st (pick-init (length items))])
          (define-values (W H) (term-size term))
          (define lines (pick-render-lines st items title render-item (max 20 W) (max 4 H)))
          (term-write term (string-append "\e[H" (string-join lines "\r\n") "\e[J"))
          (define-values (st* r) (pick-step st (term-read-key term)))
          (cond
            [(eq? r 'continue) (loop st*)]
            [(eq? r 'cancel) #f]
            [(and (pair? r) (eq? (car r) 'chosen)) (cadr r)]
            [else #f]
          ) ; end cond
        ) ; end let loop
      ) ; end body
      (lambda () (term-write term "\e[?25h\e[?1049l") (term-raw-off! term))
     ) ; end dynamic-wind
    ] ; end else
  ) ; end cond
) ; end define run-picker

;; ---------------------------------------------------------------- provide

(provide
 (struct-out pick-state)
 pick-init
 pick-step
 pick-render-lines
 run-picker
) ; end provide
