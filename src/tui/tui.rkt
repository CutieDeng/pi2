#lang tstring racket
;; tui/tui.rkt — TUI 组装：把终端 + 行编辑器合成一个可编辑的 read-line。
;; 交互期进 raw 模式做逐键编辑；提交/取消后恢复，故 agent 执行阶段仍是 cooked 模式，
;; Ctrl-C 依旧经 SIGINT→exn:break 中断当前轮。

(require
 (file "keys.rkt")
 (file "width.rkt")
 (file "terminal.rkt")
 (file "lineedit.rkt")
) ; end require

;; 取消哨兵：与 eof / 字符串区分
(define tui-cancelled (string->uninterned-symbol "tui-cancelled"))
(define (tui-cancelled? v) (eq? v tui-cancelled))

;; 读一行（带编辑）。返回 string | eof | tui-cancelled。
;; history: (listof string)，最新在前。
(define (tui-read-line term #:prompt [prompt "> "] #:history [history '()])
  (dynamic-wind
   (lambda () (when (term-interactive? term) (term-raw-on! term)))
   (lambda ()
     (define st0 (make-ledit #:history history))
     (term-write term (ledit-render st0 prompt))
     (let loop ([st st0])
       (define k (term-read-key term))
       (define-values (st* action) (ledit-apply st k))
       (case action
         [(submit)
          (term-write term "\r\n")
          (ledit-value st*)
         ] ; end submit
         [(cancel)
          (term-write term "^C\r\n")
          tui-cancelled
         ] ; end cancel
         [(eof)
          (when (term-interactive? term) (term-write term "\r\n"))
          eof
         ] ; end eof
         [(clear-screen)
          (term-write term "\e[2J\e[H")
          (term-write term (ledit-render st* prompt))
          (loop st*)
         ] ; end clear-screen
         [(ignore) (loop st*)]
         [else
          (term-write term (ledit-render st* prompt))
          (loop st*)
         ] ; end edit
       ) ; end case
     ) ; end let loop
   ) ; end thunk
   (lambda () (when (term-interactive? term) (term-raw-off! term)))
  ) ; end dynamic-wind
) ; end define tui-read-line

;; 便捷：脚本化跑一行编辑，返回 (values result output)。自动化测试用。
(define (tui-run-scripted keys #:prompt [prompt "> "] #:history [history '()]
                          #:cols [cols 80])
  (define-values (term st) (make-scripted-terminal keys #:cols cols))
  (define result (tui-read-line term #:prompt prompt #:history history))
  (values result (scripted-output st))
) ; end define tui-run-scripted

(provide
 tui-read-line
 tui-cancelled
 tui-cancelled?
 tui-run-scripted
 ;; 透传常用构造，便于上层单点 require
 make-real-terminal
 make-scripted-terminal
 scripted-output
 scripted-feed!
) ; end provide
