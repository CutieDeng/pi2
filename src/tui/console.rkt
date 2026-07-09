#lang tstring racket
;; tui/console.rkt — 异步实时控制台（design.md §11.5 TUI 实时层）
;;
;; 解决的问题：提交 prompt 后 agent 异步流式输出的同时，用户若继续键入，
;; cooked 模式的默认回显会把按键撞进输出流与状态信息里，造成串行冲突。
;;
;; 方案（标准实现，参照 prompt_toolkit patch_stdout / readline redisplay）：
;;   1. 全程 raw 模式（-echo）：终端不再自动回显，杜绝按键撞入输出。
;;   2. 底部固定一条「输入行」，异步输出一律滚动到其上方。
;;   3. 所有终端写入（reader 线程的回显、main 线程的流式输出、权限询问）
;;      经同一把锁串行化；每次输出前擦掉输入行、提交完整输出行为滚动历史、
;;      再在底部重绘输入行——用户已键入内容始终正确回显，不与异步消息冲突。
;;   4. 输入为空且正在跑 turn 时隐藏输入行，输出自然贴底（无多余空提示符）；
;;      一旦用户开始键入，输入行即刻钉在底部。
;;
;; 纯写协调 + 按键处理，线程与取消策略留给上层（repl），故本层可离线同步测试。

(require
 racket/async-channel
 (file "keys.rkt")
 (file "width.rkt")
 (file "terminal.rkt")
 (file "lineedit.rkt")
) ; end require

;; 动态区 = [可选的 pending 半行] + [可选的输入行]，钉在屏幕底部。
;; rows 记录上次绘制占用的终端行数，重绘时先按此上移并清屏。
(struct console
  (term       ; terminal
   lock       ; semaphore(1) — 串行化一切终端写
   ledit      ; box of ledit  — 当前输入行状态
   prompt     ; box of string — 输入提示符（可含 ANSI 颜色）
   pending    ; box of string — 尚未换行的输出尾（partial line）
   rows       ; box of exact  — 动态区上次绘制占用的行数
   route      ; box of ('input | channel) — 提交行的去向（询问期临时改道）
   submit     ; async-channel — 正常提交行 / eof 投递到此
   idle       ; box of boolean — 是否空闲等待输入（决定是否始终显示输入行）
   history    ; box of (listof string) — 已提交行，最新在前
   interrupt  ; (-> void) — 运行中收到 Ctrl-C 时调用（取消当前 turn）
  ) ; end fields
) ; end struct console

(define (make-console term
                      #:prompt [prompt "> "]
                      #:history [history '()]
                      #:interrupt [interrupt void])
  (console term
           (make-semaphore 1)
           (box (make-ledit #:history history))
           (box prompt)
           (box "")
           (box 0)
           (box 'input)
           (make-async-channel)
           (box #f)
           (box history)
           interrupt)
) ; end define make-console

(define (console-submit-channel con) (console-submit con))
(define (console-history-list con) (unbox (console-history con)))

;; ------------------------------------------------------------ 绘制原语（须持锁）

;; 输入行是否可见：空闲等待时始终显示；否则仅在已键入内容时钉底。
(define (input-visible? con)
  (or (unbox (console-idle con))
      (positive? (string-length (ledit-text (unbox (console-ledit con))))))
) ; end define input-visible?

;; 擦掉当前动态区：上移到其顶行、清到屏幕末，光标落在顶行行首。
(define (clear-dynamic! con)
  (define r (unbox (console-rows con)))
  (when (> r 0)
    (term-write (console-term con)
                (string-append (if (> r 1) f"\e[{(sub1 r)}A" "") "\r" "\e[J"))
  ) ; end when
  (set-box! (console-rows con) 0)
) ; end define clear-dynamic!

;; 在底部绘制动态区：pending 半行（若有）占一行，其下为输入行（若可见）。
;; 光标停在最底动态行（输入行的光标列，或 pending 尾）。
(define (draw-dynamic! con)
  (define pend (unbox (console-pending con)))
  (define has-pend? (positive? (string-length pend)))
  (define vis? (input-visible? con))
  (define body
    (string-append
     (cond
       [(and has-pend? vis?) (string-append pend "\r\n")]   ; pending 独占一行，输入行在其下
       [has-pend? pend]                                     ; 仅 pending，贴底
       [else ""])
     (if vis? (ledit-render (unbox (console-ledit con)) (unbox (console-prompt con))) "")
    ) ; end string-append
  ) ; end define body
  (term-write (console-term con) body)
  (set-box! (console-rows con) (+ (if has-pend? 1 0) (if vis? 1 0)))
) ; end define draw-dynamic!

(define (redraw! con) (clear-dynamic! con) (draw-dynamic! con))

;; ------------------------------------------------------------ 输出

;; console-emit! : 异步输出经此串行落屏。完整行提交为滚动历史（永久），
;; 未换行的尾部暂存 pending，在底部动态区滚动显示；\n 统一规整为 \r\n（raw 模式）。
(define (console-emit! con text)
  (call-with-semaphore (console-lock con)
    (lambda ()
      (define combined (string-append (unbox (console-pending con)) text))
      (define segs (regexp-split #rx"\n" combined))      ; ≥1 段，末段=新 pending
      (define complete (reverse (cdr (reverse segs))))   ; 除末段外均为完整行
      (define newpend (last segs))
      (clear-dynamic! con)
      (unless (null? complete)
        (term-write (console-term con)
                    (apply string-append
                           (for/list ([l (in-list complete)]) (string-append l "\r\n"))))
      ) ; end unless
      (set-box! (console-pending con) newpend)
      (draw-dynamic! con)
    ) ; end lambda
  ) ; end call-with-semaphore
) ; end define console-emit!

;; ------------------------------------------------------------ 提交行改道

(define (route-value! con v)
  (define r (unbox (console-route con)))
  (cond
    [(channel? r) (set-box! (console-route con) 'input) (channel-put r v)]
    [else (async-channel-put (console-submit con) v)]
  ) ; end cond
) ; end define route-value!

;; ------------------------------------------------------------ 按键处理

;; console-handle-key! : reader 线程逐键调用。更新输入行并即时回显，
;; 提交/EOF 投递到 submit（或询问改道通道）。返回 'continue | 'eof。
(define (console-handle-key! con k)
  (define st0 (unbox (console-ledit con)))
  (define-values (st* action) (ledit-apply st0 k))
  (set-box! (console-ledit con) st*)
  (case action
    [(submit)
     (define line (ledit-value st*))
     (call-with-semaphore (console-lock con)
       (lambda ()
         (clear-dynamic! con)
         ;; 把已提交行回显为滚动历史（如同 shell 回显命令）
         (term-write (console-term con)
                     (string-append (unbox (console-prompt con)) line "\r\n"))
         (set-box! (console-rows con) 0)
         (unless (string=? (string-trim line) "")
           (set-box! (console-history con) (cons line (unbox (console-history con))))
         ) ; end unless
         (set-box! (console-ledit con) (make-ledit #:history (unbox (console-history con))))
         (draw-dynamic! con)
       ) ; end lambda
     ) ; end call-with-semaphore
     (route-value! con line)
     'continue
    ] ; end submit
    [(cancel)
     (call-with-semaphore (console-lock con)
       (lambda ()
         (clear-dynamic! con)
         (term-write (console-term con)
                     (string-append (unbox (console-prompt con)) (ledit-value st0) "^C\r\n"))
         (set-box! (console-rows con) 0)
         (set-box! (console-ledit con) (make-ledit #:history (unbox (console-history con))))
         (draw-dynamic! con)
       ) ; end lambda
     ) ; end call-with-semaphore
     (unless (unbox (console-idle con)) ((console-interrupt con)))  ; 运行中 → 取消 turn
     'continue
    ] ; end cancel
    [(eof)
     (route-value! con eof)
     'eof
    ] ; end eof
    [(clear-screen)
     (call-with-semaphore (console-lock con)
       (lambda ()
         (term-write (console-term con) "\e[2J\e[H")
         (set-box! (console-rows con) 0)
         (draw-dynamic! con)
       ) ; end lambda
     ) ; end call-with-semaphore
     'continue
    ] ; end clear-screen
    [else                                        ; edit / ignore
     (call-with-semaphore (console-lock con) (lambda () (redraw! con)))
     'continue
    ] ; end else
  ) ; end case
) ; end define console-handle-key!

;; ------------------------------------------------------------ 空闲状态 / 询问

;; 主循环等待输入时置 idle，使输入行始终可见（正常 REPL 提示符）。
(define (console-set-idle! con v)
  (call-with-semaphore (console-lock con)
    (lambda () (set-box! (console-idle con) v) (redraw! con)))
) ; end define console-set-idle!

;; console-ask! : 运行中同步征询一行（权限询问）。临时改用 prompt-str 并把
;; 下一条提交行改道到本地通道；由 reader 线程投递，主线程在此阻塞取回。
(define (console-ask! con prompt-str)
  (define ch (make-channel))
  (define old-prompt (unbox (console-prompt con)))
  (define old-idle (unbox (console-idle con)))
  (call-with-semaphore (console-lock con)
    (lambda ()
      (set-box! (console-prompt con) prompt-str)
      (set-box! (console-ledit con) (make-ledit))
      (set-box! (console-idle con) #t)
      (set-box! (console-route con) ch)
      (redraw! con)
    ) ; end lambda
  ) ; end call-with-semaphore
  (define ans (channel-get ch))
  (call-with-semaphore (console-lock con)
    (lambda ()
      (set-box! (console-prompt con) old-prompt)
      (set-box! (console-idle con) old-idle)
      (set-box! (console-ledit con) (make-ledit #:history (unbox (console-history con))))
      (redraw! con)
    ) ; end lambda
  ) ; end call-with-semaphore
  ans
) ; end define console-ask!

;; ------------------------------------------------------------ 生命周期

(define (console-start! con)
  (term-raw-on! (console-term con))
  (call-with-semaphore (console-lock con)
    (lambda () (set-box! (console-idle con) #t) (draw-dynamic! con)))
) ; end define console-start!

(define (console-stop! con)
  (call-with-semaphore (console-lock con)
    (lambda ()
      (clear-dynamic! con)                       ; 擦掉底部输入行，留干净屏幕
      (set-box! (console-idle con) #f)))
  (term-raw-off! (console-term con))
) ; end define console-stop!

;; ---------------------------------------------------------------- provide

(provide
 make-console
 console?
 console-submit-channel
 console-history-list
 console-emit!
 console-handle-key!
 console-set-idle!
 console-ask!
 console-start!
 console-stop!
) ; end provide
