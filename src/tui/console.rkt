#lang tstring racket
;; tui/console.rkt — 异步实时控制台（design.md §11.3–§11.6）
;;
;; 底部固定「输入文本框」（分隔线 + [命令预览] + 提示行），异步输出滚动其上方。
;; 全程 raw 模式（-echo）杜绝 cooked 回显把按键撞进流式输出；所有终端写入经同一把锁
;; 串行化，故 reader 线程回显、main 线程流式输出、权限询问互不冲突。
;;
;; 关键能力：
;;   · 输入框末端固定 + 分隔线（Task 1）
;;   · 滚动：输出留在主屏，终端原生 scrollback（鼠标/触控板）可用；另配内存环形缓存
;;     支撑超长会话的局部提取（console-tail-lines / /tail）（Task 1、2）
;;   · Ctrl-C 阶梯语义（Task 3）：有草稿→清草稿；空+运行中→打断并回显；空+空闲→无动作
;;   · '/' 元命令实时预览面板（Task 4，预览内容由上层经 #:hint 提供）
;;
;; 纯写协调 + 按键处理，线程与取消策略留在 repl，故本层可离线同步测试。

(require
 racket/async-channel
 (file "keys.rkt")
 (file "width.rkt")
 (file "terminal.rkt")
 (file "lineedit.rkt")
) ; end require

;; ------------------------------------------------------------ 环形缓存（scrollback）

;; 定长环形缓冲：O(1) 追加、自动淘汰最旧，界定超长会话的内存占用。
(struct ring (vec cap [size #:mutable] [head #:mutable]))

(define (make-ring cap) (ring (make-vector cap #f) cap 0 0))

(define (ring-push! r x)
  (define i (modulo (+ (ring-head r) (ring-size r)) (ring-cap r)))
  (vector-set! (ring-vec r) i x)
  (if (< (ring-size r) (ring-cap r))
      (set-ring-size! r (add1 (ring-size r)))
      (set-ring-head! r (modulo (add1 (ring-head r)) (ring-cap r)))
  ) ; end if
) ; end define ring-push!

;; 取最后 n 项（最旧→最新）
(define (ring-tail r n)
  (define k (min n (ring-size r)))
  (for/list ([j (in-range (- (ring-size r) k) (ring-size r))])
    (vector-ref (ring-vec r) (modulo (+ (ring-head r) j) (ring-cap r)))
  ) ; end for/list
) ; end define ring-tail

;; ------------------------------------------------------------ console

;; 动态区（钉在屏幕底部）自顶向下：
;;   [pending 半行] [分隔线] [命令预览行…] [提示行(光标)]
;; rows 记录上次绘制占用的终端行数，重绘先按此上移清屏（相对光标移动，滚动安全）。
(struct console
  (term       ; terminal
   lock       ; semaphore(1) — 串行化一切终端写
   ledit      ; box of ledit  — 当前输入行状态
   prompt     ; box of string — 输入提示符（可含 ANSI）
   pending    ; box of string — 尚未换行的输出尾（partial line）
   rows       ; box of exact  — 动态区上次绘制占用行数
   route      ; box of ('input | channel) — 提交行去向（询问期临时改道）
   submit     ; async-channel — 正常提交行 / eof 投递到此
   idle       ; box of boolean — 是否空闲等待输入
   history    ; box of (listof string) — 已提交行，最新在前
   interrupt  ; (-> void) — 空框且运行中收到 Ctrl-C 时调用（打断当前 turn）
   cols       ; box of exact  — 终端列宽（分隔线用；启动时探测）
   hint       ; (string -> (listof string)) — 由输入文本生成命令预览行
   cache      ; ring — 已提交输出行的滚动缓存（去 ANSI 的纯文本）
  ) ; end fields
) ; end struct console

(define DEFAULT-CACHE-LINES 4000)

(define (make-console term
                      #:prompt [prompt "> "]
                      #:history [history '()]
                      #:interrupt [interrupt void]
                      #:hint [hint (lambda (_t) '())]
                      #:cache-lines [cache-lines DEFAULT-CACHE-LINES])
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
           interrupt
           (box 80)
           hint
           (make-ring cache-lines))
) ; end define make-console

(define (console-submit-channel con) (console-submit con))
(define (console-history-list con) (unbox (console-history con)))

;; 取缓存里最后 n 行输出（最旧→最新）——超长会话的局部信息提取。
(define (console-tail-lines con n) (ring-tail (console-cache con) n))

;; ------------------------------------------------------------ 绘制原语（须持锁）

(define (input-visible? con)
  (or (unbox (console-idle con))
      (positive? (string-length (ledit-text (unbox (console-ledit con))))))
) ; end define input-visible?

;; 分隔线：整宽暗色横线，界定「上文输出」与「底部输入框」。
(define (separator-line con)
  (define w (max 1 (unbox (console-cols con))))
  (define bar (make-string w (integer->char #x2500)))   ; ─ 整宽横线
  f"\e[2m{bar}\e[0m"
) ; end define separator-line

;; 输入框内提示行之上的附加行（自顶向下）：分隔线 + 命令预览。
(define (box-extra-rows con)
  (cons (separator-line con)
        ((console-hint con) (ledit-text (unbox (console-ledit con)))))
) ; end define box-extra-rows

;; 擦掉当前动态区：上移到顶行、清到屏幕末，光标落在顶行行首。
(define (clear-dynamic! con)
  (define r (unbox (console-rows con)))
  (when (> r 0)
    (term-write (console-term con)
                (string-append (if (> r 1) f"\e[{(sub1 r)}A" "") "\r" "\e[J"))
  ) ; end when
  (set-box! (console-rows con) 0)
) ; end define clear-dynamic!

;; 在底部绘制动态区。光标停在最底行（提示行光标列，或 pending 尾）。
(define (draw-dynamic! con)
  (define pend (unbox (console-pending con)))
  (define has-pend? (positive? (string-length pend)))
  (define vis? (input-visible? con))
  (define above                                  ; 提示行之上的整行（不含 pending）
    (if vis? (box-extra-rows con) '()))
  (cond
    [vis?
     ;; pending(若有) + 分隔线 + 预览行 皆为整行(\r\n)，末尾是提示行(自带光标定位)
     (define full-rows (append (if has-pend? (list pend) '()) above))
     (term-write (console-term con)
                 (string-append
                  (apply string-append
                         (for/list ([r (in-list full-rows)]) (string-append r "\r\n")))
                  (ledit-render (unbox (console-ledit con)) (unbox (console-prompt con)))))
     (set-box! (console-rows con) (+ (length full-rows) 1))
    ] ; end visible
    [has-pend?
     (term-write (console-term con) pend)        ; 仅 pending，贴底，无换行
     (set-box! (console-rows con) 1)
    ] ; end pending-only
    [else (set-box! (console-rows con) 0)]
  ) ; end cond
) ; end define draw-dynamic!

(define (redraw! con) (clear-dynamic! con) (draw-dynamic! con))

;; ------------------------------------------------------------ 输出

;; 把完整输出行提交为永久滚动历史（\n→\r\n），并入缓存（去 ANSI）；未换行尾入 pending。
(define (commit-lines! con complete)
  (unless (null? complete)
    (term-write (console-term con)
                (apply string-append
                       (for/list ([l (in-list complete)]) (string-append l "\r\n"))))
    (for ([l (in-list complete)])
      (ring-push! (console-cache con) (strip-ansi l))
    ) ; end for
  ) ; end unless
) ; end define commit-lines!

;; console-emit! : 异步输出经此串行落屏。
(define (console-emit! con text)
  (call-with-semaphore (console-lock con)
    (lambda ()
      (define combined (string-append (unbox (console-pending con)) text))
      (define segs (regexp-split #rx"\n" combined))      ; ≥1 段，末段=新 pending
      (define complete (reverse (cdr (reverse segs))))
      (define newpend (last segs))
      (clear-dynamic! con)
      (commit-lines! con complete)
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

;; console-handle-key! : reader 线程逐键调用。返回 'continue | 'eof。
(define (console-handle-key! con k)
  (define st0 (unbox (console-ledit con)))
  (define-values (st* action) (ledit-apply st0 k))
  (set-box! (console-ledit con) st*)
  (case action
    [(submit)     (handle-submit! con st*)]
    [(cancel)     (handle-cancel! con st0)]
    [(eof)        (route-value! con eof) 'eof]
    [(clear-screen)
     (call-with-semaphore (console-lock con)
       (lambda ()
         (term-write (console-term con) "\e[2J\e[H")
         (set-box! (console-rows con) 0)
         (draw-dynamic! con)))
     'continue
    ] ; end clear-screen
    [else                                        ; edit / ignore
     (call-with-semaphore (console-lock con) (lambda () (redraw! con)))
     'continue
    ] ; end else
  ) ; end case
) ; end define console-handle-key!

(define (handle-submit! con st*)
  (define line (ledit-value st*))
  (call-with-semaphore (console-lock con)
    (lambda ()
      (clear-dynamic! con)
      ;; 回显已提交行为滚动历史（如同 shell 回显命令），并入缓存
      (define echo (string-append (unbox (console-prompt con)) line))
      (term-write (console-term con) (string-append echo "\r\n"))
      (ring-push! (console-cache con) (strip-ansi echo))
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
) ; end define handle-submit!

;; Ctrl-C 阶梯（Task 3）：
;;   有草稿      → 取消草稿（清空输入框），不打断
;;   空 + 运行中 → 打断当前 turn 并回显 ^C
;;   空 + 空闲   → 无动作
(define (handle-cancel! con st0)
  (define had-text? (positive? (string-length (ledit-text st0))))
  (define running? (not (unbox (console-idle con))))
  (cond
    [had-text?
     (call-with-semaphore (console-lock con)
       (lambda ()
         (set-box! (console-ledit con) (make-ledit #:history (unbox (console-history con))))
         (redraw! con)))
    ] ; end clear-draft
    [running?
     (call-with-semaphore (console-lock con)
       (lambda ()
         (clear-dynamic! con)
         (term-write (console-term con) "^C\r\n")
         (ring-push! (console-cache con) "^C")
         (set-box! (console-rows con) 0)
         (set-box! (console-ledit con) (make-ledit #:history (unbox (console-history con))))
         (draw-dynamic! con)))
     ((console-interrupt con))
    ] ; end interrupt
    [else (void)]                                ; 空 + 空闲：无动作
  ) ; end cond
  'continue
) ; end define handle-cancel!

;; ------------------------------------------------------------ 空闲状态 / 询问

(define (console-set-idle! con v)
  (call-with-semaphore (console-lock con)
    (lambda () (set-box! (console-idle con) v) (redraw! con)))
) ; end define console-set-idle!

;; console-ask! : 运行中同步征询一行（权限询问）。临时改用 prompt-str 并把下一条
;; 提交行改道到本地通道；由 reader 线程投递，主线程在此阻塞取回。
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
      (redraw! con)))
  (define ans (channel-get ch))
  (call-with-semaphore (console-lock con)
    (lambda ()
      (set-box! (console-prompt con) old-prompt)
      (set-box! (console-idle con) old-idle)
      (set-box! (console-ledit con) (make-ledit #:history (unbox (console-history con))))
      (redraw! con)))
  ans
) ; end define console-ask!

;; ------------------------------------------------------------ 生命周期

(define (console-start! con)
  (term-raw-on! (console-term con))
  (define-values (cols _rows) (term-size (console-term con)))
  (call-with-semaphore (console-lock con)
    (lambda ()
      (set-box! (console-cols con) (or cols 80))
      (set-box! (console-idle con) #t)
      (draw-dynamic! con)))
) ; end define console-start!

(define (console-stop! con)
  (call-with-semaphore (console-lock con)
    (lambda ()
      (clear-dynamic! con)                       ; 擦掉底部输入框，留干净屏幕
      (set-box! (console-idle con) #f)))
  (term-raw-off! (console-term con))
) ; end define console-stop!

;; ---------------------------------------------------------------- provide

(provide
 make-console
 console?
 console-submit-channel
 console-history-list
 console-tail-lines
 console-emit!
 console-handle-key!
 console-set-idle!
 console-ask!
 console-start!
 console-stop!
) ; end provide
