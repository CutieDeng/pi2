#lang tstring racket
;; tui/console.rkt — 异步实时控制台（design.md §11.3–§11.7）
;;
;; 底部固定「输入文本框」（分隔线 + [命令预览] + 提示行），异步输出滚动其上方。
;; 全程 raw 模式（-echo）杜绝 cooked 回显把按键撞进流式输出；所有终端写入经同一把锁
;; 串行化，故 reader 线程回显、main 线程流式输出、权限询问互不冲突。
;;
;; 关键能力：
;;   · 输入框始终钉在末端并显示光标；LLM 流式输出滚动其上方，输出体上不驻留光标
;;     （每帧以 \e[?25l/\e[?25h 括起写入，光标只在框内闪烁）。
;;   · 滚动：输出留在主屏，终端原生 scrollback（鼠标/触控板）可用；另配内存环形缓存
;;     支撑超长会话的局部提取（console-tail-lines / /tail）。
;;   · Ctrl-C 阶梯：有草稿→清草稿；空+运行中→打断（不回显 ^C）；空+空闲→无动作。
;;   · 空输入回车不派发，仅换行。多行输入（Shift/Alt+Enter 插 \n）正确渲染与回显。
;;   · '/' 元命令实时预览面板（内容由上层经 #:hint 提供）。
;;
;; 绘制拆成「返回字符串的纯函数」+ 单次 framed 写入，故整帧原子、无光标游走，且可离线测试。

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

(define (ring-tail r n)                       ; 取最后 n 项（最旧→最新）
  (define k (min n (ring-size r)))
  (for/list ([j (in-range (- (ring-size r) k) (ring-size r))])
    (vector-ref (ring-vec r) (modulo (+ (ring-head r) j) (ring-cap r)))
  ) ; end for/list
) ; end define ring-tail

;; ------------------------------------------------------------ console

;; 动态区（钉在屏幕底部）自顶向下：
;;   [pending 半行] [分隔线] [命令预览行…] [提示行/多行输入(光标)]
;; curup 记录「上次绘制后光标距动态区顶部的行数」，重绘前据此上移到顶再清屏
;; （相对光标移动，滚动安全；也正确处理多行输入光标不在末行的情形）。
(struct console
  (term       ; terminal
   lock       ; semaphore(1) — 串行化一切终端写
   ledit      ; box of ledit
   prompt     ; box of string
   pending    ; box of string — 尚未换行的输出尾
   curup      ; box of exact  — 上次绘制后光标距动态区顶部的行数
   route      ; box of ('input | channel) — 提交行去向
   submit     ; async-channel
   idle       ; box of boolean — 是否空闲（决定 Ctrl-C 是否打断）
   history    ; box of (listof string)
   interrupt  ; (-> void) — 空框且运行中的 Ctrl-C 回调
   cols       ; box of exact — 终端列宽（分隔线用）
   hint       ; (string -> (listof string)) — 命令预览行
   cache      ; ring — 已提交输出行缓存（去 ANSI）
  ) ; end fields
) ; end struct console

(define DEFAULT-CACHE-LINES 4000)

(define (make-console term
                      #:prompt [prompt "> "]
                      #:history [history '()]
                      #:interrupt [interrupt void]
                      #:hint [hint (lambda (_t) '())]
                      #:cache-lines [cache-lines DEFAULT-CACHE-LINES])
  (console term (make-semaphore 1)
           (box (make-ledit #:history history))
           (box prompt) (box "") (box 0) (box 'input)
           (make-async-channel) (box #f) (box history)
           interrupt (box 80) hint (make-ring cache-lines))
) ; end define make-console

(define (console-submit-channel con) (console-submit con))
(define (console-history-list con) (unbox (console-history con)))
(define (console-tail-lines con n) (ring-tail (console-cache con) n))

;; ------------------------------------------------------------ 绘制（返回字符串，须持锁）

;; 分隔线：整宽暗色横线，界定「上文输出」与「底部输入框」。
(define (separator-line con)
  (define bar (make-string (max 1 (unbox (console-cols con))) (integer->char #x2500)))
  f"\e[2m{bar}\e[0m"
) ; end define separator-line

;; 提示行之上的附加整行：分隔线 + 命令预览。
(define (box-extra-rows con)
  (cons (separator-line con)
        ((console-hint con) (ledit-text (unbox (console-ledit con)))))
) ; end define box-extra-rows

;; 擦掉当前动态区：上移到顶行、清到屏幕末。返回 ANSI 串，并把 curup 归零。
(define (clear-dynamic-str con)
  (define u (unbox (console-curup con)))
  (set-box! (console-curup con) 0)
  (string-append (if (> u 0) f"\e[{u}A" "") "\r" "\e[J")
) ; end define clear-dynamic-str

;; 在底部绘制动态区（输入框始终可见）。返回 ANSI 串，并更新 curup。
(define (draw-dynamic-str con)
  (define pend (unbox (console-pending con)))
  (define has-pend? (positive? (string-length pend)))
  (define ed (unbox (console-ledit con)))
  (define full-rows (append (if has-pend? (list pend) '()) (box-extra-rows con)))
  (set-box! (console-curup con) (+ (length full-rows) (ledit-cursor-row ed)))
  (string-append
   (apply string-append
          (for/list ([r (in-list full-rows)]) (string-append r "\r\n")))
   (ledit-render ed (unbox (console-prompt con)))
  ) ; end string-append
) ; end define draw-dynamic-str

(define (redraw-str con) (string-append (clear-dynamic-str con) (draw-dynamic-str con)))

;; 单次原子写入：以 \e[?25l/…/\e[?25h 括起，写时藏光标、写毕在框内复现，故输出体上无游标。
(define (frame! con s)
  (term-write (console-term con) (string-append "\e[?25l" s "\e[?25h"))
) ; end define frame!

;; ------------------------------------------------------------ 输出

;; console-emit! : 异步输出经此串行落屏。完整行提交为永久滚动历史（\n→\r\n）并入缓存；
;; 未换行尾入 pending 于框上方滚动显示。
(define (console-emit! con text)
  (call-with-semaphore (console-lock con)
    (lambda ()
      (define combined (string-append (unbox (console-pending con)) text))
      (define segs (regexp-split #rx"\n" combined))
      (define complete (reverse (cdr (reverse segs))))
      (define newpend (last segs))
      (define cs (clear-dynamic-str con))
      (for ([l (in-list complete)]) (ring-push! (console-cache con) (strip-ansi l)))
      (define commit
        (if (null? complete) ""
            (apply string-append
                   (for/list ([l (in-list complete)]) (string-append l "\r\n")))))
      (set-box! (console-pending con) newpend)
      (frame! con (string-append cs commit (draw-dynamic-str con)))
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

(define (console-handle-key! con k)
  (define st0 (unbox (console-ledit con)))
  (define-values (st* action) (ledit-apply st0 k))
  (set-box! (console-ledit con) st*)
  (case action
    [(submit) (handle-submit! con st*)]
    [(cancel) (handle-cancel! con st0)]
    [(eof)    (route-value! con eof) 'eof]
    [(clear-screen)
     (call-with-semaphore (console-lock con)
       (lambda ()
         (set-box! (console-curup con) 0)
         (frame! con (string-append "\e[2J\e[H" (draw-dynamic-str con)))))
     'continue
    ] ; end clear-screen
    [else                                        ; edit / ignore
     (call-with-semaphore (console-lock con) (lambda () (frame! con (redraw-str con))))
     'continue
    ] ; end else
  ) ; end case
) ; end define console-handle-key!

;; 提交行回显串：首行带 prompt，续行以等宽缩进对齐（多行输入）。
(define (echo-str con line)
  (define prompt (unbox (console-prompt con)))
  (define lines (regexp-split #rx"\n" line))
  (define indent (make-string (visible-width prompt) #\space))
  (string-append
   prompt (car lines)
   (apply string-append (for/list ([l (in-list (cdr lines))]) f"\r\n{indent}{l}"))
   "\r\n"
  ) ; end string-append
) ; end define echo-str

(define (handle-submit! con st*)
  (define line (ledit-value st*))
  (cond
    ;; 空输入回车：不派发、不响应，仅换行（框下移一行）
    [(string=? (string-trim line) "")
     (call-with-semaphore (console-lock con)
       (lambda ()
         (define cs (clear-dynamic-str con))
         (set-box! (console-ledit con) (make-ledit #:history (unbox (console-history con))))
         (frame! con (string-append cs "\r\n" (draw-dynamic-str con)))))
     'continue
    ] ; end empty
    [else
     (call-with-semaphore (console-lock con)
       (lambda ()
         (define cs (clear-dynamic-str con))
         (define echo (echo-str con line))
         (for ([l (in-list (regexp-split #rx"\n" line))])
           (ring-push! (console-cache con) (strip-ansi l)))
         (set-box! (console-history con) (cons line (unbox (console-history con))))
         (set-box! (console-ledit con) (make-ledit #:history (unbox (console-history con))))
         (frame! con (string-append cs echo (draw-dynamic-str con)))))
     (route-value! con line)
     'continue
    ] ; end else
  ) ; end cond
) ; end define handle-submit!

;; Ctrl-C 阶梯（不回显 ^C）：
;;   有草稿      → 清空输入框，不打断
;;   空 + 运行中 → 清空并打断当前 turn（中断提示由 repl 作为独立元信息给出）
;;   空 + 空闲   → 无动作
(define (handle-cancel! con st0)
  (define had-text? (positive? (string-length (ledit-text st0))))
  (define running? (not (unbox (console-idle con))))
  (cond
    [had-text?
     (call-with-semaphore (console-lock con)
       (lambda ()
         (set-box! (console-ledit con) (make-ledit #:history (unbox (console-history con))))
         (frame! con (redraw-str con))))
    ] ; end clear-draft
    [running?
     (call-with-semaphore (console-lock con)
       (lambda ()
         (set-box! (console-ledit con) (make-ledit #:history (unbox (console-history con))))
         (frame! con (redraw-str con))))
     ((console-interrupt con))
    ] ; end interrupt
    [else (void)]
  ) ; end cond
  'continue
) ; end define handle-cancel!

;; ------------------------------------------------------------ 空闲状态 / 询问

;; 记录空闲态（供 Ctrl-C 判定）并刷新输入框。
(define (console-set-idle! con v)
  (call-with-semaphore (console-lock con)
    (lambda () (set-box! (console-idle con) v) (frame! con (redraw-str con))))
) ; end define console-set-idle!

;; console-ask! : 运行中同步征询一行。临时改 prompt 并把下一提交行改道到本地通道。
(define (console-ask! con prompt-str)
  (define ch (make-channel))
  (define old-prompt (unbox (console-prompt con)))
  (call-with-semaphore (console-lock con)
    (lambda ()
      (set-box! (console-prompt con) prompt-str)
      (set-box! (console-ledit con) (make-ledit))
      (set-box! (console-route con) ch)
      (frame! con (redraw-str con))))
  (define ans (channel-get ch))
  (call-with-semaphore (console-lock con)
    (lambda ()
      (set-box! (console-prompt con) old-prompt)
      (set-box! (console-ledit con) (make-ledit #:history (unbox (console-history con))))
      (frame! con (redraw-str con))))
  ans
) ; end define console-ask!

;; ------------------------------------------------------------ 生命周期

(define (console-start! con)
  (term-raw-on! (console-term con))
  (term-write (console-term con) "\e[>4;1m")   ; modifyOtherKeys=1：让 Shift+Enter 可辨识
  (define-values (cols _rows) (term-size (console-term con)))
  (call-with-semaphore (console-lock con)
    (lambda ()
      (set-box! (console-cols con) (or cols 80))
      (set-box! (console-idle con) #t)
      (frame! con (draw-dynamic-str con))))
) ; end define console-start!

(define (console-stop! con)
  (call-with-semaphore (console-lock con)
    (lambda ()
      (term-write (console-term con) (string-append (clear-dynamic-str con) "\e[?25h"))
      (set-box! (console-idle con) #f)))
  (term-write (console-term con) "\e[>4;0m")   ; 关闭 modifyOtherKeys
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
