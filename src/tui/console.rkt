#lang tstring racket
;; tui/console.rkt — 全屏异步实时控制台（design.md §11.3–§11.8）
;;
;; 接管终端窗口：进 alternate screen、禁用默认滚屏、以「自有滚动缓存 + 视口」实现滚动，
;; 每帧整屏重绘。屏幕自上而下 = [内容区(输出视口)] [可选状态行] [分隔线] [命令预览] [输入框]。
;;
;; 关键能力：
;;   · 覆写终端原生滚屏：alt screen（无原生 scrollback），滚动由我们控制——
;;     鼠标滚轮（SGR）/ PageUp·PageDown 翻动自有视口；新输出到达时自动跟随到底部。
;;   · 输入框恒钉底部并显示光标；LLM 流式输出滚动于内容区，输出体上无光标游走
;;     （每帧 \e[?25l…\e[?25h 括起，末尾绝对定位光标到框内）。
;;   · LLM 工作动画：等待模型输出（尤其首 token 前）时，状态行显示 braille 转轮 + 标签，
;;     由 animator 线程每 120ms 推进，明示「正在工作」。
;;   · 全程 raw；单锁串行化一切写；Ctrl-C 阶梯（不回显 ^C）；空回车不派发仅换行；
;;     Shift/Alt+Enter 多行；'/' 命令实时预览；权限询问改道；输出消毒在渲染层。
;;
;; 渲染为「纯函数拼字符串 + 单次 framed 写入」，故整帧原子、可离线（脚本终端 80×24）测试。

(require
 racket/async-channel
 racket/list
 (file "keys.rkt")
 (file "width.rkt")
 (file "terminal.rkt")
 (file "lineedit.rkt")
 (file "picker.rkt")
) ; end require

;; ------------------------------------------------------------ 环形缓存（scrollback）

(struct ring (vec cap [size #:mutable] [head #:mutable]))
(define (make-ring cap) (ring (make-vector cap #f) cap 0 0))
(define (ring-push! r x)
  (define i (modulo (+ (ring-head r) (ring-size r)) (ring-cap r)))
  (vector-set! (ring-vec r) i x)
  (if (< (ring-size r) (ring-cap r))
      (set-ring-size! r (add1 (ring-size r)))
      (set-ring-head! r (modulo (add1 (ring-head r)) (ring-cap r)))))
(define (ring-tail r n)                        ; 最后 n 项（最旧→最新）
  (define k (min n (ring-size r)))
  (for/list ([j (in-range (- (ring-size r) k) (ring-size r))])
    (vector-ref (ring-vec r) (modulo (+ (ring-head r) j) (ring-cap r)))))

;; ------------------------------------------------------------ ANSI 感知折行

;; 从 i 处（ESC）消费一个转义序列，返回 (values 序列串 下一索引)。主要 CSI；其余取两字符。
(define (read-escape s i)
  (define n (string-length s))
  (cond
    [(and (< (add1 i) n) (char=? (string-ref s (add1 i)) #\[))
     (let loop ([j (+ i 2)])
       (cond
         [(>= j n) (values (substring s i j) j)]
         [(let ([c (char->integer (string-ref s j))]) (and (>= c #x40) (<= c #x7E)))
          (values (substring s i (add1 j)) (add1 j))]
         [else (loop (add1 j))]))]
    [else (values (substring s i (min n (+ i 2))) (min n (+ i 2)))]
  ) ; end cond
) ; end define read-escape

;; 把一条逻辑行按显示宽度 w 折成若干视觉行（转义序列零宽、不被拆断）。
(define (wrap-visual s w0)
  (define w (max 1 w0))                         ; 防退化尺寸导致死循环
  (cond
    [(= (string-length s) 0) (list "")]
    [else
     (let loop ([i 0] [cur (open-output-string)] [curw 0] [rows '()])
       (cond
         [(>= i (string-length s)) (reverse (cons (get-output-string cur) rows))]
         [(char=? (string-ref s i) #\u1b)
          (define-values (seq j) (read-escape s i))
          (write-string seq cur)
          (loop j cur curw rows)]
         [else
          (define ch (string-ref s i))
          (define cw (char-width ch))
          (cond
            [(and (> curw 0) (> (+ curw cw) w))       ; 满宽：断行
             (loop i (open-output-string) 0 (cons (get-output-string cur) rows))]
            [else (write-char ch cur) (loop (add1 i) cur (+ curw cw) rows)])
         ] ; end else
       ) ; end cond
     ) ; end let loop
    ] ; end else
  ) ; end cond
) ; end define wrap-visual

(define (wrap-count s w) (length (wrap-visual s w)))

;; ------------------------------------------------------------ console

(define SPINNER #("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"))
(define SCROLL-STEP 3)
(define DEFAULT-CACHE-LINES 4000)

(struct console
  (term       ; terminal
   lock       ; semaphore(1)
   ledit      ; box of ledit
   prompt     ; box of string
   pending    ; box of string — 未换行的输出尾
   cache      ; ring — 已提交输出行（带样式）
   vrows      ; box of exact — 已提交行的视觉行数合计（视口 clamp 用）
   view       ; box of exact — 视口上滚行数（0=跟随底部）
   cols       ; box of exact — 终端列
   termrows   ; box of exact — 终端行
   route      ; box of ('input | channel)
   submit     ; async-channel
   idle       ; box of boolean
   history    ; box of (listof string)
   interrupt  ; (-> void)
   hint       ; (string -> (listof string))
   status     ; box of (or/c #f string) — 工作动画标签
   spin       ; box of exact — 转轮帧索引
   animator   ; box of (or/c #f thread)
   picker     ; box of (or/c #f pmode) — /resume 内嵌选择器模式
   complete   ; (string -> (or/c #f string)) — Tab 补全（返回补全后的整行或 #f）
  ) ; end fields
) ; end struct console

;; 选择器模式态（内嵌于 console，键由 reader 线程喂给 pick-step）
(struct pmode (state items render-item title ch) #:mutable)

(define (make-console term
                      #:prompt [prompt "> "]
                      #:history [history '()]
                      #:interrupt [interrupt void]
                      #:hint [hint (lambda (_t) '())]
                      #:complete [complete (lambda (_t) #f)]
                      #:cache-lines [cache-lines DEFAULT-CACHE-LINES])
  (console term (make-semaphore 1)
           (box (make-ledit #:history history))
           (box prompt) (box "") (make-ring cache-lines) (box 0) (box 0)
           (box 80) (box 24) (box 'input) (make-async-channel)
           (box #f) (box history) interrupt hint
           (box #f) (box 0) (box #f) (box #f) complete)
) ; end define make-console

;; 尺寸夹紧：拒绝退化/零尺寸（真实终端偶发上报 0，或查询失败）。
(define (clamp-dim v default lo) (max lo (if (and v (> v 0)) v default)))

(define (console-submit-channel con) (console-submit con))
(define (console-history-list con) (unbox (console-history con)))
(define (console-tail-lines con n) (map strip-ansi (ring-tail (console-cache con) n)))

;; ------------------------------------------------------------ 布局度量

(define (box-hints con) ((console-hint con) (ledit-text (unbox (console-ledit con)))))

(define (box-row-count con hints)
  (+ (if (unbox (console-status con)) 1 0)    ; 状态行
     1                                        ; 分隔线
     (length hints)                           ; 命令预览
     (ledit-line-count (unbox (console-ledit con))))  ; 输入行（多行）
) ; end define box-row-count

(define (content-height con hints)
  (max 1 (- (unbox (console-termrows con)) (box-row-count con hints)))
) ; end define content-height

(define (pending-vrows con)
  (define p (unbox (console-pending con)))
  (if (positive? (string-length p)) (wrap-count p (unbox (console-cols con))) 0)
) ; end define pending-vrows

(define (total-vrows con) (+ (unbox (console-vrows con)) (pending-vrows con)))

;; ------------------------------------------------------------ 绘制原语

(define (separator-line con)
  (define bar (make-string (max 1 (unbox (console-cols con))) (integer->char #x2500)))
  f"\e[2m{bar}\e[0m"
) ; end define separator-line

;; 尾部若干逻辑行（含 pending 作为最后一行）
(define (tail-logical con k)
  (define pend (unbox (console-pending con)))
  (define has? (positive? (string-length pend)))
  (define committed (ring-tail (console-cache con) (if has? (max 0 (sub1 k)) k)))
  (if has? (append committed (list pend)) committed)
) ; end define tail-logical

;; 补/截到恰好 n 行（不足在顶部补空行，多则保留末 n 行）
(define (fit-rows rows n)
  (define m (length rows))
  (cond [(= m n) rows]
        [(< m n) (append (make-list (- n m) "") rows)]
        [else (list-tail rows (- m n))]))

;; 输入框各视觉行（首行带 prompt；续行不缩进，与光标列计算一致）
(define (input-display-lines con)
  (define ed (unbox (console-ledit con)))
  (define prompt (unbox (console-prompt con)))
  (define lines (regexp-split #rx"\n" (ledit-text ed)))
  (cons (string-append prompt (car lines)) (cdr lines))
) ; end define input-display-lines

;; 光标在输入框内的 (行,列)：行 0 基、列 1 基（含 prompt 宽度）
(define (input-cursor-rc con)
  (define ed (unbox (console-ledit con)))
  (define prompt (unbox (console-prompt con)))
  (define t (ledit-text ed))
  (define c (ledit-cursor ed))
  (define lines (regexp-split #rx"\n" t))
  (let loop ([ls lines] [row 0] [rem c])
    (define L (string-length (car ls)))
    (if (or (null? (cdr ls)) (<= rem L))
        (values row (add1 (+ (if (= row 0) (visible-width prompt) 0)
                             (string-width-upto (car ls) (min rem L)))))
        (loop (cdr ls) (add1 row) (- rem L 1))))
) ; end define input-cursor-rc

;; 整屏帧字符串（末尾绝对定位光标到输入框）
(define (render-frame con)
  (define W (unbox (console-cols con)))
  (define H (unbox (console-termrows con)))
  (define hints (box-hints con))
  (define status (unbox (console-status con)))
  (define Ch (content-height con hints))
  ;; 内容视口
  (define vis
    (append-map (lambda (l) (wrap-visual l W))
                (tail-logical con (+ Ch (unbox (console-view con)) 2))))
  (define m (length vis))
  (define N (total-vrows con))
  (define maxv (max 0 (- N Ch)))
  (define v (max 0 (min maxv (unbox (console-view con)))))
  (set-box! (console-view con) v)
  (define endi (- m v))
  (define starti (max 0 (- endi Ch)))
  (define window (take (drop vis starti) (- endi starti)))
  (define content (fit-rows window Ch))
  ;; 底部框
  (define box-lines
    (append
     (if status (list (dim-status con status)) '())
     (list (separator-line con))
     hints
     (input-display-lines con)))
  (define all-rows (append content box-lines))
  (define body
    (string-append
     "\e[H"
     (string-join (for/list ([r (in-list all-rows)]) (string-append "\e[K" r)) "\r\n")
     "\e[J"))
  ;; 光标绝对位置
  (define-values (crow ccol) (input-cursor-rc con))
  (define input-row0 (+ Ch (if status 1 0) 1 (length hints) 1))  ; 输入首行(1 基)
  (string-append body f"\e[{(+ input-row0 crow)};{ccol}H")
) ; end define render-frame

(define (dim s) f"\e[2m{s}\e[0m")
(define (dim-status con label)
  (dim (string-append (vector-ref SPINNER (unbox (console-spin con))) " " label))
) ; end define dim-status

;; 单次原子写入：藏光标→写整帧→（帧末已定位）复现光标。
(define (frame! con s)
  (term-write (console-term con) (string-append "\e[?25l" s "\e[?25h"))
) ; end define frame!

;; 选择器模式：整屏画列表、藏光标（无输入框光标）；否则常规整帧。
(define (redraw! con)
  (define pk (unbox (console-picker con)))
  (cond
    [pk
     (define lines (pick-render-lines (pmode-state pk) (pmode-items pk) (pmode-title pk)
                                      (pmode-render-item pk)
                                      (max 20 (unbox (console-cols con)))
                                      (max 4 (unbox (console-termrows con)))))
     (term-write (console-term con)
                 (string-append "\e[?25l\e[H" (string-join lines "\r\n") "\e[J"))
    ] ; end picker
    [else (frame! con (render-frame con))]
  ) ; end cond
) ; end define redraw!

;; ------------------------------------------------------------ 输出

(define (console-emit! con text)
  (call-with-semaphore (console-lock con)
    (lambda ()
      (when (unbox (console-status con)) (set-box! (console-status con) #f))  ; 有输出→停动画
      (define W (unbox (console-cols con)))
      (define combined (string-append (unbox (console-pending con)) text))
      (define segs (regexp-split #rx"\n" combined))
      (define complete (reverse (cdr (reverse segs))))
      (define newpend (last segs))
      (define added
        (for/sum ([l (in-list complete)])
          (ring-push! (console-cache con) l)
          (wrap-count l W)))
      (set-box! (console-vrows con) (+ (unbox (console-vrows con)) added))
      (set-box! (console-pending con) newpend)
      ;; 上滚状态下保持视口锚定（新行不把用户拽走）
      (when (> (unbox (console-view con)) 0)
        (set-box! (console-view con) (+ (unbox (console-view con)) added)))
      (redraw! con)
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

(define (scroll-key? k)
  (and (eq? (kev-kind k) 'named)
       (memq (kev-name k) '(scroll-up scroll-down pgup pgdn))))

(define (tab-key? k)
  (and (eq? (kev-kind k) 'named) (eq? (kev-name k) 'tab) (not (kev-shift? k))))

(define (console-handle-key! con k)
  (cond
    [(unbox (console-picker con)) (handle-picker-key! con k) 'continue]
    [(scroll-key? k) (do-scroll! con k) 'continue]
    [(tab-key? k) (do-complete! con) 'continue]
    [else
     (define st0 (unbox (console-ledit con)))
     (define-values (st* action) (ledit-apply st0 k))
     (set-box! (console-ledit con) st*)
     (case action
       [(submit) (handle-submit! con st*)]
       [(cancel) (handle-cancel! con st0)]
       [(eof)    (route-value! con eof) 'eof]
       [(clear-screen) (call-with-semaphore (console-lock con) (lambda () (redraw! con))) 'continue]
       [else (call-with-semaphore (console-lock con) (lambda () (redraw! con))) 'continue]
     ) ; end case
    ] ; end else
  ) ; end cond
) ; end define console-handle-key!

;; 选择器模式下的按键：喂给 pick-step；选定/取消则清模式并把结果投递到通道。
(define (handle-picker-key! con k)
  (define pk (unbox (console-picker con)))
  (define-values (st* r) (pick-step (pmode-state pk) k))
  (cond
    [(eq? r 'continue)
     (call-with-semaphore (console-lock con)
       (lambda () (set-pmode-state! pk st*) (redraw! con)))
    ] ; end continue
    [else
     (define result (if (and (pair? r) (eq? (car r) 'chosen)) (cadr r) #f))
     (call-with-semaphore (console-lock con) (lambda () (set-box! (console-picker con) #f)))
     (channel-put (pmode-ch pk) result)        ; console-pick! 取回后会重绘常规界面
    ] ; end done
  ) ; end cond
) ; end define handle-picker-key!

;; Tab 补全：把当前输入交给 complete 回调，返回新行则替换（光标落末尾）。
(define (do-complete! con)
  (define cur (ledit-text (unbox (console-ledit con))))
  (define done ((console-complete con) cur))
  (when (and (string? done) (not (string=? done cur)))
    (call-with-semaphore (console-lock con)
      (lambda ()
        (set-box! (console-ledit con)
                  (make-ledit #:history (unbox (console-history con)) #:text done))
        (redraw! con)))
  ) ; end when
) ; end define do-complete!

(define (do-scroll! con k)
  (call-with-semaphore (console-lock con)
    (lambda ()
      (define Ch (content-height con (box-hints con)))
      (define step (case (kev-name k)
                     [(pgup pgdn) (max 1 (sub1 Ch))]
                     [else SCROLL-STEP]))
      (define dir (if (memq (kev-name k) '(scroll-up pgup)) 1 -1))
      (define maxv (max 0 (- (total-vrows con) Ch)))
      (set-box! (console-view con)
                (max 0 (min maxv (+ (unbox (console-view con)) (* dir step)))))
      (redraw! con)
    ) ; end lambda
  ) ; end call-with-semaphore
) ; end define do-scroll!

;; 提交行推入内容缓存（回显），并更新视觉行计数
(define (push-echo! con line)
  (define prompt (unbox (console-prompt con)))
  (define lines (regexp-split #rx"\n" line))
  (define echo-rows (cons (string-append prompt (car lines)) (cdr lines)))
  (define W (unbox (console-cols con)))
  (for ([r (in-list echo-rows)])
    (ring-push! (console-cache con) r)
    (set-box! (console-vrows con) (+ (unbox (console-vrows con)) (wrap-count r W))))
) ; end define push-echo!

(define (handle-submit! con st*)
  (define line (ledit-value st*))
  (cond
    [(string=? (string-trim line) "")            ; 空回车：不派发，仅推一空行
     (call-with-semaphore (console-lock con)
       (lambda ()
         (ring-push! (console-cache con) "")
         (set-box! (console-vrows con) (add1 (unbox (console-vrows con))))
         (set-box! (console-view con) 0)
         (set-box! (console-ledit con) (make-ledit #:history (unbox (console-history con))))
         (redraw! con)))
     'continue
    ] ; end empty
    [else
     (call-with-semaphore (console-lock con)
       (lambda ()
         (push-echo! con line)
         (set-box! (console-history con) (cons line (unbox (console-history con))))
         (set-box! (console-view con) 0)
         (set-box! (console-ledit con) (make-ledit #:history (unbox (console-history con))))
         (redraw! con)))
     (route-value! con line)
     'continue
    ] ; end else
  ) ; end cond
) ; end define handle-submit!

;; Ctrl-C 阶梯（不回显 ^C）：有草稿→清草稿；空+运行中→打断；空+空闲→无动作。
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
         (set-box! (console-ledit con) (make-ledit #:history (unbox (console-history con))))
         (redraw! con)))
     ((console-interrupt con))
    ] ; end interrupt
    [else (void)]
  ) ; end cond
  'continue
) ; end define handle-cancel!

;; ------------------------------------------------------------ 空闲 / 状态 / 询问

(define (console-set-idle! con v) (set-box! (console-idle con) v))

;; 工作动画：v = 标签串（显示转轮）或 #f（清除）。仅在变化时重绘。
(define (console-set-status! con v)
  (call-with-semaphore (console-lock con)
    (lambda ()
      (unless (equal? (unbox (console-status con)) v)
        (set-box! (console-status con) v)
        (redraw! con))))
) ; end define console-set-status!

(define (console-ask! con prompt-str)
  (define ch (make-channel))
  (define old-prompt (unbox (console-prompt con)))
  (call-with-semaphore (console-lock con)
    (lambda ()
      (set-box! (console-prompt con) prompt-str)
      (set-box! (console-ledit con) (make-ledit))
      (set-box! (console-route con) ch)
      (redraw! con)))
  (define ans (channel-get ch))
  (call-with-semaphore (console-lock con)
    (lambda ()
      (set-box! (console-prompt con) old-prompt)
      (set-box! (console-ledit con) (make-ledit #:history (unbox (console-history con))))
      (redraw! con)))
  ans
) ; end define console-ask!

;; console-pick! : 运行中同步弹出选择器（/resume）。键由 reader 线程喂给 pick-step，
;; 主线程在此阻塞取回选中下标（取消 → #f）。
(define (console-pick! con items #:title [title "select"]
                       #:render-item [render-item (lambda (x) (format "~a" x))])
  (cond
    [(null? items) #f]
    [else
     (define ch (make-channel))
     (call-with-semaphore (console-lock con)
       (lambda ()
         (set-box! (console-picker con) (pmode (pick-init (length items)) items render-item title ch))
         (redraw! con)))
     (define res (channel-get ch))
     (call-with-semaphore (console-lock con)
       (lambda () (set-box! (console-picker con) #f) (redraw! con)))
     res]
  ) ; end cond
) ; end define console-pick!

;; ------------------------------------------------------------ animator（工作动画线程）

;; 每 ~1s 重查终端尺寸；变化则更新并重绘（处理窗口 resize，无 SIGWINCH 依赖）。
(define (poll-resize! con)
  (define-values (c r) (term-size (console-term con)))
  (define nc (clamp-dim c 80 20))
  (define nr (clamp-dim r 24 4))
  (when (or (not (= nc (unbox (console-cols con)))) (not (= nr (unbox (console-termrows con)))))
    (call-with-semaphore (console-lock con)
      (lambda ()
        (set-box! (console-cols con) nc)
        (set-box! (console-termrows con) nr)
        (redraw! con))))
) ; end define poll-resize!

(define (start-animator! con)
  (set-box! (console-animator con)
    (thread
     (lambda ()
       (let loop ([tick 0])
         (sleep 0.12)
         (when (unbox (console-status con))
           (call-with-semaphore (console-lock con)
             (lambda ()
               (set-box! (console-spin con)
                         (modulo (add1 (unbox (console-spin con))) (vector-length SPINNER)))
               (redraw! con))))
         (when (>= tick 8) (poll-resize! con))     ; ~每秒查一次尺寸
         (loop (if (>= tick 8) 0 (add1 tick))))))
  ) ; end set-box!
) ; end define start-animator!

;; ------------------------------------------------------------ 生命周期

(define (console-start! con)
  (term-raw-on! (console-term con))
  ;; 进 alt screen（禁原生 scrollback）+ SGR 鼠标上报 + modifyOtherKeys
  (term-write (console-term con) "\e[?1049h\e[?1000h\e[?1006h\e[>4;1m")
  (define-values (c r) (term-size (console-term con)))
  (set-box! (console-cols con) (clamp-dim c 80 20))
  (set-box! (console-termrows con) (clamp-dim r 24 4))
  (start-animator! con)
  (call-with-semaphore (console-lock con) (lambda () (set-box! (console-idle con) #t) (redraw! con)))
) ; end define console-start!

(define (console-stop! con)
  (when (unbox (console-animator con)) (kill-thread (unbox (console-animator con))))
  (call-with-semaphore (console-lock con)
    (lambda ()
      (term-write (console-term con)
                  "\e[?1000l\e[?1006l\e[>4;0m\e[?25h\e[?1049l")))  ; 复原鼠标/键/光标/主屏
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
 console-set-status!
 console-ask!
 console-pick!
 console-start!
 console-stop!
) ; end provide
