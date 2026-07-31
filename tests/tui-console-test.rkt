#lang tstring racket
;; tui-console-test.rkt — 全屏异步控制台离线验证（脚本终端 80×24，无线程/无真 tty）。
;; 同步驱动 console-handle-key! / console-emit! / console-set-status!，检查整帧渲染与行为。

(require
 rackunit
 racket/async-channel
 (file "../src/tui/keys.rkt")
 (file "../src/tui/terminal.rkt")
 (file "../src/tui/console.rkt")
) ; end require

(define (feed! con keys)
  (for ([k (in-list (parse-keys keys))]) (console-handle-key! con k)))

;; 子串最后一次出现的起点（无则 #f）
(define (last-index-of s sub)
  (let loop ([i 0] [found #f])
    (define m (and (<= (+ i (string-length sub)) (string-length s))
                   (let scan ([j i])
                     (cond
                       [(> (+ j (string-length sub)) (string-length s)) #f]
                       [(string=? (substring s j (+ j (string-length sub))) sub) j]
                       [else (scan (add1 j))]))))
    (if m (loop (add1 m) m) found)))

;; 最新一帧（每帧以 \e[H 起始）
(define (last-frame st)
  (define o (scripted-output st))
  (define i (last-index-of o "\e[H"))
  (if i (substring o i) o))

;; ---------------------------------------------------------------- 提交 / 历史

(test-case "submit routes the line and echoes it into the content"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term #:prompt "> "))
  (feed! con "hello\r")
  (check-equal? (async-channel-try-get (console-submit-channel con)) "hello")
  (check-true (string-contains? (last-frame st) "hello"))
) ; end test-case

(test-case "submitted lines accumulate into history (newest first)"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (feed! con "one\r") (feed! con "two\r")
  (check-equal? (console-history-list con) '("two" "one"))
) ; end test-case

;; ---------------------------------------------------------------- 输出 / 缓存

(test-case "emitted output appears in the frame and commits to cache"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (console-emit! con "alpha\nbeta\ngamma\n")
  (check-true (string-contains? (last-frame st) "gamma"))
  (check-equal? (console-tail-lines con 2) '("beta" "gamma"))
  (check-equal? (console-tail-lines con 10) '("alpha" "beta" "gamma"))
) ; end test-case

(test-case "partial output (no newline) shown but not committed to cache"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (console-emit! con "streaming")
  (check-true (string-contains? (last-frame st) "streaming"))
  (check-equal? (console-tail-lines con 10) '())
  (console-emit! con "!\n")
  (check-equal? (console-tail-lines con 10) '("streaming!"))
) ; end test-case

(test-case "typing while output streams does not corrupt either"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term #:prompt "> "))
  (feed! con "abc")
  (console-emit! con "line1\n")
  (define f (last-frame st))
  (check-true (string-contains? f "line1"))
  (check-true (string-contains? f "abc"))         ; 输入框保留
  (check-false (string-contains? f "liabc"))      ; 不撕裂
) ; end test-case

(test-case "cache is bounded (ring evicts oldest)"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term #:cache-lines 3))
  (for ([i (in-range 6)]) (console-emit! con f"line{i}\n"))
  (check-equal? (console-tail-lines con 100) '("line3" "line4" "line5"))
) ; end test-case

(test-case "cached lines are stripped of ANSI styling"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (console-emit! con "\e[31mred\e[0m\n")
  (check-equal? (console-tail-lines con 1) '("red"))
) ; end test-case

;; ---------------------------------------------------------------- Ctrl-C 阶梯

(test-case "Ctrl-C with draft clears the draft, no interrupt"
  (define fired (box #f))
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term #:interrupt (lambda () (set-box! fired #t))))
  (console-set-idle! con #t)
  (feed! con (bytes-append #"abc" (bytes 3)))
  (check-false (unbox fired))
  (feed! con "x\r")
  (check-equal? (async-channel-try-get (console-submit-channel con)) "x")
) ; end test-case

(test-case "Ctrl-C with draft while running clears draft, still no interrupt"
  (define fired (box #f))
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term #:interrupt (lambda () (set-box! fired #t))))
  (console-set-idle! con #f)
  (feed! con (bytes-append #"draft" (bytes 3)))
  (check-false (unbox fired))
) ; end test-case

(test-case "Ctrl-C on empty while running interrupts (no ^C echo)"
  (define fired (box #f))
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term #:interrupt (lambda () (set-box! fired #t))))
  (console-set-idle! con #f)
  (feed! con (bytes 3))
  (check-true (unbox fired))
  (check-false (string-contains? (scripted-output st) "^C"))
) ; end test-case

(test-case "Ctrl-C on empty while idle is a no-op"
  (define fired (box #f))
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term #:interrupt (lambda () (set-box! fired #t))))
  (console-set-idle! con #t)
  (feed! con (bytes 3))
  (check-false (unbox fired))
  (check-false (string-contains? (scripted-output st) "^C"))
) ; end test-case

;; ---------------------------------------------------------------- 空回车 / 多行

(test-case "empty Enter does not dispatch (just a newline)"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (feed! con "\r")
  (check-false (async-channel-try-get (console-submit-channel con)))
  (feed! con "x\r")
  (check-equal? (async-channel-try-get (console-submit-channel con)) "x")
) ; end test-case

(test-case "Shift+Enter inserts newline; plain Enter submits multi-line"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (feed! con (bytes-append #"ab" #"\e[13;2u" #"cd" #"\r"))
  (check-equal? (async-channel-try-get (console-submit-channel con)) "ab\ncd")
) ; end test-case

(test-case "Alt+Enter also inserts a newline (portable fallback)"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (feed! con (bytes-append #"foo" #"\e\r" #"bar" #"\r"))
  (check-equal? (async-channel-try-get (console-submit-channel con)) "foo\nbar")
) ; end test-case

;; ---------------------------------------------------------------- 输入框 / 光标

(test-case "frame is written with cursor hide/show; separator present"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (console-emit! con "hello\n")
  (define out (scripted-output st))
  (check-true (string-contains? out "\e[?25l"))
  (check-true (string-contains? out "\e[?25h"))
  (check-true (string-contains? (last-frame st) "─"))    ; 分隔线
) ; end test-case

(test-case "frames are bracketed by DEC synchronized update (2026)"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (console-emit! con "hello\n")
  (define out (scripted-output st))
  ;; 每帧：\e[?2026h 开帧在 \e[H 之前，\e[?2026l 收帧在最末（光标复现之后）
  (check-true (string-contains? out "\e[?2026h\e[?25l"))
  (check-true (string-contains? out "\e[?25h\e[?2026l"))
  (check-equal? (substring out (- (string-length out) 8)) "\e[?2026l")
) ; end test-case

(test-case "rows are written content-first then \\e[K; full-width rows skip the clear"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (console-emit! con "hello\n")
  (define f (last-frame st))
  (check-true (string-contains? f "hello\e[K"))    ; 先写内容再清行尾（不闪空白）
  (check-false (string-contains? f "\e[Khello"))   ; 不再「先擦后写」
  ;; 分隔线恰满 80 列：写满末列后处于延迟折行态，\e[K 会擦掉末列字符 → 必须跳过
  (check-false (string-contains? f "\e[0m\e[K"))   ; 唯一以 \e[0m 结尾的满宽行无 \e[K
) ; end test-case

(test-case "Tab applies the completion callback to the input line"
  (define comp (lambda (t) (if (string=? t "/mo") "/model " #f)))
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term #:complete comp))
  (feed! con "/mo")
  (feed! con #"\t")                        ; Tab
  (feed! con "\r")                         ; submit
  (check-equal? (async-channel-try-get (console-submit-channel con)) "/model ")
) ; end test-case

(test-case "Tab is a no-op when completion returns #f"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term #:complete (lambda (_t) #f)))
  (feed! con "abc")
  (feed! con #"\t")
  (feed! con "\r")
  (check-equal? (async-channel-try-get (console-submit-channel con)) "abc")
) ; end test-case

(test-case "typing a slash renders the command hint preview"
  (define hint (lambda (t) (if (and (> (string-length t) 0) (char=? (string-ref t 0) #\/))
                               (list "  /model <id>  switch model") '())))
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term #:hint hint))
  (feed! con "/mo")
  (check-true (string-contains? (last-frame st) "/model"))
) ; end test-case

;; ---------------------------------------------------------------- 自有滚动视口

(test-case "PageUp reveals older content; scrolled-up view stays anchored; submit jumps to bottom"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (for ([i (in-range 60)]) (console-emit! con f"mark{i}.\n"))
  (check-true (string-contains? (last-frame st) "mark59."))   ; 最新可见
  (check-false (string-contains? (last-frame st) "mark0."))   ; 最旧不可见
  ;; PageUp 上滚多次 → 露出最旧
  (feed! con (bytes-append #"\e[5~" #"\e[5~" #"\e[5~"))
  (check-true (string-contains? (last-frame st) "mark0."))
  ;; 上滚阅读时，新输出不打扰（视口锚定，不被拽走）
  (console-emit! con "fresh\n")
  (check-true (string-contains? (last-frame st) "mark0."))
  (check-false (string-contains? (last-frame st) "fresh"))
  ;; 提交新输入 → 跳回底部跟随
  (feed! con "go\r")
  (check-false (string-contains? (last-frame st) "mark0."))
) ; end test-case

(test-case "scrolled-up view is rock-solid while a long line streams (pending wrap boundaries)"
  ;; 回归：锚定补偿曾只计已提交行——未完行每跨一次折行边界（80 列）视口上爬一行，
  ;; 行提交时又回跳。修正后按总视觉行数增量补偿，锚定期间整帧应逐字节不变。
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (for ([i (in-range 60)]) (console-emit! con f"mark{i}.\n"))
  (feed! con (bytes-append #"\e[5~" #"\e[5~" #"\e[5~"))         ; 上滚到最旧
  (check-true (string-contains? (last-frame st) "mark0."))
  (define anchored (last-frame st))
  ;; 流式追加 3×60 字符（无换行）：pending 依次折成 1/2/3 视觉行
  (for ([i (in-range 3)])
    (console-emit! con (make-string 60 (integer->char (+ (char->integer #\a) i))))
    (check-equal? (last-frame st) anchored f"chunk {i}: 视口被流式 pending 拽动"))
  ;; 该行提交（\n）：已提交行数 +3、pending 行数 -3，增量为 0 → 仍不动
  (console-emit! con "\n")
  (check-equal? (last-frame st) anchored "行提交时视口回跳")
  ;; 滚回底部后能看到新内容（数据没丢，只是锚定期间不显示）
  (feed! con "\r")                                              ; 空回车 → 跳回底部
  (check-true (string-contains? (last-frame st) "aaa"))
) ; end test-case

(test-case "at bottom (follow mode) streaming pending stays visible, no anchoring"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (console-emit! con "before\n")
  (console-emit! con (make-string 100 #\z))       ; 跨折行的未完行
  (define f (last-frame st))
  (check-true (string-contains? f "before"))
  (check-true (string-contains? f "zzz"))          ; 跟随底部：pending 可见
) ; end test-case

(test-case "mouse wheel scrolls the viewport"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (for ([i (in-range 60)]) (console-emit! con f"mark{i}.\n"))
  (for ([_ (in-range 12)]) (feed! con #"\e[<64;1;1M"))         ; 滚轮上滚 12×3 行
  (check-true (string-contains? (last-frame st) "mark20."))
  (check-false (string-contains? (last-frame st) "mark59."))
) ; end test-case

;; ---------------------------------------------------------------- 括号粘贴

(test-case "keys: bracketed paste parses into a single kpaste event"
  (define ks (parse-keys "x\e[200~line1\nline2\e[201~y"))
  (check-equal? (length ks) 3)
  (check-equal? (kev-char (car ks)) #\x)
  (check-true (kpaste? (cadr ks)))
  (check-equal? (kpaste-text (cadr ks)) "line1\nline2")
  (check-equal? (kev-char (caddr ks)) #\y)
) ; end test-case

(test-case "multi-line paste collapses to a placeholder, never dispatches; submit expands"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (feed! con "\e[200~line1\nline2\nline3\e[201~")
  (check-false (async-channel-try-get (console-submit-channel con)))  ; 粘贴不触发提交
  (check-true (string-contains? (last-frame st) "[paste #1 +3 lines]"))
  (feed! con "\r")
  (check-equal? (async-channel-try-get (console-submit-channel con)) "line1\nline2\nline3")
  ;; 回显与历史保留折叠形态
  (check-true (string-contains? (last-frame st) "[paste #1 +3 lines]"))
  (check-equal? (console-history-list con) '("[paste #1 +3 lines]"))
) ; end test-case

(test-case "short single-line paste inserts verbatim (no placeholder)"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (feed! con "\e[200~hello world\e[201~")
  (check-false (string-contains? (last-frame st) "[paste"))
  (feed! con "\r")
  (check-equal? (async-channel-try-get (console-submit-channel con)) "hello world")
) ; end test-case

(test-case "pasted \\r line endings normalize to \\n (terminals paste Enter as CR)"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (feed! con "\e[200~a\rb\e[201~")
  (check-false (async-channel-try-get (console-submit-channel con)))  ; \r 不再当 Enter
  (feed! con "\r")
  (check-equal? (async-channel-try-get (console-submit-channel con)) "a\nb")
) ; end test-case

(test-case "placeholder composes with typed text around it"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (feed! con "before ")
  (feed! con "\e[200~x\ny\e[201~")
  (feed! con " after\r")
  (check-equal? (async-channel-try-get (console-submit-channel con)) "before x\ny after")
) ; end test-case

(test-case "bracketed paste mode enabled on start, disabled on stop"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (console-start! con)
  (console-stop! con)
  (check-true (string-contains? (scripted-output st) "\e[?2004h"))
  (check-true (string-contains? (scripted-output st) "\e[?2004l"))
) ; end test-case

;; ---------------------------------------------------------------- 滚动边缘去抖

(test-case "scrolling past either edge is a no-op (no redraw storm from inertial wheel)"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (for ([i (in-range 60)]) (console-emit! con f"mark{i}.\n"))
  ;; 底部（view=0）继续下滚：不产生任何写出
  (define len0 (string-length (scripted-output st)))
  (for ([_ (in-range 5)]) (feed! con #"\e[<65;1;1M"))          ; 滚轮下滚 ×5
  (feed! con #"\e[6~")                                         ; PageDown
  (check-equal? (string-length (scripted-output st)) len0)
  ;; 顶部同理：滚到顶后继续上滚不再写出
  (for ([_ (in-range 40)]) (feed! con #"\e[5~"))               ; PageUp 到顶（夹紧）
  (define len1 (string-length (scripted-output st)))
  (for ([_ (in-range 5)]) (feed! con #"\e[<64;1;1M"))          ; 滚轮上滚 ×5
  (check-equal? (string-length (scripted-output st)) len1)
  ;; 离开边缘后滚动恢复正常
  (feed! con #"\e[<65;1;1M")                                   ; 下滚一步
  (check-true (> (string-length (scripted-output st)) len1))
) ; end test-case

;; ---------------------------------------------------------------- 工作动画

(test-case "status shows a spinner + label; output clears it"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (console-set-status! con "thinking…")
  (define f1 (last-frame st))
  (check-true (string-contains? f1 "thinking…"))
  (check-true (or (string-contains? f1 "⠋") (string-contains? f1 "⠙")))  ; 转轮帧
  ;; 输出到达自动清除动画
  (console-emit! con "answer\n")
  (check-false (string-contains? (last-frame st) "thinking…"))
) ; end test-case

;; ---------------------------------------------------------------- 贴底内联小选框

(test-case "console-choose! returns the selected index"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (define reader
    (thread (lambda () (sync (system-idle-evt))
                       (feed! con (bytes-append #"\e[B" #"\r")))))  ; ↓ 到第 2 项, Enter
  (check-equal? (console-choose! con "Approve?" '("Yes" "No")) 1)
) ; end test-case

(test-case "inline choose renders options AND keeps conversation visible above"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (console-emit! con "hello world\n")
  (define reader
    (thread (lambda () (sync (system-idle-evt)) (feed! con #"\e"))))  ; Esc 取消
  (check-false (console-choose! con "Approve tool `bash`?" '("Yes" "No — deny")))
  (define out (scripted-output st))
  (check-true (string-contains? out "Approve tool"))    ; 标题
  (check-true (string-contains? out "No — deny"))        ; 选项
  (check-true (string-contains? out "hello world"))      ; 对话内容仍在（未被全屏遮挡）
) ; end test-case

(test-case "inline choose highlights the current option (reverse video)"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (define reader (thread (lambda () (sync (system-idle-evt)) (feed! con #"\e"))))
  (console-choose! con "pick" '("aaa" "bbb"))
  ;; 选中项以反显 \e[7m 渲染，且带 › 标记
  (check-true (string-contains? (scripted-output st) "\e[7m› aaa"))
) ; end test-case

;; ---------------------------------------------------------------- 工作进度动画

(test-case "working: sweep style animates the separator (basic tty, bold heavy line)"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term #:progress-style 'sweep))
  (console-set-working! con #t)
  (define f (last-frame st))
  (check-true (string-contains? f "━"))          ; 扫光粗线
  (check-true (string-contains? f "\e[1m"))       ; bold
  (check-false (string-contains? f "█"))          ; sweep 不用块字符/颜色
) ; end test-case

(test-case "working: bar style is a thin 256-color line (not solid blocks)"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term #:progress-style 'bar))
  (console-set-working! con #t)
  (define f (last-frame st))
  (check-true (string-contains? f "\e[38;5;"))    ; 256 色
  (check-true (string-contains? f "━"))           ; 细线
  (check-false (string-contains? f "█"))          ; 已不再用实心块
) ; end test-case

(test-case "separator reverts to a static dim line when work ends"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term #:progress-style 'sweep))
  (console-set-working! con #t)
  (console-set-working! con #f)
  (define f (last-frame st))
  (check-false (string-contains? f "━"))          ; 不再扫光
  (check-true (string-contains? f "─"))           ; 静态暗线
) ; end test-case

;; ---------------------------------------------------------------- 速率档 / OSC 进度条

(test-case "token/s maps to 4 animation-speed tiers"
  (check-equal? (rate->step 0) 1)
  (check-equal? (rate->step 5) 1)
  (check-equal? (rate->step 20) 2)
  (check-equal? (rate->step 45) 3)
  (check-equal? (rate->step 120) 4)
) ; end test-case

(test-case "default progress style is bar (thin 256-color line)"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))            ; 默认样式（未设 PI_PROGRESS=sweep）
  (console-set-working! con #t)
  (define f (last-frame st))
  (check-true (string-contains? f "\e[38;5;"))    ; 默认即 bar（256 色）
  (check-false (string-contains? f "█"))          ; 但为细线，非实心块
) ; end test-case

(test-case "rate readout quantizes to multiples of 5 (jitter suppression)"
  (check-equal? (quantize-rate 2) 0)      ; <2.5 → 隐藏
  (check-equal? (quantize-rate 3) 5)
  (check-equal? (quantize-rate 22) 20)
  (check-equal? (quantize-rate 23) 25)
  (check-equal? (quantize-rate 48) 50)
) ; end test-case

(test-case "OSC 9;4 progress emitted around working when enabled"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term #:osc? #t))
  (console-set-working! con #t)
  (check-true (string-contains? (scripted-output st) "\e]9;4;3"))   ; indeterminate
  (console-set-working! con #f)
  (check-true (string-contains? (scripted-output st) "\e]9;4;0"))   ; clear
) ; end test-case

(test-case "OSC 9;4 suppressed when disabled"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term #:osc? #f))
  (console-set-working! con #t)
  (console-set-working! con #f)
  (check-false (string-contains? (scripted-output st) "\e]9;4"))
) ; end test-case

;; ---------------------------------------------------------------- 插件快捷键

(test-case "a matching plugin shortcut runs its thunk and consumes the key"
  (define fired (box #f))
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term
                #:shortcut (lambda (k)
                             (and (equal? k (kchar #\g '(ctrl)))
                                  (lambda () (set-box! fired #t))))))
  (feed! con (bytes 7))                          ; Ctrl-G → kchar #\g '(ctrl)
  (check-true (unbox fired))
  ;; 非命中键不被拦截，仍进编辑器
  (feed! con "x\r")
  (check-equal? (async-channel-try-get (console-submit-channel con)) "x")
) ; end test-case

(displayln "tui-console-test: all passed")
