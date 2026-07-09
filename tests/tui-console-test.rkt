#lang tstring racket
;; tui-console-test.rkt — 异步实时控制台（底部输入行 + 上方滚动输出）离线验证。
;; 同步驱动 console-handle-key! / console-emit!，无需线程或真实 tty。

(require
 rackunit
 racket/async-channel
 (file "../src/tui/keys.rkt")
 (file "../src/tui/terminal.rkt")
 (file "../src/tui/console.rkt")
) ; end require

;; 把字节/字符串转成 kev 列表逐个喂给 console-handle-key!
(define (feed! con keys)
  (for ([k (in-list (parse-keys keys))])
    (console-handle-key! con k)
  ) ; end for
) ; end define feed!

;; last-index-of：返回子串最后一次出现的起点（无则 #f）
(define (last-index-of s sub)
  (let loop ([i 0] [found #f])
    (define m (and (<= (+ i (string-length sub)) (string-length s))
                   (let scan ([j i])
                     (cond
                       [(> (+ j (string-length sub)) (string-length s)) #f]
                       [(string=? (substring s j (+ j (string-length sub))) sub) j]
                       [else (scan (add1 j))]))))
    (if m (loop (add1 m) m) found)
  ) ; end let loop
) ; end define last-index-of

;; ---------------------------------------------------------------- 提交与回显

(test-case "submit routes the line to the submit channel and echoes it"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term #:prompt "> "))
  (feed! con "hello\r")
  (check-equal? (async-channel-try-get (console-submit-channel con)) "hello")
  (check-true (string-contains? (scripted-output st) "hello"))
) ; end test-case

(test-case "submitted lines accumulate into history (newest first)"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (feed! con "one\r")
  (feed! con "two\r")
  (check-equal? (console-history-list con) '("two" "one"))
) ; end test-case

;; ---------------------------------------------------------------- 核心：异步输出不撞输入

(test-case "output emitted while typing does not interleave into the input"
  ;; 用户键入 "abc"（未回车），此时异步输出 "line1\n" 到达。
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term #:prompt "> "))
  (feed! con "abc")                       ; 输入行现为 "abc"
  (console-emit! con "line1\n")           ; 异步输出一整行
  (define out (scripted-output st))
  ;; 完整输出行被提交为滚动历史，且 \n 规整为 \r\n（raw 模式）
  (check-true (string-contains? out "line1\r\n"))
  ;; "line1" 保持连续，未被 "abc" 撕裂
  (check-false (string-contains? out "liabc"))
  ;; 输出之后输入行被重绘，"abc" 仍在
  (define tail (substring out (+ (or (last-index-of out "line1\r\n") 0) 7)))
  (check-true (string-contains? tail "abc"))
) ; end test-case

(test-case "partial output (no newline) is shown but not committed to cache"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (console-emit! con "streaming")         ; 无换行
  (check-true (string-contains? (scripted-output st) "streaming"))  ; 已显示（框上方）
  (check-equal? (console-tail-lines con 10) '())                    ; 未提交入缓存
  (console-emit! con "!\n")               ; 补上换行 → 提交
  (check-equal? (console-tail-lines con 10) '("streaming!"))
) ; end test-case

;; ---------------------------------------------------------------- Ctrl-C 阶梯 / EOF

;; 阶梯语义（Task 3）：有草稿→清草稿；空+运行中→打断+回显；空+空闲→无动作。

(test-case "Ctrl-C with draft clears the draft, no interrupt"
  (define fired (box #f))
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term #:interrupt (lambda () (set-box! fired #t))))
  (console-set-idle! con #t)
  (feed! con (bytes-append #"abc" (bytes 3)))  ; 键入 abc 后 Ctrl-C
  (check-false (unbox fired))                  ; 不打断
  ;; 草稿已清空：随后键入 x 回车，提交的是 "x" 而非 "abcx"
  (feed! con "x\r")
  (check-equal? (async-channel-try-get (console-submit-channel con)) "x")
) ; end test-case

(test-case "Ctrl-C with draft while running clears draft, still no interrupt"
  (define fired (box #f))
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term #:interrupt (lambda () (set-box! fired #t))))
  (console-set-idle! con #f)                   ; 运行中
  (feed! con (bytes-append #"draft" (bytes 3)))
  (check-false (unbox fired))                  ; 有草稿优先清草稿，不打断
) ; end test-case

(test-case "Ctrl-C on empty while running interrupts (no ^C echo)"
  (define fired (box #f))
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term #:interrupt (lambda () (set-box! fired #t))))
  (console-set-idle! con #f)                   ; 运行中，空框
  (feed! con (bytes 3))
  (check-true (unbox fired))
  (check-false (string-contains? (scripted-output st) "^C"))  ; 禁止 ^C 回显
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

;; ---------------------------------------------------------------- 空回车 / 多行输入

(test-case "empty Enter does not dispatch (just a newline)"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (feed! con "\r")                              ; 空回车
  (check-false (async-channel-try-get (console-submit-channel con)))  ; 未派发
  (feed! con "x\r")
  (check-equal? (async-channel-try-get (console-submit-channel con)) "x")
) ; end test-case

(test-case "Shift+Enter inserts a newline; plain Enter submits multi-line"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  ;; "ab" + Shift+Enter(CSI-u) + "cd" + Enter
  (feed! con (bytes-append #"ab" #"\e[13;2u" #"cd" #"\r"))
  (check-equal? (async-channel-try-get (console-submit-channel con)) "ab\ncd")
) ; end test-case

(test-case "Alt+Enter also inserts a newline (portable fallback)"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (feed! con (bytes-append #"foo" #"\e\r" #"bar" #"\r"))
  (check-equal? (async-channel-try-get (console-submit-channel con)) "foo\nbar")
) ; end test-case

;; ---------------------------------------------------------------- 光标：帧式写入

(test-case "writes are framed with cursor hide/show (no cursor on output body)"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (console-emit! con "hello\n")
  (define out (scripted-output st))
  (check-true (string-contains? out "\e[?25l"))   ; 写时藏光标
  (check-true (string-contains? out "\e[?25h"))   ; 写毕复现（落在框内）
) ; end test-case

(test-case "Ctrl-D on empty routes eof to submit channel"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (define r (console-handle-key! con (car (parse-keys (bytes 4)))))
  (check-eq? r 'eof)
  (check-true (eof-object? (async-channel-try-get (console-submit-channel con))))
) ; end test-case

;; ---------------------------------------------------------------- 权限询问（改道）

(test-case "console-ask! reroutes the next submitted line to the asker"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  ;; reader 线程：ask 设好改道后，喂入 "y\r" 作为回答
  (define reader
    (thread (lambda ()
              (sync (system-idle-evt))            ; 让 console-ask! 先建立改道
              (feed! con "y\r"))))
  (define ans (console-ask! con "allow? [y/n/a] "))
  (check-equal? ans "y")
  ;; 回答不应泄漏到正常 submit 通道
  (check-false (async-channel-try-get (console-submit-channel con)))
) ; end test-case

;; ---------------------------------------------------------------- 输入框：分隔线

(test-case "idle input box draws a separator line"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term #:prompt "> "))
  (console-set-idle! con #t)
  (check-true (string-contains? (scripted-output st) "─"))   ; U+2500 分隔线
) ; end test-case

;; ---------------------------------------------------------------- '/' 命令预览

(test-case "typing a slash renders the command hint preview"
  (define hint
    (lambda (text)
      (if (and (> (string-length text) 0) (char=? (string-ref text 0) #\/))
          (list "  /model <id>  switch model")
          '())))
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term #:hint hint))
  (console-set-idle! con #t)
  (feed! con "/mo")
  (check-true (string-contains? (scripted-output st) "/model"))  ; 预览已渲染
) ; end test-case

(test-case "hint disappears once input no longer starts with slash"
  (define calls (box '()))
  (define hint
    (lambda (text)
      (set-box! calls (cons text (unbox calls)))
      (if (and (> (string-length text) 0) (char=? (string-ref text 0) #\/)) (list "HINT") '())))
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term #:hint hint))
  (console-set-idle! con #t)
  (feed! con "hello")                     ; 非命令
  ;; hint 被调用过但对非 '/' 文本返回空，输出不含 HINT
  (check-false (string-contains? (scripted-output st) "HINT"))
) ; end test-case

;; ---------------------------------------------------------------- 滚动缓存（超长会话提取）

(test-case "committed output lines land in the scrollback cache"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (console-emit! con "alpha\nbeta\ngamma\n")
  (check-equal? (console-tail-lines con 2) '("beta" "gamma"))
  (check-equal? (console-tail-lines con 10) '("alpha" "beta" "gamma"))
) ; end test-case

(test-case "cache is bounded (ring evicts oldest)"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term #:cache-lines 3))
  (for ([i (in-range 6)]) (console-emit! con f"line{i}\n"))
  (check-equal? (console-tail-lines con 100) '("line3" "line4" "line5"))  ; 仅保留最后 3
) ; end test-case

(test-case "cached lines are stripped of ANSI styling"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (console-emit! con "\e[31mred\e[0m\n")
  (check-equal? (console-tail-lines con 1) '("red"))
) ; end test-case

(displayln "tui-console-test: all passed")
