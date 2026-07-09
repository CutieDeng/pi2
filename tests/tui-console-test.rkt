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

(test-case "partial output (no newline) is held and shown, not committed"
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term))
  (console-emit! con "streaming")         ; 无换行
  (define out (scripted-output st))
  (check-true (string-contains? out "streaming"))
  (check-false (string-contains? out "streaming\r\n"))  ; 尚未提交
) ; end test-case

;; ---------------------------------------------------------------- Ctrl-C / EOF

(test-case "Ctrl-C during a running turn triggers interrupt"
  (define fired (box #f))
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term #:interrupt (lambda () (set-box! fired #t))))
  (console-set-idle! con #f)              ; 模拟运行中（非空闲）
  (feed! con (bytes 3))                   ; Ctrl-C
  (check-true (unbox fired))
) ; end test-case

(test-case "Ctrl-C while idle clears the line, no interrupt"
  (define fired (box #f))
  (define-values (term st) (make-scripted-terminal ""))
  (define con (make-console term #:interrupt (lambda () (set-box! fired #t))))
  (console-set-idle! con #t)
  (feed! con (bytes-append #"abc" (bytes 3)))
  (check-false (unbox fired))
  (check-true (string-contains? (scripted-output st) "^C"))
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

(displayln "tui-console-test: all passed")
