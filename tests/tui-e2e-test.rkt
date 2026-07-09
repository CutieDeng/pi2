#lang tstring racket
;; tui-e2e-test.rkt — TUI 端到端（脚本化终端，CLI 式输入抽象的自动化验证）
;; 完全离线：脚本化输入 + 捕获输出，无需真实 tty。

(require
 rackunit
 (file "../src/tui/keys.rkt")
 (file "../src/tui/terminal.rkt")
 (file "../src/tui/tui.rkt")
) ; end require

;; ---------------------------------------------------------------- 基本读行

(test-case "scripted: type then enter returns the line"
  (define-values (result _out) (tui-run-scripted "hello\r"))
  (check-equal? result "hello")
) ; end test-case

(test-case "scripted: editing before submit"
  ;; 键入 "helo", 左移1, 插入 "l" -> "hello"（修正拼写）
  (define-values (result _out)
    (tui-run-scripted (bytes-append #"helo" #"\e[D" #"l" #"\r"))
  ) ; end define-values
  (check-equal? result "hello")
) ; end test-case

(test-case "scripted: Ctrl-C cancels"
  (define-values (result out) (tui-run-scripted (bytes-append #"abc" (bytes 3))))
  (check-true (tui-cancelled? result))
  (check-true (string-contains? out "^C"))
) ; end test-case

(test-case "scripted: EOF when input exhausted without enter"
  (define-values (result _out) (tui-run-scripted "abc"))
  (check-true (eof-object? result))
) ; end test-case

(test-case "scripted: Ctrl-D on empty is eof"
  (define-values (result _out) (tui-run-scripted (bytes 4)))
  (check-true (eof-object? result))
) ; end test-case

;; ---------------------------------------------------------------- 快捷键链

(test-case "scripted: full readline editing session"
  ;; "foo bar" -> Ctrl-A (行首) -> Ctrl-K (清空) -> 键入 "done" -> 回车
  (define-values (result _out)
    (tui-run-scripted (bytes-append #"foo bar" (bytes 1) (bytes 11) #"done" #"\r"))
  ) ; end define-values
  (check-equal? result "done")
) ; end test-case

(test-case "scripted: unicode round-trip"
  (define-values (result _out) (tui-run-scripted "你好，世界\r"))
  (check-equal? result "你好，世界")
) ; end test-case

;; ---------------------------------------------------------------- 历史

(test-case "scripted: history recall via up-arrow"
  ;; 历史里最新是 "prev-cmd"，按上箭头调出并直接回车
  (define-values (result _out)
    (tui-run-scripted "\e[A\r" #:history '("prev-cmd" "older"))
  ) ; end define-values
  (check-equal? result "prev-cmd")
) ; end test-case

(test-case "scripted: up twice then edit"
  ;; 上两次到 "older"，末尾追加 "!"，回车
  (define-values (result _out)
    (tui-run-scripted (bytes-append #"\e[A" #"\e[A" #"!" #"\r")
                      #:history '("newer" "older"))
  ) ; end define-values
  (check-equal? result "older!")
) ; end test-case

;; ---------------------------------------------------------------- 输出渲染

(test-case "scripted: output contains prompt and cursor positioning"
  (define-values (_result out) (tui-run-scripted "hi\r" #:prompt "λ "))
  (check-true (string-contains? out "λ "))       ; prompt 被渲染
  (check-true (string-contains? out "\e[K"))     ; 清行控制
) ; end test-case

;; ---------------------------------------------------------------- 增量喂入

(test-case "scripted-feed!: simulate paced input"
  (define-values (term st) (make-scripted-terminal "ab"))
  ;; 先只有 "ab"，再补 "c\r"
  (scripted-feed! st "c\r")
  (define result (tui-read-line term))
  (check-equal? result "abc")
) ; end test-case

(displayln "tui-e2e-test: all passed")
