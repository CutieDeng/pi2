#lang racket/base
;; tests/gui/replay-test.rkt — 操作日志回放（无窗口、无内核；子进程跑拼装层主循环）：
;; fixtures/journal-regressions.rktd 收录真实崩溃的复现步骤，回放时不得再出现那些错误。

(require rackunit racket/runtime-path racket/system racket/string)

(define-runtime-path APP "../../src/gui/assembly/app.rkt")
(define-runtime-path REGRESSIONS "fixtures/journal-regressions.rktd")

(define (replay path)
  (define out (open-output-string))
  (define ok? (parameterize ([current-output-port out] [current-error-port out])
                (system* (find-system-path 'exec-file) (path->string APP) "--replay" (path->string path))))
  (values ok? (get-output-string out)))

(test-case "regressions journal: placeholder click no longer crashes; malformed kernel events are contained"
  (define-values (ok? text) (replay REGRESSIONS))
  (check-true ok? text)
  (check-false (string-contains? text "vector-ref") text)                 ; 2026-10-23：空输入框点占位提示越界
  (check-false (string-contains? text "findf") text)                      ; sessions 不是表：侧栏当空表
  (check-true (string-contains? text "errors=(\"string-append: contract violation") text)   ; 守卫接住，记进日志
  (check-true (string-contains? text "progress=(13 . 13)") text)           ; 出错后继续处理后面的事件
  (check-true (string-contains? text "sends=1/1 match=#t") text))          ; 之后的输入照常发出
