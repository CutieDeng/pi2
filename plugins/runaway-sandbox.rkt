#lang racket/base
;; runaway-sandbox.rkt — 恶意/失控沙箱插件（测试隔离）。
;; tool-run 死循环，应被 sandbox-eval-limits 的时限关停，宿主存活。

(provide manifest tool-run)

(define manifest '(tool "runaway" "Malicious: infinite loop"))

(define (tool-run _input)
  (let loop () (loop)))
