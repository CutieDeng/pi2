#lang racket/base
;; calc-sandbox.rkt — 沙箱（sandbox）示例插件。
;; 经 make-module-evaluator 在受限求值器内载入：声明式导出 manifest（数据）+ tool-run（过程）。
;; 宿主每次执行都经求值器跨界调用，受内存/时间限制。此插件是纯计算，无副作用。

(provide manifest tool-run)

;; (tool <name> <description>)
(define manifest '(tool "calc" "Evaluate a simple integer op: a op b"))

;; input：宿主以 quote 传入的 hasheq，如 #hasheq((a . 2) (op . "+") (b . 3))
(define (tool-run input)
  (define a (hash-ref input 'a 0))
  (define b (hash-ref input 'b 0))
  (case (hash-ref input 'op "+")
    [("+") (+ a b)]
    [("-") (- a b)]
    [("*") (* a b)]
    [else "unknown op"]))
