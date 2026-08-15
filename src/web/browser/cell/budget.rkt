#lang racket/base
;; browser/cell/budget.rkt — L2 · 页预算（design-chrome.md L2 / §2.4）
;; 职责: 预算数据(prefab,可持久化) + 缺省值(2s CPU / 128MB / 20 req,承 design-browser §4/§5)
;; 不做: 计量与强制(cell.rkt / L9)
;; 依赖: racket/base

(require racket/contract)

(provide
 (struct-out budget)
 (contract-out
  [make-budget (->* ()
                    (#:cpu-ms exact-positive-integer?
                     #:mem-bytes exact-positive-integer?
                     #:reqs exact-positive-integer?
                     #:bytes exact-positive-integer?
                     #:vtime-ms exact-positive-integer?)
                    budget?)]
  [DEFAULT-BUDGET budget?]
 ) ; end contract-out
) ; end provide

(struct budget (cpu-ms mem-bytes reqs bytes vtime-ms) #:prefab)

(define (make-budget #:cpu-ms [cpu-ms 2000]
                     #:mem-bytes [mem-bytes (* 128 1024 1024)]
                     #:reqs [reqs 20]
                     #:bytes [bytes (* 16 1024 1024)]
                     #:vtime-ms [vtime-ms 10000])
  (budget cpu-ms mem-bytes reqs bytes vtime-ms)
) ; end define make-budget

(define DEFAULT-BUDGET (make-budget))
