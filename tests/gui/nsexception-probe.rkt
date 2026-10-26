#lang racket/base
;; tests/gui/nsexception-probe.rkt — 子进程探针（由 fault-test 调用）：触发 NSRangeException。
;; 预期：Racket with-handlers 接不到；reporter 打印 name/reason；进程 abort（exit 134）。
(require ffi/unsafe "../../src/gui/compute/mac-objc.rkt")
(module+ main
  (install-uncaught-exception-reporter!
   (lambda (name reason) (eprintf "REPORTED ~a: ~a\n" name reason)))
  (define-msg m-object-at "objectAtIndex:" _id ([i _uint64]))
  (define r (with-handlers ([(lambda (_e) #t) (lambda (_e) 'racket-caught)])
              (m-object-at (alloc-init "NSArray") 5)))
  (printf "UNREACHABLE ~a\n" r))
