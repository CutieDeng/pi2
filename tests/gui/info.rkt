#lang info
;; 真窗口冒烟会弹窗，从 raco test 目录遍历中排除；./run-tests.sh --gui 时显式运行。
;; *-probe.rkt 是子进程探针（其中一个会故意 abort），只由 fault-test.rkt 以子进程方式调用。
(define test-omit-paths (quote ("window-smoke-test.rkt" "encoder-fault-probe.rkt" "nsexception-probe.rkt" "fakes.rkt")))
