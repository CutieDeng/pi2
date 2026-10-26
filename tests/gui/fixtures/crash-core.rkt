#lang racket/base
;; 启动即抛异常的内核（kernel-test 用）
(module+ main (eprintf "about to fail\n") (error (quote boom) "boom-at-start"))
