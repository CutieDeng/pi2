#lang racket/base
;; tests/gui/fault-test.rkt — 原生侧故障的边界（design-gui.md P8），都在子进程里跑
;;  1. encoder 建好后抛 Racket 异常：守卫先 endEncoding → 进程存活、设备仍可用（Metal 校验层开启）
;;  2. NSException：Racket 接不到，只能在 abort 前由 reporter 记录原因

(require rackunit racket/runtime-path racket/system)

(define-runtime-path ENCODER-PROBE "encoder-fault-probe.rkt")
(define-runtime-path NSEXC-PROBE "nsexception-probe.rkt")

(define (run probe #:env [env '()])
  (define out (open-output-string))
  (define code
    (parameterize ([current-output-port out] [current-error-port out]
                   [current-environment-variables (environment-variables-copy (current-environment-variables))])
      (for ([kv (in-list env)]) (putenv (car kv) (cdr kv)))
      (system*/exit-code (find-system-path 'exec-file) (path->string probe))))
  (values code (get-output-string out)))

(test-case "Racket exception inside an open encoder: process survives (validation layer on)"
  (define-values (code text) (run ENCODER-PROBE #:env '(("MTL_DEBUG_LAYER" . "1"))))
  (check-equal? code 0 text)
  (check-regexp-match #rx"Metal API Validation Enabled" text)
  (check-regexp-match #rx"caught: probe: injected fault" text)
  (check-regexp-match #rx"after: BGRA\\(0,0\\)=\\(0 0 255 255\\)" text)   ; 之后仍能正确渲染
  (check-false (regexp-match? #rx"failed assertion" text)))

(test-case "NSException is not catchable from Racket; reporter sees it before abort"
  (define-values (code text) (run NSEXC-PROBE))
  (check-equal? code 134 text)                                            ; SIGABRT
  (check-regexp-match #rx"REPORTED NSRangeException: .*beyond bounds" text)
  (check-false (regexp-match? #rx"UNREACHABLE" text)))
