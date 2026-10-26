#lang racket/base
;; tests/gui/gpu-selftest-test.rkt — Metal 离屏渲染 + 像素读回（子进程跑 app.rkt --selftest，不开窗口）

(require rackunit racket/runtime-path racket/port racket/system)

(define-runtime-path APP "../../src/gui/assembly/app.rkt")

(test-case "offscreen render matches expected pixels"
  (define out (open-output-string))
  (define ok?
    (parameterize ([current-output-port out] [current-error-port out]
                   [current-environment-variables (environment-variables-copy (current-environment-variables))])
      (putenv "MTL_DEBUG_LAYER" "1")                ; Metal API 校验层：误用即 abort，测试期暴露
      (system* (find-system-path 'exec-file) (path->string APP) "--selftest")))
  (define text (get-output-string out))
  (check-true ok? text)
  (check-regexp-match #rx"SELFTEST ok" text))
