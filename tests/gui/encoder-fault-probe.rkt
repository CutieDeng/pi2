#lang racket/base
;; tests/gui/encoder-fault-probe.rkt — 子进程探针（由 encoder-guard-test 调用，raco test 不直接跑）
;; 在 encoder 建好、endEncoding 之前注入 Racket 异常，检查进程是否存活、设备是否仍可用。
(require "../../src/gui/components/mac-renderer.rkt" "../../src/gui/compute/drawlist.rkt"
         "../../src/gui/compute/hub.rkt" "../../src/gui/compute/interfaces.rkt")
(module+ main
  (define g (env-ref (instantiate renderer-component (hasheq) #:config (hasheq 'layer? #f)) gpu-port))
  (define dl (make-drawlist))
  (drawlist-rect! dl 0 0 16 16 1.0 0.0 0.0 1.0)
  (define caught
    (with-handlers ([exn:fail? exn-message])
      (parameterize ([current-encode-fault-hook (lambda (enc) (error 'probe "injected fault"))])
        (g 'render-offscreen dl '(0.0 0.0 0.0 1.0) 16 16 1.0))))
  (printf "caught: ~a\n" caught)
  (define px (g 'render-offscreen dl '(0.0 0.0 0.0 1.0) 16 16 1.0))
  (printf "after: BGRA(0,0)=~a\n" (list (bytes-ref px 0) (bytes-ref px 1) (bytes-ref px 2) (bytes-ref px 3)))
  (printf "PROBE survived\n"))
