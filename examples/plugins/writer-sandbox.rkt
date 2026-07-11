#lang racket/base
;; writer-sandbox.rkt — 沙箱插件，需要 fs-write 能力（见旁置 writer-sandbox.rktd）。
;; 未授予 fs-write 时，沙箱拒绝写入；授予后可写工作区。

(provide manifest tool-run)

(define manifest '(tool "writer" "write text to a file in the workspace (needs fs-write)"))

(define (tool-run input)
  (define path (hash-ref input 'path "sbwrite.txt"))
  (define text (hash-ref input 'text "written by a sandboxed plugin"))
  (call-with-output-file path #:exists 'replace (lambda (o) (write-string text o)))
  (format "ok: wrote ~a bytes to ~a" (string-length text) path))
