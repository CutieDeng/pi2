#lang racket/base
;; hosts-file.rkt — 主机表文件（GUI 侧读写）：{config-home}/hosts.rktd，与内核的 src/hosts.rkt 同一个文件、同一格式（hash 的表）。
;; GUI 用到的键：'id 'ssh 'serve；内核另用 'workdir 'credential 'mode 'remote-cmd 'command（GUI 原样保留不认识的键）。

(require racket/file "gui-settings.rkt")

(define (hosts-file) (build-path (gui-config-home) "hosts.rktd"))

(define (load-hosts-file)
  (with-handlers ([exn:fail? (lambda (_e) '())])
    (define v (if (file-exists? (hosts-file))
                  (call-with-input-file (hosts-file) (lambda (in) (parameterize ([read-accept-reader #f]) (read in))))
                  '()))
    (if (list? v) (filter (lambda (h) (and (hash? h) (string? (hash-ref h 'id #f)))) v) '()))
) ; end define load-hosts-file

(define (save-hosts-file! hs)
  (with-handlers ([exn:fail? (lambda (e) (eprintf "pi2 GUI: cannot save hosts: ~a\n" (exn-message e)))])
    (make-directory* (gui-config-home))
    (define tmp (path-add-extension (hosts-file) #".tmp"))
    (call-with-output-file tmp #:exists 'truncate (lambda (out) (write hs out) (newline out)))
    (rename-file-or-directory tmp (hosts-file) #t))
) ; end define save-hosts-file!

(provide load-hosts-file save-hosts-file!)
