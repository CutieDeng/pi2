#lang tstring racket
;; tools/builtin.rkt — 内置工具集装配（design.md §4.3）

(require
 (file "../tool.rkt")
 (file "bash.rkt")
 (file "file.rkt")
 (file "search.rkt")
) ; end require

(define (builtin-tools _cfg)
  (list
   (make-bash-tool)
   (make-read-file-tool)
   (make-write-file-tool)
   (make-edit-file-tool)
   (make-glob-tool)
   (make-grep-tool)
  ) ; end list
) ; end define builtin-tools

(provide
 builtin-tools
) ; end provide
