#lang tstring racket
;; tools/builtin.rkt — 内置工具集装配（design.md §4.3）

(require
 (file "../tool.rkt")
 (file "bash.rkt")
 (file "file.rkt")
 (file "search.rkt")
 (file "git.rkt")
 (file "racket-eval.rkt")               ; 沙箱 Racket（进程内按语义注入密钥）
 (file "../test-flow.rkt")              ; 工作区测试流程：tests_run / acceptance_report（design-goalmode.md §12）
) ; end require

(define (builtin-tools _cfg)
  (append (list
   (make-bash-tool)
   (make-read-file-tool)
   (make-write-file-tool)
   (make-edit-file-tool)
   (make-glob-tool)
   (make-grep-tool)
   (make-git-tool)
   (make-git-history-tool)              ; 只读：按 TYPE / 元数据查提交（datum 提交消息）
   (make-racket-eval-tool)
  ) ; end list
  ;; 工作区测试流程（没有 .pi/tests.rktd 时调用会说明「这个工作区没有测试流程」）
  (make-test-flow-tools))
) ; end define builtin-tools

(provide
 builtin-tools
) ; end provide
