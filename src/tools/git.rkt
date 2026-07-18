#lang tstring racket
;; tools/git.rkt — 一等公民 git 工具（真实项目开发的最低要求：结构化 diff/commit）。
;;
;; 相比让模型走裸 bash 拼 `git …`：
;;   * argv 数组直传 git（不过 shell）——commit message 等含空格/引号的参数零转义地狱，
;;     弱模型(deepseek/本地)也能可靠产出；也杜绝 shell 注入。
;;   * 权限分级为 'mutating（介于只读工具与 bash 'dangerous 之间）：normal 模式会征询，
;;     yolo 放行；比 bash 更贴合「仓库内改动」的语义。
;;   * 统一在 workdir 内执行，输出封顶截断。

(require
 racket/port
 (file "../tool.rkt")
 (file "bash.rkt")                      ; truncate-output（复用输出封顶）
) ; end require

(define GIT-EXE (or (find-executable-path "git") "/usr/bin/git"))
(define GIT-TIMEOUT-SECS 120)

;; 跑 git（argv 直传，不过 shell）→ (values exit-code combined-output)。
(define (run-git workdir args)
  (parameterize ([current-directory workdir])
    (define-values (proc out in _err)
      (apply subprocess #f #f 'stdout GIT-EXE args))
    (close-output-port in)
    (define buf (open-output-string))
    (define pump (thread (lambda () (copy-port out buf))))
    (define done (sync/timeout GIT-TIMEOUT-SECS proc))
    (cond
      [(not done)
       (subprocess-kill proc #t)
       (thread-wait pump)
       (values 'timeout (get-output-string buf))]
      [else
       (thread-wait pump)
       (close-input-port out)
       (values (subprocess-status proc) (get-output-string buf))])
  ) ; end parameterize
) ; end define run-git

(struct git-tool ()
  #:methods gen:tool
  [(define (tool-name _t) "git")
   (define (tool-permission-level _t) 'mutating)
   (define (tool-spec _t)
     (function-spec "git"
                    (string-append
                     "Run git in the working directory. Pass args as an array of strings "
                     "(argv, NOT a shell string) — e.g. [\"status\",\"--short\"], [\"diff\"], "
                     "[\"commit\",\"-m\",\"my message with spaces\"], [\"add\",\"-A\"], [\"log\",\"--oneline\",\"-10\"]. "
                     "No shell quoting needed. Returns combined stdout+stderr.")
                    (hasheq 'args (hasheq 'type "array"
                                          'description "git subcommand + flags as separate strings"
                                          'items (hasheq 'type "string")))
                    (list "args")
     ) ; end function-spec
   ) ; end define tool-spec
   (define (tool-run _t input ctx)
     (define raw (input-ref input 'args))
     (define args (and (list? raw) (filter string? raw)))
     (cond
       [(or (not args) (null? args))
        (err-outcome "missing required parameter: args (a non-empty array of strings, e.g. [\"status\"])")]
       [else
        (define-values (code out) (run-git (tool-ctx-workdir ctx) args))
        (define output (truncate-output out))
        (define label f"git {(string-join args " ")}")
        (cond
          [(eq? code 'timeout)
           (err-outcome f"git timed out after {GIT-TIMEOUT-SECS}s\n{output}" #:display f"{label} — timeout")]
          [(zero? code)
           (ok-outcome (if (string=? (string-trim output) "") "(no output)" output) #:display label)]
          [else
           (err-outcome f"git exited {code}\n{output}" #:display f"{label} — exit {code}")])]
     ) ; end cond
   ) ; end define tool-run
  ] ; end methods
) ; end struct git-tool

(define (make-git-tool) (git-tool))

(provide make-git-tool run-git)
