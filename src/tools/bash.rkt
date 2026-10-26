#lang tstring racket
;; tools/bash.rkt — shell 命令执行工具（design.md §5.3）

(require
 racket/string
 racket/port
 (file "../tool.rkt")
) ; end require

(define OUTPUT-CAP (* 30 1024))       ; 超过则头尾各留一半
(define HALF-CAP (quotient OUTPUT-CAP 2))

(define (truncate-output s)
  (if (<= (string-length s) OUTPUT-CAP)
      s
      (let ([n (string-length s)])
        (string-append
         (substring s 0 HALF-CAP)
         f"\n[truncated {(- n OUTPUT-CAP)} chars]\n"
         (substring s (- n HALF-CAP))
        ) ; end string-append
      ) ; end let
  ) ; end if
) ; end define truncate-output

(struct bash-tool
  (timeout-secs   ; 默认 120
  ) ; end fields
  #:methods gen:tool
  [(define (tool-name _t) "bash")
   (define (tool-permission-level _t) 'dangerous)
   (define (tool-spec _t)
     (function-spec "bash"
                    "Run a shell command with /bin/zsh in the working directory. Returns combined stdout+stderr."
                    (hasheq 'command (hasheq 'type "string"
                                             'description "The shell command to run")
                    ) ; end hasheq
                    (list "command")
     ) ; end function-spec
   ) ; end define tool-spec
   (define (tool-run t input ctx)
     (define cmd (input-str input 'command))
     (cond
       [(not cmd) (err-outcome "missing required parameter: command")]
       [else
        (parameterize ([current-directory (tool-ctx-workdir ctx)])
          ;; 独立进程组：中断 / 超时时 subprocess-kill 杀整个组（复合命令的孙进程也一并结束）
          (define-values (proc out in _err)
            (parameterize ([subprocess-group-enabled #t])
              (subprocess #f #f 'stdout "/bin/zsh" "-c" cmd))
          ) ; end define-values
          (close-output-port in)
          ;; 独立线程泵输出，避免管道写满死锁
          (define buf (open-output-string))
          (define pump
            (thread
             (lambda ()
               (copy-port out buf)
             ) ; end lambda
            ) ; end thread
          ) ; end define pump
          ;; 轮被中断（break）时杀掉子进程组再继续抛出——否则命令在后台继续跑（实测会留下孤儿进程）
          (define done
            (with-handlers ([exn:break? (lambda (e) (subprocess-kill proc #t) (raise e))])
              (sync/timeout (bash-tool-timeout-secs t) proc)))
          (cond
            [(not done)
             (subprocess-kill proc #t)
             (thread-wait pump)
             (err-outcome
              f"command timed out after {(bash-tool-timeout-secs t)}s\npartial output:\n{(truncate-output (get-output-string buf))}"
              #:display f"timeout after {(bash-tool-timeout-secs t)}s"
             ) ; end err-outcome
            ] ; end timeout case
            [else
             (thread-wait pump)
             (close-input-port out)
             (define code (subprocess-status proc))
             (define output (truncate-output (get-output-string buf)))
             (if (zero? code)
                 (ok-outcome (if (string=? output "") "(no output)" output))
                 (err-outcome f"exit code {code}\n{output}"
                              #:display f"exit {code}"
                 ) ; end err-outcome
             ) ; end if
            ] ; end done case
          ) ; end cond
        ) ; end parameterize
       ] ; end else
     ) ; end cond
   ) ; end define tool-run
  ] ; end methods
) ; end struct bash-tool

(define (make-bash-tool #:timeout-secs [timeout-secs 120])
  (bash-tool timeout-secs)
) ; end define make-bash-tool

(provide
 make-bash-tool
 truncate-output
) ; end provide
