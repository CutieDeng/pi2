#lang tstring racket
;; tools/bash-persistent.rkt — dsv4-b 锚定档案的持久化 bash（design-dsv4b.md §2.3）
;; 对齐 DeepSeek Harness minimal 预设的 bash 工具：name/参数 schema 与描述贴上游
;; （apps/cli/config/agent-presets/minimal/agent.cordis.yml 的 description 覆写），
;; 刻意删去其中「无互联网 / apt+pip 镜像」两条 bullet——那是基准沙箱的事实，
;; 在本地 pi2 环境为假，保留会误导模型拒绝联网命令。
;;
;; 「持久」的实现不走 PTY（上游用持久 shell 进程 + 标记协议），而是状态文件回放：
;; 每次调用在 /bin/bash 中先恢复上次的 env（export -p 落盘）与 cwd，执行后再落盘。
;; 覆盖描述所声明的语义（cwd + 导出环境变量跨调用保持）；函数/别名/后台任务不保。

(require
 racket/string
 racket/port
 racket/file
 (file "../tool.rkt")
 (file "bash.rkt")                      ; truncate-output
) ; end require

(define DEFAULT-TIMEOUT-SECS 300)      ; 上游 minimal 预设 timeoutMs 300000

(define PERSISTENT-BASH-DESCRIPTION
  (string-join
   '("Run commands in a bash shell"
     "* When invoking this tool, the contents of the \"command\" parameter does NOT need to be XML-escaped."
     "* State is persistent across command calls and discussions with the user."
     "* To inspect a particular line range of a file, e.g. lines 10-25, try 'sed -n 10,25p /path/to/the/file'."
     "* Please avoid commands that may produce a very large amount of output."
     "* Please run long lived commands in the background, e.g. 'sleep 10 &' or start a server in the background.")
   "\n"
  ) ; end string-join
) ; end define PERSISTENT-BASH-DESCRIPTION

;; POSIX 单引号转义：' → '\''
(define (shq s)
  (string-append "'" (string-replace s "'" "'\\''") "'")
) ; end define shq

(struct persistent-bash-tool
  (timeout-secs
   cwd-file      ; 上次 cwd
   env-file      ; 上次 export -p 快照
   cmd-file      ; 本次命令正文（source 执行，避免引号转义地狱）
  ) ; end fields
  #:methods gen:tool
  [(define (tool-name _t) "bash")
   (define (tool-permission-level _t) 'dangerous)
   (define (tool-spec _t)
     (function-spec "bash"
                    PERSISTENT-BASH-DESCRIPTION
                    (hasheq 'command
                            (hasheq 'type "string"
                                    'description "The bash command to run. Relative path is preferred in the command."))
                    (list "command")
     ) ; end function-spec
   ) ; end define tool-spec
   (define (tool-run t input ctx)
     (define cmd (input-str input 'command))
     (cond
       [(or (not cmd) (string=? (string-trim cmd) ""))
        (err-outcome "command must be a non-empty string")]
       [else
        (define cwd-f (path->string (persistent-bash-tool-cwd-file t)))
        (define env-f (path->string (persistent-bash-tool-env-file t)))
        (define cmd-f (persistent-bash-tool-cmd-file t))
        (define workdir (path->string (path->complete-path (tool-ctx-workdir ctx))))
        (display-to-file cmd cmd-f #:exists 'truncate)
        ;; 先恢复 env（PWD 等随后被 cd 修正），再恢复 cwd，source 命令，落盘新状态。
        (define wrapper
          (string-join
           (list
            f"[ -s {(shq env-f)} ] && . {(shq env-f)} >/dev/null 2>&1"
            f"if [ -s {(shq cwd-f)} ]; then cd \"$(cat {(shq cwd-f)})\" 2>/dev/null || cd {(shq workdir)}; else cd {(shq workdir)}; fi"
            f". {(shq (path->string cmd-f))}"
            "__pi2_rc=$?"
            f"pwd > {(shq cwd-f)}"
            f"export -p > {(shq env-f)}"
            "exit $__pi2_rc")
           "\n"
          ) ; end string-join
        ) ; end define wrapper
        (define-values (proc out in _err)
          (subprocess #f #f 'stdout "/bin/bash" "--noprofile" "--norc" "-c" wrapper)
        ) ; end define-values
        (close-output-port in)
        (define buf (open-output-string))
        (define pump (thread (lambda () (copy-port out buf))))
        (define done (sync/timeout (persistent-bash-tool-timeout-secs t) proc))
        (cond
          [(not done)
           (subprocess-kill proc #t)
           (drain-pump! pump out 1.0)
           (err-outcome
            f"command timed out after {(persistent-bash-tool-timeout-secs t)}s\npartial output:\n{(truncate-output (get-output-string buf))}"
            #:display f"timeout after {(persistent-bash-tool-timeout-secs t)}s"
           ) ; end err-outcome
          ] ; end timeout case
          [else
           ;; 后台任务（description 明确鼓励 `… &`）会继续持有 stdout 管道：
           ;; shell 已退出即定局，泵最多再等 1s 收尾，然后放弃管道（不阻塞在子孙进程上）。
           (drain-pump! pump out 1.0)
           (define code (subprocess-status proc))
           (define output (truncate-output (get-output-string buf)))
           (if (zero? code)
               (ok-outcome (if (string=? output "") "(no output)" output))
               (err-outcome f"exit code {code}\n{output}" #:display f"exit {code}")
           ) ; end if
          ] ; end done case
        ) ; end cond
       ] ; end else
     ) ; end cond
   ) ; end define tool-run
  ] ; end methods
) ; end struct persistent-bash-tool

;; 泵线程收尾：给 grace 秒等 EOF；仍被（后台子孙的）写端拖着就杀线程、关端口。
(define (drain-pump! pump out grace)
  (unless (sync/timeout grace pump)
    (kill-thread pump))
  (with-handlers ([exn:fail? void]) (close-input-port out))
) ; end define drain-pump!

(define (make-persistent-bash-tool #:timeout-secs [timeout-secs DEFAULT-TIMEOUT-SECS])
  (persistent-bash-tool
   timeout-secs
   (make-temporary-file "pi2-pbash-cwd-~a")
   (make-temporary-file "pi2-pbash-env-~a")
   (make-temporary-file "pi2-pbash-cmd-~a")
  ) ; end persistent-bash-tool
) ; end define make-persistent-bash-tool

(provide
 make-persistent-bash-tool
 PERSISTENT-BASH-DESCRIPTION
) ; end provide
