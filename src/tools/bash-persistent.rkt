#lang tstring racket
;; tools/bash-persistent.rkt — dsv4-b 锚定档案的持久化 bash（design-dsv4b.md §2.3）
;; 对齐 DeepSeek Harness minimal 预设的 bash 工具：name/参数 schema 与描述贴上游
;; （apps/cli/config/agent-presets/minimal/agent.cordis.yml 的 description 覆写），
;; 默认刻意删去其中「无互联网 / apt+pip 镜像」两条 bullet——那是基准沙箱的事实，
;; 在本地 pi2 环境为假，保留会误导模型拒绝联网命令。
;; 逃生门 `PI_DSV4B_BASH_DESC=verbatim`：恢复上游逐字 8 bullets，用于保真锚定实验
;; （首请求字节需与 RL 分布逐字一致时；见 tests/dsv4b-probes/ALIGNMENT.md）。
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
 (file "../commit-watch.rkt")           ; bash 里直接 git commit → 没带 pi2 标签的提示模型
) ; end require

(define DEFAULT-TIMEOUT-SECS 300)      ; 上游 minimal 预设 timeoutMs 300000

;; 上游 minimal 预设 bash 描述（agent.cordis.yml）的三段：公共头 / 仅-verbatim 两条 / 公共尾。
(define BASH-DESC-HEAD
  '("Run commands in a bash shell"
    "* When invoking this tool, the contents of the \"command\" parameter does NOT need to be XML-escaped."))
(define BASH-DESC-VERBATIM-ONLY        ; 上游有、默认删（本地为假）；verbatim 时恢复
  '("* You don't have access to the internet via this tool."
    "* You do have access to a mirror of common linux and python packages via apt and pip."))
(define BASH-DESC-TAIL
  '("* State is persistent across command calls and discussions with the user."
    "* To inspect a particular line range of a file, e.g. lines 10-25, try 'sed -n 10,25p /path/to/the/file'."
    "* Please avoid commands that may produce a very large amount of output."
    "* Please run long lived commands in the background, e.g. 'sleep 10 &' or start a server in the background."))

;; PI_DSV4B_BASH_DESC=verbatim → 上游逐字 8 bullets；否则默认 6 bullets（删联网/镜像两条）。
;; 在 tool-spec 里求值：进程启动时设的 env 即时生效，探针可控。
(define (bash-description)
  (string-join
   (append BASH-DESC-HEAD
           (if (equal? (getenv "PI_DSV4B_BASH_DESC") "verbatim") BASH-DESC-VERBATIM-ONLY '())
           BASH-DESC-TAIL)
   "\n"))

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
                    (bash-description)
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
        (watch-commits (tool-ctx-workdir ctx) (lambda ()
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
        ;; 独立进程组：中断 / 超时时 subprocess-kill 杀整个组（与 bash.rkt 一致；这条命令里起的后台任务也随之结束）
        (define-values (proc out in _err)
          (parameterize ([subprocess-group-enabled #t])
            (subprocess #f #f 'stdout "/bin/bash" "--noprofile" "--norc" "-c" wrapper))
        ) ; end define-values
        (close-output-port in)
        (define buf (open-output-string))
        (define pump (thread (lambda () (copy-port out buf))))
        ;; 轮被中断（break）时杀掉子进程组再继续抛出，否则命令留在后台继续跑
        (define done
          (with-handlers ([exn:break? (lambda (e) (subprocess-kill proc #t) (raise e))])
            (sync/timeout (persistent-bash-tool-timeout-secs t) proc)))
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
        )) ; end watch-commits
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
 bash-description
) ; end provide
