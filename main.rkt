#!/usr/bin/env racket
#lang tstring racket
;; main.rkt — pi++ 可执行入口（design.md §8）
;; 用法：
;;   racket main.rkt                          交互式会话
;;   racket main.rkt -m <model> -e <endpoint> 覆盖模型/端点
;;   racket main.rkt --resume <session.rktd>  恢复会话
;;   racket main.rkt -p "问题"                单次问答（管道友好）
;;   racket main.rkt --mode yolo|normal|strict 权限模式

(require
 racket/cmdline
 racket/string
 racket/port
 racket/runtime-path
 (file "src/model.rkt")
 (file "src/event.rkt")
 (file "src/provider.rkt")
 (file "src/tool.rkt")
 (file "src/permission.rkt")
 (file "src/loop.rkt")
 (file "src/session.rkt")
 (file "src/repl.rkt")
 (file "src/subagent.rkt")
 (file "src/tools/builtin.rkt")
) ; end require

;; 运行时目录锚定到项目根（main.rkt 所在处），与 agent 的目标工作目录无关。
;; data/  存会话 transcript；cache/  存权限等跨会话缓存。
(define-runtime-path project-root ".")
(define data-dir (build-path project-root "data"))
(define cache-dir (build-path project-root "cache"))

(define DEFAULT-SYSTEM
  (string-join
   '("You are pi++, a concise coding agent running in a terminal."
     "Use the provided tools to inspect and modify files and run commands."
     "Prefer reading files before editing. Explain briefly what you did."
    ) ; end list
   " "
  ) ; end string-join
) ; end define DEFAULT-SYSTEM

(module+ main
  (define model (box #f))
  (define endpoint (box #f))
  (define api-key (getenv "PI_API_KEY"))
  (define resume-path (box #f))
  (define prompt (box #f))
  (define mode (box #f))
  (define workdir (box (path->string (current-directory))))

  (command-line
   #:program "pi++"
   #:once-each
   [("-m" "--model") m "model id" (set-box! model m)]
   [("-e" "--endpoint") e "OpenAI-compatible base url" (set-box! endpoint e)]
   [("--resume") path "resume a .rktd session" (set-box! resume-path path)]
   [("-p" "--prompt") p "one-shot prompt (non-interactive)" (set-box! prompt p)]
   [("--mode") md "permission mode: yolo|normal|strict" (set-box! mode md)]
   [("-C" "--workdir") wd "working directory" (set-box! workdir wd)]
  ) ; end command-line

  ;; 组装 config：resume 时以存档 config 为基，命令行覆盖
  (define base-cfg
    (if (unbox resume-path)
        (agent-state-config (session-replay (unbox resume-path)))
        (struct-copy config (default-config) [system-prompt DEFAULT-SYSTEM])
    ) ; end if
  ) ; end define base-cfg
  (define cfg
    (struct-copy config base-cfg
                 [model (or (unbox model) (config-model base-cfg))]
                 [endpoint (or (unbox endpoint) (config-endpoint base-cfg))]
                 [api-key api-key]
                 [permission-mode (if (unbox mode)
                                      (string->symbol (unbox mode))
                                      (config-permission-mode base-cfg))]
                 [workdir (unbox workdir)]
    ) ; end struct-copy
  ) ; end define cfg

  ;; 装配 deps；工具集 = 内置工具 + spawn_agent（子 agent 只拿内置工具）
  (define bus (make-bus))
  (make-directory* cache-dir)
  (define perm-store (build-path cache-dir "permissions.rktd"))
  (define prov (make-openai-provider cfg))
  (define base-tools (builtin-tools cfg))
  (define spawn-tool
    (make-spawn-agent-tool #:provider prov #:sub-tools base-tools)
  ) ; end define spawn-tool
  (define d
    (make-deps #:provider prov
               #:registry (make-registry (append base-tools (list spawn-tool)))
               #:bus bus
               #:policy (make-policy cfg #:store-path perm-store)
               #:asker interactive-asker
    ) ; end make-deps
  ) ; end define d

  ;; 初始状态：resume 恢复历史，否则空
  (define st0
    (if (unbox resume-path)
        (session-replay (unbox resume-path) #:config cfg)
        (make-initial-state cfg)
    ) ; end if
  ) ; end define st0

  ;; 会话文件：新建时落到 data/，resume 时沿用给定路径
  (define sess-path
    (or (and (unbox resume-path) (string->path (unbox resume-path)))
        (fresh-session-path data-dir)
    ) ; end or
  ) ; end define sess-path
  (define sess (session-open! sess-path cfg))

  (cond
    ;; 单次问答模式
    [(unbox prompt)
     (define unsub (bus-subscribe! bus (make-renderer (lambda (s) (display s) (flush-output)))))
     (define st* (run-turn! st0 (text-msg 'user (unbox prompt)) d))
     (bus-drain! bus)
     (persist-turn! sess st0 st*)
     (unsub) (session-close! sess)
    ] ; end one-shot case
    ;; 交互式
    [else
     (run-repl! d st0 sess)
    ] ; end else
  ) ; end cond
) ; end module+ main
