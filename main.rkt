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
 (file "src/plugin.rkt")
 (file "src/providers.rkt")
 (file "src/credentials.rkt")
 (file "src/auto.rkt")
 (file "src/retry.rkt")
 (file "src/rpc.rkt")
 (file "src/resources.rkt")
 (file "src/tui/terminal.rkt")
 (file "src/tui/picker.rkt")
) ; end require

;; 运行时目录：resources（skills/prompts/plugins）随包只读，锚定到项目根（main.rkt 所在处）。
;; 会话/缓存**可写**：默认亦落项目根 data/ cache/，但可用 PI_DATA_HOME / PI_CACHE_HOME 覆盖——
;; 这样 `racket -l pi2` 从只读安装位（catalog copy）运行时，把可写产物导到用户目录即可。
(define-runtime-path project-root ".")
(define (dir-or default env)
  (let ([e (getenv env)]) (if (and e (> (string-length e) 0)) (string->path e) default)))
(define data-dir (dir-or (build-path project-root "data") "PI_DATA_HOME"))
(define cache-dir (dir-or (build-path project-root "cache") "PI_CACHE_HOME"))

;; 系统提示词：装配到每次请求的首条 system 消息（provider.rkt 读取 config-system-prompt）。
;; 命令行未指定时用此默认值；resume 会沿用存档 config 的 system-prompt。
;; 核心行为约束：不确定即查证——宁可用工具核实，不臆测。
(define DEFAULT-SYSTEM
  (string-join
   '("You are pi++, a concise coding agent running in a terminal."
     ""
     "Tools: use the provided tools (read_file, write_file, edit_file, glob, grep, bash, spawn_agent)"
     "to inspect and modify files and run commands. Prefer reading a file before editing it."
     ""
     "Verify, don't guess. When you are not certain about something — a file's contents, a symbol's"
     "definition, an API's shape, the project layout, a command's output, or whether a path/name even"
     "exists — do NOT answer from assumption. First use a tool (read_file / grep / glob / bash) to"
     "check, then answer from what you actually observed. Never fabricate file paths, identifiers, or"
     "results. If something remains uncertain after checking, say so and state the assumption you made."
     ""
     "Be concise. Explain briefly what you did and why, and prefer showing evidence (a file excerpt or"
     "command output) over asserting."
    ) ; end list
   "\n"
  ) ; end string-join
) ; end define DEFAULT-SYSTEM

;; ---------------------------------------------------------------- 会话选择辅助

(define (print-session-table infos)
  (cond
    [(null? infos) (displayln "no sessions in data/")]
    [else
     (for ([info (in-list infos)] [i (in-naturals 1)])
       (printf "~a\t~a\n" i (session-info->line info)))]
  ) ; end cond
) ; end define print-session-table

;; 把 --resume/--rm 的实参（列表序号 或 路径）解析为存在的会话路径字符串；无则 #f
(define (resolve-source arg infos data-dir)
  (define n (string->number arg))
  (cond
    [(and n (exact-integer? n) (>= n 1) (<= n (length infos)))
     (session-info-path (list-ref infos (sub1 n)))]
    [(file-exists? arg) arg]
    [(file-exists? (build-path data-dir arg)) (path->string (build-path data-dir arg))]
    [else #f]
  ) ; end cond
) ; end define resolve-source

;; 交互选择器 → 会话路径（取消返回 #f = 新建）
(define (pick-session-path infos)
  (cond
    [(null? infos) (eprintf "no sessions to resume; starting fresh\n") #f]
    [(not (terminal-port? (current-input-port)))
     (eprintf "--pick requires a terminal\n") (exit 1)]
    [else
     (define idx (run-picker (make-real-terminal) infos
                             #:title "Resume a session (Esc = new session)"
                             #:render-item session-info->line))
     (and idx (session-info-path (list-ref infos idx)))]
  ) ; end cond
) ; end define pick-session-path

(module+ main
  (define model (box #f))
  (define endpoint (box #f))
  (define api-key (resolve-key "PI_API_KEY"))     ; env > 凭据文件
  (define set-key-arg (box #f))
  (define list-keys? (box #f))
  (define rm-key-arg (box #f))
  (define resume-path (box #f))
  (define prompt (box #f))
  (define mode (box #f))
  (define workdir (box (path->string (current-directory))))
  (define continue? (box #f))
  (define pick? (box #f))
  (define list? (box #f))
  (define fork-at (box #f))
  (define rm-arg (box #f))
  (define plugin-dirs (box '()))
  (define trust-plugins? (box #f))
  (define provider-arg (box #f))
  (define rpc? (box #f))
  (define reasoning-arg (box #f))
  (define auto-arg (box #f))
  (define fallback-arg (box #f))

  (command-line
   #:program "pi++"
   #:multi
   [("--plugins") dir "load plugins from a directory (repeatable)"
                  (set-box! plugin-dirs (cons dir (unbox plugin-dirs)))]
   #:once-each
   [("--trust-plugins") "grant all plugin trust/capabilities without prompting (persists)"
                        (set-box! trust-plugins? #t)]
   [("-m" "--model") m "model id" (set-box! model m)]
   [("-e" "--endpoint") e "OpenAI-compatible base url" (set-box! endpoint e)]
   [("--provider") name "LLM provider (lmstudio | openai | anthropic | deepseek | gemini | grok | plugin name)" (set-box! provider-arg name)]
   [("--set-key") env-name "store an API key (env var name); value read from stdin, then exit" (set-box! set-key-arg env-name)]
   [("--list-keys") "list configured provider keys (masked) and exit" (set-box! list-keys? #t)]
   [("--rm-key") env-name "delete a stored API key and exit" (set-box! rm-key-arg env-name)]
   [("--reasoning") lvl "reasoning effort: off | low | medium | high | max (default off)" (set-box! reasoning-arg lvl)]
   [("--auto") onoff "auto model switching on|off (DeepSeek: flash/pro by task; default on)" (set-box! auto-arg onoff)]
   [("--fallback") chain "on-error fallback chain, comma-separated (provider[label]|model,…); overrides PI_FALLBACK" (set-box! fallback-arg chain)]
   [("--resume") arg "resume a session by list index or .rktd path" (set-box! resume-path arg)]
   [("-c" "--continue") "resume the most recent session" (set-box! continue? #t)]
   [("-i" "--pick") "pick a session to resume interactively" (set-box! pick? #t)]
   [("--list") "list sessions and exit" (set-box! list? #t)]
   [("--fork-at") n "fork the resumed session at message N (new branch)" (set-box! fork-at (string->number n))]
   [("--rm") arg "delete a session (list index or path) and exit" (set-box! rm-arg arg)]
   [("-p" "--prompt") p "one-shot prompt (non-interactive)" (set-box! prompt p)]
   [("--rpc") "headless JSONL mode over stdin/stdout (for IDE / orchestrator)" (set-box! rpc? #t)]
   [("--mode") md "permission mode: yolo|normal|strict" (set-box! mode md)]
   [("-C" "--workdir") wd "working directory" (set-box! workdir wd)]
  ) ; end command-line

  ;; ---- 推理强度（全局运行时 box，非 prefab config）：--reasoning 设初值
  (when (unbox reasoning-arg)
    (define lvl (string->symbol (unbox reasoning-arg)))
    (cond
      [(valid-reasoning-effort? lvl) (set-reasoning-effort! lvl)]
      [else (eprintf "invalid --reasoning: ~a (off|low|medium|high|max)\n" (unbox reasoning-arg)) (exit 1)]))

  ;; ---- Auto 模式：--auto on|off（默认 on，仅 DeepSeek 生效）
  (when (unbox auto-arg)
    (cond
      [(member (unbox auto-arg) '("on" "off")) (set-auto-mode! (string=? (unbox auto-arg) "on"))]
      [else (eprintf "invalid --auto: ~a (on|off)\n" (unbox auto-arg)) (exit 1)]))

  ;; ---- on-error 回退链：--fallback a,b,c（覆盖 env PI_FALLBACK；进程级 box，非 config）
  (when (unbox fallback-arg)
    (set-fallback-chain! (string-split (unbox fallback-arg) ",")))

  ;; ---- 凭据管理（--set-key / --list-keys / --rm-key）：即时执行并退出，不进会话
  (when (unbox set-key-arg)
    (define name (unbox set-key-arg))
    (eprintf "paste value for ~a (input hidden not guaranteed; prefer piping): " name)
    (define v (read-line))
    (cond
      [(or (eof-object? v) (= 0 (string-length (string-trim v))))
       (eprintf "no value given; aborted\n") (exit 1)]
      [else
       (store-key! name (string-trim v))
       (printf "stored ~a = ~a  (~a)\n" name (mask-key (string-trim v)) (credentials-path))
       (exit 0)]))
  (when (unbox rm-key-arg)
    (if (delete-key! (unbox rm-key-arg))
        (printf "removed ~a\n" (unbox rm-key-arg))
        (eprintf "no stored key: ~a\n" (unbox rm-key-arg)))
    (exit 0))
  (when (unbox list-keys?)
    (printf "credentials file: ~a\n" (credentials-path))
    (for ([p (in-list (builtin-provider-names))])
      (define env (provider-profile-key-env-of p))
      (when env
        (define src (key-source env))
        (printf "  ~a\t~a\t~a\n" p env
                (case src
                  [(env)  "env"]
                  [(file) (string-append "file " (mask-key (resolve-key env)))]
                  [else   "— (unset)"]))))
    (define insts (all-instances))
    (unless (null? insts)
      (printf "instances:\n")
      (for ([bl (in-list insts)])
        (printf "  ~a\tfile ~a\n" (instance-display (car bl) (cdr bl))
                (mask-key (resolve-instance-key (car bl) (cdr bl))))))
    (exit 0))

  ;; ---- 解析会话选择：--list / --rm 即时退出；否则解析出待恢复路径（可能 #f=新建）
  (make-directory* data-dir)                    ; 确保可写会话目录存在（尤其自定义 PI_DATA_HOME）
  (define infos (session-infos data-dir))
  (when (unbox list?) (print-session-table infos) (exit 0))
  (when (unbox rm-arg)
    (define p (resolve-source (unbox rm-arg) infos data-dir))
    (cond
      [p (session-delete! p) (printf "removed ~a\n" p)]
      [else (eprintf "no such session: ~a\n" (unbox rm-arg))])
    (exit 0)
  ) ; end when
  (define source
    (cond
      [(unbox continue?)
       (or (session-latest data-dir) (begin (eprintf "no sessions to continue\n") (exit 1)))]
      [(unbox pick?) (pick-session-path infos)]
      [(unbox resume-path)
       (or (resolve-source (unbox resume-path) infos data-dir)
           (begin (eprintf "no such session: ~a\n" (unbox resume-path)) (exit 1)))]
      [else #f]
    ) ; end cond
  ) ; end define source
  ;; --fork-at：从 source 分叉出新会话文件，恢复该分支
  (define resolved
    (let ([s (if (and source (unbox fork-at))
                 (session-fork! source data-dir #:at (unbox fork-at))
                 source)])
      (and s (if (path? s) (path->string s) s))))
  (set-box! resume-path resolved)              ; 下游按已解析路径工作

  ;; 技能/提示词资源发现（skills/ prompts/ 项目目录）。技能渐进披露进系统提示词。
  (define skills (discover-resources (build-path project-root "skills")))
  (define prompts (discover-resources (build-path project-root "prompts")))

  ;; 组装 config：resume 时以存档 config 为基，命令行覆盖；追加技能清单到系统提示词。
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
                 [system-prompt (string-append (or (config-system-prompt base-cfg) "")
                                               (skills-addendum skills))]
    ) ; end struct-copy
  ) ; end define cfg

  ;; 装配 deps；工具集 = 内置工具 + spawn_agent（子 agent 只拿内置工具）
  (define bus (make-bus))
  (make-directory* cache-dir)
  (define perm-store (build-path cache-dir "permissions.rktd"))
  (define base-tools (builtin-tools cfg))
  ;; 可变 registry + 插件宿主（共享同一 registry，故插件工具直接可被模型调用）。
  (define registry (make-registry base-tools))
  (define host (make-plugin-host #:registry registry))
  (host-set-skills! host skills)
  (host-set-prompts! host prompts)
  (register-builtin-providers! host)     ; 注册内置档案：lmstudio/openai/anthropic/gemini/grok
  ;; 能力授权：从 cache/plugin-grants.rktd 恢复已授予项；载入时按信任/能力门询问。
  (define grants (make-grants (build-path cache-dir "plugin-grants.rktd")))
  (define plugin-asker
    (cond
      [(unbox trust-plugins?) (lambda (_q) 'always)]        ; --trust-plugins：全授予并持久化
      [(terminal-port? (current-input-port)) tty-asker]     ; 交互：y/n/a 询问
      [else (lambda (_q) 'no)]))                            ; 非交互：默认拒绝（保守）
  ;; 加载 plugins/（若存在）+ 命令行 --plugins 目录（先于解析 provider，供插件注册供应商）。
  (define default-plugins-dir (build-path project-root "plugins"))
  (define plugin-load-dirs
    (append (if (directory-exists? default-plugins-dir) (list default-plugins-dir) '())
            (reverse (unbox plugin-dirs))))
  (for ([pd (in-list plugin-load-dirs)])
    (load-plugins-dir! host pd
                       #:grants grants #:asker plugin-asker
                       #:on-error (lambda (p e) (eprintf "plugin load failed (~a): ~a\n" p e))))
  (define observer-unsub (bus-subscribe! bus (make-host-observer host)))   ; 观测型钩子分发（绑定以免模块顶层打印返回值）
  (void observer-unsub)
  ;; provider：--provider 名设为初始选用（校验），用分发器以支持 /provider 运行时切换。
  (define provider-name (or (unbox provider-arg) "lmstudio"))
  (unless (host-set-provider! host provider-name)
    (eprintf "unknown provider: ~a (available: ~a)\n"
             provider-name (string-join (host-available-providers host) " "))
    (exit 1))
  ;; 显式 --provider 选内置云档案时，把其 endpoint/key(从env)/model 写进 config；
  ;; 默认 lmstudio 不覆盖（尊重用户的 --endpoint / --model）。
  (define cfg*
    (if (and (unbox provider-arg) (builtin-provider-instance? provider-name))
        (apply-provider-profile cfg provider-name)
        cfg))
  (when (and (unbox provider-arg) (builtin-provider-instance? provider-name)
             (not (config-api-key cfg*)))
    (eprintf "warning: provider ~a has no token (env ~a unset & no stored key) — requests will fail auth\n"
             provider-name (or (provider-profile-key-env-of (instance-base provider-name)) "—")))
  (define prov (make-dispatch-provider host cfg*))
  ;; spawn_agent 用解析后的 provider；加入 registry。
  (define spawn-tool (make-spawn-agent-tool #:provider prov #:sub-tools base-tools))
  (registry-add! registry spawn-tool)
  (define d
    (make-deps #:provider prov
               #:registry registry
               #:bus bus
               #:policy (make-policy cfg #:store-path perm-store)
               #:asker interactive-asker
               #:plugin-host host
    ) ; end make-deps
  ) ; end define d

  ;; 初始状态：resume 恢复历史，否则空
  (define st0
    (if (unbox resume-path)
        (session-replay (unbox resume-path) #:config cfg*)
        (make-initial-state cfg*)
    ) ; end if
  ) ; end define st0

  ;; 会话文件：新建时落到 data/，resume 时沿用给定路径
  (define sess-path
    (or (and (unbox resume-path) (string->path (unbox resume-path)))
        (fresh-session-path data-dir)
    ) ; end or
  ) ; end define sess-path
  (define sess (session-open! sess-path cfg*))

  (cond
    ;; 无头 JSONL 模式（IDE/编排器）：复用内核，事件与响应走 stdout NDJSON。
    [(unbox rpc?)
     (run-rpc! d st0 sess #:plugin-host host)
    ] ; end rpc case
    ;; 单次问答模式
    [(unbox prompt)
     (define unsub (bus-subscribe! bus (make-renderer (lambda (s) (display s) (flush-output)))))
     (define-values (st0* auto-dec) (maybe-apply-auto st0 (unbox prompt) host))
     (when auto-dec (eprintf "auto → ~a (thinking ~a)\n" (car auto-dec) (cdr auto-dec)))
     (define st* (run-turn! st0* (text-msg 'user (unbox prompt)) d))
     (bus-drain! bus)
     (persist-turn! sess st0* st*)
     (unsub) (session-close! sess)
    ] ; end one-shot case
    ;; 交互式
    [else
     (run-repl! d st0 sess
                #:data-dir (path->string data-dir)
                #:resumed? (and (unbox resume-path) #t)
                #:plugin-host host)
    ] ; end else
  ) ; end cond
) ; end module+ main
