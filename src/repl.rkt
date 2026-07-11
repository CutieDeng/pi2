#lang tstring racket
;; repl.rkt — 终端交互层（design.md §4.9 / §5.7 / §11）
;; 交互式走异步实时控制台（console）：全程 raw 模式、底部固定输入行、输出滚动其上，
;; 用户可在 agent 流式输出时异步键入而不撞入输出。管道/非交互回退纯 read-line。
;; 所有输出（流式渲染、斜杠命令回显、权限询问）统一经 emit 汇聚，避免绕过控制台锁。

(require
 racket/string
 racket/list
 racket/math                                 ; exact-round
 racket/async-channel
 racket/pvector
 (file "model.rkt")
 (file "event.rkt")
 (file "provider.rkt")
 (file "tool.rkt")
 (file "loop.rkt")
 (file "context.rkt")
 (file "session.rkt")
 (file "plugin.rkt")
 (file "tui/terminal.rkt")
 (file "tui/console.rkt")
 (file "tui/sanitize.rkt")
) ; end require

;; ---------------------------------------------------------------- ANSI

(define (dim s) f"\e[2m{s}\e[0m")
(define (bold s) f"\e[1m{s}\e[0m")
(define (cyan s) f"\e[36m{s}\e[0m")
(define (green s) f"\e[32m{s}\e[0m")
(define (red s) f"\e[31m{s}\e[0m")
(define (yellow s) f"\e[33m{s}\e[0m")

;; ---------------------------------------------------------------- 元命令表

;; '/' 元命令组：名称 · 参数 · 说明。既驱动 /help，也驱动 TUI 实时预览面板。
(define COMMANDS
  '(("/help"    ""     "show commands")
    ("/quit"    ""     "exit (session saved)")
    ("/clear"   ""     "clear conversation history")
    ("/usage"   ""     "token usage so far")
    ("/compact" ""     "summarize old history to save context")
    ("/history" ""     "message count and roles")
    ("/tail"    "[n]"  "show last n cached output lines (default 20)")
    ("/resume"  ""     "pick another session to resume")
    ("/model"   "<id>" "switch model")
   ) ; end list
) ; end define COMMANDS

;; 两串的最长公共前缀
(define (common-prefix a b)
  (define n (min (string-length a) (string-length b)))
  (let loop ([i 0])
    (if (and (< i n) (char=? (string-ref a i) (string-ref b i))) (loop (add1 i)) (substring a 0 i)))
) ; end define common-prefix

(define (longest-common-prefix strs)
  (if (null? strs) "" (for/fold ([p (car strs)]) ([s (in-list (cdr strs))]) (common-prefix p s)))
) ; end define longest-common-prefix

;; Tab 补全（对给定命令表）：唯一匹配补全为「/name 」，多匹配补到最长公共前缀。
;; 已进入参数区（命令名后有空格）或非 '/' 起头 → 返回 #f（不补全）。
(define (complete-for cmds text)
  (cond
    [(and (positive? (string-length text)) (char=? (string-ref text 0) #\/))
     (define parts (string-split text))
     (define tok (if (null? parts) "/" (car parts)))
     (cond
       [(> (string-length text) (string-length tok)) #f]   ; 命令名后已有空格/参数
       [else
        (define matches (filter (lambda (c) (string-prefix? (car c) tok)) cmds))
        (cond
          [(null? matches) #f]
          [(= (length matches) 1)
           (define name (caar matches))
           (if (string=? tok name) #f (string-append name " "))]
          [else
           (define lcp (longest-common-prefix (map car matches)))
           (if (> (string-length lcp) (string-length tok)) lcp #f)])]
     ) ; end cond
    ] ; end slash
    [else #f]
  ) ; end cond
) ; end define complete-for

(define (command-complete text) (complete-for COMMANDS text))

;; 当前输入若以 '/' 起头，返回匹配命令的暗色预览行（供 console 实时渲染）。
(define (hint-lines-for cmds text)
  (cond
    [(and (positive? (string-length text)) (char=? (string-ref text 0) #\/))
     (define parts (string-split text))
     (define tok (if (null? parts) "/" (car parts)))
     (for/list ([c (in-list cmds)] #:when (string-prefix? (car c) tok))
       (dim f"  {(car c)} {(cadr c)}  {(caddr c)}")
     ) ; end for/list
    ] ; end command case
    [else '()]
  ) ; end cond
) ; end define hint-lines-for

(define (command-hint-lines text) (hint-lines-for COMMANDS text))

;; 从插件宿主提取命令表项 (name args desc)，与内置 COMMANDS 合并供 /help、预览、补全。
(define (plugin-command-specs host)
  (if host
      (for/list ([(name spec) (in-hash (host-commands host))])
        (list name (hash-ref spec 'args "") (hash-ref spec 'desc "(plugin command)")))
      '())
) ; end define plugin-command-specs

;; ---------------------------------------------------------------- 渲染订阅者

;; 工作动画标签（首 token 前 / 每次等待模型输出时显示的转轮标签）
(define STATUS-WORKING "thinking…")

;; make-renderer : emit status! tick! -> (event -> void)
;; emit    : (string -> void)  文本汇聚到控制台/stdout
;; status! : (boolean -> void) #t=显示工作动画，#f=清除（console 输出到达时亦自动清除）
;; tick!   : (exact -> void)   上报本段输出字符数（供 token/s 速率估计）
;; 不直接触碰终端，故与 console 的写锁协调一致，也可离线测试。
(define (make-renderer emit [status! void] [tick! void])
  (define in-thinking (box #f))
  (lambda (e)
    (cond
      [(evt:delta? e)
       (tick! (string-length (evt:delta-text e)))   ; 计入速率估计
       (case (evt:delta-kind e)
         [(thinking)
          (unless (unbox in-thinking)
            (set-box! in-thinking #t)
            (emit (dim "\n💭 "))
          ) ; end unless
          (emit (dim (sanitize-untrusted (evt:delta-text e))))
         ] ; end thinking case
         [(text)
          (when (unbox in-thinking)
            (set-box! in-thinking #f)
            (emit "\n")
          ) ; end when
          (emit (sanitize-untrusted (evt:delta-text e)))
         ] ; end text case
         [else (void)]                       ; tool-json 增量不直接渲染
       ) ; end case
      ] ; end delta case
      [(evt:tool-start? e)
       (define b (evt:tool-start-block e))
       (define arg-str (dim (string-append "(" (summarize-input (tool-use-block-input b)) ")")))
       (emit (string-append "\n" (cyan "⏺") " " (bold (tool-use-block-name b)) arg-str))
      ] ; end tool-start case
      [(evt:tool-end? e)
       (define ms (exact-round (evt:tool-end-ms e)))
       (emit (string-append " " (dim f"— {ms}ms") "\n"))
       (status! #t)                          ; 工具毕，等待模型下一段输出 → 转轮
      ] ; end tool-end case
      [(evt:error? e)
       (emit (red f"\n[error] {(sanitize-untrusted (exn-message (evt:error-exn e)))}\n"))
      ] ; end error case
      [(evt:turn-end? e)
       (emit "\n")
       (status! #f)                          ; 本轮结束，停动画
      ] ; end turn-end case
      [else (void)]
    ) ; end cond
  ) ; end lambda
) ; end define make-renderer

(define (summarize-input input)
  (define s (sanitize-untrusted (format "~a" input)))   ; 参数来自模型，消毒后再显示
  (if (> (string-length s) 60)
      (string-append (substring s 0 60) "…")
      s
  ) ; end if
) ; end define summarize-input

;; ---------------------------------------------------------------- 权限询问

;; y/n/a(lways) 一行 → 决策符号
(define (parse-answer line)
  (cond
    [(eof-object? line) 'no]
    [else
     (case (string-downcase (string-trim line))
       [("y" "yes") 'yes]
       [("a" "always") 'always]
       [else 'no]
     ) ; end case
    ] ; end else
  ) ; end cond
) ; end define parse-answer

;; 阻塞式权限询问（管道/非交互：纯 stdin）
(define (tty-asker prompt)
  (display (yellow f"\n{prompt} [y/n/a(lways)] "))
  (flush-output)
  (parse-answer (read-line))
) ; end define tty-asker

;; 交互模式征询走控制台（改道下一提交行）；无控制台时回退 tty-asker。
;; 供 main 装配进 deps；run-repl! 会在交互期 parameterize current-console。
(define current-console (make-parameter #f))

;; 交互式审批用选框（比 y/n/a 回显更清晰），末项支持填写拒绝理由回传给 agent。
(define APPROVE-OPTIONS
  '(("Yes — allow once"              . yes)
    ("Yes — and don't ask again"     . always)
    ("No — deny"                     . no)
    ("No — and tell the agent why"   . reason)))

(define (interactive-approve con prompt)
  (define idx (console-choose! con (sanitize-untrusted prompt) (map car APPROVE-OPTIONS)))
  (cond
    [(not idx) 'no]                              ; Esc = 拒绝
    [else
     (case (cdr (list-ref APPROVE-OPTIONS idx))
       [(yes) 'yes]
       [(always) 'always]
       [(no) 'no]
       [(reason)
        (define r (console-ask! con (yellow "reason for the agent (Enter to skip): ")))
        (if (and (string? r) (non-empty-string? (string-trim r)))
            (cons 'no (string-trim r))
            'no)]
     ) ; end case
    ] ; end else
  ) ; end cond
) ; end define interactive-approve

(define (interactive-asker prompt)
  (define con (current-console))
  (if con (interactive-approve con prompt) (tty-asker prompt))
) ; end define interactive-asker

;; ---------------------------------------------------------------- 主循环

;; run-repl! : deps agent-state session -> void（返回时会话已持久化）
;; data-dir: 会话目录（/resume 选择器与列表用）；resumed?: 是否为恢复启动（渲染预览）
(define (run-repl! d st0 sess #:banner? [banner? #t]
                   #:data-dir [data-dir "data"] #:resumed? [resumed? #f]
                   #:plugin-host [host #f])
  (define bus (deps-bus d))
  (if (terminal-port? (current-input-port))
      (run-repl/console! d st0 sess bus banner? data-dir resumed? host)
      (run-repl/plain! d st0 sess bus banner? data-dir resumed? host)
  ) ; end if
) ; end define run-repl!

;; -------- 交互式：异步实时控制台

(define (run-repl/console! d st0 sess bus banner? data-dir resumed? host)
  (define term (make-real-terminal))
  (define main-th (current-thread))
  (define (all-cmds) (append COMMANDS (plugin-command-specs host)))   ; 内置 + 插件命令
  (define con
    (make-console term #:prompt (green "› ")
                  #:interrupt (lambda () (break-thread main-th))  ; Ctrl-C → 取消当前轮
                  #:hint (lambda (t) (hint-lines-for (all-cmds) t))  ; '/' 预览（含插件命令）
                  #:complete (lambda (t) (complete-for (all-cmds) t)))  ; Tab 补全（含插件命令）
  ) ; end define con
  (define emit (lambda (s) (console-emit! con s)))
  (define (say s) (emit (string-append s "\n")))
  (when host                                            ; 插件 ctx UI 接 console
    (host-set-notify! host (lambda (msg . _) (say (dim msg))))
    (host-set-select! host (lambda (title opts)
                             (define i (console-choose! con (sanitize-untrusted title) opts))
                             (and i (list-ref opts i))))
    (host-set-confirm! host (lambda (title)
                              (equal? 0 (console-choose! con (sanitize-untrusted title) '("Yes" "No"))))))
  (define (statusf on?) (console-set-status! con (and on? STATUS-WORKING)))
  (define (tickf n) (console-tick-tokens! con n))
  (define unsub (bus-subscribe! bus (make-renderer emit statusf tickf)))
  (define sess-box (box sess))                  ; /resume 可切换会话
  (parameterize ([current-console con])
    (console-start! con)
    (when banner?
      (say (bold "pi++ — Racket LLM agent"))
      (say (dim f"model: {(config-model (agent-state-config st0))}  |  /help for commands"))
    ) ; end when
    (when resumed? (render-resume-preview emit st0))   ; 恢复启动：预览最近几条
    ;; reader 线程：持续读键、即时回显、提交行投递到 submit 通道
    (define reader
      (thread (lambda ()
                (let rloop ()
                  (case (console-handle-key! con (term-read-key term))
                    [(eof) (void)]
                    [else (rloop)]
                  ) ; end case
                ) ; end let rloop
              )) ; end thread
    ) ; end define reader
    (dynamic-wind
     void
     (lambda ()
       (let loop ([st st0])
         (console-set-idle! con #t)
         (define line (async-channel-get (console-submit-channel con)))
         (console-set-idle! con #f)
         (cond
           [(eof-object? line) (unsub) (session-close! (unbox sess-box))]
           [(string=? (string-trim line) "") (loop st)]
           [(string-prefix? (string-trim line) "/")
            (define-values (st* continue?)
              (handle-command (string-trim line) st d sess-box say emit data-dir host))
            (if continue? (loop st*) (begin (unsub) (session-close! (unbox sess-box))))
           ] ; end command case
           [else
            (define user-msg (text-msg 'user line))
            (console-set-working! con #t)       ; 整轮：底边隔离条进度动画
            (statusf #t)                        ; 首 token 前：转轮标签
            (define st*
              (with-handlers ([exn:break?
                               (lambda (_e)
                                 (provider-cancel! (deps-provider d))
                                 ;; 中断提示作为独立元信息块：先换行结束流式输出，
                                 ;; 再单起一行，不与上文回显同块（也不回显 ^C）。
                                 (emit "\n")
                                 (say (yellow "⎯ interrupted ⎯"))
                                 st
                               ) ; end lambda
                              ]
                              [exn:fail?
                               (lambda (e) (say (red f"[error] {(exn-message e)}")) st)
                              ]) ; end handlers
                (parameterize-break #t (run-turn! st user-msg d))
              ) ; end with-handlers
            ) ; end define st*
            (statusf #f)                        ; 收尾：确保停动画
            (console-set-working! con #f)       ; 停底边进度动画
            (bus-drain! bus)
            (persist-turn! (unbox sess-box) st st*)
            (loop st*)
           ] ; end else
         ) ; end cond
       ) ; end let loop
     ) ; end body
     (lambda ()
       (kill-thread reader)
       (console-stop! con)
     ) ; end after
    ) ; end dynamic-wind
  ) ; end parameterize
) ; end define run-repl/console!

;; -------- 非交互：纯 read-line（管道友好）

(define (run-repl/plain! d st0 sess bus banner? data-dir resumed? host)
  (define emit (lambda (s) (display s) (flush-output)))
  (define (say s) (emit (string-append s "\n")))
  (define unsub (bus-subscribe! bus (make-renderer emit)))
  (define sess-box (box sess))
  (when host (host-set-notify! host (lambda (msg . _) (say msg))))
  (when banner?
    (say (bold "pi++ — Racket LLM agent"))
    (say (dim f"model: {(config-model (agent-state-config st0))}  |  /help for commands"))
  ) ; end when
  (when resumed? (render-resume-preview emit st0))
  (let loop ([st st0])
    (define line (read-input/plain))
    (cond
      [(eof-object? line) (unsub) (session-close! (unbox sess-box))]
      [(string=? (string-trim line) "") (loop st)]
      [(string-prefix? (string-trim line) "/")
       (define-values (st* continue?)
         (handle-command (string-trim line) st d sess-box say emit data-dir host))
       (if continue? (loop st*) (begin (unsub) (session-close! (unbox sess-box))))
      ] ; end command case
      [else
       (define user-msg (text-msg 'user line))
       (define st*
         (with-handlers ([exn:fail?
                          (lambda (e) (say (red f"[error] {(exn-message e)}")) st)
                         ]) ; end handlers
           (run-turn! st user-msg d)
         ) ; end with-handlers
       ) ; end define st*
       (bus-drain! bus)
       (persist-turn! (unbox sess-box) st st*)
       (loop st*)
      ] ; end else
    ) ; end cond
  ) ; end let loop
) ; end define run-repl/plain!

;; ---------------------------------------------------------------- 恢复预览

;; 恢复会话时把最近几条对话渲染进终端（design 选择：last few exchanges）。
(define RESUME-PREVIEW-N 3)
(define PREVIEW-CAP 280)                          ; 单条预览最大字符

(define (clip-text s)
  (define t (string-normalize-spaces s))
  (if (> (string-length t) PREVIEW-CAP) (string-append (substring t 0 PREVIEW-CAP) "…") t)
) ; end define clip-text

(define (format-msg-preview m)
  (define role (message-role m))
  (define txt (message-text m))
  (define tools (message-tool-uses m))
  (string-append
   (cond
     [(and (eq? role 'user) (> (string-length (string-trim txt)) 0))
      (green f"› {(clip-text txt)}\n")]
     [(and (eq? role 'assistant) (> (string-length (string-trim txt)) 0))
      f"{(sanitize-untrusted (clip-text txt))}\n"]
     [else ""])
   (apply string-append
          (for/list ([t (in-list tools)])
            (string-append (cyan "⏺") " " (bold (tool-use-block-name t)) "\n")))
  ) ; end string-append
) ; end define format-msg-preview

;; 渲染 st 历史的末尾几条（+ 省略与分隔提示）
(define (render-resume-preview emit st)
  (define hist (state-history-list st))
  (define total (length hist))
  (when (> total 0)
    (define tail (if (> total RESUME-PREVIEW-N) (take-right hist RESUME-PREVIEW-N) hist))
    (define omitted (- total (length tail)))
    (emit (dim f"\n── resumed · {total} messages ──\n"))
    (when (> omitted 0) (emit (dim f"…({omitted} earlier)\n")))
    (for ([m (in-list tail)]) (emit (format-msg-preview m)))
    (emit (dim "────────────\n"))
  ) ; end when
) ; end define render-resume-preview

;; 持久化 st→st* 之间新增的历史消息与 usage 增量（tool-result 亦完整落盘，保 resume 配对）
(define (persist-turn! sess st-before st-after)
  (define before (pvector-length (agent-state-history st-before)))
  (define after (pvector-length (agent-state-history st-after)))
  (define hist (agent-state-history st-after))
  (for ([i (in-range before after)])
    (session-append-msg! sess (pvector-ref hist i))
  ) ; end for
  (define u-before (agent-state-token-usage st-before))
  (define u-after (agent-state-token-usage st-after))
  (define delta
    (usage (- (usage-input-tokens u-after) (usage-input-tokens u-before))
           (- (usage-output-tokens u-after) (usage-output-tokens u-before))
    ) ; end usage
  ) ; end define delta
  (unless (equal? delta usage-zero)
    (session-append-usage! sess delta)
  ) ; end unless
) ; end define persist-turn!

;; 纯 read-line（\ 结尾续行）
(define (read-input/plain)
  (let loop ([acc '()])
    (define line (read-line))
    (cond
      [(eof-object? line) (if (null? acc) line (string-join (reverse acc) "\n"))]
      [(string-suffix? line "\\")
       (loop (cons (substring line 0 (sub1 (string-length line))) acc))
      ] ; end continuation case
      [else (string-join (reverse (cons line acc)) "\n")]
    ) ; end cond
  ) ; end let loop
) ; end define read-input/plain

;; ---------------------------------------------------------------- 斜杠命令

;; handle-command : cmd st d sess-box say emit data-dir host -> (values new-state continue?)
;; sess-box: box of 当前 session（/resume 可切换）；emit: 原样输出（预览用）；host: 插件宿主
(define (handle-command cmd st d sess-box say emit data-dir host)
  (define parts (string-split cmd))
  (define name (car parts))
  (define args (cdr parts))
  (case name
    [("/quit" "/exit" "/q") (values st #f)]
    [("/help")
     (say (dim "commands:"))
     (for ([c (in-list COMMANDS)])
       (say (dim f"  {(car c)} {(cadr c)}  {(caddr c)}"))
     ) ; end for
     (for ([c (in-list (plugin-command-specs host))])   ; 插件命令
       (say (dim f"  {(car c)} {(cadr c)}  {(caddr c)}")))
     (values st #t)
    ] ; end help case
    [("/clear")
     (say (dim "history cleared"))
     (values (make-initial-state (agent-state-config st)) #t)
    ] ; end clear case
    [("/usage")
     (define u (agent-state-token-usage st))
     (say f"tokens — input: {(usage-input-tokens u)}, output: {(usage-output-tokens u)}, turns: {(agent-state-turn-count st)}")
     (values st #t)
    ] ; end usage case
    [("/compact")
     (say (dim "compacting…"))
     (define st*
       (with-handlers ([exn:fail?
                        (lambda (e) (say (red f"compact failed: {(exn-message e)}")) st)
                       ]) ; end handlers
         (compact! st (deps-provider d))
       ) ; end with-handlers
     ) ; end define st*
     (say (dim f"history: {(pvector-length (agent-state-history st))} → {(pvector-length (agent-state-history st*))} messages"))
     (values st* #t)
    ] ; end compact case
    [("/history")
     (define hist (agent-state-history st))
     (say f"{(pvector-length hist)} messages:")
     (for ([m (in-pvector hist)] [i (in-naturals)])
       (define preview (message-text m))
       (say (dim f"  {i}. {(message-role m)}: {(substring preview 0 (min 50 (string-length preview)))}"))
     ) ; end for
     (values st #t)
    ] ; end history case
    [("/tail")
     ;; 从控制台滚动缓存提取最后 n 行——超长会话的局部信息显示。
     (define con (current-console))
     (cond
       [(not con) (say (dim "/tail 仅在交互式 TUI 可用")) (values st #t)]
       [else
        (define n (if (and (pair? args) (string->number (car args)))
                      (inexact->exact (string->number (car args)))
                      20))
        (define lines (console-tail-lines con n))
        (say (dim f"— last {(length lines)} cached lines —"))
        (for ([l (in-list lines)]) (say l))
        (values st #t)
       ] ; end else
     ) ; end cond
    ] ; end tail case
    [("/resume")
     ;; 内嵌选择器切换会话：关闭当前、重放所选、按当前 config 续写，并预览。
     (define con (current-console))
     (cond
       [(not con) (say (dim "/resume 仅在交互式 TUI 可用")) (values st #t)]
       [else
        (define cur (let ([p (session-path (unbox sess-box))]) (if (path? p) (path->string p) p)))
        (define others (filter (lambda (i) (not (equal? (session-info-path i) cur)))
                               (session-infos data-dir)))
        (cond
          [(null? others) (say (dim "no other sessions to resume")) (values st #t)]
          [else
           (define idx (console-pick! con others
                                      #:title "Resume a session (Esc = cancel)"
                                      #:render-item session-info->line))
           (cond
             [(not idx) (values st #t)]           ; 取消
             [else
              (define path (session-info-path (list-ref others idx)))
              (session-close! (unbox sess-box))
              (define st* (session-replay path #:config (agent-state-config st)))
              (set-box! sess-box (session-open! (string->path path) (agent-state-config st*)))
              (render-resume-preview emit st*)
              (values st* #t)
             ] ; end chosen
           ) ; end cond
          ] ; end else
        ) ; end cond
       ] ; end else
     ) ; end cond
    ] ; end resume case
    [("/model")
     (cond
       [(null? args) (say (red "usage: /model <id>")) (values st #t)]
       [else
        (define cfg* (struct-copy config (agent-state-config st) [model (car args)]))
        (say (dim f"model → {(car args)}"))
        (values (struct-copy agent-state st [config cfg*]) #t)
       ] ; end else
     ) ; end cond
    ] ; end model case
    [else
     ;; 插件命令：宿主里查到则用其 handler 执行（传 plugin-ctx）。
     (define spec (and host (host-command host name)))
     (cond
       [spec
        (define handler (hash-ref spec 'handler #f))
        (when (procedure? handler)
          (with-handlers ([exn:fail? (lambda (e) (say (red f"plugin command failed: {(exn-message e)}")))])
            (handler args (make-ctx host))))
        (values st #t)
       ] ; end plugin command
       [else
        (say (red f"unknown command: {name} (try /help)"))
        (values st #t)
       ] ; end unknown
     ) ; end cond
    ] ; end else
  ) ; end case
) ; end define handle-command

;; ---------------------------------------------------------------- provide

(provide
 run-repl!
 make-renderer
 tty-asker
 interactive-asker
 current-console
 persist-turn!
 command-complete
 command-hint-lines
) ; end provide
