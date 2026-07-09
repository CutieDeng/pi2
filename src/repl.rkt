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
    ("/model"   "<id>" "switch model")
   ) ; end list
) ; end define COMMANDS

;; 当前输入若以 '/' 起头，返回匹配命令的暗色预览行（供 console 实时渲染）。
(define (command-hint-lines text)
  (cond
    [(and (positive? (string-length text)) (char=? (string-ref text 0) #\/))
     (define parts (string-split text))
     (define tok (if (null? parts) "/" (car parts)))
     (for/list ([c (in-list COMMANDS)] #:when (string-prefix? (car c) tok))
       (dim f"  {(car c)} {(cadr c)}  {(caddr c)}")
     ) ; end for/list
    ] ; end command case
    [else '()]
  ) ; end cond
) ; end define command-hint-lines

;; ---------------------------------------------------------------- 渲染订阅者

;; 工作动画标签（首 token 前 / 每次等待模型输出时显示的转轮标签）
(define STATUS-WORKING "thinking…")

;; make-renderer : emit status! -> (event -> void)
;; emit    : (string -> void)  文本汇聚到控制台/stdout
;; status! : (boolean -> void) #t=显示工作动画，#f=清除（console 输出到达时亦自动清除）
;; 不直接触碰终端，故与 console 的写锁协调一致，也可离线测试。
(define (make-renderer emit [status! void])
  (define in-thinking (box #f))
  (lambda (e)
    (cond
      [(evt:delta? e)
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

(define (interactive-asker prompt)
  (define con (current-console))
  (if con
      (parse-answer (console-ask! con (yellow f"{prompt} [y/n/a(lways)] ")))
      (tty-asker prompt)
  ) ; end if
) ; end define interactive-asker

;; ---------------------------------------------------------------- 主循环

;; run-repl! : deps agent-state session -> void（返回时会话已持久化）
(define (run-repl! d st0 sess #:banner? [banner? #t])
  (define bus (deps-bus d))
  (if (terminal-port? (current-input-port))
      (run-repl/console! d st0 sess bus banner?)
      (run-repl/plain! d st0 sess bus banner?)
  ) ; end if
) ; end define run-repl!

;; -------- 交互式：异步实时控制台

(define (run-repl/console! d st0 sess bus banner?)
  (define term (make-real-terminal))
  (define main-th (current-thread))
  (define con
    (make-console term #:prompt (green "› ")
                  #:interrupt (lambda () (break-thread main-th))  ; Ctrl-C → 取消当前轮
                  #:hint command-hint-lines)                      ; '/' 元命令实时预览
  ) ; end define con
  (define emit (lambda (s) (console-emit! con s)))
  (define (say s) (emit (string-append s "\n")))
  (define (statusf on?) (console-set-status! con (and on? STATUS-WORKING)))
  (define unsub (bus-subscribe! bus (make-renderer emit statusf)))
  (parameterize ([current-console con])
    (console-start! con)
    (when banner?
      (say (bold "pi++ — Racket LLM agent"))
      (say (dim f"model: {(config-model (agent-state-config st0))}  |  /help for commands"))
    ) ; end when
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
           [(eof-object? line) (unsub) (session-close! sess)]
           [(string=? (string-trim line) "") (loop st)]
           [(string-prefix? (string-trim line) "/")
            (define-values (st* continue?) (handle-command (string-trim line) st d sess say))
            (if continue? (loop st*) (begin (unsub) (session-close! sess)))
           ] ; end command case
           [else
            (define user-msg (text-msg 'user line))
            (statusf #t)                        ; 首 token 前即显示工作动画
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
            (bus-drain! bus)
            (persist-turn! sess st st*)
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

(define (run-repl/plain! d st0 sess bus banner?)
  (define emit (lambda (s) (display s) (flush-output)))
  (define (say s) (emit (string-append s "\n")))
  (define unsub (bus-subscribe! bus (make-renderer emit)))
  (when banner?
    (say (bold "pi++ — Racket LLM agent"))
    (say (dim f"model: {(config-model (agent-state-config st0))}  |  /help for commands"))
  ) ; end when
  (let loop ([st st0])
    (define line (read-input/plain))
    (cond
      [(eof-object? line) (unsub) (session-close! sess)]
      [(string=? (string-trim line) "") (loop st)]
      [(string-prefix? (string-trim line) "/")
       (define-values (st* continue?) (handle-command (string-trim line) st d sess say))
       (if continue? (loop st*) (begin (unsub) (session-close! sess)))
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
       (persist-turn! sess st st*)
       (loop st*)
      ] ; end else
    ) ; end cond
  ) ; end let loop
) ; end define run-repl/plain!

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

;; handle-command : cmd st d sess say -> (values new-state continue?)
;; say : (string -> void) 输出一行（经控制台/stdout 汇聚）
(define (handle-command cmd st d sess say)
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
     (say (red f"unknown command: {name} (try /help)"))
     (values st #t)
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
) ; end provide
