#lang racket-tstring
;; repl.rkt — 终端交互层（design.md §4.9 / §5.7）

(require
 racket/string
 racket/list
 racket/pvector
 (file "model.rkt")
 (file "event.rkt")
 (file "provider.rkt")
 (file "tool.rkt")
 (file "loop.rkt")
 (file "context.rkt")
 (file "session.rkt")
) ; end require

;; ---------------------------------------------------------------- ANSI

(define (dim s) f"\e[2m{s}\e[0m")
(define (bold s) f"\e[1m{s}\e[0m")
(define (cyan s) f"\e[36m{s}\e[0m")
(define (green s) f"\e[32m{s}\e[0m")
(define (red s) f"\e[31m{s}\e[0m")
(define (yellow s) f"\e[33m{s}\e[0m")

;; ---------------------------------------------------------------- 渲染订阅者

;; 流式渲染：delta 逐段输出。用 box 跟踪当前是否在 thinking 段以便着色/换行。
(define (make-renderer)
  (define in-thinking (box #f))
  (define any-text (box #f))
  (lambda (e)
    (cond
      [(evt:delta? e)
       (case (evt:delta-kind e)
         [(thinking)
          (unless (unbox in-thinking)
            (set-box! in-thinking #t)
            (display (dim "\n💭 "))
          ) ; end unless
          (display (dim (evt:delta-text e)))
         ] ; end thinking case
         [(text)
          (when (unbox in-thinking)
            (set-box! in-thinking #f)
            (newline)
          ) ; end when
          (set-box! any-text #t)
          (display (evt:delta-text e))
         ] ; end text case
         [else (void)]                      ; tool-json 增量不直接渲染
       ) ; end case
       (flush-output)
      ] ; end delta case
      [(evt:tool-start? e)
       (define b (evt:tool-start-block e))
       (define arg-str (dim (string-append "(" (summarize-input (tool-use-block-input b)) ")")))
       (display (string-append "\n" (cyan "⏺") " " (bold (tool-use-block-name b)) arg-str))
       (flush-output)
      ] ; end tool-start case
      [(evt:tool-end? e)
       (define ms (exact-round (evt:tool-end-ms e)))
       (display (string-append " " (dim f"— {ms}ms") "\n"))
       (flush-output)
      ] ; end tool-end case
      [(evt:error? e)
       (display (red f"\n[error] {(exn-message (evt:error-exn e))}\n"))
       (flush-output)
      ] ; end error case
      [(evt:turn-end? e)
       (newline)
       (flush-output)
      ] ; end turn-end case
      [else (void)]
    ) ; end cond
  ) ; end lambda
) ; end define make-renderer

(define (summarize-input input)
  (define s (format "~a" input))
  (if (> (string-length s) 60)
      (string-append (substring s 0 60) "…")
      s
  ) ; end if
) ; end define summarize-input

(require racket/math)                        ; exact-round

;; ---------------------------------------------------------------- 询问

;; 阻塞式权限询问（tty 交互）
(define (tty-asker prompt)
  (display (yellow f"\n{prompt} [y/n/a(lways)] "))
  (flush-output)
  (define line (read-line))
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
) ; end define tty-asker

;; ---------------------------------------------------------------- 主循环

;; run-repl! : deps agent-state session -> void
;; 返回时会话已持久化。
(define (run-repl! d st0 sess #:banner? [banner? #t])
  (define bus (deps-bus d))
  (define unsub (bus-subscribe! bus (make-renderer)))
  (when banner?
    (displayln (bold "pi++ — Racket LLM agent"))
    (displayln (dim f"model: {(config-model (agent-state-config st0))}  |  /help for commands"))
  ) ; end when
  (define interactive? (terminal-port? (current-input-port)))
  (let loop ([st st0])
    (when interactive?
      (display (green "\n› "))
      (flush-output)
    ) ; end when
    (define line (read-user-input))
    (cond
      [(eof-object? line)
       (unsub) (session-close! sess)
       (when interactive? (displayln (dim "\nbye")))
      ] ; end eof case
      [(string=? (string-trim line) "") (loop st)]
      [(string-prefix? (string-trim line) "/")
       (define-values (st* continue?) (handle-command (string-trim line) st d sess))
       (if continue?
           (loop st*)
           (begin (unsub) (session-close! sess))
       ) ; end if
      ] ; end command case
      [else
       (define user-msg (text-msg 'user line))
       (define st*
         (with-handlers ([exn:break?
                          (lambda (_e)
                            (displayln (yellow "\n[cancelled]"))
                            (provider-cancel! (deps-provider d))
                            st                ; 回滚到本轮开始前
                          ) ; end lambda
                         ]
                         [exn:fail?
                          (lambda (e)
                            (displayln (red f"\n[error] {(exn-message e)}"))
                            st
                          ) ; end lambda
                         ]) ; end handlers
           (run-turn! st user-msg d)
         ) ; end with-handlers
       ) ; end define st*
       (bus-drain! bus)                     ; 渲染订阅者处理完本轮
       ;; 持久化本轮新增的全部消息（user/assistant/tool-result 按序）+ usage 增量
       (persist-turn! sess st st*)
       (loop st*)
      ] ; end else
    ) ; end cond
  ) ; end let loop
) ; end define run-repl!

;; 持久化 st→st* 之间新增的历史消息与 usage 增量。
;; 这样 tool-result（内部 user 轮）也被完整落盘，保证 resume 的配对不破。
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

;; 多行输入：以 \ 结尾续行
(define (read-user-input)
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
) ; end define read-user-input

;; ---------------------------------------------------------------- 斜杠命令

;; 返回 (values new-state continue?)
(define (handle-command cmd st d sess)
  (define parts (string-split cmd))
  (define name (car parts))
  (define args (cdr parts))
  (case name
    [("/quit" "/exit" "/q") (values st #f)]
    [("/help")
     (displayln (dim (string-join
       '("commands:"
         "  /help            show this"
         "  /quit            exit (session saved)"
         "  /clear           clear conversation history"
         "  /usage           token usage so far"
         "  /compact         summarize old history to save context"
         "  /history         message count and roles"
         "  /model <id>      switch model"
        ) ; end list
       "\n"))
     ) ; end displayln
     (values st #t)
    ] ; end help case
    [("/clear")
     (displayln (dim "history cleared"))
     (values (make-initial-state (agent-state-config st)) #t)
    ] ; end clear case
    [("/usage")
     (define u (agent-state-token-usage st))
     (displayln f"tokens — input: {(usage-input-tokens u)}, output: {(usage-output-tokens u)}, turns: {(agent-state-turn-count st)}")
     (values st #t)
    ] ; end usage case
    [("/compact")
     (displayln (dim "compacting…"))
     (define st*
       (with-handlers ([exn:fail?
                        (lambda (e)
                          (displayln (red f"compact failed: {(exn-message e)}"))
                          st
                        ) ; end lambda
                       ]) ; end handlers
         (compact! st (deps-provider d))
       ) ; end with-handlers
     ) ; end define st*
     (displayln (dim f"history: {(pvector-length (agent-state-history st))} → {(pvector-length (agent-state-history st*))} messages"))
     (values st* #t)
    ] ; end compact case
    [("/history")
     (define hist (agent-state-history st))
     (displayln f"{(pvector-length hist)} messages:")
     (for ([m (in-pvector hist)] [i (in-naturals)])
       (define preview (message-text m))
       (displayln (dim f"  {i}. {(message-role m)}: {(substring preview 0 (min 50 (string-length preview)))}"))
     ) ; end for
     (values st #t)
    ] ; end history case
    [("/model")
     (cond
       [(null? args) (displayln (red "usage: /model <id>")) (values st #t)]
       [else
        (define cfg* (struct-copy config (agent-state-config st) [model (car args)]))
        (displayln (dim f"model → {(car args)}"))
        (values (struct-copy agent-state st [config cfg*]) #t)
       ] ; end else
     ) ; end cond
    ] ; end model case
    [else
     (displayln (red f"unknown command: {name} (try /help)"))
     (values st #t)
    ] ; end else
  ) ; end case
) ; end define handle-command

;; ---------------------------------------------------------------- provide

(provide
 run-repl!
 make-renderer
 tty-asker
 persist-turn!
) ; end provide
