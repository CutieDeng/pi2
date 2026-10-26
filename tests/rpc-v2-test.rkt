#lang tstring racket
;; rpc-v2-test.rkt — RPC v2（design-gui.md R1–R4、R9）：经管道逐条收发，控制时序。
;;   R1 轮中 interrupt；R2 等审批时其它请求不丢；R3 会话管理；R4 结构化历史；R9 客户端 EOF 时跑完当前轮再退出。

(require
 rackunit
 json
 racket/async-channel
 racket/file
 racket/port
 racket/pvector
 (file "../src/model.rkt")
 (file "../src/event.rkt")
 (file "../src/provider.rkt")
 (file "../src/tool.rkt")
 (file "../src/permission.rkt")
 (file "../src/loop.rkt")
 (file "../src/session.rkt")
 (file "../src/plugin.rkt")
 (file "../src/providers.rkt")
 (file "../src/rpc.rkt")
 (file "../src/tools/builtin.rkt")
) ; end require

(void (putenv "PI_CONFIG_HOME" (path->string (make-temporary-file "pi2-rpcv2cfg-~a" (quote directory)))))

;; ---------------------------------------------------------------- 假 provider

;; 按脚本逐次回复；每项：(list 'text s) | (list 'tool id name input) | (list 'hang)（发一个 delta 后挂住直到 cancel）
;; | (list 'slow ms s)（延迟后回复文本）。cancelled：cancel 被调用的次数。
(define last-window (box '()))                                ; provider 最近一次收到的消息窗口

(define (scripted-provider steps)
  (define script (box steps))
  (define cancelled (box 0))
  (define cancel-sema (make-semaphore 0))
  (define p
    (provider "scripted"
      (lambda (msgs _tools)
        (set-box! last-window msgs)
        (define ch (make-async-channel))
        (define step (if (null? (unbox script)) (list 'text "ok") (car (unbox script))))
        (unless (null? (unbox script)) (set-box! script (cdr (unbox script))))
        (thread
         (lambda ()
           (case (car step)
             [(text)
              (async-channel-put ch (evt:delta (now-ms) 'text (cadr step)))
              (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant (cadr step))))
              (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 2 2)))]
             [(tool)
              (async-channel-put ch (evt:message (now-ms) (message 'assistant (list (apply tool-use-block (cdr step))))))
              (async-channel-put ch (evt:turn-end (now-ms) "tool_calls" (usage 1 1)))]
             [(hang)
              (async-channel-put ch (evt:delta (now-ms) 'text "partial"))
              (semaphore-wait cancel-sema)]
             [(slow)
              (sleep (/ (cadr step) 1000.0))
              (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant (caddr step))))
              (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 1 1)))])))
        ch)
      (lambda () (set-box! cancelled (add1 (unbox cancelled))) (semaphore-post cancel-sema))))
  (values p cancelled)
) ; end define scripted-provider

;; ---------------------------------------------------------------- 管道驱动

(struct rig (send! next! expect! close! thread dir sess-path))

(define (start-rpc prov #:mode [mode 'yolo] #:tools? [tools? #f]
                   #:fetch-models [fetch-models (lambda (_ep) #f)])          ; 默认不碰真实 LM Studio
  (define dir (make-temporary-file "pi2-rpcv2-~a" 'directory))
  (define cfg (struct-copy config (default-config) [workdir (path->string dir)] [permission-mode mode]))
  (define host (make-plugin-host))
  (register-builtin-providers! host)
  (define d (make-deps #:provider prov #:registry (make-registry (if tools? (builtin-tools cfg) '()))
                       #:bus (make-bus) #:policy (make-policy cfg) #:plugin-host host))
  (define sess-path (fresh-session-path (path->string dir)))
  (define sess (session-open! sess-path cfg))
  (define-values (in-r in-w) (make-pipe))
  (define-values (out-r out-w) (make-pipe))
  (define th (thread (lambda ()
                       (parameterize ([current-input-port in-r] [current-output-port out-w])
                         (run-rpc! d (make-initial-state cfg) sess #:plugin-host host #:fetch-models fetch-models))
                       (close-output-port out-w))))
  (define (send! j) (write-string (jsexpr->string j) in-w) (newline in-w) (flush-output in-w))
  (define (next! [timeout 5])
    (define l (sync/timeout timeout (read-line-evt out-r 'linefeed)))
    (cond [(string? l) (string->jsexpr l)] [else l]))       ; #f = 超时；eof
  ;; 读到 type 为止；返回 (values 该事件 途经的事件)
  (define (expect! type [timeout 5])
    (let loop ([seen '()])
      (define j (next! timeout))
      (cond
        [(not (hash? j)) (fail (format "timed out / eof waiting for ~a; saw ~a" type (map (lambda (x) (hash-ref x 'type #f)) (reverse seen))))
                         (values #f (reverse seen))]
        [(equal? (hash-ref j 'type #f) type) (values j (reverse seen))]
        [else (loop (cons j seen))])))
  (rig send! next! expect! (lambda () (close-output-port in-w)) th dir sess-path)
) ; end define start-rpc

(define (types js) (map (lambda (j) (hash-ref j 'type #f)) js))

;; ---------------------------------------------------------------- R1 中断

(test-case "R1: interrupt stops a hanging turn; provider cancel is called; state rolls back"
  (define-values (p cancelled) (scripted-provider (list (list 'hang))))
  (define r (start-rpc p))
  ((rig-expect! r) "ready")
  ((rig-send! r) (hasheq 'type "prompt" 'text "long task"))
  ((rig-expect! r) "turn_start")
  ((rig-expect! r) "delta")
  ((rig-send! r) (hasheq 'type "interrupt"))
  (define-values (ok _s1) ((rig-expect! r) "ok"))
  (check-equal? (hash-ref ok 'for) "interrupt")
  (check-true (hash-ref ok 'running))
  (define-values (tc _s2) ((rig-expect! r) "turn_complete"))
  (check-true (hash-ref tc 'interrupted))
  (check-equal? (hash-ref tc 'outcome) "interrupted")
  (check-equal? (unbox cancelled) 1)
  (check-equal? (hash-ref tc 'kept_messages) 2)            ; 用户消息 + <turn_aborted> 标记；半截文本丢弃
  ((rig-send! r) (hasheq 'type "history"))
  (define-values (h _s3) ((rig-expect! r) "history"))
  (check-equal? (map (lambda (m) (hash-ref m 'role)) (hash-ref h 'messages)) '("user" "user"))
  (check-regexp-match #rx"^<turn_aborted>" (hash-ref (second (hash-ref h 'messages)) 'text))
  ((rig-send! r) (hasheq 'type "shutdown"))
  ((rig-expect! r) "bye") (void))

(test-case "R1: interrupt with no turn running is acknowledged with running=false"
  (define-values (p _c) (scripted-provider '()))
  (define r (start-rpc p))
  ((rig-expect! r) "ready")
  ((rig-send! r) (hasheq 'type "interrupt"))
  (define-values (ok _s) ((rig-expect! r) "ok"))
  (check-false (hash-ref ok 'running))
  ((rig-send! r) (hasheq 'type "shutdown"))
  ((rig-expect! r) "bye") (void))

(test-case "during a turn: ping / list_sessions answer immediately, other requests queue until it ends"
  (define-values (p _c) (scripted-provider (list (list 'hang))))
  (define r (start-rpc p))
  ((rig-expect! r) "ready")
  ((rig-send! r) (hasheq 'type "prompt" 'text "x"))
  ((rig-expect! r) "delta")
  ((rig-send! r) (hasheq 'type "state"))
  ((rig-send! r) (hasheq 'type "ping"))
  (define-values (_p seen) ((rig-expect! r) "pong"))
  (check-false (member "state" (types seen)))              ; state 排着队
  ((rig-send! r) (hasheq 'type "list_sessions"))
  ((rig-expect! r) "sessions")
  ((rig-send! r) (hasheq 'type "interrupt"))
  (define-values (_t before) ((rig-expect! r) "turn_complete"))
  (check-false (member "state" (types before)))
  ((rig-expect! r) "state")                                 ; 轮结束后才应答
  ((rig-send! r) (hasheq 'type "shutdown"))
  ((rig-expect! r) "bye") (void))

;; ---------------------------------------------------------------- R2 审批期间不丢请求

(test-case "R2: while waiting for permission, other requests are kept; permission carries an id"
  (define-values (p _c) (scripted-provider (list (list 'tool "c1" "bash" (hasheq 'command "echo hi")) (list 'text "done"))))
  (define r (start-rpc p #:mode 'normal #:tools? #t))
  ((rig-expect! r) "ready")
  ((rig-send! r) (hasheq 'type "prompt" 'text "run it"))
  (define-values (pr _s) ((rig-expect! r) "permission_request"))
  (check-true (exact-positive-integer? (hash-ref pr 'id)))
  (check-equal? (hash-ref pr 'tool) "bash")                 ; 结构化字段
  (check-equal? (hash-ref pr 'level) "dangerous")
  (check-equal? (hash-ref (hash-ref pr 'input) 'command) "echo hi")
  ((rig-send! r) (hasheq 'type "state"))                    ; v1 会把这一行吞掉
  ((rig-send! r) (hasheq 'type "permission" 'id 999 'decision "no"))   ; 过期 id：忽略
  ((rig-send! r) (hasheq 'type "permission" 'id (hash-ref pr 'id) 'decision "yes"))
  (define-values (te _s2) ((rig-expect! r) "tool_end"))
  (check-false (hash-ref te 'is_error))
  ((rig-expect! r) "turn_complete")
  (define-values (st _s3) ((rig-expect! r) "state"))
  (check-equal? (hash-ref st 'messages) 4)                  ; user, assistant(tool_use), user(tool_result), assistant
  ((rig-send! r) (hasheq 'type "shutdown"))
  ((rig-expect! r) "bye") (void))

;; ---------------------------------------------------------------- R4 结构化历史

(test-case "R4: history carries tool_use / tool_result blocks"
  (define-values (p _c) (scripted-provider (list (list 'tool "c7" "bash" (hasheq 'command "echo structured")) (list 'text "fine"))))
  (define r (start-rpc p #:tools? #t))
  ((rig-expect! r) "ready")
  ((rig-send! r) (hasheq 'type "prompt" 'text "go"))
  ((rig-expect! r) "turn_complete")
  ((rig-send! r) (hasheq 'type "history"))
  (define-values (h _s) ((rig-expect! r) "history"))
  (define blocks (append* (map (lambda (m) (hash-ref m 'blocks)) (hash-ref h 'messages))))
  (define use (findf (lambda (b) (equal? (hash-ref b 'type) "tool_use")) blocks))
  (define res (findf (lambda (b) (equal? (hash-ref b 'type) "tool_result")) blocks))
  (check-equal? (hash-ref use 'name) "bash")
  (check-equal? (hash-ref (hash-ref use 'input) 'command) "echo structured")
  (check-equal? (hash-ref res 'tool_use_id) "c7")
  (check-regexp-match #rx"structured" (hash-ref res 'content))
  ((rig-send! r) (hasheq 'type "shutdown"))
  ((rig-expect! r) "bye") (void))

(test-case "tool_end carries a unified diff for file writes and edits (display only, not in history)"
  (define-values (p _c) (scripted-provider
                         (list (list 'tool "w1" "write_file" (hasheq 'path "a.txt" 'content "one\ntwo\nthree\n"))
                               (list 'tool "e1" "edit_file" (hasheq 'path "a.txt" 'old_string "two" 'new_string "TWO"))
                               (list 'text "done"))))
  (define r (start-rpc p #:tools? #t))
  ((rig-expect! r) "ready")
  ((rig-send! r) (hasheq 'type "prompt" 'text "go"))
  (define-values (_tc seen) ((rig-expect! r) "turn_complete"))
  (define ends (filter (lambda (j) (equal? (hash-ref j 'type #f) "tool_end")) seen))
  (check-equal? (map (lambda (j) (hash-ref j 'name)) ends) '("write_file" "edit_file"))
  (define d1 (hash-ref (car ends) 'diff))
  (check-equal? (list (hash-ref d1 'path) (hash-ref d1 'added) (hash-ref d1 'removed)) '("a.txt" 3 0))
  (define d2 (hash-ref (cadr ends) 'diff))
  (check-equal? (hash-ref d2 'text) "@@ -1,3 +1,3 @@\n one\n-two\n+TWO\n three")
  ((rig-send! r) (hasheq 'type "history"))
  (define-values (h _s) ((rig-expect! r) "history"))
  ;; diff 只挂在 tool_result 的 diff 字段上（界面用），文本内容里没有（模型看不到）
  (define results (filter (lambda (b) (equal? (hash-ref b 'type) "tool_result"))
                          (append* (map (lambda (m) (hash-ref m 'blocks)) (hash-ref h 'messages)))))
  (check-equal? (map (lambda (b) (hash-ref (hash-ref b 'diff) 'added)) results) '(3 1))
  (check-false (for/or ([b results]) (regexp-match? #rx"@@" (hash-ref b 'content))))
  ;; 落盘：新会话再切回来，diff 仍在；分叉也带上
  (define path (rig-sess-path r))
  ((rig-send! r) (hasheq 'type "new_session"))
  ((rig-expect! r) "session")
  ((rig-send! r) (hasheq 'type "resume" 'path (path->string path)))
  (define-values (sv _s2) ((rig-expect! r) "session"))
  ;; RPC v3：session 事件带尾部条目；diff 在工具结束条目（tool）里，分叉出的会话里是单独的 diff 条目
  (define (diff-ids ev) (for/list ([i (hash-ref ev 'items)]
                                   #:when (or (and (equal? (hash-ref i 'kind) "tool") (hash? (hash-ref i 'diff #f))) (equal? (hash-ref i 'kind) "diff")))
                          (hash-ref i 'call_id)))
  (check-equal? (diff-ids sv) '("w1" "e1"))
  ((rig-send! r) (hasheq 'type "fork"))
  (define-values (fv _s3) ((rig-expect! r) "session"))
  (check-equal? (diff-ids fv) '("w1" "e1"))
  ((rig-send! r) (hasheq 'type "shutdown"))
  ((rig-expect! r) "bye") (void))

(test-case "interrupt: diffs of completed tools are persisted, aborted calls have none"
  (define-values (p _c) (scripted-provider
                         (list (list 'tool "w1" "write_file" (hasheq 'path "k.txt" 'content "kept\n"))
                               (list 'hang))))
  (define r (start-rpc p #:tools? #t))
  ((rig-expect! r) "ready")
  ((rig-send! r) (hasheq 'type "prompt" 'text "go"))
  ((rig-expect! r) "tool_end")
  ((rig-expect! r) "delta")
  ((rig-send! r) (hasheq 'type "interrupt"))
  ((rig-expect! r) "turn_complete")
  ((rig-send! r) (hasheq 'type "history"))
  (define-values (h _s) ((rig-expect! r) "history"))
  (check-equal? (for*/list ([m (hash-ref h 'messages)] [b (hash-ref m 'blocks)] #:when (hash-ref b 'diff #f)) (hash-ref b 'tool_use_id))
                '("w1"))
  ((rig-send! r) (hasheq 'type "shutdown"))
  ((rig-expect! r) "bye") (void))

(test-case "list_providers: provider list with models / key status / reasoning levels; answered during a turn"
  (define-values (p _c) (scripted-provider (list (list 'hang))))
  (define r (start-rpc p))
  ((rig-expect! r) "ready")
  ((rig-send! r) (hasheq 'type "set_reasoning" 'level "high"))
  ((rig-expect! r) "ok")
  ((rig-send! r) (hasheq 'type "prompt" 'text "x"))
  ((rig-expect! r) "turn_start")
  ((rig-send! r) (hasheq 'type "list_providers"))                          ; 轮在跑：仍立即应答
  (define-values (pv _s) ((rig-expect! r) "providers"))
  (check-equal? (hash-ref pv 'reasoning) "high")
  (check-equal? (hash-ref pv 'reasoning_levels) '("off" "low" "medium" "high" "max"))
  (define lm (findf (lambda (x) (equal? (hash-ref x 'name) "lmstudio")) (hash-ref pv 'providers)))
  (check-true (hash-ref lm 'has_key))                                       ; 本地 LM Studio 不需要密钥
  (check-equal? (car (hash-ref lm 'models)) "gemma-4-31b-it@6bit")
  (check-true (string? (hash-ref pv 'current)))
  ((rig-send! r) (hasheq 'type "interrupt"))
  ((rig-expect! r) "turn_complete")
  ((rig-send! r) (hasheq 'type "set_reasoning" 'level "off"))
  ((rig-expect! r) "ok")
  ((rig-send! r) (hasheq 'type "shutdown"))
  ((rig-expect! r) "bye") (void))

(test-case "list_providers: local endpoint models arrive in a second providers event (cached afterwards)"
  (define calls (box 0))
  (define gate (make-semaphore 0))
  (define (fake ep)
    (set-box! calls (add1 (unbox calls)))
    (semaphore-wait gate)                                                  ; 查询「很慢」：第一次应答不能等它
    (and (regexp-match? #rx"localhost:1234" ep) (cons '("qwen/qwen3.5-9b" "gemma-4-31b-it@6bit") '("qwen/qwen3.5-9b"))))
  (define-values (p _c) (scripted-provider '()))
  (define r (start-rpc p #:fetch-models fake))
  ((rig-expect! r) "ready")
  ((rig-send! r) (hasheq 'type "list_providers"))
  (define-values (p1 _s1) ((rig-expect! r) "providers"))                   ; 立即应答（查询还卡着）
  (define (lm pv) (findf (lambda (x) (equal? (hash-ref x 'name) "lmstudio")) (hash-ref pv 'providers)))
  (check-equal? (hash-ref (lm p1) 'models) '("gemma-4-31b-it@6bit"))
  ((rig-send! r) (hasheq 'type "ping"))                                     ; 主循环没被查询卡住
  ((rig-expect! r) "pong")
  (semaphore-post gate)
  (define-values (p2 _s2) ((rig-expect! r) "providers"))
  (check-equal? (hash-ref (lm p2) 'models) '("gemma-4-31b-it@6bit" "qwen/qwen3.5-9b"))
  (check-equal? (hash-ref (lm p2) 'loaded) '("qwen/qwen3.5-9b"))
  ((rig-send! r) (hasheq 'type "list_providers"))                          ; 之后：缓存立即可用
  (semaphore-post gate)
  (define-values (p3 _s3) ((rig-expect! r) "providers"))
  (check-equal? (hash-ref (lm p3) 'loaded) '("qwen/qwen3.5-9b"))
  ((rig-send! r) (hasheq 'type "shutdown"))
  ((rig-expect! r) "bye") (void))

;; ---------------------------------------------------------------- R3 会话管理

(test-case "R3: list / new / fork / resume / delete; current session cannot be deleted"
  (define-values (p _c) (scripted-provider (list (list 'text "one") (list 'text "two"))))
  (define r (start-rpc p))
  (define-values (ready _s0) ((rig-expect! r) "ready"))
  (define first-path (hash-ref ready 'session))
  ((rig-send! r) (hasheq 'type "prompt" 'text "first"))
  ((rig-expect! r) "turn_complete")
  ((rig-send! r) (hasheq 'type "delete_session" 'path first-path))
  (define-values (err _s1) ((rig-expect! r) "error"))
  (check-regexp-match #rx"current session" (hash-ref err 'message))
  ;; 分叉：新会话带着同样的 2 条消息
  ((rig-send! r) (hasheq 'type "fork"))
  (define-values (fk _s2) ((rig-expect! r) "session"))
  (check-not-equal? (hash-ref fk 'path) first-path)
  (check-equal? (hash-ref fk 'messages) 2)
  (check-equal? (map (lambda (i) (hash-ref i 'kind)) (hash-ref fk 'items)) '("user" "assistant"))
  ;; 新会话：空
  ((rig-send! r) (hasheq 'type "new_session"))
  (define-values (nw _s3) ((rig-expect! r) "session"))
  (check-equal? (hash-ref nw 'messages) 0)
  ((rig-send! r) (hasheq 'type "prompt" 'text "second"))
  ((rig-expect! r) "turn_complete")
  ;; 列表：3 个会话，当前的标记 current
  ((rig-send! r) (hasheq 'type "list_sessions"))
  (define-values (ls _s4) ((rig-expect! r) "sessions"))
  (define ss (hash-ref ls 'sessions))
  (check-equal? (length ss) 3)
  (check-equal? (for/sum ([s ss]) (if (hash-ref s 'current) 1 0)) 1)
  (check-equal? (hash-ref (findf (lambda (s) (hash-ref s 'current)) ss) 'path) (hash-ref nw 'path))
  ;; 恢复第一个会话；历史回来了
  ((rig-send! r) (hasheq 'type "resume" 'path first-path))
  (define-values (rs _s5) ((rig-expect! r) "session"))
  (check-equal? (hash-ref rs 'messages) 2)
  (check-equal? (hash-ref (car (hash-ref (car (hash-ref rs 'items)) 'parts)) 'text) "first")
  ;; 删掉非当前的（新会话）
  ((rig-send! r) (hasheq 'type "delete_session" 'path (hash-ref nw 'path)))
  ((rig-expect! r) "ok")
  (check-false (file-exists? (hash-ref nw 'path)))
  ((rig-send! r) (hasheq 'type "resume" 'path "/nonexistent/x.rktd"))
  ((rig-expect! r) "error")
  ((rig-send! r) (hasheq 'type "shutdown"))
  ((rig-expect! r) "bye") (void))

(test-case "clear resets history; compact runs as an interruptible job and reports message count"
  (define-values (p _c) (scripted-provider '()))
  (define r (start-rpc p))
  ((rig-expect! r) "ready")
  (for ([i 4]) ((rig-send! r) (hasheq 'type "prompt" 'text (format "q~a" i))) ((rig-expect! r) "turn_complete"))
  ((rig-send! r) (hasheq 'type "compact"))
  (define-values (ok _s) ((rig-expect! r) "ok"))
  (check-equal? (hash-ref ok 'for) "compact")
  (check-equal? (hash-ref ok 'outcome) "ok")
  (check-true (< (hash-ref ok 'messages) 8))
  ((rig-send! r) (hasheq 'type "clear"))
  ((rig-expect! r) "ok")
  ((rig-send! r) (hasheq 'type "state"))
  (define-values (st _s2) ((rig-expect! r) "state"))
  (check-equal? (hash-ref st 'messages) 0)
  ((rig-send! r) (hasheq 'type "shutdown"))
  ((rig-expect! r) "bye") (void))

;; ---------------------------------------------------------------- R9 客户端退出

(test-case "R9: client EOF mid-turn → the turn finishes, is persisted, then the server exits"
  (define-values (p _c) (scripted-provider (list (list 'slow 300 "late answer"))))
  (define r (start-rpc p))
  ((rig-expect! r) "ready")
  ((rig-send! r) (hasheq 'type "prompt" 'text "please persist me"))
  ((rig-expect! r) "turn_start")
  ((rig-close! r))                                           ; GUI 退出：stdin EOF
  (check-not-false (sync/timeout 5 (rig-thread r)))         ; run-rpc! 自行返回
  (define st (session-replay (rig-sess-path r)))
  (check-equal? (map message-role (pvector->list (agent-state-history st))) '(user assistant))
  (check-equal? (message-text (pvector-ref (agent-state-history st) 1)) "late answer"))

(test-case "unparsable line yields an error event, server keeps going"
  (define-values (p _c) (scripted-provider '()))
  (define r (start-rpc p))
  ((rig-expect! r) "ready")
  ((rig-send! r) "not json at all")
  (define-values (e _s) ((rig-expect! r) "error"))
  (check-regexp-match #rx"unparsable|unknown" (hash-ref e 'message))
  ((rig-send! r) (hasheq 'type "ping"))
  ((rig-expect! r) "pong")
  ((rig-send! r) (hasheq 'type "shutdown"))
  ((rig-expect! r) "bye") (void))

(test-case "event order: the assistant message that asks for a tool precedes its permission_request"
  ;; 每轮：工具调用 →（拒绝后）文本收尾；脚本按轮重复 5 次
  (define-values (p _c) (scripted-provider (append* (for/list ([i 5]) (list (list 'tool (format "c~a" i) "bash" (hasheq 'command "echo order"))
                                                                            (list 'text "done"))))))
  (define r (start-rpc p #:mode 'normal #:tools? #t))
  ((rig-expect! r) "ready")
  (for ([i 5])                                              ; 竞争条件：多跑几次
    ((rig-send! r) (hasheq 'type "prompt" 'text "go"))
    (define-values (pr seen) ((rig-expect! r) "permission_request"))
    (check-not-false (member "message" (types seen)) (format "round ~a saw ~a" i (types seen)))
    (check-not-false (member "turn_end" (types seen)))
    ((rig-send! r) (hasheq 'type "permission" 'id (hash-ref pr 'id) 'decision "no"))
    ((rig-expect! r) "turn_complete"))
  ((rig-send! r) (hasheq 'type "shutdown"))
  ((rig-expect! r) "bye")
  (void))


;; ---------------------------------------------------------------- 中断语义：保留已完成步骤（参照 Codex）

(define (history-of r)
  ((rig-send! r) (hasheq 'type "history"))
  (define-values (h _s) ((rig-expect! r) "history"))
  (hash-ref h 'messages))
(define (blocks-of m t) (filter (lambda (b) (equal? (hash-ref b 'type) t)) (hash-ref m 'blocks)))

(test-case "interrupt while a tool runs: earlier steps kept, running call gets 'aborted by user after Xs', marker appended"
  (define-values (p _c) (scripted-provider (list (list 'tool "c1" "bash" (hasheq 'command "echo step-one"))
                                                 (list 'tool "c2" "bash" (hasheq 'command "sleep 5")))))
  (define r (start-rpc p #:tools? #t))                        ; yolo：不询问
  ((rig-expect! r) "ready")
  ((rig-send! r) (hasheq 'type "prompt" 'text "do two things"))
  ((rig-expect! r) "tool_end")                                ; 第一步完成
  (let loop () (define-values (ts _s) ((rig-expect! r) "tool_start")) (unless (equal? (hash-ref ts 'id) "c2") (loop)))
  (sleep 0.3)                                                 ; 让 sleep 5 真正跑起来
  ((rig-send! r) (hasheq 'type "interrupt"))
  (define-values (tc _s2) ((rig-expect! r) "turn_complete"))
  (check-true (hash-ref tc 'interrupted))
  (check-equal? (hash-ref tc 'kept_messages) 6)
  (define ms (history-of r))
  (check-equal? (map (lambda (m) (hash-ref m 'role)) ms) '("user" "assistant" "user" "assistant" "user" "user"))
  (check-regexp-match #rx"step-one" (hash-ref (car (blocks-of (third ms) "tool_result")) 'content))   ; 已完成的结果保留
  (define aborted (car (blocks-of (fifth ms) "tool_result")))
  (check-equal? (hash-ref aborted 'tool_use_id) "c2")
  (check-true (hash-ref aborted 'is_error))
  (check-regexp-match #rx"^aborted by user after [0-9.]+s$" (hash-ref aborted 'content))
  (check-regexp-match #rx"^<turn_aborted>" (hash-ref (sixth ms) 'text))
  ;; 下一轮：模型收到的窗口里有标记（模型可见）
  ((rig-send! r) (hasheq 'type "prompt" 'text "ok, continue differently"))
  ((rig-expect! r) "turn_complete")
  (check-not-false (for/or ([m (in-list (unbox last-window))]) (regexp-match? #rx"<turn_aborted>" (message-text m))))
  ((rig-send! r) (hasheq 'type "shutdown"))
  ((rig-expect! r) "bye")
  (void))

(test-case "interrupt while waiting for permission: the call gets 'aborted before dispatch' + marker"
  (define-values (p _c) (scripted-provider (list (list 'tool "c1" "bash" (hasheq 'command "echo needs-approval")))))
  (define r (start-rpc p #:mode 'normal #:tools? #t))
  ((rig-expect! r) "ready")
  ((rig-send! r) (hasheq 'type "prompt" 'text "run it"))
  ((rig-expect! r) "permission_request")
  ((rig-send! r) (hasheq 'type "interrupt"))
  (define-values (tc _s) ((rig-expect! r) "turn_complete"))
  (check-equal? (hash-ref tc 'kept_messages) 4)              ; user, assistant(tool_use), user(aborted), user(marker)
  (define ms (history-of r))
  (check-equal? (hash-ref (car (blocks-of (third ms) "tool_result")) 'content) "aborted before dispatch")
  (check-regexp-match #rx"^<turn_aborted>" (hash-ref (fourth ms) 'text))
  ((rig-send! r) (hasheq 'type "shutdown"))
  ((rig-expect! r) "bye")
  (void))

(test-case "interrupted turn is persisted: resume sees the kept steps and the marker"
  (define-values (p _c) (scripted-provider (list (list 'hang))))
  (define r (start-rpc p))
  ((rig-expect! r) "ready")
  ((rig-send! r) (hasheq 'type "prompt" 'text "keep me"))
  ((rig-expect! r) "delta")
  ((rig-send! r) (hasheq 'type "interrupt"))
  ((rig-expect! r) "turn_complete")
  ((rig-send! r) (hasheq 'type "shutdown"))
  ((rig-expect! r) "bye")
  (sync/timeout 5 (rig-thread r))
  (define st (session-replay (rig-sess-path r)))
  (check-equal? (map message-text (pvector->list (agent-state-history st)))
                (list "keep me" TURN-ABORTED-MARKER)))

(test-case "R3: a new empty session is marked ephemeral; fork on it is refused (nothing to fork) and the kernel keeps running"
  (define-values (p _c) (scripted-provider (list (list 'text "one"))))
  (define r (start-rpc p))
  (define-values (ready _s0) ((rig-expect! r) "ready"))
  ((rig-send! r) (hasheq 'type "list_sessions"))
  (define-values (ls0 _s1) ((rig-expect! r) "sessions"))
  (define cur0 (findf (lambda (s) (hash-ref s 'current)) (hash-ref ls0 'sessions)))
  (check-true (hash-ref cur0 'ephemeral))
  ((rig-send! r) (hasheq 'type "fork"))
  (define-values (err _s2) ((rig-expect! r) "error"))
  (check-regexp-match #rx"nothing to fork" (hash-ref err 'message))
  ((rig-send! r) (hasheq 'type "ping"))
  ((rig-expect! r) "pong")                                                     ; 内核还活着
  ((rig-send! r) (hasheq 'type "prompt" 'text "hello"))
  ((rig-expect! r) "turn_complete")
  ((rig-send! r) (hasheq 'type "list_sessions"))
  (define-values (ls1 _s3) ((rig-expect! r) "sessions"))
  (check-false (hash-ref (findf (lambda (s) (hash-ref s 'current)) (hash-ref ls1 'sessions)) 'ephemeral))   ; 有消息了：不再临时
  ((rig-send! r) (hasheq 'type "fork"))
  (define-values (fk _s4) ((rig-expect! r) "session"))
  (check-equal? (hash-ref fk 'messages) 2)
  ((rig-send! r) (hasheq 'type "shutdown"))
  ((rig-expect! r) "bye") (void))
