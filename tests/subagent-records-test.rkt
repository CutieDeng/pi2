#lang tstring racket
;; subagent-records-test.rkt — P5（design-content.md §3.2 / §5）：经 run-rpc! 跑真实的轮：
;; 本机子 agent 的过程作为 sub 记录挂在父工具调用（spawn_agent）下 → 条目里是 sub（内层条目照常投影）；
;; 重放（模型上下文）里没有子 agent 的消息；插件注册的工具在工具结束记录里标 plugin:<插件名>。

(require rackunit json racket/async-channel racket/pvector racket/runtime-path
         (file "../src/model.rkt") (file "../src/event.rkt") (file "../src/provider.rkt") (file "../src/tool.rkt")
         (file "../src/permission.rkt") (file "../src/loop.rkt") (file "../src/session.rkt") (file "../src/plugin.rkt")
         (file "../src/rpc.rkt") (file "../src/subagent.rkt") (file "../src/tools/builtin.rkt"))

(define dir (make-temporary-file "pi2-subrec-~a" 'directory))
(void (putenv "PI_CONFIG_HOME" (path->string dir)))
(define-runtime-path plugins-dir "../examples/plugins")

;; 脚本模型：父 → 调 spawn_agent；子（系统提示词是子 agent 的）→ 先调 echo，拿到结果后作答；父拿到子的结果 → 作答
(define (sub? msgs) (for/or ([m msgs]) (regexp-match? #rx"^count the files" (message-text m))))
(define (reply ch . evs) (for ([e evs]) (async-channel-put ch e)) ch)
(define prov
  (provider "s"
            (lambda (msgs _t)
              (define ch (make-async-channel))
              (define lm (last msgs))
              (define result? (ormap tool-result-block? (message-blocks lm)))
              (cond
                [(and (sub? msgs) (not result?))
                 (reply ch (evt:message (now-ms) (message 'assistant (list (text-block "checking") (tool-use-block "s1" "echo" (hasheq 'text "from sub")))))
                        (evt:turn-end (now-ms) "tool_calls" (usage 3 2)))]
                [(sub? msgs) (reply ch (evt:message (now-ms) (text-msg 'assistant "sub says: 3 files")) (evt:turn-end (now-ms) "stop" (usage 3 2)))]
                [(not result?)
                 (reply ch (evt:message (now-ms) (message 'assistant (list (tool-use-block "p1" "spawn_agent" (hasheq 'task "count the files")))))
                        (evt:turn-end (now-ms) "tool_calls" (usage 5 2)))]
                [else (reply ch (evt:message (now-ms) (text-msg 'assistant "parent: done")) (evt:turn-end (now-ms) "stop" (usage 5 2)))]))
            void))

(test-case "local sub-agent: its records nest under the spawn_agent call; replay has only the parent's messages; plugin tools are marked"
  (define cfg (struct-copy config (default-config) [workdir (path->string dir)] [permission-mode 'yolo]))
  (define path (fresh-session-path (path->string dir)))
  (define sess (session-open! path cfg))
  (define host (make-plugin-host))
  (load-plugin-trusted! host (build-path plugins-dir "echo-tool.rkt"))
  (define echo (host-lookup host "echo"))
  (define spawn (make-spawn-agent-tool #:provider prov #:sub-tools (list echo)))
  (define d (make-deps #:provider prov #:registry (make-registry (list spawn echo)) #:bus (make-bus) #:policy (make-policy cfg) #:plugin-host host))
  (define-values (in-r in-w) (make-pipe)) (define-values (out-r out-w) (make-pipe))
  (thread (lambda () (parameterize ([current-input-port in-r] [current-output-port out-w])
                       (run-rpc! d (make-initial-state cfg) sess #:plugin-host host #:fetch-models (lambda (_e) #f)))
            (close-output-port out-w)))
  (define (send! j) (write-string (jsexpr->string j) in-w) (newline in-w) (flush-output in-w))
  (define seen '())
  (define (read-type type) (let loop () (define j (string->jsexpr (sync/timeout 10 (read-line-evt out-r 'linefeed)))) (set! seen (cons j seen)) (if (equal? (hash-ref j 'type #f) type) j (loop))))
  (send! (hasheq 'type "prompt" 'text "how many files?"))
  (read-type "turn_complete")
  ;; 实时条目：sub 挂在 p1 下；内层有子 agent 的任务、回答（带 echo 调用）、echo 的结果、最后的回答
  (define subs (filter (lambda (j) (equal? (hash-ref j 'kind #f) "sub")) (reverse seen)))
  (check-true (andmap (lambda (j) (equal? (hash-ref j 'parent) "p1")) subs))
  (check-equal? (map (lambda (j) (hash-ref (hash-ref j 'inner) 'kind)) subs) '("user" "usage" "assistant" "tool" "results" "usage" "assistant"))
  (define sub-tool (findf (lambda (j) (equal? (hash-ref (hash-ref j 'inner) 'kind) "tool")) subs))
  (check-equal? (hash-ref (hash-ref sub-tool 'inner) 'source) "plugin:echo-tool")
  ;; 父会话的条目序列：子 agent 的不在顶层
  (check-equal? (for/list ([j (reverse seen)] #:when (and (equal? (hash-ref j 'type #f) "item") (not (equal? (hash-ref j 'kind) "sub")))) (hash-ref j 'kind))
                '("user" "usage" "assistant" "tool" "results" "usage" "assistant"))
  ;; 重放：模型上下文里只有父的 4 条消息
  (send! (hasheq 'type "shutdown")) (void (read-type "bye"))
  (define st (session-replay path))
  (check-equal? (for/list ([m (in-pvector (agent-state-history st))])
                  (define b (car (message-blocks m)))
                  (cond [(tool-result-block? b) (list 'result (tool-result-block-content b))] [(tool-use-block? b) (list 'call (tool-use-block-name b))] [else (message-text m)]))
                '("how many files?" (call "spawn_agent") (result "sub says: 3 files") "parent: done"))
  ;; 翻页取回的与实时的一样
  (define sess2 (session-open! path cfg))
  (check-equal? (length (filter (lambda (pr) (eq? (rec-type (cdr pr)) 'sub)) (session-read-range sess2 0 1000))) (length subs))
  (session-close! sess2))
