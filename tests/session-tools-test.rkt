#lang tstring racket
;; session-tools-test.rkt — 会话的信息 / 改名 / 导出（design-workspace.md W1）：经 run-rpc! 跑一轮带工具的对话，
;; session_stats 的统计与会话记录一致；set_title 改名后会话列表用新名（当前会话与别的会话都行）；export 出 Markdown / JSON；会话目录外的路径被拒。
;; 会话列表的行带工作目录的 git 远端（W2）；不是仓库的目录 → #f。

(require rackunit json racket/async-channel racket/system (file "../src/session-tools.rkt")
         (file "../src/model.rkt") (file "../src/event.rkt") (file "../src/provider.rkt") (file "../src/tool.rkt")
         (file "../src/permission.rkt") (file "../src/loop.rkt") (file "../src/session.rkt") (file "../src/plugin.rkt")
         (file "../src/rpc.rkt") (file "../src/tools/builtin.rkt"))

(define dir (make-temporary-file "pi2-stools-~a" 'directory))
(void (putenv "PI_CONFIG_HOME" (path->string dir)))

(define prov
  (provider "s"
            (lambda (msgs _t)
              (define ch (make-async-channel))
              (define lm (last msgs))
              (if (ormap tool-result-block? (message-blocks lm))
                  (begin (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant "it printed hi")))
                         (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 30 4))))
                  (begin (async-channel-put ch (evt:message (now-ms) (message 'assistant (list (thinking-block "plan" "sig") (text-block "running") (tool-use-block "c1" "bash" (hasheq 'command "echo hi"))))))
                         (async-channel-put ch (evt:turn-end (now-ms) "tool_calls" (usage 20 3)))))
              ch)
            void))

(define git (find-executable-path "git"))
(test-case "git remote of a workdir: origin URL; not a repo → #f"
  (define d2 (make-temporary-file "pi2-norepo-~a" 'directory))
  (check-false (workdir-git-remote (path->string d2)))
  (check-false (workdir-git-remote "/nonexistent/dir"))
  (delete-directory d2))

(test-case "session_stats / set_title / export_session"
  (system* git "init" "-q" (path->string dir))
  (system* git "-C" (path->string dir) "remote" "add" "origin" "git@github.com:example/pi2.git")
  (define cfg (struct-copy config (default-config) [workdir (path->string dir)] [permission-mode 'yolo] [model "deepseek-v4-flash"]))
  (define other (fresh-session-path (path->string dir)))
  (let ([s (session-open! other cfg)]) (session-append-msg! s (text-msg 'user "older question")) (session-close! s))
  (define path (fresh-session-path (path->string dir)))
  (define sess (session-open! path cfg))
  (define host (make-plugin-host))
  (define d (make-deps #:provider prov #:registry (make-registry (builtin-tools cfg)) #:bus (make-bus) #:policy (make-policy cfg) #:plugin-host host))
  (define-values (in-r in-w) (make-pipe)) (define-values (out-r out-w) (make-pipe))
  (thread (lambda () (parameterize ([current-input-port in-r] [current-output-port out-w])
                       (run-rpc! d (make-initial-state cfg) sess #:plugin-host host #:fetch-models (lambda (_e) #f)))
            (close-output-port out-w)))
  (define (send! j) (write-string (jsexpr->string j) in-w) (newline in-w) (flush-output in-w))
  (define (read-type type) (let loop () (define j (string->jsexpr (sync/timeout 10 (read-line-evt out-r 'linefeed)))) (if (equal? (hash-ref j 'type #f) type) j (loop))))
  (send! (hasheq 'type "prompt" 'text "please echo hi"))
  (read-type "turn_complete")
  ;; 统计
  (send! (hasheq 'type "session_stats"))
  (define st (read-type "session_stats"))
  (check-equal? (hash-ref st 'title) "please echo hi")
  (check-equal? (list (hash-ref st 'messages) (hash-ref st 'user_messages) (hash-ref st 'requests)) '(4 1 2))
  (check-equal? (map (lambda (m) (list (hash-ref m 'model) (hash-ref m 'input) (hash-ref m 'output))) (hash-ref st 'models)) '(("deepseek-v4-flash" 50 7)))
  (check-true (> (hash-ref st 'cost) 0))
  (check-equal? (map (lambda (t) (list (hash-ref t 'name) (hash-ref t 'calls) (hash-ref t 'errors) (hash-ref t 'source))) (hash-ref st 'tools)) '(("bash" 1 0 "builtin")))
  (check-equal? (hash-ref st 'workdir) (path->string dir))
  (check-true (> (hash-ref st 'bytes) 0))
  ;; 改名：当前会话、别的会话
  (send! (hasheq 'type "set_title" 'title "  echo test  "))
  (check-equal? (hash-ref (read-type "ok") 'title) "echo test")
  (define ss (hash-ref (read-type "sessions") 'sessions))
  (check-equal? (hash-ref (findf (lambda (s) (hash-ref s 'current)) ss) 'title) "echo test")
  (check-equal? (for/list ([s (in-list ss)] #:when (string? (hash-ref s 'workdir))) (hash-ref s 'git_remote))   ; 配置目录里的 spend-log.rktd 也被列出（无 workdir）
                '("git@github.com:example/pi2.git" "git@github.com:example/pi2.git"))
  (send! (hasheq 'type "set_title" 'path (path->string other) 'title "renamed older"))
  (read-type "ok")
  (define ss2 (hash-ref (read-type "sessions") 'sessions))
  (check-equal? (hash-ref (findf (lambda (s) (equal? (hash-ref s 'path) (path->string other))) ss2) 'title) "renamed older")
  (check-equal? (hash-ref (findf (lambda (s) (equal? (hash-ref s 'path) (path->string other))) ss2) 'messages) 1)   ; 内容不变
  (send! (hasheq 'type "set_title" 'title ""))
  (check-regexp-match #rx"non-empty title" (hash-ref (read-type "error") 'message))
  ;; 导出
  (send! (hasheq 'type "export_session" 'format "markdown"))
  (define md (hash-ref (read-type "export") 'text))
  (check-regexp-match #rx"^# echo test" md)
  (check-regexp-match #rx"## User\n\nplease echo hi" md)
  (check-regexp-match #rx"\\*\\*Tool call\\*\\* `bash`" md)
  (check-regexp-match #rx"it printed hi" md)
  (send! (hasheq 'type "export_session" 'format "json"))
  (define js (string->jsexpr (hash-ref (read-type "export") 'text)))
  (check-equal? (map (lambda (i) (hash-ref i 'kind)) (filter (lambda (i) (member (hash-ref i 'kind) '("user" "assistant" "results"))) (hash-ref js 'items)))
                '("user" "assistant" "results" "assistant"))
  ;; 会话目录外的路径被拒
  (send! (hasheq 'type "session_stats" 'path "/etc/hosts"))
  (check-regexp-match #rx"not a session in this kernel" (hash-ref (read-type "error") 'message))
  (send! (hasheq 'type "shutdown")) (void (read-type "bye")))
