#lang tstring racket
;; auto-title-test.rkt — 自动起标题（design-workspace.md §11.4 B），经 run-rpc!（本机模型注入）：
;; 没有标题的会话 → auto_title → 模型回答去掉引号 / 只取第一行 → 写成自动标题、会话列表用它、titled；
;; 之后手动改名 → 再 auto_title 被跳过（手动的不被盖）；非本机端点被拒；给模型的是前几条消息、要求只回标题。

(require rackunit json racket/async-channel racket/file
         (file "../src/model.rkt") (file "../src/event.rkt") (file "../src/provider.rkt") (file "../src/tool.rkt")
         (file "../src/permission.rkt") (file "../src/loop.rkt") (file "../src/session.rkt") (file "../src/plugin.rkt")
         (file "../src/rpc.rkt") (file "../src/tools/builtin.rkt"))

(define dir (make-temporary-file "pi2-at-~a" 'directory))
(void (putenv "PI_CONFIG_HOME" (path->string (build-path dir "cfg"))))
(define sdir (build-path dir "sessions"))
(make-directory* sdir)
(define prov (provider "s" (lambda (msgs _t) (define ch (make-async-channel))
                             (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant "ok")))
                             (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 1 1))) ch) void))
(define seen-prompt (box #f))
(define (fake-complete ep model msgs) (set-box! seen-prompt (list ep model (map message-text msgs))) "“Loader column fix”\nextra line")

(test-case "auto_title: untitled → the local model's short title (auto); a manual rename later wins; remote endpoints refused"
  (define cfg (struct-copy config (default-config) [workdir (path->string dir)] [permission-mode 'yolo]))
  (define other (fresh-session-path (path->string sdir)))
  (let ([s (session-open! other cfg)])
    (for ([q '("the loader drops a column" "which file?" "fix it")]) (session-append-msg! s (text-msg 'user q)) (session-append-msg! s (text-msg 'assistant "ok")))
    (session-close! s))
  (define sess (session-open! (fresh-session-path (path->string sdir)) cfg))
  (define host (make-plugin-host))
  (define d (make-deps #:provider prov #:registry (make-registry (builtin-tools cfg)) #:bus (make-bus) #:policy (make-policy cfg) #:plugin-host host))
  (define-values (in-r in-w) (make-pipe)) (define-values (out-r out-w) (make-pipe))
  (thread (lambda () (parameterize ([current-input-port in-r] [current-output-port out-w])
                       (run-rpc! d (make-initial-state cfg) sess #:plugin-host host #:fetch-models (lambda (_e) (cons '("tiny") '("tiny")))
                                 #:complete-local fake-complete))
            (close-output-port out-w)))
  (define (send! j) (write-string (jsexpr->string j) in-w) (newline in-w) (flush-output in-w))
  (define (read-type type)
    (let loop () (define l (sync/timeout 10 (read-line-evt out-r 'linefeed)))
      (unless (string? l) (error 'read-type "timeout waiting for ~a" type))
      (define j (string->jsexpr l)) (if (equal? (hash-ref j 'type #f) type) j (loop))))
  (define (row-of rows) (findf (lambda (r) (equal? (hash-ref r 'path) (path->string other))) rows))
  (read-type "session")
  (send! (hasheq 'type "list_sessions"))
  (check-equal? (hash-ref (row-of (hash-ref (read-type "sessions") 'sessions)) 'titled) #f)
  (send! (hasheq 'type "auto_title" 'path (path->string other)))
  (define r (read-type "auto_title"))
  (check-equal? (hash-ref r 'title) "Loader column fix")                            ; 去掉引号、只取第一行
  (define row (row-of (hash-ref (read-type "sessions") 'sessions)))
  (check-equal? (list (hash-ref row 'title) (hash-ref row 'titled)) '("Loader column fix" #t))
  (match-define (list ep model texts) (unbox seen-prompt))
  (check-equal? model "tiny")                                                        ; 没指定：端点上已加载的第一个
  (check-regexp-match #rx"title only" (car texts))
  (check-regexp-match #rx"user: the loader drops a column" (cadr texts))
  ;; 手动改名 → 再 auto_title：跳过
  (send! (hasheq 'type "set_title" 'path (path->string other) 'title "my own name"))
  (read-type "ok")
  (send! (hasheq 'type "auto_title" 'path (path->string other)))
  (check-equal? (hash-ref (read-type "auto_title") 'skipped) "already titled")
  (send! (hasheq 'type "list_sessions"))
  (check-equal? (hash-ref (row-of (hash-ref (read-type "sessions") 'sessions)) 'title) "my own name")
  ;; 非本机端点
  (send! (hasheq 'type "auto_title" 'path (path->string other) 'endpoint "https://example.com/v1"))
  (check-regexp-match #rx"not on this machine" (hash-ref (read-type "auto_title") 'error))
  (send! (hasheq 'type "shutdown")) (void (read-type "bye")))

(delete-directory/files dir)
