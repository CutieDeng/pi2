#lang tstring racket
;; organize-tools-test.rkt — agent 整理会话（design-workspace.md §11.7 C2），经 run-rpc!：
;; set_catalog_view 推目录视图 → 脚本模型调 sessions_overview（看到项目 / 置顶）→ propose_session_changes（两项有效、一项会话不存在）
;; → 界面收到 changeset_proposed（会话换成完整路径）；工具结果告诉模型「什么都没改」并列出跳过的项；会话文件没被改动。

(require rackunit json racket/async-channel racket/file
         (file "../src/model.rkt") (file "../src/event.rkt") (file "../src/provider.rkt") (file "../src/tool.rkt")
         (file "../src/permission.rkt") (file "../src/loop.rkt") (file "../src/session.rkt") (file "../src/plugin.rkt")
         (file "../src/rpc.rkt") (file "../src/tools/builtin.rkt"))

(define dir (make-temporary-file "pi2-org-~a" 'directory))
(void (putenv "PI_CONFIG_HOME" (path->string (build-path dir "cfg"))))
(define sdir (build-path dir "sessions"))
(make-directory* sdir)

(define overview-seen (box #f))
(define prov
  (provider "s"
            (lambda (msgs _t)
              (define ch (make-async-channel))
              (define lm (last msgs))
              (define tr (for/first ([b (in-list (message-blocks lm))] #:when (tool-result-block? b)) b))
              (define (call! name args) (async-channel-put ch (evt:message (now-ms) (message 'assistant (list (tool-use-block (format "c-~a" name) name args)))))
                                        (async-channel-put ch (evt:turn-end (now-ms) "tool_calls" (usage 1 1))))
              (cond
                [(and tr (regexp-match? #rx"\"sessions\"" (format "~a" (tool-result-block-content tr))))
                 (set-box! overview-seen (string->jsexpr (format "~a" (tool-result-block-content tr))))
                 (define rows (hash-ref (unbox overview-seen) 'sessions))
                 (define old (for/first ([r rows] #:when (equal? (hash-ref r 'title) "old notes")) (hash-ref r 'session)))
                 (define (file-name p) (let-values ([(_b n _d) (split-path p)]) (path->string n)))
                 (call! "propose_session_changes"
                        (hasheq 'summary "archive old notes, add GUI work to the project"
                                'items (list (hasheq 'op "archive" 'session (file-name old) 'reason "untouched")
                                             (hasheq 'op "new-project" 'project "Market")
                                             (hasheq 'op "delete" 'session "no-such.rktd"))))]
                [tr (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant (format "done: ~a" (tool-result-block-content tr)))))
                    (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 1 1)))]
                [(regexp-match? #rx"organize" (message-text lm)) (call! "sessions_overview" (hasheq))]
                [else (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant "ok")))
                      (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 1 1)))])
              ch)
            void))

(test-case "sessions_overview + propose_session_changes: the GUI gets an auditable proposal, nothing changes in the kernel"
  (define cfg (struct-copy config (default-config) [workdir (path->string dir)] [permission-mode 'yolo]))
  (define old-path (fresh-session-path (path->string sdir)))
  (let ([s (session-open! old-path cfg)]) (session-append-msg! s (text-msg 'user "old notes")) (session-close! s))
  (define old-bytes (file->bytes old-path))
  (define sess (session-open! (fresh-session-path (path->string sdir)) cfg))
  (define host (make-plugin-host))
  (define d (make-deps #:provider prov #:registry (make-registry (builtin-tools cfg)) #:bus (make-bus) #:policy (make-policy cfg) #:plugin-host host))
  (define-values (in-r in-w) (make-pipe)) (define-values (out-r out-w) (make-pipe))
  (thread (lambda () (parameterize ([current-input-port in-r] [current-output-port out-w])
                       (run-rpc! d (make-initial-state cfg) sess #:plugin-host host #:fetch-models (lambda (_e) #f)))
            (close-output-port out-w)))
  (define (send! j) (write-string (jsexpr->string j) in-w) (newline in-w) (flush-output in-w))
  (define (read-type type)
    (let loop () (define l (sync/timeout 10 (read-line-evt out-r 'linefeed)))
      (unless (string? l) (error 'read-type "timeout waiting for ~a" type))
      (define j (string->jsexpr l)) (if (equal? (hash-ref j 'type #f) type) j (loop))))
  (read-type "session")
  (send! (hasheq 'type "set_catalog_view" 'projects '("pi2 GUI") 'sessions (list (hasheq 'path (path->string old-path) 'projects '("pi2 GUI") 'pinned #t 'archived #f))))
  (send! (hasheq 'type "prompt" 'text "organize my sessions"))
  (define cs (read-type "changeset_proposed"))
  (read-type "turn_complete")
  ;; 模型看到的总览：有项目、旧会话在项目里且置顶
  (define ov (unbox overview-seen))
  (check-equal? (hash-ref ov 'projects) '("pi2 GUI"))
  (define old-row (for/first ([r (hash-ref ov 'sessions)] #:when (equal? (hash-ref r 'title) "old notes")) r))
  (check-equal? (list (hash-ref old-row 'projects) (hash-ref old-row 'pinned) (hash-ref old-row 'messages)) (list '("pi2 GUI") #t 1))
  ;; 提议：两项有效（会话名换成完整路径），不存在的会话被跳过
  (check-equal? (hash-ref cs 'summary) "archive old notes, add GUI work to the project")
  (check-equal? (map (lambda (it) (hash-ref it 'op)) (hash-ref cs 'items)) '("archive" "new-project"))
  (check-equal? (hash-ref (car (hash-ref cs 'items)) 'session) (path->string old-path))
  (check-equal? (hash-ref cs 'session) (path->string (session-path sess)))
  ;; 工具结果：什么都没改，列出跳过的
  (send! (hasheq 'type "history"))
  (define hist (hash-ref (read-type "history") 'messages))
  (define last-text (hash-ref (last hist) 'text))
  (check-regexp-match #rx"Nothing has been changed yet" last-text)
  (check-regexp-match #rx"no such session" last-text)
  ;; 内核这边没动会话文件
  (check-equal? (file->bytes old-path) old-bytes)
  (send! (hasheq 'type "shutdown")) (void (read-type "bye")))

(delete-directory/files dir)
