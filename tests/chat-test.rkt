#lang tstring racket
;; chat-test.rkt — 聊天会话（匿名工作区，design-workspace.md §10）：经 run-rpc!：
;; 聊天会话有自己的目录（应用管理的根目录下），工具在那里跑；普通会话回到内核的工作目录；续接聊天会话回到它的目录；
;; 临时（没消息就切走）的聊天会话连目录一起删；删除聊天会话 = 进回收站（目录一起）、可恢复、过期清掉；配置校验；超合计上限时删最久没用的聊天目录（不删当前的），被删的会话里记一条提示。

(require rackunit json racket/async-channel racket/file
         (file "../src/model.rkt") (file "../src/event.rkt") (file "../src/provider.rkt") (file "../src/tool.rkt")
         (file "../src/permission.rkt") (file "../src/loop.rkt") (file "../src/session.rkt") (file "../src/plugin.rkt")
         (file "../src/rpc.rkt") (file "../src/rktd.rkt") (file "../src/tools/builtin.rkt"))

(define dir (make-temporary-file "pi2-chat-~a" 'directory))
(void (putenv "PI_CONFIG_HOME" (path->string dir)))
(define base (build-path dir "project"))
(make-directory* base)

;; 脚本模型：消息里有 write → 写一个 1 KB 的文件 out-<n>.txt（相对路径）；拿到结果后回答
(define prov
  (provider "s"
            (lambda (msgs _t)
              (define ch (make-async-channel))
              (define lm (last msgs))
              (define m (regexp-match #rx"write ([a-z0-9]+)" (message-text lm)))
              (cond
                [(ormap tool-result-block? (message-blocks lm))
                 (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant "written")))
                 (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 1 1)))]
                [m (async-channel-put ch (evt:message (now-ms) (message 'assistant (list (tool-use-block "w1" "write_file" (hasheq 'path (format "out-~a.txt" (cadr m)) 'content (make-string 1024 #\x)))))))
                   (async-channel-put ch (evt:turn-end (now-ms) "tool_calls" (usage 1 1)))]
                [else (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant "ok")))
                      (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 1 1)))])
              ch)
            void))

(test-case "chat sessions: own directory; tools run there; switching / resuming / ephemeral / delete; size management"
  (define cfg (struct-copy config (default-config) [workdir (path->string base)] [permission-mode 'yolo]))
  (define sdir (build-path dir "sessions"))
  (make-directory* sdir)
  (define sess (session-open! (fresh-session-path (path->string sdir)) cfg))
  (define host (make-plugin-host))
  (define d (make-deps #:provider prov #:registry (make-registry (builtin-tools cfg)) #:bus (make-bus) #:policy (make-policy cfg) #:plugin-host host))
  (define-values (in-r in-w) (make-pipe)) (define-values (out-r out-w) (make-pipe))
  (thread (lambda () (parameterize ([current-input-port in-r] [current-output-port out-w])
                       (run-rpc! d (make-initial-state cfg) sess #:plugin-host host #:fetch-models (lambda (_e) #f)))
            (close-output-port out-w)))
  (define (send! j) (write-string (jsexpr->string j) in-w) (newline in-w) (flush-output in-w))
  (define seen '())
  (define (read-type type)
    (let loop ()
      (define l (sync/timeout 10 (read-line-evt out-r 'linefeed)))
      (unless (string? l) (error 'read-type "timeout waiting for ~a; last events ~s" type (take seen (min 6 (length seen)))))
      (define j (string->jsexpr l))
      (set! seen (cons (list (hash-ref j 'type #f) (hash-ref j 'message (hash-ref j 'for #f))) seen))
      (if (equal? (hash-ref j 'type #f) type) j (loop))))
  (define root (build-path sdir "chats"))
  (read-type "session")                                                                 ; 启动时的那个
  ;; 1. 聊天会话：目录在根下、按会话一个；工具写在那里
  (send! (hasheq 'type "new_session" 'chat #t))
  (define s1 (read-type "session"))
  (check-true (hash-ref s1 'chat))
  (define c1 (hash-ref s1 'workdir))
  (check-true (string-prefix? c1 (path->string root)) (format "~a vs ~a" c1 root))
  (check-true (directory-exists? c1))
  (send! (hasheq 'type "prompt" 'text "write a"))
  (read-type "turn_complete")
  (check-true (file-exists? (build-path c1 "out-a.txt")))
  (check-false (file-exists? (build-path base "out-a.txt")))
  (send! (hasheq 'type "list_sessions"))
  (define row1 (findf (lambda (s) (hash-ref s 'current)) (hash-ref (read-type "sessions") 'sessions)))
  (check-equal? (list (hash-ref row1 'chat) (hash-ref row1 'workdir)) (list #t c1))
  (send! (hasheq 'type "session_stats"))
  (define st1 (read-type "session_stats"))
  (check-equal? (list (hash-ref st1 'chat) (hash-ref st1 'workdir)) (list #t c1))
  (check-true (>= (hash-ref st1 'workdir_bytes) 1024))
  ;; 2. 普通会话：回到内核的工作目录
  (send! (hasheq 'type "new_session"))
  (define s2 (read-type "session"))
  (check-equal? (list (hash-ref s2 'chat) (hash-ref s2 'workdir)) (list #f (path->string base)))
  (send! (hasheq 'type "prompt" 'text "write b"))
  (read-type "turn_complete")
  (check-true (file-exists? (build-path base "out-b.txt")))
  ;; 3. 续接聊天会话：回到它的目录
  (send! (hasheq 'type "resume" 'path (hash-ref s1 'path)))
  (check-equal? (hash-ref (read-type "session") 'workdir) c1)
  ;; 4. 临时聊天会话（没消息就切走）：会话文件与目录都删
  (send! (hasheq 'type "new_session" 'chat #t))
  (define c3 (hash-ref (read-type "session") 'workdir))
  (check-true (directory-exists? c3))
  (send! (hasheq 'type "resume" 'path (hash-ref s1 'path)))
  (read-type "session")
  (check-false (directory-exists? c3))
  ;; 5. 配置校验
  (send! (hasheq 'type "set_chat_config" 'max_total_gb -1))
  (check-regexp-match #rx"positive" (hash-ref (read-type "error") 'message))
  (send! (hasheq 'type "set_chat_config" 'root "relative/path"))
  (check-regexp-match #rx"absolute" (hash-ref (read-type "error") 'message))
  ;; 6. 占用管理：再建一个旧的聊天会话（写 1 KB），把合计上限设到 1.5 KB → 最久没用的（不是当前）被删，那个会话里有提示
  (send! (hasheq 'type "new_session" 'chat #t))
  (define s4 (read-type "session"))
  (send! (hasheq 'type "prompt" 'text "write c"))
  (read-type "turn_complete")
  (file-or-directory-modify-seconds (hash-ref s4 'path) (- (current-seconds) 3600))     ; 它是更旧的那个
  (send! (hasheq 'type "resume" 'path (hash-ref s1 'path)))
  (read-type "session")
  (send! (hasheq 'type "set_chat_config" 'max_total_gb (/ 1500.0 1073741824) 'min_free_gb 0.001))
  (read-type "ok")
  (define u (let loop () (define j (read-type "chat_usage")) (if (pair? (hash-ref j 'removed)) j (loop))))
  (check-equal? (map (lambda (x) (hash-ref x 'name)) (hash-ref u 'removed))
                (list (regexp-replace #rx"[.]rktd$" (let-values ([(_b n _d) (split-path (hash-ref s4 'path))]) (path->string n)) "")))
  (check-false (directory-exists? (hash-ref s4 'workdir)))
  (check-true (directory-exists? c1))                                                   ; 当前的不删
  (check-not-false (findf (lambda (r) (and (rec? r) (eq? (rec-type r) 'note) (equal? (hash-ref (rec-data r) 'code #f) "chat_cleanup")))
                          (for/list ([x (in-datum-log (hash-ref s4 'path))]) x)))
  ;; 7. 续接被清理过的聊天会话：重建空目录、记提示
  (send! (hasheq 'type "resume" 'path (hash-ref s4 'path)))
  (check-true (directory-exists? (hash-ref (read-type "session") 'workdir)))
  ;; 8. 删聊天会话：进回收站（会话文件、索引、聊天目录一起挪进 trash/）；不在列表里；恢复 → 都回到原处
  (send! (hasheq 'type "resume" 'path (hash-ref s2 'path)))
  (read-type "session")
  (send! (hasheq 'type "delete_session" 'path (hash-ref s1 'path)))
  (check-equal? (hash-ref (read-type "ok") 'restorable) #t)
  (define stem1 (regexp-replace #rx"[.]rktd$" (let-values ([(_b n _d) (split-path (hash-ref s1 'path))]) (path->string n)) ""))
  (check-false (directory-exists? c1))
  (check-false (file-exists? (hash-ref s1 'path)))
  (check-true (file-exists? (build-path sdir "trash" (string-append stem1 ".rktd"))))
  (check-true (directory-exists? (build-path sdir "trash" (string-append stem1 ".chat"))))
  (send! (hasheq 'type "list_sessions"))
  (check-false (findf (lambda (r) (equal? (hash-ref r 'path) (hash-ref s1 'path))) (hash-ref (read-type "sessions") 'sessions)))
  (send! (hasheq 'type "restore_session" 'path (hash-ref s1 'path)))
  (read-type "ok")
  (check-true (file-exists? (hash-ref s1 'path)))
  (check-true (file-exists? (build-path c1 "out-a.txt")))                               ; 聊天目录连内容一起回来
  (send! (hasheq 'type "list_sessions"))
  (check-not-false (findf (lambda (r) (equal? (hash-ref r 'path) (hash-ref s1 'path))) (hash-ref (read-type "sessions") 'sessions)))
  (send! (hasheq 'type "restore_session" 'path (hash-ref s1 'path)))                   ; 不在回收站：报错
  (check-regexp-match #rx"not in the trash" (hash-ref (read-type "error") 'message))
  ;; 9. 过期清理：再删一次，把回收站里的文件改成 8 天前 → trash-purge! 清掉会话文件、索引、聊天目录
  (send! (hasheq 'type "delete_session" 'path (hash-ref s1 'path)))
  (read-type "ok")
  (define t1 (build-path sdir "trash" (string-append stem1 ".rktd")))
  (file-or-directory-modify-seconds t1 (- (current-seconds) (* 8 86400)))
  (check-equal? (trash-purge! sdir) (list stem1))
  (check-false (file-exists? t1))
  (check-false (directory-exists? (build-path sdir "trash" (string-append stem1 ".chat"))))
  (send! (hasheq 'type "shutdown")) (void (read-type "bye")))

(delete-directory/files dir)
