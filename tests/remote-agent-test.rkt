#lang tstring racket
;; remote-agent-test.rkt — 跨主机子 agent（spawn_agent 的 host 参数）端到端：
;; 「远端」= 本机起的真 pi2 内核（main.rkt --rpc，主机表 'command 传输；与 ssh 时同一协议，只少了 ssh 与 -R 隧道），
;; 模型上游 = 假的 OpenAI 兼容流式接口（第一次让模型调 bash 访问网络 → auto 模式要询问；拿到工具结果后回答）。
;; 检查：询问转到本机（工具名带主机前缀）；proxy：上游只见到真 key、远端只拿到 token、跑完 token 吊销、远端会话文件里没有真 key；
;; share：上游收到的就是真 key；用量并进父会话；未知主机报错。

(require rackunit json racket/tcp racket/file racket/runtime-path
         (file "../src/model.rkt") (file "../src/tool.rkt") (file "../src/permission.rkt") (file "../src/loop.rkt")
         (file "../src/subagent.rkt") (file "../src/hosts.rkt") (file "../src/remote-agent.rkt"))

(define-runtime-path MAIN "../main.rkt")
(define-runtime-path FAKE-SSH "fixtures/fake-ssh")
(define cfg-dir (make-temporary-file "pi2-remote-agent-~a" 'directory))
(define data-dir (make-temporary-file "pi2-remote-agent-data-~a" 'directory))
(define work (make-temporary-file "pi2-remote-agent-ws-~a" 'directory))
(void (putenv "PI_CONFIG_HOME" (path->string cfg-dir)))
(void (putenv "PI_DATA_HOME" (path->string data-dir)))
(define racket (path->string (find-executable-path (find-system-path 'exec-file))))

;; ---- 假上游（OpenAI chat completions，流式）
(define seen-auth '())
(define up (tcp-listen 0 16 #t "127.0.0.1"))
(define up-port (let-values ([(_a p _b _c) (tcp-addresses up #t)]) p))
(define (sse out . objs)
  (for ([o objs]) (fprintf out "data: ~a\n\n" (if (string? o) o (jsexpr->string o))) (flush-output out)))
(void (thread (lambda ()
                (with-handlers ([exn:fail? void])
                  (let loop ()
                    (define-values (in out) (tcp-accept up))
                    (thread (lambda ()
                              (read-line in 'return-linefeed)
                              (let hdrs ([len 0] [auth #f])
                                (define l (read-line in 'return-linefeed))
                                (cond
                                  [(or (eof-object? l) (string=? l ""))
                                   (define body (bytes->jsexpr (read-bytes len in)))
                                   (set! seen-auth (cons auth seen-auth))
                                   (define tool-result? (for/or ([m (hash-ref body 'messages '())]) (equal? (hash-ref m 'role #f) "tool")))
                                   (fprintf out "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nConnection: close\r\n\r\n")
                                   (cond
                                     [tool-result?
                                      (define r (for/last ([m (hash-ref body 'messages '())] #:when (equal? (hash-ref m 'role #f) "tool")) (hash-ref m 'content "")))
                                      (sse out (hasheq 'choices (list (hasheq 'index 0 'delta (hasheq 'content (format "remote done: ~a" (string-trim (format "~a" r)))))))
                                           (hasheq 'choices (list (hasheq 'index 0 'delta (hasheq) 'finish_reason "stop")) 'usage (hasheq 'prompt_tokens 100 'completion_tokens 20))
                                           "[DONE]")]
                                     [else
                                      (sse out (hasheq 'choices (list (hasheq 'index 0 'delta (hasheq 'tool_calls (list (hasheq 'index 0 'id "c1" 'type "function"
                                                                                                                             'function (hasheq 'name "bash" 'arguments (jsexpr->string (hasheq 'command "curl -s -m 1 http://127.0.0.1:9/ || echo net-blocked")))))))))
                                           (hasheq 'choices (list (hasheq 'index 0 'delta (hasheq) 'finish_reason "tool_calls")) 'usage (hasheq 'prompt_tokens 100 'completion_tokens 20))
                                           "[DONE]")])
                                   (close-output-port out) (close-input-port in)]
                                  [(regexp-match? #rx"(?i:^authorization: )" l) (hdrs len l)]
                                  [(regexp-match #rx"(?i:^content-length: ([0-9]+))$" l) => (lambda (m) (hdrs (string->number (cadr m)) auth))]
                                  [else (hdrs len auth)]))))
                    (loop))))))

(define parent-cfg (struct-copy config (default-config) [endpoint (format "http://127.0.0.1:~a/v1" up-port)] [api-key "sk-parent-secret"]
                                [model "fake-model"] [workdir (path->string work)]))
(define tool (make-spawn-agent-tool #:provider #f #:sub-tools '()))
;; 父会话的记录（sub 记录 = 拉回的远端过程）收在这里
(define recorded '())
(define (spawn! input)
  (define asks '())
  (define extra (box (usage 0 0)))
  (set! recorded '())
  (define o (parameterize ([current-asker (lambda (prompt) (set! asks (cons (current-permission-request) asks)) 'yes)]
                           [current-extra-usage extra]
                           [current-record-sink (lambda (kind payload) (set! recorded (cons (cons kind payload) recorded)))]
                           [current-tool-call-id "call-9"])
              (tool-run tool input (tool-ctx (path->string work) void parent-cfg))))
  (values o (reverse asks) (unbox extra)))

(define (set-host! cred)
  (save-hosts! (list (hasheq 'id "box" 'command (list racket (path->string MAIN)) 'credential cred 'workdir (path->string work)))))

(test-case "proxy credential: permission asked here with host prefix; upstream sees only the real key; token revoked; usage rolled up"
  (set-host! "proxy")
  (set! seen-auth '())
  (define-values (o asks extra) (spawn! (hasheq 'task "check whether the network is reachable" 'host "box")))
  (check-false (tool-outcome-is-error? o) (tool-outcome-content o))
  (check-regexp-match #rx"remote done: .*net-blocked" (tool-outcome-content o))
  (check-regexp-match #rx"spawn_agent @box \\(fake-model\\)" (tool-outcome-display o))
  (check-regexp-match #rx"key proxy: 2 requests" (tool-outcome-display o))              ; 两次模型请求都经本机转发
  (check-equal? (length asks) 1)
  ;; 决定 4：远端的过程拉回一份，作为 sub 记录挂在父调用下（用户消息、回答 + bash 调用、工具结束、结果、回答…）
  (define subs (for/list ([r (in-list (reverse recorded))] #:when (eq? (car r) 'sub)) (cdr r)))
  (check-true (andmap (lambda (h) (and (equal? (hash-ref h 'parent) "call-9") (equal? (hash-ref h 'host) "box"))) subs))
  (check-not-false (member "results" (map (lambda (h) (hash-ref (hash-ref h 'item) 'kind)) subs)))
  (check-not-false (findf (lambda (h) (and (equal? (hash-ref (hash-ref h 'item) 'kind) "tool") (equal? (hash-ref (hash-ref h 'item) 'name) "bash"))) subs))
  (check-equal? (hash-ref (car asks) 'tool) "box:bash")
  (check-equal? seen-auth (list "Authorization: Bearer sk-parent-secret" "Authorization: Bearer sk-parent-secret"))
  (check-equal? (list (usage-input-tokens extra) (usage-output-tokens extra)) '(200 40))
  ;; 远端会话文件里没有真 key（只有经转发的端点）
  (define files (for/list ([f (in-directory data-dir)] #:when (regexp-match? #rx"[.]rktd$" (path->string f))) f))
  (check-true (pair? files))
  (check-false (for/or ([f files]) (regexp-match? #rx"sk-parent-secret" (file->string f)))))

(test-case "share credential: the remote kernel gets the real key in memory and calls the upstream directly"
  (set-host! "share")
  (set! seen-auth '())
  (define-values (o asks _extra) (spawn! (hasheq 'task "check the network" 'host "box")))
  (check-false (tool-outcome-is-error? o) (tool-outcome-content o))
  (check-equal? seen-auth (list "Authorization: Bearer sk-parent-secret" "Authorization: Bearer sk-parent-secret"))
  (check-false (regexp-match? #rx"key proxy" (tool-outcome-display o))))                    ; 没经转发

(test-case "ssh path (fake ssh first in PATH): -R tunnel back to the key proxy, quoted remote command; a taken remote port is retried"
  (define bin (make-temporary-file "pi2-fakebin-~a" 'directory))
  (make-file-or-directory-link (simplify-path FAKE-SSH) (build-path bin "ssh"))
  (define log (build-path bin "ssh.log"))
  (putenv "FAKE_SSH_LOG" (path->string log))
  (define old-path (getenv "PATH"))
  (putenv "PATH" (string-append (path->string bin) ":" old-path))
  (save-hosts! (list (hasheq 'id "lan-x" 'ssh "lan-x" 'remote-cmd (format "~a ~a" racket (path->string MAIN)) 'workdir (path->string work))))
  ;; 第一次给一个已被占用的端口：fake ssh 报 remote port forwarding failed → 换端口重试
  (define busy (tcp-listen 0 4 #t "127.0.0.1"))
  (define busy-port (let-values ([(_a p _b _c) (tcp-addresses busy #t)]) p))
  (define ports (list busy-port (+ 30000 (random 20000))))
  (set! seen-auth '())
  (define-values (o asks extra)
    (parameterize ([current-pick-port (lambda () (begin0 (car ports) (when (pair? (cdr ports)) (set! ports (cdr ports)))))])
      (spawn! (hasheq 'task "check the network" 'host "lan-x"))))
  (putenv "PATH" old-path)
  (tcp-close busy)
  (check-false (tool-outcome-is-error? o) (tool-outcome-content o))
  (check-regexp-match #rx"key proxy: 2 requests" (tool-outcome-display o))
  (check-equal? (hash-ref (car asks) 'tool) "lan-x:bash")
  (check-equal? seen-auth (list "Authorization: Bearer sk-parent-secret" "Authorization: Bearer sk-parent-secret"))
  (define calls (with-input-from-file log (lambda () (for/list ([x (in-port read)]) x))))
  (check-equal? (length calls) 2)                                                     ; 第一次端口冲突，第二次成功
  (check-regexp-match (pregexp (format "^127\\.0\\.0\\.1:~a:127\\.0\\.0\\.1:[0-9]+$" busy-port)) (list-ref (car calls) (add1 (index-of (car calls) "-R"))))
  (check-equal? (take-right (cadr calls) 2)
                (list "lan-x" (format "~a ~a '--rpc' '--mode' 'auto' '-C' '~a'" racket (path->string MAIN) (path->string work))))
  (delete-directory/files bin))

(test-case "unknown host → error listing the configured hosts"
  (define-values (o _a _e) (spawn! (hasheq 'task "x" 'host "nope")))
  (check-true (tool-outcome-is-error? o))
  (check-regexp-match #rx"unknown host nope \\(hosts: lan-x\\)" (tool-outcome-content o)))

(tcp-close up)
(delete-directory/files cfg-dir)
