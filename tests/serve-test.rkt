#lang tstring racket
;; serve-test.rkt — 无头模式：守护进程（子进程，脚本化内核 tests/gui/fixtures/scripted-core.rkt）+ 客户端连上 / 断开 / 再连。
;; 断开期间内核照常跑；再连上的客户端拿到与一直连着时相同的对话（基线 session 事件的尾部条目 + 之后的 item 事件）；
;; 断开时没答的权限询问，再连上后还能答；prompt 子命令与 GUI 客户端同时连着；令牌不对拒绝；stop 跑完后退出并删状态文件。

(require rackunit json racket/file racket/port racket/runtime-path (file "../src/serve.rkt"))

(define cfg-dir (make-temporary-file "pi2-serve-test-~a" 'directory))
(void (putenv "PI_CONFIG_HOME" (path->string cfg-dir)))
(define racket-exe (find-executable-path (find-system-path 'exec-file)))
(define-runtime-path SERVE-PATH "../src/serve.rkt")
(define-runtime-path CORE-PATH "gui/fixtures/scripted-core.rkt")
(define SERVE (path->string (simplify-path SERVE-PATH)))
(define CORE (path->string (simplify-path CORE-PATH)))
(define NAME (format "t~a" (random 100000)))

;; 守护进程：等它在 stderr 打出监听行
(define-values (daemon d-out d-in d-err)
  (subprocess #f #f #f racket-exe SERVE "start" "--name" NAME "--entry" CORE))
(close-output-port d-in)
(define daemon-err-lines '())
(define listening (make-semaphore 0))
(void (thread (lambda ()
                (let loop ()
                  (define l (read-line d-err 'any))
                  (unless (eof-object? l)
                    (set! daemon-err-lines (cons l daemon-err-lines))
                    (when (regexp-match? #rx"^pi2 serve: name=" l) (semaphore-post listening))
                    (loop))))))
(void (thread (lambda () (copy-port d-out (open-output-nowhere)))))
(unless (sync/timeout 30 listening) (error 'serve-test "daemon did not start: ~a" (reverse daemon-err-lines)))

(define (attach) (serve-connect NAME "attach"))
(define (send! out j) (write-json j out) (newline out) (flush-output out))
;; 读事件直到 pred 为真（含该条）；→ 读到的全部（正序）
(define (read-until in pred #:timeout [secs 15])
  (let loop ([acc '()])
    (define l (sync/timeout secs (read-line-evt in 'linefeed)))
    (cond [(not (string? l)) (error 'read-until "timeout / eof; got ~s" (map (lambda (j) (hash-ref j 'type #f)) (reverse acc)))]
          [else (define j (string->jsexpr l))
                (if (pred j) (reverse (cons j acc)) (loop (cons j acc)))])))
(define ((type? . ts) j) (and (member (hash-ref j 'type #f) ts) #t))
(define (types evs) (map (lambda (j) (hash-ref j 'type #f)) evs))
(define (texts evs) (string-join (for/list ([j evs]) (format "~a" (hash-ref j 'text (hash-ref j 'items "")))) " "))
;; 会话记录投影的条目（RPC v3）
(define ((item? kind) j) (and (equal? (hash-ref j 'type #f) "item") (equal? (hash-ref j 'kind #f) kind)))
(define (user-item? text) (lambda (j) (and ((item? "user") j) (for/or ([p (hash-ref j 'parts '())]) (equal? (hash-ref p 'text #f) text)))))
(define (close! in out) (close-output-port out) (close-input-port in))

(test-case "attach → ready (attached) → prompt streams; a later client gets the same conversation"
  (define-values (in out ack) (attach))
  (define pre (read-until in (type? "replay_end")))
  (check-not-false (member "ready" (types pre)))
  (check-true (hash-ref (findf (type? "ready") pre) 'attached #f))
  (send! out (hasheq 'type "prompt" 'text "hello one"))
  (define evs (read-until in (type? "turn_complete")))
  (check-equal? (hash-ref (findf (type? "turn_start") evs) 'prompt_from) (hash-ref ack 'client_id))
  (check-regexp-match #rx"收到：hello one" (texts evs))
  (check-not-false (findf (user-item? "hello one") evs))        ; 用户消息也是条目（发 prompt 的客户端自己也收到）
  (close! in out)
  (sleep 0.5)                                                    ; 守护进程空闲后刷新基线
  (define-values (in2 out2 _ack2) (attach))
  (define pre2 (read-until in2 (type? "replay_end")))
  (define sess (findf (type? "session") pre2))
  (check-not-false sess "baseline session event")
  (check-not-false (findf (user-item? "hello one") (hash-ref sess 'items)))
  (check-regexp-match #rx"收到：hello one" (jsexpr->string (hash-ref sess 'items)))
  (close! in2 out2))

(test-case "the turn keeps running while detached; an unanswered permission request can be answered after reattach"
  (define-values (in out _ack) (attach))
  (read-until in (type? "replay_end"))
  (send! out (hasheq 'type "prompt" 'text "please echo"))        ; 脚本模型：请求 bash → normal 模式询问权限
  (define evs (read-until in (type? "permission_request")))
  (define pid (hash-ref (findf (type? "permission_request") evs) 'id))
  (close! in out)                                                ; 不答就断开
  (sleep 0.3)
  (define-values (in2 out2 _ack2) (attach))
  (define pre (read-until in2 (type? "replay_end")))
  (check-true (hash-ref (findf (type? "ready") pre) 'busy #f))
  (check-not-false (findf (user-item? "please echo") pre))
  (check-equal? (hash-ref (findf (item? "permission") pre) 'perm_id) pid)     ; 询问在会话记录里：再连上的客户端照样看到
  (check-false (findf (item? "answer") pre))
  (send! out2 (hasheq 'type "permission" 'id pid 'decision "yes"))
  (define evs2 (read-until in2 (type? "turn_complete")))
  (check-not-false (member "tool_end" (types evs2)))
  (check-regexp-match #rx"hi-from-tool" (texts evs2))
  ;; 第三个客户端：每个询问都有回答条目（在基线尾部里）
  (sleep 0.5)
  (define-values (in3 out3 _ack3) (attach))
  (define pre3 (read-until in3 (type? "replay_end")))
  (check-regexp-match #rx"please echo" (jsexpr->string pre3))
  (define items3 (append* (for/list ([j pre3]) (cond [((type? "session") j) (hash-ref j 'items)] [((type? "item") j) (list j)] [else '()]))))
  (check-not-false (findf (item? "permission") items3))
  (check-false (for/or ([j items3] #:when ((item? "permission") j))
                 (not (findf (lambda (a) (and ((item? "answer") a) (equal? (hash-ref a 'perm_id) (hash-ref j 'perm_id)))) items3))))
  (close! in3 out3)
  (close! in2 out2))

(test-case "prompt subcommand alongside an attached client; shutdown from a client only detaches it"
  (define-values (in out _ack) (attach))
  (read-until in (type? "replay_end"))
  (define-values (p p-out p-in p-err) (subprocess #f #f #f racket-exe SERVE "prompt" "--name" NAME "ping" "two"))
  (close-output-port p-in)
  (define printed (port->string p-out))
  (subprocess-wait p)
  (check-equal? (subprocess-status p) 0 (port->string p-err))
  (check-regexp-match #rx"收到：ping two" printed)
  (define evs (read-until in (type? "turn_complete")))
  (check-not-false (findf (user-item? "ping two") evs))                    ; 别的客户端发的：收到它的用户消息条目
  (send! out (hasheq 'type "shutdown"))
  (check-true (hash-ref (car (read-until in (type? "bye"))) 'detached #f))
  (close! in out)
  (check-false (hash-ref (serve-request NAME "status") 'busy)))  ; 内核还活着

(test-case "a wrong token is refused before anything is sent"
  (define st (read-state NAME))
  (define-values (in out) (tcp-connect "127.0.0.1" (hash-ref st 'port)))
  (send! out (hasheq 'pi2_serve 1 'token "nope" 'op "attach"))
  (define j (string->jsexpr (read-line in)))
  (check-equal? (hash-ref j 'type) "serve_refused")
  (check-true (eof-object? (read-line in)))
  (close! in out))

(test-case "stop: the kernel exits, the daemon exits 0 and removes its state file"
  (serve-request NAME "stop")
  (check-not-false (sync/timeout 20 daemon))
  (check-equal? (subprocess-status daemon) 0 (string-join (reverse daemon-err-lines) "\n"))
  (check-false (file-exists? (state-path NAME))))

(test-case "attach --spawn: two clients starting at once get one detached daemon (log file); it exits by itself when idle"
  (define NAME2 (format "s~a" (random 100000)))
  (define (spawn-attach)
    (define-values (p o i e) (subprocess #f #f #f racket-exe SERVE "attach" "--name" NAME2 "--spawn" "--entry" CORE "--idle-exit" "2"))
    (list p o i e))
  (define a (spawn-attach))
  (define b (spawn-attach))                                       ; 同时起：只该有一个守护进程
  (match-define (list pa oa ia ea) a)
  (match-define (list pb ob ib eb) b)
  (define pre-a (read-until oa (type? "replay_end") #:timeout 60))
  (define pre-b (read-until ob (type? "replay_end") #:timeout 60))
  (check-true (hash-ref (findf (type? "ready") pre-a) 'attached #f))
  (check-true (hash-ref (findf (type? "ready") pre-b) 'attached #f))
  (check-equal? (hash-ref (serve-request NAME2 "status") 'clients) 2)
  (send! ia (hasheq 'type "prompt" 'text "spawned"))
  (check-regexp-match #rx"收到：spawned" (texts (read-until oa (type? "turn_complete"))))
  (close-output-port ia) (close-output-port ib)                    ; 两个客户端都断开
  (subprocess-wait pa) (subprocess-wait pb)
  (check-equal? (list (subprocess-status pa) (subprocess-status pb)) '(0 0) (port->string ea))
  (check-true (serve-alive? NAME2))                                ; 客户端走了，守护进程还在
  (check-true (file-exists? (build-path cfg-dir "serve" (string-append NAME2 ".log"))))
  (check-false (file-exists? (build-path cfg-dir "serve" (string-append NAME2 ".lock"))))
  (sleep 4.5)                                                      ; 空闲 2 s + 检查间隔 0.5 s + 内核退出
  (check-false (serve-alive? NAME2))
  (check-false (file-exists? (state-path NAME2))))

(delete-directory/files cfg-dir)
