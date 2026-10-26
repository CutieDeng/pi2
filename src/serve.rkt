#lang tstring racket
;; serve.rkt — 无头模式：在服务器（无显示）上常驻 pi2 内核；GUI 经 ssh 连上、断开，断开时内核照常跑完。
;; 用法：
;;   racket src/serve.rkt start [--name N] [--entry 模块.rkt] [-- 内核参数…]   前台常驻（放 tmux / systemd / nohup 里跑）
;;   racket src/serve.rkt attach [--name N] [--spawn [--entry 模块] [--idle-exit 秒] [-- 内核参数…]] [--wait 秒]
;;                                                   stdin / stdout 接到常驻内核（GUI：ssh -T 主机 racket …/serve.rkt attach --spawn）；
;;                                                   --spawn：没在跑就先在后台起一个（脱离本进程，日志写 {config-home}/serve/<名字>.log），等它就绪再连
;;   racket src/serve.rkt prompt [--name N] 文本     不开 GUI 交一个提示，流式打印回答；遇到权限询问就断开（留给 GUI 答），退出码 3
;;   racket src/serve.rkt status|stop|list [--name N]
;; 内核 = 入口模块（缺省 main.rkt）的 main 子模块，参数 --rpc 内核参数…，在本进程里跑，stdin / stdout 接进程内管道（同 GUI 的 kernel-place）。
;; 连接：127.0.0.1 随机端口 + 32 字节随机令牌，记在 {config-home}/serve/<名字>.rktd（目录 0700、文件 0600）。
;;   客户端首行握手 {"pi2_serve":1,"token":…,"op":"attach"|"status"|"stop"}；attach 之后的连接上就是 RPC（src/rpc.rkt，协议 v3）的 NDJSON。
;; 可以同时连多个客户端：内核事件发给所有客户端；任一客户端都能发请求、答权限。
;; 新客户端补齐对话区（守护进程记「基线 + 之后的事件」）：
;;   基线 = 内核最近一条 session 事件（只带尾部条目）；内核空闲后（turn_complete / goal_end / compact / clear）守护进程发 session_info 刷新基线（应答不转发）。
;;   之后的事件 = item（会话记录投影成的界面条目：用户消息、权限询问与回答都在里面）、delta（相邻同类合并）、turn_start / turn_complete / goal_end。
;;   翻页应答（items / item / index_summary）照常广播，客户端按请求时带的 'tag 认领自己的。
;;   新客户端收到：ready（attached #t、busy、client_id）→ 基线 → 之后的事件 → 还没开始的 prompt → replay_end。
;; 客户端发 shutdown 只断开自己，不停内核；停内核用 stop（关内核 stdin，内核跑完当前轮后退出）。
;; --idle-exit 秒：没有客户端连着、没有轮在跑、也没有排队的提示，持续这么久 → 同 stop。GUI 自动起的本机守护进程用 1800。

(require racket/tcp racket/os racket/random racket/async-channel racket/runtime-path json file/sha1
         (file "credentials.rkt"))

(define-runtime-path PI2-MAIN "../main.rkt")
(define-runtime-path SELF "serve.rkt")

(define PROTOCOL 1)
(define LOG-MAX-BYTES (* 48 1024 1024))               ; 之后的事件超过这么多字节：丢最早的
(define CLIENT-QUEUE-MAX 200000)                      ; 某客户端积压这么多条还没写出（网络卡死）：断开它

;; ---------------------------------------------------------------- 状态文件

(define (serve-dir) (build-path (config-home) "serve"))
(define (state-path name) (build-path (serve-dir) (string-append name ".rktd")))

(define (valid-name? n) (and (string? n) (regexp-match? #px"^[A-Za-z0-9_.-]{1,64}$" n)))

(define (read-state name)
  (define p (state-path name))
  (and (file-exists? p)
       (with-handlers ([exn:fail? (lambda (_e) #f)])
         (define v (call-with-input-file p read))
         (and (hash? v) v)))
) ; end define read-state

(define (write-state! name h)
  (define dir (serve-dir))
  (make-directory* dir)
  (file-or-directory-permissions dir #o700)
  (define p (state-path name))
  (define tmp (path-add-extension p #".tmp"))
  (call-with-output-file tmp #:exists 'truncate (lambda (out) (write h out)))
  (file-or-directory-permissions tmp #o600)
  (rename-file-or-directory tmp p #t)
) ; end define write-state!

;; ---------------------------------------------------------------- 客户端侧：连接与握手

;; → (values in out ack)；没有守护进程 / 令牌不对 → raise exn:fail（消息给人看）
(define (serve-connect name op #:extra [extra (hasheq)])
  (define st (read-state name))
  (unless st (error 'serve "no pi2 serve named ~s (start one with: racket src/serve.rkt start --name ~a)" name name))
  (define-values (in out)
    (with-handlers ([exn:fail:network? (lambda (_e) (error 'serve "pi2 serve ~s is not running (stale state file)" name))])
      (tcp-connect "127.0.0.1" (hash-ref st 'port))))
  (write-json (for/fold ([h (hasheq 'pi2_serve PROTOCOL 'token (hash-ref st 'token) 'op op)]) ([(k v) (in-hash extra)]) (hash-set h k v)) out)
  (newline out) (flush-output out)
  (define line (sync/timeout 10 (read-line-evt in 'linefeed)))
  (define ack (and (string? line) (with-handlers ([exn:fail? (lambda (_e) #f)]) (string->jsexpr line))))
  (unless (and (hash? ack) (equal? (hash-ref ack 'type #f) "serve_ok"))
    (close-input-port in) (close-output-port out)
    (error 'serve "handshake refused: ~a" (if (hash? ack) (hash-ref ack 'message "?") (or line "no answer"))))
  (values in out ack)
) ; end define serve-connect

(define (serve-request name op)
  (define-values (in out ack) (serve-connect name op))
  (close-input-port in) (close-output-port out)
  ack
) ; end define serve-request

(define (serve-alive? name)
  (with-handlers ([exn:fail? (lambda (_e) #f)]) (and (serve-request name "status") #t))
) ; end define serve-alive?

;; ---------------------------------------------------------------- 守护进程

;; q：待写出的事件（倒序），n：条数，sig：每放一条 post 一次；写线程整批取走
(struct client (id in out [q #:mutable] [n #:mutable] sig [alive? #:mutable]))

;; 入口模块的 main 子模块当内核；返回退出码。#:on-listening 在开始监听后调用（测试 / 启动提示）
(define (serve-start! name #:entry [entry PI2-MAIN] #:args [kernel-args '()] #:on-listening [on-listening void]
                      #:idle-exit [idle #f])
  (unless (valid-name? name) (error 'serve "bad name: ~s (letters, digits, _ . -)" name))
  (when (serve-alive? name) (error 'serve "pi2 serve ~s is already running" name))
  (define token (bytes->hex-string (crypto-random-bytes 32)))
  (define listener (tcp-listen 0 16 #t "127.0.0.1"))
  (define port (let-values ([(_a p _b _c) (tcp-addresses listener #t)]) p))
  (define started (current-seconds))
  (define st-path (state-path name))                  ; 内核可能改 PI_CONFIG_HOME（测试内核会）：路径先算好
  (write-state! name (hasheq 'port port 'token token 'pid (getpid) 'started started
                             'cwd (path->string (current-directory)) 'entry (path->string (simplify-path entry))))
  ;; ---- 内核
  (define-values (kin-r kin-w) (make-pipe))
  (define-values (kout-r kout-w) (make-pipe))
  (define-values (kerr-r kerr-w) (make-pipe))
  (define kernel-done (make-semaphore 0))
  (define kernel-ready (make-semaphore 0))             ; 内核发出 ready（或已结束）后才给新客户端补齐
  (define kernel-code #f)
  (define kernel-thread
    (thread
     (lambda ()
       (define code
         (let/ec k
           (parameterize ([current-command-line-arguments (list->vector (cons "--rpc" kernel-args))]
                          [current-input-port kin-r]
                          [current-output-port kout-w]
                          [current-error-port kerr-w]
                          [exit-handler (lambda (c) (k (if (exact-integer? c) c 0)))])
             (with-handlers ([exn:fail? (lambda (e) (eprintf "kernel crashed: ~a\n" (exn-message e)) 70)])
               (dynamic-require (list 'submod (list 'file (path->string (simplify-path entry))) 'main) #f)
               0))))
       (set! kernel-code code)
       (close-output-port kout-w)
       (close-output-port kerr-w)
       (semaphore-post kernel-done))))
  ;; ---- 共享状态（都在 lock 里改）
  (define lock (make-semaphore 1))
  (define-syntax-rule (locked body ...) (call-with-semaphore lock (lambda () body ...)))
  (define clients '())
  (define next-id 0)
  (define ready-ev #f)
  (define busy? #f)
  (define baseline #f)                                 ; session 事件或 #f（= 空对话）
  (define log '())                                     ; 之后的事件，倒序
  (define log-bytes 0)
  (define pending-prompts '())                         ; 还没开始的 prompt 的来源客户端 id，正序（turn_start 带 prompt_from）
  (define session-q '())                               ; 会产生 session 事件、还没应答的请求：'client / 'daemon，正序
  (define stderr-tail '())
  (define last-active (current-inexact-milliseconds))  ; 空闲退出：最近一次有客户端进出 / 轮开始结束
  (define (touch!) (set! last-active (current-inexact-milliseconds)))
  (define (send-to! c j)
    (when (client-alive? c)
      (cond [(> (client-n c) CLIENT-QUEUE-MAX) (drop-client! c)]
            [else (enqueue! c j)])))
  (define (enqueue! c j)
    (set-client-q! c (cons j (client-q c)))
    (set-client-n! c (add1 (client-n c)))
    (semaphore-post (client-sig c)))
  (define (broadcast! j #:except [except #f])
    (for ([c (in-list clients)] #:unless (eqv? (client-id c) except)) (send-to! c j)))
  (define (drop-client! c)
    (when (client-alive? c)
      (touch!)
      (set-client-alive?! c #f)
      (enqueue! c 'close)
      (set! clients (remq c clients))))
  (define (to-kernel! line)
    (with-handlers ([exn:fail? void])
      (write-string line kin-w) (newline kin-w) (flush-output kin-w)))
  ;; ---- 之后的事件
  (define (log! j size)
    (set! log (cons j log))
    (set! log-bytes (+ log-bytes size))
    (when (> log-bytes LOG-MAX-BYTES)                  ; 很长的一轮（goal 模式）：丢最早的，留一条说明
      (define kept (let loop ([xs log] [acc 0] [out '()])
                     (if (or (null? xs) (> acc (* 3/4 LOG-MAX-BYTES))) (reverse out)
                         (loop (cdr xs) (+ acc (string-length (jsexpr->string (car xs)))) (cons (car xs) out)))))
      (set! log (append kept (list (hasheq 'type "note" 'level "warn" 'text "(earlier output of this turn was not kept by the server)"))))
      (set! log-bytes (for/sum ([x (in-list log)]) (string-length (jsexpr->string x))))))
  (define (log-delta! j size)
    (define top (and (pair? log) (car log)))
    (cond [(and top (equal? (hash-ref top 'type #f) "delta") (equal? (hash-ref top 'kind #f) (hash-ref j 'kind #f)))
           (set! log (cons (hash-set top 'text (string-append (hash-ref top 'text "") (hash-ref j 'text ""))) (cdr log)))
           (set! log-bytes (+ log-bytes (string-length (hash-ref j 'text ""))))]
          [else (log! j size)]))
  (define (set-baseline! ev)
    (set! baseline ev)
    (set! log '())
    (set! log-bytes 0))
  (define (refresh-baseline!)
    (set! session-q (append session-q (list 'daemon)))
    (to-kernel! (jsexpr->string (hasheq 'type "session_info"))))
  (define REPLAYED '("turn_start" "turn_complete" "goal_end" "item"))
  (define (on-kernel-line! line)
    (define j (with-handlers ([exn:fail? (lambda (_e) #f)]) (let ([v (string->jsexpr line)]) (and (hash? v) v))))
    (define size (string-length line))
    (locked
     (cond
       [(not j) (broadcast! (hasheq 'type "core_garbage" 'line line))]
       [else
        (define type (hash-ref j 'type #f))
        (cond
          [(equal? type "ready")
           (unless ready-ev (semaphore-post kernel-ready))
           (set! ready-ev j)
           (broadcast! (hash-set j 'attached #t))]
          [(equal? type "session")
           ;; 守护进程自己要的（刷新基线）不转发；客户端的（切会话、session_info）与内核主动发的（ready 之后）转发
           (define who (if (pair? session-q) (car session-q) 'client))
           (when (pair? session-q) (set! session-q (cdr session-q)))
           (when ready-ev (set! ready-ev (hash-set ready-ev 'session (hash-ref j 'path ""))))
           (set-baseline! j)
           (unless (eq? who 'daemon) (broadcast! j))]
          [(equal? type "turn_start")
           (set! busy? #t)
           (touch!)
           (define j*
             (cond [(and (equal? (hash-ref j 'kind #f) "prompt") (pair? pending-prompts))
                    (define from (car pending-prompts))
                    (set! pending-prompts (cdr pending-prompts))
                    (hash-set j 'prompt_from from)]
                   [else j]))
           (log! j* size)
           (broadcast! j*)]
          [(member type '("turn_complete" "goal_end"))
           (set! busy? #f)
           (touch!)
           (log! j size)
           (broadcast! j)
           (refresh-baseline!)]
          [(and (equal? type "ok") (equal? (hash-ref j 'for #f) "compact"))
           (set! busy? #f)
           (broadcast! j)
           (refresh-baseline!)]
          [(and (equal? type "ok") (equal? (hash-ref j 'for #f) "clear"))
           (broadcast! j)
           (refresh-baseline!)]
          [(equal? type "delta") (log-delta! j size) (broadcast! j)]
          [(member type REPLAYED) (log! j size) (broadcast! j)]
          [else (broadcast! j)])])))
  (define (on-client-line! c line)
    (define j (with-handlers ([exn:fail? (lambda (_e) #f)]) (let ([v (string->jsexpr line)]) (and (hash? v) v))))
    (define type (and j (hash-ref j 'type #f)))
    (locked
     (cond
       [(equal? type "shutdown")                       ; 只断开这个客户端
        (send-to! c (hasheq 'type "bye" 'detached #t))
        (drop-client! c)]
       [else
        (cond
          [(equal? type "prompt") (set! pending-prompts (append pending-prompts (list (client-id c))))]
          ;; 会产生 session 事件的请求（resume 只在文件存在时：不存在时内核回 error，不能占队列位置）
          [(or (member type '("new_session" "fork" "session_info"))
               (and (equal? type "resume") (let ([p (hash-ref j 'path #f)]) (and (string? p) (file-exists? p)))))
           (set! session-q (append session-q (list 'client)))])
        (to-kernel! line)])))
  ;; ---- 读内核
  (define kernel-reader
    (thread (lambda ()
              (let loop ()
                (define l (read-line kout-r 'any))
                (unless (eof-object? l) (on-kernel-line! l) (loop))))))
  (define kernel-err-reader
    (thread (lambda ()
              (let loop ()
                (define l (read-line kerr-r 'any))
                (unless (eof-object? l)
                  (eprintf "[kernel] ~a\n" l)
                  (locked (set! stderr-tail (let ([t (cons l stderr-tail)]) (if (> (length t) 50) (take t 50) t))))
                  (loop))))))
  ;; ---- 接客户端
  (define (status-json)
    (hasheq 'type "serve_ok" 'name name 'pid (getpid) 'started started 'busy busy? 'clients (length clients)
            'session (if ready-ev (hash-ref ready-ev 'session "") 'null) 'log_events (length log)
            'stderr (reverse (take stderr-tail (min 10 (length stderr-tail))))))
  (define (attach! in out)
    (sync/timeout 120 (semaphore-peek-evt kernel-ready) (semaphore-peek-evt kernel-done))
    (define c (locked (set! next-id (add1 next-id)) (client next-id in out '() 0 (make-semaphore 0) #t)))
    ;; 写线程：网络慢 / 卡住不拖住内核
    (thread (lambda ()
              (with-handlers ([exn:fail? (lambda (_e) (locked (drop-client! c)))])
                (let loop ()
                  (semaphore-wait (client-sig c))
                  (define batch (locked (begin0 (reverse (client-q c)) (set-client-q! c '()) (set-client-n! c 0))))
                  (define closed?
                    (for/or ([j (in-list batch)])
                      (cond [(eq? j 'close) #t]
                            [else (write-json j out) (newline out) #f])))
                  (flush-output out)
                  (unless closed? (loop))))
              (with-handlers ([exn:fail? void]) (close-output-port out))
              (with-handlers ([exn:fail? void]) (close-input-port in))))
    (locked
     (send-to! c (hasheq 'type "serve_ok" 'client_id (client-id c)))
     (when ready-ev
       (send-to! c (hash-set* ready-ev 'attached #t 'busy busy? 'client_id (client-id c) 'server (format "~a@~a" name (gethostname)))))
     (when baseline (send-to! c baseline))
     (for ([j (in-list (reverse log))]) (send-to! c j))
     (send-to! c (hasheq 'type "replay_end"))
     (when kernel-code (send-to! c (hasheq 'type "server_exit" 'code kernel-code)))
     (touch!)
     (set! clients (append clients (list c))))
    ;; 读：客户端的请求 → 内核
    (thread (lambda ()
              (with-handlers ([exn:fail? void])
                (let loop ()
                  (define l (read-line in 'any))
                  (unless (or (eof-object? l) (not (client-alive? c)))
                    (unless (string=? (string-trim l) "") (on-client-line! c l))
                    (loop))))
              (locked (drop-client! c)))))
  (define (handle-connection in out)
    (define line (sync/timeout 5 (read-line-evt in 'linefeed)))
    (define hello (and (string? line) (with-handlers ([exn:fail? (lambda (_e) #f)]) (string->jsexpr line))))
    (define (refuse! msg)
      (with-handlers ([exn:fail? void])
        (write-json (hasheq 'type "serve_refused" 'message msg) out) (newline out) (flush-output out)
        (close-output-port out) (close-input-port in)))
    (define (reply-close! j)
      (with-handlers ([exn:fail? void])
        (write-json j out) (newline out) (flush-output out) (close-output-port out) (close-input-port in)))
    (cond
      [(not (and (hash? hello) (equal? (hash-ref hello 'pi2_serve #f) PROTOCOL))) (refuse! "bad handshake")]
      [(not (equal? (hash-ref hello 'token #f) token)) (refuse! "bad token")]
      [else
       (case (hash-ref hello 'op #f)
         [("attach") (attach! in out)]
         [("status") (reply-close! (locked (status-json)))]
         [("stop")
          (reply-close! (hasheq 'type "serve_ok" 'stopping #t))
          (locked (close-output-port kin-w))]          ; 内核读到 EOF：跑完当前轮后退出
         [else (refuse! "unknown op")])]))
  (define acceptor
    (thread (lambda ()
              (let loop ()
                (define-values (in out) (tcp-accept listener))
                (thread (lambda () (handle-connection in out)))
                (loop)))))
  (sync (semaphore-peek-evt kernel-ready) (semaphore-peek-evt kernel-done))
  (on-listening port)
  ;; ---- 空闲退出
  (when idle
    (thread (lambda ()
              (let loop ()
                (sleep (max 0.25 (min 15.0 (/ idle 4.0))))
                (define idle? (locked (and (null? clients) (not busy?) (null? pending-prompts)
                                           (> (- (current-inexact-milliseconds) last-active) (* 1000.0 idle)))))
                (cond [idle? (eprintf "pi2 serve: ~s idle for ~as, exiting\n" name idle)
                             (locked (with-handlers ([exn:fail? void]) (close-output-port kin-w)))]
                      [else (loop)])))))
  ;; ---- 等内核结束
  (sync (semaphore-peek-evt kernel-done))
  (thread-wait kernel-reader)
  (thread-wait kernel-err-reader)
  (locked (broadcast! (hasheq 'type "server_exit" 'code kernel-code))
          (for ([c (in-list clients)]) (drop-client! c)))
  (sleep 0.2)                                          ; 让写线程把 server_exit 写出去
  (kill-thread acceptor)
  (tcp-close listener)
  (when (and (file-exists? st-path) (equal? (with-handlers ([exn:fail? (lambda (_e) #f)]) (hash-ref (call-with-input-file st-path read) 'token #f)) token))
    (delete-file st-path))
  kernel-code
) ; end define serve-start!

;; ---------------------------------------------------------------- attach：stdin / stdout ↔ 守护进程

(define (copy-bytes! from to)
  (define buf (make-bytes 65536))
  (let loop ()
    (define n (read-bytes-avail! buf from))
    (unless (eof-object? n)
      (write-bytes buf to 0 n) (flush-output to)
      (loop)))
) ; end define copy-lines!

;; ---------------------------------------------------------------- spawn：没在跑就在后台起一个

(define (racket-exe)
  (define p (find-system-path 'exec-file))
  (or (and (absolute-path? p) p) (find-executable-path p) p)
) ; end define racket-exe

;; 起守护进程：新进程组（终端的 Ctrl-C / 挂断不波及），stdin = /dev/null，stdout / stderr → 日志文件，本进程退出后它照常运行。
;; 并发：{名字}.lock 独占创建，拿到锁的起进程，其余只等；等到能连上（status 应答）为止，超时 → raise
(define (serve-spawn! name #:entry [entry #f] #:args [args '()] #:idle-exit [idle #f] #:wait [wait 60])
  (unless (valid-name? name) (error 'serve "bad name: ~s (letters, digits, _ . -)" name))
  (unless (serve-alive? name)
    (define dir (serve-dir))
    (make-directory* dir)
    (file-or-directory-permissions dir #o700)
    (define lock (build-path dir (string-append name ".lock")))
    (when (and (file-exists? lock) (> (- (current-seconds) (file-or-directory-modify-seconds lock)) 120))
      (with-handlers ([exn:fail? void]) (delete-file lock)))                          ; 上次起到一半崩了留下的
    (define got-lock? (with-handlers ([exn:fail:filesystem? (lambda (_e) #f)])
                        (call-with-output-file lock #:exists 'error (lambda (out) (write (getpid) out))) #t))
    (dynamic-wind
     void
     (lambda ()
       (when (and got-lock? (not (serve-alive? name)))
         (define log (open-output-file (build-path dir (string-append name ".log")) #:exists 'append))
         (define null-in (open-input-file "/dev/null"))
         (define-values (_p _o _i _e)
           (apply subprocess log null-in 'stdout 'new (racket-exe) (path->string SELF) "start" "--name" name
                  (append (if entry (list "--entry" (path->string (path->complete-path entry))) '())
                          (if idle (list "--idle-exit" (number->string idle)) '())
                          (list "--") args)))
         (close-output-port log)
         (close-input-port null-in))
       (let loop ([t0 (current-inexact-milliseconds)])
         (cond [(serve-alive? name) (void)]
               [(> (- (current-inexact-milliseconds) t0) (* 1000.0 wait))
                (error 'serve "pi2 serve ~s did not come up within ~as (log: ~a)" name wait (build-path dir (string-append name ".log")))]
               [else (sleep 0.1) (loop t0)])))
     (lambda () (when got-lock? (with-handlers ([exn:fail? void]) (delete-file lock))))))
) ; end define serve-spawn!

(define (serve-attach! name)
  (define-values (in out ack) (serve-connect name "attach"))
  (define up (thread (lambda ()
                       (with-handlers ([exn:fail? void]) (copy-bytes! (current-input-port) out))
                       (with-handlers ([exn:fail? void]) (close-output-port out)))))   ; stdin 关了 = 断开
  (with-handlers ([exn:fail? void]) (copy-bytes! in (current-output-port)))
  (kill-thread up)
  0
) ; end define serve-attach!

;; ---------------------------------------------------------------- prompt：不开 GUI 交一个提示

;; 只打印自己这一轮（turn_start 的 prompt_from = 自己的 client_id）；→ 退出码 0 完成 / 1 出错或中断 / 3 等权限（已断开）
(define (serve-prompt! name text #:out [o (current-output-port)] #:err [e (current-error-port)])
  (define-values (in out ack) (serve-connect name "attach"))
  (define me (hash-ref ack 'client_id #f))
  (define (send! j) (write-json j out) (newline out) (flush-output out))
  (define (finish! code) (with-handlers ([exn:fail? void]) (close-output-port out) (close-input-port in)) code)
  (let loop ([mine? #f] [sent? #f])
    (define l (read-line in 'linefeed))
    (cond
      [(eof-object? l) (fprintf e "pi2 serve: connection closed\n") (finish! 1)]
      [else
       (define j (with-handlers ([exn:fail? (lambda (_e) (hasheq))]) (string->jsexpr l)))
       (define type (hash-ref j 'type #f))
       (cond
         [(and (equal? type "replay_end") (not sent?)) (send! (hasheq 'type "prompt" 'text text)) (loop mine? #t)]
         [(not sent?) (loop mine? sent?)]                                  ; 补齐的旧事件不打印
         [(and (equal? type "turn_start") (equal? (hash-ref j 'prompt_from #f) me)) (loop #t sent?)]
         [(not mine?) (loop mine? sent?)]
         [(equal? type "delta") (when (equal? (hash-ref j 'kind #f) "text") (write-string (hash-ref j 'text "") o) (flush-output o)) (loop mine? sent?)]
         [(equal? type "tool_start") (fprintf e "\n[tool ~a]\n" (hash-ref j 'name "?")) (loop mine? sent?)]
         [(equal? type "error") (fprintf e "\n[error] ~a\n" (hash-ref j 'message "?")) (loop mine? sent?)]
         [(equal? type "permission_request")
          (fprintf e "\n[waiting for permission] ~a\n(the turn waits; attach the GUI to answer)\n" (hash-ref j 'prompt ""))
          (finish! 3)]
         [(member type '("turn_complete" "goal_end"))
          (newline o) (flush-output o)
          (finish! (if (equal? (hash-ref j 'outcome "ok") "ok") 0 1))]
         [(equal? type "server_exit") (fprintf e "\npi2 serve: kernel exited ~a\n" (hash-ref j 'code "?")) (finish! 1)]
         [else (loop mine? sent?)])]))
) ; end define serve-prompt!

(define (serve-list)
  (define dir (serve-dir))
  (for/list ([f (in-list (if (directory-exists? dir) (directory-list dir) '()))]
             #:when (regexp-match? #rx"[.]rktd$" (path->string f)))
    (define name (regexp-replace #rx"[.]rktd$" (path->string f) ""))
    (cons name (serve-alive? name)))
) ; end define serve-list

(provide serve-start! serve-spawn! serve-connect serve-request serve-attach! serve-prompt! serve-list serve-alive? read-state state-path)

;; ---------------------------------------------------------------- 命令行

(module+ main
  (define name "default")
  (define entry #f)
  (define idle #f)
  (define spawn? #f)
  (define wait #f)
  (define argv (vector->list (current-command-line-arguments)))
  (define (usage!)
    (eprintf "usage: racket src/serve.rkt start|attach|prompt|status|stop|list [--name N] [--entry module.rkt] [--idle-exit secs] [--spawn] [--wait secs] [-- kernel args…]\n")
    (exit 64))
  (when (null? argv) (usage!))
  (define cmd (car argv))
  (define-values (opts rest)
    (let loop ([xs (cdr argv)] [pos '()])
      (cond
        [(null? xs) (values (reverse pos) '())]
        [(equal? (car xs) "--") (values (reverse pos) (cdr xs))]
        [(and (equal? (car xs) "--name") (pair? (cdr xs))) (set! name (cadr xs)) (loop (cddr xs) pos)]
        [(and (equal? (car xs) "--entry") (pair? (cdr xs))) (set! entry (path->complete-path (cadr xs))) (loop (cddr xs) pos)]
        [(and (equal? (car xs) "--idle-exit") (pair? (cdr xs)) (string->number (cadr xs))) (set! idle (string->number (cadr xs))) (loop (cddr xs) pos)]
        [(and (equal? (car xs) "--wait") (pair? (cdr xs)) (string->number (cadr xs))) (set! wait (string->number (cadr xs))) (loop (cddr xs) pos)]
        [(equal? (car xs) "--spawn") (set! spawn? #t) (loop (cdr xs) pos)]
        [else (loop (cdr xs) (cons (car xs) pos))])))
  (define (fail! e) (eprintf "~a\n" (exn-message e)) (exit 2))
  (with-handlers ([exn:fail? fail!])
    (case cmd
      [("start")
       (exit (or (serve-start! name #:entry (or entry PI2-MAIN) #:args (append opts rest) #:idle-exit idle
                               #:on-listening (lambda (port) (eprintf "pi2 serve: name=~a port=~a pid=~a cwd=~a\n" name port (getpid) (current-directory))))
                 0))]
      [("attach")
       (cond [spawn? (serve-spawn! name #:entry entry #:args (append opts rest) #:idle-exit idle #:wait (or wait 60))]
             [wait (let loop ([t0 (current-inexact-milliseconds)])           ; 不起，只等别人起
                     (unless (serve-alive? name)
                       (when (> (- (current-inexact-milliseconds) t0) (* 1000.0 wait)) (error 'serve "pi2 serve ~s is not running" name))
                       (sleep 0.1) (loop t0)))])
       (exit (serve-attach! name))]
      [("prompt")
       (when (null? opts) (usage!))
       (exit (serve-prompt! name (string-join opts " ")))]
      [("status") (write-json (serve-request name "status")) (newline)]
      [("stop") (serve-request name "stop") (eprintf "pi2 serve: \"~a\" stopping (finishes the current turn)\n" name)]
      [("list") (for ([p (in-list (serve-list))]) (printf "~a\t~a\n" (car p) (if (cdr p) "running" "stale")))]
      [else (usage!)])))
