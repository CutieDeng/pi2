#lang racket/base
;; kernels.rkt — 内核连接组件（一窗多主机）：同时连多个 pi2 内核，每个有一个主机 id（"local"、服务器等）。
;; 两种传输（按 add! 的配置选）：
;;   'command 参数表 → 传输子进程（serve.rkt attach：本机守护进程 / ssh 到服务器），经它的 stdin / stdout 说 RPC v2 NDJSON；
;;     关闭 = 关它的 stdin = 断开（守护进程里的内核照常跑）；结束 → core_exit 带 'remote #t（有 server_exit 时 code 用它的）
;;   否则 → 本进程的独立 place（kernel-place.rkt；'entry 缺省 pi2 main.rkt，'args，'cwd）；关闭 = 关内核 stdin，跑完当前轮后退出
;; 实测（design-gui.md §11.2）：内核不能和显示同 place（内核忙时 100% 帧超预算）；独立 place / 独立进程都不受影响。
;; provides kernel-set：add! (id 配置)、remove! (id [超时]) → 退出码、ids ()、send! (id j) → bool、
;;   poll! () → ((id . 事件) …)、alive? (id)、stderr-tail (id [n])、close-all! ([超时])

(require racket/place racket/list racket/runtime-path json "../compute/hub.rkt" "../compute/interfaces.rkt")

(define-runtime-path KERNEL-PLACE "kernel-place.rkt")
(define-runtime-path PI2-MAIN "../../../main.rkt")

(define STDERR-KEEP 200)

;; 启动时的环境变量快照：传输子进程（含断线重连时）都用它。内核 place 与 GUI 同一进程，place 里的 putenv（脚本化内核会改
;; PI_CONFIG_HOME）是进程级的，不用快照的话之后起的子进程会去错的配置目录找 / 起守护进程。
(define initial-env (environment-variables-copy (current-environment-variables)))

;; 一个连接 = 五个闭包
(struct conn (send! poll! close! alive? stderr-tail))

(define (keep-line lines l) (take (cons l lines) (min STDERR-KEEP (add1 (length lines)))))

;; ---------------------------------------------------------------- place 传输

(define (place-conn config)
  (define p (dynamic-place KERNEL-PLACE 'main))
  (place-channel-put p (hasheq 'entry (path->string (simplify-path (hash-ref config 'entry PI2-MAIN)))
                               'args (hash-ref config 'args '("--rpc"))
                               'cwd (let ([c (hash-ref config 'cwd #f)]) (and c (path->string (path->complete-path c))))))
  (define stderr '())
  (define exited #f)
  (define (poll!)
    (let loop ([acc '()])
      (define m (sync/timeout 0 p))
      (cond
        [(not m)
         ;; place 非正常结束（没发 core_exit）也要报告
         (cond [(and (not exited) (sync/timeout 0 (place-dead-evt p)))
                (set! exited (place-wait p))
                (reverse (cons (hasheq 'type "core_exit" 'code exited) acc))]
               [else (reverse acc)])]
        [(equal? (hash-ref m 'type #f) "core_stderr") (set! stderr (keep-line stderr (hash-ref m 'line ""))) (loop acc)]
        [(equal? (hash-ref m 'type #f) "core_exit") (set! exited (hash-ref m 'code 0)) (loop (cons m acc))]
        [else (loop (cons m acc))])))
  (conn (lambda (j) (and (not exited) (begin (place-channel-put p j) #t)))
        poll!
        (lambda ([timeout 5.0])
          (unless exited
            (place-channel-put p 'close)
            (unless (sync/timeout timeout (place-dead-evt p)) (place-kill p)))
          (or exited (place-wait p)))
        (lambda () (and (not exited) (not (sync/timeout 0 (place-dead-evt p)))))
        (lambda ([n 20]) (reverse (take stderr (min n (length stderr))))))
) ; end define place-conn

;; ---------------------------------------------------------------- 传输子进程

(define (command-conn config)
  (define argv (hash-ref config 'command))
  (define exe (or (find-executable-path (car argv)) (string->path (car argv))))
  (define cwd (hash-ref config 'cwd #f))
  (define-values (proc from to err)
    (parameterize ([current-directory (if cwd (path->complete-path cwd) (current-directory))]
                   [current-environment-variables (environment-variables-copy initial-env)])
      (apply subprocess #f #f #f exe (cdr argv))))
  (define lock (make-semaphore 1))
  (define inbox '())                                   ; 倒序
  (define stderr '())
  (define server-code #f)
  (define exited #f)
  (define (take-inbox!) (call-with-semaphore lock (lambda () (begin0 (reverse inbox) (set! inbox '())))))
  (define reader
    (thread (lambda ()
              (let loop ()
                (define l (with-handlers ([exn:fail? (lambda (_e) eof)]) (read-line from 'any)))
                (unless (eof-object? l)
                  (define j (or (with-handlers ([exn:fail? (lambda (_e) #f)])
                                  (let ([v (string->jsexpr l)]) (and (hash? v) v)))
                                (hasheq 'type "core_garbage" 'line l)))
                  (when (equal? (hash-ref j 'type #f) "server_exit") (set! server-code (hash-ref j 'code #f)))
                  (call-with-semaphore lock (lambda () (set! inbox (cons j inbox))))
                  (loop))))))
  (void (thread (lambda ()
                  (let loop ()
                    (define l (with-handlers ([exn:fail? (lambda (_e) eof)]) (read-line err 'any)))
                    (unless (eof-object? l)
                      (call-with-semaphore lock (lambda () (set! stderr (keep-line stderr l))))
                      (loop))))))
  (define (poll!)
    (define msgs (take-inbox!))
    (cond
      [(and (not exited) (thread-dead? reader) (not (eq? (subprocess-status proc) 'running)))
       (define rest (take-inbox!))
       (set! exited (or server-code (subprocess-status proc)))
       (append msgs rest (list (hasheq 'type "core_exit" 'code exited 'remote #t)))]
      [else msgs]))
  (conn (lambda (j)
          (and (not exited)
               (with-handlers ([exn:fail? (lambda (_e) #f)])
                 (write-json j to) (newline to) (flush-output to) #t)))
        poll!
        (lambda ([timeout 5.0])
          (with-handlers ([exn:fail? void]) (close-output-port to))
          (unless (sync/timeout timeout proc) (subprocess-kill proc #t))
          (or exited (subprocess-status proc)))
        (lambda () (and (not exited) (eq? (subprocess-status proc) 'running)))
        (lambda ([n 20]) (call-with-semaphore lock (lambda () (reverse (take stderr (min n (length stderr))))))))
) ; end define command-conn

;; ---------------------------------------------------------------- 组件

(define kernels-component
  (component 'kernels '() (list kernel-set)
             (lambda (use config)
               (define conns (make-hash))                ; id → conn
               (define order '())                       ; 加入顺序（poll! 按它轮询）
               (define (get id) (hash-ref conns id #f))
               (define (remove! id [timeout 5.0])
                 (define c (get id))
                 (begin0 (and c ((conn-close! c) timeout))
                         (hash-remove! conns id)
                         (set! order (remove id order))))
               (list (implement kernel-set
                                'add! (lambda (id cfg)
                                        (when (get id) (remove! id 1.0))
                                        (hash-set! conns id (if (hash-ref cfg 'command #f) (command-conn cfg) (place-conn cfg)))
                                        (set! order (append order (list id))))
                                'remove! remove!
                                'ids (lambda () order)
                                'send! (lambda (id j) (let ([c (get id)]) (and c ((conn-send! c) j))))
                                'poll! (lambda () (for*/list ([id (in-list order)] [ev (in-list ((conn-poll! (get id))))]) (cons id ev)))
                                'alive? (lambda (id) (let ([c (get id)]) (and c ((conn-alive? c)))))
                                'stderr-tail (lambda (id [n 20]) (let ([c (get id)]) (if c ((conn-stderr-tail c) n) '())))
                                'close-all! (lambda ([timeout 5.0])
                                              (for/list ([id (in-list order)]) (cons id (remove! id timeout)))))))))

(provide kernels-component)
