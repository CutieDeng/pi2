#lang racket/base
;; kernel-place.rkt — 内核 place 的入口（在独立 OS 线程上运行，与 GUI 的主 place 不共享 Racket 线程调度）。
;; 收到的第一条消息是配置：(hasheq 'entry 模块路径 'args 参数表 'cwd 工作目录或#f)。
;; 以该模块的 main 子模块为内核入口（缺省 main.rkt + "--rpc"，与命令行启动的装配完全一致），
;; 把它的 stdin / stdout / stderr 接到进程内管道，再桥接到 place channel：
;;   GUI → 内核：jsexpr（'close = 关闭内核 stdin，内核按 R9 跑完当前轮后退出）
;;   内核 → GUI：jsexpr 事件；stderr 行 → (hasheq 'type "core_stderr" 'line l)；结束 → (hasheq 'type "core_exit" 'code n)
;; 内核里的 (exit n) 被截住，只结束本 place 的入口，不影响 GUI。
;; 注：入口模块经 --rpc 说 NDJSON，桥接时在进程内编解码一次（没有子进程）；main.rkt 暴露可调用的装配函数后可去掉这层编解码。

(require racket/place racket/port json)

(provide main)

(define (main ch)
  (define cfg (place-channel-get ch))
  (define entry (hash-ref cfg 'entry))
  (define args (hash-ref cfg 'args '()))
  (define cwd (hash-ref cfg 'cwd #f))
  (define-values (in-r in-w) (make-pipe))
  (define-values (out-r out-w) (make-pipe))
  (define-values (err-r err-w) (make-pipe))
  (define to-kernel
    (thread
     (lambda ()
       (let loop ()
         (define m (place-channel-get ch))
         (cond
           [(eq? m 'close) (close-output-port in-w)]
           [else
            (with-handlers ([exn:fail? void])
              (write-json m in-w) (newline in-w) (flush-output in-w))
            (loop)])))))
  (define from-kernel
    (thread
     (lambda ()
       (let loop ()
         (define l (read-line out-r 'any))
         (unless (eof-object? l)
           (place-channel-put ch (or (with-handlers ([exn:fail? (lambda (_e) #f)])
                                       (let ([j (string->jsexpr l)]) (and (hash? j) j)))
                                     (hasheq 'type "core_garbage" 'line l)))
           (loop))))))
  (define from-err
    (thread
     (lambda ()
       (let loop ()
         (define l (read-line err-r 'any))
         (unless (eof-object? l)
           (place-channel-put ch (hasheq 'type "core_stderr" 'line l))
           (loop))))))
  (define code
    (let/ec k
      (parameterize ([current-command-line-arguments (list->vector args)]
                     [current-input-port in-r]
                     [current-output-port out-w]
                     [current-error-port err-w]
                     [current-directory (or cwd (current-directory))]
                     [exit-handler (lambda (c) (k (if (exact-integer? c) c 0)))])
        (with-handlers ([exn:fail? (lambda (e) (eprintf "kernel crashed: ~a\n" (exn-message e)) 70)])
          (dynamic-require (list 'submod (list 'file entry) 'main) #f)
          0))))
  (close-output-port out-w)
  (close-output-port err-w)
  (thread-wait from-kernel)
  (thread-wait from-err)
  (kill-thread to-kernel)
  (place-channel-put ch (hasheq 'type "core_exit" 'code code))
) ; end define main
