#lang tstring racket
;; rktd.rkt — .rktd datum-log 流式读写（design.md §4.6 / §5.6）
;; 零依赖底层模块：session / permission / config 共用。

(require
 racket/pretty
 racket/file
) ; end require

;; ---------------------------------------------------------------- 写

(struct datum-log
  (path    ; path-string
   port    ; output-port (append mode)
   sema    ; semaphore — 多线程追加时 datum 不交错
  ) ; end fields
) ; end struct datum-log

(define (datum-log-open! path)
  (define-values (dir _name _dir?) (split-path (path->complete-path path)))
  (when (path? dir)
    (make-directory* dir)
  ) ; end when
  (define port (open-output-file path #:exists 'append))
  (datum-log path port (make-semaphore 1))
) ; end define datum-log-open!

(define (datum-log-append! log d)
  (call-with-semaphore
   (datum-log-sema log)
   (lambda ()
     (pretty-write d (datum-log-port log))
     (flush-output (datum-log-port log))
   ) ; end lambda
  ) ; end call-with-semaphore
) ; end define datum-log-append!

(define (datum-log-close! log)
  (close-output-port (datum-log-port log))
) ; end define datum-log-close!

;; ---------------------------------------------------------------- 读（流式）

;; 安全 read：关闭 reader 扩展；残缺尾 datum（崩溃截断）视为流结束
(define (safe-read in)
  (with-handlers ([exn:fail:read?
                   (lambda (e)
                     (log-warning f"rktd: truncated/bad datum in stream: {(exn-message e)}")
                     eof
                   ) ; end lambda
                  ]) ; end handlers
    (parameterize ([read-accept-lang #f]
                   [read-accept-reader #f])
      (read in)
    ) ; end parameterize
  ) ; end with-handlers
) ; end define safe-read

;; 惰性 datum 序列：常数内存，可提前停止。port 随序列耗尽关闭。
(define (in-datum-log path)
  (define in (open-input-file path))
  (in-port
   (lambda (p)
     (define d (safe-read p))
     (when (eof-object? d)
       (close-input-port p)
     ) ; end when
     d
   ) ; end lambda
   in
  ) ; end in-port
) ; end define in-datum-log

;; 主重放入口：确定性关闭
(define (datum-log-fold path f init)
  (call-with-input-file path
    (lambda (in)
      (let loop ([acc init])
        (define d (safe-read in))
        (if (eof-object? d)
            acc
            (loop (f d acc))
        ) ; end if
      ) ; end let loop
    ) ; end lambda
  ) ; end call-with-input-file
) ; end define datum-log-fold

;; 只读首个 datum（session-list 用），文件空/坏返回 #f
(define (datum-log-first path)
  (with-handlers ([exn:fail? (lambda (_e) #f)])
    (call-with-input-file path
      (lambda (in)
        (define d (safe-read in))
        (if (eof-object? d) #f d)
      ) ; end lambda
    ) ; end call-with-input-file
  ) ; end with-handlers
) ; end define datum-log-first

;; tail -f 语义：读到 EOF 后轮询等待新 datum；handler 返回 'stop 时退出
(define (datum-log-follow! path handler #:poll-ms [poll-ms 200])
  (call-with-input-file path
    (lambda (in)
      (let loop ()
        (define d (safe-read in))
        (cond
          [(eof-object? d)
           (sleep (/ poll-ms 1000.0))
           (loop)
          ] ; end eof case
          [(eq? (handler d) 'stop) (void)]
          [else (loop)]
        ) ; end cond
      ) ; end let loop
    ) ; end lambda
  ) ; end call-with-input-file
) ; end define datum-log-follow!

;; ---------------------------------------------------------------- provide

(provide
 datum-log?
 datum-log-path
 datum-log-open!
 datum-log-append!
 datum-log-close!
 in-datum-log
 datum-log-fold
 datum-log-first
 datum-log-follow!
) ; end provide
