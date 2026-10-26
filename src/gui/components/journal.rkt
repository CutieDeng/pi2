#lang racket/base
;; journal.rkt — 操作日志组件（G6）：按发生顺序记录窗口事件、内核事件、发给内核的请求、内部错误，
;; 写到日志目录（默认 ~/Library/Logs/pi2/），供崩溃排查与回放（--replay）。内存里另留最近 400 条给调试面板。
;; 每条写完即 flush：Racket 侧异常在处理器里落盘；原生 abort 时最多丢正在写的那一条。
;; 日志里有用户输入的文字与粘贴内容、模型输出：只写本机文件，不外发；--no-journal 关闭落盘。
;; needs （无）→ provides journal
;; config：'dir（目录；#f = 只在内存）'keep（保留最近几份，默认 20）'header（hash，写进首行）

(require racket/list racket/file
         "../compute/journal-format.rkt" "../compute/hub.rkt" "../compute/interfaces.rkt")

(define RING 400)
(define ERRORS-KEPT 20)

(struct jn ([port #:mutable] path t0 [seq #:mutable] ring [ring-n #:mutable] [errors #:mutable]))

;; 目录里只留最近 keep-1 份（加上这次新开的一份 = keep）
(define (rotate! dir keep)
  (define olds (sort (for/list ([f (in-list (directory-list dir #:build? #t))]
                                #:when (regexp-match? #rx"^journal-.*[.]rktd$" (path->string (file-name-from-path* f)))) f)
                     > #:key (lambda (f) (file-or-directory-modify-seconds f))))
  (for ([f (in-list (if (> (length olds) (sub1 keep)) (drop olds (sub1 keep)) '()))])
    (with-handlers ([exn:fail? void]) (delete-file f)))
) ; end define rotate!

(define (file-name-from-path* p) (let-values ([(_b name _d) (split-path p)]) name))

(define (stamp)
  (define d (seconds->date (current-seconds)))
  (define (p2 n) (if (< n 10) (format "0~a" n) (format "~a" n)))
  (format "~a~a~a-~a~a~a-~a" (date-year d) (p2 (date-month d)) (p2 (date-day d))
          (p2 (date-hour d)) (p2 (date-minute d)) (p2 (date-second d)) (random 10000))
) ; end define stamp

(define (open-journal dir keep header)
  (define path
    (and dir
         (with-handlers ([exn:fail? (lambda (e) (eprintf "pi2 GUI: journal disabled (~a)\n" (exn-message e)) #f)])
           (make-directory* dir)
           (rotate! dir keep)
           (build-path dir (format "journal-~a.rktd" (stamp))))))
  (define port (and path (open-output-file path #:exists 'truncate)))
  (when port
    (write (journal-header-datum header) port) (newline port) (flush-output port))
  (jn port path (current-inexact-milliseconds) 0 (make-vector RING #f) 0 '())
) ; end define open-journal

(define (record! j kind payload)
  (define t (inexact->exact (round (- (current-inexact-milliseconds) (jn-t0 j)))))
  (define seq (jn-seq j))
  (set-jn-seq! j (add1 seq))
  (unless (eq? kind 'frame)                                              ; 重绘标记只进文件（光标闪烁每秒 2 条，面板里没用）
    (vector-set! (jn-ring j) (modulo (jn-ring-n j) RING) (vector t kind (record-summary kind payload)))
    (set-jn-ring-n! j (add1 (jn-ring-n j))))
  (when (eq? kind 'error)
    (set-jn-errors! j (take (cons payload (jn-errors j)) (min ERRORS-KEPT (add1 (length (jn-errors j)))))))
  (define port (jn-port j))
  (when port
    (with-handlers ([exn:fail? (lambda (e)                                ; 磁盘满 / 被删：停写，不影响界面
                                 (set-jn-port! j #f)
                                 (eprintf "pi2 GUI: journal write failed, stopped (~a)\n" (exn-message e)))])
      (write (list t seq kind payload) port) (newline port) (flush-output port)))
) ; end define record!

;; 事后清掉原文（选中文字标记为密钥）：内存里的条目、本次的日志文件（读出全部记录、替换、原子写回、接着追加）。
;; 只处理本次的文件；之前轮转留下的旧日志不动
(define (redact! j needle rep)
  (for ([i (in-range (vector-length (jn-ring j)))])
    (define v (vector-ref (jn-ring j) i))
    (when v (vector-set! (jn-ring j) i (redact-datum v needle rep))))
  (define path (jn-path j))
  (when (and path (jn-port j))
    (with-handlers ([exn:fail? (lambda (e) (eprintf "pi2 GUI: journal redact failed (~a)\n" (exn-message e)))])
      (close-output-port (jn-port j))
      (define ds (call-with-input-file path
                   (lambda (in) (parameterize ([read-accept-reader #f])
                                  (let loop ([acc '()]) (define d (read in)) (if (eof-object? d) (reverse acc) (loop (cons d acc))))))))
      (define tmp (path-add-extension path #".tmp"))
      (call-with-output-file tmp #:exists 'truncate
        (lambda (out) (for ([d (in-list ds)]) (write (redact-datum d needle rep) out) (newline out))))
      (rename-file-or-directory tmp path #t)
      (set-jn-port! j (open-output-file path #:exists 'append))))
) ; end define redact!

(define (tail j n)
  (define total (jn-ring-n j))
  (for/list ([i (in-range (max 0 (- total (min n RING))) total)])
    (vector-ref (jn-ring j) (modulo i RING)))
) ; end define tail

(define journal-component
  (component 'journal '() (list journal)
             (lambda (use config)
               (define j (open-journal (hash-ref config 'dir #f) (hash-ref config 'keep 20) (hash-ref config 'header (hasheq))))
               (list (implement journal
                                'record! (lambda (kind payload) (record! j kind payload))
                                'path (lambda () (jn-path j))
                                'tail (lambda (n) (tail j n))
                                'errors (lambda () (jn-errors j))
                                'flush! (lambda () (when (jn-port j) (flush-output (jn-port j))))
                                'redact! (lambda (needle rep) (redact! j needle rep))
                                'close! (lambda () (when (jn-port j) (close-output-port (jn-port j)) (set-jn-port! j #f))))))))

(provide journal-component)
