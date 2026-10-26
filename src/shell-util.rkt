#lang tstring racket
;; shell-util.rkt — 小工具（acceptance.rkt / test-flow.rkt 共用；不依赖 loop.rkt，给指令收集与内置工具用）

(require racket/port)

;; ---------------------------------------------------------------- shell（带时限）
(define (shell-run cmd dir #:timeout [secs 300])
  (parameterize ([current-directory dir])
    (define-values (proc out in _err) (subprocess #f #f 'stdout "/bin/zsh" "-c" cmd))
    (close-output-port in)
    (define buf (open-output-string))
    (define pump (thread (lambda () (copy-port out buf))))
    (define done (sync/timeout secs proc))
    (cond
      [(not done) (subprocess-kill proc #t) (thread-wait pump) (values 'timeout (get-output-string buf))]
      [else (thread-wait pump) (close-input-port out) (values (subprocess-status proc) (get-output-string buf))])))
(define (clip-tail s n) (if (> (string-length s) n) (string-append "…\n" (substring s (- (string-length s) n))) s))

;; glob → regexp：** 跨目录、* 不跨、? 一个字符
(define (glob->regexp g)
  (let loop ([cs (string->list g)] [acc '()])
    (cond
      [(null? cs) (pregexp (string-append "^" (apply string-append (reverse acc)) "$"))]
      [(and (char=? (car cs) #\*) (pair? (cdr cs)) (char=? (cadr cs) #\*))
       (if (and (pair? (cddr cs)) (char=? (caddr cs) #\/))
           (loop (cdddr cs) (cons "(?:.*/)?" acc))
           (loop (cddr cs) (cons ".*" acc)))]
      [(char=? (car cs) #\*) (loop (cdr cs) (cons "[^/]*" acc))]
      [(char=? (car cs) #\?) (loop (cdr cs) (cons "[^/]" acc))]
      [else (loop (cdr cs) (cons (regexp-quote (string (car cs))) acc))])))

(provide shell-run clip-tail glob->regexp)
