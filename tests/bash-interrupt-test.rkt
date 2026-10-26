#lang racket/base
;; bash-interrupt-test.rkt — 轮被中断（break）时 bash 工具的子进程（含复合命令的孙进程）必须结束

(require rackunit racket/system racket/port racket/string
         (file "../src/tool.rkt")
         (file "../src/tools/bash.rkt")
         (file "../src/tools/bash-persistent.rkt"))

(define (running? tag)
  (not (string=? "" (string-trim (with-output-to-string
                                   (lambda () (system (format "pgrep -f '~a' || true" tag))))))))

(define (break-during cmd tag #:tool [t (make-bash-tool)])
  (define ctx (tool-ctx (path->string (current-directory)) void #f))
  (define th (thread (lambda () (with-handlers ([exn:break? void]) (tool-run t (hasheq 'command cmd) ctx)))))
  (let wait ([n 0]) (unless (or (running? tag) (> n 100)) (sleep 0.02) (wait (add1 n))))
  (check-true (running? tag) "command should be running before the break")
  (break-thread th)
  (thread-wait th)
  (sleep 0.2)
  (begin0 (running? tag) (void (system (format "pkill -f '~a'" tag)))))

(test-case "single command is killed on break"
  (check-false (break-during "sleep 41.7171" "sleep 41.7171")))

(test-case "grandchild of a compound command is killed on break (process group)"
  (check-false (break-during "sleep 42.7272; echo after" "sleep 42.7272")))

(test-case "persistent bash: command (and its grandchildren) killed on break"
  (check-false (break-during "sleep 43.7373; echo after" "sleep 43.7373" #:tool (make-persistent-bash-tool))))
