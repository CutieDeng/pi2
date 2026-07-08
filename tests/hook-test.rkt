#lang tstring racket
;; hook-test.rkt — pre-tool-hook 拦截单测（mock provider）

(require
 rackunit
 racket/async-channel
 racket/string
 racket/file
 racket/pvector
 (file "../src/model.rkt")
 (file "../src/event.rkt")
 (file "../src/provider.rkt")
 (file "../src/tool.rkt")
 (file "../src/permission.rkt")
 (file "../src/loop.rkt")
 (file "../src/tools/builtin.rkt")
) ; end require

(define (mock-provider script-box)
  (provider "mock"
            (lambda (_m _t)
              (define ch (make-async-channel))
              (define msg (car (unbox script-box)))
              (set-box! script-box (cdr (unbox script-box)))
              (thread
               (lambda ()
                 (async-channel-put ch (evt:message (now-ms) msg))
                 (async-channel-put ch
                                    (evt:turn-end (now-ms)
                                                  (if (null? (message-tool-uses msg)) "stop" "tool_calls")
                                                  usage-zero))
               ) ; end lambda
              ) ; end thread
              ch
            ) ; end lambda
            void
  ) ; end provider
) ; end define mock-provider

(define tmpdir (make-temporary-file "pi2-hooktest-~a" 'directory))

(test-case "pre-tool-hook blocks a tool; result carries the reason"
  (define cfg
    (struct-copy config (default-config)
                 [workdir (path->string tmpdir)]
                 [permission-mode 'yolo]
    ) ; end struct-copy
  ) ; end define cfg
  (define script
    (list
     (message 'assistant (list (tool-use-block "c1" "bash" (hasheq 'command "rm -rf /"))))
     (text-msg 'assistant "understood, stopping")
    ) ; end list
  ) ; end define script
  (define blocked (box '()))
  (define d
    (make-deps #:provider (mock-provider (box script))
               #:registry (make-registry (builtin-tools cfg))
               #:bus (make-bus)
               #:policy (make-policy cfg)
               #:pre-tool-hook
               (lambda (b)
                 (cond
                   [(and (string=? (tool-use-block-name b) "bash")
                         (regexp-match? #rx"rm -rf" (format "~a" (tool-use-block-input b))))
                    (set-box! blocked (cons (tool-use-block-id b) (unbox blocked)))
                    "dangerous rm command refused by policy hook"
                   ] ; end block case
                   [else #f]
                 ) ; end cond
               ) ; end lambda
    ) ; end make-deps
  ) ; end define d
  (define st (run-turn! (make-initial-state cfg) (text-msg 'user "clean up") d))
  ;; hook 记录了被拦截调用
  (check-equal? (unbox blocked) '("c1"))
  ;; tool-result 是 error，含拦截原因
  (define tr (car (message-blocks (pvector-ref (agent-state-history st) 2))))
  (check-true (tool-result-block-is-error? tr))
  (check-true (string-contains? (tool-result-block-content tr) "blocked by hook"))
  (check-true (string-contains? (tool-result-block-content tr) "dangerous rm"))
) ; end test-case

(delete-directory/files tmpdir)
(displayln "hook-test: all passed")
