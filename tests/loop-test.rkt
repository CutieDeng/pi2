#lang racket-tstring
;; loop-test.rkt — 主循环单测（mock provider，离线确定性）

(require
 rackunit
 racket/async-channel
 racket/string
 racket/list
 racket/file
 racket/pvector
 (file "../model.rkt")
 (file "../event.rkt")
 (file "../provider.rkt")
 (file "../tool.rkt")
 (file "../permission.rkt")
 (file "../loop.rkt")
 (file "../tools/builtin.rkt")
) ; end require

;; mock provider：按脚本依次吐出预设的 assistant 消息
(define (make-mock-provider script-box)
  (provider
   "mock"
   (lambda (_msgs _tools)
     (define ch (make-async-channel))
     (define msg (car (unbox script-box)))
     (set-box! script-box (cdr (unbox script-box)))
     (thread
      (lambda ()
        (for ([b (in-list (message-blocks msg))]
              #:when (text-block? b))
          (async-channel-put ch (evt:delta (now-ms) 'text (text-block-text b)))
        ) ; end for
        (async-channel-put ch (evt:message (now-ms) msg))
        (async-channel-put ch
                           (evt:turn-end (now-ms)
                                         (if (null? (message-tool-uses msg)) "stop" "tool_calls")
                                         (usage 10 5)
                           ) ; end evt:turn-end
        ) ; end async-channel-put
      ) ; end lambda
     ) ; end thread
     ch
   ) ; end stream! lambda
   void
  ) ; end provider
) ; end define make-mock-provider

(define tmpdir (make-temporary-file "pi2-looptest-~a" 'directory))

(define (make-test-deps script #:mode [mode 'yolo])
  (define cfg
    (struct-copy config (default-config)
                 [workdir (path->string tmpdir)]
                 [permission-mode mode]
    ) ; end struct-copy
  ) ; end define cfg
  (values
   (make-deps #:provider (make-mock-provider (box script))
              #:registry (make-registry (builtin-tools cfg))
              #:bus (make-bus)
              #:policy (make-policy cfg)
   ) ; end make-deps
   cfg
  ) ; end values
) ; end define make-test-deps

;; ---------------------------------------------------------------- 纯文本轮

(test-case "plain text turn"
  (define-values (d cfg)
    (make-test-deps (list (text-msg 'assistant "hi there")))
  ) ; end define-values
  (define st (run-turn! (make-initial-state cfg) (text-msg 'user "hello") d))
  (check-equal? (pvector-length (agent-state-history st)) 2)
  (check-equal? (agent-state-turn-count st) 1)
  (check-equal? (agent-state-token-usage st) (usage 10 5))
) ; end test-case

;; ---------------------------------------------------------------- 工具轮

(test-case "tool call loop: bash then answer"
  (define-values (d cfg)
    (make-test-deps
     (list
      (message 'assistant
               (list (tool-use-block "c1" "bash" (hasheq 'command "echo 42"))
               ) ; end list
      ) ; end message
      (text-msg 'assistant "the answer is 42")
     ) ; end list
    ) ; end make-test-deps
  ) ; end define-values
  ;; 记录事件流
  (define events (box '()))
  (bus-subscribe! (deps-bus d)
    (lambda (e)
      (set-box! events (cons e (unbox events)))
    ) ; end lambda
  ) ; end bus-subscribe!
  (define st (run-turn! (make-initial-state cfg) (text-msg 'user "what is 6*7? use bash") d))
  ;; history: user / assistant(tool-use) / user(tool-result) / assistant(text)
  (check-equal? (pvector-length (agent-state-history st)) 4)
  (define tr-msg (pvector-ref (agent-state-history st) 2))
  (define tr (car (message-blocks tr-msg)))
  (check-true (tool-result-block? tr))
  (check-equal? (tool-result-block-tool-use-id tr) "c1")
  (check-false (tool-result-block-is-error? tr))
  (check-true (string-contains? (tool-result-block-content tr) "42"))
  ;; usage 累计了两次调用
  (check-equal? (agent-state-token-usage st) (usage 20 10))
  ;; 事件流里有 tool-start / tool-end
  (bus-drain! (deps-bus d))
  (define evs (reverse (unbox events)))
  (check-true (ormap evt:tool-start? evs))
  (check-true (ormap evt:tool-end? evs))
) ; end test-case

;; ------------------------------------------------------------ 错误与拒绝

(test-case "unknown tool and denied tool become error results"
  (define-values (d cfg)
    (make-test-deps
     (list
      (message 'assistant (list (tool-use-block "c1" "no_such_tool" (hasheq))))
      (message 'assistant (list (tool-use-block "c2" "bash" (hasheq 'command "echo x"))))
      (text-msg 'assistant "done")
     ) ; end list
     #:mode 'normal                        ; bash dangerous → ask → asker 默认 no
    ) ; end make-test-deps
  ) ; end define-values
  (define st (run-turn! (make-initial-state cfg) (text-msg 'user "go") d))
  (define tr1 (car (message-blocks (pvector-ref (agent-state-history st) 2))))
  (check-true (tool-result-block-is-error? tr1))
  (check-true (string-contains? (tool-result-block-content tr1) "unknown tool"))
  (define tr2 (car (message-blocks (pvector-ref (agent-state-history st) 4))))
  (check-true (tool-result-block-is-error? tr2))
  (check-true (string-contains? (tool-result-block-content tr2) "denied"))
) ; end test-case

;; ------------------------------------------------------------ 预算护栏

(test-case "turn budget: injects notice and forces final answer"
  (define call-forever
    (message 'assistant (list (tool-use-block "cx" "bash" (hasheq 'command "echo loop"))))
  ) ; end define call-forever
  (define-values (d cfg0)
    (make-test-deps
     ;; 1-3 条被执行（ncalls=3），第 4 条触发预算，收尾流取第 5 条
     (append (make-list 4 call-forever)
             (list (text-msg 'assistant "forced final"))
     ) ; end append
    ) ; end make-test-deps
  ) ; end define-values
  (define cfg
    (struct-copy config cfg0 [turn-max-calls 3])
  ) ; end define cfg
  (define st (run-turn! (make-initial-state cfg) (text-msg 'user "loop forever") d))
  (define last-msg
    (pvector-ref (agent-state-history st)
                 (sub1 (pvector-length (agent-state-history st)))
    ) ; end pvector-ref
  ) ; end define last-msg
  (check-equal? (message-text last-msg) "forced final")
  ;; 倒数第二条是预算提示（含 error tool-result + 文本）
  (define notice
    (pvector-ref (agent-state-history st)
                 (- (pvector-length (agent-state-history st)) 2)
    ) ; end pvector-ref
  ) ; end define notice
  (check-true (string-contains? (message-text notice) "budget"))
) ; end test-case

;; ------------------------------------------------------------ provider 错误

(test-case "provider error propagates as exception"
  (define err-provider
    (provider "err"
              (lambda (_m _t)
                (define ch (make-async-channel))
                (thread
                 (lambda ()
                   (async-channel-put ch
                                      (evt:error (now-ms) (make-exn:fail "boom" (current-continuation-marks)) #f)
                   ) ; end async-channel-put
                 ) ; end lambda
                ) ; end thread
                ch
              ) ; end lambda
              void
    ) ; end provider
  ) ; end define err-provider
  (define cfg (default-config))
  (define d
    (make-deps #:provider err-provider
               #:registry (make-registry '())
               #:bus (make-bus)
               #:policy (make-policy cfg)
    ) ; end make-deps
  ) ; end define d
  (check-exn exn:fail?
    (lambda ()
      (run-turn! (make-initial-state cfg) (text-msg 'user "x") d)
    ) ; end lambda
  ) ; end check-exn
) ; end test-case

(delete-directory/files tmpdir)
(displayln "loop-test: all passed")
