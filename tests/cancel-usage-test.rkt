#lang tstring racket
;; cancel-usage-test.rkt — P4：在途请求全部可取消（并行子 agent 不再只取消最后一个）；子 agent 无 host 时回退不污染模型名；
;; 工具里（子 agent）产生的模型用量并进父会话。

(require
 rackunit
 racket/async-channel
 racket/pvector
 (file "../src/model.rkt")
 (file "../src/event.rkt")
 (file "../src/provider.rkt")
 (file "../src/tool.rkt")
 (file "../src/permission.rkt")
 (file "../src/loop.rkt")
 (file "../src/retry.rkt")
 (file "../src/secrets.rkt")
 racket/file
) ; end require

(test-case "stream-set: cancel shuts down every in-flight request, not only the last one"
  (define ss (make-stream-set))
  (define gate (make-semaphore 0))
  (define ts (for/list ([i 3]) (define c (stream-set-spawn! ss (lambda () (semaphore-wait gate)))) c))
  (check-equal? (stream-set-count ss) 3)
  (stream-set-cancel-all! ss)
  (check-equal? (stream-set-count ss) 0)
  ;; 正常结束的请求自己从表里删掉
  (stream-set-spawn! ss void)
  (sleep 0.05)
  (check-equal? (stream-set-count ss) 0))

(test-case "fallback without a host (subagent): a provider target leaves the config alone instead of becoming the model name"
  (define st (make-initial-state (struct-copy config (default-config) [model "m-sub"])))
  (check-equal? (config-model (agent-state-config (apply-fallback-target st #f "anthropic"))) "m-sub")
  (check-equal? (config-model (agent-state-config (apply-fallback-target st #f "cheaper-model"))) "cheaper-model"))

;; 假工具：模拟子 agent 在工具里调了模型（用量 100/40）
(struct fake-sub-tool ()
  #:methods gen:tool
  [(define (tool-name _t) "fake_sub")
   (define (tool-spec _t) (hasheq 'type "function" 'function (hasheq 'name "fake_sub" 'description "x" 'parameters (hasheq 'type "object" 'properties (hasheq)))))
   (define (tool-permission-level _t) 'read-only)
   (define (tool-run _t _input _ctx) (add-extra-usage! (usage 100 40)) (ok-outcome "sub done"))])

(define (scripted msgs)
  (define script (box msgs))
  (provider "mock"
            (lambda (_m _t)
              (define ch (make-async-channel))
              (define msg (car (unbox script)))
              (set-box! script (cdr (unbox script)))
              (thread (lambda ()
                        (async-channel-put ch (evt:message (now-ms) msg))
                        (async-channel-put ch (evt:turn-end (now-ms) (if (null? (message-tool-uses msg)) "stop" "tool_calls") (usage 10 5)))))
              ch)
            void))

(test-case "model usage inside tools (subagents) is added to the parent session's usage"
  (define cfg (struct-copy config (default-config) [permission-mode 'yolo]))
  (define d (make-deps #:provider (scripted (list (message 'assistant (list (tool-use-block "c1" "fake_sub" (hasheq))
                                                                            (tool-use-block "c2" "fake_sub" (hasheq))))
                                                  (text-msg 'assistant "done")))
                       #:registry (make-registry (list (fake-sub-tool)))
                       #:bus (make-bus) #:policy (make-policy cfg)))
  (define st (run-turn! (make-initial-state cfg) (text-msg 'user "go") d))
  (define u (agent-state-token-usage st))
  ;; 两次模型请求 10/5 ×2 + 两个工具里的子 agent 100/40 ×2（两个工具调用可能并行）
  (check-equal? (list (usage-input-tokens u) (usage-output-tokens u)) '(220 90)))

;; 工具输出里出现的已知密钥原值：进历史（给模型、写会话）之前换成句柄
(struct leaky-tool ()
  #:methods gen:tool
  [(define (tool-name _t) "leaky")
   (define (tool-spec _t) (hasheq 'type "function" 'function (hasheq 'name "leaky" 'description "x" 'parameters (hasheq 'type "object" 'properties (hasheq)))))
   (define (tool-permission-level _t) 'read-only)
   (define (tool-run _t _input _ctx) (ok-outcome "OPENAI_API_KEY=sk-leaked-abcdef123456 found in .env"))])

(test-case "tool output containing a registered secret reaches history only as a handle"
  (define dir (make-temporary-file "pi2-leak-~a" 'directory))
  (putenv "PI_CONFIG_HOME" (path->string dir))
  (define h (register-secret! "sk-leaked-abcdef123456" "test"))
  (define cfg (struct-copy config (default-config) [permission-mode 'yolo]))
  (define d (make-deps #:provider (scripted (list (message 'assistant (list (tool-use-block "c1" "leaky" (hasheq))))
                                                  (text-msg 'assistant "done")))
                       #:registry (make-registry (list (leaky-tool)))
                       #:bus (make-bus) #:policy (make-policy cfg)))
  (define st (run-turn! (make-initial-state cfg) (text-msg 'user "go") d))
  (define result (for*/first ([m (in-pvector (agent-state-history st))] [b (in-list (message-blocks m))] #:when (tool-result-block? b))
                   (tool-result-block-content b)))
  (check-equal? result (format "OPENAI_API_KEY=⟦secret:~a⟧ found in .env" h)))
