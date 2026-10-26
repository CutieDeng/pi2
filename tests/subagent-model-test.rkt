#lang tstring racket
;; subagent-model-test.rkt — 父代理给子 agent 指定模型（spawn_agent 的 model 参数：别名或模型名）；
;; 帐号跟父会话（路由池里只换模型，成员不变）。

(require
 rackunit
 racket/async-channel
 racket/file
 (file "../src/model.rkt")
 (file "../src/event.rkt")
 (file "../src/provider.rkt")
 (file "../src/tool.rkt")
 (file "../src/plugin.rkt")
 (file "../src/route.rkt")
 (file "../src/subagent.rkt")
) ; end require

(define cfg-dir (make-temporary-file "pi2-submodel-~a" 'directory))
(void (putenv "PI_CONFIG_HOME" (path->string cfg-dir)))
(with-output-to-file (build-path cfg-dir "model-aliases.rktd") (lambda () (write '(("fast" "m-fast") ("strong" "m-strong")))))

;; 记下每次请求的 (provider名 模型)
(define seen '())
(define (recording-provider name)
  (provider name
            (lambda (_msgs _tools)
              (set! seen (append seen (list (list name (config-model (current-config))))))
              (define ch (make-async-channel))
              (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant "sub answer")))
              (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 1 1)))
              ch)
            void))

(define parent-cfg (struct-copy config (default-config) [model "m-parent"]))
(define (ctx) (tool-ctx "." void parent-cfg))

(test-case "the tool description lists the aliases; model = alias / id / omitted"
  (define tool (make-spawn-agent-tool #:provider (recording-provider "direct") #:sub-tools '()))
  (define desc (hash-ref (hash-ref (hash-ref (hash-ref (tool-spec tool) 'function) 'parameters) 'properties) 'model))
  (check-regexp-match #rx"fast = m-fast" (hash-ref desc 'description))
  (set! seen '())
  (parameterize ([current-config parent-cfg])
    (tool-run tool (hasheq 'task "t" 'model "fast") (ctx))
    (tool-run tool (hasheq 'task "t" 'model "custom-id") (ctx))
    (tool-run tool (hasheq 'task "t") (ctx)))
  (check-equal? (map cadr seen) '("m-fast" "custom-id" "m-parent")))

(test-case "inside a routing pool the sub-agent keeps the parent's member (account) and only changes the model"
  (define host (make-plugin-host))
  (hash-set! (plugin-host-providers host) "acct-a" (lambda (_c) (recording-provider "acct-a")))
  (hash-set! (plugin-host-providers host) "acct-b" (lambda (_c) (recording-provider "acct-b")))
  (define aff (make-affinity (route "pool" (list (rmember "acct-a" "m-a") (rmember "acct-b" "m-b")))))
  (set-affinity-index! aff 1)                                                   ; 父会话固定在 acct-b
  (define dispatch (make-dispatch-provider host parent-cfg))
  (define tool (make-spawn-agent-tool #:provider dispatch #:sub-tools '()))
  (set! seen '())
  (call-with-route aff host
                   (lambda () (parameterize ([current-config parent-cfg])
                                (tool-run tool (hasheq 'task "t" 'model "strong") (ctx))
                                (tool-run tool (hasheq 'task "t") (ctx)))))
  (check-equal? seen '(("acct-b" "m-strong") ("acct-b" "m-b"))))

(delete-directory/files cfg-dir)
