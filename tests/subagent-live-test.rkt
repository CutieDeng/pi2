#lang tstring racket
;; subagent-live-test.rkt — M5 真机验收：spawn_agent 委派子任务给 gemma

(require
 rackunit
 racket/string
 racket/file
 racket/pvector
 (file "../src/model.rkt")
 (file "../src/event.rkt")
 (file "../src/provider.rkt")
 (file "../src/tool.rkt")
 (file "../src/permission.rkt")
 (file "../src/loop.rkt")
 (file "../src/subagent.rkt")
 (file "../src/tools/builtin.rkt")
) ; end require

(define tmpdir (make-temporary-file "pi2-sublive-~a" 'directory))
(display-to-file "the buried token is ZK-4417\n" (build-path tmpdir "notes.txt"))

(define cfg
  (struct-copy config (default-config)
               [workdir (path->string tmpdir)]
               [temperature 0.0]
               [max-tokens 512]
               [permission-mode 'yolo]
               [system-prompt "You are pi++, an agent. When a task is self-contained, delegate it to a sub-agent via spawn_agent. Be concise."]
  ) ; end struct-copy
) ; end define cfg

(define provider (make-openai-provider cfg))
(define sub-tools (builtin-tools cfg))
(define spawn-tool
  (make-spawn-agent-tool #:provider provider #:sub-tools sub-tools)
) ; end define spawn-tool

(define parent-registry
  (make-registry (append sub-tools (list spawn-tool)))
) ; end define parent-registry

(define bus (make-bus))
(bus-subscribe! bus
  (lambda (e)
    (when (evt:tool-start? e)
      (printf f"  ⏺ {(tool-use-block-name (evt:tool-start-block e))}\n")
    ) ; end when
  ) ; end lambda
) ; end bus-subscribe!

(define d
  (make-deps #:provider provider
             #:registry parent-registry
             #:bus bus
             #:policy (make-policy cfg)
  ) ; end make-deps
) ; end define d

(test-case "live: parent delegates a file-reading subtask to a sub-agent"
  (define st
    (run-turn! (make-initial-state cfg)
               (text-msg 'user "Use spawn_agent to delegate this subtask: find the buried token inside notes.txt. Then report the token.")
               d
    ) ; end run-turn!
  ) ; end define st
  (define final
    (pvector-ref (agent-state-history st)
                 (sub1 (pvector-length (agent-state-history st)))
    ) ; end pvector-ref
  ) ; end define final
  (printf f"  final: {(message-text final)}\n")
  ;; 父历史里应出现 spawn_agent 的 tool-use
  (define used-spawn?
    (for/or ([m (in-pvector (agent-state-history st))])
      (for/or ([b (in-list (message-blocks m))])
        (and (tool-use-block? b) (string=? (tool-use-block-name b) "spawn_agent"))
      ) ; end for/or
    ) ; end for/or
  ) ; end define used-spawn?
  (check-true used-spawn? "parent should have called spawn_agent")
  (check-true (string-contains? (message-text final) "ZK-4417"))
) ; end test-case

(delete-directory/files tmpdir)
(displayln "subagent-live-test: all passed")
