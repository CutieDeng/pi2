#lang tstring racket
;; loop-live-test.rkt — M2 真机验收：gemma 完整工具循环完成真实小任务
;; 需要 LM Studio 运行于 localhost:1234。

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
 (file "../src/tools/builtin.rkt")
) ; end require

(define tmpdir (make-temporary-file "pi2-live-~a" 'directory))

;; 布置任务现场：一个藏着数字的文件
(display-to-file "The secret number is 7391.\n" (build-path tmpdir "secret.txt"))

(define cfg
  (struct-copy config (default-config)
               [workdir (path->string tmpdir)]
               [temperature 0.0]
               [max-tokens 512]
               [permission-mode 'yolo]      ; 测试环境直通
               [system-prompt "You are a helpful agent. Use the provided tools to complete tasks. Be concise."]
  ) ; end struct-copy
) ; end define cfg

(define d
  (make-deps #:provider (make-openai-provider cfg)
             #:registry (make-registry (builtin-tools cfg))
             #:bus (make-bus)
             #:policy (make-policy cfg)
  ) ; end make-deps
) ; end define d

;; 观察工具事件
(bus-subscribe! (deps-bus d)
  (lambda (e)
    (cond
      [(evt:tool-start? e)
       (define b (evt:tool-start-block e))
       (printf f"  ⏺ {(tool-use-block-name b)} {(tool-use-block-input b)}\n")
      ] ; end tool-start case
      [(evt:tool-end? e)
       (printf f"  ✓ done in {(exact-round (evt:tool-end-ms e))}ms\n")
      ] ; end tool-end case
      [else (void)]
    ) ; end cond
  ) ; end lambda
) ; end bus-subscribe!

(require racket/math)

(test-case "live: agent reads a file via tools and reports its content"
  (define st
    (run-turn! (make-initial-state cfg)
               (text-msg 'user "Read the file secret.txt and tell me the secret number.")
               d
    ) ; end run-turn!
  ) ; end define st
  (define final
    (pvector-ref (agent-state-history st)
                 (sub1 (pvector-length (agent-state-history st)))
    ) ; end pvector-ref
  ) ; end define final
  (printf f"  final answer: {(message-text final)}\n")
  (printf f"  history length: {(pvector-length (agent-state-history st))}, usage: {(agent-state-token-usage st)}\n")
  (check-true (>= (pvector-length (agent-state-history st)) 4)
              "should have at least one tool round")
  (check-true (string-contains? (message-text final) "7391"))
) ; end test-case

(test-case "live: agent creates a file via tools"
  (define st
    (run-turn! (make-initial-state cfg)
               (text-msg 'user "Create a file named greeting.txt containing exactly: hello from pi++")
               d
    ) ; end run-turn!
  ) ; end define st
  (define f (build-path tmpdir "greeting.txt"))
  (check-true (file-exists? f))
  (printf f"  file content: {(file->string f)}\n")
  (check-true (string-contains? (file->string f) "hello from pi++"))
) ; end test-case

(delete-directory/files tmpdir)
(displayln "loop-live-test: all passed")
