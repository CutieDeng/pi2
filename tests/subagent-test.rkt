#lang tstring racket
;; subagent-test.rkt — spawn_agent 离线单测（mock provider，无真模型）。
;; 覆盖此前只有 live 测试的部分：结果返回、子上下文收紧（建议③）、depth-1 剔除。

(require
 rackunit
 racket/async-channel
 racket/file
 racket/list
 (file "../src/model.rkt")
 (file "../src/event.rkt")
 (file "../src/provider.rkt")
 (file "../src/tool.rkt")
 (file "../src/subagent.rkt")
 (file "../src/tools/builtin.rkt")
) ; end require

(define tmpdir (make-temporary-file "pi2-subtest-~a" 'directory))

(define parent-cfg
  (struct-copy config (default-config)
               [workdir (path->string tmpdir)]
               [max-tokens 4096] [context-budget 8192] [permission-mode 'normal]))

;; mock 子 provider：记录它看到的 (current-config) 与 tool-specs，回一段固定文本。
(define seen-cfg (box #f))
(define seen-specs (box '()))
(define (mock-sub reply)
  (provider "mock-sub"
    (lambda (_msgs tool-specs)
      (set-box! seen-cfg (current-config))
      (set-box! seen-specs tool-specs)
      (define ch (make-async-channel))
      (thread (lambda ()
        (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant reply)))
        (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 1 1)))))
      ch)
    void)
) ; end define mock-sub

(define (run-spawn spawn cfg #:task [task "do it"])
  (tool-run spawn (hasheq 'task task)
            (tool-ctx (path->string tmpdir) (lambda (_e) (void)) cfg))
) ; end define run-spawn

(define (spec-names) (map (lambda (s) (hash-ref (hash-ref s 'function) 'name)) (unbox seen-specs)))

;; ------------------------------------------------------------ 基本委派

(test-case "spawn_agent runs a sub-turn and returns its final text; sub uses SUB-SYSTEM + yolo"
  (define spawn (make-spawn-agent-tool #:provider (mock-sub "sub answer")
                                       #:sub-tools (builtin-tools parent-cfg)))
  (define oc (run-spawn spawn parent-cfg #:task "find X"))
  (check-false (tool-outcome-is-error? oc))
  (check-equal? (tool-outcome-content oc) "sub answer")
  (define sc (unbox seen-cfg))
  (check-true (string-contains? (config-system-prompt sc) "sub-agent"))   ; SUB-SYSTEM
  (check-equal? (config-permission-mode sc) 'yolo)
) ; end test-case

(test-case "missing task param → error outcome"
  (define spawn (make-spawn-agent-tool #:provider (mock-sub "x")
                                       #:sub-tools (builtin-tools parent-cfg)))
  (define oc (tool-run spawn (hasheq)
                       (tool-ctx (path->string tmpdir) (lambda (_e) (void)) parent-cfg)))
  (check-true (tool-outcome-is-error? oc))
  (check-true (string-contains? (tool-outcome-content oc) "task"))
) ; end test-case

;; ------------------------------------------------------------ 上下文收紧（建议③）

(test-case "sub context tightened: max-tokens/context-budget = min(parent, cap)"
  (define spawn (make-spawn-agent-tool #:provider (mock-sub "ok")
                                       #:sub-tools (builtin-tools parent-cfg)))
  (run-spawn spawn parent-cfg)
  (define sc (unbox seen-cfg))
  (check-equal? (config-max-tokens sc) 1024)          ; min(4096, 1024)
  (check-equal? (config-context-budget sc) 4096)      ; min(8192, 4096)
) ; end test-case

(test-case "sub caps never exceed an already-smaller parent"
  (define small (struct-copy config parent-cfg [max-tokens 256] [context-budget 2048]))
  (define spawn (make-spawn-agent-tool #:provider (mock-sub "ok")
                                       #:sub-tools (builtin-tools parent-cfg)))
  (run-spawn spawn small)
  (define sc (unbox seen-cfg))
  (check-equal? (config-max-tokens sc) 256)           ; min(256, 1024)
  (check-equal? (config-context-budget sc) 2048)      ; min(2048, 4096)
) ; end test-case

(test-case "custom caps honored"
  (define spawn (make-spawn-agent-tool #:provider (mock-sub "ok")
                                       #:sub-tools (builtin-tools parent-cfg)
                                       #:max-tokens 512 #:context-budget 3000))
  (run-spawn spawn parent-cfg)
  (define sc (unbox seen-cfg))
  (check-equal? (config-max-tokens sc) 512)
  (check-equal? (config-context-budget sc) 3000)
) ; end test-case

;; ------------------------------------------------------------ depth-1

(test-case "depth-1: sub-agent's toolset excludes spawn_agent, keeps the rest"
  (define inner (make-spawn-agent-tool #:provider (mock-sub "inner")
                                       #:sub-tools (builtin-tools parent-cfg)))
  ;; 把一个名为 spawn_agent 的工具塞进外层子工具集，应被剔除
  (define outer (make-spawn-agent-tool #:provider (mock-sub "outer done")
                                       #:sub-tools (cons inner (builtin-tools parent-cfg))))
  (run-spawn outer parent-cfg)
  (define names (spec-names))
  (check-false (member "spawn_agent" names))          ; 递归自派生被阻断
  (check-true (and (member "bash" names) #t))          ; 其余工具保留
  (check-true (and (member "read_file" names) #t))
) ; end test-case

(delete-directory/files tmpdir)
(displayln "subagent-test: all passed")
