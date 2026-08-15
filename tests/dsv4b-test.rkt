#lang tstring racket
;; dsv4b-test.rkt — dsv4-b 锚定档案与两阶段促迁（design-dsv4b.md §5.1）。

(require
 rackunit
 racket/file
 racket/string
 (file "../src/model.rkt")
 (file "../src/tool.rkt")
 (file "../src/plugin.rkt")
 (file "../src/providers.rkt")
 (file "../src/credentials.rkt")
 (file "../src/auto.rkt")
 (file "../src/escalate.rkt")
 (file "../src/dsv4b.rkt")
 (file "../src/tools/editor.rkt")
 (file "../src/tools/bash-persistent.rkt")
 (file "../src/tools/file.rkt")
) ; end require

;; 隔离凭据目录（同 providers-test 惯例）
(putenv "PI_CONFIG_HOME" (path->string (make-temporary-file "pi2-dsv4b-~a" 'directory)))

(define (make-host-on name)
  (define host (make-plugin-host))
  (register-builtin-providers! host)
  (check-true (host-set-provider! host name))
  host
) ; end define make-host-on

;; 每用例复位进程级促迁状态
(define (with-clean-dsv4b thunk)
  (dynamic-wind dsv4b-reset! thunk dsv4b-reset!)
) ; end define with-clean-dsv4b

;; ------------------------------------------------------------ 档案

(test-case "dsv4-b 档案：anthropic 线路、deepseek 端点、默认 v4-pro、共享 key-env"
  (define p (profile-by-name "dsv4-b"))
  (check-not-false p)
  (check-equal? (provider-profile-kind p) 'anthropic)
  (check-equal? (provider-profile-endpoint p) "https://api.deepseek.com/anthropic")
  (check-equal? (provider-profile-model p) "deepseek-v4-pro")
  (check-equal? (provider-profile-key-env-of "dsv4-b") "DEEPSEEK_API_KEY")
  (define host (make-plugin-host))
  (register-builtin-providers! host)
  (check-not-false (member "dsv4-b" (host-available-providers host)))
) ; end test-case

(test-case "apply-provider-profile：写入端点/模型；default 标签共享 deepseek 存量 token"
  (putenv "DEEPSEEK_API_KEY" "")
  (store-instance-key! "deepseek" "default" "sk-shared-anchor")
  (check-equal? (resolve-provider-token "dsv4-b" "default") "sk-shared-anchor")
  (define c (apply-provider-profile (default-config) "dsv4-b"))
  (check-equal? (config-endpoint c) "https://api.deepseek.com/anthropic")
  (check-equal? (config-model c) "deepseek-v4-pro")
  (check-equal? (config-api-key c) "sk-shared-anchor")
) ; end test-case

;; ------------------------------------------------------------ gating

(test-case "base ≠ deepseek ⇒ auto / 升级梯天然不生效；dsv4b-active? 恰在 dsv4-b 上真"
  (check-false (string=? (provider-base-name "dsv4-b") "deepseek"))
  (check-false (string=? (provider-base-name "dsv4-b[work]") "deepseek"))
  (define h (make-host-on "dsv4-b"))
  (check-false (auto-active? h))
  (check-false (escalation-active? h))
  (check-true (dsv4b-active? h))
  (check-true (dsv4b-active? (make-host-on "dsv4-b[work]")))
  (check-false (dsv4b-active? (make-host-on "deepseek")))
  (check-false (dsv4b-active? #f))
  ;; 回归：deepseek 本体的 auto gate 不受影响
  (check-true (auto-active? (make-host-on "deepseek")))
) ; end test-case

;; ------------------------------------------------------------ 促迁

(define (anchor-registry)
  (make-registry (list (make-persistent-bash-tool) (make-editor-tool)))
) ; end define anchor-registry

(define (anchor-state)
  (make-initial-state (struct-copy config (default-config) [system-prompt ANCHOR-SYSTEM]))
) ; end define anchor-state

(define (spec-names reg) (sort (map tool-name (registry-tools reg)) string<?))

(test-case "锚定态 registry 恰两个 schema（首请求纯净性——锚定的命门）"
  (check-equal? (spec-names (anchor-registry)) '("bash" "str_replace_editor"))
) ; end test-case

(test-case "促迁：tool-call 触发 → registry 灌全量、解锁段追加、锚定句保持在最前；不可逆"
  (with-clean-dsv4b
   (lambda ()
     (define reg (anchor-registry))
     (define host (make-host-on "dsv4-b"))
     (define addendum (dsv4b-unlock-addendum "" ""))
     (set-dsv4b-payload!
      (lambda () (registry-add! reg (make-read-file-tool)) addendum))
     ;; 默认 promote-on='tool-call：纯文本回复不促迁
     (define-values (st0 n0) (dsv4b-maybe-promote! host (anchor-state) 'assistant-message))
     (check-false n0)
     (check-false (dsv4b-promoted?))
     (check-equal? (spec-names reg) '("bash" "str_replace_editor"))
     ;; 工具调用促迁
     (define-values (st1 n1) (dsv4b-maybe-promote! host st0 'tool-call))
     (check-not-false n1)
     (check-true (dsv4b-promoted?))
     (check-equal? (spec-names reg) '("bash" "read_file" "str_replace_editor"))
     (define sys (config-system-prompt (agent-state-config st1)))
     (check-true (string-prefix? sys ANCHOR-SYSTEM))          ; 锚定句仍在最前
     (check-true (string-contains? sys UNLOCK-MARKER))
     ;; 幂等/不可逆：再触发为 no-op
     (define-values (st2 n2) (dsv4b-maybe-promote! host st1 'tool-call))
     (check-false n2)
     (check-eq? st2 st1)))
) ; end test-case

(test-case "促迁：either 模式下首条纯文本回复亦促迁"
  (with-clean-dsv4b
   (lambda ()
     (set-dsv4b-promote-on! 'either)
     (define host (make-host-on "dsv4-b"))
     (set-dsv4b-payload! (lambda () (dsv4b-unlock-addendum "" "")))
     (define-values (_st n) (dsv4b-maybe-promote! host (anchor-state) 'assistant-message))
     (check-not-false n)))
) ; end test-case

(test-case "促迁前提：payload 未设 / provider 非 dsv4-b → 永不触发"
  (with-clean-dsv4b
   (lambda ()
     (define host (make-host-on "dsv4-b"))
     (define-values (_s1 n1) (dsv4b-maybe-promote! host (anchor-state) 'tool-call))
     (check-false n1)                                         ; payload 未设
     (set-dsv4b-payload! (lambda () "x"))
     (define-values (_s2 n2) (dsv4b-maybe-promote! (make-host-on "deepseek") (anchor-state) 'tool-call))
     (check-false n2)))                                       ; 非 dsv4-b
) ; end test-case

(test-case "resume 幂等：system-prompt 已含解锁段 → 只重放 registry 副作用，不重复追加"
  (with-clean-dsv4b
   (lambda ()
     (define reg (anchor-registry))
     (define host (make-host-on "dsv4-b"))
     (set-dsv4b-payload!
      (lambda () (registry-add! reg (make-read-file-tool)) (dsv4b-unlock-addendum "" "")))
     (define archived-sys (string-append ANCHOR-SYSTEM (dsv4b-unlock-addendum "sk" "pj")))
     (define st (make-initial-state
                 (struct-copy config (default-config) [system-prompt archived-sys])))
     (define-values (st* n) (dsv4b-maybe-promote! host st 'tool-call))
     (check-not-false n)
     (check-equal? (spec-names reg) '("bash" "read_file" "str_replace_editor"))
     (check-equal? (config-system-prompt (agent-state-config st*)) archived-sys)))
) ; end test-case

(test-case "history-has-tool-call?：assistant 工具调用为真；纯文本为假"
  (check-false (history-has-tool-call? (list (text-msg 'user "hi") (text-msg 'assistant "yo"))))
  (check-true (history-has-tool-call?
               (list (text-msg 'user "hi")
                     (message 'assistant (list (tool-use-block "1" "bash" (hasheq)))))))
) ; end test-case

(test-case "解锁段：提及全量工具、含技能/项目指令注入"
  (define add (dsv4b-unlock-addendum "\n\n## Available skills\n- x" "\n\n## Project instructions\nY"))
  (check-true (string-prefix? add (string-append "\n\n" UNLOCK-MARKER)))
  (for ([n (in-list '("read_file" "write_file" "edit_file" "glob" "grep" "git" "spawn_agent"))])
    (check-true (string-contains? add n) (format "解锁段应提及 ~a" n)))
  (check-true (string-contains? add "## Available skills"))
  (check-true (string-contains? add "## Project instructions"))
) ; end test-case

(test-case "ANCHOR-SYSTEM 逐字（上游 minimal 预设 persona.text）"
  (check-equal? ANCHOR-SYSTEM "You are a helpful software engineer assistant.")
) ; end test-case
