#lang tstring racket
;; adapt-test.rkt — 增强适配规则：内置规则的匹配与效果、用户 / 项目规则覆盖、工具子集与描述、开关落盘、经分发 provider 生效

(require
 rackunit
 racket/async-channel
 racket/file
 (file "../src/model.rkt")
 (file "../src/provider.rkt")
 (file "../src/plugin.rkt")
 (file "../src/adapt.rkt")
) ; end require

(define cfg-dir (make-temporary-file "pi2-adapt-~a" 'directory))
(void (putenv "PI_CONFIG_HOME" (path->string cfg-dir)))
(define work (make-temporary-file "pi2-adapt-ws-~a" 'directory))

(define (cfg-for model) (struct-copy config (default-config) [model model] [system-prompt "SYS"] [workdir (path->string work)]))
(define MSGS (list (text-msg 'user "hello")))
(define TOOLS (list (hasheq 'type "function" 'function (hasheq 'name "bash" 'description "run shell"))
                    (hasheq 'type "function" 'function (hasheq 'name "grep" 'description "search"))))

(test-case "qwen3 with reasoning off gets /no_think appended; with reasoning on it does not apply"
  (define-values (c m t applied) (apply-adaptations "lmstudio" (cfg-for "qwen/qwen3.5-9b") MSGS TOOLS 'off))
  (check-equal? applied '("qwen3-no-think"))
  (check-equal? (config-system-prompt c) "SYS\n\n/no_think")
  (define-values (c2 _m2 _t2 applied2) (apply-adaptations "lmstudio" (cfg-for "qwen/qwen3.5-9b") MSGS TOOLS 'high))
  (check-equal? applied2 '())
  (check-equal? (config-system-prompt c2) "SYS"))

(test-case "deepseek-r1: temperature 0.6 and the system prompt moves into the first user message"
  (define-values (c m _t applied) (apply-adaptations "lmstudio" (cfg-for "deepseek-r1-distill-qwen-32b") MSGS TOOLS 'off))
  (check-equal? applied '("deepseek-r1-usage"))
  (check-equal? (config-temperature c) 0.6)
  (check-false (config-system-prompt c))
  (check-equal? (message-text (car m)) "SYS\n\nhello")
  (define-values (c2 _m2 _t2 applied2) (apply-adaptations "lmstudio" (cfg-for "gemma-4-31b-it@6bit") MSGS TOOLS 'off))
  (check-equal? applied2 '())                                                       ; 别的模型不受影响
  (check-equal? (config-temperature c2) (config-temperature (default-config))))

(test-case "user rules override built-in ones by name; project rules add more; tools-only and tool-description"
  (with-output-to-file (user-rules-path) #:exists 'truncate
    (lambda ()
      (write '((name "qwen3-no-think") (description "mine") (model "qwen3") (system-append "/think")))
      (write '((name "gemma-tools") (model "gemma") (tools-only "bash") (tool-description "bash" "run one command")))))
  (make-directory* (build-path work ".pi"))
  (with-output-to-file (project-rules-path (path->string work)) #:exists 'truncate
    (lambda () (write '((name "project-style") (model "gemma") (system-prepend "Answer in English.")))))
  (define-values (c _m _t applied) (apply-adaptations "lmstudio" (cfg-for "qwen3-x") MSGS TOOLS 'high))
  (check-equal? applied '("qwen3-no-think"))                                        ; 用户版本：不限推理档
  (check-equal? (config-system-prompt c) "SYS\n\n/think")
  (define-values (c2 _m2 t2 applied2) (apply-adaptations "lmstudio" (cfg-for "gemma-4") MSGS TOOLS 'off))
  (check-equal? applied2 '("gemma-tools" "project-style"))
  (check-equal? (map (lambda (s) (hash-ref (hash-ref s 'function) 'name)) t2) '("bash"))
  (check-equal? (hash-ref (hash-ref (car t2) 'function) 'description) "run one command")
  (check-equal? (config-system-prompt c2) "Answer in English.\n\nSYS")
  (check-equal? (map (lambda (j) (hash-ref j 'source)) (rules->jsexpr "lmstudio" "gemma-4" 'off (path->string work))) '("user" "project")))

(test-case "a rule switched off stays off (file) and is still listed for the UI"
  (set-rule-enabled! "gemma-tools" #f)
  (define-values (_c _m t applied) (apply-adaptations "lmstudio" (cfg-for "gemma-4") MSGS TOOLS 'off))
  (check-equal? applied '("project-style"))
  (check-equal? (length t) 2)
  (define listed (rules->jsexpr "lmstudio" "gemma-4" 'off (path->string work)))
  (check-equal? (map (lambda (j) (list (hash-ref j 'name) (hash-ref j 'enabled))) listed) '(("gemma-tools" #f) ("project-style" #t)))
  (set-rule-enabled! "gemma-tools" #t))

(test-case "rules apply on the way through the dispatch provider (what the concrete provider sees)"
  (define seen #f)
  (define host (make-plugin-host))
  (hash-set! (plugin-host-providers host) "fake"
             (lambda (_c) (provider "fake" (lambda (msgs tools)
                                             (set! seen (list (config-system-prompt (current-config)) (length tools)))
                                             (define ch (make-async-channel))
                                             (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 0 0)))
                                             ch)
                                    void)))
  (host-set-provider! host "fake")
  (define d (make-dispatch-provider host (cfg-for "gemma-4")))
  (sync (parameterize ([current-config (cfg-for "gemma-4")] [current-reasoning-effort 'off]) (provider-stream! d MSGS TOOLS)))
  (check-equal? seen '("Answer in English.\n\nSYS" 1)))

(delete-directory/files cfg-dir)
(delete-directory/files work)
