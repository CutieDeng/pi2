#lang tstring racket
;; dsv4b-live-test.rkt — 真机：dsv4-b 锚定档案端到端（design-dsv4b.md §5.3）。
;; 首请求恰两工具（持久 bash + str_replace_editor）+ Minimal 原句提示词；断言
;; 模型能在锚定态完成「建文件 → bash 回显」任务，且首个工具调用后发生促迁。
;; 需 DEEPSEEK_API_KEY（env 或凭据文件）；无密钥 → 跳过（退出 0）。显式运行：
;;   racket tests/dsv4b-live-test.rkt
;; A/B 对照（人工）：同任务分别跑 --provider dsv4-b 与 --provider deepseek -m deepseek-v4-pro，
;; 比较轮数/token（锚定收益为经验命题，不在此写死断言）。

(require
 racket/file
 racket/string
 (file "../src/model.rkt")
 (file "../src/event.rkt")
 (file "../src/tool.rkt")
 (file "../src/permission.rkt")
 (file "../src/loop.rkt")
 (file "../src/plugin.rkt")
 (file "../src/providers.rkt")
 (file "../src/provider-anthropic.rkt")
 (file "../src/credentials.rkt")
 (file "../src/dsv4b.rkt")
 (file "../src/tools/builtin.rkt")
 (file "../src/tools/editor.rkt")
 (file "../src/tools/bash-persistent.rkt")
) ; end require

;; 注意用 resolve-provider-token（实例文件密钥 > env > 兄弟档案回退），
;; 而非裸 resolve-key——密钥常以 deepseek 实例形式存于凭据文件。
(define key (resolve-provider-token "dsv4-b" "default"))

(cond
  [(not key)
   (displayln "dsv4b-live-test: SKIP (set DEEPSEEK_API_KEY or run: racket main.rkt --set-key DEEPSEEK_API_KEY)")]
  [else
   (define tmp (path->string (make-temporary-file "pi2-dsv4b-live-~a" 'directory)))
   (define cfg
     (apply-provider-profile
      (struct-copy config (default-config)
                   [system-prompt ANCHOR-SYSTEM]
                   [max-tokens 2048]
                   [context-budget 200000]
                   [permission-mode 'yolo]
                   [workdir tmp])
      "dsv4-b"))
   (printf "endpoint: ~a  model: ~a  key: ~a\n"
           (config-endpoint cfg) (config-model cfg) (mask-key (config-api-key cfg)))

   (define host (make-plugin-host))
   (register-builtin-providers! host)
   (host-set-provider! host "dsv4-b")
   (define registry (make-registry (list (make-persistent-bash-tool) (make-editor-tool))))
   (define (names) (sort (map tool-name (registry-tools registry)) string<?))
   (printf "boot registry: ~a\n" (names))
   (unless (equal? (names) '("bash" "str_replace_editor"))
     (eprintf "dsv4b-live-test: FAIL boot registry not anchored\n") (exit 1))
   (set-dsv4b-payload!
    (lambda ()
      (for ([t (in-list (builtin-tools cfg))]
            #:unless (string=? (tool-name t) "bash"))
        (registry-add! registry t))
      (dsv4b-unlock-addendum "" "")))

   (define bus (make-bus))
   (define unsub
     (bus-subscribe! bus (lambda (e)
                           (when (evt:delta? e) (display (evt:delta-text e)) (flush-output)))))
   (define d (make-deps #:provider (make-anthropic-provider cfg)
                        #:registry registry
                        #:bus bus
                        #:policy (make-policy cfg)
                        #:asker (lambda (_q) 'yes)
                        #:plugin-host host))
   (define st (make-initial-state cfg))
   (define task
     f"Create a file at {tmp}/hello.txt containing exactly the single word: anchored\nThen print its contents with bash. Stop after that.")
   (define st* (run-turn! st (text-msg 'user task) d))
   (bus-drain! bus)
   (unsub)
   (newline)

   (define created (build-path tmp "hello.txt"))
   (define ok-file (and (file-exists? created)
                        (string-contains? (file->string created) "anchored")))
   (printf "promoted: ~a  registry after: ~a\n" (dsv4b-promoted?) (names))
   (printf "usage — in:~a out:~a  turns:~a\n"
           (usage-input-tokens (agent-state-token-usage st*))
           (usage-output-tokens (agent-state-token-usage st*))
           (agent-state-turn-count st*))
   (cond
     [(and ok-file (dsv4b-promoted?)
           (string-contains? (config-system-prompt (agent-state-config st*)) UNLOCK-MARKER))
      (displayln "dsv4b-live-test: OK")]
     [else
      (eprintf "dsv4b-live-test: FAIL file=~a promoted=~a\n" ok-file (dsv4b-promoted?))
      (exit 1)])
   (delete-directory/files (string->path tmp))]
) ; end cond
