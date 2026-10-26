#lang tstring racket
;; probe-faithful.rkt — 自包含保真探针：复用 pi2 真实装配层做单轮 A/B。
;;   FAITHFUL-MIN : 真 persistent-bash(verbatim) + editor + ANCHOR-SYSTEM（字节对齐 RL 分布）
;;   STANDARD     : builtin-tools 全量 + DEFAULT-SYSTEM
;; 同模型 deepseek-v4-pro，同任务，同 reasoning=max，只变「工具 schema + system prompt」。
;; 结果落 .rktd + 打印摘要，跑完即退（不轮询；靠 harness 完成通知）。
;;   PI_DSV4B_BASH_DESC=verbatim racket tests/dsv4b-probes/probe-faithful.rkt
(require json net/http-client racket/port
         (file "../../src/model.rkt")
         (file "../../src/tool.rkt")
         (file "../../src/provider-anthropic.rkt")
         (file "../../src/providers.rkt")
         (file "../../src/dsv4b.rkt")
         (file "../../src/tools/bash-persistent.rkt")
         (file "../../src/tools/editor.rkt")
         (file "../../src/tools/builtin.rkt"))

(define DEFAULT-SYSTEM
  (string-join
   '("You are pi++, a concise coding agent running in a terminal."
     ""
     "Tools: use the provided tools (read_file, write_file, edit_file, glob, grep, bash, spawn_agent)"
     "to inspect and modify files and run commands. Prefer reading a file before editing it."
     ""
     "Verify, don't guess. When you are not certain about something — a file's contents, a symbol's"
     "definition, an API's shape, the project layout, a command's output, or whether a path/name even"
     "exists — do NOT answer from assumption. First use a tool (read_file / grep / glob / bash) to"
     "check, then answer from what you actually observed. Never fabricate file paths, identifiers, or"
     "results. If something remains uncertain after checking, say so and state the assumption you made."
     ""
     "Be concise. Explain briefly what you did and why, and prefer showing evidence (a file excerpt or"
     "command output) over asserting.")
   "\n"))

(define KEY (or (resolve-provider-token "deepseek" "default")
                (error "no deepseek token")))
(define TASK
  (string-append
   "A user reports that dedup() sometimes returns items from a previous, unrelated call. "
   "Root cause + one-line fix.\n\n"
   "def dedup(items, _seen=[]):\n    for it in items:\n        if it not in _seen:\n"
   "            _seen.append(it)\n    return _seen"))

(current-reasoning-effort 'max)                       ; 两臂同档，隔离到 schema+prompt

;; 单次保真请求：build-anthropic-body 组包（stream 关掉做一次性 JSON），POST，解析。
(define (call-arm system tools)
  (define cfg (struct-copy config (default-config) [model "deepseek-v4-pro"] [system-prompt system]))
  (define reg (make-registry tools))
  (define body (hash-set (build-anthropic-body cfg (list (text-msg 'user TASK)) (registry-specs reg))
                         'stream #f))
  (define-values (status hdrs in)
    (http-sendrecv "api.deepseek.com" "/anthropic/v1/messages" #:ssl? #t #:method "POST"
                   #:headers (list f"x-api-key: {KEY}" "anthropic-version: 2023-06-01"
                                   "content-type: application/json")
                   #:data (jsexpr->string body)))
  (define d (read-json in))
  (define content (hash-ref d 'content '()))
  (define (blocks-of ty) (filter (lambda (b) (equal? (hash-ref b 'type #f) ty)) content))
  (define think (apply string-append (map (lambda (b) (hash-ref b 'thinking "")) (blocks-of "thinking"))))
  (define text  (apply string-append (map (lambda (b) (hash-ref b 'text "")) (blocks-of "text"))))
  (define tools-used (map (lambda (b) (hash-ref b 'name "?")) (blocks-of "tool_use")))
  (define lc (string-downcase (string-append think " " text)))
  (define recall? (and (regexp-match? #rx"default" lc)
                       (or (regexp-match? #rx"mutable" lc) (regexp-match? #rx"shared" lc)
                           (regexp-match? #rx"none" lc) (regexp-match? #rx"_seen" lc))))
  (hash 'think-chars (string-length think) 'text-chars (string-length text)
        'tools tools-used 'recall recall? 'stop (hash-ref d 'stop_reason "?")
        'think think 'text text))

(define ARMS
  (list (cons "FAITHFUL-MIN" (lambda () (call-arm ANCHOR-SYSTEM
                                                  (list (make-persistent-bash-tool) (make-editor-tool)))))
        (cons "STANDARD"     (lambda () (call-arm DEFAULT-SYSTEM (builtin-tools #f))))))

(define results
  (for*/list ([rep (in-list '(1 2))] [arm (in-list ARMS)])
    (define label f"{(car arm)} r{rep}")
    (define r (with-handlers ([exn:fail? (lambda (e) (hash 'error (exn-message e)))]) ((cdr arm))))
    (printf "~a\n" label)
    (if (hash-has-key? r 'error)
        (printf "  ERROR: ~a\n" (hash-ref r 'error))
        (printf "  think=~a text=~a tools=~a recall=~a stop=~a\n"
                (hash-ref r 'think-chars) (hash-ref r 'text-chars)
                (hash-ref r 'tools) (hash-ref r 'recall) (hash-ref r 'stop)))
    (cons label r)))

(define bash-mode (or (getenv "PI_DSV4B_BASH_DESC") "default"))
(write-to-file (list (cons 'bash-desc-mode bash-mode) (cons 'results results))
               "/tmp/probe-faithful-results.rktd" #:exists 'replace)
(printf "\nwrote /tmp/probe-faithful-results.rktd (bash-desc=~a)\n" bash-mode)
