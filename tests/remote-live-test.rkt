#lang tstring racket
;; remote-live-test.rkt — 无头模式 / 跨主机子 agent 的真机验收（design-gui.md §17、§21；2026-10-24 因 lan-11 连不上挂起，见 §21.6）。
;; 不在离线套件里（tests/info.rkt 排除）。用法：
;;   PI2_LIVE_HOST=lan-11 raco test tests/remote-live-test.rkt                      第 1、2 段（不花钱）
;;   PI2_LIVE_HOST=lan-11 PI2_LIVE_SPEND=1 raco test tests/remote-live-test.rkt     再加第 3 段（DeepSeek v4-flash，约 $0.001）
;; 第 1 段：ssh 可达（BatchMode）、远端 racket 版本、`racket -l pi2` 可用。
;; 第 2 段：远端守护进程经 `attach --spawn` 起来、ready（attached）、ping；断开 ssh 后守护进程仍在（status）；stop。
;; 第 3 段：spawn_agent host=那台，凭证本地签发（key-proxy + ssh -R）：回答里有远端 `uname -s` 的输出、摘要里有 key proxy 请求数。
;; 环境变量没给 → 什么都不跑。

(require rackunit json racket/port
         (file "../src/model.rkt") (file "../src/tool.rkt") (file "../src/permission.rkt") (file "../src/loop.rkt")
         (file "../src/providers.rkt") (file "../src/subagent.rkt") (file "../src/hosts.rkt"))

(define HOST (getenv "PI2_LIVE_HOST"))
(define SPEND? (equal? (getenv "PI2_LIVE_SPEND") "1"))

(define (ssh . cmd)
  (define-values (p o i e) (apply subprocess #f #f #f (find-executable-path "ssh") "-T" "-o" "BatchMode=yes" "-o" "ConnectTimeout=10" HOST cmd))
  (close-output-port i)
  (define err (box ""))
  (define et (thread (lambda () (set-box! err (port->string e)))))
  (define out (port->string o))
  (unless (sync/timeout 120 p) (subprocess-kill p #t))
  (thread-wait et)
  (values (subprocess-status p) out (unbox err)))

(cond
  [(not HOST) (printf "remote-live-test: PI2_LIVE_HOST not set, skipped\n")]
  [else
   (test-case "1. ssh reachable; racket and pi2 present on the host"
     (define-values (c out err) (ssh "racket --version && racket -l pi2 -- --help >/dev/null 2>&1 && echo PI2-OK"))
     (check-equal? c 0 err)
     (check-regexp-match #rx"Welcome to Racket v9" out)
     (check-regexp-match #rx"PI2-OK" out))

   (test-case "2. daemon via attach --spawn over ssh: ready, ping; survives the ssh disconnect; stop"
     (define name "pi2-live")
     (define-values (p o i e)
       (subprocess #f #f #f (find-executable-path "ssh") "-T" "-o" "BatchMode=yes" HOST
                   (format "racket -l pi2/src/serve -- attach --spawn --name ~a --idle-exit 300" name)))
     (define (next) (let ([l (sync/timeout 90 (read-line-evt o 'linefeed))]) (and (string? l) (string->jsexpr l))))
     (define ready (let loop () (define j (next)) (if (or (not j) (equal? (hash-ref j 'type #f) "ready")) j (loop))))
     (check-true (and ready (hash-ref ready 'attached #f)) "ready + attached")
     (write-json (hasheq 'type "ping") i) (newline i) (flush-output i)
     (check-not-false (let loop () (define j (next)) (cond [(not j) #f] [(equal? (hash-ref j 'type #f) "pong") j] [else (loop)])))
     (close-output-port i)                                         ; 断开
     (unless (sync/timeout 30 p) (subprocess-kill p #t))
     (sleep 2)
     (define-values (c out err) (ssh (format "racket -l pi2/src/serve -- status --name ~a" name)))
     (check-equal? c 0 err)
     (check-regexp-match #rx"serve_ok" out)                          ; ssh 断开后守护进程仍在
     (ssh (format "racket -l pi2/src/serve -- stop --name ~a" name)))

   (when SPEND?
     (test-case "3. spawn_agent on the host with a locally issued credential (key proxy over ssh -R)"
       (define key (resolve-provider-token "deepseek" "default"))
       (check-not-false key "DeepSeek key in local credentials")
       (define cfg-dir (make-temporary-file "pi2-live-remote-~a" 'directory))
       (putenv "PI_CONFIG_HOME" (path->string cfg-dir))
       (save-hosts! (list (hasheq 'id HOST 'ssh HOST 'credential "proxy")))
       (define parent (apply-provider-profile (struct-copy config (default-config) [workdir (path->string cfg-dir)]) "deepseek-lite"))
       (define parent* (struct-copy config parent [api-key key]))
       (define tool (make-spawn-agent-tool #:provider #f #:sub-tools '()))
       (define asks '())
       (define o (parameterize ([current-asker (lambda (_p) (set! asks (cons (current-permission-request) asks)) 'yes)])
                   (tool-run tool (hasheq 'task "Run the shell command `uname -s` and report its exact output." 'host HOST)
                             (tool-ctx (path->string cfg-dir) void parent*))))
       (check-false (tool-outcome-is-error? o) (tool-outcome-content o))
       (check-regexp-match #rx"Linux" (tool-outcome-content o))
       (check-regexp-match #rx"key proxy: [1-9]" (tool-outcome-display o))
       (printf "live spawn_agent: ~a\n  asks: ~s\n" (tool-outcome-display o) (map (lambda (a) (hash-ref a 'tool)) asks))))])
