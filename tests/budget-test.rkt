#lang tstring racket
;; budget-test.rkt — 费用软上限：记账与今日合计；每轮上限到了在下一次请求前问；「本轮继续」本轮不再问；「停止」抛 exn:fail:budget；
;; 每天上限 +「今天不再询问」；RPC 下停止 = 按中断收尾（保留已完成的步骤、标记写明是费用上限）。
;; 假模型：每次请求用量 1,000,000 输入 token，模型 deepseek-v4-pro → 估算 $0.55 / 次。

(require
 rackunit json racket/async-channel racket/file racket/pvector
 (file "../src/model.rkt") (file "../src/event.rkt") (file "../src/provider.rkt") (file "../src/tool.rkt")
 (file "../src/permission.rkt") (file "../src/loop.rkt") (file "../src/session.rkt") (file "../src/plugin.rkt")
 (file "../src/rpc.rkt") (file "../src/budget.rkt")
) ; end require

(define cfg-dir (make-temporary-file "pi2-budget-~a" 'directory))
(void (putenv "PI_CONFIG_HOME" (path->string cfg-dir)))
(define (reset!) (for ([p (list (budget-path) (spend-log-path))]) (when (file-exists? p) (delete-file p))))

(struct noop-tool ()
  #:methods gen:tool
  [(define (tool-name _t) "noop")
   (define (tool-spec _t) (hasheq 'type "function" 'function (hasheq 'name "noop" 'description "x" 'parameters (hasheq 'type "object" 'properties (hasheq)))))
   (define (tool-permission-level _t) 'read-only)
   (define (tool-run _t _input _ctx) (ok-outcome "ok"))])

;; 前 n 次请求都调工具，第 n+1 次回答；每次用量 1M 输入
(define (tool-loop-provider n)
  (define k (box 0))
  (provider "mock"
            (lambda (_m _t)
              (define ch (make-async-channel))
              (set-box! k (add1 (unbox k)))
              (define i (unbox k))
              (thread (lambda ()
                        (define msg (if (<= i n)
                                        (message 'assistant (list (tool-use-block (format "c~a" i) "noop" (hasheq))))
                                        (text-msg 'assistant "done")))
                        (async-channel-put ch (evt:message (now-ms) msg))
                        (async-channel-put ch (evt:turn-end (now-ms) (if (<= i n) "tool_calls" "stop") (usage 1000000 0)))))
              ch)
            void))

(define cfg (struct-copy config (default-config) [model "deepseek-v4-pro"] [permission-mode 'yolo] [turn-max-calls 50]))
(define (deps prov asker) (make-deps #:provider prov #:registry (make-registry (list (noop-tool))) #:bus (make-bus)
                                     #:policy (make-policy cfg) #:asker asker))

(test-case "record-spend! appends to the log; spent-today sums by model; unknown (local) models cost nothing"
  (reset!)
  (check-equal? (record-spend! "deepseek-v4-pro" (usage 1000000 0)) 0.55)
  (record-spend! "deepseek-v4-pro" (usage 0 1000000))
  (check-equal? (record-spend! "gemma-local-xyz" (usage 1000000 1000000)) 0.0)
  (define-values (total by) (spent-today))
  (check-= total 2.74 1e-9)
  (check-equal? (map car by) '("deepseek-v4-pro")))

(test-case "turn cap: asked before the request that would go past it; 'no stops the turn with exn:fail:budget"
  (reset!)
  (set-budget! #:turn 1.0)
  (define asks '())
  (define (asker text) (set! asks (cons (current-permission-request) asks)) 'no)
  (check-exn exn:fail:budget?
             (lambda () (run-turn! (make-initial-state cfg) (text-msg 'user "go") (deps (tool-loop-provider 10) asker))))
  (check-equal? (length asks) 1)
  (define in (hash-ref (car asks) 'input))
  (check-equal? (hash-ref (car asks) 'tool) 'budget)
  (check-equal? (hash-ref in 'scope) "turn")
  (check-equal? (hash-ref in 'spent) 1.1)                  ; 两次请求之后（0.55 × 2）才越过 1.0
  (define-values (total _by) (spent-today))
  (check-= total 1.1 1e-9))

(test-case "turn cap: 'yes = continue this turn without asking again; the next turn asks again"
  (reset!)
  (set-budget! #:turn 1.0)
  (define n 0)
  (define (asker _text) (set! n (add1 n)) 'yes)
  (define d (deps (tool-loop-provider 5) asker))
  (define st (run-turn! (make-initial-state cfg) (text-msg 'user "go") d))
  (check-equal? n 1)
  (check-equal? (message-text (pvector-ref (agent-state-history st) (sub1 (pvector-length (agent-state-history st))))) "done")
  (run-turn! st (text-msg 'user "again") (deps (tool-loop-provider 3) asker))
  (check-equal? n 2))

(test-case "daily cap counts earlier spend (all sessions); 'always snoozes it for today"
  (reset!)
  (record-spend! "deepseek-v4-pro" (usage 10000000 0))   ; 今天已花 $5.5
  (set-budget! #:daily 5.0)
  (define asks '())
  (define (asker _t) (set! asks (cons (hash-ref (hash-ref (current-permission-request) 'input) 'scope) asks)) 'always)
  (run-turn! (make-initial-state cfg) (text-msg 'user "go") (deps (tool-loop-provider 1) asker))
  (check-equal? asks '("day"))
  (check-true (snoozed-today?))
  (run-turn! (make-initial-state cfg) (text-msg 'user "go") (deps (tool-loop-provider 1) asker))
  (check-equal? asks '("day"))                             ; 今天不再问
  (set-budget! #:daily #f)
  (check-false (daily-cap)))

(test-case "RPC: the cap question is a permission_request (tool budget); 'no' ends the turn like an interrupt, keeping finished steps"
  (reset!)
  (set-budget! #:turn 1.0)
  (define dir (make-temporary-file "pi2-budget-ws-~a" 'directory))
  (define c (struct-copy config cfg [workdir (path->string dir)]))
  (define host (make-plugin-host))
  (define d (make-deps #:provider (tool-loop-provider 10) #:registry (make-registry (list (noop-tool)))
                       #:bus (make-bus) #:policy (make-policy c) #:plugin-host host))
  (define sess-path (fresh-session-path (path->string dir)))
  (define sess (session-open! sess-path c))
  (define-values (in-r in-w) (make-pipe))
  (define-values (out-r out-w) (make-pipe))
  (thread (lambda () (parameterize ([current-input-port in-r] [current-output-port out-w])
                       (run-rpc! d (make-initial-state c) sess #:plugin-host host #:fetch-models (lambda (_e) #f)))
            (close-output-port out-w)))
  (define (send! j) (write-string (jsexpr->string j) in-w) (newline in-w) (flush-output in-w))
  (define (expect! type)
    (let loop ([seen '()])
      (define j (string->jsexpr (sync/timeout 10 (read-line-evt out-r 'linefeed))))
      (if (equal? (hash-ref j 'type #f) type) (values j (reverse seen)) (loop (cons j seen)))))
  (expect! "ready")
  (send! (hasheq 'type "get_budget"))
  (define-values (b0 _b) (expect! "budget"))
  (check-equal? (hash-ref b0 'turn_usd) 1.0)
  (send! (hasheq 'type "prompt" 'text "go"))
  (define-values (pr _s) (expect! "permission_request"))
  (check-equal? (hash-ref pr 'tool) "budget")
  (check-equal? (hash-ref (hash-ref pr 'input) 'scope) "turn")
  (send! (hasheq 'type "permission" 'id (hash-ref pr 'id) 'decision "no"))
  (define-values (tc seen) (expect! "turn_complete"))
  (check-equal? (hash-ref tc 'outcome) "budget")
  (check-true (hash-ref tc 'interrupted))
  (check-true (>= (hash-ref tc 'kept_messages) 5))         ; 用户消息 + 两步（调用 + 结果）+ 标记
  (check-not-false (findf (lambda (j) (and (equal? (hash-ref j 'type #f) "error") (regexp-match? #rx"spending cap" (hash-ref j 'message "")))) seen))
  (send! (hasheq 'type "history"))
  (define-values (h _h) (expect! "history"))
  (check-regexp-match #rx"spending cap" (jsexpr->string (last (hash-ref h 'messages))))
  (send! (hasheq 'type "set_budget" 'turn_usd 'null 'daily_usd 3))
  (expect! "ok")
  (send! (hasheq 'type "state"))
  (define-values (stt _st) (expect! "state"))
  (check-equal? (hash-ref (hash-ref stt 'budget) 'turn_usd) 'null)
  (check-equal? (hash-ref (hash-ref stt 'budget) 'daily_usd) 3)
  (send! (hasheq 'type "shutdown"))
  (expect! "bye"))

(test-case "borrowed credential (remote sub-agent): spend not written here, still counted for this turn's cap"
  (reset!)
  (set-borrowed-credential! #t)
  (define tb (make-turn-budget))
  (parameterize ([current-turn-budget tb])
    (check-equal? (record-spend! "deepseek-v4-pro" (usage 1000000 0)) 0.55))
  (set-borrowed-credential! #f)
  (check-false (file-exists? (spend-log-path)))
  (check-= (vector-ref tb 0) 0.55 1e-9))

(test-case "combined cap (GUI: other hosts' spend pushed in): asked with scope combined; always = not again today; a stale date is ignored"
  (reset!)
  (record-spend! "deepseek-v4-pro" (usage 1000000 0))     ; 本机今天 $0.55
  (set-external-spend! 0.6 1.0)                           ; 其他主机 $0.6，合计上限 $1
  (define bj (budget->jsexpr))
  (check-regexp-match #px"^[0-9a-f]{12}$" (hash-ref bj 'ledger))
  (check-equal? (hash-ref bj 'external_usd) 0.6)
  (check-equal? (hash-ref bj 'combined_usd) 1.0)
  (define asks '())
  (define (asker _t) (set! asks (cons (hash-ref (current-permission-request) 'input) asks)) 'always)
  (run-turn! (make-initial-state cfg) (text-msg 'user "go") (deps (tool-loop-provider 0) asker))
  (check-equal? (length asks) 1)
  (check-equal? (hash-ref (car asks) 'scope) "combined")
  (check-equal? (hash-ref (car asks) 'spent) 1.15)
  (run-turn! (make-initial-state cfg) (text-msg 'user "go") (deps (tool-loop-provider 0) asker))
  (check-equal? (length asks) 1)                          ; 今天不再问
  (check-true (hash-ref (budget->jsexpr) 'combined_snoozed))
  (set-external-spend! 5.0 1.0 "2000-01-01")              ; 旧日期：不算
  (check-equal? (hash-ref (budget->jsexpr) 'external_usd) 0.0)
  (check-equal? (hash-ref (budget->jsexpr) 'combined_usd) 'null))

(delete-directory/files cfg-dir)
