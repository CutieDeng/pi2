#lang tstring racket
;; truncation-test.rkt — 输出撞上 max_tokens（loop.rkt 截断分支）：
;;   第一次 → max_tokens ×4 重发同一请求（残段不进历史）；升过的上限本轮内沿用，不进会话 config；
;;   到 OUTPUT-CAP 后 → 残段文字留下 + 「接着做」提示；一轮最多处理 MAX-TRUNCATIONS 次，之后照旧终止；
;;   每次都记一条 note（code output_truncated）。
;; 起因：deepseek-v4-flash 思考占满 4096，回复只有思考块 → 整轮被当成答完（E1 实测 6/15 次运行这样提前结束）。

(require
 rackunit racket/async-channel racket/pvector
 (file "../src/model.rkt") (file "../src/event.rkt") (file "../src/provider.rkt") (file "../src/tool.rkt")
 (file "../src/permission.rkt") (file "../src/loop.rkt")
) ; end require

(struct noop-tool ()
  #:methods gen:tool
  [(define (tool-name _t) "noop")
   (define (tool-spec _t) (hasheq 'type "function" 'function (hasheq 'name "noop" 'description "x" 'parameters (hasheq 'type "object" 'properties (hasheq)))))
   (define (tool-permission-level _t) 'read-only)
   (define (tool-run _t _input _ctx) (ok-outcome "ok"))])

;; replies：每次请求依次取一个 (cons 消息 stop-reason)；seen：每次请求看到的 (max_tokens . 最后一条消息)
(define (scripted replies)
  (define left (box replies))
  (define seen (box '()))
  (values
   (provider "mock"
             (lambda (msgs _t)
               (set-box! seen (cons (cons (config-max-tokens (current-config)) (last msgs)) (unbox seen)))
               (define r (car (unbox left)))
               (set-box! left (cdr (unbox left)))
               (define ch (make-async-channel))
               (thread (lambda ()
                         (async-channel-put ch (evt:message (now-ms) (car r)))
                         (async-channel-put ch (evt:turn-end (now-ms) (cdr r) (usage 10 5)))))
               ch)
             void)
   (lambda () (reverse (unbox seen)))))

(define (cfg mt) (struct-copy config (default-config) [model "mock"] [permission-mode 'yolo] [turn-max-calls 50] [max-tokens mt]))
(define (deps prov c) (make-deps #:provider prov #:registry (make-registry (list (noop-tool))) #:bus (make-bus)
                                 #:policy (make-policy c) #:asker (lambda (_t) 'yes)))
(define (run c prov)
  (define notes '())
  (define st (parameterize ([current-record-sink (lambda (k p) (when (eq? k 'note) (set! notes (cons p notes))))])
               (run-turn! (make-initial-state c) (text-msg 'user "go") (deps prov c))))
  (values st (reverse notes)))

(define (hist st) (for/list ([m (in-pvector (agent-state-history st))]) m))
(define think-only (message 'assistant (list (thinking-block "long reasoning ..." "sig"))))
(define (calls-noop id) (message 'assistant (list (tool-use-block id "noop" (hasheq)))))

(test-case "thinking-only reply cut at max_tokens → same request again with ×4; the raised limit holds for the turn, config keeps 4096"
  (define-values (p seen) (scripted (list (cons think-only "max_tokens") (cons (calls-noop "c1") "tool_use") (cons (text-msg 'assistant "done") "end_turn"))))
  (define-values (st notes) (run (cfg 4096) p))
  (check-equal? (map car (seen)) '(4096 16384 16384))
  (check-equal? (message-role (cdr (cadr (seen)))) 'user "the retry resends the same window: the cut-off reply is not in it")
  (check-equal? (message-text (last (hist st))) "done")
  (check-false (for/or ([m (hist st)]) (for/or ([b (message-blocks m)]) (thinking-block? b))) "the cut-off reply never enters history")
  (check-equal? (config-max-tokens (agent-state-config st)) 4096)
  (check-equal? (map (lambda (n) (hash-ref n 'code)) notes) '("output_truncated"))
  (check-equal? (usage-input-tokens (agent-state-token-usage st)) 30 "the cut-off request's usage is still counted"))

(test-case "already at the cap (openai \"length\") → keep the text, ask to continue; tool calls of the cut reply are dropped"
  (define cut (message 'assistant (list (text-block "partial plan") (tool-use-block "c9" "noop" (hasheq)))))
  (define-values (p seen) (scripted (list (cons cut "length") (cons (text-msg 'assistant "done") "stop"))))
  (define-values (st _notes) (run (cfg 32768) p))
  (check-equal? (map car (seen)) '(32768 32768))
  (define asked (cdr (cadr (seen))))
  (check-equal? (message-role asked) 'user)
  (check-regexp-match #rx"cut off" (message-text asked))
  (define h (hist st))
  (check-equal? (message-text (list-ref h (- (length h) 3))) "partial plan")
  (check-false (for/or ([m h]) (pair? (message-tool-uses m))) "the cut tool call is neither kept nor run"))

(test-case "at the cap, a cut thinking-only reply keeps its thinking (DeepSeek: reasoning must be passed back when tools are sent) plus a placeholder text"
  (define-values (p _seen) (scripted (list (cons think-only "max_tokens") (cons (text-msg 'assistant "done") "end_turn"))))
  (define-values (st _notes) (run (cfg 32768) p))
  (define h (hist st))
  (define kept (list-ref h (- (length h) 3)))
  (check-equal? (message-role kept) 'assistant)
  (check-true (thinking-block? (car (message-blocks kept))))
  (check-regexp-match #rx"cut off" (message-text kept)))

(test-case "keeps getting cut → at most 3 handled per turn, the 4th ends the turn as before"
  (define-values (p seen) (scripted (make-list 6 (cons think-only "max_tokens"))))
  (define-values (_st notes) (run (cfg 4096) p))
  (check-equal? (map car (seen)) '(4096 16384 32768 32768))
  (check-equal? (length notes) 3))

(test-case "a normal stop reason is untouched"
  (define-values (p seen) (scripted (list (cons (text-msg 'assistant "hi") "end_turn"))))
  (define-values (_st notes) (run (cfg 4096) p))
  (check-equal? (length (seen)) 1)
  (check-equal? notes '()))
