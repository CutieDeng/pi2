#lang racket
;; compact-test.rkt — 上下文压缩（context.rkt 三级，2026-10-25 重写）：
;; 切点不拆工具调用 / 结果（原缺陷的复现场景 + 随机历史的性质检查）；总结请求是纯文字（无工具块）、分段不超上限；角色交替；
;; 删旧工具输出（保护当前轮与最近的）；轮开始前的 maybe-compact：先删、再总结；总结失败记提示、这一轮照常

(require rackunit racket/async-channel racket/list
         (file "../src/model.rkt") (file "../src/event.rkt") (file "../src/provider.rkt") (file "../src/context.rkt")
         (file "../src/tool.rkt") (file "../src/permission.rkt") (file "../src/loop.rkt"))

(define (tu id [name "bash"]) (message 'assistant (list (tool-use-block id name (hasheq 'command "ls")))))
(define (tr id [out "out"]) (message 'user (list (tool-result-block id out #f))))
(define (u s) (text-msg 'user s)) (define (a s) (text-msg 'assistant s))
(define (st-of msgs #:budget [budget 100000])
  (for/fold ([s (make-initial-state (struct-copy config (default-config) [context-budget budget]))]) ([m msgs]) (state-append s m)))

;; 合法性：tool_result 的 id 在紧挨着的上一条 assistant 的 tool_use 里；tool_use 后面紧跟它的结果；没有两条相连的同角色消息
(define (orphans msgs)
  (for*/list ([(m i) (in-indexed msgs)] [b (message-blocks m)] #:when (tool-result-block? b)
              #:unless (and (> i 0) (eq? (message-role (list-ref msgs (sub1 i))) 'assistant)
                            (for/or ([x (message-blocks (list-ref msgs (sub1 i)))]) (and (tool-use-block? x) (equal? (tool-use-block-id x) (tool-result-block-tool-use-id b))))))
    (tool-result-block-tool-use-id b)))
(define (dangling msgs)
  (for*/list ([(m i) (in-indexed msgs)] #:when (eq? (message-role m) 'assistant) [b (message-blocks m)] #:when (tool-use-block? b)
              #:unless (and (< (add1 i) (length msgs))
                            (for/or ([x (message-blocks (list-ref msgs (add1 i)))]) (and (tool-result-block? x) (equal? (tool-result-block-tool-use-id x) (tool-use-block-id b))))))
    (tool-use-block-id b)))
(define (same-role-runs msgs) (for/sum ([x msgs] [y (if (pair? msgs) (cdr msgs) '())]) (if (eq? (message-role x) (message-role y)) 1 0)))
(define (well-formed? msgs) (and (null? (orphans msgs)) (null? (dangling msgs)) (zero? (same-role-runs msgs))))

;; 总结用的假模型：记下每次请求；可设输入上限（超了报错）
(define requests (box '()))
(define (summarizer #:limit [limit #f] #:fail? [fail? #f])
  (provider "sum" (lambda (msgs _t)
                    (set-box! requests (cons msgs (unbox requests)))
                    (define ch (make-async-channel))
                    (define in (apply + (map (lambda (m) (string-length (message-text m))) msgs)))
                    (cond
                      [fail? (async-channel-put ch (evt:error (now-ms) (exn:fail "HTTP 400: tool_use ids were found without tool_result blocks" (current-continuation-marks)) #f))]
                      [(and limit (> in limit)) (async-channel-put ch (evt:error (now-ms) (exn:fail (format "input too long: ~a" in) (current-continuation-marks)) #f))]
                      [else (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant (format "SUMMARY#~a" (length (unbox requests))))))
                            (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 1 1)))])
                    ch) void))

(test-case "the old defect: user / call c1 / result c1 / call c2 / result c2 / answer → well-formed after compact!; the summary request has no tool blocks"
  (set-box! requests '())
  (define st (st-of (list (u "task") (tu "c1") (tr "c1") (tu "c2") (tr "c2") (a "done"))))
  (define st2 (parameterize ([current-record-sink void]) (compact! st (summarizer) #:keep-recent 4)))
  (define h (state-history-list st2))
  (check-true (well-formed? h) (format "~a" (map message-role h)))
  (check-true (regexp-match? #rx"compacted context summary" (message-text (car h))))
  (define req (car (unbox requests)))
  (check-equal? (length req) 1)                                                    ; 一条 user 文字消息
  (check-false (for*/or ([m req] [b (message-blocks m)]) (or (tool-use-block? b) (tool-result-block? b))))
  (check-true (regexp-match? #rx"ASSISTANT called bash" (message-text (car req)))))

(test-case "property: random histories with tool calls stay well-formed after compact! at any keep-recent / keep-tokens"
  (random-seed 7)
  (for ([trial (in-range 300)])
    (define msgs
      (let loop ([turns (+ 1 (random 5))] [acc '()] [k 0])
        (if (zero? turns) (reverse acc)
            (let* ([steps (random 4)]
                   [acc1 (cons (u (format "q~a" k)) acc)]
                   [acc2 (for/fold ([acc acc1]) ([s (in-range steps)])
                           (define id (format "c~a-~a" k s))
                           (list* (tr id (make-string (random 3000) #\x)) (tu id) acc))]
                   [acc3 (cons (a (format "a~a" k)) acc2)])
              (loop (sub1 turns) acc3 (add1 k))))))
    (check-true (well-formed? msgs))                                               ; 生成的本身合法
    (define kr (+ 1 (random (length msgs))))
    (define st2 (parameterize ([current-record-sink void])
                  (if (even? trial) (compact! (st-of msgs) (summarizer) #:keep-recent kr) (compact! (st-of msgs #:budget (+ 50 (random 4000))) (summarizer)))))
    (check-true (well-formed? (state-history-list st2)) (format "trial ~a kr ~a: ~a" trial kr (map message-role (state-history-list st2))))))

(test-case "split points: turn boundaries preferred; inside one long turn: after a tool result; nothing → #f"
  (define msgs (list (u "q0") (tu "c1") (tr "c1") (a "a0") (u "q1") (tu "c2") (tr "c2") (tu "c3") (tr "c3") (a "a1")))
  (check-equal? (split-points msgs) '((1 . step) (3 . step) (4 . turn) (5 . step) (7 . step) (9 . step)))   ; assistant 紧跟用户话 / 工具结果：都是安全切点
  (check-equal? (choose-split msgs #:keep-tokens 1000000) 4)                      ; 尾部都放得下 → 最早的轮边界
  (check-equal? (choose-split msgs #:keep-tokens 1 #:keep-recent 3) 7)            ; 最多留 3 条：轮边界不行 → 当前轮里的 step
  (check-false (choose-split (list (u "only")) #:keep-tokens 10)))

(test-case "chunked summary: each request stays under the limit; later chunks carry the earlier summary"
  (set-box! requests '())
  (define msgs (append* (for/list ([k 20]) (list (u (format "question ~a ~a" k (make-string 400 #\q))) (a (format "answer ~a ~a" k (make-string 400 #\a)))))))
  (define st (st-of (append msgs (list (u "now"))) #:budget 1000))                  ; 每段上限 max(2000, 0.5×1000) token
  (define st2 (parameterize ([current-record-sink void]) (compact! st (summarizer #:limit 9000))))
  (check-true (> (length (unbox requests)) 1))
  (check-true (for/and ([r (unbox requests)]) (<= (string-length (message-text (car r))) 9000)))
  (check-true (regexp-match? #rx"Summary of the earlier part" (message-text (car (car (unbox requests))))))
  (check-true (well-formed? (state-history-list st2))))

(test-case "prune: big old tool outputs → stubs; the current turn and the recent part protected; small ones and idempotent"
  (define big (make-string 5000 #\y))
  (define msgs (list (u "q0") (tu "c1") (tr "c1" big) (tu "c2") (tr "c2" "small") (a "a0")
                     (u "q1") (tu "c3") (tr "c3" big) (a "a1")))
  (define-values (out n saved) (prune-tool-outputs msgs #:protect-tokens 10))
  (check-equal? n 1)                                                               ; c1 换了；c3 在当前轮（受保护）；c2 小
  (check-true (> saved 4000))
  (check-regexp-match #rx"^\\[tool output trimmed to save context \\(bash, 5000 characters\\)" (tool-result-block-content (car (message-blocks (list-ref out 2)))))
  (check-equal? (list-ref out 8) (list-ref msgs 8))
  (check-true (well-formed? out))
  (define-values (out2 n2 _s) (prune-tool-outputs out #:protect-tokens 10))
  (check-equal? n2 0)
  (define-values (_o3 n3 _s3) (prune-tool-outputs msgs #:protect-tokens 100000))   ; 都在保护范围里
  (check-equal? n3 0))

;; ---------------------------------------------------------------- 轮开始前（loop.rkt maybe-compact）
(define (turn-provider #:fail-summary? [fail? #f])
  (provider "t" (lambda (msgs _t)
                  (define ch (make-async-channel))
                  (define summary-req? (regexp-match? #rx"handoff note" (message-text (last msgs))))
                  (cond
                    [(and summary-req? fail?) (async-channel-put ch (evt:error (now-ms) (exn:fail "HTTP 400: bad request" (current-continuation-marks)) #f))]
                    [summary-req? (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant "THE SUMMARY")))
                                  (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 1 1)))]
                    [else (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant "ok")))
                          (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 1 1)))])
                  ch) void))
(define (run-one st prov)
  (define recs (box '()))
  (define cfg (agent-state-config st))
  (define d (make-deps #:provider prov #:registry (make-registry '()) #:bus (make-bus) #:policy (make-policy cfg)))
  (define st2 (parameterize ([current-record-sink (lambda (k p) (set-box! recs (cons (cons k p) (unbox recs))))]) (run-turn! st (u "next") d)))
  (values st2 (reverse (unbox recs))))

(test-case "turn start: over 0.7 × budget → old tool outputs trimmed (recorded, no model call); over 1.0 × after that → summary"
  (define big (make-string 6000 #\z))
  (define base (list (u "q0") (tu "c1") (tr "c1" big) (a "a0") (u "q1") (a "a1")))
  ;; 删完就够：只删
  (define-values (st1 recs1) (run-one (st-of base #:budget 2200) (turn-provider)))
  (define kinds1 (for/list ([r recs1] #:when (eq? (car r) 'compact)) (hash-ref (cdr r) 'kind)))
  (check-equal? kinds1 '("prune"))
  (check-true (well-formed? (state-history-list st1)))
  ;; 删完还超：再总结
  (define long (append (for*/list ([k 12] [m (list (u (format "q~a ~a" k (make-string 900 #\w))) (a (format "a~a ~a" k (make-string 900 #\v))))]) m)))
  (define-values (st2 recs2) (run-one (st-of (append base long) #:budget 2200) (turn-provider)))
  (check-equal? (for/list ([r recs2] #:when (eq? (car r) 'compact)) (hash-ref (cdr r) 'kind)) '("prune" "summary"))
  (check-true (regexp-match? #rx"THE SUMMARY" (message-text (car (state-history-list st2)))))
  (check-true (well-formed? (state-history-list st2))))

(test-case "turn start: the summary request fails → a compact_failed note is recorded (not swallowed) and the turn still answers"
  (define long (append* (for/list ([k 12]) (list (u (format "q~a ~a" k (make-string 900 #\w))) (a (format "a~a ~a" k (make-string 900 #\v)))))))
  (define-values (st2 recs) (run-one (st-of long #:budget 2200) (turn-provider #:fail-summary? #t)))
  (define notes (for/list ([r recs] #:when (and (eq? (car r) 'note) (equal? (hash-ref (cdr r) 'code #f) "compact_failed"))) (hash-ref (cdr r) 'text)))
  (check-equal? (length notes) 1)
  (check-regexp-match #rx"HTTP 400" (car notes))
  (check-equal? (message-text (last (state-history-list st2))) "ok"))
