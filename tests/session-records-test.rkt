#lang tstring racket
;; session-records-test.rkt — 会话记录 P1「所有信息都有记录」（design-content.md §5）：经 run-rpc! 跑真实的轮，
;; 会话文件里有：权限询问与回答、每次模型请求的用量、工具结束（耗时、出错、摘要）、被中断时流到一半的文字（在中断标记之前）、出错提示。

(require rackunit json racket/async-channel
         (file "../src/model.rkt") (file "../src/event.rkt") (file "../src/provider.rkt") (file "../src/tool.rkt")
         (file "../src/permission.rkt") (file "../src/loop.rkt") (file "../src/session.rkt") (file "../src/plugin.rkt")
         (file "../src/rpc.rkt") (file "../src/rktd.rkt") (file "../src/tools/builtin.rkt"))

(define dir (make-temporary-file "pi2-recs-~a" 'directory))
(void (putenv "PI_CONFIG_HOME" (path->string dir)))

;; 脚本模型：含 run → 先调 bash（normal 模式要询问）；拿到结果后回答；含 hang → 吐一段后挂住；含 boom → 报错；其余 → 回答
(define (make-prov)
  (define cancel (make-semaphore 0))
  (provider "s"
            (lambda (msgs _t)
              (define ch (make-async-channel))
              (define lm (last msgs))
              (define tr (for/first ([b (message-blocks lm)] #:when (tool-result-block? b)) b))
              (define txt (message-text lm))
              (thread (lambda ()
                        (cond
                          [tr (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant "done")))
                              (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 30 4)))]
                          [(regexp-match? #rx"run" txt)
                           (async-channel-put ch (evt:message (now-ms) (message 'assistant (list (tool-use-block "c1" "bash" (hasheq 'command "echo hi"))))))
                           (async-channel-put ch (evt:turn-end (now-ms) "tool_calls" (usage 20 3)))]
                          [(regexp-match? #rx"hang" txt)
                           (async-channel-put ch (evt:delta (now-ms) 'thinking "considering"))
                           (async-channel-put ch (evt:delta (now-ms) 'text "partial answer so far"))
                           (semaphore-wait cancel)]
                          [(regexp-match? #rx"boom" txt)
                           (async-channel-put ch (evt:error (now-ms) (exn:fail "boom: upstream exploded" (current-continuation-marks)) #f))]
                          [else (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant "ok")))
                                (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 1 1)))])))
              ch)
            (lambda () (semaphore-post cancel))))

(test-case "permission ask / answer, per-request usage, tool end, partial text on interrupt, error note — all in the session file"
  (define cfg (struct-copy config (default-config) [workdir (path->string dir)] [permission-mode 'normal] [model "deepseek-v4-flash"]))
  (define path (fresh-session-path (path->string dir)))
  (define sess (session-open! path cfg))
  (define host (make-plugin-host))
  (define d (make-deps #:provider (make-prov) #:registry (make-registry (builtin-tools cfg)) #:bus (make-bus) #:policy (make-policy cfg) #:plugin-host host))
  (define-values (in-r in-w) (make-pipe)) (define-values (out-r out-w) (make-pipe))
  (thread (lambda () (parameterize ([current-input-port in-r] [current-output-port out-w])
                       (run-rpc! d (make-initial-state cfg) sess #:plugin-host host #:fetch-models (lambda (_e) #f)))
            (close-output-port out-w)))
  (define (send! j) (write-string (jsexpr->string j) in-w) (newline in-w) (flush-output in-w))
  (define (expect! type) (let loop () (define j (string->jsexpr (sync/timeout 10 (read-line-evt out-r 'linefeed)))) (if (equal? (hash-ref j 'type #f) type) j (loop))))
  (expect! "ready")
  (send! (hasheq 'type "prompt" 'text "please run it"))
  (define pr (expect! "permission_request"))
  (send! (hasheq 'type "permission" 'id (hash-ref pr 'id) 'decision "yes"))
  (expect! "turn_complete")
  (send! (hasheq 'type "prompt" 'text "now hang"))
  (sleep 0.3)
  (send! (hasheq 'type "interrupt"))
  (expect! "turn_complete")
  (send! (hasheq 'type "prompt" 'text "boom please"))
  (expect! "turn_complete")
  (send! (hasheq 'type "shutdown")) (expect! "bye")
  (define (data type) (map rec-data (records-of-type path type)))
  (define perm (car (data 'perm)))
  (check-equal? (hash-ref perm 'tool) "bash")
  (check-equal? (hash-ref (hash-ref perm 'input) 'command) "echo hi")
  (check-equal? (map (lambda (h) (hash-ref h 'decision)) (data 'perm-answer)) '("yes"))
  (define reqs (data 'req))
  (check-equal? (map (lambda (h) (list (hash-ref h 'input) (hash-ref h 'output))) reqs) '((20 3) (30 4)))
  (check-true (real? (hash-ref (car reqs) 'cost)))                 ; deepseek-v4-flash 有单价
  (define tool (car (data 'tool)))
  (check-equal? (list (hash-ref tool 'id) (hash-ref tool 'name) (hash-ref tool 'error) (hash-ref tool 'source)) '("c1" "bash" #f "builtin"))
  (check-true (real? (hash-ref tool 'ms)))
  (define partial (car (data 'partial)))
  (check-equal? (hash-ref partial 'text) "partial answer so far")
  (check-equal? (hash-ref partial 'reasoning) "considering")
  (check-not-false (findf (lambda (h) (and (equal? (hash-ref h 'code) "error") (regexp-match? #rx"boom" (hash-ref h 'text)))) (data 'note)))
  ;; 顺序：partial 在中断标记之前
  (define order (for/list ([d (in-datum-log path)] #:when (and (rec? d) (memq (rec-type d) '(partial msg))))
                  (if (eq? (rec-type d) 'partial) 'partial (let ([t (message-text (rec-data d))]) (if (regexp-match? #rx"^<turn_aborted>" t) 'marker 'msg)))))
  (check-equal? (take-right (takef order (lambda (x) (not (eq? x 'marker)))) 1) '(partial))
  (check-not-false (member 'marker order)))
