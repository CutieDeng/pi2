#lang tstring racket
;; route-test.rkt — 路由池（src/route.rkt）：出错切换、亲和不回切、已出内容后出错交给上层、全部不可用、额度用尽到次日、
;; 跨端点 thinking 块、子 agent 线程继承路由、健康状态落盘。假 provider，不碰网络。

(require
 rackunit
 racket/async-channel
 racket/file
 racket/date
 (file "../src/model.rkt")
 (file "../src/provider.rkt")
 (file "../src/plugin.rkt")
 (file "../src/route.rkt")
) ; end require

(define cfg-dir (make-temporary-file "pi2-route-~a" 'directory))
(void (putenv "PI_CONFIG_HOME" (path->string cfg-dir)))

;; 假 provider：behavior 盒子决定下一次请求的结果：'ok | (cons 'err 文案) | (cons 'err-after-data 文案)
;; calls：被请求的 (实例 模型 endpoint) 记录
(define calls '())
(define (fake-factory inst behavior)
  (lambda (_cfg)
    (provider inst
              (lambda (msgs _tools)
                (define ch (make-async-channel))
                (define c (current-config))
                (set! calls (append calls (list (list inst (config-model c)))))
                (define b (unbox behavior))
                (thread (lambda ()
                          (match b
                            ['ok (async-channel-put ch (evt:delta (now-ms) 'text "hi"))
                                 (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant (format "from ~a" inst))))
                                 (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 1 1)))]
                            [(cons 'err text) (async-channel-put ch (evt:error (now-ms) (exn:fail text (current-continuation-marks)) #f))]
                            [(cons 'err-after-data text)
                             (async-channel-put ch (evt:delta (now-ms) 'text "par"))
                             (async-channel-put ch (evt:error (now-ms) (exn:fail text (current-continuation-marks)) #f))])))
                ch)
              void)))

(define ba (box 'ok))
(define bb (box 'ok))
(define host (make-plugin-host))
(hash-set! (plugin-host-providers host) "fa" (fake-factory "fa" ba))
(hash-set! (plugin-host-providers host) "fb" (fake-factory "fb" bb))

(define R (route "pool" (list (rmember "fa" "m1") (rmember "fb" "m2"))))

;; 跑一次请求：→ (values 事件类型表 最后的消息文字或错误文案)
(define (request aff [msgs (list (text-msg 'user "q"))])
  (define p (affinity-route-provider! aff host))
  (define ch (parameterize ([current-config (default-config)]) (provider-stream! p msgs '())))
  (let loop ([kinds '()])
    (define e (sync/timeout 5 ch))
    (cond [(evt:error? e) (values (reverse (cons 'error kinds)) (exn-message (evt:error-exn e)))]
          [(evt:turn-end? e) (values (reverse (cons 'end kinds)) #f)]
          [(evt:message? e) (loop (cons (message-text (evt:message-msg e)) kinds))]
          [(evt:delta? e) (loop (cons 'delta kinds))]
          [else (error 'request "timeout")])))

(define switches '())
(on-route-change! (lambda (kind data) (when (eq? kind 'switch) (set! switches (append switches (list (list (rmember-instance (cadr data)) (rmember-instance (caddr data)) (cadddr data))))))))

(test-case "a rate-limited member before any output: marked cooling, the request is re-sent to the next member"
  (reload-health!)
  (set-box! ba (cons 'err "HTTP 429: rate limit reached"))
  (set-box! bb 'ok)
  (set! calls '())
  (define aff (make-affinity R))
  (define-values (kinds err) (request aff))
  (check-equal? kinds '(delta "from fb" end))                                ; 调用方只看到一次成功的回答
  (check-equal? calls '(("fa" "m1") ("fb" "m2")))                            ; 成员的模型套到 config 上
  (check-equal? (health-state (member-health (rmember "fa" "m1"))) 'cooling)
  (check-false (member-available? (rmember "fa" "m1")))
  (check-equal? (affinity-index aff) 1)
  (check-equal? (last switches) (list "fa" "fb" "HTTP 429: rate limit reached")))

(test-case "affinity: after fa recovers the session stays on fb (no switch back); a new session starts at fa"
  (reset-member! (rmember "fa" "m1"))
  (set-box! ba 'ok)
  (set! calls '())
  (define aff (make-affinity R))
  (set-affinity-index! aff 1)
  (request aff)
  (check-equal? calls '(("fb" "m2")))
  (define aff2 (make-affinity R))
  (request aff2)
  (check-equal? (last calls) '("fa" "m1")))

(test-case "an error after output started is passed up (not re-sent here) and the member is marked"
  (reload-health!)
  (for ([m (route-members R)]) (reset-member! m))
  (set-box! ba (cons 'err-after-data "stream interrupted mid-flight (network)"))
  (set! calls '())
  (define aff (make-affinity R))
  (define-values (kinds err) (request aff))
  (check-equal? kinds '(delta error))
  (check-equal? calls '(("fa" "m1")))
  (check-equal? (health-state (member-health (rmember "fa" "m1"))) 'cooling)   ; 网络中断归为瞬时
  (set-box! bb 'ok)
  (define-values (k2 _e2) (request aff))                                     ; 上层重试：选到下一个成员
  (check-equal? (last calls) '("fb" "m2")))

(test-case "all members unavailable → one error that names every member's reason"
  (for ([m (route-members R)]) (reset-member! m))
  (set-box! ba (cons 'err "HTTP 402: insufficient balance"))
  (set-box! bb (cons 'err "HTTP 401: invalid api key"))
  (define aff (make-affinity R))
  (define-values (kinds err) (request aff))
  (check-equal? kinds '(error))
  (check-equal? (health-state (member-health (rmember "fa" "m1"))) 'exhausted)
  (check-equal? (health-state (member-health (rmember "fb" "m2"))) 'auth)
  (define-values (k2 e2) (request aff))
  (check-regexp-match #rx"all members unavailable" e2)
  (check-regexp-match #rx"insufficient balance" e2)
  (check-regexp-match #rx"invalid api key" e2))

(test-case "quota exhaustion lasts until the next local midnight; health survives a restart (file)"
  (define now (find-seconds 0 30 22 23 10 2026))                               ; 22:30:00
  (check-equal? (- (next-midnight now) now) 5400)
  (reload-health!)                                                            ; 重新从文件读
  (check-equal? (health-state (member-health (rmember "fa" "m1"))) 'exhausted)
  (check-false (member-available? (rmember "fa" "m1")))
  (check-true (member-available? (rmember "fa" "m1") (+ (current-seconds) 90000))))

(test-case "thinking blocks go back only to the endpoint that produced them"
  (define a1 (message 'assistant (list (thinking-block "t" "sig-A") (text-block "x"))))
  (define a2 (message 'assistant (list (thinking-block "t2" "sig-?") (text-block "y"))))
  (define msgs (list (text-msg 'user "q") a1 a2))
  ;; a1 来源未知 → 按会话原 endpoint（"http://A"）判断；a2 同理
  (define to-A (strip-foreign-thinking msgs "http://A" "http://A"))
  (check-equal? (length (message-blocks (second to-A))) 2)
  (define to-B (strip-foreign-thinking msgs "http://B" "http://A"))
  (check-equal? (map (lambda (b) (thinking-block? b)) (message-blocks (second to-B))) '(#f))
  (check-equal? (message-blocks (first to-B)) (message-blocks (first msgs))))

(test-case "threads started inside call-with-route (tools, subagents) use the same route"
  (for ([m (route-members R)]) (reset-member! m))
  (set-box! ba 'ok)
  (define aff (make-affinity R))
  (define seen
    (call-with-route aff host
                     (lambda ()
                       (define ch (make-channel))
                       (thread (lambda () (channel-put ch (list (current-dispatch-override) (current-affinity)))))
                       (channel-get ch))))
  (check-eq? (first seen) (affinity-route-provider! aff host))
  (check-eq? (second seen) aff)
  (check-false (current-dispatch-override)))                                ; 外面不受影响

(test-case "routes file: define, read back, delete"
  (define-route! "p1" (list (rmember "fa" "m1") (rmember "fb" "m2")))
  (check-equal? (route-members (route-by-name "p1")) (list (rmember "fa" "m1") (rmember "fb" "m2")))
  (define-route! "p1" '())
  (check-false (route-by-name "p1")))

(delete-directory/files cfg-dir)
