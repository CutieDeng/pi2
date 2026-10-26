#lang tstring racket
;; route.rkt — 路由池（design-gui.md §15.3）：有序成员表，成员 = (帐号实例, 模型)，可以跨供应商。
;;
;;   亲和：会话固定在一个成员上（默认第一个可用的 = 用户的主帐号）；当前成员不可用才切到下一个，切过去不主动切回。
;;   健康：全局（所有会话共享）——ok / cooling（限流、瞬时、网络，按连续失败次数退避，到期恢复）
;;         / exhausted（额度，到次日 0 点）/ auth（鉴权失败，换 key 后恢复）。存到配置目录，重启后保留。
;;   切换在 provider 层完成（对外是一个 provider）：成员在**还没吐出任何内容**时出可切换类错误 → 标记、换下一个、重发；
;;   已经吐出内容后出错 → 标记并把错误交给上层（loop 的重试会再请求，届时选到下一个成员）。
;;   子 agent：轮线程里 parameterize 的 current-affinity / current-dispatch-override 被子 agent 继承 → 用父会话的亲和状态。
;;   跨端点：带签名的 thinking 块只回给产生它的端点（按消息记来源端点；来源不明的按会话原 endpoint 判断）。
;;
;; 池定义：{config-home}/routes.rktd：(list (list 名 (list (list 实例 模型) …)) …)。
;; 不改 config（prefab，不加字段）：成员的 endpoint / key / model 在请求时套到 current-config 上。

(require
 racket/async-channel
 racket/file
 racket/list
 racket/string
 (file "model.rkt")
 (file "provider.rkt")
 (file "plugin.rkt")
 (file "providers.rkt")
 (file "credentials.rkt")
 (file "retry.rkt")
 (file "adapt.rkt")                     ; 增强适配：按成员（帐号 + 模型）匹配
) ; end require

;; ---------------------------------------------------------------- 定义

(struct rmember (instance model) #:transparent)   ; 路由成员（不叫 member：会遮住 racket 的 member）
(struct route (name members) #:transparent)

(define (routes-path) (build-path (config-home) "routes.rktd"))
(define (health-path) (build-path (config-home) "route-health.rktd"))

(define (read-rktd path default)
  (with-handlers ([exn:fail? (lambda (_e) default)])
    (if (file-exists? path)
        (call-with-input-file path (lambda (in) (parameterize ([read-accept-reader #f] [read-accept-lang #f]) (read in))))
        default))
) ; end define read-rktd

(define (write-rktd! path v)
  (make-directory* (config-home))
  (define tmp (path-add-extension path #".tmp"))
  (call-with-output-file tmp #:exists 'truncate (lambda (out) (write v out)))
  (rename-file-or-directory tmp path #t)
) ; end define write-rktd!

;; 池名不带「@」；对外（供应商名空间里）用「@名」表示池
(define (route-ref-name? s) (and (string? s) (> (string-length s) 1) (char=? (string-ref s 0) #\@)))
(define (route-ref->name s) (substring s 1))

(define (load-routes)
  (define raw (read-rktd (routes-path) '()))
  (if (list? raw)
      (for/list ([r (in-list raw)]
                 #:when (and (list? r) (= (length r) 2) (string? (car r)) (list? (cadr r))))
        (route (car r) (for/list ([m (in-list (cadr r))]
                                  #:when (and (list? m) (= (length m) 2) (string? (car m)) (string? (cadr m))))
                         (rmember (car m) (cadr m)))))
      '())
) ; end define load-routes

(define (save-routes! rs)
  (write-rktd! (routes-path) (for/list ([r (in-list rs)])
                               (list (route-name r) (for/list ([m (in-list (route-members r))]) (list (rmember-instance m) (rmember-model m))))))
) ; end define save-routes!

(define (route-by-name name) (findf (lambda (r) (equal? (route-name r) name)) (load-routes)))

;; 新建 / 覆盖；成员为空 = 删除
(define (define-route! name members)
  (define others (filter (lambda (r) (not (equal? (route-name r) name))) (load-routes)))
  (save-routes! (if (null? members) others (append others (list (route name members)))))
) ; end define define-route!

;; ---------------------------------------------------------------- 健康（全局）

;; state ∈ ok cooling exhausted auth；until：秒或 #f；fp：鉴权失败时 token 的指纹（token 变了就恢复）
(struct health (state until reason fails fp) #:prefab)
(define OK-HEALTH (health 'ok #f #f 0 #f))

(define health-table #f)                      ; 惰性加载
(define health-sema (make-semaphore 1))

(define (member-key m) (format "~a|~a" (rmember-instance m) (rmember-model m)))

(define (table)
  (unless health-table
    (define raw (read-rktd (health-path) (hash)))
    (set! health-table (make-hash (if (hash? raw) (for/list ([(k v) (in-hash raw)] #:when (and (string? k) (health? v))) (cons k v)) '()))))
  health-table
) ; end define table

(define (save-health!)
  (with-handlers ([exn:fail? void])
    (write-rktd! (health-path) (for/hash ([(k v) (in-hash (table))] #:unless (eq? (health-state v) 'ok)) (values k v))))
) ; end define save-health!

(define (token-fp inst)
  (define-values (base label) (parse-instance inst))
  (define tok (with-handlers ([exn:fail? (lambda (_e) #f)]) (resolve-provider-token base label)))
  (and tok (equal-hash-code tok))
) ; end define token-fp

;; 次日 0 点（本地时间）
(define (next-midnight now)
  (define d (seconds->date now))
  (+ now (- 86400 (+ (* 3600 (date-hour d)) (* 60 (date-minute d)) (date-second d))))
) ; end define next-midnight

(define (member-health m) (call-with-semaphore health-sema (lambda () (hash-ref (table) (member-key m) OK-HEALTH))))

;; 现在是否可用（到期的冷却 / 用尽、换过 key 的鉴权失败 → 可用）
(define (member-available? m [now (current-seconds)])
  (define h (member-health m))
  (case (health-state h)
    [(ok) #t]
    [(cooling exhausted) (and (health-until h) (>= now (health-until h)))]
    [(auth) (not (equal? (health-fp h) (token-fp (rmember-instance m))))]
    [else #t])
) ; end define member-available?

;; 出错 → 更新健康；返回新的 health 或 #f（这类错误不影响成员可用性）
(define (mark-failure! m cat reason [now (current-seconds)])
  (define h0 (member-health m))
  (define fails (add1 (health-fails h0)))                       ; 连续失败次数（成功一次清零）
  (define h
    (case cat
      [(rate transient) (health 'cooling (+ now (min 600 (* 30 (expt 2 (min 5 (sub1 fails)))))) reason fails #f)]
      [(quota) (health 'exhausted (next-midnight now) reason fails #f)]
      [(auth) (health 'auth #f reason fails (token-fp (rmember-instance m)))]
      [else #f]))
  (when h
    (call-with-semaphore health-sema (lambda () (hash-set! (table) (member-key m) h)))
    (save-health!)
    (notify! 'health (list m h)))
  h
) ; end define mark-failure!

(define (mark-ok! m)
  (define h0 (member-health m))
  (unless (and (eq? (health-state h0) 'ok) (zero? (health-fails h0)))
    (call-with-semaphore health-sema (lambda () (hash-set! (table) (member-key m) OK-HEALTH)))
    (save-health!)
    (notify! 'health (list m OK-HEALTH)))
) ; end define mark-ok!

;; 手动恢复（界面「恢复」/ 测试）
(define (reset-member! m)
  (call-with-semaphore health-sema (lambda () (hash-remove! (table) (member-key m))))
  (save-health!)
  (notify! 'health (list m OK-HEALTH))
) ; end define reset-member!

;; 测试用：换配置目录后重新加载
(define (reload-health!) (set! health-table #f))

;; ---------------------------------------------------------------- 变化通知

(define listeners (box '()))
(define (on-route-change! f) (set-box! listeners (cons f (unbox listeners))))
(define (off-route-change! f) (set-box! listeners (remq f (unbox listeners))))
(define (notify! kind data) (for ([f (in-list (unbox listeners))]) (with-handlers ([exn:fail? void]) (f kind data))))

;; ---------------------------------------------------------------- 亲和

;; 一个会话一个；index：当前固定的成员下标；provider：本亲和的路由 provider（惰性建）；
;; base-endpoint：开始路由前会话自己的 endpoint（判断来源不明的 thinking 块）
(struct affinity (route [index #:mutable] [provider #:mutable] base-endpoint))
(define current-affinity (make-parameter #f))

;; 模型覆盖：父代理给子 agent 指定的模型（spawn_agent 的 model 参数）。帐号仍按亲和取成员，只换模型。
(define current-model-override (make-parameter #f))

(define (make-affinity r #:base-endpoint [base-ep #f]) (affinity r 0 #f base-ep))

(define (affinity-member aff) (list-ref (route-members (affinity-route aff)) (affinity-index aff)))

;; 选成员：当前可用就留着；否则从下一个开始找第一个可用的（绕回）。→ 下标或 #f（全部不可用）
(define (pick! aff [now (current-seconds)])
  (define ms (route-members (affinity-route aff)))
  (define n (length ms))
  (define i0 (affinity-index aff))
  (define idx (for/first ([k (in-range n)]
                          #:when (member-available? (list-ref ms (modulo (+ i0 k) n)) now))
                (modulo (+ i0 k) n)))
  (when (and idx (not (= idx i0)))
    (define from (list-ref ms i0))
    (set-affinity-index! aff idx)
    (notify! 'switch (list aff from (list-ref ms idx) (health-reason (member-health from)))))
  idx
) ; end define pick!

;; 手动固定到某个成员（界面点成员）
(define (pin! aff idx)
  (when (< -1 idx (length (route-members (affinity-route aff))))
    (define from (affinity-member aff))
    (set-affinity-index! aff idx)
    (notify! 'switch (list aff from (affinity-member aff) 'manual)))
) ; end define pin!

;; ---------------------------------------------------------------- 跨端点：thinking 块的来源

(define provenance (make-weak-hasheq))          ; 助手消息 → 产生它的 endpoint

(define (strip-foreign-thinking msgs endpoint base-endpoint)
  (for/list ([m (in-list msgs)])
    (cond
      [(and (eq? (message-role m) 'assistant) (ormap thinking-block? (message-blocks m)))
       (define src (hash-ref provenance m base-endpoint))
       (if (equal? src endpoint)
           m
           (struct-copy message m [blocks (filter (lambda (b) (not (thinking-block? b))) (message-blocks m))]))]
      [else m]))
) ; end define strip-foreign-thinking

;; ---------------------------------------------------------------- 路由 provider

;; 成员的 config：会话 config 上套档案（endpoint / key）与成员模型
(define (member-config base-cfg m)
  (struct-copy config (apply-provider-profile base-cfg (rmember-instance m)) [model (rmember-model m)])
) ; end define member-config

(define (all-unavailable-error r)
  (exn:fail (format "route @~a: all members unavailable (~a)" (route-name r)
                    (string-join (for/list ([m (in-list (route-members r))])
                                   (define h (member-health m))
                                   (format "~a ~a: ~a" (rmember-instance m) (rmember-model m) (or (health-reason h) (health-state h))))
                                 "; "))
            (current-continuation-marks))
) ; end define all-unavailable-error

(define (make-route-provider aff host)
  (define cache (make-hash))                       ; 实例名 → 具体 provider（按实例的线路工厂建）
  (define forwarders (make-stream-set))
  (define cancelled (box #f))
  (define (concrete m cfg)
    (hash-ref! cache (rmember-instance m)
               (lambda ()
                 (define f (host-provider host (rmember-instance m)))
                 (cond [f (f cfg)]
                       [else (error 'route "unknown provider for member: ~a" (rmember-instance m))]))))
  (provider
   "route"
   (lambda (msgs tools)
     (define out (make-async-channel))
     (define base (or (current-config) (default-config)))
     (define eff (current-reasoning-effort))
     (define model-override (current-model-override))
     (set-box! cancelled #f)
     (stream-set-spawn! forwarders
      (lambda ()
        (with-handlers ([exn:fail? (lambda (e) (async-channel-put out (evt:error (now-ms) e #f)))])
          (let attempt ()
            (define r (affinity-route aff))
            (define idx (pick! aff))
            (cond
              [(not idx) (async-channel-put out (evt:error (now-ms) (all-unavailable-error r) #f))]
              [else
               (define m (list-ref (route-members r) idx))
               (define cfg0 (let ([c (member-config base m)]) (if model-override (struct-copy config c [model model-override]) c)))
               (define p (concrete m cfg0))
               (define-values (cfg msgs1 tools* _applied)
                 (apply-adaptations (rmember-instance m) cfg0 (strip-foreign-thinking msgs (config-endpoint cfg0) (affinity-base-endpoint aff)) tools eff))
               (define msgs* msgs1)
               (define ch (parameterize ([current-config cfg] [current-reasoning-effort eff]) (provider-stream! p msgs* tools*)))
               (let fwd ([saw-data? #f])
                 (define e (sync ch))
                 (cond
                   [(evt:error? e)
                    (define cat (err-category (evt:error-exn e)))
                    (define h (and (not (unbox cancelled)) (mark-failure! m cat (exn-message (evt:error-exn e)))))
                    ;; 还没吐内容、这类错误让成员不可用、且有别的成员可用 → 换成员重发；否则把错误交给上层
                    (if (and h (not saw-data?) (let ([n (pick! aff)]) (and n (not (= n idx)))))
                        (attempt)
                        (async-channel-put out e))]
                   [(evt:message? e)
                    (hash-set! provenance (evt:message-msg e) (config-endpoint cfg))
                    (async-channel-put out e)
                    (fwd #t)]
                   [(evt:turn-end? e) (mark-ok! m) (async-channel-put out e)]
                   [else (async-channel-put out e) (fwd (or saw-data? (evt:delta? e)))]))])))))
     out)
   ;; 取消：本路由的转发线程 + 所有成员 provider 的在途请求（父会话与子 agent 可能在不同成员上）
   (lambda () (set-box! cancelled #t) (stream-set-cancel-all! forwarders) (for ([p (in-list (hash-values cache))]) (provider-cancel! p))))
) ; end define make-route-provider

(define (affinity-route-provider! aff host)
  (or (affinity-provider aff)
      (let ([p (make-route-provider aff host)]) (set-affinity-provider! aff p) p))
) ; end define affinity-route-provider!

;; 在路由下跑 thunk：本线程（及其中创建的线程：工具、子 agent）的请求都走这个亲和
(define (call-with-route aff host thunk)
  (if aff
      (parameterize ([current-affinity aff] [current-dispatch-override (affinity-route-provider! aff host)]) (thunk))
      (thunk))
) ; end define call-with-route

;; ---------------------------------------------------------------- 对外描述（RPC / 界面）

(define (health->jsexpr h now)
  (hasheq 'state (symbol->string (health-state h))
          'until (or (health-until h) 'null)
          'seconds_left (if (health-until h) (max 0 (- (health-until h) now)) 'null)
          'reason (or (health-reason h) 'null))
) ; end define health->jsexpr

(define (member->jsexpr m [now (current-seconds)])
  (define h (member-health m))
  (hash-set* (health->jsexpr h now)
             'instance (rmember-instance m) 'model (rmember-model m)
             'available (member-available? m now))
) ; end define member->jsexpr

(define (route->jsexpr r [aff #f] [now (current-seconds)])
  (hasheq 'name (route-name r)
          'members (for/list ([m (in-list (route-members r))]) (member->jsexpr m now))
          'current (if (and aff (equal? (route-name (affinity-route aff)) (route-name r))) (affinity-index aff) 'null)
          'unavailable (for/sum ([m (in-list (route-members r))]) (if (member-available? m now) 0 1)))
) ; end define route->jsexpr

(provide
 (struct-out rmember) (struct-out route) (struct-out health)
 route-ref-name? route-ref->name
 load-routes save-routes! route-by-name define-route!
 member-health member-available? mark-failure! mark-ok! reset-member! reload-health! next-midnight
 on-route-change! off-route-change!
 (struct-out affinity) current-affinity current-model-override make-affinity affinity-member pick! pin!
 strip-foreign-thinking member-config make-route-provider affinity-route-provider! call-with-route
 member->jsexpr route->jsexpr
) ; end provide
