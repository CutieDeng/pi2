#lang tstring racket
;; plugin.rkt — 插件运行时（design-plugins.md M1–M3）
;;
;; 利用 Racket 的运行时解释与安全求值能力载入第三方扩展：
;;   · trusted：dynamic-require 直载，全权（对标 pi 的默认）。
;;   · sandbox：make-module-evaluator 在受限求值器内跑不可信代码（限内存/时间/文件/网络），
;;     per-plugin custodian 回收。失控插件被资源限额关停，宿主存活——pi 缺失的能力。
;;
;; 扩展面（plugin-api 传给插件）：
;;   register-tool! / register-command! / register-shortcut! / on!(钩子) / ctx
;; 工具进入宿主统一的可变 registry（loop 直接读到，故模型可调用插件工具）。
;; 钩子分两类：
;;   变换型（loop 顺序咨询）：tool-call / tool-result / before-turn / context
;;   观测型（bus 分发）：tool-start / tool-end / turn-end / message / delta / error

(require
 racket/sandbox
 (file "model.rkt")
 (file "tool.rkt")
) ; end require

;; ------------------------------------------------------------ 插件 SDK（供插件构造工具）

;; 由 name/desc/level/params/run 直接构造一个 gen:tool。插件 require 本模块用它。
(struct simple-tool (name desc level params run)
  #:methods gen:tool
  [(define (tool-name t) (simple-tool-name t))
   (define (tool-spec t) (function-spec (simple-tool-name t) (simple-tool-desc t)
                                        (simple-tool-params t) '()))
   (define (tool-permission-level t) (simple-tool-level t))
   (define (tool-run t input ctx) ((simple-tool-run t) input ctx))]
) ; end struct simple-tool

(define (make-simple-tool #:name name #:desc desc #:run run
                          #:level [level 'read-only] #:params [params (hasheq)])
  (simple-tool name desc level params run)
) ; end define make-simple-tool

;; ------------------------------------------------------------ 钩子结果

(struct hook-block (reason) #:transparent)    ; 拦截（拒绝工具调用）
(struct hook-replace (value) #:transparent)   ; 替换（input/outcome/window）

;; ------------------------------------------------------------ 宿主

(struct loaded-plugin (name kind cust [tools #:mutable] [cmds #:mutable]) #:transparent)  ; kind: 'trusted|'sandbox

(struct plugin-host
  (registry     ; tool.rkt registry（可变；base + 插件工具）
   commands     ; mutable hash name→command-spec
   shortcuts    ; mutable hash key→handler
   hooks        ; mutable hash event→(listof handler)（注册序）
   plugins      ; box of (listof loaded-plugin)
   notify-box   ; box of (proc)：显示消息（console 就绪后由宿主注入）
   session-box  ; box of (or/c #f agent-state)：当前状态（loop 更新）
  ) ; end fields
) ; end struct plugin-host

(define (make-plugin-host #:registry [reg (make-registry '())])
  (plugin-host reg (make-hash) (make-hash) (make-hash) (box '())
               (box (lambda (msg . _) (void))) (box #f))
) ; end define make-plugin-host

(define (host-registry h) (plugin-host-registry h))
(define (host-tools h) (registry-tools (plugin-host-registry h)))
(define (host-lookup h name) (registry-lookup (plugin-host-registry h) name))
(define (host-commands h) (plugin-host-commands h))
(define (host-command h name) (hash-ref (plugin-host-commands h) name #f))
(define (host-shortcut h key) (hash-ref (plugin-host-shortcuts h) key #f))
(define (host-hooks h event) (reverse (hash-ref (plugin-host-hooks h) event '())))
(define (host-plugins h) (unbox (plugin-host-plugins h)))

;; console/session 注入（供 ctx 使用）
(define (host-set-notify! h proc) (set-box! (plugin-host-notify-box h) proc))
(define (host-set-session! h st) (set-box! (plugin-host-session-box h) st))

;; ------------------------------------------------------------ 插件上下文（传给命令/回调）

(struct plugin-ctx (host notify session) #:transparent)
;; notify : (-> string [sym] void)  ；session : (-> (or/c #f agent-state))

(define (make-ctx host)
  (plugin-ctx host
              (lambda (msg . t) ((unbox (plugin-host-notify-box host)) msg (if (pair? t) (car t) 'info)))
              (lambda () (unbox (plugin-host-session-box host))))
) ; end define make-ctx

;; ------------------------------------------------------------ 插件 API

(struct plugin-api (register-tool! register-command! register-shortcut! on! ctx) #:transparent)

(define (make-plugin-api host lp)
  (plugin-api
   (lambda (t)
     (registry-add! (plugin-host-registry host) t)
     (set-loaded-plugin-tools! lp (cons (tool-name t) (loaded-plugin-tools lp))))
   (lambda (name spec)
     (hash-set! (plugin-host-commands host) name spec)
     (set-loaded-plugin-cmds! lp (cons name (loaded-plugin-cmds lp))))
   (lambda (key handler) (hash-set! (plugin-host-shortcuts host) key handler))
   (lambda (event handler)
     (hash-update! (plugin-host-hooks host) event (lambda (l) (cons handler l)) '()))
   (make-ctx host)
  ) ; end plugin-api
) ; end define make-plugin-api

;; ------------------------------------------------------------ 变换型钩子（loop 咨询）

;; tool-call: handler (-> name input result)；result: #f | (hook-block reason) | (hook-replace new-input)
;; 返回 (values decision input*)，decision = 'allow | (cons 'deny reason)
(define (run-tool-call-hooks host name input)
  (let loop ([hs (host-hooks host 'tool-call)] [input input])
    (cond
      [(null? hs) (values 'allow input)]
      [else
       (define r ((car hs) name input))
       (cond
         [(hook-block? r) (values (cons 'deny (hook-block-reason r)) input)]
         [(hook-replace? r) (loop (cdr hs) (hook-replace-value r))]
         [else (loop (cdr hs) input)])]
    ) ; end cond
  ) ; end let loop
) ; end define run-tool-call-hooks

;; tool-result: handler (-> name input outcome result)；result: #f | (hook-replace new-outcome)
(define (run-tool-result-hooks host name input outcome)
  (for/fold ([outcome outcome]) ([h (in-list (host-hooks host 'tool-result))])
    (define r (h name input outcome))
    (if (hook-replace? r) (hook-replace-value r) outcome))
) ; end define run-tool-result-hooks

;; before-turn: handler (-> st (or #f message))；返回要注入的消息列表
(define (run-before-turn-hooks host st)
  (filter values (for/list ([h (in-list (host-hooks host 'before-turn))]) (h st)))
) ; end define run-before-turn-hooks

;; context: handler (-> window result)；result: #f | (hook-replace new-window)
(define (run-context-hooks host window)
  (for/fold ([window window]) ([h (in-list (host-hooks host 'context))])
    (define r (h window))
    (if (hook-replace? r) (hook-replace-value r) window))
) ; end define run-context-hooks

;; ------------------------------------------------------------ 观测型钩子（bus 分发）

(define (evt->hook-symbol e)
  (cond
    [(evt:tool-start? e) 'tool-start]
    [(evt:tool-end? e) 'tool-end]
    [(evt:turn-end? e) 'turn-end]
    [(evt:message? e) 'message]
    [(evt:delta? e) 'delta]
    [(evt:error? e) 'error]
    [else #f]
  ) ; end cond
) ; end define evt->hook-symbol

;; 返回一个可 bus-subscribe! 的处理器：把事件分发给对应观测钩子。
(define (make-host-observer host)
  (lambda (e)
    (define sym (evt->hook-symbol e))
    (when sym
      (for ([h (in-list (host-hooks host sym))])
        (with-handlers ([exn:fail? (lambda (_e) (void))]) (h e)))))
) ; end define make-host-observer

;; ------------------------------------------------------------ 加载：trusted

(define (load-plugin-trusted! host path #:name [name (path->plugin-name path)])
  (define cust (make-custodian))
  (define lp (loaded-plugin name 'trusted cust '() '()))
  (parameterize ([current-custodian cust])
    (define register (dynamic-require path 'plugin))
    (register (make-plugin-api host lp)))
  (set-box! (plugin-host-plugins host) (cons lp (host-plugins host)))
  lp
) ; end define load-plugin-trusted!

;; ------------------------------------------------------------ 加载：sandbox

;; 声明式导出 manifest=(tool "name" "desc") + tool-run(过程)。工具经求值器跨界执行，受限额。
(define (load-plugin-sandbox! host path
                              #:name [name (path->plugin-name path)]
                              #:memory-mb [memory-mb 64]
                              #:eval-limits [eval-limits (list 3 20)])
  (define cust (make-custodian))
  (define lp (loaded-plugin name 'sandbox cust '() '()))
  (define abs (path->complete-path (string->path (path->string* path))))
  (define-values (pdir _n _q) (split-path abs))
  (define ev
    (parameterize ([current-custodian cust]
                   [sandbox-memory-limit memory-mb]
                   [sandbox-eval-limits eval-limits]
                   [sandbox-path-permissions (list (list 'read (current-directory))
                                                   (list 'read pdir))])
      (make-module-evaluator abs)))
  (define manifest (ev 'manifest))
  (define tname (cadr manifest))
  (define tdesc (caddr manifest))
  (define t
    (make-simple-tool
     #:name tname #:desc tdesc #:level 'read-only
     #:run (lambda (input _ctx)
             (with-handlers
               ([exn:fail:resource?
                 (lambda (e) (err-outcome f"sandbox plugin `{tname}` hit a resource limit: {(exn-message e)}"))]
                [exn:fail?
                 (lambda (e) (err-outcome f"sandbox plugin `{tname}` errored: {(exn-message e)}"))])
               (ok-outcome (format "~a" (ev (list 'tool-run (list 'quote input)))))))))
  (registry-add! (plugin-host-registry host) t)
  (set-loaded-plugin-tools! lp (list tname))
  (set-box! (plugin-host-plugins host) (cons lp (host-plugins host)))
  lp
) ; end define load-plugin-sandbox!

;; ------------------------------------------------------------ 卸载

(define (unload-plugin! host lp)
  (for ([tname (in-list (loaded-plugin-tools lp))])
    (registry-remove! (plugin-host-registry host) tname))
  (for ([cn (in-list (loaded-plugin-cmds lp))])
    (hash-remove! (plugin-host-commands host) cn))
  (custodian-shutdown-all (loaded-plugin-cust lp))
  (set-box! (plugin-host-plugins host) (remq lp (host-plugins host)))
) ; end define unload-plugin!

;; ------------------------------------------------------------ 发现（plugins/ 目录）

;; 目录里每个 .rkt 视为插件。含 `plugin.rktd` 清单则据其 kind 决定 trusted/sandbox；
;; 否则：provide `plugin` → trusted；provide `manifest`+`tool-run` → sandbox（用 #lang 探测代价高，
;; 简化为：文件名以 `-sandbox` 结尾 → sandbox，否则 trusted）。可被清单覆盖。
(define (discover-plugin-files dir)
  (if (directory-exists? dir)
      (for/list ([f (in-directory dir)]
                 #:when (and (file-exists? f) (regexp-match? #rx"\\.rkt$" (path->string f))))
        f)
      '())
) ; end define discover-plugin-files

(define (plugin-kind-of path)
  (if (regexp-match? #rx"-sandbox\\.rkt$" (path->string* path)) 'sandbox 'trusted)
) ; end define plugin-kind-of

;; 载入一个目录下的全部插件，返回 host。载入失败的插件记录到 errors。
(define (load-plugins-dir! host dir #:on-error [on-error void])
  (for ([f (in-list (discover-plugin-files dir))])
    (with-handlers ([exn:fail? (lambda (e) (on-error (path->string f) (exn-message e)))])
      (case (plugin-kind-of f)
        [(sandbox) (load-plugin-sandbox! host f)]
        [else (load-plugin-trusted! host f)])))
  host
) ; end define load-plugins-dir!

;; ------------------------------------------------------------ 工具

(define (path->plugin-name p)
  (define b (if (path? p) p (string->path p)))
  (define-values (_d name _dir?) (split-path b))
  (regexp-replace #rx"\\.rkt$" (path->string name) "")
) ; end define path->plugin-name

(define (path->string* p) (if (path? p) (path->string p) p))

;; ---------------------------------------------------------------- provide

(provide
 ;; SDK（供插件）
 make-simple-tool (struct-out simple-tool)
 (struct-out hook-block) (struct-out hook-replace)
 ok-outcome err-outcome input-ref input-str input-int
 ;; 宿主
 (struct-out plugin-host) (struct-out loaded-plugin) (struct-out plugin-api) (struct-out plugin-ctx)
 make-plugin-host host-registry host-tools host-lookup host-commands host-command
 host-shortcut host-hooks host-plugins host-set-notify! host-set-session! make-ctx
 make-plugin-api
 ;; 钩子运行器（loop 用）
 run-tool-call-hooks run-tool-result-hooks run-before-turn-hooks run-context-hooks
 make-host-observer
 ;; 加载
 load-plugin-trusted! load-plugin-sandbox! unload-plugin!
 discover-plugin-files plugin-kind-of load-plugins-dir!
 path->plugin-name
) ; end provide
