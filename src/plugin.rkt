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
 racket/path
 racket/async-channel
 (file "model.rkt")
 (file "tool.rkt")
 (file "provider.rkt")
 (file "rktd.rkt")
 (file "tui/keys.rkt")
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

;; 便捷：由 reply 函数构造一个 provider（供插件写自定义 LLM 供应商）。
;; reply : (listof message) -> string。宿主自建通道/线程/事件：先 delta（供渲染显示），
;; 再 message（入历史），再 turn-end。
(define (make-simple-provider #:name name #:reply reply)
  (provider
   name
   (lambda (msgs _tool-specs)
     (define ch (make-async-channel))
     (thread
      (lambda ()
        (with-handlers ([exn:fail? (lambda (e) (async-channel-put ch (evt:error (now-ms) e #f)))])
          (define txt (reply msgs))
          (async-channel-put ch (evt:delta (now-ms) 'text txt))                    ; 显示
          (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant txt)))  ; 入历史
          (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 0 0))))))
     ch)
   void)
) ; end define make-simple-provider

;; ------------------------------------------------------------ 能力授权（grants）

;; 持久化的能力授权集合。key = "name|cap"（cap: 'trust 或能力符号 fs-write/network/exec）。
;; 复用 pi2 权限体系语义：yes=本次、always=持久、no=拒绝。
(struct grants (set path) #:transparent)   ; set: mutable hash; path: .rktd store 或 #f

(define (grant-key name cap) (format "~a|~a" name cap))

(define (make-grants [path #f])
  (define s (make-hash))
  (when (and path (file-exists? path))
    (with-handlers ([exn:fail? (lambda (_e) (void))])
      (for ([d (in-datum-log path)]
            #:when (and (pair? d) (eq? (car d) 'grant) (>= (length d) 3)))
        (hash-set! s (grant-key (cadr d) (caddr d)) #t))))
  (grants s path)
) ; end define make-grants

(define (grants-has? g name cap) (hash-ref (grants-set g) (grant-key name cap) #f))

(define (grants-add! g name cap #:persist? [persist? #t])
  (hash-set! (grants-set g) (grant-key name cap) #t)
  (when (and persist? (grants-path g))
    (define lg (datum-log-open! (grants-path g)))
    (datum-log-append! lg (list 'grant name cap))
    (datum-log-close! lg))
) ; end define grants-add!

;; 沙箱插件的声明式能力：旁置 <base>.rktd 内 (caps fs-write network …)。安全读取（不执行代码）。
(define (read-plugin-caps path)
  (define sidecar (path-replace-extension (string->path (path->string* path)) #".rktd"))
  (if (file-exists? sidecar)
      (with-handlers ([exn:fail? (lambda (_e) '())])
        (define d (datum-log-first sidecar))
        (if (and (pair? d) (eq? (car d) 'caps)) (cdr d) '()))
      '())
) ; end define read-plugin-caps

;; 能力 → 沙箱 path-permissions。默认只读 cwd+插件目录；fs-write 加写 cwd；fs-read 加读根。
(define (caps->path-permissions granted cwd pdir)
  (append (list (list 'read cwd) (list 'read pdir))
          (if (memq 'fs-write granted) (list (list 'write cwd)) '())
          (if (memq 'fs-read granted) (list (list 'read (bytes->path #"/"))) '()))
) ; end define caps->path-permissions

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
   providers    ; mutable hash name→factory (config→provider)
   plugins      ; box of (listof loaded-plugin)
   notify-box   ; box of (proc)：显示消息（console 就绪后由宿主注入）
   session-box  ; box of (or/c #f agent-state)：当前状态（loop 更新）
   select-box   ; box of (title options -> (or/c #f string))：选框（repl 注入 console-choose!）
   confirm-box  ; box of (title -> boolean)：确认框
   provider-sel ; box of string：当前选用的 provider 名（/provider 运行时切换）
   skills       ; box of (listof resource)：发现的技能（渐进披露进系统提示词）
   prompts      ; box of (listof resource)：发现的提示词（/prompt 激活）
  ) ; end fields
) ; end struct plugin-host

(define (make-plugin-host #:registry [reg (make-registry '())])
  (plugin-host reg (make-hash) (make-hash) (make-hash) (make-hash) (box '())
               (box (lambda (msg . _) (void))) (box #f)
               (box (lambda (_t _o) #f)) (box (lambda (_t) #f)) (box "lmstudio")
               (box '()) (box '()))
) ; end define make-plugin-host

(define (host-skills h) (unbox (plugin-host-skills h)))
(define (host-prompts h) (unbox (plugin-host-prompts h)))
(define (host-set-skills! h ss) (set-box! (plugin-host-skills h) ss))
(define (host-set-prompts! h ps) (set-box! (plugin-host-prompts h) ps))

(define (host-registry h) (plugin-host-registry h))
(define (host-tools h) (registry-tools (plugin-host-registry h)))
(define (host-lookup h name) (registry-lookup (plugin-host-registry h) name))
(define (host-commands h) (plugin-host-commands h))
(define (host-command h name) (hash-ref (plugin-host-commands h) name #f))
(define (host-shortcut h key) (hash-ref (plugin-host-shortcuts h) key #f))
(define (host-hooks h event) (reverse (hash-ref (plugin-host-hooks h) event '())))
(define (host-plugins h) (unbox (plugin-host-plugins h)))
;; 供应商实例名 "base[label]" → base（无方括号则原样）。供 provider 选用/分发按 base 解析工厂，
;; 而 label 只影响用哪套 token（token 装进 config 由上层 apply-provider-profile 处理）。
;; 此处仅做纯字符串剥离，避免依赖 providers.rkt（保持内核层次）。
(define (provider-base-name name)
  (define m (regexp-match #rx"^([^][]+)\\[" name))
  (if m (cadr m) name))
;; provider 工厂：返回 (config→provider) 或 #f；"lmstudio" 未注册时由宿主回退到内置 openai 兼容。
(define (host-provider h name) (hash-ref (plugin-host-providers h) (provider-base-name name) #f))
(define (host-provider-names h) (hash-keys (plugin-host-providers h)))
;; 可选 provider = 默认本地 lmstudio + 注册的内置/插件供应商（去重）
(define (host-available-providers h) (remove-duplicates (cons "lmstudio" (host-provider-names h))))
(define (host-current-provider h) (unbox (plugin-host-provider-sel h)))
;; 校验并切换当前 provider；未知名返回 #f（不改）。实例名按 base 校验。
(define (host-set-provider! h name)
  (and (member (provider-base-name name) (host-available-providers h))
       (begin (set-box! (plugin-host-provider-sel h) name) #t)))

;; 分发 provider：每次请求按 host 当前选用名解析真实 provider（惰性构建 + 缓存），委派其
;; stream!/cancel!。故 /provider 改选用名即运行时切换供应商。base-cfg 仅供工厂初始化
;; （provider 运行时读 current-config 取 live 值）。未知名 → 该请求以 error 事件返回。
(define (make-dispatch-provider host base-cfg)
  (define cache (make-hash))
  (define last (box #f))
  (define (resolve name)
    (hash-ref! cache name
      (lambda ()
        (define factory (host-provider host name))
        (cond
          [factory (factory base-cfg)]
          [(string=? name "lmstudio") (make-openai-provider base-cfg)]
          [else (error 'provider "unknown provider: ~a" name)]))))
  (provider
   "dispatch"
   (lambda (msgs tool-specs)
     (with-handlers ([exn:fail?
                      (lambda (e)
                        (define ch (make-async-channel))
                        (async-channel-put ch (evt:error (now-ms) e #f))
                        ch)])
       (define p (resolve (host-current-provider host)))
       (set-box! last p)
       (provider-stream! p msgs tool-specs)))
   (lambda () (when (unbox last) (provider-cancel! (unbox last)))))
) ; end define make-dispatch-provider

;; console/session UI 注入（供 ctx 使用；repl 就绪后接 console）
(define (host-set-notify! h proc) (set-box! (plugin-host-notify-box h) proc))
(define (host-set-session! h st) (set-box! (plugin-host-session-box h) st))
(define (host-set-select! h proc) (set-box! (plugin-host-select-box h) proc))
(define (host-set-confirm! h proc) (set-box! (plugin-host-confirm-box h) proc))

;; ------------------------------------------------------------ 插件上下文（传给命令/回调）

(struct plugin-ctx (host notify session select confirm) #:transparent)
;; notify:(-> string [sym] void)  session:(-> (or #f agent-state))
;; select:(-> title options (or #f string))  confirm:(-> title boolean)

(define (make-ctx host)
  (plugin-ctx host
              (lambda (msg . t) ((unbox (plugin-host-notify-box host)) msg (if (pair? t) (car t) 'info)))
              (lambda () (unbox (plugin-host-session-box host)))
              (lambda (title options) ((unbox (plugin-host-select-box host)) title options))
              (lambda (title) ((unbox (plugin-host-confirm-box host)) title)))
) ; end define make-ctx

;; ------------------------------------------------------------ 插件 API

(struct plugin-api (register-tool! register-command! register-shortcut! register-provider! on! ctx) #:transparent)

(define (make-plugin-api host lp)
  (plugin-api
   (lambda (t)
     (registry-add! (plugin-host-registry host) t)
     (set-loaded-plugin-tools! lp (cons (tool-name t) (loaded-plugin-tools lp))))
   (lambda (name spec)
     (hash-set! (plugin-host-commands host) name spec)
     (set-loaded-plugin-cmds! lp (cons name (loaded-plugin-cmds lp))))
   (lambda (key handler) (hash-set! (plugin-host-shortcuts host) key handler))
   (lambda (name factory) (hash-set! (plugin-host-providers host) name factory))
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
                              #:eval-limits [eval-limits (list 3 20)]
                              #:caps [caps '()])      ; 已授予的能力（放开对应沙箱权限）
  (define cust (make-custodian))
  (define lp (loaded-plugin name 'sandbox cust '() '()))
  (define abs (path->complete-path (string->path (path->string* path))))
  (define-values (pdir _n _q) (split-path abs))
  (define ev
    (parameterize ([current-custodian cust]
                   [sandbox-memory-limit memory-mb]
                   [sandbox-eval-limits eval-limits]
                   ;; 默认仅可读工作区与插件目录；按授予能力放开写/读根（未授予者沙箱硬拒）。
                   [sandbox-path-permissions (caps->path-permissions caps (current-directory) pdir)])
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

;; 受信插件加载（带信任门）：已授予或无门 → 直载；否则经 asker 询问信任（yes/always/no）。
;; asker : (-> string (or/c 'yes 'always 'no))。返回 lp 或 #f（被拒绝跳过）。
(define (gated-load-trusted! host f g asker)
  (define name (path->plugin-name f))
  (cond
    [(or (not g) (not asker)) (load-plugin-trusted! host f)]
    [(grants-has? g name 'trust) (load-plugin-trusted! host f)]
    [else
     (case (asker f"Trust plugin `{name}` — it will run with FULL access?")
       [(always) (grants-add! g name 'trust) (load-plugin-trusted! host f)]
       [(yes) (load-plugin-trusted! host f)]
       [else #f])]              ; 拒绝：不加载
  ) ; end cond
) ; end define gated-load-trusted!

;; 沙箱插件加载（带能力门）：读声明能力，逐项授权，以已授予能力集载入。
(define (gated-load-sandbox! host f g asker)
  (define name (path->plugin-name f))
  (define caps (read-plugin-caps f))
  (define granted
    (cond
      [(null? caps) '()]                                  ; 无能力需求 → 默认安全加载
      [(or (not g) (not asker)) caps]                     ; 无门 → 全给
      [else
       (filter (lambda (cap)
                 (or (grants-has? g name cap)
                     (case (asker f"Grant capability `{cap}` to sandbox plugin `{name}`?")
                       [(always) (grants-add! g name cap) #t]
                       [(yes) #t]
                       [else #f])))
               caps)]))
  (load-plugin-sandbox! host f #:caps granted)
) ; end define gated-load-sandbox!

;; 载入一个目录下的全部插件，返回 host。给 #:grants + #:asker 则启用信任/能力授权门。
(define (load-plugins-dir! host dir #:on-error [on-error void] #:grants [g #f] #:asker [asker #f])
  (for ([f (in-list (discover-plugin-files dir))])
    (with-handlers ([exn:fail? (lambda (e) (on-error (path->string f) (exn-message e)))])
      (case (plugin-kind-of f)
        [(sandbox) (gated-load-sandbox! host f g asker)]
        [else (gated-load-trusted! host f g asker)])))
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
 make-simple-provider
 kchar knamed                          ; 构造快捷键 kev（供 register-shortcut!）
 (struct-out hook-block) (struct-out hook-replace)
 ok-outcome err-outcome input-ref input-str input-int
 ;; 宿主
 (struct-out plugin-host) (struct-out loaded-plugin) (struct-out plugin-api) (struct-out plugin-ctx)
 make-plugin-host host-registry host-tools host-lookup host-commands host-command
 host-shortcut host-hooks host-plugins host-provider host-provider-names
 host-available-providers host-current-provider host-set-provider! make-dispatch-provider provider-base-name
 host-skills host-prompts host-set-skills! host-set-prompts!
 host-set-notify! host-set-session! host-set-select! host-set-confirm! make-ctx
 make-plugin-api
 ;; 钩子运行器（loop 用）
 run-tool-call-hooks run-tool-result-hooks run-before-turn-hooks run-context-hooks
 make-host-observer
 ;; 加载
 load-plugin-trusted! load-plugin-sandbox! unload-plugin!
 discover-plugin-files plugin-kind-of load-plugins-dir!
 gated-load-trusted! gated-load-sandbox!
 path->plugin-name
 ;; 能力授权
 (struct-out grants) make-grants grants-has? grants-add!
 read-plugin-caps caps->path-permissions
) ; end provide
