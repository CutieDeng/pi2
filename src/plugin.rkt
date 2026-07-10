#lang tstring racket
;; plugin.rkt — 插件运行时核心（design-plugins.md M1）
;;
;; 利用 Racket 的运行时解释与安全求值能力，把第三方扩展载入 pi++：
;;   · trusted 插件：dynamic-require 直载，全权（对标 pi 的默认行为）。
;;   · sandbox 插件：make-module-evaluator 在受限求值器内运行不可信代码——限内存/时间/
;;     文件/网络，per-plugin custodian 回收。恶意插件被资源限制关停，宿主存活。
;;     （这是 pi「全信任、无代码沙箱」所缺失的能力。）
;;
;; 扩展面（本 PoC 覆盖工具；命令/钩子留桩，后续阶段接 loop/repl/console）：
;;   register-tool! / register-command! / on!  经 plugin-api 传给插件。
;; 工具进入**可变注册表**（mutable-registry），支持插件动态增/覆盖工具。

(require
 racket/sandbox
 (file "tool.rkt")
) ; end require

;; ------------------------------------------------------------ 可变注册表

;; 名字→工具的可变表；后注册者覆盖同名（对标 pi 的 same-name override）。
(struct mutable-registry (tbl) #:transparent)      ; tbl: mutable hash string→tool

(define (make-mutable-registry [initial '()])
  (define h (make-hash))
  (for ([t (in-list initial)]) (hash-set! h (tool-name t) t))
  (mutable-registry h)
) ; end define make-mutable-registry

(define (mreg-add! mr t) (hash-set! (mutable-registry-tbl mr) (tool-name t) t))
(define (mreg-remove! mr name) (hash-remove! (mutable-registry-tbl mr) name))
(define (mreg-lookup mr name) (hash-ref (mutable-registry-tbl mr) name #f))
(define (mreg-tools mr) (hash-values (mutable-registry-tbl mr)))
(define (mreg-specs mr) (map tool-spec (mreg-tools mr)))

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

;; ------------------------------------------------------------ 宿主与插件 API

;; 已加载插件的记录（含 custodian，用于卸载回收）
(struct loaded-plugin (name kind cust [tools #:mutable]) #:transparent)  ; kind: 'trusted|'sandbox

(struct plugin-host
  (registry     ; mutable-registry
   commands     ; mutable hash name→spec（PoC：仅存储）
   hooks        ; mutable hash event→(listof handler)（PoC：仅存储）
   plugins      ; box of (listof loaded-plugin)
  ) ; end fields
) ; end struct plugin-host

(define (make-plugin-host #:tools [initial '()])
  (plugin-host (make-mutable-registry initial) (make-hash) (make-hash) (box '()))
) ; end define make-plugin-host

(define (host-registry h) (plugin-host-registry h))
(define (host-tools h) (mreg-tools (plugin-host-registry h)))
(define (host-lookup h name) (mreg-lookup (plugin-host-registry h) name))
(define (host-commands h) (hash-copy (plugin-host-commands h)))
(define (host-hooks h event) (reverse (hash-ref (plugin-host-hooks h) event '())))

;; 传给插件的一等 API（闭包在 host + 当前 loaded-plugin 上）
(struct plugin-api (register-tool! register-command! on! ctx) #:transparent)

(define (make-plugin-api host lp)
  (plugin-api
   ;; register-tool!
   (lambda (t)
     (mreg-add! (plugin-host-registry host) t)
     (set-loaded-plugin-tools! lp (cons (tool-name t) (loaded-plugin-tools lp))))
   ;; register-command!
   (lambda (name spec) (hash-set! (plugin-host-commands host) name spec))
   ;; on!
   (lambda (event handler)
     (hash-update! (plugin-host-hooks host) event (lambda (l) (cons handler l)) '()))
   ;; ctx（PoC：最小；后续接 console/session）
   (hasheq 'host host 'plugin (loaded-plugin-name lp))
  ) ; end plugin-api
) ; end define make-plugin-api

;; ------------------------------------------------------------ 加载：trusted

;; 受信插件：dynamic-require 载入 `plugin`（注册函数）并调用。独立 custodian 便于回收。
(define (load-plugin-trusted! host path #:name [name (path->plugin-name path)])
  (define cust (make-custodian))
  (define lp (loaded-plugin name 'trusted cust '()))
  (parameterize ([current-custodian cust])
    (define register (dynamic-require path 'plugin))
    (register (make-plugin-api host lp)))
  (set-box! (plugin-host-plugins host) (cons lp (unbox (plugin-host-plugins host))))
  lp
) ; end define load-plugin-trusted!

;; ------------------------------------------------------------ 加载：sandbox

;; 沙箱插件：受限求值器载入。声明式导出 `manifest`（数据）+ `tool-run`（过程）。
;; 工具经求值器跨界调用，每次调用受内存/时间限制；触发限额 → err-outcome，宿主存活。
;; manifest 形如：(tool "name" "description")
(define (load-plugin-sandbox! host path
                              #:name [name (path->plugin-name path)]
                              #:memory-mb [memory-mb 64]
                              #:eval-limits [eval-limits (list 3 20)])   ; 秒, MB/次
  (define cust (make-custodian))
  (define lp (loaded-plugin name 'sandbox cust '()))
  (define abs (path->complete-path (string->path (path->string* path))))
  (define-values (pdir _n _q) (split-path abs))
  (define ev
    (parameterize ([current-custodian cust]
                   [sandbox-memory-limit memory-mb]
                   [sandbox-eval-limits eval-limits]
                   ;; 默认仅可读工作区与插件自身目录；写/网络需按能力另行放开（后续阶段）。
                   [sandbox-path-permissions (list (list 'read (current-directory))
                                                   (list 'read pdir))])
      (make-module-evaluator abs)))
  (define manifest (ev 'manifest))
  (define tname (cadr manifest))
  (define tdesc (caddr manifest))
  ;; 包装成 gen:tool：每次执行都经 ev 在沙箱内跑，捕获资源超限。
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
  (mreg-add! (plugin-host-registry host) t)
  (set-loaded-plugin-tools! lp (list tname))
  (set-box! (plugin-host-plugins host) (cons lp (unbox (plugin-host-plugins host))))
  lp
) ; end define load-plugin-sandbox!

;; ------------------------------------------------------------ 卸载

;; 注销插件的工具/命令/钩子并回收其 custodian（关停线程/端口/求值器）。
(define (unload-plugin! host lp)
  (for ([tname (in-list (loaded-plugin-tools lp))])
    (mreg-remove! (plugin-host-registry host) tname))
  (custodian-shutdown-all (loaded-plugin-cust lp))
  (set-box! (plugin-host-plugins host)
            (remq lp (unbox (plugin-host-plugins host))))
) ; end define unload-plugin!

;; ------------------------------------------------------------ 工具

(define (path->plugin-name p)
  (define b (if (path? p) p (string->path p)))
  (define-values (_d name _dir?) (split-path b))
  (regexp-replace #rx"\\.rkt$" (path->string name) "")
) ; end define path->plugin-name

(define (path->string* p) (if (path? p) (path->string p) p))

;; ---------------------------------------------------------------- provide

(provide
 ;; 注册表
 (struct-out mutable-registry)
 make-mutable-registry mreg-add! mreg-remove! mreg-lookup mreg-tools mreg-specs
 ;; SDK（供插件）：构造工具 + 结果 + 取参
 make-simple-tool
 (struct-out simple-tool)
 ok-outcome err-outcome input-ref input-str input-int
 ;; 宿主
 (struct-out plugin-host)
 (struct-out loaded-plugin)
 (struct-out plugin-api)
 make-plugin-host host-registry host-tools host-lookup host-commands host-hooks
 make-plugin-api
 load-plugin-trusted! load-plugin-sandbox! unload-plugin!
 path->plugin-name
) ; end provide
