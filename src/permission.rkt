#lang tstring racket
;; permission.rkt — 权限门控（design.md §4.7）
;; 三档模式 × 工具权限级决策矩阵 + always 记忆（经 rktd 持久化）。

(require
 racket/string
 (file "model.rkt")
 (file "tool.rkt")
 (file "rktd.rkt")
) ; end require

(struct permission-policy
  (mode          ; 'strict | 'normal | 'yolo | 'auto（作用域自动批准）
   always-set    ; mutable hash: tool-name -> #t（用户答过 always 的工具）
   store-path    ; (or/c #f path-string) — always 记忆的持久化 .rktd
   workdir       ; string — 作用域根（'auto 模式判定读写是否在项目内）
  ) ; end fields
) ; end struct permission-policy

(define (make-policy cfg #:store-path [store-path #f])
  (define always (make-hash))
  ;; 从 .rktd 恢复 always 记忆：文件是 (always <tool-name>) datum 流
  (when (and store-path (file-exists? store-path))
    (datum-log-fold store-path
      (lambda (d _acc)
        (when (and (pair? d) (eq? (car d) 'always) (string? (cadr d)))
          (hash-set! always (cadr d) #t)
        ) ; end when
        (void)
      ) ; end lambda
      (void)
    ) ; end datum-log-fold
  ) ; end when
  (permission-policy (config-permission-mode cfg) always store-path (config-workdir cfg))
) ; end define make-policy

;; 决策矩阵：需要询问的组合返回 'ask，否则 'allow
(define (matrix-decision mode level)
  (case mode
    [(yolo) 'allow]
    [(strict)
     (case level
       [(read-only) 'allow]
       [else 'ask]
     ) ; end case level
    ] ; end strict case
    [else                                 ; normal
     (case level
       [(dangerous) 'ask]
       [else 'allow]
     ) ; end case level
    ] ; end normal case
  ) ; end case mode
) ; end define matrix-decision

;; ------------------------------------------------ 作用域自动批准（'auto 模式）
;; 无人值守长跑用：项目内读写自动放行,越界/网络/破坏性操作走 asker（交互式征询,
;; 无头则 asker 返回 no → 拒绝）。best-effort 防线,非沙箱——bash 不透明,只按启发式拦明显危险。

;; 路径是否在 workdir 之内（纯字符串归一,不触碰文件系统/符号链接）。
(define (path-in-workdir? p workdir)
  (and (string? p) (string? workdir)
       (with-handlers ([exn:fail? (lambda (_e) #f)])
         (define wd (simplify-path (path->complete-path workdir) #f))
         (define full (simplify-path (path->complete-path p wd) #f))
         (define ws (path->string wd))
         (define fs (path->string full))
         (define ws/ (if (string-suffix? ws "/") ws (string-append ws "/")))
         (or (string=? fs ws) (string-prefix? fs ws/)))))

;; git 子命令：网络类（push/pull/fetch/clone/remote/submodule）→ ask；其余本地仓库操作 → allow。
(define GIT-NETWORK-SUBCMDS '("push" "pull" "fetch" "clone" "remote" "submodule"))
(define (git-scope-decision input)
  (define args (let ([a (input-ref input 'args)]) (and (list? a) (filter string? a))))
  (cond
    [(not (pair? args)) 'ask]
    [(member (car args) GIT-NETWORK-SUBCMDS) 'ask]
    [else 'allow]))

;; bash 命令启发式：命中网络出口或破坏性/提权模式 → ask；否则视作项目内构建/测试 → allow。
(define BASH-NETWORK-RX
  #px"(?i:\\b(curl|wget|nc|ncat|netcat|telnet|ssh|scp|sftp|ftp)\\b|rsync\\b.*::|git\\s+(push|pull|fetch|clone)|\\b(pip[0-9]*|pipx)\\s+install|\\bnpm\\s+(install|i|publish|ci)\\b|\\b(yarn|pnpm)\\s+(add|install)\\b|\\bgem\\s+install|\\bcargo\\s+(install|publish)|\\bgo\\s+get\\b|\\bbrew\\s+(install|upgrade)|\\bapt(-get)?\\s+(install|update|upgrade)|\\b(apk|dnf|yum)\\s+(add|install))")
(define BASH-DANGER-RX
  #px"(?i:\\brm\\s+-[a-z]*[rf]|\\bsudo\\b|\\bchmod\\s+-R|\\bchown\\s+-R|\\bmkfs|\\bdd\\s+if=|:\\(\\)\\s*\\{|>\\s*/dev/|\\bshutdown\\b|\\breboot\\b|\\bkillall\\b|\\blaunchctl\\b|/etc/|~/\\.ssh|\\bgit\\s+.*--hard\\b)")
(define (bash-scope-decision cmd)
  (cond
    [(not (string? cmd)) 'ask]
    [(regexp-match? BASH-NETWORK-RX cmd) 'ask]
    [(regexp-match? BASH-DANGER-RX cmd) 'ask]
    [else 'allow]))

;; 'auto 决策：read-only 放行；write/edit 看路径是否在 workdir；git/bash 走各自启发式；
;; 其它 mutating/dangerous（如插件工具）保守 ask。
(define (scoped-decision name level input workdir)
  (cond
    [(eq? level 'read-only) 'allow]
    [(member name '("write_file" "edit_file"))
     (if (path-in-workdir? (input-str input 'path) workdir) 'allow 'ask)]
    [(string=? name "git")  (git-scope-decision input)]
    [(string=? name "bash") (bash-scope-decision (input-str input 'command))]
    [else 'ask]
  ) ; end cond
) ; end define scoped-decision

;; 主入口：返回 'allow | 'deny | (cons 'deny reason)
;; asker : (-> string decision)，decision ∈ 'yes | 'always | 'no | (cons 'no reason-string)
;;   —— 阻塞式询问用户。(cons 'no reason) 表示拒绝并附带给 agent 的理由。
(define (permission-check policy t input asker)
  (define name (tool-name t))
  (define level (tool-permission-level t))
  (define mode (permission-policy-mode policy))
  ;; 'auto 走作用域判定；其余走静态矩阵。两者都可能得 'allow 或 'ask。
  (define base
    (if (eq? mode 'auto)
        (scoped-decision name level input (permission-policy-workdir policy))
        (matrix-decision mode level)))
  (cond
    [(eq? base 'allow) 'allow]
    [(hash-ref (permission-policy-always-set policy) name #f) 'allow]
    [else
     (define answer
       (asker f"allow tool `{name}` ({level}) with input {input}?")
     ) ; end define answer
     (cond
       [(eq? answer 'yes) 'allow]
       [(eq? answer 'always)
        (hash-set! (permission-policy-always-set policy) name #t)
        (define sp (permission-policy-store-path policy))
        (when sp
          (define lg (datum-log-open! sp))
          (datum-log-append! lg (list 'always name))
          (datum-log-close! lg)
        ) ; end when
        'allow
       ] ; end always case
       [else                                    ; 'no 或 (cons 'no reason)
        (define reason (and (pair? answer) (cdr answer)))
        (if (and (string? reason) (non-empty-string? (string-trim reason)))
            (cons 'deny (string-trim reason))
            'deny)
       ] ; end deny case
     ) ; end cond
    ] ; end else
  ) ; end cond
) ; end define permission-check

(provide
 permission-policy?
 permission-policy-mode
 permission-policy-workdir
 make-policy
 permission-check
 ;; 作用域自动批准（'auto）— 导出供单测/复用
 scoped-decision path-in-workdir? bash-scope-decision git-scope-decision
) ; end provide
