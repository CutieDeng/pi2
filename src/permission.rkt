#lang tstring racket
;; permission.rkt — 权限门控（design.md §4.7）
;; 三档模式 × 工具权限级决策矩阵 + always 记忆（经 rktd 持久化）。

(require
 racket/string
 (file "model.rkt")
 (file "tool.rkt")
 (file "rktd.rkt")
 (file "cmdscan.rkt")                   ; bash 命令静态判级（'read-only 模式用）
) ; end require

(struct permission-policy
  (mode          ; 'strict | 'normal | 'yolo | 'auto（作用域自动批准）| 'read-only
   always-set    ; mutable hash: tool-name -> #t（用户答过 always 的工具）
   store-path    ; (or/c #f path-string) — always 记忆的持久化 .rktd
   workdir       ; string — 作用域根（'auto 模式判定读写是否在项目内）
   write-allow   ; (listof string) — 'read-only 模式下**例外允许写**的路径 glob（按 basename 匹配）
  ) ; end fields
) ; end struct permission-policy

(define (make-policy cfg #:store-path [store-path #f] #:write-allow [write-allow '()])
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
  (permission-policy (config-permission-mode cfg) always store-path (config-workdir cfg) write-allow)
) ; end define make-policy

;; 派生一个换了作用域根 workdir 的策略副本（并行 worker 各在自己 worktree，需各自的作用域 +
;; 独立 always-set，避免并发 hash 竞态）。P4.2 用。
(define (policy-with-workdir p dir)
  (permission-policy (permission-policy-mode p) (make-hash) (permission-policy-store-path p) dir
                     (permission-policy-write-allow p)))

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

;; ------------------------------------------------ 只读评审（'read-only 模式）
;; 审查/分析型会话：不询问、不放行任何写。read-only 级工具直通；bash 经
;; cmdscan 静态判级（默认拒绝白名单 + 脚本递归展开——框架据此拿到实际要执行
;; 的内容，间接禁止了不可检查的「真实 bash」）；git 走只读子命令白名单；
;; str_replace_editor 仅 view 子命令。其余 mutating/dangerous 一律拒绝并附因。
;; 返回 'allow | (cons 'deny reason)。

;; 结构化拒绝解释：告诉模型「谁越界、命中哪条规则、树里哪一层、怎么改」——
;; 让 agent 能读懂判决并自我修正，而非对着一句话反复重试。
(define (read-only-deny #:tool tool #:verdict verdict #:rule rule #:fix fix)
  (cons 'deny
        (string-append
         "DENIED (read-only session).\n"
         f"  tool: {tool}\n"
         f"  why: {verdict}\n"
         f"  rule: {rule}\n"
         f"  fix: {fix}\n"
         "  Do not retry this call unchanged.")))

;; 调试（PI_CMDSCAN_DEBUG 置位）：把完整判决因果链 + 检查过的脚本打到 stderr，
;; 供开发者排查「为什么这条被判越界」。不影响返回值。
(define (maybe-debug! cmd v)
  (when (getenv "PI_CMDSCAN_DEBUG")
    (eprintf "[cmdscan] command: ~a\n~a" cmd (verdict->debug v)))
) ; end define maybe-debug!

;; glob → 正则（* → .*，? → .，其余转义）。仅按**文件名**（basename）匹配。
(define (glob->rx g)
  (regexp
   (string-append
    "^"
    (apply string-append
           (for/list ([ch (in-string g)])
             (case ch
               [(#\*) ".*"] [(#\?) "."]
               [(#\. #\+ #\( #\) #\[ #\] #\{ #\} #\^ #\$ #\\ #\|)
                (string #\\ ch)]
               [else (string ch)])))
    "$")))

;; 目标路径是否在 write-allow 名单内：在 workdir 之内 且 basename 命中任一 glob。
(define (write-allowed? path workdir allow)
  (and (string? path) (pair? allow)
       (path-in-workdir? path workdir)
       (let-values ([(_d base _dir?) (split-path (string->path path))])
         (and (path? base)
              (let ([bn (path->string base)])
                (for/or ([g (in-list allow)]) (regexp-match? (glob->rx g) bn)))))))

;; 写工具（write_file/edit_file/editor create|str_replace|insert）在 read-only 下的判定：
;; 命中 write-allow → 允许（作用域写）；否则拒绝并解释。
(define (scoped-write-decision tool path workdir allow)
  (if (write-allowed? path workdir allow)
      'allow
      (read-only-deny
       #:tool tool
       #:verdict f"write target `{(or path "?")}` is outside the write-allow list"
       #:rule (if (null? allow)
                  "read-only mode denies all file writes."
                  f"read-only mode permits writes only to: {(string-join allow ", ")} (within the workdir).")
       #:fix (if (null? allow)
                 "do not write files."
                 f"write only to an allowed path (e.g. {(car allow)}); leave other files untouched."))))

(define (read-only-decision name level input workdir [write-allow '()])
  (cond
    [(eq? level 'read-only) 'allow]
    ;; 作用域写例外：命中 write-allow 的路径放行（bash 仍走 cmdscan，不吃此例外）
    [(member name '("write_file" "edit_file"))
     (scoped-write-decision name (input-str input 'path) workdir write-allow)]
    [(and (string=? name "str_replace_editor")
          (member (input-str input 'command) '("create" "str_replace" "insert")))
     (scoped-write-decision name (input-str input 'path) workdir write-allow)]
    [(string=? name "bash")
     (define cmd (input-str input 'command))
     (define v (classify-command cmd #:workdir workdir))
     (maybe-debug! cmd v)
     (cond
       [(eq? (cmd-verdict-level v) 'read-only) 'allow]
       [else
        (define lv (cmd-verdict-level v))
        (read-only-deny
         #:tool "bash"
         #:verdict (or (cmd-verdict-reason v) "not provably read-only")
         #:rule (if (eq? lv 'mutating)
                    "command has a write effect (mutating). read-only mode allows only provably read-only commands."
                    "command is not statically provable as read-only (opaque: e.g. dynamic name, interpreter, unmodeled construct); rejected conservatively.")
         #:fix (if (eq? lv 'mutating)
                   "use a read-only alternative (grep/cat/ls/git diff/…); remove writes, file redirects, and in-place edits."
                   "rewrite so every command is a plain read-only invocation the framework can inspect (avoid eval/xargs/interpreters and dynamic command names)."))])]
    [(string=? name "git")
     (define args (let ([a (input-ref input 'args)]) (if (list? a) a '())))
     (if (git-args-read-only? args)
         'allow
         (read-only-deny
          #:tool "git"
          #:verdict f"git subcommand `{(let ([a (input-ref input 'args)]) (if (and (list? a) (pair? a)) (car a) "?"))}` is not read-only"
          #:rule "read-only mode allows only read-only git subcommands."
          #:fix "use status/log/diff/show/blame/ls-files/rev-parse/…; avoid commit/checkout/push/stash/reset."))]
    [(string=? name "str_replace_editor")
     (if (equal? (input-str input 'command) "view")
         'allow
         (read-only-deny
          #:tool "str_replace_editor"
          #:verdict f"editor command `{(input-str input 'command)}` mutates files"
          #:rule "read-only mode allows only the editor `view` command."
          #:fix "use command=view to read; do not create/str_replace/insert."))]
    [else
     (read-only-deny
      #:tool name
      #:verdict f"tool `{name}` has permission level {level}"
      #:rule "read-only mode permits only read-only tools."
      #:fix "accomplish the task with read-only tools (read_file/grep/glob/bash read-only/git read-only).")]
  ) ; end cond
) ; end define read-only-decision

;; 主入口：返回 'allow | 'deny | (cons 'deny reason)
;; asker : (-> string decision)，decision ∈ 'yes | 'always | 'no | (cons 'no reason-string)
;;   —— 阻塞式询问用户。(cons 'no reason) 表示拒绝并附带给 agent 的理由。
(define (permission-check policy t input asker)
  (define name (tool-name t))
  (define level (tool-permission-level t))
  (define mode (permission-policy-mode policy))
  ;; 'read-only 直接判 allow/deny（不询问）；'auto 走作用域判定；其余走静态矩阵。
  (define base
    (cond
      [(eq? mode 'read-only)
       (read-only-decision name level input (permission-policy-workdir policy)
                           (permission-policy-write-allow policy))]
      [(eq? mode 'auto)
       (scoped-decision name level input (permission-policy-workdir policy))]
      [else (matrix-decision mode level)]))
  (cond
    [(and (pair? base) (eq? (car base) 'deny)) base]
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
 make-policy policy-with-workdir
 permission-check
 ;; 作用域自动批准（'auto）— 导出供单测/复用
 scoped-decision path-in-workdir? bash-scope-decision git-scope-decision
 read-only-decision
) ; end provide
