#lang tstring racket
;; worktree.rkt — git worktree 生命周期（Goal 模式 P4：并行 worker 的文件隔离基础）。
;; 每个 worker 在自己的 worktree（共享 .git、独立工作目录 + 分支）里干活，看不到彼此未提交改动 →
;; 无文件竞态；完成后把分支 merge 回主分支。**未做隔离前绝不在共享 workdir 上并行写**。
;; 见 design-goalmode-p4.md。本模块只是纯 git 原语；调度/合并策略在 goal.rkt。

(require racket/string racket/port)

(define GIT-EXE (or (find-executable-path "git") "/usr/bin/git"))
(define GIT-TIMEOUT-SECS 120)

;; 在 dir 里跑 git（argv 直传，不过 shell）→ (values exit-code|'timeout combined-output)。
(define (run-git-in dir . args)
  (parameterize ([current-directory dir])
    (define-values (proc out in _err) (apply subprocess #f #f 'stdout GIT-EXE args))
    (close-output-port in)
    (define buf (open-output-string))
    (define pump (thread (lambda () (copy-port out buf))))
    (define done (sync/timeout GIT-TIMEOUT-SECS proc))
    (cond
      [(not done) (subprocess-kill proc #t) (thread-wait pump) (values 'timeout (get-output-string buf))]
      [else (thread-wait pump) (close-input-port out) (values (subprocess-status proc) (get-output-string buf))])
  ) ; end parameterize
) ; end define run-git-in

(define (git-ok? dir . args)
  (define-values (c _o) (apply run-git-in dir args)) (eqv? c 0))

;; dir 是否 git 仓库工作树。
(define (git-repo? dir) (git-ok? dir "rev-parse" "--git-dir"))

;; 当前短 HEAD（#f 若非仓库/无提交）。
(define (git-head dir)
  (define-values (c o) (run-git-in dir "rev-parse" "--short" "HEAD"))
  (if (eqv? c 0) (string-trim o) #f))

;; 新建 worktree：repo-dir 里 `git worktree add -b <branch> <wt-dir> <base>`。返回 #t/#f。
;; wt-dir 应**尚不存在**（git 创建它）；放 repo 外(系统临时区)避免嵌套/污染主树。
(define (worktree-create! repo-dir branch wt-dir [base "HEAD"])
  (git-ok? repo-dir "worktree" "add" "-b" branch wt-dir base))

;; 移除 worktree（+ 删分支 + prune）。best-effort，不抛。
(define (worktree-remove! repo-dir wt-dir [branch #f])
  (run-git-in repo-dir "worktree" "remove" "--force" wt-dir)
  (when branch (run-git-in repo-dir "branch" "-D" branch))
  (run-git-in repo-dir "worktree" "prune")
  (void))

;; 提交 dir 里的全部改动。有改动并提交成功 → #t；无改动 → #f（commit 非零）。
(define (git-commit-all! dir msg)
  (run-git-in dir "add" "-A")
  (define-values (c _o) (run-git-in dir "commit" "-m" msg))
  (eqv? c 0))

;; 把 branch merge 回 repo-dir 当前分支。→ 'ok | 'conflict（已 abort） | 'error。
(define (worktree-merge! repo-dir branch [msg #f])
  (define-values (c o)
    (apply run-git-in repo-dir
           (append (list "merge" "--no-ff") (if msg (list "-m" msg) '()) (list branch))))
  (cond
    [(eqv? c 0) 'ok]
    [(regexp-match? #rx"(?i:conflict)" o) (run-git-in repo-dir "merge" "--abort") 'conflict]
    [else 'error]))

;; 撤销最近一次 merge commit（--no-ff 保证有 merge commit，HEAD~1=merge 前）。全局验收破坏时用。
(define (revert-last-merge! repo-dir)
  (git-ok? repo-dir "reset" "--hard" "HEAD~1"))

(provide
 run-git-in git-ok? git-repo? git-head
 worktree-create! worktree-remove! worktree-merge! git-commit-all! revert-last-merge!)
