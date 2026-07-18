#lang tstring racket
;; worktree-test.rkt — git worktree 原语（真机 git，离线）。P4.1 隔离基础。

(require
 rackunit
 racket/file
 (file "../src/worktree.rkt")
) ; end require

(define (git! dir . args) (define-values (c o) (apply run-git-in dir args)) (void))

;; 建一个带一次提交的 git 仓库。
(define repo (make-temporary-file "pi2-wt-repo-~a" 'directory))
(git! repo "init" "-q")
(git! repo "config" "user.email" "t@e.com")
(git! repo "config" "user.name" "t")
(void (call-with-output-file (build-path repo "base.txt") (lambda (o) (write-string "base" o))))
(git! repo "add" "-A")
(git! repo "commit" "-m" "init")

(test-case "git-repo? / git-head"
  (check-true (git-repo? repo))
  (check-true (string? (git-head repo)))
  (define notrepo (make-temporary-file "pi2-notrepo-~a" 'directory))
  (check-false (git-repo? notrepo))
  (delete-directory/files notrepo)
) ; end test-case

(test-case "worktree：create → 在隔离副本提交 → merge 回 main → 文件现身主树 → remove"
  (define wt (make-temporary-file "pi2-wt-~a" 'directory))
  (delete-directory wt)                                  ; git worktree add 要求尚不存在
  (define wtd (path->string wt))
  (check-true (worktree-create! repo "pi/t1" wtd "HEAD"))
  (check-true (directory-exists? wtd))
  ;; 在 worktree 写 + 提交一个文件
  (call-with-output-file (build-path wtd "new.txt") (lambda (o) (write-string "hello" o)))
  (check-true (git-commit-all! wtd "add new.txt"))
  (check-false (file-exists? (build-path repo "new.txt")))   ; 主树还没有(隔离)
  ;; merge 回 main
  (check-equal? (worktree-merge! repo "pi/t1" "merge t1") 'ok)
  (check-true (file-exists? (build-path repo "new.txt")))    ; merge 后现身
  ;; 清理
  (worktree-remove! repo wtd "pi/t1")
  (check-false (directory-exists? wtd))
) ; end test-case

(test-case "git-commit-all!：无改动 → #f"
  (check-false (git-commit-all! repo "nothing changed"))
) ; end test-case

(delete-directory/files repo)
(displayln "worktree-test: all passed")
