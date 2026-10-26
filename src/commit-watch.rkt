#lang racket/base
;; commit-watch.rkt — bash 里直接 `git commit` 的检查（用户 2026-10-26：自动化提交必须写元数据；选「事后提示」）。
;;   模型在 bash 里提交不经过 git 工具，消息里没有 (pi2 agent …)。bash 工具跑命令前记下工作目录仓库的 HEAD，
;;   跑完 HEAD 变了 → 列出新提交，没带 pi2 标签 / trailer 的在工具结果末尾提示模型改用 git 工具或 amend。
;;   只提示、不改提交。只看工作目录所在的仓库（命令 cd 去别的仓库提交的看不到）。

(require racket/string racket/list
         (file "tool.rkt")
         (only-in (file "worktree.rkt") run-git-in)
         (only-in (file "commit-datum.rkt") parse-commit-message has-pi2-tag?))

(define (head-of dir)
  (with-handlers ([exn:fail? (lambda (_e) #f)])
    (define-values (c o) (run-git-in dir "rev-parse" "-q" "--verify" "HEAD"))
    (and (eqv? c 0) (let ([h (string-trim o)]) (and (regexp-match? #px"^[0-9a-f]{40,64}$" h) h)))))

;; before：命令前的 HEAD（#f = 当时还没有提交 / 不是仓库）→ 新提交里没带 pi2 标签的：(listof (cons 短 hash 标题))
(define (untagged-new-commits dir before)
  (define after (head-of dir))
  (cond
    [(or (not after) (equal? before after)) '()]
    [else
     (define-values (c o) (run-git-in dir "log" "-n20" "--format=%H%x1f%B%x1e" (if before (string-append before ".." after) after)))
     (if (not (eqv? c 0))
         '()
         (for*/list ([rec (in-list (string-split o "\x1e"))]
                     #:when (regexp-match? #rx"\x1f" rec)
                     [parts (in-value (string-split rec "\x1f" #:trim? #f))]
                     [msg (in-value (if (> (length parts) 1) (cadr parts) ""))]
                     #:unless (let-values ([(_s _t _ti meta _d) (parse-commit-message msg)]) (has-pi2-tag? meta)))
           (define-values (_s _t title _m _d) (parse-commit-message msg))
           (cons (substring (string-trim (car parts)) 0 7) title)))]))

(define (watch-note commits)
  (format "\n\n[pi2] This command created ~a commit(s) without pi2 automation metadata: ~a. Automated commits must carry it: commit with the git tool (it adds (pi2 agent …) to the message), or fix the latest one with the git tool: [\"commit\", \"--amend\", \"-m\", <the same message>]."
          (length commits)
          (string-join (for/list ([c (in-list commits)]) (format "~a ~s" (car c) (cdr c))) ", ")))

;; 包住 bash 工具的一次运行：thunk → tool-outcome；有没带标签的新提交 → 结果末尾加提示
(define (watch-commits dir thunk)
  (define before (head-of dir))
  (define r (thunk))
  (define bad (with-handlers ([exn:fail? (lambda (_e) '())]) (untagged-new-commits dir before)))
  (if (and (tool-outcome? r) (pair? bad))
      (struct-copy tool-outcome r [content (string-append (tool-outcome-content r) (watch-note bad))])
      r))

(provide watch-commits untagged-new-commits head-of)
