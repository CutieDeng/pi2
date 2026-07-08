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
  (mode          ; 'strict | 'normal | 'yolo
   always-set    ; mutable hash: tool-name -> #t（用户答过 always 的工具）
   store-path    ; (or/c #f path-string) — always 记忆的持久化 .rktd
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
  (permission-policy (config-permission-mode cfg) always store-path)
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

;; 主入口：'allow | 'deny
;; asker : (-> string (or/c 'yes 'no 'always)) — 阻塞式询问用户
(define (permission-check policy t input asker)
  (define name (tool-name t))
  (define level (tool-permission-level t))
  (cond
    [(eq? (matrix-decision (permission-policy-mode policy) level) 'allow) 'allow]
    [(hash-ref (permission-policy-always-set policy) name #f) 'allow]
    [else
     (define answer
       (asker f"allow tool `{name}` ({level}) with input {input}?")
     ) ; end define answer
     (case answer
       [(yes) 'allow]
       [(always)
        (hash-set! (permission-policy-always-set policy) name #t)
        (define sp (permission-policy-store-path policy))
        (when sp
          (define lg (datum-log-open! sp))
          (datum-log-append! lg (list 'always name))
          (datum-log-close! lg)
        ) ; end when
        'allow
       ] ; end always case
       [else 'deny]
     ) ; end case
    ] ; end else
  ) ; end cond
) ; end define permission-check

(provide
 permission-policy?
 permission-policy-mode
 make-policy
 permission-check
) ; end provide
