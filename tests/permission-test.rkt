#lang tstring racket
;; permission-test.rkt — 权限门控 + 拒绝理由回传（离线）。

(require
 rackunit
 racket/file
 (file "../src/model.rkt")
 (file "../src/tool.rkt")
 (file "../src/event.rkt")
 (file "../src/permission.rkt")
 (file "../src/loop.rkt")
) ; end require

;; 最小 mock 工具
(struct mock-tool (name level)
  #:methods gen:tool
  [(define (tool-name t) (mock-tool-name t))
   (define (tool-spec t) (hasheq))
   (define (tool-permission-level t) (mock-tool-level t))
   (define (tool-run t input ctx) (tool-outcome "ran" #f #f))]
) ; end struct mock-tool

(define (policy mode) (make-policy (struct-copy config (default-config) [permission-mode mode])))
(define danger (mock-tool "danger" 'dangerous))
(define reader (mock-tool "reader" 'read-only))

;; 记录 asker 是否被调用
(define (asker-const v #:seen [seen (box #f)])
  (values (lambda (_q) (set-box! seen #t) v) seen))

;; ---------------------------------------------------------------- 决策矩阵

(test-case "normal: read-only allowed without asking"
  (define-values (a seen) (asker-const 'no))
  (check-eq? (permission-check (policy 'normal) reader (hasheq) a) 'allow)
  (check-false (unbox seen))                     ; 未询问
) ; end test-case

(test-case "yolo: everything allowed without asking"
  (define-values (a seen) (asker-const 'no))
  (check-eq? (permission-check (policy 'yolo) danger (hasheq) a) 'allow)
  (check-false (unbox seen))
) ; end test-case

(test-case "normal: dangerous asks; yes→allow, no→deny"
  (define-values (ay _1) (asker-const 'yes))
  (check-eq? (permission-check (policy 'normal) danger (hasheq) ay) 'allow)
  (define-values (an _2) (asker-const 'no))
  (check-eq? (permission-check (policy 'normal) danger (hasheq) an) 'deny)
) ; end test-case

(test-case "always is remembered within a policy (no second ask)"
  (define p (policy 'normal))
  (define-values (aa seen) (asker-const 'always))
  (check-eq? (permission-check p danger (hasheq) aa) 'allow)
  (define-values (an seen2) (asker-const 'no))
  (check-eq? (permission-check p danger (hasheq) an) 'allow)   ; 记住了
  (check-false (unbox seen2))                                  ; 第二次不再询问
) ; end test-case

;; ---------------------------------------------------------------- 拒绝理由

(test-case "deny with reason → (cons 'deny reason); empty reason → plain 'deny"
  (define-values (ar _1) (asker-const (cons 'no "this could delete files")))
  (check-equal? (permission-check (policy 'normal) danger (hasheq) ar)
                (cons 'deny "this could delete files"))
  (define-values (aw _2) (asker-const (cons 'no "   ")))        ; 空白理由
  (check-eq? (permission-check (policy 'normal) danger (hasheq) aw) 'deny)
) ; end test-case

;; ---------------------------------------------------------------- 持久化

(test-case "always persists to .rktd and restores in a fresh policy"
  (define store (make-temporary-file "pi2-perm-~a.rktd"))
  (delete-file store)                            ; 让 make-policy 视作新文件
  (define cfg (struct-copy config (default-config) [permission-mode 'normal]))
  (define p1 (make-policy cfg #:store-path store))
  (define-values (aa _1) (asker-const 'always))
  (check-eq? (permission-check p1 danger (hasheq) aa) 'allow)
  ;; 新策略从同一 store 恢复 → 直接放行、不询问
  (define p2 (make-policy cfg #:store-path store))
  (define-values (an seen) (asker-const 'no))
  (check-eq? (permission-check p2 danger (hasheq) an) 'allow)
  (check-false (unbox seen))
  (delete-file store)
) ; end test-case

;; ---------------------------------------------------------------- 理由抵达 tool-result

(test-case "denied tool-result carries the user's reason for the model"
  (define d (make-deps #:provider #f
                       #:registry (make-registry (list danger))
                       #:bus (make-bus)
                       #:policy (policy 'normal)
                       #:asker (lambda (_q) (cons 'no "not on production"))))
  (define calls (list (tool-use-block "c1" "danger" (hasheq 'x 1))))
  (define results (execute-calls! calls d))
  (check-equal? (length results) 1)
  (define r (car results))
  (check-true (tool-result-block-is-error? r))
  (check-true (string-contains? (tool-result-block-content r) "not on production"))
  (check-true (string-contains? (tool-result-block-content r) "do not retry"))
) ; end test-case

(displayln "permission-test: all passed")
