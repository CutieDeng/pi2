#lang racket/base
;; tests/gui/memory-store-test.rkt — 记忆库（design-memory.md M2）：只追加记录重放 = 当前状态；增 / 改 / 置顶 / 删 / 恢复 / 确认 / 拒绝；
;; 重开一致、id 续号；快照往返；推给内核的视图（作用域过滤、最近改过的在前、不带钟点）；/remember 解析

(require rackunit racket/file racket/list "../../src/gui/compute/memory-store.rkt")

(define dir (make-temporary-file "pi2-memstore-~a" 'directory))
(define path (build-path dir "memory.rktd"))

(test-case "ops replay to the same state after reopening; ids continue; view order = most recently changed first"
  (define m (memory-open path))
  (define a (memory-add! m "global" "  reply in Chinese  "))
  (define b (memory-add! m "w1" "run make test" #:session "local\u0001/d/s.rktd"))
  (define c (memory-add! m "global" "use tabs" #:source "model" #:state "pending"))
  (check-equal? (list a b c) '("m1" "m2" "m3"))
  (check-equal? (hash-ref (memory-entry m a) 'text) "reply in Chinese")
  (check-equal? (map (lambda (e) (hash-ref e 'id)) (memory-pending m)) '("m3"))
  (memory-edit! m a "reply in English")
  (memory-pin! m b #t)
  (check-equal? (map (lambda (e) (hash-ref e 'id)) (memory-entries m)) '("m2" "m1"))   ; 置顶也算一次改动
  (memory-accept! m c)
  (memory-delete! m b)
  (check-equal? (map (lambda (e) (hash-ref e 'id)) (memory-entries m)) '("m3" "m1"))
  (memory-restore! m b)
  (check-equal? (hash-ref (memory-entry m b) 'state) "active")
  (check-true (hash-ref (memory-entry m b) 'pinned))
  (define m2 (memory-open path))
  (check-equal? (map (lambda (e) (list (hash-ref e 'id) (hash-ref e 'text) (hash-ref e 'state) (hash-ref e 'pinned)))
                     (memory-entries m2))
                (map (lambda (e) (list (hash-ref e 'id) (hash-ref e 'text) (hash-ref e 'state) (hash-ref e 'pinned)))
                     (memory-entries m)))
  (check-equal? (memory-add! m2 "w1" "x") "m4")
  (memory-reject! m2 "m4")
  (check-false (member "m4" (map (lambda (e) (hash-ref e 'id)) (memory-entries m2))))
  ;; 推给内核：只要这些作用域；没有钟点
  (define v (memory-view-entries m2 '("w1")))
  (check-equal? (map (lambda (e) (hash-ref e 'id)) v) '("m2"))
  (check-equal? (sort (hash-keys (car v)) symbol<?) '(id pinned scope text)))

(test-case "snapshot → in-memory store with the same state; writes after it stay in memory"
  (define m (memory-open path))
  (define s (memory-from-snapshot (memory-snapshot m)))
  (check-equal? (map (lambda (e) (hash-ref e 'id)) (memory-entries s)) (map (lambda (e) (hash-ref e 'id)) (memory-entries m)))
  (define before (file-size path))
  (check-equal? (memory-add! s "global" "only in memory") "m5")
  (check-equal? (file-size path) before)
  (check-equal? (memory-entries (memory-from-snapshot #f)) '()))

(test-case "a truncated last line keeps everything before it"
  (with-output-to-file path #:exists 'append (lambda () (write-string "#hasheq((op . \"add\") (id . \"m9")))
  (check-equal? (length (memory-entries (memory-open path))) 3))

(test-case "/remember parsing: optional scope word (English / Chinese), default workspace"
  (define (p s) (call-with-values (lambda () (parse-remember s)) list))
  (check-equal? (p "/remember run make test") '("workspace" "run make test"))
  (check-equal? (p "/remember global: answer in Chinese") '("global" "answer in Chinese"))
  (check-equal? (p "/remember project prices in cents") '("project" "prices in cents"))
  (check-equal? (p "/remember 全局：用中文回答") '("global" "用中文回答"))
  (check-equal? (p "/remember") '("workspace" ""))
  (check-true (remember-command? "/remember x"))
  (check-true (remember-command? "/remember"))
  (check-false (remember-command? "/remembering x"))
  (check-false (remember-command? "please /remember x")))

(test-case "M3: used → last-used; processed per session; dormant = unpinned and unused for 30 days (pinned exempt); view marks dormant; snapshot keeps processed"
  (define p2 (build-path dir "m3.rktd"))
  (define m (memory-open p2))
  (define a (memory-add! m "global" "old fact" #:source "auto" #:evidence "quote"))
  (define b (memory-add! m "global" "pinned old"))
  (memory-pin! m b #t)
  (check-equal? (hash-ref (memory-entry m a) 'evidence) "quote")
  (define later (+ (current-seconds) (* 40 86400)))
  (check-true (memory-dormant? (memory-entry m a) later))
  (check-false (memory-dormant? (memory-entry m b) later))
  (check-false (memory-dormant? (memory-entry m a) (current-seconds)))
  (check-equal? (map (lambda (v) (hash-ref v 'dormant #f)) (memory-view-entries m '("global") #:now later)) '(#f #t))   ; b（置顶、最近改过）在前
  (memory-used! m (list a) "injected")
  (check-true (string? (hash-ref (memory-entry m a) 'last-used #f)))
  (memory-processed! m "local\u0001/d/s.rktd" 8)
  (define m2 (memory-open p2))
  (check-equal? (hash-ref (memory-processed m2) "local\u0001/d/s.rktd") 8)
  (check-equal? (hash-ref (memory-processed (memory-from-snapshot (memory-snapshot m2))) "local\u0001/d/s.rktd") 8))

(delete-directory/files dir)
