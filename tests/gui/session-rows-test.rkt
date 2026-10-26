#lang racket/base
;; tests/gui/session-rows-test.rkt — 侧栏的行（design-workspace.md W2）：置顶在前、归档收进分组行、当前 / 临时会话不藏；多主机按块

(require rackunit racket/list racket/date "../../src/gui/compute/session-rows.rkt")

(define (row p . kv) (apply hasheq 'path p kv))
(define (keys rs) (for/list ([r rs]) (if (equal? (hash-ref r 'kind #f) "group") (list 'group (hash-ref r 'count) (hash-ref r 'open)) (hash-ref r 'path (hash-ref r 'key #f)))))
(define (arrange rows attrs [open '()])
  (arrange-session-rows rows #:attr (lambda (r k) (member k (hash-ref attrs (hash-ref r 'path #f) '())))
                        #:open? (lambda (g d) (if (member g open) #t d)) #:archived-label (lambda (n) (format "Archived (~a)" n))))

(test-case "single host: pinned first (stable), archived into a closed group at the end; current and ephemeral stay visible"
  (define rows (list (row "a") (row "b") (row "c" 'current #t) (row "d") (row "e" 'ephemeral #t) (row "f")))
  (define attrs (hash "b" '(archived) "c" '(archived) "d" '(pinned) "f" '(pinned archived) "e" '(archived)))
  (define rs (arrange rows attrs))
  (check-equal? (keys rs) '("d" "a" "c" "e" (group 2 #f)))
  (check-equal? (hash-ref (findf (lambda (r) (equal? (hash-ref r 'path #f) "d")) rs) 'pinned) #t)
  (check-equal? (hash-ref (findf (lambda (r) (equal? (hash-ref r 'path #f) "c")) rs) 'archived) #t)
  (check-equal? (hash-ref (last rs) 'label) "Archived (2)")
  ;; 展开：归档的跟在分组行后面（置顶 + 归档的 f 在归档区里）
  (check-equal? (keys (arrange rows attrs (list "\u0002archived\u0001"))) '("d" "a" "c" "e" (group 2 #t) "b" "f")))

(test-case "multi host: each host block arranged on its own; group keys carry the host"
  (define rows (list (hasheq 'kind "header" 'key "\u0001local" 'host "local") (row "l1") (row "l2")
                     (hasheq 'kind "header" 'key "\u0001srv" 'host "srv") (row "s1") (row "s2")))
  (define attrs (hash "l1" '(archived) "s2" '(pinned)))
  (check-equal? (keys (arrange rows attrs)) '("\u0001local" "l2" (group 1 #f) "\u0001srv" "s2" "s1"))
  (define g (findf (lambda (r) (equal? (hash-ref r 'kind #f) "group")) (arrange rows attrs)))
  (check-equal? (hash-ref g 'key) "\u0002archived\u0001local"))

(test-case "nothing pinned or archived: rows unchanged"
  (define rows (list (row "a") (row "b")))
  (check-equal? (arrange rows (hash)) rows))

(test-case "time sections (§11.4 B): pinned / today / yesterday / week / recent / older (folded); a folded section keeps only the current row"
  (define now (find-seconds 0 0 12 25 10 2026))                                     ; 2026-10-25 12:00 本地
  (define (ago days [h 0]) (- now (* days 86400) (* h 3600)))
  (define rows (list (row "t1" 'mtime (ago 0 1)) (row "y1" 'mtime (ago 1)) (row "w1" 'mtime (ago 3)) (row "r1" 'mtime (ago 10))
                     (row "o1" 'mtime (ago 40)) (row "o2" 'mtime (ago 90) 'current #t) (row "o3" 'mtime (ago 100)) (row "p1" 'mtime (ago 50))))
  (define (arr open) (arrange-session-rows rows #:attr (lambda (r k) (and (eq? k 'pinned) (equal? (hash-ref r 'path) "p1")))
                                            #:open? (lambda (g d) (if (member g open) (not d) d)) #:archived-label (lambda (n) "A")
                                            #:now now #:older-days 14 #:section-label (lambda (sec n) (format "~a ~a" sec n))))
  (define (shape rs) (for/list ([r rs]) (if (equal? (hash-ref r 'kind #f) "group") (list (hash-ref r 'label) (hash-ref r 'open)) (hash-ref r 'path))))
  ;; 更早收起：只留当前会话（o2）
  (check-equal? (shape (arr '())) '(("pinned 1" #t) "p1" ("today 1" #t) "t1" ("yesterday 1" #t) "y1" ("week 1" #t) "w1" ("recent 1" #t) "r1"
                                    ("older 3" #f) "o2"))
  ;; 当前会话不在更早：更早缺省收起；展开状态由调用方给
  (define rows2 (map (lambda (r) (hash-remove r 'current)) rows))
  (define (arr2 open) (arrange-session-rows rows2 #:attr (lambda (r k) #f) #:open? (lambda (g d) (if (member g open) (not d) d)) #:archived-label (lambda (n) "A")
                                             #:now now #:older-days 14 #:section-label (lambda (sec n) (format "~a ~a" sec n))))
  (check-equal? (last (shape (arr2 '()))) '("older 4" #f))
  (check-equal? (take-right (shape (arr2 (list "\u0002sec\u0001\u0001older"))) 5) '(("older 4" #t) "o1" "o2" "o3" "p1")))

(test-case "group mode (W3): a group per project, a session in two projects shows twice, ungrouped last, pinned first, folding keeps the current row"
  (define rows (list (row "a") (row "b" 'current #t) (row "c") (row "d") (row "e")))
  (define groups (hash "a" '(("p2" . "Beta")) "b" '(("p1" . "alpha") ("p2" . "Beta")) "c" '()))
  (define (arr open)
    (arrange-session-rows rows #:attr (lambda (r k) (and (eq? k 'pinned) (equal? (hash-ref r 'path) "e")))
                          #:open? (lambda (g d) (if (member g open) (not d) d)) #:archived-label (lambda (n) "A")
                          #:groups-of (lambda (r) (hash-ref groups (hash-ref r 'path) '())) #:ungrouped-label (lambda (n) (format "none ~a" n))))
  (define (shape rs) (for/list ([r rs]) (if (equal? (hash-ref r 'kind #f) "group") (hash-ref r 'label) (hash-ref r 'path))))
  (check-equal? (shape (arr '())) '("e" "alpha（1）" "b" "Beta（2）" "a" "b" "none 2" "c" "d"))
  (check-equal? (shape (arr (list "\u0002grp\u0001\u0001p2"))) '("e" "alpha（1）" "b" "Beta（2）" "b" "none 2" "c" "d")))
