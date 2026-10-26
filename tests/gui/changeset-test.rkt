#lang racket/base
;; tests/gui/changeset-test.rkt — 批量变更清单（design-workspace.md §11.7 C1）：校验（ok / noop / stale / invalid，本清单新建的项目）、
;; 应用所选（目录的变化、发给内核的请求）、记录（审阅 / 应用 / 撤销都在、重开文件与快照一样）、整份撤销、分组

(require rackunit racket/list racket/file "../../src/gui/compute/catalog.rkt" "../../src/gui/compute/changeset.rkt")

(define dir (make-temporary-file "pi2-cs-~a" 'directory))
(define path (build-path dir "catalog.rktd"))
(define S1 (session-id "local" "/d/s1.rktd")) (define S2 (session-id "local" "/d/s2.rktd")) (define S3 (session-id "local" "/d/s3.rktd"))
(define rows (hash S1 (hasheq 'path "/d/s1.rktd" 'title "fix loader" 'current #t)
                   S2 (hasheq 'path "/d/s2.rktd" 'title "gui sidebar" 'current #f)
                   S3 (hasheq 'path "/d/s3.rktd" 'title "old chat" 'current #f)))
(define ITEMS
  (list (hasheq 'op "new-project" 'project "Market" 'reason "two sessions about the market tool")
        (hasheq 'op "link" 'session S2 'project "market" 'reason "about market")                  ; 本清单新建的（大小写不同）
        (hasheq 'op "link" 'session S1 'project "pi2 GUI")
        (hasheq 'op "link" 'session S3 'project "nope")                                          ; 没有这个项目
        (hasheq 'op "archive" 'session S3 'reason "untouched for weeks")
        (hasheq 'op "pin" 'session S2)
        (hasheq 'op "rename" 'session S2 'new_title "GUI: sidebar actions")
        (hasheq 'op "delete" 'session S1)                                                        ; 当前会话
        (hasheq 'op "delete" 'session S3)
        (hasheq 'op "link" 'session (session-id "local" "/d/gone.rktd") 'project "pi2 GUI")      ; 会话没了
        (hasheq 'op "unlink" 'session S2 'project "pi2 GUI")                                     ; 本来就不在
        (hasheq 'op "explode" 'session S2)))

(test-case "validate, apply the accepted items, records, reopen, undo the whole changeset"
  (define c (catalog-open path))
  (define gui (catalog-add-entity! c "project" "pi2 GUI"))
  (define id (catalog-add-changeset! c (hasheq 'by "agent:test" 'source (hasheq 'request "organize last week") 'summary "s" 'items ITEMS)))
  (define v (changeset-validate c ITEMS rows))
  (check-equal? (map (lambda (it) (hash-ref it 'status)) v)
                '("ok" "ok" "ok" "invalid" "ok" "ok" "ok" "invalid" "ok" "stale" "noop" "invalid"))
  (check-equal? (hash-ref (list-ref v 2) 'project_id) gui)
  ;; 应用：除了删除 S3（5、6、0、1、2、4 … 接受；8 不接受）
  (define sent '())
  (define (kernel! host req) (set! sent (cons (list host req) sent)))
  (define res (changeset-apply! c id '(0 1 2 4 5 6 3 7 9 10 11) rows #:kernel kernel!))
  (check-equal? (for/list ([r res]) (list (hash-ref r 'n) (hash-ref r 'status)))
                '((0 "ok") (1 "ok") (2 "ok") (3 "skipped") (4 "ok") (5 "ok") (6 "sent") (7 "skipped") (9 "skipped") (10 "skipped") (11 "skipped")))
  (define market (hash-ref (findf (lambda (e) (equal? (hash-ref e 'name) "Market")) (catalog-entities-of c "project")) 'id))
  (check-not-false (member (cons market "member") (catalog-parents c S2)))
  (check-not-false (member (cons gui "member") (catalog-parents c S1)))
  (check-equal? (catalog-session-attr c S3 'archived) #t)
  (check-equal? (catalog-session-attr c S2 'pinned) #t)
  (check-equal? sent (list (list "local" (hasheq 'type "set_title" 'path "/d/s2.rktd" 'title "GUI: sidebar actions"))))
  (check-equal? (changeset-state (catalog-changeset c id)) 'applied)
  (check-equal? (hash-ref (hash-ref (catalog-changeset c id) 'review) 'rejected) '(8))
  ;; 记录：重开文件、快照，都一样
  (for ([c2 (list (catalog-open path) (catalog-from-snapshot (catalog-snapshot c)))])
    (define cs (catalog-changeset c2 id))
    (check-equal? (changeset-state cs) 'applied)
    (check-equal? (hash-ref (hash-ref cs 'proposal) 'by) "agent:test")
    (check-equal? (length (hash-ref (hash-ref cs 'applied) 'results)) 11)
    (check-equal? (catalog-session-attr c2 S3 'archived) #t))
  ;; 撤销整份（在重开的目录上做：逆操作在记录里）
  (define c3 (catalog-open path))
  (set! sent '())
  (define ur (changeset-undo! c3 id #:kernel kernel!))
  (check-true (andmap (lambda (s) (and (member s '("ok" "sent")) #t)) ur))
  (check-false (member market (map (lambda (e) (hash-ref e 'id)) (catalog-entities-of c3 "project"))))
  (check-false (member (cons gui "member") (catalog-parents c3 S1)))
  (check-equal? (catalog-session-attr c3 S3 'archived) #f)
  (check-equal? (catalog-session-attr c3 S2 'pinned) #f)
  (check-equal? sent (list (list "local" (hasheq 'type "set_title" 'path "/d/s2.rktd" 'title "gui sidebar"))))
  (check-equal? (changeset-state (catalog-changeset c3 id)) 'undone)
  (check-equal? (changeset-undo! c3 id #:kernel kernel!) '())                      ; 撤销过：不再做
  (check-equal? (changeset-state (catalog-changeset (catalog-open path) id)) 'undone))

(test-case "groups for the review panel: new projects, per-project links, pin, archive, rename, delete"
  (define gs (changeset-groups ITEMS))
  (check-equal? (map (lambda (g) (list (first g) (second g) (length (third g)))) gs)
                '((new-project "" 1) (link "market" 1) (link "nope" 1) (link "pi2 GUI" 2) (unlink "pi2 GUI" 1)
                  (pin "" 1) (archive "" 1) (rename "" 1) (delete "" 2) (other "" 1))))

(delete-directory/files dir)
