#lang racket/base
;; changeset.rkt — 会话的批量变更清单（design-workspace.md §11.7 C）：agent（或用户）提议 → 界面校验、审阅 → 应用 → 一步撤销整份。
;; 记录都在目录 catalog 里（catalog-add-changeset! / catalog-changeset-event!），只追加：谁提议、依据、每项前后状态、接受了哪些、结果、逆操作。
;;
;; 一项（hasheq）：'op ∈ OPS；'session 会话 id（主机 \u0001 路径）；'project 项目名（link / unlink / new-project）；'new_title（rename）；'reason 理由。
;; rows：会话 id → 会话行（内核 list_sessions 的行，'title 'current 'path）——校验与应用时的现状。
;; 应用时内核那一侧的操作（改名、删除）经 kernel! 回调发出：(kernel! 主机 请求)。

(require racket/list racket/string "catalog.rkt")

(define OPS '("link" "unlink" "new-project" "rename" "pin" "unpin" "archive" "unarchive" "delete"))
(define SESSION-OPS (remove "new-project" OPS))

(define (j h k [d #f]) (if (hash? h) (hash-ref h k d) d))
(define (sid-host sid) (car (string-split sid "\u0001" #:trim? #f)))
(define (sid-path sid) (let ([p (string-split sid "\u0001" #:trim? #f)]) (if (= (length p) 2) (cadr p) sid)))
(define (norm s) (string-downcase (string-trim s)))

;; 项目名 → id（不分大小写）；同名多个 → 'ambiguous
(define (project-index cat)
  (for/fold ([h (hash)]) ([e (in-list (catalog-entities-of cat "project"))])
    (define k (norm (hash-ref e 'name)))
    (hash-set h k (if (hash-ref h k #f) 'ambiguous (hash-ref e 'id)))))

(define (member-of? cat pid sid) (and (member (cons pid "member") (catalog-parents cat sid)) #t))
(define (in-project? cat pid sid) (for/or ([p (in-list (catalog-parents cat sid))]) (equal? (car p) pid)))

;; → 各项加上 'n（项号）'status（"ok" "noop" "stale" "invalid"）'why；项目名能对上的加 'project_id（新建的在应用时才有）
(define (changeset-validate cat items rows)
  (define pidx (project-index cat))
  (define new-names (for/list ([it (in-list items)] #:when (equal? (j it 'op) "new-project") #:when (string? (j it 'project))) (norm (j it 'project))))
  (for/list ([it (in-list items)] [n (in-naturals)])
    (define op (j it 'op))
    (define sid (j it 'session))
    (define row (and (string? sid) (hash-ref rows sid #f)))
    (define pname (j it 'project))
    (define pid (and (string? pname) (hash-ref pidx (norm pname) #f)))
    (define (out status [why #f] #:pid [p #f]) (let* ([r (hash-set* it 'n n 'status status)] [r (if why (hash-set r 'why why) r)]) (if (string? p) (hash-set r 'project_id p) r)))
    (cond
      [(not (member op OPS)) (out "invalid" (format "unknown op ~s" op))]
      [(and (member op SESSION-OPS) (not row)) (out "stale" "session no longer exists")]
      [else
       (case op
         [("new-project") (cond [(not (and (string? pname) (non-empty-string? (string-trim pname)))) (out "invalid" "missing project name")]
                                [pid (out "noop" "project already exists" #:pid (and (string? pid) pid))]
                                [else (out "ok")])]
         [("link" "unlink")
          (cond [(eq? pid 'ambiguous) (out "invalid" "several projects have this name")]
                [(and (not pid) (equal? op "link") (member (and (string? pname) (norm pname)) new-names)) (out "ok")]   ; 本清单新建的
                [(not pid) (out "invalid" "no such project")]
                [(equal? op "link") (if (in-project? cat pid sid) (out "noop" "already in the project" #:pid pid) (out "ok" #:pid pid))]
                [else (if (in-project? cat pid sid) (out "ok" #:pid pid) (out "noop" "not in the project" #:pid pid))])]
         [("rename") (define nt (j it 'new_title))
                     (cond [(not (and (string? nt) (non-empty-string? (string-trim nt)))) (out "invalid" "missing new title")]
                           [(equal? (string-trim nt) (j row 'title)) (out "noop" "same title")]
                           [else (out "ok")])]
         [("pin" "unpin") (if (eq? (and (catalog-session-attr cat sid 'pinned #f) #t) (equal? op "pin")) (out "noop" "already so") (out "ok"))]
         [("archive" "unarchive") (if (eq? (and (catalog-session-attr cat sid 'archived #f) #t) (equal? op "archive")) (out "noop" "already so") (out "ok"))]
         [("delete") (if (j row 'current #f) (out "invalid" "the current session cannot be deleted") (out "ok"))]
         [else (out "invalid" "unknown op")])])))

;; 应用接受的项（项号表）：新建项目先做，其余按顺序。→ 每项结果（hasheq 'n 'status "ok"|"sent"|"skipped"|"failed" 'detail）；
;; 记审阅与应用两条记录（逆操作存在应用记录里：重启后也能撤销）
(define (changeset-apply! cat id accepted rows #:kernel kernel!)
  (define cs (catalog-changeset cat id))
  (define items (changeset-validate cat (j (hash-ref cs 'proposal) 'items '()) rows))
  (define acc (for/list ([it (in-list items)] #:when (member (hash-ref it 'n) accepted)) it))
  (define ordered (append (filter (lambda (it) (equal? (j it 'op) "new-project")) acc) (filter (lambda (it) (not (equal? (j it 'op) "new-project"))) acc)))
  (define created (make-hash))                                                    ; 本次新建的项目：名 → id
  (define inverses '())
  (define (inv! x) (set! inverses (cons x inverses)))
  (define results
    (catalog-batch!
     cat
     (lambda ()
       (for/list ([it (in-list ordered)])
         (define n (hash-ref it 'n))
         (define sid (j it 'session))
         (define (res status [detail #f]) (hasheq 'n n 'status status 'detail (or detail 'null)))
         (cond
           [(not (equal? (hash-ref it 'status) "ok")) (res "skipped" (or (j it 'why) (hash-ref it 'status)))]
           [else
            (case (j it 'op)
              [("new-project") (define pid (catalog-add-entity! cat "project" (string-trim (j it 'project))))
                               (hash-set! created (norm (j it 'project)) pid)
                               (inv! (hasheq 'op "remove-entity" 'id pid)) (res "ok" pid)]
              [("link") (define pid (or (j it 'project_id) (hash-ref created (norm (j it 'project)) #f)))
                        (cond [pid (catalog-link! cat pid sid "member") (inv! (hasheq 'op "unlink" 'from pid 'to sid)) (res "ok")]
                              [else (res "failed" "no such project")])]
              [("unlink") (define pid (j it 'project_id))
                          (define role (for/first ([p (in-list (catalog-parents cat sid))] #:when (equal? (car p) pid)) (cdr p)))
                          (catalog-unlink! cat pid sid) (inv! (hasheq 'op "link" 'from pid 'to sid 'role (or role "member"))) (res "ok")]
              [("pin" "unpin" "archive" "unarchive")
               (define attr (if (member (j it 'op) '("pin" "unpin")) 'pinned 'archived))
               (define old (catalog-session-attr cat sid attr #f))
               (catalog-set-session! cat sid #:host (sid-host sid) #:path (sid-path sid) #:attrs (hasheq attr (and (member (j it 'op) '("pin" "archive")) #t)))
               (inv! (hasheq 'op "set-attr" 'session sid 'attr (symbol->string attr) 'value old)) (res "ok")]
              [("rename") (define old (j (hash-ref rows sid #f) 'title))
                          (kernel! (sid-host sid) (hasheq 'type "set_title" 'path (sid-path sid) 'title (string-trim (j it 'new_title))))
                          (inv! (hasheq 'op "kernel" 'host (sid-host sid) 'req (hasheq 'type "set_title" 'path (sid-path sid) 'title (if (string? old) old ""))))
                          (res "sent")]
              [("delete") (kernel! (sid-host sid) (hasheq 'type "delete_session" 'path (sid-path sid)))
                          (inv! (hasheq 'op "kernel" 'host (sid-host sid) 'req (hasheq 'type "restore_session" 'path (sid-path sid))))
                          (res "sent")]
              [else (res "failed" "unknown op")])])))))
  (catalog-changeset-event! cat id 'review (hasheq 'accepted accepted 'rejected (for/list ([it (in-list items)] #:unless (member (hash-ref it 'n) accepted)) (hash-ref it 'n))))
  (catalog-changeset-event! cat id 'applied (hasheq 'results results 'inverses inverses))   ; inverses：逆序（最后做的在前）
  results
) ; end define changeset-apply!

;; 撤销整份：逆操作按相反顺序（应用记录里已是逆序）；记一条撤销记录。→ 结果表
(define (changeset-undo! cat id #:kernel kernel!)
  (define cs (catalog-changeset cat id))
  (define applied (and cs (hash-ref cs 'applied)))
  (cond
    [(not applied) '()]
    [(hash-ref cs 'undone) '()]                                                     ; 已经撤销过
    [else
     (define results
       (catalog-batch!
        cat
        (lambda ()
          (for/list ([x (in-list (j applied 'inverses '()))])
            (case (j x 'op)
              [("remove-entity") (catalog-remove-entity! cat (j x 'id)) "ok"]
              [("unlink") (catalog-unlink! cat (j x 'from) (j x 'to)) "ok"]
              [("link") (catalog-link! cat (j x 'from) (j x 'to) (j x 'role "member")) "ok"]
              [("set-attr") (catalog-set-session! cat (j x 'session) #:attrs (hasheq (string->symbol (j x 'attr)) (j x 'value))) "ok"]
              [("kernel") (kernel! (j x 'host) (j x 'req)) "sent"]
              [else "failed"])))))
     (catalog-changeset-event! cat id 'undone (hasheq 'results results))
     results])
) ; end define changeset-undo!

;; 状态：'proposed 'applied 'undone 'discarded（审阅记录带 'discarded：全部放弃）
(define (changeset-state cs)
  (cond [(hash-ref cs 'undone #f) 'undone] [(hash-ref cs 'applied #f) 'applied]
        [(let ([r (hash-ref cs 'review #f)]) (and r (hash-ref r 'discarded #f))) 'discarded]
        [else 'proposed]))

;; 按目标分组（审阅面板用）：→ (listof (list 组键 组名 (listof 项)))；组：每个项目一组（归入 / 移出）、新建项目、置顶、归档、改名、删除
(define (changeset-groups items)
  (define (key it)
    (case (j it 'op)
      [("link") (list 'link (format "~a" (j it 'project)))]
      [("unlink") (list 'unlink (format "~a" (j it 'project)))]
      [("new-project") (list 'new-project "")]
      [("pin" "unpin") (list 'pin "")]
      [("archive" "unarchive") (list 'archive "")]
      [("rename") (list 'rename "")]
      [("delete") (list 'delete "")]
      [else (list 'other "")]))
  (define order '(new-project link unlink pin archive rename delete other))
  (define groups (group-by key items))
  (sort (for/list ([g (in-list groups)]) (let ([k (key (car g))]) (list (car k) (cadr k) g)))
        (lambda (a b) (let ([ia (index-of order (car a))] [ib (index-of order (car b))])
                        (if (= ia ib) (string<? (cadr a) (cadr b)) (< ia ib))))))

(provide OPS changeset-validate changeset-apply! changeset-undo! changeset-state changeset-groups)
