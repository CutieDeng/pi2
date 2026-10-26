#lang racket/base
;; catalog.rkt — 会话的组织（design-workspace.md W2）：project / super-workspace / workspace 三种实体 + 多对多的边 + 会话条目。
;; 只放 GUI 这台机器（{config-home}/catalog.rktd）；内核不认识它。
;;
;; 文件 = 只追加的记录（每行一个 hasheq，write / read）；读时按顺序重放成当前状态（实体、边的数量是百 / 千级，全量读）：
;;   (hasheq 'op "entity" 'id 'kind 'name 'attrs 'ts)   新建或整体改写实体（attrs：设置；workspace 有 'host 'dir，聊天工作区 'host 'chat #t）
;;   (hasheq 'op "remove" 'id 'ts)                       删实体（指向它的边一起失效；会话本身不动）
;;   (hasheq 'op "edge" 'from 'to 'role 'ts)            建边：project → super-workspace / workspace / session，super-workspace → workspace，
;;                                                      workspace → session（role "primary" | "visited"）
;;   (hasheq 'op "unedge" 'from 'to 'ts)                解边
;;   (hasheq 'op "session" 'id 'host 'path 'attrs 'ts)  会话条目（attrs：'pinned 'archived …，合并进旧值）
;;   变更清单（design-workspace.md §11.7 C；只追加、可审计，compute/changeset.rkt 用）：
;;   (hasheq 'op "changeset" 'id 'by 'source 'summary 'items 'ts)          提议（agent / 用户）
;;   (hasheq 'op "changeset-review" 'id 'accepted 'rejected 'ts)           审阅（接受 / 拒绝的项号）
;;   (hasheq 'op "changeset-applied" 'id 'results 'inverses 'ts)           应用（每项结果；撤销用的逆操作）
;;   (hasheq 'op "changeset-undone" 'id 'results 'ts)                      撤销
;; 会话 id = "主机\u0001路径"（session-id）。实体 id = 种类首字母 + 序号（p1 s2 w3），由记录推出下一个序号。
;; 边按两个方向建索引（from → to、to → from），工作区按「主机 + 目录」/「主机 + 聊天」建索引：会话列表每次到达都同步，不能全表扫。
;; 不能写文件（回放）时：path #f → 只在内存里；回放从录制时的快照（catalog-snapshot）开始。

(require racket/list racket/string racket/file)

(define KINDS '("project" "super-workspace" "workspace"))

(struct catalog (path [entities #:mutable] [out #:mutable] [in #:mutable] [sessions #:mutable] [next #:mutable] [ws-key #:mutable]
                      [batch #:mutable] [changesets #:mutable]))
;; changesets：id → (hasheq 'proposal 记录 'review 记录或#f 'applied 记录或#f 'undone 记录或#f)
;; entities：id → (hasheq 'id 'kind 'name 'attrs)；out：from → (hash to → role)；in：to → (hash from → role)；
;; sessions：id → (hasheq 'id 'host 'path 'attrs)；ws-key：(cons 主机 目录 | 'chat) → 工作区 id；batch：#f | 待写的记录（逆序）

(define (session-id host path) (string-append host "\u0001" path))

(define (now-iso)
  (define d (seconds->date (current-seconds) #f))
  (format "~a-~a-~aT~a:~a:~aZ" (date-year d) (pad (date-month d)) (pad (date-day d)) (pad (date-hour d)) (pad (date-minute d)) (pad (date-second d))))
(define (pad n) (if (< n 10) (format "0~a" n) (format "~a" n)))

(define (ws-key-of e)
  (define a (hash-ref e 'attrs))
  (define host (hash-ref a 'host #f))
  (and (equal? (hash-ref e 'kind) "workspace") (string? host)
       (cond [(eq? (hash-ref a 'chat #f) #t) (cons host 'chat)]
             [(string? (hash-ref a 'dir #f)) (cons host (hash-ref a 'dir))]
             [else #f])))

(define (drop-ws-key! c id)
  (set-catalog-ws-key! c (for/hash ([(k v) (in-hash (catalog-ws-key c))] #:unless (equal? v id)) (values k v))))

(define (apply-record! c r)
  (define (j k [d #f]) (hash-ref r k d))
  (case (j 'op)
    [("entity")
     (when (and (string? (j 'id)) (member (j 'kind) KINDS))
       (define e (hasheq 'id (j 'id) 'kind (j 'kind) 'name (let ([n (j 'name)]) (if (string? n) n (j 'id)))
                         'attrs (let ([a (j 'attrs)]) (if (hash? a) a (hasheq)))))
       (when (hash-ref (catalog-entities c) (j 'id) #f) (drop-ws-key! c (j 'id)))
       (set-catalog-entities! c (hash-set (catalog-entities c) (j 'id) e))
       (let ([k (ws-key-of e)]) (when k (set-catalog-ws-key! c (hash-set (catalog-ws-key c) k (j 'id)))))
       (bump-next! c (j 'id)))]
    [("remove") (set-catalog-entities! c (hash-remove (catalog-entities c) (j 'id))) (drop-ws-key! c (j 'id))]
    [("edge") (when (and (string? (j 'from)) (string? (j 'to)))
                (define role (let ([r (j 'role)]) (if (string? r) r "member")))
                (set-catalog-out! c (hash-update (catalog-out c) (j 'from) (lambda (m) (hash-set m (j 'to) role)) (hash)))
                (set-catalog-in! c (hash-update (catalog-in c) (j 'to) (lambda (m) (hash-set m (j 'from) role)) (hash))))]
    [("unedge")
     (set-catalog-out! c (hash-update (catalog-out c) (j 'from) (lambda (m) (hash-remove m (j 'to))) (hash)))
     (set-catalog-in! c (hash-update (catalog-in c) (j 'to) (lambda (m) (hash-remove m (j 'from))) (hash)))]
    [("session")
     (when (string? (j 'id))
       (define old (hash-ref (catalog-sessions c) (j 'id) (hasheq 'id (j 'id) 'attrs (hasheq))))
       (set-catalog-sessions! c (hash-set (catalog-sessions c) (j 'id)
                                          (hasheq 'id (j 'id) 'host (or (j 'host) (hash-ref old 'host #f)) 'path (or (j 'path) (hash-ref old 'path #f))
                                                  'attrs (for/fold ([a (hash-ref old 'attrs)]) ([(k v) (in-hash (let ([x (j 'attrs)]) (if (hash? x) x (hasheq))))])
                                                           (hash-set a k v))))))]
    [("changeset")
     (when (string? (j 'id))
       (set-catalog-changesets! c (hash-set (catalog-changesets c) (j 'id) (hasheq 'proposal r 'review #f 'applied #f 'undone #f)))
       (bump-next! c (j 'id)))]
    [("changeset-review" "changeset-applied" "changeset-undone")
     (define cs (hash-ref (catalog-changesets c) (j 'id) #f))
     (when cs
       (set-catalog-changesets! c (hash-set (catalog-changesets c) (j 'id)
                                            (hash-set cs (case (j 'op) [("changeset-review") 'review] [("changeset-applied") 'applied] [else 'undone]) r))))]
    [else (void)])
) ; end define apply-record!

(define (bump-next! c id)
  (define m (regexp-match #px"^[a-z]([0-9]+)$" id))
  (when m (set-catalog-next! c (max (catalog-next c) (add1 (string->number (cadr m)))))))

(define (empty-catalog path) (catalog path (hash) (hash) (hash) (hash) 1 (hash) #f (hash)))

;; 打开（文件不存在 = 空目录）；坏行跳过
(define (catalog-open path)
  (define c (empty-catalog path))
  (when (and path (file-exists? path))
    (call-with-input-file path
      (lambda (in)
        (let loop ()
          (define r (with-handlers ([exn:fail? (lambda (_e) 'bad)]) (parameterize ([read-accept-reader #f]) (read in))))
          (cond [(eof-object? r) (void)]
                [(eq? r 'bad) (void)]                                  ; 截断的尾行：停在这里
                [else (when (hash? r) (apply-record! c r)) (loop)])))))
  c
) ; end define catalog-open

(define (write-records! c rs)
  (when (and (catalog-path c) (pair? rs))
    (make-parent-directory* (catalog-path c))
    (call-with-output-file (catalog-path c) #:exists 'append
      (lambda (out) (for ([r (in-list rs)]) (write r out) (newline out))))))

(define (append! c r0)
  (define r (hash-set r0 'ts (now-iso)))
  (apply-record! c r)
  (if (catalog-batch c)
      (set-catalog-batch! c (cons r (catalog-batch c)))
      (write-records! c (list r)))
  r
) ; end define append!

;; thunk 里的写入攒起来，最后一次写文件（同步一次会话列表可能是上千条边）
(define (catalog-batch! c thunk)
  (cond [(catalog-batch c) (thunk)]
        [else (set-catalog-batch! c '())
              (dynamic-wind void thunk
                            (lambda () (define rs (reverse (catalog-batch c))) (set-catalog-batch! c #f) (write-records! c rs)))]))

;; ---------------------------------------------------------------- 快照（回放用：录制时的当前状态，不含历史）

(define (catalog-snapshot c)
  (hasheq 'entities (hash-values (catalog-entities c))
          'edges (for*/list ([(from m) (in-hash (catalog-out c))] [(to role) (in-hash m)]) (list from to role))
          'sessions (hash-values (catalog-sessions c))
          'changesets (for*/list ([cs (in-hash-values (catalog-changesets c))] [k (in-list '(proposal review applied undone))] #:when (hash-ref cs k #f))
                        (hash-ref cs k))
          'next (catalog-next c)))

(define (catalog-from-snapshot snap)
  (define c (empty-catalog #f))
  (when (hash? snap)
    (for ([e (in-list (hash-ref snap 'entities '()))] #:when (hash? e)) (apply-record! c (hash-set e 'op "entity")))
    (for ([x (in-list (hash-ref snap 'edges '()))] #:when (and (list? x) (= (length x) 3)))
      (apply-record! c (hasheq 'op "edge" 'from (first x) 'to (second x) 'role (third x))))
    (for ([s (in-list (hash-ref snap 'sessions '()))] #:when (hash? s)) (apply-record! c (hash-set s 'op "session")))
    (define css (filter hash? (hash-ref snap 'changesets '())))                  ; 提议在前，审阅 / 应用 / 撤销在后
    (for ([r (in-list (append (filter (lambda (r) (equal? (hash-ref r 'op #f) "changeset")) css)
                              (filter (lambda (r) (not (equal? (hash-ref r 'op #f) "changeset"))) css)))])
      (apply-record! c r))
    (let ([n (hash-ref snap 'next 1)]) (when (exact-positive-integer? n) (set-catalog-next! c (max n (catalog-next c))))))
  c
) ; end define catalog-from-snapshot

;; ---------------------------------------------------------------- 写

(define (catalog-add-entity! c kind name [attrs (hasheq)])
  (unless (member kind KINDS) (raise-argument-error 'catalog-add-entity! "project | super-workspace | workspace" kind))
  (define id (format "~a~a" (substring kind 0 1) (catalog-next c)))
  (append! c (hasheq 'op "entity" 'id id 'kind kind 'name name 'attrs attrs))
  id
) ; end define catalog-add-entity!

(define (catalog-update-entity! c id #:name [name #f] #:attrs [attrs #f])
  (define e (hash-ref (catalog-entities c) id #f))
  (when e
    (append! c (hasheq 'op "entity" 'id id 'kind (hash-ref e 'kind) 'name (or name (hash-ref e 'name))
                       'attrs (if attrs (for/fold ([a (hash-ref e 'attrs)]) ([(k v) (in-hash attrs)]) (hash-set a k v)) (hash-ref e 'attrs)))))
) ; end define catalog-update-entity!

(define (edge-role c from to) (hash-ref (hash-ref (catalog-out c) from (hash)) to #f))
(define (catalog-remove-entity! c id) (when (hash-ref (catalog-entities c) id #f) (append! c (hasheq 'op "remove" 'id id))))
(define (catalog-link! c from to [role "member"])
  (unless (equal? (edge-role c from to) role) (append! c (hasheq 'op "edge" 'from from 'to to 'role role))))
(define (catalog-unlink! c from to) (when (edge-role c from to) (append! c (hasheq 'op "unedge" 'from from 'to to))))
(define (catalog-set-session! c sid #:host [host #f] #:path [path #f] #:attrs [attrs (hasheq)])
  (append! c (hasheq 'op "session" 'id sid 'host host 'path path 'attrs attrs)))

;; 会话删掉了：解开它的所有边（条目的属性留着，无害）
(define (catalog-forget-session! c sid)
  (catalog-batch! c (lambda () (for ([from (in-list (hash-keys (hash-ref (catalog-in c) sid (hash))))]) (catalog-unlink! c from sid)))))

;; 变更清单：提议 → id（c1 c2 …）；之后的审阅 / 应用 / 撤销按 kind 记
(define (catalog-add-changeset! c proposal)
  (define id (format "c~a" (catalog-next c)))
  (append! c (hash-set* proposal 'op "changeset" 'id id))
  id)
(define (catalog-changeset-event! c id kind data)               ; kind：'review 'applied 'undone
  (append! c (hash-set* data 'op (case kind [(review) "changeset-review"] [(applied) "changeset-applied"] [else "changeset-undone"]) 'id id)))
(define (catalog-changeset c id) (hash-ref (catalog-changesets c) id #f))
;; 全部清单，新的在前
(define (catalog-changesets-list c)
  (sort (hash-values (catalog-changesets c)) string>? #:key (lambda (cs) (format "~a" (hash-ref (hash-ref cs 'proposal) 'ts "")))))

;; ---------------------------------------------------------------- 读

(define (catalog-entity c id) (hash-ref (catalog-entities c) id #f))
(define (live? c id) (or (hash-ref (catalog-entities c) id #f) (hash-ref (catalog-sessions c) id #f) (regexp-match? #rx"\u0001" id)))
(define (catalog-entities-of c kind)
  (sort (for/list ([e (in-hash-values (catalog-entities c))] #:when (equal? (hash-ref e 'kind) kind)) e) string<? #:key (lambda (e) (hash-ref e 'name))))
;; 边（只算两端都还在的）→ (listof (cons id 角色))
(define (catalog-children c id)
  (if (hash-ref (catalog-entities c) id #f)                                       ; 删掉的实体：没有子
      (for/list ([(to role) (in-hash (hash-ref (catalog-out c) id (hash)))] #:when (live? c to)) (cons to role))
      '()))
(define (catalog-parents c id)
  (for/list ([(from role) (in-hash (hash-ref (catalog-in c) id (hash)))] #:when (hash-ref (catalog-entities c) from #f)) (cons from role)))
(define (catalog-session c sid) (hash-ref (catalog-sessions c) sid #f))
(define (catalog-session-attr c sid k [d #f]) (let ([s (catalog-session c sid)]) (if s (hash-ref (hash-ref s 'attrs) k d) d)))

;; 会话的归属：→ (values 工作区 super-workspace 项目)，各是实体的表（按名字排）。
;; 项目 = 直接挂的 + 经工作区 / super-workspace 推出的
(define (catalog-session-groups c sid)
  (define (of-kind ids kind) (filter (lambda (e) (and e (equal? (hash-ref e 'kind) kind))) (map (lambda (id) (catalog-entity c id)) ids)))
  (define direct (map car (catalog-parents c sid)))
  (define ws (of-kind direct "workspace"))
  (define sws (of-kind (remove-duplicates (append* (for/list ([w (in-list ws)]) (map car (catalog-parents c (hash-ref w 'id)))))) "super-workspace"))
  (define ps (of-kind (remove-duplicates (append direct
                                                 (append* (for/list ([e (in-list (append ws sws))]) (map car (catalog-parents c (hash-ref e 'id)))))))
                      "project"))
  (define (by-name l) (sort (remove-duplicates l) string<? #:key (lambda (e) (hash-ref e 'name))))
  (values (by-name ws) (by-name sws) (by-name ps))
) ; end define catalog-session-groups

;; 实体的指令（attrs 'instructions；design-memory.md M1）：非空白的文字或 #f
(define (catalog-entity-instructions e)
  (define v (and e (hash-ref (hash-ref e 'attrs (hasheq)) 'instructions #f)))
  (and (string? v) (non-empty-string? (string-trim v)) v))
;; 会话的指令链：有指令的 项目 → super-workspace → 主工作区（role primary；不含聊天工作区）→ 实体表。
;; 只「去过」的工作区（visited）不算：那是会话临时进过的目录，不是它的归属。
;; 还没进目录的会话（新建、没消息：同步跳过临时会话）：#:host + #:dir（或 #:chat? #t）找已有的工作区（只读，不建），从它推出
;; super-workspace 与项目——否则新会话的第一轮拿不到工作区 / 项目的指令
(define (catalog-instruction-chain c sid #:host [host #f] #:dir [dir #f] #:chat? [chat? #f])
  (filter catalog-entity-instructions (catalog-scope-chain c sid #:host host #:dir dir #:chat? chat?)))
;; 会话的作用域链（不管有没有指令）：项目 → super-workspace → 主工作区。#:include-chat? #t：聊天工作区也算（记忆用；指令不用）
(define (catalog-scope-chain c sid #:host [host #f] #:dir [dir #f] #:chat? [chat? #f] #:include-chat? [include-chat? #f])
  (define-values (ws sws ps) (catalog-session-groups c sid))
  (define primary (for/list ([pr (in-list (catalog-parents c sid))] #:when (equal? (cdr pr) "primary")) (car pr)))
  (define fallback (and (null? primary) host (hash-ref (catalog-ws-key c) (cons host (if chat? 'chat dir)) #f)))
  (define (chat-ws? w) (hash-ref (hash-ref w 'attrs (hasheq)) 'chat #f))
  (define (parents-of-kind id kind) (filter (lambda (e) (and e (equal? (hash-ref e 'kind) kind))) (map (lambda (pr) (catalog-entity c (car pr))) (catalog-parents c id))))
  (cond
    [(and fallback (catalog-entity c fallback))
     (define w (catalog-entity c fallback))
     (define sws2 (parents-of-kind fallback "super-workspace"))
     (define ps2 (remove-duplicates (append ps (parents-of-kind fallback "project")
                                            (append* (for/list ([e (in-list sws2)]) (parents-of-kind (hash-ref e 'id) "project"))))))
     (define (by-name l) (sort l string<? #:key (lambda (e) (hash-ref e 'name))))
     (append (by-name ps2) (by-name sws2) (if (and (chat-ws? w) (not include-chat?)) '() (list w)))]
    [else
     (append ps sws (filter (lambda (w) (and (member (hash-ref w 'id) primary) (or include-chat? (not (chat-ws? w))))) ws))]))

;; ---------------------------------------------------------------- 迁移 / 同步

(define (dir-name dir) (let-values ([(_b n _d) (split-path (string->path (regexp-replace #rx"/+$" dir "")))]) (if (path? n) (path->string n) dir)))

;; 主机 + 目录 → 工作区 id（没有就建，名字 = 目录名 @ 主机）
(define (catalog-ensure-workspace! c host dir)
  (or (hash-ref (catalog-ws-key c) (cons host dir) #f)
      (catalog-add-entity! c "workspace" (format "~a @ ~a" (dir-name dir) host) (hasheq 'host host 'dir dir))))

;; 主机的聊天工作区（匿名工作区：这台主机的聊天会话都挂在这里）
(define (catalog-ensure-chat-workspace! c host)
  (or (hash-ref (catalog-ws-key c) (cons host 'chat) #f)
      (catalog-add-entity! c "workspace" (format "chat @ ~a" host) (hasheq 'host host 'chat #t))))

(define (has-primary-workspace? c sid)
  (for/or ([(from role) (in-hash (hash-ref (catalog-in c) sid (hash)))])
    (and (equal? role "primary") (let ([e (catalog-entity c from)]) (and e (equal? (hash-ref e 'kind) "workspace"))))))

;; 会话挂到工作区（已经有 primary 工作区的不动）。幂等
(define (catalog-assign-session! c sid host dir)
  (unless (or (has-primary-workspace? c sid) (not (string? dir)))
    (catalog-link! c (catalog-ensure-workspace! c host dir) sid "primary")))

;; 一台主机的会话列表（内核 list_sessions 的行）→ 挂到工作区：聊天会话 → 这台的聊天工作区；其它 → 主机 + 工作目录。
;; 临时会话（'ephemeral）不挂（切走就删）。行上的 git_remote 记到工作区（变了才写）。幂等：第二次同步不写任何记录
(define (catalog-sync-host! c host rows)
  (catalog-batch!
   c
   (lambda ()
     (for ([r (in-list rows)] #:when (hash? r))
       (define path (hash-ref r 'path #f))
       (define dir (hash-ref r 'workdir #f))
       (when (and (string? path) (not (eq? (hash-ref r 'ephemeral #f) #t)))
         (define sid (session-id host path))
         (cond
           [(eq? (hash-ref r 'chat #f) #t)
            (unless (has-primary-workspace? c sid) (catalog-link! c (catalog-ensure-chat-workspace! c host) sid "primary"))]
           [(string? dir)
            (catalog-assign-session! c sid host dir)
            (define remote (hash-ref r 'git_remote #f))
            (define wid (hash-ref (catalog-ws-key c) (cons host dir) #f))
            (when (and (string? remote) wid (not (equal? (hash-ref (hash-ref (catalog-entity c wid) 'attrs) 'git_remote #f) remote)))
              (catalog-update-entity! c wid #:attrs (hasheq 'git_remote remote)))]
           [else (void)]))))))

;; git 远端的规范形式：去掉协议、用户名、端口、.git 与结尾的 /，scp 形式的「主机:路径」改成「主机/路径」，主机名小写。
;;   git@github.com:me/pi2.git、https://github.com/me/pi2、ssh://git@github.com:22/me/pi2.git → github.com/me/pi2
(define (normalize-remote u)
  (define s1 (regexp-replace #px"^[a-zA-Z][a-zA-Z0-9+.-]*://" (string-trim u) ""))
  (define s2 (regexp-replace #px"^[^@/]+@" s1 ""))
  (define s3 (regexp-replace #px"^([^/:]+):[0-9]+/" s2 "\\1/"))
  (define s4 (regexp-replace #px"^([^/:]+):" s3 "\\1/"))
  (define s5 (regexp-replace #px"(\\.git)?/*$" s4 ""))
  (define m (regexp-match #px"^([^/]*)(.*)$" s5))
  (string-append (string-downcase (cadr m)) (caddr m))
) ; end define normalize-remote

;; super-workspace 建议：git 远端（规范形式）相同、还没在同一个 super-workspace 下的工作区 → (list (cons 远端 (工作区 id …)) …)
(define (catalog-super-suggestions c)
  (define by-remote (make-hash))
  (for ([e (in-list (catalog-entities-of c "workspace"))])
    (define r (hash-ref (hash-ref e 'attrs) 'git_remote #f))
    (when (string? r) (hash-update! by-remote (normalize-remote r) (lambda (l) (cons (hash-ref e 'id) l)) '())))
  (sort
   (for*/list ([(r ids) (in-hash by-remote)] #:when (> (length ids) 1)
               #:unless (for/or ([sw (in-list (catalog-entities-of c "super-workspace"))])
                          (define kids (map car (catalog-children c (hash-ref sw 'id))))
                          (andmap (lambda (id) (member id kids)) ids)))
     (cons r (sort ids string<?)))
   string<? #:key car)
) ; end define catalog-super-suggestions

(provide catalog-add-changeset! catalog-changeset-event! catalog-changeset catalog-changesets-list
         catalog? catalog-open catalog-path session-id catalog-batch!
         catalog-snapshot catalog-from-snapshot
         catalog-add-entity! catalog-update-entity! catalog-remove-entity! catalog-link! catalog-unlink! catalog-set-session! catalog-forget-session!
         catalog-entity catalog-entities-of catalog-children catalog-parents catalog-session catalog-session-attr catalog-session-groups
         catalog-entity-instructions catalog-instruction-chain catalog-scope-chain
         catalog-ensure-workspace! catalog-ensure-chat-workspace! catalog-assign-session! catalog-sync-host!
         normalize-remote catalog-super-suggestions)
