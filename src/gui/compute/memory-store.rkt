#lang racket/base
;; memory-store.rkt — 记忆条目（design-memory.md §2.2 M2）：GUI 这台机器的 {config-home}/memory.rktd；只追加的记录，读时重放（同 catalog.rkt）。
;;   (hasheq 'op "add" 'id 'scope 'text 'source "user"|"model" 'session 会话id或#f 'state "active"|"pending" 'ts)
;;   (hasheq 'op "edit" 'id 'text 'ts)       改文字
;;   (hasheq 'op "pin" 'id 'pinned 'ts)      置顶 / 取消
;;   (hasheq 'op "delete" 'id 'ts)           删（条目留在状态里，state "deleted"，可 restore）
;;   (hasheq 'op "restore" 'id 'ts)          撤销删除
;;   (hasheq 'op "accept" 'id 'ts) / (hasheq 'op "reject" 'id 'ts)   待确认的（模型记到全局的）：确认 / 拒绝（拒绝 = state "rejected"）
;;   (hasheq 'op "used" 'ids 'how 'ts)        被注入 / 被查到（内核 memory_used 事件）→ 'last-used（老化：M3）
;;   (hasheq 'op "processed" 'session 'messages 'ts)   自动记忆处理过这个会话（到第几条消息）
;; add 可带 'evidence（自动提取的：会话里的一句原话）；source "auto" = 自动提取
;; scope = "global" 或目录 catalog 的实体 id（p1 s2 w3）。条目 id = m + 序号（由记录推出）。
;; 'seq：每次增 / 改 / 置顶 / 恢复都取一个新序号 = 「最近改过」的次序（推给内核的顺序用它，不用钟点：回放时一致）。
;; 不能写文件（回放）时：path #f → 只在内存里；回放从录制时的快照开始。

(require racket/list racket/string racket/date)

(struct mstore (path [entries #:mutable] [next #:mutable] [seq #:mutable] [processed #:mutable]))   ; entries：id → hasheq；processed：会话 → 消息数

(define (now-iso)
  (define d (seconds->date (current-seconds)))
  (define (p n) (if (< n 10) (format "0~a" n) (format "~a" n)))
  (format "~a-~a-~a ~a:~a" (date-year d) (p (date-month d)) (p (date-day d)) (p (date-hour d)) (p (date-minute d))))

(define (apply-rec! m r)
  (define id (hash-ref r 'id #f))
  (define (bump) (set-mstore-seq! m (add1 (mstore-seq m))) (mstore-seq m))
  (define (upd! f) (let ([e (hash-ref (mstore-entries m) id #f)]) (when e (set-mstore-entries! m (hash-set (mstore-entries m) id (f e))))))
  (case (hash-ref r 'op #f)
    [("add")
     (when (string? id)
       (define n (string->number (substring id 1)))
       (when (and n (>= n (mstore-next m))) (set-mstore-next! m (add1 n)))
       (set-mstore-entries! m (hash-set (mstore-entries m) id
                                        (hasheq 'id id 'scope (hash-ref r 'scope "global") 'text (hash-ref r 'text "")
                                                'source (hash-ref r 'source "user") 'session (hash-ref r 'session #f) 'evidence (hash-ref r 'evidence #f)
                                                'state (hash-ref r 'state "active") 'pinned #f
                                                'created (hash-ref r 'ts "") 'updated (hash-ref r 'ts "") 'seq (bump)))))]
    [("edit") (upd! (lambda (e) (hash-set* e 'text (hash-ref r 'text (hash-ref e 'text)) 'updated (hash-ref r 'ts "") 'seq (bump))))]
    [("pin") (upd! (lambda (e) (hash-set* e 'pinned (eq? (hash-ref r 'pinned #f) #t) 'seq (bump))))]
    [("delete") (upd! (lambda (e) (hash-set* e 'state "deleted" 'prev-state (hash-ref e 'state))))]
    [("restore") (upd! (lambda (e) (hash-set* e 'state (hash-ref e 'prev-state "active") 'seq (bump))))]
    [("accept") (upd! (lambda (e) (hash-set* e 'state "active" 'seq (bump))))]
    [("reject") (upd! (lambda (e) (hash-set e 'state "rejected")))]
    [("used") (for ([i (in-list (let ([v (hash-ref r 'ids '())]) (if (list? v) v '())))])
                (define e (hash-ref (mstore-entries m) i #f))
                (when e (set-mstore-entries! m (hash-set (mstore-entries m) i (hash-set e 'last-used (hash-ref r 'ts ""))))))]
    [("processed") (set-mstore-processed! m (hash-set (mstore-processed m) (hash-ref r 'session "") (hash-ref r 'messages 0)))]
    [else (void)]))

(define (append! m r0)
  (define r (hash-set r0 'ts (now-iso)))
  (apply-rec! m r)
  (when (mstore-path m)
    (with-output-to-file (mstore-path m) #:exists 'append
      (lambda () (write r) (newline)))))

;; 打开（文件不存在 = 空）；坏行跳过（截断的尾行等）
(define (memory-open path)
  (define m (mstore path (hash) 1 0 (hash)))
  (when (and path (file-exists? path))
    (call-with-input-file path
      (lambda (in)
        (let loop ()
          (define r (with-handlers ([exn:fail? (lambda (_e) 'bad)]) (read in)))
          (cond [(eof-object? r) (void)]
                [(hash? r) (apply-rec! m r) (loop)]
                [(eq? r 'bad) (void)]                      ; 读坏了：后面的不要了（只追加：坏的只可能是尾行）
                [else (loop)])))))
  m)

;; ---------------------------------------------------------------- 快照（回放用）
(define (memory-snapshot m) (hasheq 'entries (mstore-entries m) 'next (mstore-next m) 'seq (mstore-seq m) 'processed (mstore-processed m)))
(define (memory-from-snapshot snap)
  (if (hash? snap)
      (mstore #f (let ([e (hash-ref snap 'entries (hash))]) (if (hash? e) e (hash))) (hash-ref snap 'next 1) (hash-ref snap 'seq 0)
              (let ([p (hash-ref snap 'processed (hash))]) (if (hash? p) p (hash))))
      (mstore #f (hash) 1 0 (hash))))

;; ---------------------------------------------------------------- 写
(define (memory-add! m scope text #:source [source "user"] #:session [session #f] #:state [state "active"] #:evidence [evidence #f])
  (define id (format "m~a" (mstore-next m)))
  (append! m (let ([r (hasheq 'op "add" 'id id 'scope scope 'text (string-trim text) 'source source 'session session 'state state)])
               (if evidence (hash-set r 'evidence evidence) r)))
  id)
(define (memory-used! m ids how) (when (pair? ids) (append! m (hasheq 'op "used" 'ids ids 'how how))))
(define (memory-processed! m session n) (append! m (hasheq 'op "processed" 'session session 'messages n)))
(define (memory-processed m) (mstore-processed m))

;; 老化（M3）：没置顶、最近一次被用到（没用过 = 建的时候）距今超过 days 天 → 不注入（仍可搜）
(define (ts->seconds s)
  (define x (and (string? s) (regexp-match #px"^(\\d{4})-(\\d{2})-(\\d{2}) (\\d{2}):(\\d{2})" s)))
  (and x (with-handlers ([exn:fail? (lambda (_e) #f)])
           (apply find-seconds 0 (map string->number (list (list-ref x 5) (list-ref x 4) (list-ref x 3) (list-ref x 2) (list-ref x 1)))))))
(define (memory-dormant? e now [days 30])
  (and (not (hash-ref e 'pinned #f))
       (let ([t (or (ts->seconds (hash-ref e 'last-used #f)) (ts->seconds (hash-ref e 'created #f)))])
         (and t (> (- now t) (* days 86400))))))
(define (memory-edit! m id text) (append! m (hasheq 'op "edit" 'id id 'text (string-trim text))))
(define (memory-pin! m id pinned?) (append! m (hasheq 'op "pin" 'id id 'pinned (and pinned? #t))))
(define (memory-delete! m id) (append! m (hasheq 'op "delete" 'id id)))
(define (memory-restore! m id) (append! m (hasheq 'op "restore" 'id id)))
(define (memory-accept! m id) (append! m (hasheq 'op "accept" 'id id)))
(define (memory-reject! m id) (append! m (hasheq 'op "reject" 'id id)))

;; ---------------------------------------------------------------- 读
(define (memory-entry m id) (hash-ref (mstore-entries m) id #f))
;; 某状态的条目，最近改过的在前（置顶不在这里排：显示 / 注入各自处理）
(define (memory-entries m #:state [state "active"])
  (sort (filter (lambda (e) (equal? (hash-ref e 'state) state)) (hash-values (mstore-entries m))) > #:key (lambda (e) (hash-ref e 'seq 0))))
(define (memory-pending m) (memory-entries m #:state "pending"))

;; 推给内核的条目（作用域在 scopes 里的活动条目；最近改过的在前；不带钟点；久没用的标 dormant：不注入、仍可搜）
(define (memory-view-entries m scopes #:now [now (current-seconds)])
  (for/list ([e (in-list (memory-entries m))] #:when (member (hash-ref e 'scope) scopes))
    (define v (hasheq 'id (hash-ref e 'id) 'scope (hash-ref e 'scope) 'text (hash-ref e 'text) 'pinned (hash-ref e 'pinned #f)))
    (if (memory-dormant? e now) (hash-set v 'dormant #t) v)))

;; 「/remember …」：→ (values 作用域种类 文字)；第一个词是 global / project / workspace（或中文 全局 / 项目 / 工作区）就是作用域，否则缺省 workspace
(define (parse-remember s)
  (define body (string-trim (regexp-replace #rx"^/remember" s "")))
  (define m (regexp-match #px"^(global|project|workspace|全局|项目|工作区)(?:[:：]\\s*|\\s+)(.*)$" body))
  (if m
      (values (case (cadr m) [("global" "全局") "global"] [("project" "项目") "project"] [else "workspace"]) (string-trim (caddr m)))
      (values "workspace" body)))
(define (remember-command? s) (regexp-match? #px"^/remember(\\s|$)" s))

(provide memory-used! memory-processed! memory-processed memory-dormant?
         memory-open memory-snapshot memory-from-snapshot
         memory-add! memory-edit! memory-pin! memory-delete! memory-restore! memory-accept! memory-reject!
         memory-entry memory-entries memory-pending memory-view-entries
         parse-remember remember-command?)
