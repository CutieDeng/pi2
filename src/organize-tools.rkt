#lang tstring racket
;; organize-tools.rkt — 让 agent 整理会话（design-workspace.md §11.7 C2）：
;;   sessions_overview          只读：这台内核的会话（标题、最近活动、消息数、工作目录、聊天？）+ 界面推来的目录视图（项目、置顶、归档）
;;   propose_session_changes    提交一份变更清单 → 发事件 changeset_proposed 给界面审阅；内核这边什么都不改（工具本身没有副作用）
;; 目录（项目 / 工作区）在界面那台机器上（catalog）；界面用 RPC set_catalog_view 把与本内核会话相关的部分推过来。

(require json (file "tool.rkt") (file "session.rkt"))

(define OPS '("link" "unlink" "new-project" "rename" "pin" "unpin" "archive" "unarchive" "delete"))

;; 闭包做的工具
(struct fn-tool (name level spec run)
  #:methods gen:tool
  [(define (tool-name t) (fn-tool-name t))
   (define (tool-permission-level t) (fn-tool-level t))
   (define (tool-spec t) (fn-tool-spec t))
   (define (tool-run t input ctx) ((fn-tool-run t) input ctx))])

(define (iso secs)
  (define d (seconds->date secs #f))
  (define (p n) (if (< n 10) (format "0~a" n) (format "~a" n)))
  (format "~a-~a-~aT~a:~a:~aZ" (date-year d) (p (date-month d)) (p (date-day d)) (p (date-hour d)) (p (date-minute d)) (p (date-second d))))

;; session-dir：() → 会话目录；catalog-view：() → hasheq（'projects (名…) 'sessions ((hasheq 'path 'projects 'pinned 'archived) …)）或 #f；
;; emit!：jsexpr → 发给界面；current-session：() → 当前会话路径（字符串）
(define (make-organize-tools #:session-dir session-dir #:catalog-view catalog-view #:emit! emit! #:current-session current-session)
  (define (view-of path)
    (define v (catalog-view))
    (and (hash? v) (for/first ([s (in-list (hash-ref v 'sessions '()))] #:when (equal? (hash-ref s 'path #f) path)) s)))
  (define (resolve p)                                                         ; 路径或文件名 → 会话目录里的完整路径（不在 → #f）
    (and (string? p)
         (let* ([dir (session-dir)]
                [cand (if (absolute-path? p) (string->path p) (build-path dir p))]
                [cand (if (regexp-match? #rx"[.]rktd$" (path->string cand)) cand (path-add-extension cand #".rktd"))])
           (and (file-exists? cand)
                (equal? (simplify-path (path->complete-path (let-values ([(b _n _d) (split-path cand)]) b)))
                        (simplify-path (path->complete-path dir)))
                (path->string cand)))))
  (define overview
    (fn-tool "sessions_overview" 'read-only
             (function-spec "sessions_overview"
                            "List the user's chat sessions on this machine (most recent first): title, last activity, message count, working folder, whether it is a chat session, and how the user has organized them (projects, pinned, archived). Use it before proposing changes with propose_session_changes."
                            (hasheq 'days (hasheq 'type "integer" 'description "Only sessions active within this many days (optional).")
                                    'query (hasheq 'type "string" 'description "Only sessions whose title contains this text, case-insensitive (optional).")
                                    'limit (hasheq 'type "integer" 'description "At most this many sessions (default 200)."))
                            '())
             (lambda (input _ctx)
               (define days (input-ref input 'days #f))
               (define q (let ([v (input-ref input 'query #f)]) (and (string? v) (string-downcase v))))
               (define limit (let ([v (input-ref input 'limit 200)]) (if (exact-positive-integer? v) v 200)))
               (define now (current-seconds))
               (define rows
                 (for/list ([i (in-list (session-infos (session-dir)))]
                            #:when (or (not (exact-nonnegative-integer? days)) (<= (- now (session-info-mtime i)) (* days 86400)))
                            #:when (or (not q) (regexp-match? (regexp-quote q) (string-downcase (format "~a" (session-info-title i))))))
                   (define p (path->string (if (path? (session-info-path i)) (session-info-path i) (string->path (session-info-path i)))))
                   (define v (view-of p))
                   (hasheq 'session p 'title (session-info-title i) 'last_active (iso (session-info-mtime i)) 'messages (session-info-nmsg i)
                           'workdir (or (session-info-workdir i) 'null) 'chat (and (session-info-chat? i) #t)
                           'current (equal? p (current-session))
                           'projects (if v (hash-ref v 'projects '()) '()) 'pinned (and v (hash-ref v 'pinned #f) #t) 'archived (and v (hash-ref v 'archived #f) #t))))
               (define shown (take rows (min limit (length rows))))
               (define v (catalog-view))
               (ok-outcome (jsexpr->string (hasheq 'projects (if (hash? v) (hash-ref v 'projects '()) '()) 'total (length rows) 'shown (length shown) 'sessions shown))
                           #:display (format "~a sessions" (length shown))))))
  (define propose
    (fn-tool "propose_session_changes" 'read-only
             (function-spec "propose_session_changes"
                            "Propose a batch of changes to how the user's sessions are organized. Nothing is changed by this call: the user sees an auditable list (each item with its reason, before and after) and applies or rejects items. Ops: new-project (project), link / unlink (session, project), rename (session, new_title), pin, unpin, archive, unarchive, delete (session). `session` is the session path from sessions_overview. Give every item a short reason."
                            (hasheq 'summary (hasheq 'type "string" 'description "One sentence: what this changeset does and why.")
                                    'items (hasheq 'type "array"
                                                   'items (hasheq 'type "object"
                                                                  'properties (hasheq 'op (hasheq 'type "string" 'enum OPS)
                                                                                      'session (hasheq 'type "string")
                                                                                      'project (hasheq 'type "string")
                                                                                      'new_title (hasheq 'type "string")
                                                                                      'reason (hasheq 'type "string"))
                                                                  'required '("op"))))
                            '("summary" "items"))
             (lambda (input _ctx)
               (define items (input-ref input 'items '()))
               (define summary (let ([s (input-ref input 'summary "")]) (if (string? s) s "")))
               (cond
                 [(not (and (list? items) (pair? items))) (err-outcome "items must be a non-empty array")]
                 [else
                  (define checked
                    (for/list ([it (in-list items)] [n (in-naturals)])
                      (define op (and (hash? it) (hash-ref it 'op #f)))
                      (define sp (and (hash? it) (resolve (hash-ref it 'session #f))))
                      (cond
                        [(not (member op OPS)) (cons n (format "item ~a: unknown op ~s" n op))]
                        [(and (not (equal? op "new-project")) (not sp)) (cons n (format "item ~a: no such session ~s" n (hash-ref it 'session #f)))]
                        [else (if sp (hash-set it 'session sp) it)])))
                  (define good (filter hash? checked))
                  (define bad (map cdr (filter pair? checked)))
                  (cond
                    [(null? good) (err-outcome (string-join (cons "no valid items:" bad) "\n"))]
                    [else
                     (emit! (hasheq 'type "changeset_proposed" 'summary summary 'items good 'session (current-session)))
                     (ok-outcome (string-append
                                  (format "Sent ~a proposed change(s) to the user for review. Nothing has been changed yet; the user applies or rejects each item." (length good))
                                  (if (null? bad) "" (string-append "\nSkipped:\n" (string-join bad "\n"))))
                                 #:display (format "proposed ~a change(s) for review" (length good)))])]))))
  (list overview propose)
) ; end define make-organize-tools

(provide make-organize-tools)
