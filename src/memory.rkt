#lang tstring racket
;; memory.rkt — 记忆（design-memory.md §2.2 M2）。条目存在 GUI 那台机器（{config-home}/memory.rktd，界面那边的 memory-store.rkt）；
;; 界面经 set_catalog_view 把与本内核会话相关的部分推过来（'memories 条目 + 每个会话行的 'mem 作用域链与读 / 写开关）。这里：
;;   memory-addendum      会话开始时注入系统提示词的「## Memory」一节（合计上限 4 KiB；置顶的先、再按最近改过的；按作用域链分组）
;;   memory-search        memory_search 的打分（查询词 / 中日韩字的二元组出现几个）
;;   make-memory-tools    memory_search（只读）/ memory_note（写：发事件 memory_note 给界面，界面落盘；全局作用域要用户确认）
;; 条目（界面推来的）：(hasheq 'id 'scope 'text 'pinned 'updated)；scope = "global" 或目录实体 id（p1 s2 w3）
;; 作用域链（会话行的 'mem）：(listof (hasheq 'id 'kind 'name))，从一般到具体：global → 项目 → super-workspace → 工作区

(require json (file "tool.rkt"))

(define MEMORY-HEADER "\n\n## Memory")
(define MEMORY-MAX-BYTES 4096)
(define NOTE-MAX-CHARS 500)

(define (j h k [d #f]) (if (hash? h) (hash-ref h k d) d))
(define (utf8-len s) (bytes-length (string->bytes/utf-8 s)))
(define (one-line s) (string-normalize-spaces (regexp-replace* #rx"[\r\n]+" s " ")))

;; 链上作用域的条目：置顶的先，再按最近改过的（'updated：ISO 时刻串，可比）
(define (chain-entries entries chain)
  (define ids (map (lambda (c) (j c 'id)) chain))
  (sort (filter (lambda (e) (member (j e 'scope) ids)) entries)
        (lambda (a b)
          (define pa (eq? (j a 'pinned) #t)) (define pb (eq? (j b 'pinned) #t))
          (cond [(and pa (not pb)) #t] [(and pb (not pa)) #f]
                [else (string>? (format "~a" (j a 'updated "")) (format "~a" (j b 'updated "")))]))))

(define (scope-title c)
  (define k (j c 'kind "global"))
  (cond [(equal? k "global") "### Global"]
        [else (string-append "### " (string-replace k "_" "-") " \"" (format "~a" (j c 'name "?")) "\"")]))

;; → (values 一节的文字或 "" 注入的条目 id 表)
(define (memory-addendum entries chain #:max-bytes [max-bytes MEMORY-MAX-BYTES] #:tools? [tools? #t])
  (define picked
    (let loop ([es (filter (lambda (e) (not (eq? (j e 'dormant) #t))) (chain-entries entries chain))] [left max-bytes] [acc '()])   ; 久没用的（M3 老化）不注入，仍可搜
      (cond
        [(null? es) (reverse acc)]
        [else
         (define n (+ 10 (utf8-len (one-line (format "~a" (j (car es) 'text ""))))))
         (if (<= n left) (loop (cdr es) (- left n) (cons (car es) acc)) (loop (cdr es) left acc))])))   ; 放不下的跳过，后面短的还能放
  (cond
    [(null? picked) (values "" '())]
    [else
     (define body
       (apply string-append
              (for/list ([c (in-list chain)]
                         #:when (ormap (lambda (e) (equal? (j e 'scope) (j c 'id))) picked))
                (string-append "\n" (scope-title c) "\n"
                               (apply string-append
                                      (for/list ([e (in-list picked)] #:when (equal? (j e 'scope) (j c 'id)))
                                        (format "- [~a] ~a\n" (j e 'id) (one-line (format "~a" (j e 'text ""))))))))))
     (values (string-append MEMORY-HEADER
                            "\nFacts remembered from earlier sessions (saved by the user or by you), from general to specific. Use them when relevant; "
                            "they can be outdated, so check before relying on one for anything risky."
                            (if tools? " Look up more with memory_search; save a new durable fact with memory_note." "")
                            "\n" body)
             (map (lambda (e) (j e 'id)) picked))]))

;; 查询 → 词：拉丁字母 / 数字的词（小写）+ 中日韩连续字的二元组（单字就是它自己）
(define (query-terms q)
  (define lower (string-downcase q))
  (define words (regexp-match* #px"[a-z0-9_.-]{2,}" lower))
  (define cjk (append* (for/list ([run (in-list (regexp-match* #px"[぀-ヿ㐀-鿿가-힯]+" lower))])
                         (if (= (string-length run) 1) (list run)
                             (for/list ([i (in-range (sub1 (string-length run)))]) (substring run i (+ i 2)))))))
  (remove-duplicates (append words cjk)))

;; → 命中的条目（分数高的先，同分按最近改过的），最多 limit 条
(define (memory-search entries chain query #:limit [limit 10])
  (define terms (query-terms query))
  (define scored
    (for*/list ([e (in-list (chain-entries entries chain))]
                [s (in-value (let ([t (string-downcase (format "~a" (j e 'text "")))]) (for/sum ([w (in-list terms)]) (if (string-contains? t w) 1 0))))]
                #:when (> s 0))
      (cons s e)))
  (define sorted (sort scored > #:key car))                                       ; sort 稳定：同分保持链里的顺序（置顶、最近）
  (map cdr (take sorted (min limit (length sorted)))))

;; 闭包做的工具
(struct fn-tool (name level spec run)
  #:methods gen:tool
  [(define (tool-name t) (fn-tool-name t))
   (define (tool-permission-level t) (fn-tool-level t))
   (define (tool-spec t) (fn-tool-spec t))
   (define (tool-run t input ctx) ((fn-tool-run t) input ctx))])

;; view-of：() → (values 条目表 作用域链 读? 写? 全局自动?) 或 (values #f …)（界面没推过 / 这个会话不在视图里）
;; emit!：jsexpr → 发给界面；record-used!：(ids query) → 会话里记一条 memory_used
(define (make-memory-tools #:view view-of #:emit! emit! #:record-used! record-used! #:current-session current-session)
  (define search
    (fn-tool "memory_search" 'read-only
             (function-spec "memory_search"
                            "Search the memories saved from earlier sessions (facts the user or you chose to keep: preferences, project conventions, build and test commands, known pitfalls). Returns matching entries with their ids and scopes. The Memory section of the system prompt only holds the most relevant few; use this for anything else."
                            (hasheq 'query (hasheq 'type "string" 'description "Words to look for (any language)."))
                            '("query"))
             (lambda (input _ctx)
               (define q (input-ref input 'query ""))
               (define-values (entries chain read? _w _g) (view-of))
               (cond
                 [(not entries) (err-outcome "memory is not available: no app is connected to this session")]
                 [(not read?) (err-outcome "reading memory is turned off for this session")]
                 [(not (and (string? q) (non-empty-string? (string-trim q)))) (err-outcome "query must be a non-empty string")]
                 [else
                  (define hits (memory-search entries chain q))
                  (record-used! (map (lambda (e) (j e 'id)) hits) q)
                  (define scope-name (for/hash ([c (in-list chain)]) (values (j c 'id) (if (equal? (j c 'kind) "global") "global" (format "~a \"~a\"" (j c 'kind) (j c 'name))))))
                  (ok-outcome (if (null? hits)
                                    "no matching memories"
                                    (string-join (for/list ([e (in-list hits)])
                                                   (format "[~a] (~a) ~a" (j e 'id) (hash-ref scope-name (j e 'scope) "?") (one-line (format "~a" (j e 'text "")))))
                                                 "\n")))]))))
  (define note
    (fn-tool "memory_note" 'mutating
             (function-spec "memory_note"
                            "Save one short, durable fact for later sessions: a user preference, a project convention, a build / test command, a pitfall and its fix. Not for task progress or anything only true right now. Scope: \"workspace\" (this folder; default), \"project\", or \"global\" (every session of this user; the user confirms global ones before they are used)."
                            (hasheq 'text (hasheq 'type "string" 'description "The fact, one or two sentences.")
                                    'scope (hasheq 'type "string" 'enum '("workspace" "project" "global") 'description "Where it applies (default workspace)."))
                            '("text"))
             (lambda (input _ctx)
               (define text (let ([v (input-ref input 'text "")]) (if (string? v) (string-trim v) "")))
               (define kind (let ([v (input-ref input 'scope "workspace")]) (if (member v '("workspace" "project" "global")) v "workspace")))
               (define-values (entries chain _r write? global-auto?) (view-of))
               (define target (and chain (for/first ([c (in-list (reverse chain))] #:when (equal? (j c 'kind) kind)) c)))   ; 同种多个时取最具体的
               (cond
                 [(not entries) (err-outcome "memory is not available: no app is connected to this session")]
                 [(not write?) (err-outcome "writing memory is turned off for this session")]
                 [(string=? text "") (err-outcome "text must be a non-empty string")]
                 [(> (string-length text) NOTE-MAX-CHARS) (err-outcome (format "text is too long (~a characters; at most ~a): keep one short fact" (string-length text) NOTE-MAX-CHARS))]
                 [(not target) (err-outcome (format "this session has no ~a; use ~a" kind (if (equal? kind "project") "workspace or global" "global")))]
                 [else
                  (emit! (hasheq 'type "memory_note" 'session (current-session) 'scope (j target 'id) 'kind kind 'text text))
                  (ok-outcome (cond [(and (equal? kind "global") (not global-auto?))
                                       "proposed for global memory; the user has to confirm it before it is used in other sessions"]
                                      [(equal? kind "global") "saved to global memory"]
                                      [else (format "saved to ~a memory \"~a\"" kind (j target 'name "?"))]))]))))
  (list search note))

(provide MEMORY-HEADER MEMORY-MAX-BYTES NOTE-MAX-CHARS memory-addendum memory-search query-terms chain-entries make-memory-tools)
