#lang tstring racket
;; session-tools.rkt — 会话的信息与导出（design-workspace.md W1；界面「信息…」「导出…」）。
;; 统计经索引只读需要的记录类型（不重放、不解析整个文件）；导出读全部记录、投影成条目再排版。

(require racket/pvector json
         (file "model.rkt") (file "rktd.rkt") (file "session.rkt") (file "session-index.rkt") (file "items.rkt") (file "chat-space.rkt"))

(define (data-of r) (and (rec? r) (rec-data r)))
(define (jnum h k) (let ([v (and (hash? h) (hash-ref h k #f))]) (if (real? v) v 0)))

;; → jsexpr（hasheq）
(define (session-stats path)
  (define ix (index-open path))
  (define n (index-count ix))
  (define (positions . types)
    (define codes (map type-code types))
    (index-positions ix (lambda (e) (memv (ientry-type e) codes))))
  (define (read-type . types) (map cdr (index-read ix (apply positions types))))
  (define (count-type t) (length (positions t)))
  (define info (read-session-info path))
  (define meta (let ([rs (index-read ix (if (> n 0) '(0) '()))]) (and (pair? rs) (data-of (cdr (car rs))))))
  (define cfg (and (list? meta) (>= (length meta) 3) (config? (third meta)) (third meta)))
  ;; 用量与花费：每次请求一条 req 记录（按模型合计）
  (define by-model (make-hash))
  (for ([d (in-list (map data-of (read-type 'req)))] #:when (hash? d))
    (define m (let ([v (hash-ref d 'model "?")]) (if (string? v) v "?")))
    (hash-update! by-model m (lambda (a) (vector (+ (vector-ref a 0) 1) (+ (vector-ref a 1) (jnum d 'input)) (+ (vector-ref a 2) (jnum d 'output))
                                                 (+ (vector-ref a 3) (jnum d 'cost))))
                  (vector 0 0 0 0)))
  ;; 工具：每次调用结束一条 tool 记录
  (define by-tool (make-hash))
  (for ([d (in-list (map data-of (read-type 'tool)))] #:when (hash? d))
    (define name (let ([v (hash-ref d 'name "?")]) (if (string? v) v "?")))
    (hash-update! by-tool name (lambda (a) (vector (+ (vector-ref a 0) 1) (+ (vector-ref a 1) (jnum d 'ms)) (+ (vector-ref a 2) (if (eq? (hash-ref d 'error #f) #t) 1 0))
                                                   (let ([s (hash-ref d 'source #f)]) (if (string? s) s (vector-ref a 3)))))
                  (vector 0 0 0 "builtin")))
  ;; 权限问答
  (define answers (for/fold ([h (hash)]) ([d (in-list (map data-of (read-type 'perm-answer)))] #:when (hash? d))
                    (hash-update h (format "~a" (hash-ref d 'decision "?")) add1 0)))
  ;; 媒体（带媒体的消息）
  (define media (for*/list ([pos (in-list (index-positions ix (lambda (e) (and (= (ientry-type e) (type-code 'msg)) (positive? (bitwise-and (ientry-flags e) FLAG-MEDIA))))))]
                            [r (in-value (cdr (car (index-read ix (list pos)))))]
                            #:when (message? (data-of r))
                            [b (in-list (message-blocks (data-of r)))] #:when (media-block? b))
                  b))
  ;; 提示（按 code）、子 agent、分叉来源
  (define notes (for/fold ([h (hash)]) ([d (in-list (map data-of (read-type 'note)))] #:when (hash? d))
                  (hash-update h (format "~a" (hash-ref d 'code "?")) add1 0)))
  (define subs (for/fold ([h (hash)]) ([d (in-list (map data-of (read-type 'sub)))] #:when (hash? d))
                 (hash-update h (format "~a" (hash-ref d 'parent "?")) add1 0)))
  (define fork-of (for/first ([d (in-list (map data-of (read-type 'fork)))] #:when (and (list? d) (pair? d))) (car d)))
  ;; 注入的指令（最后一条 instructions 记录：哪些文件 / 设置、多少字节、有没有被截断；design-memory.md M1）
  (define instr (let ([ds (filter hash? (map data-of (read-type 'instructions)))]) (and (pair? ds) (car (reverse ds)))))
  ;; 记忆（design-memory.md M2）：会话开始时注入的条目（最后一条 memory 记录）+ memory_search 查到过的（去重，按第一次出现）
  (define mem (let ([ds (filter hash? (map data-of (read-type 'memory)))]) (and (pair? ds) (car (reverse ds)))))
  (define used (remove-duplicates (append* (for/list ([d (in-list (map data-of (read-type 'memory_used)))] #:when (and (hash? d) (list? (hash-ref d 'ids #f)))) (hash-ref d 'ids)))))
  ;; 时间：meta 的时刻 = 开始；最后一条记录的时刻 = 最近活动
  (define last-ts (and (> n 0) (let ([r (cdr (car (index-read ix (list (sub1 n)))))]) (and (rec? r) (rec-ts r)))))
  (define users (length (index-positions ix (lambda (e) (and (= (ientry-type e) (type-code 'msg)) (= (ientry-role e) ROLE-USER))))))
  (hasheq 'path (if (path? path) (path->string path) path)
          'instructions (if instr (hash-ref instr 'sources '()) 'null)
          'instructions_max_bytes (if instr (hash-ref instr 'max_bytes 'null) 'null)
          'memory (if mem (hasheq 'ids (hash-ref mem 'ids '()) 'bytes (hash-ref mem 'bytes 0)) 'null)
          'memory_used used
          'title (session-info-title info)
          'created (session-info-ts info)
          'last_active (or last-ts 'null)
          'workdir (or (session-info-workdir info) (and cfg (config-workdir cfg)) 'null)
          'chat (and (session-info-chat? info) #t)
          'workdir_bytes (let ([w (session-info-workdir info)])                      ; 聊天会话：它的目录占多少（最多量 1 s）
                           (if (and (session-info-chat? info) w (directory-exists? w))
                               (let-values ([(b _ok) (dir-size w #:deadline (+ (current-inexact-milliseconds) 1000.0))]) b)
                               'null))
          'model (session-info-model info)
          'bytes (file-size path)
          'index_bytes (let ([p (index-path path)]) (if (file-exists? p) (file-size p) 0))
          'records n
          'messages (session-info-nmsg info)
          'user_messages users
          'compactions (count-type 'compact)
          'requests (for/sum ([v (in-hash-values by-model)]) (vector-ref v 0))
          'models (for/list ([(m v) (in-hash by-model)])
                    (hasheq 'model m 'requests (vector-ref v 0) 'input (vector-ref v 1) 'output (vector-ref v 2) 'cost (vector-ref v 3)))
          'cost (for/sum ([v (in-hash-values by-model)]) (vector-ref v 3))
          'tools (for/list ([(name v) (in-hash by-tool)])
                   (hasheq 'name name 'calls (vector-ref v 0) 'ms (vector-ref v 1) 'errors (vector-ref v 2) 'source (vector-ref v 3)))
          'permissions (for/hasheq ([(k v) (in-hash answers)]) (values (string->symbol k) v))
          'notes (for/hasheq ([(k v) (in-hash notes)]) (values (string->symbol k) v))
          'media (hasheq 'count (length media)
                         'bytes (for/sum ([b (in-list media)]) (let ([m (media-block-meta b)]) (if (hash? m) (jnum m 'bytes) 0))))
          'subagents (for/list ([(k v) (in-hash subs)]) (hasheq 'call k 'records v))
          'forked_from (if (string? fork-of) fork-of 'null)
          'effects (count-type 'effect))
) ; end define session-stats

;; ---------------------------------------------------------------- 导出

(define (all-items path)
  (define ix (index-open path))
  (filter values (for/list ([pr (in-list (index-read ix (for/list ([i (in-range (index-count ix))]) i)))])
                   (record->item (car pr) (cdr pr) #:preview-bytes 100000000))))

(define (fence s) (let ([n (let loop ([k 3]) (if (regexp-match? (regexp (make-string k #\`)) s) (loop (add1 k)) k))]) (make-string n #\`)))

(define (export-markdown path)
  (define info (read-session-info path))
  (define out (open-output-string))
  (fprintf out "# ~a\n\n_~a · ~a · ~a_\n" (session-info-title info) (session-info-ts info) (session-info-model info) path)
  (for ([it (in-list (all-items path))])
    (define (parts) (hash-ref it 'parts '()))
    (case (hash-ref it 'kind)
      [("user")
       (fprintf out "\n## User\n\n")
       (for ([p (parts)])
         (case (hash-ref p 'type)
           [("text") (fprintf out "~a\n" (hash-ref p 'text ""))]
           [("media") (fprintf out "\n[~a: ~a]\n" (hash-ref p 'kind "?") (let ([n (hash-ref p 'name 'null)]) (if (string? n) n (hash-ref p 'sha ""))))]
           [("tool_result") (define c (hash-ref p 'content "")) (fprintf out "\n~a\n~a\n~a\n" (fence c) c (fence c))]
           [else (void)]))]
      [("assistant")
       (fprintf out "\n## Assistant\n\n")
       (for ([p (parts)])
         (case (hash-ref p 'type)
           [("thinking") (unless (string=? (hash-ref p 'text "") "") (fprintf out "> ~a\n\n" (regexp-replace* #rx"\n" (hash-ref p 'text "") "\n> ")))]
           [("text") (fprintf out "~a\n" (hash-ref p 'text ""))]
           [("tool_call") (define in (jsexpr->string (hash-ref p 'input (hasheq))))
                          (fprintf out "\n**Tool call** `~a`\n~ajson\n~a\n~a\n" (hash-ref p 'name "?") (fence in) in (fence in))]
           [else (void)]))]
      [("results")
       (for ([p (parts)])
         (define c (hash-ref p 'content ""))
         (fprintf out "\n**Tool result**~a\n~a\n~a\n~a\n" (if (hash-ref p 'is_error #f) " (error)" "") (fence c) c (fence c)))]
      [("note") (fprintf out "\n_~a_\n" (hash-ref it 'text ""))]
      [("compact") (fprintf out "\n---\n_context compacted (~a messages kept)_\n---\n" (hash-ref it 'count 0))]
      [else (void)]))
  (get-output-string out)
) ; end define export-markdown

;; 给记忆提取用的会话文字（design-memory.md M3）：用户 / 助手的文字 + 工具调用一行，不要工具输出（大、可能有密钥）与思考；每条截 1500 字；
;; 超过 max-bytes：留第一条用户消息（任务）+ 从尾往前放得下的（最近的更有用）。→ (values 文字 消息数)
(define (memory-input path #:max-bytes [max-bytes 16000])
  (define (clip s n) (if (> (string-length s) n) (string-append (substring s 0 n) " …") s))
  (define lines
    (append*
     (for/list ([it (in-list (all-items path))])
       (define (parts) (hash-ref it 'parts '()))
       (case (hash-ref it 'kind)
         [("user") (for/list ([p (parts)] #:when (equal? (hash-ref p 'type #f) "text")) (string-append "USER: " (clip (hash-ref p 'text "") 1500)))]
         [("assistant")
          (for/list ([p (parts)] #:when (member (hash-ref p 'type #f) '("text" "tool_call")))
            (if (equal? (hash-ref p 'type) "text")
                (string-append "ASSISTANT: " (clip (hash-ref p 'text "") 1500))
                (format "ASSISTANT called ~a ~a" (hash-ref p 'name "?") (clip (jsexpr->string (hash-ref p 'input (hasheq))) 200))))]
         [else '()]))))
  (define (bytes-of l) (add1 (bytes-length (string->bytes/utf-8 l))))
  (define text
    (cond
      [(<= (for/sum ([l lines]) (bytes-of l)) max-bytes) (string-join lines "\n")]
      [else
       (define first-user (for/first ([l lines] #:when (string-prefix? l "USER: ")) l))
       (define budget (- max-bytes (if first-user (bytes-of first-user) 0) 40))
       (define recent (let loop ([ls (reverse lines)] [left budget] [acc '()])
                        (if (or (null? ls) (> (bytes-of (car ls)) left)) acc (loop (cdr ls) (- left (bytes-of (car ls))) (cons (car ls) acc)))))
       (string-join (append (if (and first-user (not (member first-user recent))) (list first-user "… (earlier part of the conversation omitted)") '()) recent) "\n")]))
  (values text (session-info-nmsg (read-session-info path))))

;; JSON：全部条目（与界面同一投影）+ 会话信息
(define (export-json path)
  (jsexpr->string (hasheq 'path (if (path? path) (path->string path) path) 'stats (session-stats path) 'items (all-items path)))
) ; end define export-json

;; 工作目录的 git 远端（origin 的 URL）。不是 git 仓库 / 没有 origin / 目录不在 / 2 s 内没结束 → #f。
;; 界面的目录 catalog 用它给「不同机器上的同一仓库」建议 super-workspace（design-workspace.md §7）
(define (workdir-git-remote dir)
  (with-handlers ([exn:fail? (lambda (_e) #f)])
    (define git (find-executable-path "git"))
    (and git (string? dir) (directory-exists? dir)
         (let-values ([(p out in err) (subprocess #f #f #f git "-C" dir "config" "--get" "remote.origin.url")])
           (close-output-port in)
           (define done? (sync/timeout 2 p))            ; 输出只有一行（远小于管道缓冲），等进程结束再读不会卡住
           (unless done? (subprocess-kill p #t))
           (define s (if done? (string-trim (port->string out)) ""))
           (close-input-port out) (close-input-port err)
           (and done? (eqv? (subprocess-status p) 0) (not (string=? s "")) s))))
) ; end define workdir-git-remote

(provide session-stats export-markdown export-json workdir-git-remote memory-input)
