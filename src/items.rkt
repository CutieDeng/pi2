#lang tstring racket
;; items.rkt — 会话记录 → 界面条目（design-content.md §3.2 / §4.2；RPC v3）。实时（记录写下时发 item 事件）与翻页取回（items 请求）
;; 走同一个投影，界面也只有一个归约函数。条目 id = 记录在会话文件里的序号（索引条目序号），稳定、可随机访问。
;; 一条记录最多投影成一个条目；meta / usage（每轮增量）/ turn / route / effect / fork 不显示（不投影）。
;;
;; 条目（jsexpr，'type "item"，'id 序号，'kind）：
;;   user        'parts（text / media（kind media_type sha name meta））'synthetic #f | "aborted" | "summary" | "notice"
;;   assistant   'parts（thinking / text / tool_call）
;;   results     'parts（tool_result：call_id content is_error total_len truncated）
;;   permission  'perm_id 'tool 'level 'input 'prompt        answer   'perm_id 'decision
;;   note        'level 'code 'text 'data                       usage    'model 'input 'output 'cost
;;   tool        'call_id 'name 'ms 'error 'display 'source 'diff    diff 'call_id 'diff（旧会话；新会话的 diff 在 tool 里）
;;   partial     'text 'reasoning                                compact  'count 'summary
;;   sub         'parent（父工具调用 id）'host（远端拉回的：主机）'inner（子 agent 那条记录的条目，没有 type / id）
;; 长工具输出：条目里只带预览（前 preview-bytes 字节），'total_len 全长，'truncated #t；全文用 item 请求按 id 取。

(require (file "model.rkt"))

(define DEFAULT-PREVIEW-BYTES 16384)

(define (rec-parts r)                                   ; prefab rec → (values type ts data)；不是 rec → (values #f #f #f)
  (if (and (prefab-struct-key r) (eq? (prefab-struct-key r) 'rec))
      (let ([v (struct->vector r)]) (values (vector-ref v 1) (vector-ref v 2) (vector-ref v 3)))
      (values #f #f #f)))

(define (clip s n)
  (if (and (string? s) (> (string-length s) n)) (substring s 0 n) s))

(define (block->part b preview-bytes)
  (cond
    [(text-block? b) (hasheq 'type "text" 'text (text-block-text b))]
    [(thinking-block? b) (hasheq 'type "thinking" 'text (thinking-block-text b))]
    [(tool-use-block? b) (hasheq 'type "tool_call" 'id (tool-use-block-id b) 'name (tool-use-block-name b) 'input (tool-use-block-input b))]
    [(tool-result-block? b)
     (define c (let ([c (tool-result-block-content b)]) (if (string? c) c (format "~a" c))))
     (define full (string-length c))
     (hasheq 'type "tool_result" 'call_id (tool-result-block-tool-use-id b) 'is_error (and (tool-result-block-is-error? b) #t)
             'content (clip c preview-bytes) 'total_len full 'truncated (> full preview-bytes))]
    [(image-block? b) (hasheq 'type "image" 'media_type (image-block-media-type b))]
    [(media-block? b) (media->jsexpr b)]
    [else (hasheq 'type "unknown")])
) ; end define block->part

(define (synthetic-kind text)
  (cond [(regexp-match? #rx"^<turn_aborted>" text) "aborted"]
        [(regexp-match? #rx"^\\[compacted context summary\\]" text) "summary"]
        [(regexp-match? #rx"^Tool budget exhausted" text) "notice"]
        [else #f])
) ; end define synthetic-kind

(define (j h k [d 'null]) (if (hash? h) (hash-ref h k d) d))

;; 媒体段：内容不在条目里，界面按 sha 用 blob 请求取
(define (media->jsexpr b)
  (hasheq 'type "media" 'kind (symbol->string (media-block-kind b)) 'media_type (media-block-media-type b)
          'sha (media-block-sha b) 'name (or (media-block-name b) 'null)
          'meta (let ([m (media-block-meta b)]) (if (hash? m) m (hasheq)))))

;; 记录 → 条目或 #f
(define (record->item pos r #:preview-bytes [pb DEFAULT-PREVIEW-BYTES])
  (define-values (type ts data) (rec-parts r))
  (define (item kind . kvs) (apply hasheq 'type "item" 'id pos 'kind kind 'ts (or ts 'null) kvs))
  (case type
    [(msg)
     (cond
       [(message? data)
        (define bs (message-blocks data))
        (define parts (for/list ([b (in-list bs)]) (block->part b pb)))
        (cond
          [(eq? (message-role data) 'assistant) (item "assistant" 'parts parts)]
          [(and (pair? bs) (andmap tool-result-block? bs)) (item "results" 'parts parts)]
          [else
           (define text (message-text data))
           ;; 中断时补的工具结果 + 中断标记 / 工具额度提示：带工具结果的「合成」用户消息 → 仍按 results 处理工具结果部分
           (item "user" 'parts parts 'synthetic (or (synthetic-kind text) #f))])]
       [else #f])]
    [(perm) (item "permission" 'perm_id (j data 'id) 'tool (j data 'tool) 'level (j data 'level) 'input (j data 'input) 'prompt (j data 'prompt ""))]
    [(perm-answer) (item "answer" 'perm_id (j data 'id) 'decision (j data 'decision "no"))]
    [(note) (item "note" 'level (j data 'level "dim") 'code (j data 'code "") 'text (j data 'text "") 'data (j data 'data))]
    [(req) (item "usage" 'model (j data 'model "?") 'input (j data 'input 0) 'output (j data 'output 0) 'cost (let ([c (j data 'cost #f)]) (if (real? c) c 'null)))]
    [(tool) (item "tool" 'call_id (j data 'id) 'name (j data 'name "?") 'ms (j data 'ms) 'error (j data 'error #f)
                  'display (let ([d (j data 'display)]) (if (string? d) d 'null)) 'source (let ([s (j data 'source)]) (if (string? s) s "builtin"))
                  'diff (let ([d (j data 'diff)]) (if (hash? d) d 'null)))]
    [(diff) (and (list? data) (= (length data) 2) (item "diff" 'call_id (car data) 'diff (cadr data)))]
    [(sub)
     ;; 子 agent 的记录（P5）：本机的 = 内层记录（kind data）→ 同样投影；远端拉回的 = 已投影的条目（'item）
     (define inner (cond [(hash? (j data 'item #f)) (j data 'item)]
                         [(symbol? (j data 'kind #f)) (record->item pos (make-prefab-struct 'rec (j data 'kind) ts (j data 'data #f)) #:preview-bytes pb)]
                         [else #f]))
     (and inner (item "sub" 'parent (j data 'parent) 'host (let ([h (j data 'host #f)]) (if (string? h) h 'null))
                      'inner (hash-remove (hash-remove inner 'type) 'id)))]
    [(tests)                                                       ; 工作区测试流程（design-goalmode.md §12）：run / acceptance / proposal / decision
     (item "tests" 'kind (j data 'kind "run") 'ok (j data 'ok 'null) 'summary (j data 'summary 'null) 'reason (j data 'reason 'null)
           'steps (let ([v (j data 'steps '())]) (if (list? v) v '())) 'changed (let ([v (j data 'changed '())]) (if (list? v) v '()))
           'proposal (j data 'id 'null) 'decision (j data 'decision 'null) 'diff (let ([d (j data 'diff #f)]) (if (hash? d) d 'null)))]
    [(partial) (and (not (eq? (j data 'checkpoint #f) #t))          ; 流式中的检查点（决定 1）：只为记录，不显示
                    (item "partial" 'text (j data 'text "") 'reasoning (j data 'reasoning "")))]
    [(compact)
     (define h (cond [(list? data) data] [(hash? data) (hash-ref data 'history '())] [else '()]))
     (define first (and (pair? h) (message? (car h)) (message-text (car h))))
     (item "compact" 'count (length h)
           'kind (let ([k (and (hash? data) (hash-ref data 'kind #f))]) (if (string? k) k "summary"))   ; summary | prune（删旧工具输出）
           'pruned (if (hash? data) (hash-ref data 'pruned 0) 0) 'saved (if (hash? data) (hash-ref data 'saved 0) 0)
           'summary (if (and first (regexp-match? #rx"^\\[compacted context summary\\]" first))
                        (regexp-replace #rx"^\\[compacted context summary\\]\n?" first "")
                        (if (null? h) "" 'null)))]
    [else #f])
) ; end define record->item

;; 索引条目 → 显示种类（界面估计高度用；不读记录本身）：
;;   0 不显示 1 user 2 assistant 3 results 4 permission 5 note 6 usage 7 partial 8 compact 9 只是更新别的条目 10 合成提示
(define (display-kind type-name role)
  (case type-name
    [(msg) (case role [(1) 1] [(2) 2] [(3) 3] [(4) 10] [else 1])]
    [(perm) 4] [(note tests) 5] [(req) 6] [(partial) 7] [(compact) 8]
    [(perm-answer tool diff sub) 9]
    [else 0]))

(provide record->item display-kind DEFAULT-PREVIEW-BYTES media->jsexpr)
