#lang racket/base
;; transcript.rkt — 对话区组件（RPC v3；design-content.md §4.2–4.3）：内核投影出的条目（item，src/items.rkt）→ 显示条目，排版、绘制、滚动、命中。
;; 单一归约：实时 item 事件、翻页取回的 items、session 事件带的尾部，都经 add-item! 变成显示条目 → 实时与重建得到同一张表。
;; 显示条目：用户消息（含折叠 chip）/ 助手文本（含推理）/ 工具卡片（输出默认折叠；改文件的工具显示彩色 diff）/ 权限卡片 / 提示行 / 用量行。
;;
;; 局部加载：会话里每条记录一个「槽」（槽 s ≥ 1 = 记录 s-1；槽 0 = 第一条记录之前），槽高度存 Fenwick 树（前缀和 ↔ 槽号互查 O(log N)）。
;; 只装视口附近的记录（按 PAGE 条一页向内核取，'wanted 交出请求）；没装的按索引摘要（index_summary）估高，装上后量出实际高度替换。
;; 滚动位置存成「锚点槽 + 槽内偏移」：锚点上方的高度变化（估计 → 实际、摘要到达）不让视口跳动。远处的页逐出（只留高度）。
;; 界面本地的条目（连接提示、刚发出还没写进会话的用户消息）挂在发生时最后那条记录的槽上；用户消息的记录到达时接管本地那条（保留 chip）。
;; 流式中的回答（delta）不是记录：画在所有槽之后，assistant / partial 条目到达时丢弃。
;; 只绘制与视口相交的条目；段落排版结果缓存（流式时只有最后一段在变，它不进缓存）。

(require racket/list racket/string racket/flonum json net/base64
         "../compute/drawlist.rkt" "../compute/palette.rkt" "../compute/layout.rkt" "../compute/paint.rkt" "../compute/rich.rkt" "../compute/fold.rkt"
         "../compute/markdown.rkt" "../compute/mdview.rkt"
         "../compute/hub.rkt" "../compute/interfaces.rkt" "../compute/i18n.rkt" "../compute/ansi.rkt"
         "../compute/view.rkt")                                         ; 视图平面

;; ---------------------------------------------------------------- 显示条目

;; media：媒体段（src/items.rkt 的 media 段：kind media_type sha name meta），图片 / 音频 / 文件在文字下面排成一行行的块
(struct e-user (text folds media))
;; md-src / md-blocks：Markdown 解析缓存（文本变化才重新解析）
(struct e-asst ([text #:mutable] [reasoning #:mutable] [open? #:mutable] [interrupted? #:mutable]
                [md-src #:mutable #:auto] [md-blocks #:mutable #:auto]) #:auto-value #f)
;; status：running（还没有结果）ok error aborted；diff：src/diff.rkt file-diff（path added removed text truncated）或 #f；
;; total：结果全长（字符）；rid：结果所在记录（取全文用）；truncated?：输出只是预览；source：工具来源（builtin / mcp:… / plugin:…）
(struct e-tool (id name input [status #:mutable] [output #:mutable] [ms #:mutable] [expanded? #:mutable]
                [diff #:mutable] [total #:mutable] [rid #:mutable] [truncated? #:mutable] [source #:mutable]
                [fetching? #:mutable]
                [picked #:mutable]                    ; smart 预览：#f 还没要 / 'pending / 'failed / 本地模型挑的行（0 起）
                [ui #:mutable]))                      ; 结构化结果（git_history：{kind "commits" commits …}）或 #f
(struct e-perm (id prompt tool level input [answer #:mutable]))
(struct e-note (text level))                          ; level ∈ info dim warn error usage

(define PAGE 100)                                     ; 翻页单位（记录数）
(define MAX-PAGES 30)                                 ; 同时装载的页数上限（超过 → 逐出离视口最远的）
(define SUMMARY-CHUNK 20000)                          ; index_summary 一次取的记录数
(define PREVIEW-BYTES 8192)                           ; 翻页时工具输出的预览长度（字符）
(define REQUEST-RETRY-MS 10000.0)                     ; 请求发出这么久还没应答 → 再要一次
(define UNKNOWN-H 24.0)                               ; 还没有摘要的记录的估计高度

(struct transcript (rc para-cache
                    code-open [code-version #:mutable]   ; 展开的长代码块键集合；变化计数（作废高度缓存）
                    heights                           ; 条目高度缓存：条目 → (vector 宽 签名 高)
                    [total #:mutable]                 ; 会话里的记录数（槽数 = total + 1）
                    [cap #:mutable]                   ; 下面各数组的容量（槽）
                    [state #:mutable]                 ; bytes：记录 0 未装载 2 已装载
                    [hints #:mutable]                 ; bytes：每条记录 6 字节摘要（种类 标志 行数 u16 文字长/16 u16；种类 255 = 没有）
                    [sh #:mutable] [sw #:mutable]     ; flvector：槽高度、量高度时的宽度（0 = 估计值）
                    [fen #:mutable]                   ; flvector：Fenwick（1 起）
                    groups                            ; 记录序号 → 显示条目表（已装载的）
                    local                             ; 槽 → 本地条目表（正序）
                    [pending #:mutable]               ; 本地用户消息（还没被记录接管），正序：(cons 槽 条目)
                    tools perms                       ; call-id → e-tool；perm-id → e-perm（已装载的）
                    aux                               ; 更新：键 → (hash 记录序号 → 字段)；键 = (cons 'tool call-id) / (cons 'perm perm-id)
                    aux-src                           ; 记录序号 → 它贡献的键表（逐出时删）
                    loaded-pages requested            ; 页 → #t；页（或 (cons 'sum 起点) / (cons 'item 序号)）→ 请求时刻
                    [out #:mutable]                   ; 待交出的请求（倒序）
                    [stream #:mutable]                ; 流式中的回答条目或 #f
                    [busy? #:mutable] [turns #:mutable]
                    [scroll #:mutable] [stick? #:mutable] [anchor #:mutable] [offset #:mutable]
                    [content-h #:mutable] [view-h #:mutable] [width #:mutable]
                    [hits #:mutable]
                    [lines #:mutable]                 ; 本帧画出的文字行：vector of (vector 条目键 行序 文字 字符x表 y 行高 软换行?)
                    [sel-anchor #:mutable] [sel-focus #:mutable]   ; 选择端点：(vector 条目键 行序 字符) 或 #f
                    [preview #:mutable]               ; 工具输出折叠预览的配置（见 preview-lines）
                    nonce [gen #:mutable]
                    [ver #:mutable]                   ; 内容版本（装载 / 摘要 / 实时条目到达时加一）：小地图据此重算刻度
                    [mini #:mutable]                  ; 小地图缓存：(vector 版本 帧计数 刻度表) 或 #f
                    media                             ; media-port（缩略图、音频播放）或 #f（测试）
                    blobs                             ; 取媒体中：sha → (vector 块表（倒序） 已收字节 全长 用途)
                    audio-bytes                       ; 取到的音频：sha → 字节（播放用）
                    failed                            ; 取 / 解码失败的 sha → #t
                    [play-pending #:mutable]          ; 点了播放、字节还在取的 sha
                    children))                        ; 子 agent 的显示条目（P5）：父工具调用 id → (hash 记录序号 → 条目表)           ; 请求标签 = 文档 id:gen；会话重置时 gen 加一 → 旧会话的迟到应答被丢弃
                                                      ; （确定性：回放时与录制时同一标签；连同一守护进程的两个窗口标签可能相同 → 多装几页，无害）

(define (make-transcript rc [id "local"] #:media [media #f])
  (define tr (transcript rc (make-hash) (make-hash) 0 (make-weak-hasheq)
                         0 0 (bytes) (bytes) (flvector) (flvector) (flvector 0.0)
                         (make-hasheqv) (make-hasheqv) '() (make-hash) (make-hash) (make-hash) (make-hasheqv)
                         (make-hasheqv) (make-hash) '() #f #f 0
                         0.0 #t 0 0.0 0.0 0.0 0.0 '() (vector) #f #f DEFAULT-PREVIEW
                         id 0 0 #f media (make-hash) (make-hash) (make-hash) #f (make-hash)))
  (ensure-cap! tr 1)
  tr
) ; end define make-transcript

(define (j h k [d #f]) (if (hash? h) (hash-ref h k d) d))

;; ---------------------------------------------------------------- 槽与 Fenwick

(define (nslots tr) (add1 (transcript-total tr)))

(define (ensure-cap! tr n)                             ; 至少 n 个槽
  (when (> n (transcript-cap tr))
    (define cap (let loop ([c (max 1024 (transcript-cap tr))]) (if (>= c n) c (loop (* 2 c)))))
    (define old (transcript-cap tr))
    (define st (make-bytes cap 0)) (bytes-copy! st 0 (transcript-state tr))
    (define hs (make-bytes (* 6 cap) 0)) (bytes-copy! hs 0 (transcript-hints tr))
    (for ([r (in-range old cap)]) (bytes-set! hs (* 6 r) 255))
    (define sh (make-flvector cap 0.0)) (for ([i (in-range old)]) (flvector-set! sh i (flvector-ref (transcript-sh tr) i)))
    (define sw (make-flvector cap 0.0)) (for ([i (in-range old)]) (flvector-set! sw i (flvector-ref (transcript-sw tr) i)))
    ;; Fenwick 按新容量重建：O(n)
    (define f (make-flvector (add1 cap) 0.0))
    (for ([i (in-range cap)]) (flvector-set! f (add1 i) (flvector-ref sh i)))
    (for ([k (in-range 1 (add1 cap))])
      (define p (+ k (bitwise-and k (- k))))
      (when (<= p cap) (flvector-set! f p (fl+ (flvector-ref f p) (flvector-ref f k)))))
    (set-transcript-state! tr st) (set-transcript-hints! tr hs)
    (set-transcript-sh! tr sh) (set-transcript-sw! tr sw) (set-transcript-fen! tr f)
    (set-transcript-cap! tr cap))
) ; end define ensure-cap!

(define (fen-add! tr i delta)
  (define f (transcript-fen tr))
  (define n (transcript-cap tr))
  (let loop ([k (add1 i)]) (when (<= k n) (flvector-set! f k (fl+ (flvector-ref f k) delta)) (loop (+ k (bitwise-and k (- k))))))
) ; end define fen-add!

(define (fen-prefix tr i)                              ; 槽 [0, i) 的高度和
  (define f (transcript-fen tr))
  (let loop ([k (min i (transcript-cap tr))] [s 0.0]) (if (> k 0) (loop (- k (bitwise-and k (- k))) (fl+ s (flvector-ref f k))) s))
) ; end define fen-prefix

;; 纵坐标 y（槽坐标，槽 0 顶 = 0）所在的槽：前缀和 ≤ y 的最大槽号（零高度的槽被跳过）；夹到 [0, 槽数-1]
(define (fen-find tr y)
  (define f (transcript-fen tr))
  (define n (transcript-cap tr))
  (define top (let loop ([p 1]) (if (> (* 2 p) n) p (loop (* 2 p)))))
  (define pos
    (let loop ([pos 0] [rem y] [step top])
      (if (zero? step) pos
          (let ([nx (+ pos step)])
            (if (and (<= nx n) (<= (flvector-ref f nx) rem))
                (loop nx (- rem (flvector-ref f nx)) (quotient step 2))
                (loop pos rem (quotient step 2)))))))
  (max 0 (min pos (sub1 (nslots tr))))
) ; end define fen-find

(define (slot-h tr s) (flvector-ref (transcript-sh tr) s))
(define (set-slot-h! tr s h0)
  (define h (real->double-flonum h0))
  (define d (fl- h (slot-h tr s)))
  (unless (fl= d 0.0)
    (flvector-set! (transcript-sh tr) s h)
    (fen-add! tr s d))
) ; end define set-slot-h!

(define (fen-total tr) (fen-prefix tr (nslots tr)))

(define (rec-loaded? tr r) (= (bytes-ref (transcript-state tr) r) 2))
(define (page-of r) (quotient r PAGE))

;; 记录数增长到 n（新记录：已装载的空记录 → 高 0；否则按摘要估）
(define (extend-total! tr n #:loaded? [loaded? #f])
  (define old (transcript-total tr))
  (when (> n old)
    (ensure-cap! tr (add1 n))
    (set-transcript-total! tr n)
    (for ([r (in-range old n)])
      (when loaded? (bytes-set! (transcript-state tr) r 2) (hash-set! (transcript-loaded-pages tr) (page-of r) #t))
      (set-slot-h! tr (add1 r) (if loaded? 0.0 (hint-estimate tr r (transcript-width tr))))))
) ; end define extend-total!

;; ---------------------------------------------------------------- 高度估计（按索引摘要 / 按条目文字长度）

(define LH 18.0) (define LH-SMALL 14.0) (define LH-MONO 16.0)

(define (cpl w) (max 20.0 (/ (- (if (> w 0.0) w 800.0) (* 2 PAD)) 8.0)))
(define (vis-lines lines chars w) (max 1 lines (inexact->exact (ceiling (/ chars (cpl w))))))

(define (hint-estimate tr r w)
  (define hs (transcript-hints tr))
  (define o (* 6 r))
  (define k (bytes-ref hs o))
  (define flags (bytes-ref hs (+ o 1)))
  (define lines (integer-bytes->integer hs #f #f (+ o 2) (+ o 4)))
  (define chars (* 16 (integer-bytes->integer hs #f #f (+ o 4) (+ o 6))))
  (define v (vis-lines lines chars w))
  (case k
    [(255) UNKNOWN-H]
    [(1) (+ (* 2 PAD) (* LH v) GAP (if (bitwise-bit-set? flags 2) (+ 8.0 TILE-H) 0.0))]
    [(2 7) (+ (if (zero? chars) 0.0 (+ (* LH v) 4.0 GAP))
              (if (bitwise-bit-set? flags 0) (* 5 LH-SMALL) 0.0)
              (if (bitwise-bit-set? flags 1) (+ 70.0 GAP) 0.0))]
    [(4) (+ 140.0 GAP)]
    [(5) (+ (* LH-SMALL v) GAP)]
    [(6 8 10) (+ LH-SMALL GAP)]
    [else 0.0])
) ; end define hint-estimate

(define (entry-estimate e w)
  (define (tl s) (vis-lines (for/sum ([c (in-string s)]) (if (char=? c #\newline) 1 0)) (string-length s) w))
  (+ GAP
     (cond [(e-user? e) (+ (* 2 PAD) (* LH (tl (e-user-text e))) (if (null? (e-user-media e)) 0.0 (+ 8.0 TILE-H)))]
           [(e-asst? e) (+ (* LH (tl (e-asst-text e))) (if (string=? (e-asst-reasoning e) "") 0.0 (* 5 LH-SMALL)) 4.0)]
           [(e-tool? e) 70.0]
           [(e-perm? e) 140.0]
           [else (* LH-SMALL (tl (e-note-text e)))]))
) ; end define entry-estimate

;; 槽里的显示条目（记录的 + 本地的）
(define (slot-entries tr s)
  (define r (sub1 s))
  (define rs (if (and (>= r 0) (rec-loaded? tr r)) (hash-ref (transcript-groups tr) r '()) '()))
  (define ls (hash-ref (transcript-local tr) s '()))
  (if (null? ls) rs (append rs ls))
) ; end define slot-entries

;; 槽内容变了（不在视口附近时）：按文字长度估；渲染时在视口附近的会量出实际高度
(define (estimate-slot! tr s)
  (set-transcript-ver! tr (add1 (transcript-ver tr)))
  (define r (sub1 s))
  (define w (transcript-width tr))
  (flvector-set! (transcript-sw tr) s 0.0)
  (set-slot-h! tr s (+ (if (or (< r 0) (rec-loaded? tr r)) 0.0 (hint-estimate tr r w))
                       (for/sum ([e (in-list (slot-entries tr s))]) (entry-estimate e w))))
) ; end define estimate-slot!

;; ---------------------------------------------------------------- 归约：条目 → 显示条目

(define (blank? s) (string=? (string-trim s) ""))
(define (parts-of it) (let ([p (j it 'parts '())]) (if (list? p) p '())))
(define (part-texts parts type) (string-join (for/list ([p (in-list parts)] #:when (equal? (j p 'type) type)) (let ([s (j p 'text "")]) (if (string? s) s ""))) ""))

;; 更新（工具结果 / 工具结束 / diff / 权限回答）：按键合并，作用到已装载的目标上；目标后装载时也能补上
(define (aux-update! tr rid key fields)
  (define h (hash-ref! (transcript-aux tr) key (lambda () (make-hasheqv))))
  (hash-set! h rid fields)
  (hash-update! (transcript-aux-src tr) rid (lambda (ks) (if (member key ks) ks (cons key ks))) '())
  (apply-aux! tr key)
) ; end define aux-update!

(define (aux-fields tr key)                            ; 各来源的字段，按记录序号合并（后写的覆盖先写的）
  (define h (hash-ref (transcript-aux tr) key #f))
  (if h
      (for/fold ([acc (hasheq)]) ([rid (in-list (sort (hash-keys h) <))])
        (for/fold ([a acc]) ([(k v) (in-hash (hash-ref h rid))]) (hash-set a k v)))
      (hasheq))
) ; end define aux-fields

(define (aborted-text? s) (and (string? s) (regexp-match? #rx"^(aborted by user|aborted before dispatch)" s)))

(define (apply-aux! tr key)
  (define f (aux-fields tr key))
  (case (car key)
    [(tool)
     (define e (hash-ref (transcript-tools tr) (cdr key) #f))
     (when e
       (when (hash-has-key? f 'ms) (set-e-tool-ms! e (let ([m (hash-ref f 'ms)]) (and (real? m) m))))
       (when (hash-has-key? f 'source) (set-e-tool-source! e (hash-ref f 'source)))
       (let ([d (hash-ref f 'diff #f)]) (when (and (hash? d) (string? (j d 'text))) (set-e-tool-diff! e d)))
       (let ([u (hash-ref f 'ui #f)]) (when (hash? u) (set-e-tool-ui! e u)))
       (cond
         [(hash-has-key? f 'content)                   ; 有结果
          (define c (hash-ref f 'content))
          (set-e-tool-output! e (if (string? c) c ""))
          (set-e-tool-total! e (hash-ref f 'total_len #f))
          (set-e-tool-rid! e (hash-ref f 'rid #f))
          (set-e-tool-truncated?! e (and (hash-ref f 'truncated #f) #t))
          (set-e-tool-status! e (cond [(and (hash-ref f 'is_error #f) (aborted-text? c)) 'aborted]
                                      [(hash-ref f 'is_error #f) 'error]
                                      [else 'ok]))]
         [(hash-has-key? f 'error) (set-e-tool-status! e (if (hash-ref f 'error) 'error 'ok))]))]
    [(perm)
     (define e (hash-ref (transcript-perms tr) (cdr key) #f))
     (when (and e (hash-has-key? f 'decision)) (set-e-perm-answer! e (hash-ref f 'decision)))])
) ; end define apply-aux!

(define (result-fields p rid)
  (hasheq 'content (j p 'content "") 'is_error (and (j p 'is_error) #t) 'total_len (j p 'total_len #f)
          'truncated (and (j p 'truncated) #t) 'rid rid)
) ; end define result-fields

(define (note-text it)
  (define code (j it 'code ""))
  (define text (let ([s (j it 'text "")]) (if (string? s) s "")))
  (define d (j it 'data))
  (define (who m) (format "~a/~a" (j m 'instance "?") (j m 'model "?")))
  (cond
    [(and (equal? code "route_switch") (hash? d))
     (define reason (j d 'reason))
     (if (equal? reason "manual")
         (t 'tr.route-pinned (j d 'route "?") (who (j d 'to)))
         (t 'tr.route-switch (j d 'route "?") (who (j d 'from))
            (let ([r (if (string? reason) reason "?")]) (if (> (string-length r) 60) (string-append (substring r 0 60) "…") r))
            (who (j d 'to))))]
    [(and (equal? code "auto") (hash? d)) (t 'tr.auto (j d 'model "?") (j d 'reasoning "?"))]
    [(and (equal? code "goal_start") (hash? d)) (t 'tr.goal (j d 'goal ""))]
    [(member code '("error" "budget_stop")) (t 'tr.error text)]
    [else text])
) ; end define note-text

(define (note-level it)
  (define l (j it 'level))
  (if (member l '("dim" "warn" "error" "info")) (string->symbol l) 'dim)
) ; end define note-level

(define (usage-text it)
  (define c (j it 'cost))
  (t 'tr.usage (j it 'model "?") (j it 'input 0) (j it 'output 0)
     (if (real? c) (format " · $~a" (/ (round (* c 10000)) 10000.0)) ""))
) ; end define usage-text

(define (make-tool! tr p)
  (define id (j p 'id))
  (define e (e-tool id (let ([n (j p 'name)]) (if (string? n) n "?")) (let ([i (j p 'input)]) (if (hash? i) i (hasheq)))
                    'running "" #f #f #f #f #f #f "builtin" #f #f #f))
  (hash-set! (transcript-tools tr) id e)
  (apply-aux! tr (cons 'tool id))
  e
) ; end define make-tool!

;; 一条条目 → 它的显示条目表（同时登记它带来的更新）
(define (item-entries! tr it)
  (define rid (j it 'id))
  (define parts (parts-of it))
  (case (j it 'kind)
    [("user")
     (for ([p (in-list parts)] #:when (equal? (j p 'type) "tool_result"))   ; 中断 / 工具额度提示里带的工具结果
       (aux-update! tr rid (cons 'tool (j p 'call_id)) (result-fields p rid)))
     (case (j it 'synthetic)
       [("aborted") (list (e-note (t 'tr.interrupted-history) 'dim))]
       [("summary") (list (e-note (t 'tr.summary-msg) 'dim))]
       [("notice") (list (e-note (t 'tr.tool-budget) 'warn))]
       [else
        (define text (part-texts parts "text"))
        (define imgs (for/list ([p (in-list parts)] #:when (equal? (j p 'type) "image")) (t 'tr.image (j p 'media_type ""))))   ; 旧会话的内联图片：只有占位
        (define all (string-join (filter (lambda (s) (not (string=? s ""))) (cons text imgs)) "\n"))
        (define media (filter (lambda (p) (and (equal? (j p 'type) "media") (string? (j p 'sha)))) parts))
        (if (and (string=? all "") (null? media)) '() (list (e-user all '() media)))])]
    [("assistant")
     (define text (part-texts parts "text"))
     (define reasoning (part-texts parts "thinking"))
     (define a (if (and (blank? text) (blank? reasoning)) '() (list (e-asst text reasoning #f #f))))
     (append a (for/list ([p (in-list parts)] #:when (equal? (j p 'type) "tool_call")) (make-tool! tr p)))]
    [("results")
     (for ([p (in-list parts)] #:when (equal? (j p 'type) "tool_result"))
       (aux-update! tr rid (cons 'tool (j p 'call_id)) (result-fields p rid)))
     '()]
    [("permission")
     (define e (e-perm (j it 'perm_id) (let ([s (j it 'prompt "")]) (if (string? s) s "")) (j it 'tool) (j it 'level) (j it 'input) #f))
     (hash-set! (transcript-perms tr) (j it 'perm_id) e)
     (apply-aux! tr (cons 'perm (j it 'perm_id)))
     (list e)]
    [("answer") (aux-update! tr rid (cons 'perm (j it 'perm_id)) (hasheq 'decision (let ([d (j it 'decision)]) (if (string? d) d "no")))) '()]
    [("note") (list (e-note (note-text it) (note-level it)))]
    [("usage") (list (e-note (usage-text it) 'usage))]
    [("tool")
     (aux-update! tr rid (cons 'tool (j it 'call_id))
                  (let* ([f (hasheq 'ms (j it 'ms) 'error (and (j it 'error) #t))]
                         [f (if (string? (j it 'source)) (hash-set f 'source (j it 'source)) f)]
                         [f (if (hash? (j it 'diff)) (hash-set f 'diff (j it 'diff)) f)]
                         [f (if (hash? (j it 'ui)) (hash-set f 'ui (j it 'ui)) f)])
                    f))
     '()]
    [("diff") (when (hash? (j it 'diff)) (aux-update! tr rid (cons 'tool (j it 'call_id)) (hasheq 'diff (j it 'diff)))) '()]
    [("partial")
     (define text (let ([s (j it 'text "")]) (if (string? s) s "")))
     (define reasoning (let ([s (j it 'reasoning "")]) (if (string? s) s "")))
     (if (and (blank? text) (blank? reasoning)) '() (list (e-asst text reasoning #f #t)))]
    [("tests")                                                     ; 工作区测试流程（design-goalmode.md §12）：运行 / 验收报告 / 修改提议 / 处理结果
     (define steps-text (string-join (for/list ([r (let ([v (j it 'steps '())]) (if (list? v) v '()))])
                                       (format "~a ~a" (if (eq? (j r 'ok) #t) "✓" "✗") (j r 'step "?")))
                                     "  "))
     (define pass? (eq? (j it 'ok) #t))
     (case (j it 'kind "run")
       [("acceptance") (list (e-note (t 'tr.tests-acceptance (if pass? "PASS" "FAIL") steps-text) (if pass? 'info 'warn)))]
       [("proposal") (list (e-note (t 'tr.tests-proposal (let ([r (j it 'reason)]) (if (string? r) r ""))) 'warn))]
       [("decision") (list (e-note (t 'tr.tests-decision (j it 'decision "?") (let ([r (j it 'reason)]) (if (string? r) r ""))) 'info))]
       [else (list (e-note (t 'tr.tests-run (if pass? "PASS" "FAIL") steps-text) 'dim))])]
    [("compact")
     (list (e-note (cond [(equal? (j it 'kind) "prune") (t 'tr.pruned (j it 'pruned 0) (quotient (let ([v (j it 'saved 0)]) (if (real? v) (inexact->exact (round v)) 0)) 1000))]
                         [(eqv? (j it 'count) 0) (t 'tr.cleared)]
                         [else (t 'tr.compacted (j it 'count 0))])
                   'info))]
    [("sub")
     ;; 子 agent 的一条记录（本机的，或远端拉回的副本）：同样投影，显示条目挂到父工具卡片下面
     (define inner (j it 'inner))
     (define parent (j it 'parent))
     (when (and (hash? inner) (string? parent))
       (define es (item-entries! tr (hash-set (hash-set inner 'id rid) 'type "item")))
       (hash-set! (hash-ref! (transcript-children tr) parent make-hasheqv) rid es)
       (hash-update! (transcript-aux-src tr) rid (lambda (ks) (cons (cons 'child parent) ks)) '()))
     '()]
    [else '()])
) ; end define item-entries!

;; 工具卡片下的子 agent 条目（按记录顺序）
(define (tool-children tr e)
  (define h (hash-ref (transcript-children tr) (e-tool-id e) #f))
  (if h (append* (for/list ([rid (in-list (sort (hash-keys h) <))]) (hash-ref h rid))) '()))

;; 条目进表（实时 / 翻页同一入口）。已有显示条目的记录不重复加（翻页与实时的重叠、守护进程重放）。
;; live?：实时到达的用户消息接管本地那条（保留 chip）
(define (add-item! tr it #:live? [live? #f])
  (define rid (j it 'id))
  (when (exact-nonnegative-integer? rid)
    (when (>= rid (transcript-total tr)) (extend-total! tr (add1 rid) #:loaded? #t))   ; 实时：中间没显示的记录都是不显示的
    (unless (pair? (hash-ref (transcript-groups tr) rid '()))
      (define es0 (item-entries! tr it))
      (define es
        (cond
          [(and live? (equal? (j it 'kind) "user") (not (j it 'synthetic)) (pair? es0) (pair? (transcript-pending tr)))
           (define pr (car (transcript-pending tr)))
           (set-transcript-pending! tr (cdr (transcript-pending tr)))
           (hash-update! (transcript-local tr) (car pr) (lambda (ls) (remq (cdr pr) ls)) '())
           (estimate-slot! tr (car pr))
           (define local (cdr pr))
           ;; 本地那条（带 chip）接管记录的；媒体段以记录的为准（sha 与本地算的相同）
           (cons (if (and (e-user? (car es0)) (pair? (e-user-media (car es0))))
                     (e-user (e-user-text local) (e-user-folds local) (e-user-media (car es0)))
                     local)
                 (cdr es0))]
          [else es0]))
      (hash-set! (transcript-groups tr) rid es)
      (bytes-set! (transcript-state tr) rid 2)
      (hash-set! (transcript-loaded-pages tr) (page-of rid) #t)
      (estimate-slot! tr (add1 rid))
      ;; 定稿的回答 / 被中断的部分到了：流式条目作废
      (when (and live? (member (j it 'kind) '("assistant" "partial"))) (set-transcript-stream! tr #f))))
) ; end define add-item!

;; 一段记录 [from, to) 装载完成：其中没有条目的记录是不显示的
(define (load-range! tr from to items)
  (extend-total! tr to)
  (for ([it (in-list items)]) (add-item! tr it))
  (for ([r (in-range from (min to (transcript-total tr)))] #:unless (rec-loaded? tr r))
    (bytes-set! (transcript-state tr) r 2)
    (hash-set! (transcript-loaded-pages tr) (page-of r) #t)
    (estimate-slot! tr (add1 r)))
  (for ([p (in-range (page-of from) (add1 (page-of (max from (sub1 to)))))]) (hash-remove! (transcript-requested tr) p))
) ; end define load-range!

;; 逐出一页：显示条目、它们登记的工具 / 权限、它们贡献的更新都删掉；高度留着
(define (evict-page! tr p)
  (for ([r (in-range (* p PAGE) (min (transcript-total tr) (* (add1 p) PAGE)))] #:when (rec-loaded? tr r))
    (for ([e (in-list (hash-ref (transcript-groups tr) r '()))])
      (cond [(and (e-tool? e) (eq? (hash-ref (transcript-tools tr) (e-tool-id e) #f) e)) (hash-remove! (transcript-tools tr) (e-tool-id e))]
            [(and (e-perm? e) (eq? (hash-ref (transcript-perms tr) (e-perm-id e) #f) e)) (hash-remove! (transcript-perms tr) (e-perm-id e))]))
    (hash-remove! (transcript-groups tr) r)
    (for ([k (in-list (hash-ref (transcript-aux-src tr) r '()))] #:when (eq? (car k) 'child))
      (define h (hash-ref (transcript-children tr) (cdr k) #f))
      (when h (hash-remove! h r)))
    (for ([k (in-list (hash-ref (transcript-aux-src tr) r '()))] #:unless (eq? (car k) 'child))
      (define h (hash-ref (transcript-aux tr) k #f))
      (when h (hash-remove! h r) (when (zero? (hash-count h)) (hash-remove! (transcript-aux tr) k))))
    (hash-remove! (transcript-aux-src tr) r)
    (bytes-set! (transcript-state tr) r 0))
  (hash-remove! (transcript-loaded-pages tr) p)
) ; end define evict-page!

;; 装载的页太多：逐出离视口最远的（尾部两页不逐出：实时内容在那里）
(define (maybe-evict! tr)
  (define pages (hash-keys (transcript-loaded-pages tr)))
  (when (> (length pages) MAX-PAGES)
    (define here (page-of (max 0 (sub1 (transcript-anchor tr)))))
    (define last-p (page-of (max 0 (sub1 (transcript-total tr)))))
    (define cands (sort (filter (lambda (p) (< p (sub1 last-p))) pages) > #:key (lambda (p) (abs (- p here)))))
    (for ([p (in-list cands)] [_ (in-range (- (length pages) MAX-PAGES))]) (evict-page! tr p)))
) ; end define maybe-evict!

(define (reset! tr)
  (define fresh (make-transcript (transcript-rc tr) (transcript-nonce tr)))
  (hash-clear! (transcript-blobs tr))
  (hash-clear! (transcript-children tr))
  (for ([acc (list set-transcript-total! set-transcript-cap! set-transcript-state! set-transcript-hints! set-transcript-sh!
                   set-transcript-sw! set-transcript-fen!)]
        [get (list transcript-total transcript-cap transcript-state transcript-hints transcript-sh transcript-sw transcript-fen)])
    (acc tr (get fresh)))
  (for ([h (list (transcript-groups tr) (transcript-local tr) (transcript-tools tr) (transcript-perms tr) (transcript-aux tr)
                 (transcript-aux-src tr) (transcript-loaded-pages tr) (transcript-requested tr))])
    (hash-clear! h))
  (set-transcript-pending! tr '())
  (set-transcript-out! tr '())
  (set-transcript-gen! tr (add1 (transcript-gen tr)))
  (set-transcript-stream! tr #f)
  (set-transcript-anchor! tr 0) (set-transcript-offset! tr 0.0) (set-transcript-scroll! tr 0.0)
  (set-transcript-stick?! tr #t)
  (transcript-clear-selection! tr)
) ; end define reset!

(define (add-local! tr e)
  (define s (transcript-total tr))
  (hash-update! (transcript-local tr) s (lambda (ls) (append ls (list e))) '())
  (estimate-slot! tr s)
  s
) ; end define add-local!

;; media：发出的附件（kind media_type sha name meta bytes）：图片立刻交给解码（缩略图），音频字节留着播放
(define (transcript-add-user! tr text folds [media '()])
  (for ([m (in-list media)] #:when (bytes? (j m 'bytes)))
    (case (j m 'kind)
      [("image") (let ([mp (transcript-media tr)]) (when mp (mp 'decode! (j m 'sha) (j m 'bytes))))]
      [("audio") (hash-set! (transcript-audio-bytes tr) (j m 'sha) (j m 'bytes))]
      [else (void)]))
  (define e (e-user text folds (for/list ([m (in-list media)]) (hash-remove (hash-set m 'type "media") 'bytes))))
  (define s (add-local! tr e))
  (set-transcript-pending! tr (append (transcript-pending tr) (list (cons s e))))
  (set-transcript-stick?! tr #t)
) ; end define transcript-add-user!

(define (transcript-note! tr text [level 'dim]) (add-local! tr (e-note text level)) (void))

;; 请求带标签（内核原样带回）：守护进程把应答广播给所有客户端，别的客户端 / 旧会话的应答按标签认出来丢掉
(define (tag-of tr) (format "~a:~a" (transcript-nonce tr) (transcript-gen tr)))
(define (want! tr req) (set-transcript-out! tr (cons (hash-set req 'tag (tag-of tr)) (transcript-out tr))))
(define (ours? tr ev) (equal? (j ev 'tag) (tag-of tr)))

(define (stream-entry tr)
  (or (transcript-stream tr) (let ([e (e-asst "" "" #t #f)]) (set-transcript-stream! tr e) e))
) ; end define stream-entry

;; 内核事件 → 显示条目。返回 #t 表示有可见变化。
(define (transcript-apply! tr ev)
  (case (j ev 'type)
    [("item") (add-item! tr ev #:live? #t) #t]
    [("items")
     (define from (j ev 'from 0)) (define to (j ev 'to 0))
     (when (and (ours? tr ev) (exact-nonnegative-integer? from) (exact-nonnegative-integer? to))
       (load-range! tr from to (let ([is (j ev 'items '())]) (if (list? is) is '())))
       (maybe-evict! tr))
     #t]
    [("item_full")
     (hash-remove! (transcript-requested tr) (cons 'item (j ev 'id)))
     (for ([p (in-list (if (ours? tr ev) (parts-of ev) '()))] #:when (equal? (j p 'type) "tool_result"))
       (define e (hash-ref (transcript-tools tr) (j p 'call_id) #f))
       (when e
         (set-e-tool-output! e (let ([c (j p 'content "")]) (if (string? c) c "")))
         (set-e-tool-truncated?! e #f)
         (set-e-tool-fetching?! e #f)))
     #t]
    [("blob_data")
     ;; blob 请求的应答（分块）：收齐 → 图片交给解码，音频留着播放
     (define sha (j ev 'sha))
     (define b (and (ours? tr ev) (hash-ref (transcript-blobs tr) sha #f)))
     (when b
       (define data (and (string? (j ev 'data)) (base64-decode (string->bytes/latin-1 (j ev 'data)))))
       (cond
         [(or (j ev 'error) (not data) (not (exact-nonnegative-integer? (j ev 'total))))
          (hash-remove! (transcript-blobs tr) sha) (hash-set! (transcript-failed tr) sha #t)]
         [else
          (define got (+ (vector-ref b 1) (bytes-length data)))
          (define total (j ev 'total))
          (define chunks (cons data (vector-ref b 0)))
          (cond
            [(and (< got total) (> (bytes-length data) 0))
             (hash-set! (transcript-blobs tr) sha (vector chunks got total (vector-ref b 3)))
             (want! tr (hasheq 'type "blob" 'sha sha 'offset got 'length BLOB-CHUNK))]
            [else
             (hash-remove! (transcript-blobs tr) sha)
             (define bs (apply bytes-append (reverse chunks)))
             (case (vector-ref b 3)
               [(image) (let ([mp (transcript-media tr)]) (when mp (mp 'decode! sha bs)))]
               [(audio) (hash-set! (transcript-audio-bytes tr) sha bs)
                        (when (equal? (transcript-play-pending tr) sha)
                          (set-transcript-play-pending! tr #f)
                          (let ([mp (transcript-media tr)]) (when mp (mp 'play! sha bs))))])])]))
     #t]
    [("lines")
     ;; select_lines 的应答：本机小模型挑的行（1 起）或出错（→ 退回按模式显示）
     (define e (and (ours? tr ev) (hash-ref (transcript-tools tr) (j ev 'call_id) #f)))
     (when e
       (define ls (j ev 'lines))
       (set-e-tool-picked! e (if (and (list? ls) (pair? ls) (andmap exact-positive-integer? ls)) (map sub1 ls) 'failed)))
     #t]
    [("index_summary")
     (define from (j ev 'from 0))
     (define bs (with-handlers ([exn:fail? (lambda (_e) #"")]) (base64-decode (string->bytes/latin-1 (j ev 'data "")))))
     (hash-remove! (transcript-requested tr) (cons 'sum from))
     (when (and (ours? tr ev) (exact-nonnegative-integer? from))
       (define n (quotient (bytes-length bs) 6))
       (extend-total! tr (+ from n))
       (for ([k (in-range n)])
         (define r (+ from k))
         (bytes-copy! (transcript-hints tr) (* 6 r) bs (* 6 k) (* 6 (add1 k)))
         (unless (rec-loaded? tr r) (estimate-slot! tr (add1 r)))))
     #t]
    [("session")
     (reset! tr)
     (define total (let ([n (j ev 'total)]) (if (exact-nonnegative-integer? n) n 0)))
     (define from (let ([n (j ev 'tail_from)]) (if (exact-nonnegative-integer? n) (min n total) 0)))
     (extend-total! tr total)
     (load-range! tr from total (let ([is (j ev 'items '())]) (if (list? is) is '())))
     (for ([a (in-range 0 from SUMMARY-CHUNK)])                       ; 更早的记录：先要摘要（估高），内容按需翻页
       (want! tr (hasheq 'type "index_summary" 'from a 'to (min from (+ a SUMMARY-CHUNK))))
       (hash-set! (transcript-requested tr) (cons 'sum a) (current-inexact-milliseconds)))
     (set-transcript-turns! tr (let ([n (j ev 'turns)]) (if (exact-nonnegative-integer? n) n 0)))
     (transcript-note! tr (t 'tr.session (regexp-replace #rx"^.*/" (format "~a" (j ev 'path "")) "") (j ev 'messages 0)) 'dim)
     #t]
    [("turn_start") (set-transcript-busy?! tr #t) #t]
    [("delta")
     ;; kind：text（回答）/ thinking（推理）/ tool-json（流式工具参数，工具卡片随 assistant 条目给出，这里忽略）
     (case (j ev 'kind)
       [("text") (define e (stream-entry tr)) (set-e-asst-text! e (string-append (e-asst-text e) (j ev 'text ""))) #t]
       [("thinking" "reasoning") (define e (stream-entry tr)) (set-e-asst-reasoning! e (string-append (e-asst-reasoning e) (j ev 'text ""))) #t]
       [else #f])]
    [("turn_complete" "goal_end")
     ;; 中断语义（与内核一致）：流到一半的文字不进模型上下文，以 partial 条目显示（灰）；没跑完的工具由内核补了 aborted 结果
     (set-transcript-stream! tr #f)
     (set-transcript-busy?! tr #f)
     (set-transcript-turns! tr (add1 (transcript-turns tr)))
     #t]
    [("error")
     ;; 轮里的错误内核记成提示（note 条目）；轮外的（请求不合法、续接失败…）只在这里
     (unless (transcript-busy? tr) (transcript-note! tr (t 'tr.error (j ev 'message "?")) 'error))
     #t]
    [("note") (transcript-note! tr (j ev 'text "") (note-level ev)) #t]     ; 守护进程的说明（不是记录）
    [("ready")
     (define p (j ev 'protocol))
     (when (and (exact-integer? p) (< p 3)) (transcript-note! tr (t 'tr.old-protocol p) 'error))
     #f]
    [("core_exit")
     (set-transcript-busy?! tr #f)
     (set-transcript-stream! tr #f)
     (transcript-note! tr (if (j ev 'remote) (t 'tr.remote-closed (j ev 'code "?")) (t 'tr.core-exit (j ev 'code "?")))
                       (if (and (eqv? (j ev 'code) 0) (not (j ev 'remote))) 'dim 'error))
     #t]
    [else #f])
) ; end define transcript-apply!

;; 权限卡片的回答：没答的在轮进行中有按钮；轮结束（或没在跑）还没答 = 作废
(define (perm-answer tr e) (or (e-perm-answer e) (and (not (transcript-busy? tr)) "cancelled")))

;; 用户点了权限按钮：标记已答，返回要发给内核的请求
(define (transcript-answer! tr id decision)
  (define e (hash-ref (transcript-perms tr) id #f))
  (and e (not (perm-answer tr e))
       (begin (set-e-perm-answer! e decision)
              (hasheq 'type "permission" 'id id 'decision decision)))
) ; end define transcript-answer!

;; ---------------------------------------------------------------- 媒体

(define BLOB-CHUNK (* 512 1024))
(define MAX-MEDIA-FETCH (* 64 1024 1024))              ; 比这大的媒体不取（块上只显示名字与大小）

;; 还没取的 blob → 发第一块的请求（用途：image 解码 / audio 播放）
(define (ensure-blob! tr sha purpose size)
  (unless (or (hash-ref (transcript-blobs tr) sha #f) (hash-ref (transcript-failed tr) sha #f)
              (and (real? size) (> size MAX-MEDIA-FETCH)))
    (hash-set! (transcript-blobs tr) sha (vector '() 0 #f purpose))
    (want! tr (hasheq 'type "blob" 'sha sha 'offset 0 'length BLOB-CHUNK)))
) ; end define ensure-blob!

;; 点了音频块的 ▶ / ❚❚
(define (transcript-toggle-play! tr sha)
  (define mp (transcript-media tr))
  (when mp
    (define st (mp 'audio sha))
    (define bs (hash-ref (transcript-audio-bytes tr) sha #f))
    (cond [(and st (vector-ref st 0)) (mp 'pause! sha)]
          [st (mp 'resume! sha)]
          [bs (mp 'play! sha bs)]
          [else (set-transcript-play-pending! tr sha) (ensure-blob! tr sha 'audio #f)]))
) ; end define transcript-toggle-play!

;; 媒体块的显示尺寸（point）：图片按原图比例放进 TILE-W × TILE-H（不超过原图像素的一半：2× 屏上一像素对一像素）；音频条；文件块
(define TILE-W 240.0)
(define TILE-H 180.0)
(define (media-box p)
  (define m (let ([v (j p 'meta)]) (if (hash? v) v (hasheq))))
  (case (j p 'kind)
    [("image")
     (define ow (j m 'width #f)) (define oh (j m 'height #f))
     (if (and (real? ow) (real? oh) (> ow 0) (> oh 0))
         (let ([k (min (/ TILE-W ow) (/ TILE-H oh) 0.5)]) (values (max 24.0 (* k ow)) (max 24.0 (* k oh))))
         (values TILE-W 135.0))]
    [("audio") (values 300.0 40.0)]
    [else (values 240.0 36.0)])
) ; end define media-box

(define (fmt-time s) (let* ([s (max 0 (inexact->exact (floor (if (real? s) s 0))))]) (format "~a:~a" (quotient s 60) (let ([r (remainder s 60)]) (if (< r 10) (format "0~a" r) r)))))
(define (fmt-bytes n) (cond [(not (real? n)) ""] [(< n 1024) (format "~a B" n)] [(< n 1048576) (format "~a KB" (round (/ n 1024)))] [else (format "~a MB" (/ (round (/ n 104857.6)) 10.0))]))

;; 媒体块排成行（放不下换行）→ 高度
(define (draw-media! tr parts dl x0 y0 inner scale paint? hit!)
  (define rc (transcript-rc tr))
  (define lay (rich-ctx-layout rc))
  (define mp (transcript-media tr))
  (define (label! s x y w color)
    (define tb (lay s 'small w #f))
    (when paint? (paint-text! dl (rich-ctx-atlas rc) tb x y color #:scale scale))
    (text-block-height tb))
  (define-values (_x _y row-h)
    (for/fold ([x x0] [y y0] [rh 0.0]) ([p (in-list parts)])
      (define-values (bw bh) (media-box p))
      (define-values (bx by) (if (and (> x x0) (> (+ x bw) (+ x0 inner))) (values x0 (+ y rh 8.0)) (values x y)))
      (define rh* (if (= by y) rh 0.0))
      (define sha (j p 'sha))
      (define name (let ([n (j p 'name)]) (if (string? n) n (j p 'media_type "?"))))
      (define meta (let ([v (j p 'meta)]) (if (hash? v) v (hasheq))))
      (case (j p 'kind)
        [("image")
         (define th (and mp (mp 'thumb sha)))
         (cond
           [(and th paint?) (drawlist-image! dl bx by bw bh (vector-ref th 0) (vector-ref th 1) (vector-ref th 2) (vector-ref th 3) #:radius 6.0)]
           [paint?
            (drawlist-rect! dl bx by bw bh 0.16 0.17 0.20 1.0 #:radius 6.0)
            (define st (cond [(hash-ref (transcript-failed tr) sha #f) 'failed] [mp (mp 'status sha)] [else 'none]))
            (when (and mp (eq? st 'none)) (ensure-blob! tr sha 'image (j meta 'bytes #f)))
            (label! (format "~a · ~a" name (if (eq? st 'failed) (t 'tr.media-failed) (t 'tr.loading))) (+ bx 8.0) (+ by 8.0) (- bw 16.0) DIM)]
           [else (void)])]
        [("audio")
         (define st (and mp (mp 'audio sha)))
         (define playing? (and st (vector-ref st 0)))
         (define dur (or (and st (vector-ref st 2)) (j meta 'duration #f)))
         (when paint?
           (drawlist-rect! dl bx by bw bh 0.16 0.17 0.20 1.0 #:radius 8.0)
           (drawlist-rect! dl (+ bx 6.0) (+ by 6.0) 28.0 28.0 0.24 0.36 0.62 1.0 #:radius 14.0)
           (label! (if playing? "❚❚" "▶") (+ bx 13.0) (+ by 12.0) 20.0 TEXT)
           ;; 进度条
           (when (and st (real? dur) (> dur 0))
             (drawlist-rect! dl (+ bx 42.0) (+ by bh -6.0) (* (- bw 50.0) (min 1.0 (/ (vector-ref st 1) dur))) 2.0 0.45 0.62 0.95 1.0))
           (label! (format "~a · ~a~a" name (if st (string-append (fmt-time (vector-ref st 1)) " / ") "") (if (real? dur) (fmt-time dur) "?"))
                   (+ bx 42.0) (+ by 12.0) (- bw 50.0) OUT))
         (hit! 'media-play sha bx by bw bh)]
        [else
         (when paint?
           (drawlist-rect! dl bx by bw bh 0.16 0.17 0.20 1.0 #:radius 6.0)
           (label! (format "📎 ~a · ~a" name (fmt-bytes (j meta 'bytes #f))) (+ bx 10.0) (+ by 10.0) (- bw 20.0) OUT))])
      (values (+ bx bw 8.0) by (max rh* bh))))
  (+ (- _y y0) row-h)
) ; end define draw-media!

;; 展开 / 收起工具卡片；展开时输出只是预览 → 要全文
(define (transcript-toggle-tool! tr id)
  (define e (hash-ref (transcript-tools tr) id #f))
  (when e
    (set-e-tool-expanded?! e (not (e-tool-expanded? e)))
    (when (and (e-tool-expanded? e) (e-tool-truncated? e) (exact-nonnegative-integer? (e-tool-rid e)) (not (e-tool-fetching? e)))
      (set-e-tool-fetching?! e #t)
      (hash-set! (transcript-requested tr) (cons 'item (e-tool-rid e)) (current-inexact-milliseconds))
      (want! tr (hasheq 'type "item" 'id (e-tool-rid e)))))
) ; end define transcript-toggle-tool!

(define (loaded-entries tr)                            ; 已装载的显示条目，按显示顺序（测试 / 统计用）
  (append (for*/list ([s (in-range (nslots tr))]
                      #:when (or (= s 0) (rec-loaded? tr (sub1 s)) (pair? (hash-ref (transcript-local tr) s '())))
                      [e (in-list (slot-entries tr s))])
            e)
          (if (transcript-stream tr) (list (transcript-stream tr)) '()))
) ; end define loaded-entries

(define (transcript-pending-permissions tr)
  (if (transcript-busy? tr)
      (sort (for/list ([e (in-hash-values (transcript-perms tr))] #:unless (e-perm-answer e)) (e-perm-id e))
            (lambda (a b) (if (and (real? a) (real? b)) (< a b) (string<? (format "~a" a) (format "~a" b)))))
      '())
) ; end define transcript-pending-permissions

;; 交出本帧想要的请求（翻页 / 摘要 / 全文），并清空
(define (transcript-wanted! tr)
  (define out (reverse (transcript-out tr)))
  (set-transcript-out! tr '())
  out
) ; end define transcript-wanted!

;; ---------------------------------------------------------------- 工具输出预览（折叠时显示哪几行）

;; 配置（gui.rktd 'tool-preview；设置页「工具输出」）：
;;   'lines n           折叠时显示几行（缺省 5）
;;   'mode              head | tail | head_tail | match（先显示匹配 'keep 的行，不够再补开头）| smart（本机小模型挑）
;;   'keep / 'drop      正则：match 模式优先的行 / 预览里不显示的行
;;   'tools             (hash 工具名 → 同样的键)：按工具覆盖
;;   'model 'endpoint 'allow_remote 'min_lines   smart 用的本地模型（缺省：本机 LM Studio 已加载的第一个）；输出至少 min_lines 行才请模型挑
(define DEFAULT-PREVIEW (hasheq 'lines 5 'mode "head"))
(define PREVIEW-MODES '("head" "tail" "head_tail" "match" "smart"))

(define rx-cache (make-hash))
(define (rx-of s) (and (string? s) (not (string=? s "")) (hash-ref! rx-cache s (lambda () (with-handlers ([exn:fail? (lambda (_e) #f)]) (pregexp s))))))

;; → (values 行数 模式 keep正则 drop正则)
(define (preview-spec tr name)
  (define cfg (transcript-preview tr))
  (define per (let ([ts (j cfg 'tools)]) (and (hash? ts) (or (hash-ref ts name #f) (hash-ref ts (string->symbol name) #f)))))
  (define (pick k d) (let ([v (j per k)]) (if v v (or (j cfg k) d))))
  (values (let ([n (pick 'lines 5)]) (if (exact-nonnegative-integer? n) n 5))
          (let ([m (pick 'mode "head")]) (if (member m PREVIEW-MODES) m "head"))
          (rx-of (pick 'keep #f))
          (rx-of (pick 'drop #f)))
) ; end define preview-spec

;; 输出 → (values 显示的文字 省略的行数)。中间、开头跳过的行用「⋯ k ⋯」一行标出；末尾省略的由页脚说明
(define (preview-lines s n mode #:keep [keep #f] #:drop [drop-rx #f] #:picked [picked #f])
  (define ls0 (string-split s "\n" #:trim? #f))
  (define ls (list->vector (if (and (pair? ls0) (string=? (last ls0) "")) (drop-right ls0 1) ls0)))
  (define k (vector-length ls))
  (define cand (for/list ([i (in-range k)] #:unless (and drop-rx (regexp-match? drop-rx (vector-ref ls i)))) i))
  (define (first-n xs m) (take xs (min m (length xs))))
  (define chosen
    (cond
      [(<= k n) (range k)]
      [(and (list? picked) (pair? picked)) (sort (filter (lambda (i) (< -1 i k)) picked) <)]
      [(equal? mode "tail") (take-right cand (min n (length cand)))]
      [(equal? mode "head_tail")
       (define h (quotient (add1 n) 2))
       (if (<= (length cand) n) cand (append (take cand h) (take-right cand (- n h))))]
      [(and (equal? mode "match") keep)
       (define m (first-n (filter (lambda (i) (regexp-match? keep (vector-ref ls i))) cand) n))
       (sort (append m (first-n (filter (lambda (i) (not (memv i m))) cand) (- n (length m)))) <)]
      [else (first-n cand n)]))
  (define out
    (let loop ([prev -1] [is chosen] [acc '()])
      (cond [(null? is) (reverse acc)]
            [else
             (define i (car is))
             (define gap (- i prev 1))
             (loop i (cdr is) (cons (vector-ref ls i) (if (> gap 0) (cons (format "⋯ ~a ⋯" gap) acc) acc)))])))
  (values (string-join out "\n") (- k (length chosen)))
) ; end define preview-lines

;; ---------------------------------------------------------------- 排版辅助

(define WARN '(0.95 0.75 0.35 1.0))
(define ERR '(0.95 0.45 0.45 1.0))
(define OUT '(0.78 0.80 0.84 1.0))
(define PARA-CACHE-MAX 3000)

;; 文本按换行分段排版；完整段（后面还有换行）进缓存，最后一段（流式中会变）不进
(define (para-blocks tr str font w)
  (define cache (transcript-para-cache tr))
  (when (> (hash-count cache) PARA-CACHE-MAX) (hash-clear! cache))
  (define lay (rich-ctx-layout (transcript-rc tr)))
  (define paras (string-split str "\n" #:trim? #f))
  (for/list ([p (in-list paras)] [i (in-naturals)])
    (define last? (= i (sub1 (length paras))))
    (if last?
        (lay p font w #f)
        (hash-ref! cache (list p font w) (lambda () (lay p font w #f)))))
) ; end define para-blocks

(define (blocks-height bs) (for/sum ([b (in-list bs)]) (text-block-height b)))

(define (paint-blocks! dl tr bs x y color scale)
  (for/fold ([yy y]) ([b (in-list bs)])
    (paint-text! dl (rich-ctx-atlas (transcript-rc tr)) b x yy color #:scale scale)
    (+ yy (text-block-height b)))
) ; end define paint-blocks!

;; 带 ANSI 颜色的段落（工具输出，design-gui.md §30）：spans 是纯文字（各段按 "\n" 拼起来）上的 (vector 起 止 前景 背景)。
;; 先画背景色块（按行的字形位置），再按字取前景色；颜色调到卡片上看得清（≥ 4.5）
(define CARD-BG '(0.16 0.16 0.18))
(define (paint-ansi-blocks! dl tr bs spans x y color scale)
  (for/fold ([yy y] [off 0] #:result yy) ([b (in-list bs)])
    (define len (string-length (text-block-source b)))
    (define mine (for/list ([sp (in-list spans)] #:when (and (< (vector-ref sp 0) (+ off len)) (> (vector-ref sp 1) off))) sp))
    (define lh (text-block-line-height b))
    (for ([sp (in-list mine)] #:when (vector-ref sp 3))                    ; 背景
      (define bg (dark-bg (vector-ref sp 3) CARD-BG))
      (for ([ln (in-list (text-block-lines b))] [k (in-naturals)])
        (define a (max (line-start ln) (- (vector-ref sp 0) off)))
        (define z (min (line-end ln) (- (vector-ref sp 1) off)))
        (when (< a z)
          (define x0 (line-x-of ln a)) (define x1 (line-x-of ln z))
          (drawlist-rect! dl (+ x x0) (+ yy (* k lh)) (max 1.0 (- x1 x0)) lh (car bg) (cadr bg) (caddr bg) 1.0))))
    (define (color-at c)
      (define g (+ off c))
      (define sp (for/first ([sp (in-list mine)] #:when (and (<= (vector-ref sp 0) g) (< g (vector-ref sp 1)))) sp))
      (cond [(and sp (vector-ref sp 2)) (append (readable-fg (vector-ref sp 2) (if (vector-ref sp 3) (dark-bg (vector-ref sp 3) CARD-BG) CARD-BG)) '(1.0))]
            [sp (append (readable-fg (take color 3) (if (vector-ref sp 3) (dark-bg (vector-ref sp 3) CARD-BG) CARD-BG)) '(1.0))]
            [else color]))
    (paint-text! dl (rich-ctx-atlas (transcript-rc tr)) b x yy color #:scale scale #:color-at (and (pair? mine) color-at))
    (values (+ yy (text-block-height b)) (+ off len 1)))
) ; end define paint-ansi-blocks!

(define (head-lines s n)                               ; 前 n 行 + 剩余行数
  (define ls (string-split s "\n" #:trim? #f))
  (define ls* (if (and (pair? ls) (string=? (last ls) "")) (drop-right ls 1) ls))
  (values (string-join (take ls* (min n (length ls*))) "\n") (max 0 (- (length ls*) n)))
) ; end define head-lines

(define (tool-summary name input)
  (define (s k) (let ([v (j input k)]) (and (string? v) v)))
  (define raw
    (or (and (member name '("bash" "bash_persistent")) (s 'command))
        (s 'path) (s 'file_path) (s 'pattern)
        (let ([a (j input 'args)]) (and (list? a) (string-join (map (lambda (x) (format "~a" x)) a) " ")))
        (with-handlers ([exn:fail? (lambda (_e) "")]) (jsexpr->string input))))
  (truncate-graphemes (regexp-replace* #rx"[\n\t ]+" raw " ") 90)
) ; end define tool-summary

;; 权限卡片正文：输入的各字段逐行「键: 值」，多行值缩进续行（例如 edit_file 的 old_string / new_string）
(define (input->text input)
  (string-join
   (for/list ([k (in-list (sort (hash-keys input)
                                (lambda (a b)
                                  (define (rank x) (or (index-of '(path file_path command pattern old_string new_string) x) 99))
                                  (if (= (rank a) (rank b)) (string<? (symbol->string a) (symbol->string b)) (< (rank a) (rank b))))))]
              #:when #t
              [v (in-value (hash-ref input k))])
     (define sv (cond [(string? v) v]
                      ;; 测试流程的信任 / 验收测试的确认：步骤逐行「id: $ 命令」（不是一串 JSON）
                      [(and (eq? k 'steps) (list? v) (andmap hash? v))
                       (string-join (for/list ([st v]) (format "~a: $ ~a" (hash-ref st 'id "?") (hash-ref st 'command (hash-ref st 'cmd "?")))) "\n")]
                      [(and (eq? k 'files) (list? v) (andmap string? v)) (string-join v "\n")]
                      [else (with-handlers ([exn:fail? (lambda (_e) (format "~a" v))]) (jsexpr->string v))]))
     (if (regexp-match? #rx"\n" sv)
         (format "~a:\n  ~a" k (string-replace sv "\n" "\n  "))
         (format "~a: ~a" k sv)))
   "\n")
) ; end define input->text

(define (permission-level-label l)
  (case l [("dangerous") (t 'tr.level.dangerous)] [("mutating") (t 'tr.level.mutating)] [("read-only") (t 'tr.level.read-only)]
          [("trust") (t 'tr.level.trust)] [("approve") (t 'tr.level.approve)] [else (or l "?")])
) ; end define permission-level-label

;; ---------------------------------------------------------------- 条目绘制

(define PAD 14.0)
(define GAP 10.0)
(define EXPANDED-LINES 400)
(define COMMITS-COLLAPSED 8)                           ; git_history 的卡片收起时画几行提交

;; git_history 的提交行：hash（次要色）· TYPE 标记 · 标题（一行，放不下截断）；有标签 → 下一行一串标签片（放不下换行）。
;; 颜色只用已有的：TYPE 标记用 chip 的种类色（rich.rkt KIND-COLORS 的同一组值，配浅色字已过对比度检查），普通标签用中性的 chip 灰，
;; 自动化提交的 pi2 标签用「运行中」角标的实心浅蓝 + 深色字（palette.rkt BADGE-BUSY），一眼区分人写的和自动化的。
(define TYPE-COLORS (hash "FEAT" '(0.17 0.33 0.24) "FIX" '(0.42 0.20 0.22) "REFACTOR" '(0.29 0.22 0.42)
                          "TEST" '(0.17 0.28 0.37) "DOCS" '(0.38 0.31 0.15) "BUILD" '(0.27 0.28 0.32)))
(define CHIP-FILL '(0.27 0.28 0.32))
(define CHIP-TEXT '(0.93 0.94 0.96 1.0))
(define CHIP-PX 6.0)
;; → (values 高度 (dl atlas x y scale → void))
(define (commit-rows lay rows w)
  (define ops '())                                      ; (x y → 画)，相对坐标
  (define (op! f) (set! ops (cons f ops)))
  (define (chip! tb cx cy fill fg)
    (define cw (+ (text-block-width tb) (* 2 CHIP-PX)))
    (define ch (+ (text-block-height tb) 2.0))
    (op! (lambda (dl atlas x y scale)
           (drawlist-rect! dl (+ x cx) (+ y cy) cw ch (car fill) (cadr fill) (caddr fill) 1.0 #:radius 5.0)
           (paint-text! dl atlas tb (+ x cx CHIP-PX) (+ y cy 1.0) fg #:scale scale)))
    cw)
  ;; 两列对齐：hash 列宽 = 最宽的 hash，TYPE 列宽 = 最宽的标记（比例字体下各行的 hash 宽度不同，不对齐时标记与标题参差）
  (define htbs (for/list ([r (in-list rows)]) (lay (format "~a" (j r 'hash "")) 'small 1000.0 #f)))
  (define (type-of r) (let ([v (j r 'type)]) (if (string? v) v "text")))
  (define ttbs (for/list ([r (in-list rows)]) (lay (type-of r) 'small 1000.0 #f)))
  (define x1 (+ (for/fold ([m 0.0]) ([tb (in-list htbs)]) (max m (text-block-width tb))) 8.0))
  (define x2 (+ x1 (for/fold ([m 0.0]) ([tb (in-list ttbs)]) (max m (text-block-width tb))) (* 2 CHIP-PX) 8.0))
  (define total
    (for/fold ([yy 0.0]) ([r (in-list rows)] [htb (in-list htbs)] [tytb (in-list ttbs)])
      (define lh (+ (text-block-height htb) 2.0))
      (op! (lambda (dl atlas x y scale) (paint-text! dl atlas htb (+ x 0.0) (+ y yy 1.0) DIM #:scale scale)))
      (chip! tytb x1 yy (hash-ref TYPE-COLORS (type-of r) CHIP-FILL) CHIP-TEXT)
      (define ttb (fit-line lay (let ([v (j r 'title)]) (if (string? v) v "")) 'small (max 40.0 (- w x2))))
      (op! (lambda (dl atlas x y scale) (paint-text! dl atlas ttb (+ x x2) (+ y yy 1.0) TEXT #:scale scale)))
      (define labels (let ([v (j r 'labels)]) (if (list? v) (filter hash? v) '())))
      (define label-h
        (cond
          [(null? labels) 0.0]
          [else
           (for/fold ([cx x1] [cy (+ yy lh 3.0)] #:result (- (+ cy lh) (+ yy lh)))
                     ([l (in-list labels)])
             (define tb (fit-line lay (format "~a" (j l 'text "")) 'small (- w x1 (* 2 CHIP-PX))))
             (define cw (+ (text-block-width tb) (* 2 CHIP-PX)))
             (define-values (px py) (if (and (> (+ cx cw) w) (> cx x1)) (values x1 (+ cy lh 3.0)) (values cx cy)))
             (define auto? (eq? (j l 'auto #f) #t))
             (chip! tb px py (if auto? BADGE-BUSY CHIP-FILL) (if auto? BADGE-BUSY-TEXT CHIP-TEXT))
             (values (+ px cw 4.0) py))]))
      (+ yy lh label-h 6.0)))                           ; label-h 已含标题行与标签行之间的 3
  (values (max 0.0 (- total 6.0))
          (lambda (dl atlas x y scale) (for ([f (in-list (reverse ops))]) (f dl atlas x y scale)))))
(define DIFF-COLLAPSED-LINES 16)

;; diff 行 → (values 背景色或 #f 文字色)
(define (diff-line-style l)
  (cond [(and (> (string-length l) 0) (char=? (string-ref l 0) #\+)) (values '(0.12 0.24 0.16 1.0) '(0.76 0.93 0.79 1.0))]
        [(and (> (string-length l) 0) (char=? (string-ref l 0) #\-)) (values '(0.29 0.13 0.14 1.0) '(0.96 0.79 0.79 1.0))]
        [(regexp-match? #rx"^@@" l) (values #f '(0.52 0.64 0.88 1.0))]
        [else (values #f OUT)])
) ; end define diff-line-style

;; 工具卡片的状态：没有结果且轮不在跑 → 未知（结果还没装载，或旧会话里缺了）
(define (tool-status tr e)
  (define s (e-tool-status e))
  (if (and (eq? s 'running) (not (transcript-busy? tr))) 'unknown s)
) ; end define tool-status

;; 条目 → 元素树（视图平面）：盒子与文字由 view.rkt 排；带 chip 的文字、Markdown、媒体块、工具输出（ANSI / diff / 提交行）是自画的叶子。
;; 量（entry-height）与画（draw!）用同一棵树：量 = measure，画 = render-view!（命中区、文字行记录（选择 / 复制）在画的时候产生，按树的顺序 = 视觉顺序）

(define (leaf measure paint) (ui-custom #:measure measure #:paint paint))
(define (sp h) (ui-spacer #:h h))
(define (col . els) (apply ui-box #:dir 'column els))
(define (txt str font color) (ui-text str #:font font #:color color #:fit? #f))
(define (ind n el #:right [r 0.0]) (ui-box #:pad (vector 0.0 r 0.0 n) el))
;; 只在画的时候做的事（看得到才发的请求）：零高的叶子
(define (on-paint thunk) (leaf (lambda (_w) 0.0) (lambda (dl x y w h scale) (thunk) '())))
;; hit! 风格的绘制函数（md-draw! draw-media!）→ 叶子要返回的命中区表（登记顺序）
(define (collect-hits f)
  (define hs '())
  (f (lambda (kind payload hx hy hw hh) (set! hs (cons (vector kind payload hx hy hw hh) hs))))
  (reverse hs))

;; 按换行分段的文字（段落排版走 para-blocks 缓存）；spans：ANSI 颜色片段（见 paint-ansi-blocks!）
(define (blocks-leaf tr str font color #:ansi [spans '()])
  (leaf (lambda (w) (blocks-height (para-blocks tr str font w)))
        (lambda (dl x y w h scale)
          (define bs (para-blocks tr str font w))
          (if (pair? spans) (paint-ansi-blocks! dl tr bs spans x y color scale) (paint-blocks! dl tr bs x y color scale))
          '())))

;; diff：每行一块，按 +/-/@@ 着色，加 / 删行带底色（底色左右各多出 4）
(define (diff-leaf tr shown)
  (define ls (string-split shown "\n" #:trim? #f))
  (leaf (lambda (w) (for/sum ([l (in-list ls)]) (blocks-height (para-blocks tr l 'mono (- w 8.0)))))
        (lambda (dl x y w h scale)
          (for/fold ([yy y]) ([l (in-list ls)])
            (define bs (para-blocks tr l 'mono (- w 8.0)))
            (define-values (bg fg) (diff-line-style l))
            (when bg (drawlist-rect! dl (- x 4.0) yy (+ w 8.0) (blocks-height bs) (car bg) (cadr bg) (caddr bg) (cadddr bg)))
            (paint-blocks! dl tr bs x yy fg scale))
          '())))

(define (media-leaf tr parts)
  (leaf (lambda (w) (draw-media! tr parts #f 0.0 0.0 w 1.0 #f void))
        (lambda (dl x y w h scale) (collect-hits (lambda (hit!) (draw-media! tr parts dl x y w scale #t hit!))))))

;; Markdown 的高：量一次 = 一次完整排版；解析结果（块表）不变、宽与长代码块的展开状态不变就跨帧复用
(define md-heights (make-weak-hasheq))                 ; 块表 → (vector 宽 展开版本 高)

(define (entry-el tr e)
  (define rc (transcript-rc tr))
  (define lay (rich-ctx-layout rc))
  (cond
    [(e-user? e)
     (define text? (not (string=? (e-user-text e) "")))
     (define media (e-user-media e))
     (ui-box #:fill '(0.17 0.22 0.32 1.0) #:radius 10.0 #:pad PAD
             (and text? (leaf (lambda (w) (let-values ([(tb _l) (rich-layout rc (e-user-text e) (e-user-folds e) w 'body)]) (text-block-height tb)))
                              (lambda (dl x y w h scale)
                                (let-values ([(tb labels) (rich-layout rc (e-user-text e) (e-user-folds e) w 'body)])
                                  (rich-paint! dl rc tb labels (e-user-folds e) x y TEXT scale))
                                '())))
             (and text? (pair? media) (sp 8.0))
             (and (pair? media) (media-leaf tr media)))]
    [(e-asst? e)
     (define reasoning (and (not (string=? (e-asst-reasoning e) ""))
                            (let-values ([(head more) (head-lines (e-asst-reasoning e) 4)]) (if (> more 0) (t 'tr.reasoning-more head more) head))))
     ;; 回答按 Markdown 画（S14）；流式中在末尾带光标
     (define src (string-append (e-asst-text e) (if (e-asst-open? e) " ▍" "")))
     (unless (equal? src (e-asst-md-src e))
       (set-e-asst-md-src! e src)
       (set-e-asst-md-blocks! e (parse-markdown src)))
     (define blocks (e-asst-md-blocks e))
     (define code-open (transcript-code-open tr))
     (define (md! dl x y w scale paint? hit!)
       (md-draw! rc blocks dl x y w scale #:paint? paint? #:hit! hit!
                 #:expanded? (lambda (k) (hash-ref code-open k #f)) #:key (eq-hash-code e)
                 #:color (if (e-asst-interrupted? e) DIM TEXT)))
     ;; 只给像样的回答配「复制」：一句话的过渡说明（"我先跑一下命令。"）不配；被中断的不配
     (define copy? (and (not (e-asst-open? e)) (not (e-asst-interrupted? e))
                        (let ([s (string-trim (e-asst-text e))]) (or (>= (string-length s) 60) (regexp-match? #rx"\n" s)))))
     (col (and reasoning (ind PAD #:right PAD (blocks-leaf tr reasoning 'small DIM)))
          (and reasoning (sp 6.0))
          (ind PAD #:right PAD (leaf (lambda (w)
                                       (define ver (transcript-code-version tr))
                                       (define c (hash-ref md-heights blocks #f))
                                       (if (and c (= (vector-ref c 0) w) (= (vector-ref c 1) ver))
                                           (vector-ref c 2)
                                           (let ([h (md! #f 0.0 0.0 w 1.0 #f void)]) (hash-set! md-heights blocks (vector w ver h)) h)))
                                     (lambda (dl x y w h scale) (collect-hits (lambda (hit!) (md! dl x y w scale #t hit!))))))
          ;; 被中断时流到一半的文字：下面一行说明（不在模型上下文里）
          (and (e-asst-interrupted? e) (ind PAD #:right PAD (col (sp 4.0) (txt (t 'tr.partial) 'small DIM))))
          (if copy?
              (ui-box #:dir 'row #:pad (vector 2.0 0.0 0.0 (- PAD 4.0))
                      (ui-box #:hit (cons 'copy (e-asst-text e)) #:pad 4.0 (ui-text (t 'tr.copy) #:font 'small #:color DIM)))
              (sp 4.0)))]
    [(e-tool? e)
     (define status (tool-status tr e))
     (define expanded? (e-tool-expanded? e))
     (define id (e-tool-id e))
     (define-values (icon _icolor) (case status
                                     [(running) (values "●" WARN)]
                                     [(ok) (values "✓" '(0.45 0.85 0.55 1.0))]
                                     [(aborted) (values (t 'tr.tool-aborted) WARN)]
                                     [(unknown) (values (t 'tr.tool-unknown) DIM)]
                                     [else (values "✗" ERR)]))
     ;; 改文件的工具：正文是 diff，否则是工具输出
     (define diff (and (eq? status 'ok) (e-tool-diff e)))
     ;; git_history：结构化的提交列表 → 画成一行行（hash、TYPE 标记、标题、标签片）而不是工具输出文字
     (define commits (and (eq? status 'ok) (not diff)
                          (let ([u (e-tool-ui e)]) (and (hash? u) (equal? (j u 'kind) "commits") (list? (j u 'commits)) (pair? (j u 'commits))
                                                        (filter hash? (j u 'commits))))))
     (define src (e-tool-source e))
     (define head (format "~a  ~a~a  ~a~a~a" icon
                          (if (and (string? src) (not (equal? src "builtin"))) (format "[~a] " src) "")
                          (e-tool-name e) (tool-summary (e-tool-name e) (e-tool-input e))
                          (if diff (format "  +~a −~a" (j diff 'added 0) (j diff 'removed 0)) "")
                          (if (real? (e-tool-ms e)) (format "  · ~a ms" (inexact->exact (round (e-tool-ms e)))) "")))
     (define out (if diff (j diff 'text) (e-tool-output e)))
     (define-values (pn pmode pkeep pdrop) (preview-spec tr (e-tool-name e)))
     (define smart? (and (equal? pmode "smart") (not diff) (not commits) (not expanded?)))
     (define-values (shown more)
       (cond [commits (values "" (if expanded? 0 (max 0 (- (length commits) COMMITS-COLLAPSED))))]
             [expanded? (head-lines out EXPANDED-LINES)]
             [diff (head-lines out DIFF-COLLAPSED-LINES)]
             [else (preview-lines out pn pmode #:keep pkeep #:drop pdrop #:picked (and smart? (e-tool-picked e)))]))
     ;; smart：看得到的（画的时候）、行数够多的、有结果的卡片 → 请本机小模型挑行（每张卡片一次）
     (define pick? (and smart? (> more 0) (not (e-tool-picked e)) (memq status '(ok error)) (exact-nonnegative-integer? (e-tool-rid e))
                        (>= (+ pn more) (let ([m (j (transcript-preview tr) 'min_lines 20)]) (if (exact-nonnegative-integer? m) m 20)))))
     (define (pick!)
       (unless (e-tool-picked e)
         (set-e-tool-picked! e 'pending)
         (define cfg (transcript-preview tr))
         (want! tr (hasheq 'type "select_lines" 'id (e-tool-rid e) 'call_id id 'tool (e-tool-name e)
                           'summary (tool-summary (e-tool-name e) (e-tool-input e)) 'max_lines pn
                           'model (let ([m (j cfg 'model)]) (if (string? m) m 'null))
                           'endpoint (let ([m (j cfg 'endpoint)]) (if (string? m) m 'null))
                           'allow_remote (and (j cfg 'allow_remote) #t)))))
     (define color (if (eq? status 'error) ERR OUT))
     (define body
       (cond [commits
              (define rows (if expanded? commits (take commits (min COMMITS-COLLAPSED (length commits)))))
              (leaf (lambda (w) (let-values ([(h _p) (commit-rows lay rows w)]) h))
                    (lambda (dl x y w h scale) (let-values ([(_h p) (commit-rows lay rows w)]) (p dl (rich-ctx-atlas rc) x y scale)) '()))]
             [(string=? shown "") #f]
             [diff (diff-leaf tr shown)]
             ;; ANSI（工具输出常带颜色）：去掉转义、颜色记成片段
             [else (let-values ([(plain spans) (ansi-parse shown)]) (blocks-leaf tr plain 'mono color #:ansi spans))]))
     (define footer
       (cond [(and commits (> more 0)) (t 'tr.more-commits more (t 'tr.expand))]
             [commits #f]
             [(and expanded? (e-tool-fetching? e)) (t 'tr.full-loading)]
             [(and smart? (> more 0) (eq? (e-tool-picked e) 'pending)) (t 'tr.picking more)]
             [(and smart? (> more 0) (pair? (e-tool-picked e))) (t 'tr.picked more)]
             [(> more 0) (t 'tr.more-lines more (if expanded? (t 'tr.collapse) (t 'tr.expand)))]
             [(and (not diff) (e-tool-truncated? e)) (t 'tr.truncated-total (or (e-tool-total e) "?"))]
             [(and diff (eq? (j diff 'truncated) #t)) (t 'tr.diff-truncated)]
             [else #f]))
     ;; 子 agent 的步骤（P5）：收起时一行计数（点了展开），展开时缩进画在输出下面
     (define kids (tool-children tr e))
     (define kids-el
       (and (pair? kids)
            (col (ui-box #:hit (cons 'tool id) #:pad (vector 6.0 PAD 0.0 PAD)
                         (txt (t (if expanded? 'tr.sub-steps-open 'tr.sub-steps) (length kids)) 'small DIM))
                 (and expanded? (apply ui-box #:pad (vector 6.0 6.0 6.0 PAD) #:gap 6.0 (for/list ([c (in-list kids)]) (entry-el tr c)))))))
     ;; 工具卡片：0.13 与对话区的底只有 1.12 → 0.16；标题行与「还有 N 行」行可点（展开 / 收起）
     (ui-box #:fill '(0.16 0.16 0.18 1.0) #:radius 8.0
             (and pick? (on-paint pick!))
             (ui-box #:hit (cons 'tool id) #:pad (vector (* 0.7 PAD) PAD 0.0 PAD) (txt head 'small (if (eq? status 'error) ERR DIM)))
             (and body (ui-box #:pad (vector 6.0 PAD 0.0 PAD) body))
             ;; 「还有 N 行 · 点标题展开」：可点的范围连同卡片底边距（没有子 agent 步骤时）
             (and footer (ui-box #:hit (cons 'tool id) #:pad (vector 4.0 PAD (if kids-el 0.0 PAD) PAD) (txt footer 'small DIM)))
             (and kids-el (sp (* 0.3 PAD)))
             kids-el
             (sp (cond [kids-el (* 0.7 PAD)] [footer 0.0] [else PAD])))]
    [(e-perm? e)
     (define tool (and (string? (e-perm-tool e)) (e-perm-tool e)))
     ;; 费用上限询问（内核借权限询问的通道）：自己的标题、说明与按钮文字
     (define budget? (equal? tool "budget"))
     (define bin (and budget? (hash? (e-perm-input e)) (e-perm-input e)))
     (define title (cond [budget? (t 'tr.budget-title)]
                         [tool (t 'tr.perm-title tool (permission-level-label (e-perm-level e)))]
                         [else (t 'tr.perm-title0)]))
     (define-values (shown more)
       (head-lines (cond [bin (t (case (hash-ref bin 'scope #f) [("day") 'tr.budget-day] [("combined") 'tr.budget-combined] [else 'tr.budget-turn])
                                 (hash-ref bin 'spent "?") (hash-ref bin 'cap "?"))]
                         [(hash? (e-perm-input e)) (input->text (e-perm-input e))]
                         [else (e-perm-prompt e)]) 10))
     (define ans (perm-answer tr e))
     (define buttons (if budget?
                         (list (list "yes" (t 'tr.budget-continue))
                               (list "always" (if (and bin (member (hash-ref bin 'scope #f) '("day" "combined"))) (t 'tr.budget-snooze) (t 'tr.budget-continue2)))
                               (list "no" (t 'tr.budget-stop)))
                         (list (list "yes" (t 'tr.allow)) (list "always" (t 'tr.always)) (list "no" (t 'tr.deny)))))
     (define row-h (+ 8.0 (text-block-line-height (lay (cadr (car buttons)) 'body 1000.0 #f))))
     ;; 橙色边框（1.5）里的深色卡片：标题、输入（最多 10 行）、按钮（允许 / 总是 / 拒绝）或已答的结果
     (ui-box #:fill '(0.55 0.42 0.18 1.0) #:radius 10.0 #:pad 1.5
             (ui-box #:fill '(0.15 0.13 0.10 1.0) #:radius 9.0 #:pad (- PAD 1.5)
                     (txt title 'title WARN)
                     (sp 6.0)
                     (blocks-leaf tr (if (> more 0) (t 'tr.more-lines-paren shown more) shown) (if budget? 'body 'mono) OUT)
                     (sp 10.0)
                     (if ans
                         (txt (case ans
                                [("yes") (if budget? (t 'tr.budget-continued) (t 'tr.allowed))]
                                [("always") (if budget? (t 'tr.budget-continued) (t 'tr.always-allowed))]
                                [("no") (if budget? (t 'tr.budget-stopped) (t 'tr.denied))]
                                [else (t 'tr.void)])
                              'small DIM)
                         (apply ui-box #:dir 'row #:gap 10.0
                                (for/list ([b (in-list buttons)])
                                  (ui-box #:h row-h #:pad (vector 4.0 14.0 0.0 14.0) #:radius 6.0 #:hit (cons 'perm (cons (e-perm-id e) (car b)))
                                          #:fill (if (equal? (car b) "no") '(0.30 0.20 0.20 1.0) '(0.24 0.36 0.26 1.0))
                                          (ui-text (cadr b) #:font 'body #:color TEXT)))))))]
    [(e-note? e)
     (ind PAD #:right PAD (txt (e-note-text e) 'small (case (e-note-level e) [(warn) WARN] [(error) ERR] [(info) TEXT] [else DIM])))])
) ; end define entry-el

;; ---------------------------------------------------------------- 渲染 / 滚动 / 命中

(define (entry-signature tr e)
  (cond [(e-asst? e) (list (string-length (e-asst-text e)) (string-length (e-asst-reasoning e)) (e-asst-open? e)
                           (e-asst-interrupted? e) (transcript-code-version tr))]
        [(e-tool? e) (list (tool-status tr e) (string-length (e-tool-output e)) (e-tool-expanded? e) (e-tool-ms e) (and (e-tool-diff e) #t)
                           (e-tool-truncated? e) (e-tool-fetching? e) (e-tool-source e) (transcript-preview tr) (e-tool-picked e)
                           (let ([ks (tool-children tr e)]) (if (e-tool-expanded? e) (map (lambda (c) (entry-signature tr c)) ks) (length ks))))]
        [(e-perm? e) (perm-answer tr e)]
        [else 'static])
) ; end define entry-signature

;; 条目高度：签名与宽度不变就用缓存（每帧只重新量变化的条目）
(define (entry-height tr e w scale)
  (define sig (entry-signature tr e))
  (define c (hash-ref (transcript-heights tr) e #f))
  (cond [(and c (= (vector-ref c 0) w) (equal? (vector-ref c 1) sig)) (vector-ref c 2)]
        [else (define-values (_w hh) (measure (rich-ctx-layout (transcript-rc tr)) (entry-el tr e) w))
              (hash-set! (transcript-heights tr) e (vector w sig hh))
              hh])
) ; end define entry-height

;; 量槽的实际高度（视口附近的）：装载了的按条目量；没装载的按摘要估 + 本地条目实量
(define (measure-slot! tr s w scale)
  (define r (sub1 s))
  (define loaded? (or (< r 0) (rec-loaded? tr r)))
  (cond
    [loaded?
     (flvector-set! (transcript-sw tr) s w)
     (set-slot-h! tr s (for/sum ([e (in-list (slot-entries tr s))]) (+ (entry-height tr e w scale) GAP)))]
    [(fl> (flvector-ref (transcript-sw tr) s) 0.0) (void)]          ; 逐出了的：留着量过的高度
    [else (set-slot-h! tr s (+ (hint-estimate tr r w)
                               (for/sum ([e (in-list (slot-entries tr s))]) (+ (entry-height tr e w scale) GAP))))])
) ; end define measure-slot!

(define (entry-key s k) (+ (* s 1024) k))              ; 选区里条目的顺序键

;; 视口 (x y w h)。槽坐标：内容顶 = 0，槽 0 顶 = GAP。
(define (transcript-render! tr dl x y w h scale)
  (set-transcript-width! tr w)
  (set-transcript-view-h! tr h)
  (define n (nslots tr))
  (define (slot-top s) (+ GAP (fen-prefix tr s)))
  (define (from-anchor) (+ (slot-top (transcript-anchor tr)) (transcript-offset tr)))
  (define (stream-h) (let ([e (transcript-stream tr)]) (if e (+ (entry-height tr e w scale) GAP) 0.0)))
  (define (content-h) (+ GAP (fen-total tr) (stream-h)))
  ;; 1. 视口附近（上下各一屏）的槽量实际高度；锚点不动 → 上方的高度变化不让内容跳
  (define s0 (if (transcript-stick? tr) (max 0.0 (- (content-h) h)) (from-anchor)))
  (for ([s (in-range (fen-find tr (- s0 GAP h)) (min n (add1 (fen-find tr (+ s0 (* 2 h))))))]) (measure-slot! tr s w scale))
  (define total (content-h))
  (set-transcript-content-h! tr total)
  (define max-scroll (max 0.0 (- total h)))
  (define sc (max 0.0 (min max-scroll (if (transcript-stick? tr) max-scroll (from-anchor)))))
  (set-transcript-scroll! tr sc)
  (let ([a (fen-find tr (- sc GAP))]) (set-transcript-anchor! tr a) (set-transcript-offset! tr (- sc (slot-top a))))
  ;; 2. 要装载的页：视口上下各两屏内没装载的记录
  (define now (current-inexact-milliseconds))
  (define req (transcript-requested tr))
  (for ([p (in-range (page-of (max 0 (sub1 (fen-find tr (- sc GAP (* 2 h))))))
                     (add1 (page-of (max 0 (sub1 (fen-find tr (+ sc (* 3 h))))))))])
    (define from (* p PAGE))
    (define to (min (transcript-total tr) (* (add1 p) PAGE)))
    (when (and (< from to)
               (for/or ([r (in-range from to)]) (not (rec-loaded? tr r)))
               (let ([t0 (hash-ref req p #f)]) (or (not t0) (> (- now t0) REQUEST-RETRY-MS))))
      (hash-set! req p now)
      (want! tr (hasheq 'type "items" 'from from 'to to 'preview_bytes PREVIEW-BYTES))))
  ;; 3. 画
  (define top (- y sc))
  (define hits '())
  (define (hit! kind payload hx hy hw hh)
    (when (and (< hy (+ y h)) (> (+ hy hh) y))                ; 只登记视口内可见的
      (set! hits (cons (vector kind payload hx hy hw hh) hits))))
  (drawlist-set-clip! dl (vector x y w h))
  (define recorded '())
  (define (draw! e key ey)
    (define k 0)
    (parameterize ([current-text-sink
                    (lambda (str xs ly lh soft?)
                      (set! recorded (cons (vector key k str xs ly lh soft?) recorded))
                      (set! k (add1 k)))])
      (define-values (hs _regions _scrolls)
        (render-view! (transcript-rc tr) dl (entry-el tr e) x ey w (entry-height tr e w scale) scale #:clip (vector x y w h)))
      (for ([hv (in-list hs)]) (apply hit! (vector->list hv)))))
  (let loop ([s (fen-find tr (- sc GAP))] [sy (+ top (slot-top (fen-find tr (- sc GAP))))])
    (when (and (< s n) (< sy (+ y h)))
      (define r (sub1 s))
      (define ey
        (cond
          [(or (< r 0) (rec-loaded? tr r)) sy]
          [else                                                    ; 没装载：占位块（估计高度 / 逐出前量过的高度）
           (define eh (- (if (null? (hash-ref (transcript-local tr) s '())) (slot-h tr s) (hint-estimate tr r w)) GAP))
           (when (> eh 4.0) (drawlist-rect! dl x sy w eh 0.12 0.12 0.14 1.0 #:radius 8.0))
           (+ sy (max 0.0 eh) (if (> eh 0.0) GAP 0.0))]))
      (for/fold ([ey ey]) ([e (in-list (slot-entries tr s))] [k (in-naturals)])
        (define eh (entry-height tr e w scale))
        (when (and (< ey (+ y h)) (> (+ ey eh) y)) (draw! e (entry-key s k) ey))
        (+ ey eh GAP))
      (loop (add1 s) (+ sy (slot-h tr s)))))
  (let ([e (transcript-stream tr)])
    (when e
      (define ey (+ top GAP (fen-total tr)))
      (when (< ey (+ y h)) (draw! e (entry-key n 0) ey))))
  (set-transcript-lines! tr (list->vector (reverse recorded)))
  ;; 选区高亮（盖在文字上，半透明）
  (define-values (sa sb) (selection-range tr))
  (when sa
    (for ([r (in-vector (transcript-lines tr))])
      (define-values (a b) (line-selection r sa sb))
      (when (and a (< a b))
        (define xs (vector-ref r 3))
        (drawlist-rect! dl (vector-ref xs a) (vector-ref r 4) (max 2.0 (- (vector-ref xs b) (vector-ref xs a))) (vector-ref r 5)
                        0.30 0.50 0.95 0.35))))
  ;; 上翻离开底部时：「↓ 回到底部」
  (when (< sc (- max-scroll 4.0))
    (define th (text-block-height ((rich-ctx-layout (transcript-rc tr)) (t 'tr.to-bottom) 'small 1000.0 #f)))
    (define pill (ui-box #:dir 'row #:pad (vector 5.0 12.0 5.0 12.0) #:fill '(0.24 0.36 0.62 0.95) #:radius (/ (+ th 10.0) 2) #:hit (cons 'to-bottom #t)
                         (ui-text (t 'tr.to-bottom) #:font 'small #:color TEXT)))
    (define-values (bw bh) (measure (rich-ctx-layout (transcript-rc tr)) pill w))   ; 胶囊：居中，离底 10
    (define-values (hs _r _s) (render-view! (transcript-rc tr) dl pill (+ x (/ (- w bw) 2)) (- (+ y h) bh 10.0) bw bh scale #:clip (vector x y w h)))
    (for ([hv (in-list hs)]) (apply hit! (vector->list hv))))
  (drawlist-set-clip! dl #f)
  ;; 小地图：画在对话区右侧的边距里（不占排版宽度）；只在内容超过一屏时
  (when (> total (* 1.5 h)) (draw-minimap! tr dl (+ x w 7.0) y 6.0 h sc total hit!))
  (set-transcript-hits! tr (reverse hits))                    ; 按绘制顺序；命中取最后一个（浮在上面的）
) ; end define transcript-render!

;; ---------------------------------------------------------------- 小地图（P3）

(define MINI-REFRESH-FRAMES 30)                         ; 内容变了：最多每这么多帧重算一次刻度（O(记录数)）

;; 刻度：用户消息（蓝）、权限询问（橙）在整个内容里的纵向位置（0..1），按像素桶去重。
;; 记录种类：装载了的看显示条目，没装载的看索引摘要。
(define (minimap-ticks tr total-h buckets)
  (define seen (make-hasheqv))
  (define hs (transcript-hints tr))
  (let loop ([s 1] [y (+ GAP (slot-h tr 0))] [acc '()])
    (cond
      [(> s (transcript-total tr)) (reverse acc)]
      [else
       (define r (sub1 s))
       (define kind
         (cond [(rec-loaded? tr r)
                (define es (hash-ref (transcript-groups tr) r '()))
                (cond [(ormap e-user? es) 'user] [(ormap e-perm? es) 'perm] [else #f])]
               [else (case (bytes-ref hs (* 6 r)) [(1) 'user] [(4) 'perm] [else #f])]))
       (define b (and kind (inexact->exact (floor (* buckets (/ y total-h))))))
       (define new? (and kind (not (eq? (hash-ref seen b #f) kind)) (not (eq? (hash-ref seen b #f) 'perm))))
       (when new? (hash-set! seen b kind))
       (loop (add1 s) (+ y (slot-h tr s)) (if new? (cons (cons (/ y total-h) kind) acc) acc))]))
) ; end define minimap-ticks

(define (draw-minimap! tr dl mx my mw mh sc total hit!)
  (define c (transcript-mini tr))
  (define ticks
    (cond
      [(and c (= (vector-ref c 0) (transcript-ver tr))) (vector-ref c 2)]
      [(and c (< (vector-ref c 1) MINI-REFRESH-FRAMES)) (vector-set! c 1 (add1 (vector-ref c 1))) (vector-ref c 2)]
      [else (define ts (minimap-ticks tr total (max 1 (inexact->exact (floor mh)))))
            (set-transcript-mini! tr (vector (transcript-ver tr) 0 ts))
            ts]))
  (drawlist-rect! dl mx my mw mh 0.16 0.16 0.19 0.9 #:radius 3.0)
  (for ([t (in-list ticks)])
    (define ty (+ my (* (car t) mh)))
    (if (eq? (cdr t) 'perm)
        (drawlist-rect! dl mx ty mw 2.0 0.95 0.70 0.30 1.0)
        (drawlist-rect! dl (+ mx 1.0) ty (- mw 2.0) 1.0 0.45 0.62 0.95 0.9)))
  ;; 视口位置
  (define th (max 6.0 (* mh (/ mh total))))
  (define ty (+ my (* (- mh th) (/ sc (max 1.0 (- total mh))))))
  (drawlist-rect! dl (- mx 1.0) ty (+ mw 2.0) th 0.80 0.82 0.88 0.35 #:radius 3.0)
  (hit! 'minimap (vector my mh) (- mx 4.0) my (+ mw 8.0) mh)
) ; end define draw-minimap!

;; 点 / 拖小地图：纵坐标 y → 让内容里对应的位置居中
(define (transcript-minimap-jump! tr y)
  (define hrec (for/first ([h (in-list (transcript-hits tr))] #:when (eq? (vector-ref h 0) 'minimap)) (vector-ref h 1)))
  (when hrec
    (define frac (max 0.0 (min 1.0 (/ (- y (vector-ref hrec 0)) (vector-ref hrec 1)))))
    (define total (transcript-content-h tr))
    (define vh (transcript-view-h tr))
    (define max-scroll (max 0.0 (- total vh)))
    (define sc (max 0.0 (min max-scroll (- (* frac total) (/ vh 2)))))
    (set-transcript-scroll! tr sc)
    (let ([a (fen-find tr (- sc GAP))]) (set-transcript-anchor! tr a) (set-transcript-offset! tr (- sc GAP (fen-prefix tr a))))
    (set-transcript-stick?! tr (>= sc (- max-scroll 2.0))))
) ; end define transcript-minimap-jump!

(define (transcript-scroll-to-bottom! tr) (set-transcript-stick?! tr #t))

;; dy：NSEvent scrollingDeltaY（point；手指下滑为正 → 内容下移 → 滚动位置减小）
(define (transcript-scroll! tr dy)
  (define max-scroll (max 0.0 (- (transcript-content-h tr) (transcript-view-h tr))))
  (define sc (max 0.0 (min max-scroll (- (transcript-scroll tr) dy))))
  (set-transcript-scroll! tr sc)
  (let ([a (fen-find tr (- sc GAP))]) (set-transcript-anchor! tr a) (set-transcript-offset! tr (- sc GAP (fen-prefix tr a))))
  (set-transcript-stick?! tr (>= sc (- max-scroll 2.0)))
) ; end define transcript-scroll!

;; ---------------------------------------------------------------- 选择 / 复制（按看到的文字）

(define (pos<? a b)
  (let loop ([i 0]) (cond [(= i 3) #f] [(< (vector-ref a i) (vector-ref b i)) #t] [(> (vector-ref a i) (vector-ref b i)) #f] [else (loop (add1 i))]))
) ; end define pos<?

(define (selection-range tr)
  (define a (transcript-sel-anchor tr))
  (define f (transcript-sel-focus tr))
  (cond [(or (not a) (not f) (equal? a f)) (values #f #f)]
        [(pos<? a f) (values a f)]
        [else (values f a)])
) ; end define selection-range

;; 行记录 r 在选区 [sa, sb) 里的字符区间 → (values a b) 或 (values #f #f)
(define (line-selection r sa sb)
  (define key (vector (vector-ref r 0) (vector-ref r 1) 0))
  (define n (string-length (vector-ref r 2)))
  (define (line-pos p) (cond [(and (= (vector-ref p 0) (vector-ref r 0)) (= (vector-ref p 1) (vector-ref r 1))) (min n (vector-ref p 2))]
                             [(pos<? p key) 0]
                             [else n]))
  (define a (line-pos sa))
  (define b (line-pos sb))
  (if (< a b) (values a b) (values #f #f))
) ; end define line-selection

;; 点 (x, y) → 最近的文字位置；离任何行都远 → 取纵向最近的行
(define (position-at tr x y)
  (define ls (transcript-lines tr))
  (and (> (vector-length ls) 0)
       (let* ([r (for/fold ([best #f] [bd +inf.0] #:result best) ([r (in-vector ls)])
                   (define top (vector-ref r 4))
                   (define d (cond [(< y top) (- top y)] [(> y (+ top (vector-ref r 5))) (- y top (vector-ref r 5))] [else 0.0]))
                   (if (< d bd) (values r d) (values best bd)))]
              [xs (vector-ref r 3)]
              [i (for/fold ([bi 0] [bd +inf.0] #:result bi) ([v (in-vector xs)] [j (in-naturals)])
                   (if (< (abs (- v x)) bd) (values j (abs (- v x))) (values bi bd)))])
         (vector (vector-ref r 0) (vector-ref r 1) i)))
) ; end define position-at

(define (transcript-select-start! tr x y)
  (define p (position-at tr x y))
  (set-transcript-sel-anchor! tr p)
  (set-transcript-sel-focus! tr p)
) ; end define transcript-select-start!

(define (transcript-select-extend! tr x y)
  (when (transcript-sel-anchor tr)
    (define p (position-at tr x y))
    (when p (set-transcript-sel-focus! tr p)))
) ; end define transcript-select-extend!

(define (transcript-clear-selection! tr) (set-transcript-sel-anchor! tr #f) (set-transcript-sel-focus! tr #f))

;; 选中的文字：同段软换行直接相连，段之间换行，条目之间空一行；chip 占位符去掉
(define (transcript-selection-text tr)
  (define-values (sa sb) (selection-range tr))
  (cond
    [(not sa) #f]
    [else
     (define out (open-output-string))
     (for/fold ([prev #f]) ([r (in-vector (transcript-lines tr))])
       (define-values (a b) (line-selection r sa sb))
       (cond
         [(and a (< a b))
          (when prev
            (write-string (cond [(not (= (vector-ref prev 0) (vector-ref r 0))) "\n\n"] [(vector-ref prev 6) ""] [else "\n"]) out))
          (write-string (substring (vector-ref r 2) a b) out)
          r]
         [else prev]))
     (regexp-replace* (string (integer->char #xFFFC)) (get-output-string out) "")])
) ; end define transcript-selection-text

(define (transcript-toggle-code! tr key)
  (define s (transcript-code-open tr))
  (if (hash-ref s key #f) (hash-remove! s key) (hash-set! s key #t))
  (set-transcript-code-version! tr (add1 (transcript-code-version tr)))
) ; end define transcript-toggle-code!

;; → (list 'perm id decision) | (list 'tool id) | (list 'copy 文本) | (list 'code-toggle 键) | (list 'to-bottom) | (list 'minimap)
;;   | (list 'media-play sha) | #f
;; 命中表按绘制顺序：取最后一个相交的 = 最上面的（浮层「回到底部」先于被它盖住的工具卡片）
(define (transcript-hit tr x y)
  (for/last ([h (in-list (transcript-hits tr))]
              #:when (and (<= (vector-ref h 2) x (+ (vector-ref h 2) (vector-ref h 4)))
                          (<= (vector-ref h 3) y (+ (vector-ref h 3) (vector-ref h 5)))))
    (case (vector-ref h 0)
      [(perm) (list 'perm (car (vector-ref h 1)) (cdr (vector-ref h 1)))]
      [(copy) (list 'copy (vector-ref h 1))]
      [(code-toggle) (list 'code-toggle (vector-ref h 1))]
      [(to-bottom) (list 'to-bottom)]
      [(minimap) (list 'minimap)]
      [(media-play) (list 'media-play (vector-ref h 1))]
      [else (list 'tool (vector-ref h 1))]))
) ; end define transcript-hit

;; 测试 / 冒烟：某个命中目标的中心点
(define (transcript-hit-center tr kind payload)
  (for/first ([h (in-list (transcript-hits tr))] #:when (and (eq? (vector-ref h 0) kind) (equal? (vector-ref h 1) payload)))
    (cons (+ (vector-ref h 2) (/ (vector-ref h 4) 2)) (+ (vector-ref h 3) (/ (vector-ref h 5) 2))))
) ; end define transcript-hit-center

;; 权限卡片的结构化内容：→ (list tool level input) 或 #f
(define (transcript-permission-info tr id)
  (define e (hash-ref (transcript-perms tr) id #f))
  (and e (list (e-perm-tool e) (e-perm-level e) (e-perm-input e)))
) ; end define transcript-permission-info

;; 摘要（冒烟统计）：每个已装载的显示条目一个符号（用量行不计）
(define (transcript-summary tr)
  (for/list ([e (in-list (loaded-entries tr))] #:unless (and (e-note? e) (eq? (e-note-level e) 'usage)))
    (cond [(e-user? e) 'user]
          [(e-asst? e) (if (e-asst-interrupted? e) 'assistant/interrupted 'assistant)]
          [(e-tool? e) (string->symbol (format "tool:~a/~a~a" (e-tool-name e) (e-tool-status e)
                                               (let ([d (e-tool-diff e)]) (if d (format "+~a-~a" (j d 'added 0) (j d 'removed 0)) ""))))]
          [(e-perm? e) (string->symbol (format "perm/~a" (or (perm-answer tr e) "pending")))]
          [else 'note]))
) ; end define transcript-summary

;; 最后一条回答（流式中的优先）：从尾部的槽往前找
(define (transcript-last-assistant-text tr)
  (define e (or (transcript-stream tr)
                (for*/first ([s (in-range (sub1 (nslots tr)) -1 -1)]
                             [e (in-list (reverse (slot-entries tr s)))] #:when (e-asst? e))
                  e)))
  (and e (e-asst-text e))
) ; end define transcript-last-assistant-text

;; 统计（测试 / 基准）：记录数、装载的记录数与页数、内容高度
(define (transcript-stats tr)
  (hasheq 'total (transcript-total tr)
          'loaded (for/sum ([r (in-range (transcript-total tr))]) (if (rec-loaded? tr r) 1 0))
          'pages (hash-count (transcript-loaded-pages tr))
          'content_h (transcript-content-h tr) 'scroll (transcript-scroll tr) 'anchor (transcript-anchor tr))
) ; end define transcript-stats

;; ---------------------------------------------------------------- 组件

;; needs text-layout → provides transcript-port
;; 一窗多主机：每个主机一份对话（文档）。后台主机的事件照常进它自己的文档（apply-to!），切换主机 = 换当前文档
;; （滚动位置、展开的卡片、选区都跟着文档走）。除 *-to! / doc 系列外，方法都作用于当前文档。
(define transcript-component
  (component 'transcript (list text-layout media-port) (list transcript-port)
             (lambda (use config)
               (define rc ((use text-layout) 'rich))
               (define mp (use media-port))
               (define docs (make-hash))
               (define cur "local")
               (define preview DEFAULT-PREVIEW)
               (define (doc id) (hash-ref! docs id (lambda () (let ([d (make-transcript rc id #:media mp)]) (set-transcript-preview! d preview) d))))
               (define (D) (doc cur))
               (list (implement transcript-port
                                'apply! (lambda (ev) (transcript-apply! (D) ev))
                                'add-user! (lambda (text folds [media '()]) (transcript-add-user! (D) text folds media))
                                'toggle-play! (lambda (sha) (transcript-toggle-play! (D) sha))
                                'note! (lambda (text [level 'dim]) (transcript-note! (D) text level))
                                'answer! (lambda (id decision) (transcript-answer! (D) id decision))
                                'toggle-tool! (lambda (id) (transcript-toggle-tool! (D) id))
                                'pending-permissions (lambda () (transcript-pending-permissions (D)))
                                'permission-info (lambda (id) (transcript-permission-info (D) id))
                                'render! (lambda (dl x y w h scale) (transcript-render! (D) dl x y w h scale))
                                'scroll! (lambda (dy) (transcript-scroll! (D) dy))
                                'scroll-to-bottom! (lambda () (transcript-scroll-to-bottom! (D)))
                                'toggle-code! (lambda (key) (transcript-toggle-code! (D) key))
                                'select-start! (lambda (x y) (transcript-select-start! (D) x y))
                                'select-extend! (lambda (x y) (transcript-select-extend! (D) x y))
                                'clear-selection! (lambda () (transcript-clear-selection! (D)))
                                'selection-text (lambda () (transcript-selection-text (D)))
                                'hit (lambda (x y) (transcript-hit (D) x y))
                                'hit-center (lambda (kind payload) (transcript-hit-center (D) kind payload))
                                'summary (lambda () (transcript-summary (D)))
                                'busy? (lambda () (transcript-busy? (D)))
                                'turns (lambda () (transcript-turns (D)))
                                'last-assistant-text (lambda () (transcript-last-assistant-text (D)))
                                'wanted (lambda () (transcript-wanted! (D)))
                                'minimap-jump! (lambda (y) (transcript-minimap-jump! (D) y))
                                'stats (lambda () (transcript-stats (D)))
                                'tool-uis (lambda ()
                                            (for/list ([e (in-hash-values (transcript-tools (D)))] #:when (hash? (e-tool-ui e)))
                                              (list (e-tool-name e) (j (e-tool-ui e) 'kind) (let ([c (j (e-tool-ui e) 'commits)]) (if (list? c) (length c) 0)))))
                                'set-preview! (lambda (cfg)
                                                (set! preview (if (hash? cfg) cfg DEFAULT-PREVIEW))
                                                (for ([d (in-hash-values docs)]) (set-transcript-preview! d preview)))
                                ;; 多主机
                                'use-doc! (lambda (id) (set! cur id) (void (doc id)))
                                'current-doc (lambda () cur)
                                'apply-to! (lambda (id ev) (transcript-apply! (doc id) ev))
                                'note-to! (lambda (id text [level 'dim]) (transcript-note! (doc id) text level))
                                'reset-doc! (lambda (id) (hash-remove! docs id) (void (doc id)))
                                'drop-doc! (lambda (id) (hash-remove! docs id) (when (equal? id cur) (set! cur "local")))
                                'doc-info (lambda (id)
                                            (define d (hash-ref docs id #f))
                                            (and d (hasheq 'busy (transcript-busy? d) 'pending (length (transcript-pending-permissions d))
                                                           'turns (transcript-turns d)))))))))

(provide transcript-component)
