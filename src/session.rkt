#lang tstring racket
;; session.rkt — .rktd 会话持久化与流式重放（design.md §4.6 / §5.6）
;; transcript 即真相源；重放复用运行时同一套 state-append 迁移函数。

(require
 racket/list
 racket/string
 racket/file
 racket/date
 racket/pvector
 (file "model.rkt")
 (file "rktd.rkt")
 (file "session-index.rkt")             ; <会话>.rktd.idx：随机访问 / 局部加载
) ; end require

;; 记录：prefab，直接 write 进 datum log
(struct rec
  (type   ; 'meta | 'msg | 'usage | 'compact | 'turn | 'diff | 'fork | 'route | 'effect
          ; 界面记录（design-content.md §5「所有信息都有记录」，data 都是 hash）：
          ; 'perm（权限询问 id tool level input prompt）'perm-answer（id decision）'note（level code text）
          ; 'req（每次模型请求：model input output cost）'tool（工具结束：id name ms error source）'partial（被中断时流到一半的 text reasoning）
          ; 'compact：data = 压缩后的完整历史（消息表；清空 = '()）；重放时历史换成它（压缩前的原消息仍在文件里）
          ; 'turn：data = 'begin | 'end | 'revert：重放时 'revert 退回还没 'end 的那个 'begin 时的状态（出错 / 中断的轮）
          ; 'workspace：会话在哪个工作区跑（hash：'kind "chat" 'dir 聊天目录 …；后写的覆盖先写的）。没有 = 内核的工作目录
          ; 'title：用户改的标题（字符串；后写的覆盖先写的；会话列表优先用它）
          ; 'sub：子 agent 的记录（parent 父工具调用 id；本机：kind data = 内层记录；远端拉回：host item = 已投影的条目）；重放跳过
   ts     ; ISO8601-ish string
   data   ; 对应 prefab 数据
  ) ; end fields
  #:prefab
) ; end struct rec

(struct session
  (path    ; path-string
   log     ; datum-log
   fresh?  ; boolean — 本次运行新建的文件（用于关闭时清理空会话）
   nmsg    ; box of exact — 本次运行追加的消息数
   idx     ; session-index：写日志时同步追加索引条目
   [listener #:mutable]   ; #f 或 (序号 记录 → void)：每写一条记录后调用（锁外；RPC 用它把记录实时发成界面条目）
  ) ; end fields
) ; end struct session

(define (iso-now)
  (define d (seconds->date (current-seconds)))
  (define (p2 n) (if (< n 10) f"0{n}" f"{n}"))
  f"{(date-year d)}-{(p2 (date-month d))}-{(p2 (date-day d))}T{(p2 (date-hour d))}:{(p2 (date-minute d))}:{(p2 (date-second d))}"
) ; end define iso-now

;; 打开或新建；新文件写入 meta 首记录
(define (session-open! path cfg)
  (define fresh? (not (file-exists? path)))
  (define lg (datum-log-open! path))
  (define s (session path lg fresh? (box 0) (index-open path #:writable? #t) #f))
  (when fresh?
    ;; 密钥不落存档：meta 里的 config 去掉 api-key（恢复时由 PI_API_KEY / 帐号重新解析：main.rkt resume 本来就覆盖它，
    ;; RPC 切换会话用当前 config）
    (append-rec! s (rec 'meta (iso-now) (list 'pi2 1 (struct-copy config cfg [api-key #f]))))
  ) ; end when
  s
) ; end define session-open!

(define (session-append-msg! s msg)
  (set-box! (session-nmsg s) (add1 (unbox (session-nmsg s))))
  (append-rec! s (rec 'msg (iso-now) msg))
) ; end define session-append-msg!

(define (session-append-compact! s msgs)
  (append-rec! s (rec 'compact (iso-now) msgs))
) ; end define session-append-compact!

(define (session-append-turn! s what)
  (append-rec! s (rec 'turn (iso-now) what))
) ; end define session-append-turn!

;; 一轮结束后落盘：
;;   装了记录去向（current-record-sink）：消息已在追加时记下，这里只记用量增量；
;;   没装（旧调用方）：轮后历史以轮前历史为前缀 → 追加新增消息；不是（轮内压缩过）→ 记一条 compact（压缩后的完整历史，本轮消息都在里面）
(define (session-persist-turn! s st-before st-after)
  (define hb (agent-state-history st-before))
  (define ha (agent-state-history st-after))
  (define b (pvector-length hb))
  (define a (pvector-length ha))
  (unless (current-record-sink)
    (cond
      [(and (>= a b) (or (= b 0) (eq? (pvector-ref ha (sub1 b)) (pvector-ref hb (sub1 b)))))
       (for ([i (in-range b a)]) (session-append-msg! s (pvector-ref ha i)))]
      [else (session-append-compact! s (hasheq 'history (for/list ([m (in-pvector ha)]) m) 'usage (agent-state-token-usage st-after)))]))
  (define ub (agent-state-token-usage st-before))
  (define ua (agent-state-token-usage st-after))
  (define du (usage (- (usage-input-tokens ua) (usage-input-tokens ub)) (- (usage-output-tokens ua) (usage-output-tokens ub))))
  (unless (equal? du usage-zero) (session-append-usage! s du))
) ; end define session-persist-turn!

;; 写一条记录：日志与索引在同一把锁里追加（偏移一致）
(define (append-rec! s r)
  (define ix (session-idx s))
  (define pos (box #f))
  (datum-log-append! (session-log s) r #:after (lambda (off len) (index-append! ix r off len) (set-box! pos (sub1 (index-count ix)))))
  (let ([l (session-listener s)]) (when l (l (unbox pos) r)))
) ; end define append-rec!

(define (session-on-append! s f) (set-session-listener! s f))
(define (session-index s) (session-idx s))
(define (session-record-count s) (index-count (session-idx s)))
;; 序号区间 [from, to) 的记录：→ (listof (cons 序号 记录))
(define (session-read-range s from to)
  (define ix (session-idx s))
  (define n (index-count ix))
  (index-read ix (range (max 0 from) (min n (max from to))))
) ; end define session-read-range

;; 界面记录（perm / perm-answer / note / req / tool / partial）：data 是 hash
(define (session-append-rec! s type data)
  (append-rec! s (rec type (iso-now) data))
) ; end define session-append-rec!

;; 会话 id = 文件名去掉 .rktd（自动化提交的元数据里用）
(define (session-id s)
  (let-values ([(_d n _x) (split-path (session-path s))]) (regexp-replace #rx"[.]rktd$" (path->string n) "")))

;; 内核的记录去向（model.rkt current-record-sink）→ 写这个会话文件
(define (session-record-sink s)
  (lambda (kind payload)
    (case kind
      [(msg) (session-append-msg! s payload)]
      [(compact) (session-append-compact! s payload)]
      [(turn) (session-append-turn! s payload)]
      [(perm perm-answer note req tool partial sub tests) (session-append-rec! s kind payload)]
      [else (void)]))
) ; end define session-record-sink

(define (session-append-usage! s u)
  (append-rec! s (rec 'usage (iso-now) u))
) ; end define session-append-usage!

;; 会话的工作区（聊天会话：'kind "chat" 'dir 目录）
(define (session-append-workspace! s data)
  (append-rec! s (rec 'workspace (iso-now) data))
) ; end define session-append-workspace!

;; 会话的工作区记录（最后一条）→ hash 或 #f（经索引只读这一条）
(define (session-workspace path)
  (with-handlers ([exn:fail? (lambda (_e) #f)])
    (and (file-exists? path)
         (let* ([ix (index-open path)]
                [code (type-code 'workspace)]
                [i (for/last ([i (in-range (index-count ix))] #:when (= (ientry-type (index-ref ix i)) code)) i)])
           (and i (let ([r (cdr (car (index-read ix (list i))))]) (and (rec? r) (hash? (rec-data r)) (rec-data r))))))))

;; 用户改的标题（会话列表优先用最后一条）
;; 改名：数据是字符串（手动）；auto? → (hasheq 'title 名 'auto #t)（本机模型起的，§11.4 B）。读的时候都认，最后一条算数
(define (session-append-title! s title #:auto? [auto? #f])
  (append-rec! s (rec 'title (iso-now) (if auto? (hasheq 'title title 'auto #t) title)))
) ; end define session-append-title!
(define (title-of-rec-data d) (cond [(string? d) d] [(and (hash? d) (string? (hash-ref d 'title #f))) (hash-ref d 'title)] [else #f]))

;; 文件改动 diff（只给界面看，不进 agent-state）：(rec 'diff ts (list 工具调用id diff-jsexpr))
;; 重放跳过它（未知类型分支）；旧版本读到也跳过。
(define (session-append-diff! s id diff)
  (append-rec! s (rec 'diff (iso-now) (list id diff)))
) ; end define session-append-diff!

;; 效果账本（effects.rkt）：(rec 'effect ts effect)，同一 op 后写的覆盖先写的。重放跳过它。
(define (session-append-effect! s e)
  (append-rec! s (rec 'effect (iso-now) e))
) ; end define session-append-effect!

;; → 每个 op 的最新记录（顺序不保证）
(define (session-effects path)
  (with-handlers ([exn:fail? (lambda (_e) '())])
    (hash-values
     (for/fold ([h (hash)]) ([d (in-list (records-of-type path 'effect))] #:when (and (rec? d) (eq? (rec-type d) 'effect) (vector? (struct->vector (rec-data d)))))
       (define e (rec-data d))
       (define v (struct->vector e))
       ;; prefab effect：#s(effect op state method url body-fp idem-key at response)
       (if (and (>= (vector-length v) 2) (string? (vector-ref v 1))) (hash-set h (vector-ref v 1) e) h))))
) ; end define session-effects

;; 可能引用媒体的记录（带媒体的消息、压缩记录）：blob 垃圾回收用（src/media.rkt blobs-gc!）
(define (session-media-records path)
  (if (file-exists? path)
      (let* ([ix (index-open path)]
             [msg (type-code 'msg)] [cmp (type-code 'compact)])
        (map cdr (index-read ix (index-positions ix (lambda (e) (or (= (ientry-type e) cmp)
                                                                    (and (= (ientry-type e) msg) (positive? (bitwise-and (ientry-flags e) FLAG-MEDIA)))))))))
      '())
) ; end define session-media-records

;; 某类型的全部记录（经索引只读这些记录，不解析整个文件）
(define (records-of-type path type)
  (if (file-exists? path)
      (let ([ix (index-open path)])
        (map cdr (index-read ix (index-positions ix (lambda (e) (= (ientry-type e) (type-code type)))))))
      '())
) ; end define records-of-type

;; 会话选用的路由池：(rec 'route ts "@名")；"" = 不再走路由。重放跳过它。
(define (session-append-route! s ref)
  (append-rec! s (rec 'route (iso-now) ref))
) ; end define session-append-route!

;; 最后一次路由记录：→ "@名" | "" | #f（从没记过）
(define (session-route path)
  (with-handlers ([exn:fail? (lambda (_e) #f)])
    (for/fold ([r #f]) ([d (in-list (records-of-type path 'route))] #:when (and (rec? d) (eq? (rec-type d) 'route) (string? (rec-data d))))
      (rec-data d)))
) ; end define session-route

;; 会话文件里的全部 diff → hash 工具调用id → diff（坏文件 / 截断尾：返回已读到的部分）
(define (session-diffs path)
  (define h (make-hash))
  (with-handlers ([exn:fail? (lambda (_e) (void))])
    (for ([d (in-list (records-of-type path 'diff))] #:when (and (rec? d) (eq? (rec-type d) 'diff)))
      (define v (rec-data d))
      (when (and (list? v) (= (length v) 2) (string? (car v)))
        (hash-set! h (car v) (cadr v)))
    ) ; end for
    ;; 新会话：diff 在工具结束记录（tool）里
    (for ([d (in-list (records-of-type path 'tool))] #:when (and (rec? d) (hash? (rec-data d))))
      (define v (rec-data d))
      (when (and (string? (hash-ref v 'id #f)) (hash? (hash-ref v 'diff #f)))
        (hash-set! h (hash-ref v 'id) (hash-ref v 'diff)))
    ) ; end for
  ) ; end with-handlers
  h
) ; end define session-diffs

;; 本次新建、还没有消息：关闭（切到别的会话 / 退出）时会被删掉（界面把它标成临时的）
(define (session-ephemeral? s) (and (session-fresh? s) (zero? (unbox (session-nmsg s)))))

;; 关闭；若是本次新建且从未落过消息（仅 meta），自动清理该空会话文件。
(define (session-close! s)
  (datum-log-close! (session-log s))
  (index-close! (session-idx s))
  (when (and (session-fresh? s) (zero? (unbox (session-nmsg s)))
             (file-exists? (session-path s)))
    (delete-file (session-path s))
    (delete-index! (session-path s))
  ) ; end when
) ; end define session-close!

(define (delete-index! path)
  (define ip (index-path path))
  (when (file-exists? ip) (delete-file ip))
) ; end define delete-index!

;; 旧会话文件的 meta 里可能存了 api-key（2026-10-23 之前）：重写该文件，meta 的 api-key 置 #f，其余记录原样。
;; 临时文件 + rename（原子）；没有 key 的文件不动。→ 是否改了
(define (session-scrub-key! path)
  (define ds (for/list ([d (in-datum-log path)]) d))
  (define (keyed? d) (and (rec? d) (eq? (rec-type d) 'meta) (list? (rec-data d)) (>= (length (rec-data d)) 3)
                          (config? (third (rec-data d))) (config-api-key (third (rec-data d)))))
  (cond
    [(not (ormap keyed? ds)) #f]
    [else
     (define tmp (path-add-extension path #".scrub"))
     (call-with-output-file tmp #:exists 'truncate
       (lambda (out)
         (for ([d (in-list ds)])
           (pretty-write (if (keyed? d)
                             (let ([m (rec-data d)])
                               (rec 'meta (rec-ts d) (list* (first m) (second m) (struct-copy config (third m) [api-key #f]) (cdddr m))))
                             d)
                         out))))
     (rename-file-or-directory tmp path #t)
     (delete-index! path)                                         ; 文件重写过：索引作废（下次打开时重建）
     #t])
) ; end define session-scrub-key!

;; 删除会话文件（幂等）
(define (session-delete! path)
  (when (file-exists? path) (delete-file path))
  (delete-index! path)
) ; end define session-delete!

;; ------------------------------------------------------------ 回收站（design-workspace.md §11 A：删除可撤销，不再确认）
;; <会话目录>/trash/<名字>.rktd（+ .idx；聊天会话的目录挪成 <名字>.chat/）。删除 = 挪进去（修改时间设成现在），恢复 = 挪回来；
;; 保留 TRASH-DAYS 天，过期的在删除时与内核启动时清掉。
(define TRASH-DAYS 7)
(define (trash-dir-of path) (let-values ([(base _n _d) (split-path (path->complete-path path))]) (build-path base "trash")))
(define (trash-path-of path) (let-values ([(_b name _d) (split-path path)]) (build-path (trash-dir-of path) name)))
(define (stem-of p) (regexp-replace #rx"[.]rktd$" (let-values ([(_b n _d) (split-path p)]) (path->string n)) ""))
(define (move! from to)
  (when (or (file-exists? to) (directory-exists? to)) (if (directory-exists? to) (delete-directory/files to) (delete-file to)))
  (rename-file-or-directory from to))
;; → 回收站里的路径。chat-dir：聊天会话的目录（一起挪）或 #f
(define (session-trash! path #:chat-dir [chat-dir #f])
  (define t (trash-path-of path))
  (make-directory* (trash-dir-of path))
  (when (file-exists? path) (move! path t) (file-or-directory-modify-seconds t (current-seconds)))
  (define ix (index-path path))
  (when (file-exists? ix) (move! ix (index-path t)))
  (when (and chat-dir (directory-exists? chat-dir))
    (move! chat-dir (build-path (trash-dir-of path) (string-append (stem-of path) ".chat"))))
  t
) ; end define session-trash!
;; 原路径 → 挪回来（原路径已有文件 → 报错）；chat-dir：恢复后聊天目录放回哪里（会话的 workspace 记录里的目录）
(define (session-restore! path #:chat-dir [chat-dir-of-restored (lambda (_p) #f)])
  (define t (trash-path-of path))
  (unless (file-exists? t) (raise-user-error 'session-restore! "not in the trash: ~a" path))
  (when (file-exists? path) (raise-user-error 'session-restore! "a session already exists at ~a" path))
  (move! t path)
  (when (file-exists? (index-path t)) (move! (index-path t) (index-path path)))
  (define held (build-path (trash-dir-of path) (string-append (stem-of path) ".chat")))
  (define dest (chat-dir-of-restored path))
  (when (and dest (directory-exists? held))
    (make-parent-directory* dest)
    (move! held dest))
  path
) ; end define session-restore!
;; 清掉回收站里超过 days 天的（会话文件、索引、聊天目录）；→ 清掉的会话名
(define (trash-purge! dir #:days [days TRASH-DAYS] #:now [now (current-seconds)])
  (define td (build-path dir "trash"))
  (if (directory-exists? td)
      (for/list ([f (in-list (directory-list td #:build? #t))]
                 #:when (and (file-exists? f) (regexp-match? #rx"[.]rktd$" (path->string f))
                             (> (- now (file-or-directory-modify-seconds f)) (* days 86400))))
        (define stem (stem-of f))
        (delete-file f)
        (let ([ix (index-path f)]) (when (file-exists? ix) (delete-file ix)))
        (let ([c (build-path td (string-append stem ".chat"))]) (when (directory-exists? c) (delete-directory/files c)))
        stem)
      '())
) ; end define trash-purge!

;; ------------------------------------------------------------ 流式重放

;; compact 记录的数据：P0 时是消息表；之后是 (hasheq 'history 消息表 'usage 当时的累计用量)
(define (compact-history d)
  (define v (rec-data d))
  (cond [(and (list? v) (andmap message? v)) v]
        [(and (hash? v) (list? (hash-ref v 'history #f)) (andmap message? (hash-ref v 'history))) (hash-ref v 'history)]
        [else #f])
) ; end define compact-history
(define (compact-usage d) (let ([v (rec-data d)]) (and (hash? v) (usage? (hash-ref v 'usage #f)) (hash-ref v 'usage))))
(define (history->pvector msgs) (for/fold ([pv (pvector)]) ([m (in-list msgs)]) (pvector-cons-right pv m)))

;; 重放器：一条条喂记录 → 状态。全量重放与从压缩点开始的快速重放共用
(define (make-replayer override-cfg)
  (define st0 (box #f))
  (define turn-start (box #f))                        ; 还没 'end 的那个 'begin 时的状态（'revert 退回它）
  (define msg-count (box 0))
  (define (step! d)
    (when (rec? d)
      (case (rec-type d)
        [(meta)
         (define meta (rec-data d))
         (define cfg (or override-cfg
                         (if (and (list? meta) (>= (length meta) 3) (config? (third meta))) (third meta) (default-config))))
         (set-box! st0 (make-initial-state cfg))]
        [(msg)
         (when (and (unbox st0) (message? (rec-data d)))
           (set-box! st0 (state-append (unbox st0) (rec-data d)))
           (set-box! msg-count (add1 (unbox msg-count))))]
        [(usage)
         (when (and (unbox st0) (usage? (rec-data d)))
           (set-box! st0 (state-add-usage (unbox st0) (rec-data d))))]
        [(compact)
         (define h (compact-history d))
         (when (and (unbox st0) h)
           (set-box! st0 (struct-copy agent-state (unbox st0) [history (history->pvector h)])))]
        [(turn)
         (case (rec-data d)
           [(begin) (set-box! turn-start (unbox st0))]
           [(end) (set-box! turn-start #f)]
           [(revert) (when (unbox turn-start) (set-box! st0 (unbox turn-start)) (set-box! turn-start #f))]
           [else (void)])]
        [else (void)])))                                 ; 其他记录（界面记录、未知类型）：不影响模型上下文
  (values step! (lambda () (unbox st0)) (lambda () (unbox msg-count)) (lambda (st) (set-box! st0 st)))
) ; end define make-replayer

;; 重建 agent-state；meta 里的 config 可被 override 覆盖。
;; 可传 #:stop-after 在第 k 条 msg 后停（/resume 到历史某点、fork 会话）。
;; 有压缩记录时走快速路径：经索引只读 meta 与最后一次压缩之后的记录（与会话总长度无关）
(define (session-replay path #:config [override-cfg #f] #:stop-after [stop-after #f])
  (or (and (not stop-after) (file-exists? path) (fast-replay path override-cfg))
      (full-replay path override-cfg stop-after))
) ; end define session-replay

(define (full-replay path override-cfg stop-after)
  (define-values (step! result count _set) (make-replayer override-cfg))
  (for ([d (in-datum-log path)])
    #:break (and stop-after (>= (count) stop-after))
    (step! d))
  (or (result) (make-initial-state (or override-cfg (default-config))))
) ; end define full-replay

;; → 状态，或 #f（没有可用的压缩点：走全量）
(define (fast-replay path override-cfg)
  (with-handlers ([exn:fail? (lambda (_e) #f)])
    (define ix (index-open path))
    (define n (index-count ix))
    (define ccode (type-code 'compact))
    (define tcode (type-code 'turn))
    (define c (for/first ([i (in-range (sub1 n) -1 -1)] #:when (= (ientry-type (index-ref ix i)) ccode)) i))
    (define first-turn (and c (for/first ([i (in-range (add1 c) n)] #:when (= (ientry-type (index-ref ix i)) tcode))
                                (ientry-role (index-ref ix i)))))
    (cond
      [(or (not c) (< n 1) (not (= (ientry-type (index-ref ix 0)) (type-code 'meta)))) #f]
      [(eqv? first-turn 3) #f]                          ; 压缩之后第一个轮标记是撤回：那次压缩被撤回了
      [else
       (define recs (index-read ix (cons 0 (range c n))))
       (define cd (cdr (cadr recs)))
       (define u (compact-usage cd))
       (define h (compact-history cd))
       (cond
         [(not (and u h)) #f]                            ; P0 时的压缩记录（没有累计用量）：走全量
         [else
          (define-values (step! result _count set-state!) (make-replayer override-cfg))
          (step! (cdr (car recs)))                       ; meta
          (set-state! (struct-copy agent-state (result) [history (history->pvector h)] [token-usage u]))
          (for ([pr (in-list (cddr recs))]) (step! (cdr pr)))
          (result)])]))
) ; end define fast-replay

;; ------------------------------------------------------------ 会话列表与元信息

;; 列表/选择器用的富元信息
(struct session-info
  (path     ; path-string
   ts       ; string — meta 时间戳
   model    ; string — 存档 config 的 model
   title    ; string — 首条 user 消息派生的标题
   nmsg     ; exact — 消息记录数
   mtime    ; exact — 文件修改秒（排序用）
   workdir  ; (or/c #f string) — 工作目录：聊天会话 = 它的聊天目录；否则 meta 里开始时的 workdir
   chat?    ; boolean — 聊天会话（匿名工作区）
   titled?  ; boolean — 有过 title 记录（手动或自动）：自动起标题只给没有的起
  ) ; end fields
  #:transparent
) ; end struct session-info

;; 首条 user 消息 → 单行标题（去换行、截断）
(define (title-of-message m)
  (define raw (string-normalize-spaces (message-text m)))
  (cond
    [(= (string-length raw) 0) "(no text)"]
    [(> (string-length raw) 48) (string-append (substring raw 0 48) "…")]
    [else raw]
  ) ; end cond
) ; end define title-of-message

;; 单个会话文件 → session-info：经索引只读 meta 与第一条用户消息（消息数、是否分叉从索引条目来）；索引不可用 → 扫描整个文件
(define (read-session-info path)
  (or (with-handlers ([exn:fail? (lambda (_e) #f)]) (read-session-info/index path))
      (read-session-info/scan path))
) ; end define read-session-info

(define (read-session-info/index path)
  (define ix (index-open path))
  (define n (index-count ix))
  (define mcode (type-code 'msg))
  (define nmsg (for/sum ([i (in-range n)]) (if (= (ientry-type (index-ref ix i)) mcode) 1 0)))
  (define fork? (for/or ([i (in-range n)]) (= (ientry-type (index-ref ix i)) (type-code 'fork))))
  (define first-user (for/first ([i (in-range n)] #:when (let ([e (index-ref ix i)]) (and (= (ientry-type e) mcode) (= (ientry-role e) ROLE-USER)))) i))
  (define recs (index-read ix (if (and first-user (> n 0)) (list 0 first-user) (if (> n 0) (list 0) '()))))
  (define meta (and (pair? recs) (cdr (car recs))))
  (define user (and first-user (pair? recs) (pair? (cdr recs)) (cdr (cadr recs))))
  ;; 改过名：最后一条 title 记录
  (define tcode (type-code 'title))
  (define last-title (for/last ([i (in-range n)] #:when (= (ientry-type (index-ref ix i)) tcode)) i))
  (define renamed (and last-title (let ([r (cdr (car (index-read ix (list last-title))))]) (and (rec? r) (title-of-rec-data (rec-data r))))))
  (define wcode (type-code 'workspace))
  (define last-ws (for/last ([i (in-range n)] #:when (= (ientry-type (index-ref ix i)) wcode)) i))
  (define ws (and last-ws (let ([r (cdr (car (index-read ix (list last-ws))))]) (and (rec? r) (hash? (rec-data r)) (rec-data r)))))
  (define cfg (and (rec? meta) (eq? (rec-type meta) 'meta) (list? (rec-data meta)) (>= (length (rec-data meta)) 3)
                   (config? (third (rec-data meta))) (third (rec-data meta))))
  (session-info (if (string? path) path (path->string path))
                (if (rec? meta) (rec-ts meta) "?") (if cfg (config-model cfg) "?")
                (or renamed (string-append (if fork? "⑂ " "") (if (and (rec? user) (message? (rec-data user))) (title-of-message (rec-data user)) "(empty)")))
                nmsg
                (with-handlers ([exn:fail? (lambda (_e) 0)]) (file-or-directory-modify-seconds path))
                (or (and ws (let ([d (hash-ref ws 'dir #f)]) (and (string? d) d))) (and cfg (config-workdir cfg)))
                (and ws (equal? (hash-ref ws 'kind #f) "chat"))
                (and last-title #t))
) ; end define read-session-info/index

;; 扫描单个会话文件 → session-info（容忍截断尾；坏文件返回带占位的 info）
(define (read-session-info/scan path)
  (define ts (box "?")) (define model (box "?"))
  (define title (box #f)) (define nmsg (box 0)) (define fork? (box #f)) (define renamed (box #f)) (define wdir (box #f)) (define ws (box #f))
  (with-handlers ([exn:fail? (lambda (_e) (void))])
    (for ([d (in-datum-log path)] #:when (rec? d))
      (case (rec-type d)
        [(meta)
         (set-box! ts (rec-ts d))
         (define m (rec-data d))
         (when (and (list? m) (>= (length m) 3) (config? (third m)))
           (set-box! model (config-model (third m)))
           (set-box! wdir (config-workdir (third m)))
         ) ; end when
        ] ; end meta
        [(fork) (set-box! fork? #t)]
        [(title) (when (title-of-rec-data (rec-data d)) (set-box! renamed (title-of-rec-data (rec-data d))))]
        [(workspace) (when (hash? (rec-data d)) (set-box! ws (rec-data d)))]
        [(msg)
         (set-box! nmsg (add1 (unbox nmsg)))
         (when (and (not (unbox title)) (message? (rec-data d))
                    (eq? (message-role (rec-data d)) 'user))
           (set-box! title (title-of-message (rec-data d)))
         ) ; end when
        ] ; end msg
        [else (void)]
      ) ; end case
    ) ; end for
  ) ; end with-handlers
  (session-info (if (string? path) path (path->string path))
                (unbox ts) (unbox model)
                (or (unbox renamed) (string-append (if (unbox fork?) "⑂ " "") (or (unbox title) "(empty)"))) (unbox nmsg)
                (with-handlers ([exn:fail? (lambda (_e) 0)])
                  (file-or-directory-modify-seconds path))
                (or (and (unbox ws) (let ([d (hash-ref (unbox ws) 'dir #f)]) (and (string? d) d))) (unbox wdir))
                (and (unbox ws) (equal? (hash-ref (unbox ws) 'kind #f) "chat"))
                (and (unbox renamed) #t))
) ; end define read-session-info

;; session-info → 列表/选择器单行文本
(define (session-info->line info)
  (format "~a  ~a  ~a msgs  ~a"
          (session-info-ts info) (session-info-model info)
          (session-info-nmsg info) (session-info-title info))
) ; end define session-info->line

;; 目录下全部会话 info，按修改时间**降序**（最近在前）
(define (session-infos dir)
  (if (directory-exists? dir)
      (sort
       (for/list ([f (in-list (directory-list dir #:build? #t))]                ; 只看这一层：trash/、chats/ 里的不是会话
                  #:when (and (file-exists? f)
                              (regexp-match? #rx"\\.rktd$" (path->string f))))
         (read-session-info f))
       > #:key session-info-mtime)
      '()
  ) ; end if
) ; end define session-infos

;; 最近一次会话的路径（-c/--continue、崩溃恢复），无则 #f
(define (session-latest dir)
  (define infos (session-infos dir))
  (if (null? infos) #f (session-info-path (car infos)))
) ; end define session-latest

;; ------------------------------------------------------------ 派生/分叉

;; 从 src 在第 n 条消息处分叉出新会话：重放前 n 条 → 写入 dir 下新文件，返回新路径。
(define (session-fork! src dir #:at [n #f])
  (define st (session-replay src #:stop-after n))
  (define new-path (fresh-session-path dir))
  (define s (session-open! new-path (agent-state-config st)))
  (append-rec! s (rec 'fork (iso-now) (list (if (path? src) (path->string src) src) n)))   ; 来源（列表标题加「⑂」）
  (for ([m (in-pvector (agent-state-history st))])
    (session-append-msg! s m)
  ) ; end for
  ;; 已经发生的远端效果跟着分叉走（分叉出的会话再执行同一个 op 时返回记录的结果，不重复执行）
  (for ([e (in-list (session-effects src))]) (session-append-effect! s e))
  ;; 带上分叉点之前那些工具调用的 diff
  (define diffs (session-diffs src))
  (for* ([m (in-pvector (agent-state-history st))]
         [b (in-list (message-blocks m))]
         #:when (and (tool-result-block? b) (hash-ref diffs (tool-result-block-tool-use-id b) #f)))
    (session-append-diff! s (tool-result-block-tool-use-id b) (hash-ref diffs (tool-result-block-tool-use-id b)))
  ) ; end for*
  (session-close! s)
  new-path
) ; end define session-fork!

;; 默认会话文件名：data/<iso-date>-<4位随机>.rktd
(define (fresh-session-path [dir "data"])
  (define d (seconds->date (current-seconds)))
  (define (p2 n) (if (< n 10) f"0{n}" f"{n}"))
  (define tag (number->string (+ 1000 (random 9000))))
  (build-path dir
              f"{(date-year d)}{(p2 (date-month d))}{(p2 (date-day d))}-{(p2 (date-hour d))}{(p2 (date-minute d))}-{tag}.rktd"
  ) ; end build-path
) ; end define fresh-session-path

;; ---------------------------------------------------------------- provide

(provide
 session-trash! session-restore! trash-purge! TRASH-DAYS
 (struct-out rec)
 session-append-compact! session-append-turn! session-record-sink session-persist-turn! session-append-rec!
 session-append-title! session-append-workspace! session-workspace records-of-type session-on-append! session-index session-record-count session-read-range session-media-records session-ephemeral?
 session?
 session-path
 session-id
 session-open!
 session-append-msg!
 session-append-usage!
 session-append-diff!
 session-diffs
 session-append-route!
 session-route
 session-append-effect!
 session-effects
 session-scrub-key!
 session-close!
 session-delete!
 session-replay
 (struct-out session-info)
 read-session-info
 session-info->line
 session-infos
 session-latest
 session-fork!
 fresh-session-path
) ; end provide
