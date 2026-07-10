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
) ; end require

;; 记录：prefab，直接 write 进 datum log
(struct rec
  (type   ; 'meta | 'msg | 'usage | 'compact
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
  (define s (session path lg fresh? (box 0)))
  (when fresh?
    (datum-log-append! lg (rec 'meta (iso-now) (list 'pi2 1 cfg)))
  ) ; end when
  s
) ; end define session-open!

(define (session-append-msg! s msg)
  (set-box! (session-nmsg s) (add1 (unbox (session-nmsg s))))
  (datum-log-append! (session-log s) (rec 'msg (iso-now) msg))
) ; end define session-append-msg!

(define (session-append-usage! s u)
  (datum-log-append! (session-log s) (rec 'usage (iso-now) u))
) ; end define session-append-usage!

;; 关闭；若是本次新建且从未落过消息（仅 meta），自动清理该空会话文件。
(define (session-close! s)
  (datum-log-close! (session-log s))
  (when (and (session-fresh? s) (zero? (unbox (session-nmsg s)))
             (file-exists? (session-path s)))
    (delete-file (session-path s))
  ) ; end when
) ; end define session-close!

;; 删除会话文件（幂等）
(define (session-delete! path)
  (when (file-exists? path) (delete-file path))
) ; end define session-delete!

;; ------------------------------------------------------------ 流式重放

;; 逐 datum fold 重建 agent-state；meta 里的 config 可被 override 覆盖。
;; 可传 #:stop-after 在第 k 条 msg 后停（/resume 到历史某点、fork 会话）。
(define (session-replay path #:config [override-cfg #f] #:stop-after [stop-after #f])
  (define msg-count (box 0))
  (define st0 (box #f))
  (for ([d (in-datum-log path)])
    #:break (and stop-after (>= (unbox msg-count) stop-after))
    (when (rec? d)
      (case (rec-type d)
        [(meta)
         (define meta (rec-data d))
         (define cfg
           (or override-cfg
               (if (and (list? meta) (>= (length meta) 3) (config? (third meta)))
                   (third meta)
                   (default-config)
               ) ; end if
           ) ; end or
         ) ; end define cfg
         (set-box! st0 (make-initial-state cfg))
        ] ; end meta case
        [(msg)
         (when (and (unbox st0) (message? (rec-data d)))
           (set-box! st0 (state-append (unbox st0) (rec-data d)))
           (set-box! msg-count (add1 (unbox msg-count)))
         ) ; end when
        ] ; end msg case
        [(usage)
         (when (and (unbox st0) (usage? (rec-data d)))
           (set-box! st0 (state-add-usage (unbox st0) (rec-data d)))
         ) ; end when
        ] ; end usage case
        [else (void)]                       ; 未知记录类型：向前兼容，跳过
      ) ; end case
    ) ; end when
  ) ; end for
  (or (unbox st0)
      (make-initial-state (or override-cfg (default-config)))
  ) ; end or
) ; end define session-replay

;; ------------------------------------------------------------ 会话列表与元信息

;; 列表/选择器用的富元信息
(struct session-info
  (path     ; path-string
   ts       ; string — meta 时间戳
   model    ; string — 存档 config 的 model
   title    ; string — 首条 user 消息派生的标题
   nmsg     ; exact — 消息记录数
   mtime    ; exact — 文件修改秒（排序用）
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

;; 扫描单个会话文件 → session-info（容忍截断尾；坏文件返回带占位的 info）
(define (read-session-info path)
  (define ts (box "?")) (define model (box "?"))
  (define title (box #f)) (define nmsg (box 0))
  (with-handlers ([exn:fail? (lambda (_e) (void))])
    (for ([d (in-datum-log path)] #:when (rec? d))
      (case (rec-type d)
        [(meta)
         (set-box! ts (rec-ts d))
         (define m (rec-data d))
         (when (and (list? m) (>= (length m) 3) (config? (third m)))
           (set-box! model (config-model (third m)))
         ) ; end when
        ] ; end meta
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
                (or (unbox title) "(empty)") (unbox nmsg)
                (with-handlers ([exn:fail? (lambda (_e) 0)])
                  (file-or-directory-modify-seconds path)))
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
       (for/list ([f (in-directory dir)]
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
  (for ([m (in-pvector (agent-state-history st))])
    (session-append-msg! s m)
  ) ; end for
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
 (struct-out rec)
 session?
 session-path
 session-open!
 session-append-msg!
 session-append-usage!
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
