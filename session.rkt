#lang racket-tstring
;; session.rkt — .rktd 会话持久化与流式重放（design.md §4.6 / §5.6）
;; transcript 即真相源；重放复用运行时同一套 state-append 迁移函数。

(require
 racket/list
 racket/string
 racket/file
 racket/date
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
  (path   ; path-string
   log    ; datum-log
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
  (define s (session path lg))
  (when fresh?
    (datum-log-append! lg (rec 'meta (iso-now) (list 'pi2 1 cfg)))
  ) ; end when
  s
) ; end define session-open!

(define (session-append-msg! s msg)
  (datum-log-append! (session-log s) (rec 'msg (iso-now) msg))
) ; end define session-append-msg!

(define (session-append-usage! s u)
  (datum-log-append! (session-log s) (rec 'usage (iso-now) u))
) ; end define session-append-usage!

(define (session-close! s)
  (datum-log-close! (session-log s))
) ; end define session-close!

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

;; ------------------------------------------------------------ 会话列表

(struct session-meta
  (path     ; path-string
   ts       ; string — meta 记录时间
  ) ; end fields
  #:transparent
) ; end struct session-meta

;; 每文件只读首个 datum
(define (session-list dir)
  (if (directory-exists? dir)
      (for/list ([f (in-directory dir)]
                 #:when (and (file-exists? f)
                             (regexp-match? #rx"\\.rktd$" (path->string f))
                        ) ; end and
                ) ; end binding
        (define d (datum-log-first f))
        (session-meta (path->string f)
                      (if (rec? d) (rec-ts d) "?")
        ) ; end session-meta
      ) ; end for/list
      '()
  ) ; end if
) ; end define session-list

;; 默认会话文件名：sessions/<iso-date>-<4位随机>.rktd
(define (fresh-session-path [dir "sessions"])
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
 session-replay
 (struct-out session-meta)
 session-list
 fresh-session-path
) ; end provide
