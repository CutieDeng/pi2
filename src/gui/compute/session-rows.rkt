#lang racket/base
;; session-rows.rkt — 侧栏的行（design-workspace.md W2 / W3）：会话列表（各主机内核的行 + 主机标题行）× 目录 catalog 的会话属性 → 显示的行。
;; 按块处理（块 = 一个主机标题行 + 它下面的会话行；单主机时整个列表一块）：
;;   置顶的排在块的前面（各自保持原来的顺序）；行上标 'pinned。
;;   归档的（当前会话、临时会话除外：它们总是显示）收进块末尾的分组行「已归档（n）」（'kind "group"），分组展开时跟在后面；行上标 'archived。
;; 时间分段（design-workspace.md §11.4 B：「少做」——久没动的不用手动归档）：#:now 给了就按最近活动（行上 'mtime，秒）分段：
;;   置顶 / 今天 / 昨天 / 7 天内 / N 天内 / 更早（N = #:older-days；更早缺省收起）；按本地日历日算。每段一个分组行（'section）。
;;   收起的段只留当前会话的行（点分组行总有看得见的变化）。没有 'mtime 的行算今天。
;; 纯函数：属性、展开状态、现在的时刻由调用方给（回放用录制时的时刻）。

(require racket/list)

(define (header? r) (equal? (hash-ref r 'kind #f) "header"))

;; rows：(listof hasheq)；attr：(行 键) → 值（置顶 'pinned、归档 'archived）；open?：(分组键) → 是否展开；label：(个数) → 分组行文字
;; preview：#f | (行) → #f 或 (listof (cons 文字 种类))——变更清单审阅时的预览，放进行的 'preview
;; open?：(分组键 缺省) → 是否展开
(define SECTIONS '(pinned today yesterday week recent older))
(define (local-day secs tz) (floor (/ (+ secs tz) 86400)))
(define (section-of r now older-days tz)
  (define m (let ([v (hash-ref r 'mtime #f)]) (if (real? v) v now)))
  (define dd (- (local-day now tz) (local-day m tz)))
  (cond [(<= dd 0) 'today] [(= dd 1) 'yesterday] [(< dd 7) 'week] [(< dd older-days) 'recent] [else 'older]))
;; 分组方式（W3）：mode 'recent（时间分段 / 平铺）| 'groups——groups-of：(行) → (listof (cons 组键 组名))（项目 / 工作区 / super-workspace，
;;   拼装层从目录算）；一个会话属于几个组就出现几次；没有组的归「未归类」（ungrouped-label）；组按名字排；置顶的仍在最前；已归档的仍在最后
(define (arrange-session-rows rows #:attr attr #:open? open? #:archived-label label #:preview [preview #f]
                              #:now [now #f] #:older-days [older-days 14] #:section-label [section-label (lambda (sec n) (format "~a (~a)" sec n))]
                              #:groups-of [groups-of #f] #:ungrouped-label [ungrouped-label (lambda (n) (format "Ungrouped (~a)" n))]
                              #:group-label [group-label (lambda (name n) (format "~a（~a）" name n))])
  (define (block-rows head body)
    (define host (if head (hash-ref head 'host "") ""))
    (define marked (for/list ([r (in-list body)])
                     (define p? (and (attr r 'pinned) #t))
                     (define a? (and (attr r 'archived) #t))
                     (define pv (and preview (preview r)))
                     (let* ([r (if p? (hash-set r 'pinned #t) r)] [r (if a? (hash-set r 'archived #t) r)] [r (if (pair? pv) (hash-set r 'preview pv) r)]) r)))
    (define (hidden? r) (and (hash-ref r 'archived #f) (not (hash-ref r 'current #f)) (not (eq? (hash-ref r 'ephemeral #f) #t))))
    (define-values (archived shown) (partition hidden? marked))
    (define-values (pinned rest) (partition (lambda (r) (hash-ref r 'pinned #f)) shown))
    (define gkey (string-append "\u0002archived\u0001" host))
    (define tz (if now (date-time-zone-offset (seconds->date now)) 0))
    (define (sectioned)                                                     ; 分段：每段一个分组行 + 它的行（收起的段只有分组行）
      (append*
       (for/list ([sec (in-list SECTIONS)])
         (define rs (if (eq? sec 'pinned) pinned (filter (lambda (r) (eq? (section-of r now older-days tz) sec)) rest)))
         (cond
           [(null? rs) '()]
           [else
            (define k (string-append "\u0002sec\u0001" host "\u0001" (symbol->string sec)))
            (define o? (and (open? k (not (eq? sec 'older))) #t))
            (cons (hasheq 'kind "group" 'key k 'label (section-label sec (length rs)) 'count (length rs) 'open o? 'section (symbol->string sec))
                  (if o? rs (filter (lambda (r) (hash-ref r 'current #f)) rs)))]))))     ; 收起的段：只留当前会话的行
    (define (grouped)                                                       ; 按组：每组一个分组行 + 它的行
      (define gs (make-hash))                                               ; 组键 → (cons 名 行表（逆序）)
      (define loose '())
      (for ([r (in-list rest)])
        (define g (groups-of r))
        (if (null? g)
            (set! loose (cons r loose))
            (for ([kn (in-list g)]) (hash-update! gs (car kn) (lambda (v) (cons (car v) (cons r (cdr v)))) (cons (cdr kn) '())))))
      (define (block k name rs)
        (define gk (string-append "\u0002grp\u0001" host "\u0001" k))
        (define o? (and (open? gk #t) #t))
        (cons (hasheq 'kind "group" 'key gk 'label (group-label name (length rs)) 'count (length rs) 'open o?)
              (if o? rs (filter (lambda (r) (hash-ref r 'current #f)) rs))))
      (append pinned
              (append* (for/list ([kv (in-list (sort (hash->list gs) string-ci<? #:key cadr))]) (block (car kv) (cadr kv) (reverse (cddr kv)))))
              (if (null? loose) '()
                  (let ([gk (string-append "\u0002grp\u0001" host "\u0001-")])
                    (define o? (and (open? gk #t) #t))
                    (cons (hasheq 'kind "group" 'key gk 'label (ungrouped-label (length loose)) 'count (length loose) 'open o?)
                          (if o? (reverse loose) (filter (lambda (r) (hash-ref r 'current #f)) (reverse loose))))))))
    (append (if head (list head) '())
            (cond [groups-of (grouped)] [now (sectioned)] [else (append pinned rest)])
            (if (null? archived)
                '()
                (cons (hasheq 'kind "group" 'key gkey 'label (label (length archived)) 'count (length archived) 'open (and (open? gkey #f) #t))
                      (if (open? gkey #f) archived '())))))
  (let loop ([rs rows] [head #f] [body '()] [acc '()])
    (cond
      [(null? rs) (append* (reverse (cons (block-rows head (reverse body)) acc)))]
      [(header? (car rs))
       (loop (cdr rs) (car rs) '() (if (or head (pair? body)) (cons (block-rows head (reverse body)) acc) acc))]
      [else (loop (cdr rs) head (cons (car rs) body) acc)]))
) ; end define arrange-session-rows

(provide arrange-session-rows)
