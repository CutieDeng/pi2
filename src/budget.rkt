#lang tstring racket
;; budget.rkt — 费用软上限：pi2 自己按估算花费设的上限（供应商那边的额度 / 硬上限另算）。
;; 两种上限（都可不设）：
;;   每天（本机日期，所有会话、所有 pi2 进程合计）、每轮（一个用户轮，含子 agent 与 goal 的内部轮）。
;; 「软」：到了上限不直接掐断，而是在下一次请求之前问用户：本轮继续 / 今天不再询问 / 停止。
;;   停止 → 抛 exn:fail:budget；RPC 按中断处理（保留已完成的步骤，加 <turn_aborted> 标记，原因写明是费用上限）。
;; 花费 = pricing.rkt 的估算（静态单价 × token；不含缓存折扣、推理 token 单独计价等），未知模型（本地）记 0。
;;   上限在请求之前检查，花费在请求完成后才知道 → 越过上限的那一次请求会照常发出（最多超出一次请求的花费）。
;; 跨主机（GUI 一窗多主机）：GUI 把「其他主机今天花了多少」与「合计每天上限」推进来（set-external-spend!，只在内存里、带日期），
;;   请求前再按「本机 + 其他」对比合计上限（范围 "combined"）。账本标识（ledger-id = 主机名 | 花费文件路径）让 GUI 合计时去重。
;; 借来的凭证（use_endpoint：本机签发的 token / 临时分享的 key，远端子 agent 用）：花的是父会话那边的钱，由那边记账；
;;   这边不写花费文件，只计入本轮花费（每轮上限照常）。
;; 文件：{config-home}/budget.rktd（上限：hash 'daily-usd 'turn-usd 'snooze-day）；
;;       {config-home}/spend-log.rktd（每次请求一行 (日期 时间戳 模型 美元)，多进程追加；超过 1 MB 时去掉 31 天前的行）。

(require racket/file racket/date racket/os file/sha1 (file "credentials.rkt") (file "pricing.rkt") (file "model.rkt"))

(struct exn:fail:budget exn:fail (scope spent cap))

(define (budget-path) (build-path (config-home) "budget.rktd"))
(define (spend-log-path) (build-path (config-home) "spend-log.rktd"))

(define (today) (let ([d (seconds->date (current-seconds))]) (format "~a-~a-~a" (date-year d) (pad2 (date-month d)) (pad2 (date-day d)))))
(define (pad2 n) (if (< n 10) (format "0~a" n) (format "~a" n)))

;; ---------------------------------------------------------------- 上限

(define (load-budget)
  (with-handlers ([exn:fail? (lambda (_e) (hasheq))])
    (define v (if (file-exists? (budget-path))
                  (call-with-input-file (budget-path) (lambda (in) (parameterize ([read-accept-reader #f]) (read in))))
                  (hasheq)))
    (if (hash? v) v (hasheq)))
) ; end define load-budget

(define (save-budget! h)
  (make-directory* (config-home))
  (define tmp (path-add-extension (budget-path) #".tmp"))
  (call-with-output-file tmp #:exists 'truncate (lambda (out) (write h out)))
  (rename-file-or-directory tmp (budget-path) #t)
) ; end define save-budget!

(define (cap-value v) (and (real? v) (> v 0) v))

;; daily / turn：正数 = 设上限，#f = 取消；不传 = 不变
(define (set-budget! #:daily [daily 'keep] #:turn [turn 'keep])
  (define h0 (load-budget))
  (define (upd h k v) (cond [(eq? v 'keep) h] [(cap-value v) (hash-set h k v)] [else (hash-remove h k)]))
  (save-budget! (upd (upd h0 'daily-usd daily) 'turn-usd turn))
) ; end define set-budget!

(define (daily-cap [b (load-budget)]) (cap-value (hash-ref b 'daily-usd #f)))
(define (turn-cap [b (load-budget)]) (cap-value (hash-ref b 'turn-usd #f)))
(define (snoozed-today? [b (load-budget)]) (equal? (hash-ref b 'snooze-day #f) (today)))
(define (snooze-today!) (save-budget! (hash-set (load-budget) 'snooze-day (today))))

;; ---------------------------------------------------------------- 花费记录

(define log-sema (make-semaphore 1))

(define (spend-lines)
  (with-handlers ([exn:fail? (lambda (_e) '())])
    (if (file-exists? (spend-log-path))
        (call-with-input-file (spend-log-path)
          (lambda (in)
            (parameterize ([read-accept-reader #f])
              (let loop ([acc '()])
                (define d (with-handlers ([exn:fail:read? (lambda (_e) eof)]) (read in)))   ; 并发写到一半的行：到此为止
                (if (eof-object? d) (reverse acc)
                    (loop (if (and (list? d) (= (length d) 4) (string? (car d)) (real? (cadddr d))) (cons d acc) acc)))))))
        '()))
) ; end define spend-lines

;; → (values 今日合计 (listof (cons 模型 美元)))
(define (spent-today)
  (define t (today))
  (define by (make-hash))
  (for ([l (in-list (spend-lines))] #:when (equal? (car l) t))
    (hash-update! by (caddr l) (lambda (x) (+ x (cadddr l))) 0.0))
  (values (for/sum ([v (in-hash-values by)]) v)
          (sort (hash->list by) > #:key cdr))
) ; end define spent-today

(define (prune-log!)
  (when (and (file-exists? (spend-log-path)) (> (file-size (spend-log-path)) (* 1024 1024)))
    (define cutoff (let ([d (seconds->date (- (current-seconds) (* 31 86400)))]) (format "~a-~a-~a" (date-year d) (pad2 (date-month d)) (pad2 (date-day d)))))
    (define keep (filter (lambda (l) (string>=? (car l) cutoff)) (spend-lines)))
    (define tmp (path-add-extension (spend-log-path) #".tmp"))
    (call-with-output-file tmp #:exists 'truncate (lambda (out) (for ([l keep]) (write l out) (newline out))))
    (rename-file-or-directory tmp (spend-log-path) #t))
) ; end define prune-log!

;; 本轮的花费与选择：top-level run-turn! 建一个，子 agent / 工具线程继承
;; (vector 花费 allowed? stopped)：stopped = #f 或 (list 范围 花费 上限)（用户选了「停止」）
(define current-turn-budget (make-parameter #f))
(define (make-turn-budget) (vector 0.0 #f #f))

;; 一次请求完成后记账；→ 这次的美元估算
;; 借来的凭证：花费由借出方记账（这边不写花费文件）
(define borrowed? (box #f))
(define (set-borrowed-credential! on?) (set-box! borrowed? (and on? #t)))

(define (record-spend! model u)
  (define usd (or (and (usage? u) (estimate-cost model u)) 0.0))
  (when (and (> usd 0) (not (unbox borrowed?)))
    (call-with-semaphore log-sema
      (lambda ()
        (make-directory* (config-home))
        (with-output-to-file (spend-log-path) #:exists 'append
          (lambda () (write (list (today) (current-seconds) model (exact->inexact usd))) (newline)))
        (prune-log!)))
    (void))
  (define tb (current-turn-budget))
  (when (and tb (> usd 0)) (vector-set! tb 0 (+ (vector-ref tb 0) usd)))
  usd
) ; end define record-spend!

;; ---------------------------------------------------------------- 跨主机合计

;; 账本标识：同一台机器上同一个花费文件 = 同一本账（GUI 合计时按它去重）
(define (spend-ledger-id)
  (substring (sha1 (open-input-string (string-append (gethostname) "|" (path->string (simplify-path (path->complete-path (spend-log-path))))))) 0 12)
) ; end define spend-ledger-id

;; GUI 推来的：(vector 日期 其他主机花费 合计上限或#f)；合计范围选了「今天不再询问」的日期
(define external (box #f))
(define combined-snooze (box #f))
(define (set-external-spend! others cap [date (today)])
  (set-box! external (vector date (if (real? others) others 0.0) (cap-value cap))))
(define (external-now)                                    ; → (values 其他主机花费 合计上限)；日期不是今天 → 0 / #f
  (define v (unbox external))
  (if (and v (equal? (vector-ref v 0) (today))) (values (vector-ref v 1) (vector-ref v 2)) (values 0.0 #f)))

;; ---------------------------------------------------------------- 请求前检查

;; ask：(说明文字 输入hash → 'yes | 'always | 'no)。超上限且本轮没放行：问；'no → raise exn:fail:budget
(define (budget-gate! ask)
  (define tb (current-turn-budget))
  (define b (load-budget))
  (cond
    [(and tb (vector-ref tb 2)) (apply raise-budget (vector-ref tb 2))]   ; 本轮已选「停止」（如在子 agent 里选的）：不再问
    [(and tb (vector-ref tb 1)) (void)]
    [else
     (define-values (day _by) (spent-today))
     (define-values (others combined-cap) (external-now))
     (define over
       (cond [(and (turn-cap b) tb (>= (vector-ref tb 0) (turn-cap b))) (list "turn" (vector-ref tb 0) (turn-cap b))]
             [(and (daily-cap b) (not (snoozed-today? b)) (>= day (daily-cap b))) (list "day" day (daily-cap b))]
             [(and combined-cap (not (equal? (unbox combined-snooze) (today))) (>= (+ day others) combined-cap))
              (list "combined" (+ day others) combined-cap)]
             [else #f]))
     (when over
       (define-values (scope spent cap) (apply values over))
       (define text (format "Spending cap reached (~a): estimated $~a of $~a. Continue this turn?"
                            (scope-label scope) (round4 spent) (round4 cap)))
       (case (ask text (hasheq 'scope scope 'spent (round4 spent) 'cap (round4 cap)))
         [(yes) (when tb (vector-set! tb 1 #t))]
         [(always) (case scope [("day") (snooze-today!)] [("combined") (set-box! combined-snooze (today))] [else (void)])
                   (when tb (vector-set! tb 1 #t))]
         [else (when tb (vector-set! tb 2 over)) (raise-budget scope spent cap)]))])
) ; end define budget-gate!

(define (round4 x) (/ (round (* x 10000)) 10000.0))

(define (scope-label scope)
  (case scope [("day") "today"] [("combined") "today, all hosts"] [else "this turn"]))

(define (raise-budget scope spent cap)
  (raise (exn:fail:budget (format "spending cap reached (~a): estimated $~a of $~a; turn stopped"
                                  (scope-label scope) (round4 spent) (round4 cap))
                          (current-continuation-marks) scope spent cap))
) ; end define raise-budget

(define (budget->jsexpr)
  (define b (load-budget))
  (define-values (day by) (spent-today))
  (hasheq 'daily_usd (or (daily-cap b) 'null) 'turn_usd (or (turn-cap b) 'null) 'snoozed (snoozed-today? b)
          'spent_today (round4 day)
          'by_model (for/list ([p (in-list by)]) (hasheq 'model (car p) 'usd (round4 (cdr p))))
          'date (today)
          'ledger (spend-ledger-id)
          'borrowed (unbox borrowed?)
          'external_usd (let-values ([(o _c) (external-now)]) (round4 o))
          'combined_usd (let-values ([(_o c) (external-now)]) (or c 'null))
          'combined_snoozed (equal? (unbox combined-snooze) (today)))
) ; end define budget->jsexpr

(provide set-borrowed-credential! set-external-spend! spend-ledger-id)
(provide (struct-out exn:fail:budget) load-budget set-budget! daily-cap turn-cap snoozed-today? snooze-today!
         spent-today record-spend! budget-gate! current-turn-budget make-turn-budget budget->jsexpr
         budget-path spend-log-path today)
