#lang racket/base
;; tests-panel.rkt — 工作区测试流程面板（design-goalmode.md §12 T2）：盖在对话区上的卡片（同信息面板），可滚动；✕ / Esc / 点外面关闭。
;; 显示：流程定义（项目根、文件、步骤命令、解析器、保护的文件）、信任状态、每步最近一次结果（失败的测试、可展开的输出尾部）、
;;       模型的修改提议（原因 + diff；批准 / 拒绝）、运行历史。只显示、不发请求：命中结果是动作，发什么由拼装层决定。
;;   data（内核 test_flow 事件）：'exists 'root 'file 'name 'sha 'trusted 'steps ((hasheq 'id 'cmd 'parse 'timeout) …) 'protect 'history 'proposals 'error
;;   result（test_flow_result 事件）：'ok 'results ((hasheq 'step 'ok 'exit 'ms 'passed 'failed 'skipped 'failures 'tail) …)
;;   命中 → (list 'close) | (list 'trust sha) | (list 'run #f|步骤id) | (list 'approve 提议id) | (list 'reject 提议id) | #f
;; needs text-layout → provides tests-view

(require racket/list racket/string
         "../compute/drawlist.rkt" "../compute/palette.rkt" "../compute/layout.rkt" "../compute/paint.rkt" "../compute/rich.rkt"
         "../compute/hub.rkt" "../compute/interfaces.rkt" "../compute/i18n.rkt"
         "../compute/view.rkt" "../compute/widgets.rkt")                ; 视图平面：元素树 → 布局 → drawlist + 命中

(define HEAD '(0.55 0.70 1.0 1.0))
(define OK '(0.45 0.80 0.55 1.0))
(define WARN '(0.95 0.75 0.35 1.0))
(define ADD '(0.55 0.85 0.62 1.0))
(define DEL '(0.95 0.58 0.50 1.0))

(struct tp (rc [data #:mutable] [result #:mutable] [running #:mutable] [shown #:mutable] [scroll #:mutable] [hits #:mutable] [content-h #:mutable] [view-h #:mutable]))

(define (j h k [d #f]) (if (hash? h) (hash-ref h k d) d))
(define (lst v) (if (list? v) v '()))

;; 某步最近一次结果：这次面板里跑的 → 否则历史里最新的那次
(define (last-result s id)
  (or (for/first ([r (lst (j (tp-result s) 'results))] #:when (equal? (j r 'step) id)) r)
      (for*/first ([h (lst (j (tp-data s) 'history))] [r (lst (j h 'steps))] #:when (equal? (j r 'step) id)) r)))
(define (counts r)
  (define (n k) (let ([v (j r k)]) (and (real? v) v)))
  (string-join (filter values (list (and (n 'passed) (t 'ts.passed (n 'passed))) (and (n 'failed) (t 'ts.failed (n 'failed)))
                                    (and (n 'skipped) (> (n 'skipped) 0) (t 'ts.skipped (n 'skipped)))
                                    (and (n 'ms) (format "~a s" (/ (round (/ (n 'ms) 100.0)) 10.0)))))
               " · "))

(define (render! s dl x y w h scale)
  ;; 视图平面：模态卡片（compute/widgets.rkt），正文是元素树
  (define d (tp-data s))
  (define (line str font color) (para str #:font font #:color color))
  (define (diff-line l) (line (if (string=? l "") " " l) 'mono
                              (cond [(regexp-match? #rx"^\\+" l) ADD] [(regexp-match? #rx"^-" l) DEL] [else DIM])))
  ;; 修改提议：原因 + diff（+ 绿、− 红）+ 批准 / 拒绝
  (define (proposals)
    (define ps (lst (j d 'proposals)))
    (if (null? ps) '()
        (cons (line (t 'ts.proposals (length ps)) 'body WARN)
              (for/list ([p ps])
                (define diff-text (let ([df (j p 'diff)]) (if (hash? df) (j df 'text "") "")))
                (apply ui-box #:dir 'column #:gap 0.0 #:pad (vector 6.0 0.0 16.0 0.0)
                       (line (t 'ts.reason (j p 'reason "")) 'body TEXT)
                       (append (for/list ([l (string-split diff-text "\n")]) (diff-line l))
                               (list (ui-box #:dir 'row #:gap 8.0 #:pad (vector 6.0 0.0 0.0 0.0)
                                             (button (t 'ts.approve) (cons 'approve (j p 'id)) #:fill '(0.18 0.30 0.24 1.0))
                                             (button (t 'ts.reject) (cons 'reject (j p 'id)))))))))))
  (define (step-el st)
    (define id (j st 'id "?"))
    (define r (last-result s id))
    (define mark (cond [(not r) "·"] [(eq? (j r 'ok) #t) "✓"] [else "✗"]))
    (define fails (lst (and r (j r 'failures))))
    (define tail (and r (not (eq? (j r 'ok) #t)) (let ([v (j r 'tail)]) (and (string? v) (not (string=? v "")) v))))
    (define open? (and tail (member id (tp-shown s))))
    (ui-box #:dir 'column #:gap 2.0 #:pad (vector 0.0 0.0 12.0 0.0)
            (ui-box #:dir 'row #:align 'start
                    (ui-box #:dir 'column #:grow 1 #:gap 2.0
                            (line (string-append mark " " id (if r (string-append "  " (counts r)) "")) 'body
                                  (cond [(not r) TEXT] [(eq? (j r 'ok) #t) OK] [else WARN]))
                            (line (format "$ ~a" (j st 'cmd "")) 'mono DIM)
                            (line (t 'ts.parse (j st 'parse "exit") (j st 'timeout 600)) 'small DIM))
                    (ui-box #:w 70.0 (button (t 'ts.run) (cons 'run id))))
            (and (pair? fails) (line (t 'ts.failing (string-join (take fails (min 6 (length fails))) ", ")) 'small WARN))
            (and tail (ui-box #:dir 'row #:pad (vector 4.0 0.0 0.0 0.0) (button (t (if open? 'ts.hide-output 'ts.output)) (cons 'toggle id)) (ui-spacer #:grow 1)))
            (and open? (line tail 'mono DIM))))
  (define body
    (filter values
     (cond
       [(not d) (list (line (t 'ts.loading) 'body DIM))]
       [(not (eq? (j d 'exists) #t))
        (append (list (line (t 'ts.none (j d 'root "?")) 'body TEXT)
                      (and (string? (j d 'error)) (ui-box #:pad (vector 6.0 0.0 0.0 0.0) (line (j d 'error) 'small WARN)))
                      (ui-spacer #:h 14.0))
                (proposals))]
       [else
        (define hist (lst (j d 'history)))
        (append
         (list (line (format "~a" (j d 'file "")) 'small DIM)
               ;; 信任
               (if (eq? (j d 'trusted) #t)
                   (ui-box #:pad (vector 6.0 0.0 4.0 0.0) (line (t 'ts.trusted) 'small OK))
                   (ui-box #:dir 'row #:align 'start #:pad (vector 6.0 0.0 4.0 0.0) #:gap 20.0
                           (ui-box #:grow 1 (line (t 'ts.untrusted) 'small WARN))
                           (button (t 'ts.trust) (cons 'trust (j d 'sha)))))
               ;; 全部运行
               (ui-box #:dir 'row #:align 'center #:gap 12.0 #:pad (vector 6.0 0.0 10.0 0.0)
                       (button (t 'ts.run-all) (cons 'run-all #f))
                       (and (tp-running s) (line (t 'ts.running) 'small HEAD))
                       (ui-spacer #:grow 1))
               ;; 步骤
               (ui-box #:pad (vector 4.0 0.0 6.0 0.0) (line (t 'ts.steps) 'body HEAD)))
         (map step-el (lst (j d 'steps)))
         (if (pair? (lst (j d 'protect))) (list (ui-box #:pad (vector 0.0 0.0 10.0 0.0) (line (t 'ts.protect (string-join (lst (j d 'protect)) ", ")) 'small DIM))) '())
         (proposals)
         ;; 历史
         (if (null? hist) '()
             (cons (ui-box #:pad (vector 6.0 0.0 4.0 0.0) (line (t 'ts.history) 'body HEAD))
                   (for/list ([hh (take hist (min 10 (length hist)))])
                     (ui-box #:pad (vector 0.0 0.0 2.0 0.0)
                             (line (format "~a · ~a · ~a · ~a" (j hh 'ts "") (j hh 'kind "")
                                           (if (eq? (j hh 'ok) #t) "PASS" "FAIL")
                                           (string-join (for/list ([r (lst (j hh 'steps))]) (format "~a ~a" (if (eq? (j r 'ok) #t) "✓" "✗") (j r 'step "?"))) "  "))
                                   'small (if (eq? (j hh 'ok) #t) DIM WARN)))))))])))
  (define-values (hits regions scrolls)
    (render-view! (tp-rc s) dl
                  (modal-card #:title (if (and (j d 'exists) (string? (j d 'name))) (t 'ts.title-named (j d 'name)) (t 'ts.title))
                              #:body body #:offset (tp-scroll s) #:max-w 700.0)
                  x y w h scale))
  (define sc (hash-ref scrolls 'body))
  (set-tp-scroll! s (vector-ref sc 0))
  (set-tp-content-h! s (vector-ref sc 1))
  (set-tp-view-h! s (vector-ref sc 2))
  (set-tp-hits! s (reverse hits)))                                        ; 找命中时从最上面（后画的）找起

(define (find-hit s x y)
  (for/first ([h (in-list (tp-hits s))]
              #:when (and (<= (vector-ref h 2) x (+ (vector-ref h 2) (vector-ref h 4)))
                          (<= (vector-ref h 3) y (+ (vector-ref h 3) (vector-ref h 5)))))
    h))

(define (hit s x y)
  (define h (find-hit s x y))
  (cond
    [(not h) #f]
    [else
     (define kind (vector-ref h 0)) (define payload (vector-ref h 1))
     (case kind
       [(close outside) (set-tp-data! s #f) (set-tp-result! s #f) (list 'close)]
       [(toggle) (set-tp-shown! s (if (member payload (tp-shown s)) (remove payload (tp-shown s)) (cons payload (tp-shown s)))) #f]
       [(trust) (list 'trust payload)]
       [(run) (list 'run payload)]
       [(run-all) (list 'run #f)]
       [(approve) (list 'approve payload)]
       [(reject) (list 'reject payload)]
       [else #f])]))

(define tests-panel-component
  (component 'tests-panel (list text-layout) (list tests-view)
             (lambda (use config)
               (define s (tp ((use text-layout) 'rich) #f #f #f '() 0.0 '() 0.0 0.0))
               (list (implement tests-view
                                'open! (lambda (data) (set-tp-data! s data) (set-tp-result! s #f) (set-tp-running! s #f) (set-tp-shown! s '()) (set-tp-scroll! s 0.0))
                                'update! (lambda (data) (set-tp-data! s data))
                                'set-result! (lambda (r) (set-tp-result! s r) (set-tp-running! s #f))
                                'set-running! (lambda (v) (set-tp-running! s v))
                                'close! (lambda () (set-tp-data! s #f) (set-tp-result! s #f))
                                'open? (lambda () (and (tp-data s) #t))
                                'data (lambda () (tp-data s))
                                'render! (lambda (dl x y w h scale) (when (tp-data s) (render! s dl x y w h scale)))
                                'hit (lambda (x y) (hit s x y))
                                'scroll! (lambda (dy) (set-tp-scroll! s (- (tp-scroll s) dy)))
                                'hit-center (lambda (kind [payload #f])
                                              (for/first ([h (in-list (tp-hits s))] #:when (and (eq? (vector-ref h 0) kind) (or (not payload) (equal? (vector-ref h 1) payload))))
                                                (cons (+ (vector-ref h 2) (/ (vector-ref h 4) 2)) (+ (vector-ref h 3) (/ (vector-ref h 5) 2))))))))))

(provide tests-panel-component)
