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
         "../compute/hub.rkt" "../compute/interfaces.rkt" "../compute/i18n.rkt")

(define HEAD '(0.55 0.70 1.0 1.0))
(define OK '(0.45 0.80 0.55 1.0))
(define WARN '(0.95 0.75 0.35 1.0))
(define ADD '(0.55 0.85 0.62 1.0))
(define DEL '(0.95 0.58 0.50 1.0))
(define PAD 18.0)

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
  (define rc (tp-rc s))
  (define lay (rich-ctx-layout rc))
  (define at (rich-ctx-atlas rc))
  (define hits '())
  (define (hit! kind payload hx hy hw hh) (set! hits (cons (vector kind payload hx hy hw hh) hits)))
  (hit! 'outside #t x y w h)
  (define cw (min 700.0 (- w 40.0)))
  (define cx (+ x (/ (- w cw) 2)))
  (define cy (+ y 30.0))
  (define ch (- h 60.0))
  (drawlist-rect! dl (- cx 1.0) (- cy 1.0) (+ cw 2.0) (+ ch 2.0) 0.30 0.31 0.36 1.0 #:radius 11.0)
  (drawlist-rect! dl cx cy cw ch 0.12 0.12 0.15 1.0 #:radius 10.0)
  (hit! 'panel #t cx cy cw ch)
  (define d (tp-data s))
  (define title (lay (if (and (j d 'exists) (string? (j d 'name))) (t 'ts.title-named (j d 'name)) (t 'ts.title)) 'title (- cw 80.0) #f))
  (paint-text! dl at title (+ cx PAD) (+ cy 14.0) TEXT #:scale scale)
  (define xb (lay "✕" 'body 100.0 #f))
  (define xx (- (+ cx cw) PAD (text-block-width xb) 6.0))
  (paint-text! dl at xb (+ xx 3.0) (+ cy 14.0) DIM #:scale scale)
  (hit! 'close #t (- xx 6.0) (+ cy 6.0) (+ (text-block-width xb) 18.0) 30.0)
  ;; 正文（裁剪 + 滚动）
  (define by (+ cy 14.0 (text-block-height title) 12.0))
  (define bh (- (+ cy ch) by 12.0))
  (set-tp-view-h! s bh)
  (drawlist-set-clip! dl (vector cx by cw bh))
  (define lx (+ cx PAD))
  (define tw (- cw (* 2 PAD)))
  (define (in-view? yy hh) (and (< yy (+ by bh)) (> (+ yy hh) by)))
  (define (text! yy str font color [ww tw] #:x [xx lx])
    (define tb (lay str font ww #f))
    (when (in-view? yy (text-block-height tb)) (paint-text! dl at tb xx yy color #:scale scale))
    (+ yy (text-block-height tb)))
  (define (button! bx yy label kind payload #:fill [fill (take BUTTON 3)])
    (define tb (lay label 'small 1000.0 #f))
    (define bw (+ (text-block-width tb) 18.0))
    (define bh* (+ (text-block-height tb) 8.0))
    (when (in-view? yy bh*)
      (drawlist-rect! dl bx yy bw bh* (car fill) (cadr fill) (caddr fill) 1.0 #:radius 6.0)
      (paint-text! dl at tb (+ bx 9.0) (+ yy 4.0) TEXT #:scale scale)
      (let ([y0 (max yy by)] [y1 (min (+ yy bh*) (+ by bh))]) (when (> y1 y0) (hit! kind payload bx y0 bw (- y1 y0)))))
    bw)
  ;; 修改提议：原因 + diff（+ 绿、− 红）+ 批准 / 拒绝
  (define (proposals! y0)
    (define ps (lst (j d 'proposals)))
    (cond
      [(null? ps) y0]
      [else
       (define yh (text! y0 (t 'ts.proposals (length ps)) 'body WARN))
       (for/fold ([yy (+ yh 6.0)]) ([p ps])
         (define r1 (text! yy (t 'ts.reason (j p 'reason "")) 'body TEXT))
         (define diff-text (let ([df (j p 'diff)]) (if (hash? df) (j df 'text "") "")))
         (define r2
           (for/fold ([y1 (+ r1 4.0)]) ([line (string-split diff-text "\n")])
             (text! y1 (if (string=? line "") " " line) 'mono
                    (cond [(regexp-match? #rx"^\\+" line) ADD] [(regexp-match? #rx"^-" line) DEL] [else DIM]))))
         (define w1 (button! lx (+ r2 6.0) (t 'ts.approve) 'approve (j p 'id) #:fill '(0.18 0.30 0.24)))
         (button! (+ lx w1 8.0) (+ r2 6.0) (t 'ts.reject) 'reject (j p 'id))
         (+ r2 46.0))]))
  (define y-end
    (let ([y0 (- by (tp-scroll s))])
      (cond
        [(not d) (text! y0 (t 'ts.loading) 'body DIM)]
        [(not (eq? (j d 'exists) #t))
         (define y1 (text! y0 (t 'ts.none (j d 'root "?")) 'body TEXT))
         (define y2 (if (string? (j d 'error)) (text! (+ y1 6.0) (j d 'error) 'small WARN) y1))
         (proposals! (+ y2 14.0))]
        [else
         (define y1 (text! y0 (format "~a" (j d 'file "")) 'small DIM))
         ;; 信任
         (define y2
           (cond
             [(eq? (j d 'trusted) #t) (+ (text! (+ y1 6.0) (t 'ts.trusted) 'small OK) 4.0)]
             [else
              (define yt (text! (+ y1 6.0) (t 'ts.untrusted) 'small WARN (- tw 120.0)))
              (button! (+ lx tw -100.0) (+ y1 4.0) (t 'ts.trust) 'trust (j d 'sha))
              (+ (max yt (+ y1 34.0)) 4.0)]))
         ;; 全部运行
         (define y3
           (let ([w1 (button! lx (+ y2 6.0) (t 'ts.run-all) 'run-all #f)])
             (when (tp-running s) (text! (+ y2 10.0) (t 'ts.running) 'small HEAD #:x (+ lx w1 12.0)))
             (+ y2 40.0)))
         ;; 步骤
         (define y4 (text! (+ y3 4.0) (t 'ts.steps) 'body HEAD))
         (define y5
           (for/fold ([yy (+ y4 6.0)]) ([st (lst (j d 'steps))])
             (define id (j st 'id "?"))
             (define r (last-result s id))
             (define mark (cond [(not r) "·"] [(eq? (j r 'ok) #t) "✓"] [else "✗"]))
             (define r1 (text! yy (string-append mark " " id (if r (string-append "  " (counts r)) "")) 'body
                               (cond [(not r) TEXT] [(eq? (j r 'ok) #t) OK] [else WARN]) (- tw 90.0)))
             (button! (+ lx tw -70.0) (- yy 2.0) (t 'ts.run) 'run id)
             (define r2 (text! (+ r1 2.0) (format "$ ~a" (j st 'cmd "")) 'mono DIM (- tw 90.0)))
             (define r3 (text! (+ r2 1.0) (t 'ts.parse (j st 'parse "exit") (j st 'timeout 600)) 'small DIM))
             (define fails (lst (and r (j r 'failures))))
             (define r4 (if (pair? fails) (text! (+ r3 2.0) (t 'ts.failing (string-join (take fails (min 6 (length fails))) ", ")) 'small WARN) r3))
             (define tail (and r (not (eq? (j r 'ok) #t)) (let ([v (j r 'tail)]) (and (string? v) (not (string=? v "")) v))))
             (define r5
               (cond
                 [(not tail) r4]
                 [else
                  (define open? (member id (tp-shown s)))
                  (button! lx (+ r4 4.0) (t (if open? 'ts.hide-output 'ts.output)) 'toggle id)
                  (if open?
                      (text! (+ r4 34.0) tail 'mono DIM)
                      (+ r4 30.0))]))
             (+ r5 12.0)))
         (define y6 (if (pair? (lst (j d 'protect))) (+ (text! y5 (t 'ts.protect (string-join (lst (j d 'protect)) ", ")) 'small DIM) 10.0) y5))
         (define y7 (proposals! y6))
         ;; 历史
         (define hist (lst (j d 'history)))
         (cond
           [(null? hist) y7]
           [else
            (define yh (text! (+ y7 6.0) (t 'ts.history) 'body HEAD))
            (for/fold ([yy (+ yh 4.0)]) ([hh (take hist (min 10 (length hist)))])
              (+ (text! yy (format "~a · ~a · ~a · ~a" (j hh 'ts "") (j hh 'kind "")
                                   (if (eq? (j hh 'ok) #t) "PASS" "FAIL")
                                   (string-join (for/list ([r (lst (j hh 'steps))]) (format "~a ~a" (if (eq? (j r 'ok) #t) "✓" "✗") (j r 'step "?"))) "  "))
                        'small (if (eq? (j hh 'ok) #t) DIM WARN))
                 2.0))])])))
  (drawlist-set-clip! dl #f)
  (set-tp-content-h! s (+ (- y-end by) (tp-scroll s)))
  (set-tp-scroll! s (max 0.0 (min (tp-scroll s) (max 0.0 (- (tp-content-h s) bh)))))
  (set-tp-hits! s hits))

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
