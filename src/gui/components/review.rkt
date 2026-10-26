#lang racket/base
;; review.rkt — 变更清单的审阅面板（design-workspace.md §11.7 C3）：agent 提议的会话整理，逐项勾选后应用；应用过的可整份撤销。
;; 盖在对话区上的卡片（同信息面板）。数据由拼装层给（已校验的各项，compute/changeset.rkt）；勾选状态在这里；不发请求，命中结果是动作。
;;   data：(hasheq 'id 'by 'when 'request 'summary 'state ('proposed 'applied 'undone 'discarded) 'items (各项：'n 'op 'title 'project 'new_title 'reason 'status 'why))
;;   命中：(list 'toggle 项号) (list 'toggle-group 组) 'apply 'discard 'undo 'close
;; needs text-layout → provides review-view

(require racket/list racket/string
         "../compute/drawlist.rkt" "../compute/palette.rkt" "../compute/layout.rkt" "../compute/paint.rkt" "../compute/rich.rkt"
         "../compute/hub.rkt" "../compute/interfaces.rkt" "../compute/i18n.rkt" "../compute/changeset.rkt"
         "../compute/view.rkt" "../compute/widgets.rkt")                ; 视图平面：元素树 → 布局 → drawlist + 命中

(define WARN '(0.95 0.55 0.45 1.0))
(define ADD '(0.55 0.85 0.62 1.0))                      ; 新增（新标题、归入的项目）
(define LINK '(0.62 0.76 1.0 1.0))
(define FOOT-H 52.0)

(struct rv (rc [data #:mutable] [checked #:mutable] [scroll #:mutable] [hits #:mutable] [content-h #:mutable] [view-h #:mutable]))

(define (j h k [d #f]) (if (hash? h) (hash-ref h k d) d))
(define (selectable? it) (equal? (j it 'status) "ok"))
(define (items s) (let ([v (j (rv-data s) 'items '())]) (if (list? v) v '())))
(define (editable? s) (eq? (j (rv-data s) 'state 'proposed) 'proposed))

;; 缺省勾选：能做的都勾，删除不勾（删除可撤销，但让人自己点）
(define (default-checked its) (for/list ([it (in-list its)] #:when (and (selectable? it) (not (equal? (j it 'op) "delete")))) (j it 'n)))

(define (group-label kind name)
  (case kind
    [(new-project) (t 'rv.g-new-project)] [(link) (t 'rv.g-link name)] [(unlink) (t 'rv.g-unlink name)]
    [(pin) (t 'rv.g-pin)] [(archive) (t 'rv.g-archive)] [(rename) (t 'rv.g-rename)] [(delete) (t 'rv.g-delete)] [else (t 'rv.g-other)]))

;; 一项的「变成什么」：→ (listof (cons 文字 颜色))
(define (change-parts it)
  (case (j it 'op)
    [("new-project") (list (cons (format "+ ~a" (j it 'project "")) ADD))]
    [("link") (list (cons (format "+ ~a" (j it 'project "")) LINK))]
    [("unlink") (list (cons (format "− ~a" (j it 'project "")) DIM))]
    [("pin") (list (cons (t 'rv.c-pin) TEXT))] [("unpin") (list (cons (t 'rv.c-unpin) DIM))]
    [("archive") (list (cons (t 'rv.c-archive) DIM-ON-ACCENT))] [("unarchive") (list (cons (t 'rv.c-unarchive) TEXT))]
    [("rename") (list (cons (format "→ ~a" (j it 'new_title "")) ADD))]
    [("delete") (list (cons (t 'rv.c-delete) WARN))]
    [else (list (cons (format "~a" (j it 'op)) DIM))]))

;; 视图平面：模态卡片（compute/widgets.rkt）；头里是谁提议、原话、摘要、计数；正文按组列项；脚是按钮（固定在底部）
(define (render! s dl x y w h scale)
  (define d (rv-data s))
  (define its (items s))
  (define on (rv-checked s))
  (define groups (changeset-groups its))
  (define ed? (editable? s))
  (define counts (string-join (for/list ([g (in-list groups)])
                                (format "~a ~a/~a" (group-label (first g) (second g))
                                        (length (filter (lambda (it) (memv (j it 'n) on)) (third g))) (length (third g))))
                              " · "))
  (define head
    (filter values
            (list (para (t 'rv.meta (j d 'by "?") (j d 'when "")) #:font 'small #:color DIM)
                  (and (non-empty-string? (j d 'request "")) (para (t 'rv.request (j d 'request "")) #:font 'body #:color TEXT))
                  (and (non-empty-string? (j d 'summary "")) (para (j d 'summary "") #:font 'small #:color DIM))
                  (ui-box #:pad (vector 2.0 0.0 0.0 0.0) (para counts #:font 'small #:color DIM-ON-ACCENT)))))
  (define (item-el it)
    (define ok? (selectable? it))
    (define c? (and (memv (j it 'n) on) #t))
    (define head-text (let ([hd (if (equal? (j it 'op) "new-project") (j it 'project "") (j it 'title (t 'sb.untitled)))]) (if (string? hd) hd "?")))
    (define note (if ok? (j it 'reason #f) (format "~a~a" (t (case (j it 'status) [("stale") 'rv.st-stale] [("noop") 'rv.st-noop] [else 'rv.st-invalid]))
                                                   (let ([w (j it 'why #f)]) (if (string? w) (format "（~a）" w) "")))))
    (ui-box #:dir 'row #:align 'start #:pad (vector 3.0 0.0 7.0 10.0) #:hit (and ed? ok? (cons 'toggle (j it 'n)))
            (and ed? (ui-box #:w 24.0 #:pad (vector 2.0 0.0 0.0 0.0) (checkbox c? ok?)))
            (ui-box #:dir 'column #:grow 1 #:gap 2.0
                    (apply ui-box #:dir 'row #:align 'start #:gap 8.0
                           (ui-text head-text #:font 'body #:color (if ok? TEXT DIM) #:grow 1 #:strike? (equal? (j it 'op) "rename"))   ; 改名：旧标题画删除线
                           (for/list ([p (in-list (change-parts it))])
                             (ui-box #:pad (vector 2.0 0.0 0.0 0.0) (ui-text (car p) #:font 'small #:color (if ok? (cdr p) DIM) #:fit? #f))))
                    (and (string? note) (non-empty-string? note) (ui-text note #:font 'small #:color (if ok? DIM WARN))))))
  (define body
    (for/list ([g (in-list groups)])
      (define gkey (cons (first g) (second g)))
      (define sel (filter selectable? (third g)))
      (define all-on? (and (pair? sel) (andmap (lambda (it) (memv (j it 'n) on)) sel)))
      (apply ui-box #:dir 'column #:pad (vector 0.0 0.0 0.0 0.0)
             (ui-box #:dir 'row #:h 26.0 #:align 'center #:fill '(0.17 0.18 0.22 1.0) #:radius 5.0 #:pad (vector 0.0 10.0 0.0 10.0)
                     #:hit (and ed? (pair? sel) (cons 'toggle-group gkey))
                     (and ed? (ui-box #:w 24.0 (checkbox all-on? (pair? sel))))
                     (ui-text (format "~a（~a）" (group-label (first g) (second g)) (length (third g))) #:font 'body #:color TEXT #:grow 1))
             (ui-spacer #:h 6.0)
             (for/list ([it (in-list (third g))]) (item-el it)))))
  (define buttons
    (case (j d 'state 'proposed)
      [(proposed) (list (list 'apply (t 'rv.apply (length on)) (pair? on) #t) (list 'discard (t 'rv.discard) #t #f))]
      [(applied) (list (list 'undo (t 'rv.undo) #t #f) (list 'close (t 'rv.close) #t #f))]
      [else (list (list 'close (t 'rv.close) #t #f))]))
  (define footer
    (ui-box #:dir 'column #:h FOOT-H
            (ui-box #:h 1.0 #:fill '(0.26 0.27 0.31 1.0))
            (apply ui-box #:dir 'row #:gap 10.0 #:align 'center #:pad (vector 9.0 0.0 0.0 0.0)
                   (ui-spacer #:grow 1)
                   (for/list ([b (in-list buttons)])
                     ;; 主按钮的蓝：0.25/0.40/0.75 上白字只有 4.42
                     (button (second b) (and (third b) (cons (first b) #t)) #:font 'body #:h 30.0 #:color (if (third b) TEXT TEXT-OFF)
                             #:fill (if (fourth b) (list 0.20 0.33 0.66 (if (third b) 1.0 0.5)) (list (car BUTTON) (cadr BUTTON) (caddr BUTTON) (if (third b) 1.0 0.5))))))))
  (define-values (hits regions scrolls)
    (render-view! (rv-rc s) dl
                  (modal-card #:title (t 'rv.title (j d 'id "?") (t (case (j d 'state 'proposed) [(applied) 'rv.s-applied] [(undone) 'rv.s-undone] [(discarded) 'rv.s-discarded] [else 'rv.s-proposed])))
                              #:head-extra head #:body body #:offset (rv-scroll s) #:max-w 720.0 #:top 24.0 #:bottom 48.0 #:footer footer #:body-gap 6.0)
                  x y w h scale))
  (define sc (hash-ref scrolls 'body))
  (set-rv-scroll! s (vector-ref sc 0))
  (set-rv-content-h! s (vector-ref sc 1))
  (set-rv-view-h! s (vector-ref sc 2))
  (set-rv-hits! s (reverse hits))                                         ; 找命中时从最上面（后画的）找起
) ; end define render!

;; → 动作或 #f（点在卡片上的空处）
(define (hit s x y)
  (define h (for/first ([h (in-list (rv-hits s))]
                        #:when (and (<= (vector-ref h 2) x (+ (vector-ref h 2) (vector-ref h 4)))
                                    (<= (vector-ref h 3) y (+ (vector-ref h 3) (vector-ref h 5)))))
              h))
  (cond [(not h) #f]
        [(memq (vector-ref h 0) '(close outside)) (list 'close)]
        [(eq? (vector-ref h 0) 'panel) #f]
        [(eq? (vector-ref h 0) 'toggle) (toggle! s (vector-ref h 1)) (list 'toggle (vector-ref h 1))]
        [(eq? (vector-ref h 0) 'toggle-group) (toggle-group! s (vector-ref h 1)) (list 'toggle-group (vector-ref h 1))]
        [else (list (vector-ref h 0))])
) ; end define hit

(define (toggle! s n)
  (when (for/or ([it (in-list (items s))]) (and (eqv? (j it 'n) n) (selectable? it)))
    (set-rv-checked! s (if (memv n (rv-checked s)) (remv n (rv-checked s)) (sort (cons n (rv-checked s)) <)))))
(define (toggle-group! s gkey)
  (define g (for/first ([g (in-list (changeset-groups (items s)))] #:when (equal? (cons (first g) (second g)) gkey)) g))
  (when g
    (define ns (for/list ([it (in-list (third g))] #:when (selectable? it)) (j it 'n)))
    (define all-on? (andmap (lambda (n) (memv n (rv-checked s))) ns))
    (set-rv-checked! s (sort (if all-on? (filter (lambda (n) (not (memv n ns))) (rv-checked s)) (remove-duplicates (append ns (rv-checked s)))) <))))

(define review-component
  (component 'review (list text-layout) (list review-view)
             (lambda (use config)
               (define s (rv ((use text-layout) 'rich) #f '() 0.0 '() 0.0 0.0))
               (list (implement review-view
                                'open! (lambda (data) (set-rv-data! s data) (set-rv-scroll! s 0.0)
                                         (set-rv-checked! s (if (eq? (j data 'state 'proposed) 'proposed) (default-checked (items s))
                                                                (let ([a (j data 'accepted '())]) (if (list? a) a '())))))
                                'close! (lambda () (set-rv-data! s #f))
                                'open? (lambda () (and (rv-data s) #t))
                                'data (lambda () (rv-data s))
                                'accepted (lambda () (rv-checked s))
                                'render! (lambda (dl x y w h scale) (when (rv-data s) (render! s dl x y w h scale)))
                                'hit (lambda (x y) (hit s x y))
                                'scroll! (lambda (dy) (set-rv-scroll! s (- (rv-scroll s) dy)))
                                'hit-center (lambda (kind [payload #t])
                                              (for/first ([h (in-list (rv-hits s))] #:when (and (eq? (vector-ref h 0) kind) (equal? (vector-ref h 1) payload)))
                                                (cons (+ (vector-ref h 2) (/ (vector-ref h 4) 2)) (+ (vector-ref h 3) (/ (vector-ref h 5) 2))))))))))

(provide review-component default-checked)
