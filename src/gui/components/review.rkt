#lang racket/base
;; review.rkt — 变更清单的审阅面板（design-workspace.md §11.7 C3）：agent 提议的会话整理，逐项勾选后应用；应用过的可整份撤销。
;; 盖在对话区上的卡片（同信息面板）。数据由拼装层给（已校验的各项，compute/changeset.rkt）；勾选状态在这里；不发请求，命中结果是动作。
;;   data：(hasheq 'id 'by 'when 'request 'summary 'state ('proposed 'applied 'undone 'discarded) 'items (各项：'n 'op 'title 'project 'new_title 'reason 'status 'why))
;;   命中：(list 'toggle 项号) (list 'toggle-group 组) 'apply 'discard 'undo 'close
;; needs text-layout → provides review-view

(require racket/list racket/string
         "../compute/drawlist.rkt" "../compute/palette.rkt" "../compute/layout.rkt" "../compute/paint.rkt" "../compute/rich.rkt"
         "../compute/hub.rkt" "../compute/interfaces.rkt" "../compute/i18n.rkt" "../compute/changeset.rkt")

(define WARN '(0.95 0.55 0.45 1.0))
(define ADD '(0.55 0.85 0.62 1.0))                      ; 新增（新标题、归入的项目）
(define LINK '(0.62 0.76 1.0 1.0))
(define PAD 18.0)
(define FOOT-H 52.0)
(define BOX 15.0)

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

(define (checkbox! dl x y on? enabled?)
  (drawlist-rect! dl x y BOX BOX 0.45 0.47 0.55 (if enabled? 1.0 0.5) #:radius 4.0)
  (drawlist-rect! dl (+ x 1.5) (+ y 1.5) (- BOX 3.0) (- BOX 3.0) (if on? 0.30 0.12) (if on? 0.45 0.13) (if on? 0.80 0.16) 1.0 #:radius 3.0))

(define (render! s dl x y w h scale)
  (define rc (rv-rc s))
  (define lay (rich-ctx-layout rc))
  (define at (rich-ctx-atlas rc))
  (define d (rv-data s))
  (define hits '())
  (define (hit! kind payload hx hy hw hh) (set! hits (cons (vector kind payload hx hy hw hh) hits)))
  (hit! 'outside #t x y w h)
  (define cw (min 720.0 (- w 40.0)))
  (define cx (+ x (/ (- w cw) 2)))
  (define cy (+ y 24.0))
  (define ch (- h 72.0))                                                    ; 底下留出状态行
  (drawlist-rect! dl (- cx 1.0) (- cy 1.0) (+ cw 2.0) (+ ch 2.0) 0.30 0.31 0.36 1.0 #:radius 11.0)
  (drawlist-rect! dl cx cy cw ch 0.12 0.12 0.15 1.0 #:radius 10.0)
  (hit! 'panel #t cx cy cw ch)
  (define inner (- cw (* 2 PAD)))
  ;; 头：标题 + 状态 + ✕；谁提议、何时；用户原话；摘要；计数
  (define title (lay (t 'rv.title (j d 'id "?") (t (case (j d 'state 'proposed) [(applied) 'rv.s-applied] [(undone) 'rv.s-undone] [(discarded) 'rv.s-discarded] [else 'rv.s-proposed]))) 'title (- inner 40.0) #f))
  (paint-text! dl at title (+ cx PAD) (+ cy 14.0) TEXT #:scale scale)
  (define xb (lay "✕" 'body 100.0 #f))
  (define xx (- (+ cx cw) PAD (text-block-width xb) 6.0))
  (paint-text! dl at xb (+ xx 3.0) (+ cy 14.0) DIM #:scale scale)
  (hit! 'close #t (- xx 6.0) (+ cy 6.0) (+ (text-block-width xb) 18.0) 30.0)
  (define (line! yy text style color) (define tb (lay text style inner #f)) (paint-text! dl at tb (+ cx PAD) yy color #:scale scale) (+ yy (text-block-height tb) 4.0))
  (define y1 (line! (+ cy 14.0 (text-block-height title) 6.0) (t 'rv.meta (j d 'by "?") (j d 'when "")) 'small DIM))
  (define y2 (if (non-empty-string? (j d 'request "")) (line! y1 (t 'rv.request (j d 'request "")) 'body TEXT) y1))
  (define y3 (if (non-empty-string? (j d 'summary "")) (line! y2 (j d 'summary "") 'small DIM) y2))
  (define its (items s))
  (define on (rv-checked s))
  (define groups (changeset-groups its))
  (define counts (string-join (for/list ([g (in-list groups)])
                                (format "~a ~a/~a" (group-label (first g) (second g))
                                        (length (filter (lambda (it) (memv (j it 'n) on)) (third g))) (length (third g))))
                              " · "))
  (define y4 (line! (+ y3 2.0) counts 'small DIM-ON-ACCENT))
  ;; 列表（裁剪 + 滚动）
  (define by (+ y4 6.0))
  (define bh (- (+ cy ch) by FOOT-H))
  (set-rv-view-h! s bh)
  (drawlist-set-clip! dl (vector cx by cw bh))
  (define ed? (editable? s))
  (define y-end
    (for/fold ([yy (- by (rv-scroll s))]) ([g (in-list groups)])
      (define gkey (cons (first g) (second g)))
      (define sel (filter selectable? (third g)))
      (define all-on? (and (pair? sel) (andmap (lambda (it) (memv (j it 'n) on)) sel)))
      (drawlist-rect! dl (+ cx 8.0) yy (- cw 16.0) 26.0 0.17 0.18 0.22 1.0 #:radius 5.0)
      (when ed? (checkbox! dl (+ cx PAD) (+ yy 5.5) all-on? (pair? sel)))
      (define gl (lay (format "~a（~a）" (group-label (first g) (second g)) (length (third g))) 'body (- inner 30.0) #f))
      (paint-text! dl at gl (+ cx PAD (if ed? 24.0 0.0)) (+ yy 4.0) TEXT #:scale scale)
      (when (and ed? (pair? sel)) (hit! 'toggle-group gkey (+ cx 8.0) yy (- cw 16.0) 26.0))
      (for/fold ([ry (+ yy 32.0)]) ([it (in-list (third g))])
        (define ok? (selectable? it))
        (define c? (and (memv (j it 'n) on) #t))
        (define tx (+ cx PAD (if ed? 24.0 0.0)))
        (when ed? (checkbox! dl (+ cx PAD) (+ ry 2.0) c? ok?))
        ;; 第一行：会话标题（新建项目：项目名）+ 右边「变成什么」
        (define parts (change-parts it))
        (define pw (for/sum ([p (in-list parts)]) (+ 8.0 (text-block-width (lay (car p) 'small 1000.0 #f)))))
        (define head (if (equal? (j it 'op) "new-project") (j it 'project "") (j it 'title (t 'sb.untitled))))
        (define hb (fit-line lay (if (string? head) head "?") 'body (max 60.0 (- inner (- tx cx PAD) pw 8.0))))
        (paint-text! dl at hb tx ry (if ok? TEXT DIM) #:scale scale)
        (when (equal? (j it 'op) "rename")                                       ; 改名：旧标题画删除线
          (drawlist-rect! dl tx (+ ry (/ (text-block-line-height hb) 2)) (text-block-width hb) 1.0 0.62 0.64 0.69 1.0))
        (for/fold ([px (- (+ cx cw) PAD pw)]) ([p (in-list parts)])
          (define pb (lay (car p) 'small 1000.0 #f))
          (paint-text! dl at pb (+ px 8.0) (+ ry 2.0) (if ok? (cdr p) DIM) #:scale scale)
          (+ px 8.0 (text-block-width pb)))
        ;; 第二行：理由（灰）或不能做的原因（警告色）
        (define note (if ok? (j it 'reason #f) (format "~a~a" (t (case (j it 'status) [("stale") 'rv.st-stale] [("noop") 'rv.st-noop] [else 'rv.st-invalid]))
                                                       (let ([w (j it 'why #f)]) (if (string? w) (format "（~a）" w) "")))))
        (define nh (if (and (string? note) (non-empty-string? note))
                       (let ([nb (fit-line lay note 'small (- inner (- tx cx PAD)))])
                         (paint-text! dl at nb tx (+ ry (text-block-line-height hb) 2.0) (if ok? DIM WARN) #:scale scale)
                         (+ 2.0 (text-block-line-height nb)))
                       0.0))
        (define rh (+ (text-block-line-height hb) nh 10.0))
        (when (and ed? ok?) (hit! 'toggle (j it 'n) (+ cx 8.0) (- ry 3.0) (- cw 16.0) rh))
        (+ ry rh))))
  (drawlist-set-clip! dl #f)
  (set-rv-content-h! s (+ (- y-end by) (rv-scroll s)))
  (set-rv-scroll! s (max 0.0 (min (rv-scroll s) (max 0.0 (- (rv-content-h s) bh)))))
  ;; 脚：按钮
  (define fy (+ cy ch (- FOOT-H) 10.0))
  (drawlist-rect! dl (+ cx 10.0) (- fy 8.0) (- cw 20.0) 1.0 0.26 0.27 0.31 1.0)
  (define buttons
    (case (j d 'state 'proposed)
      [(proposed) (list (list 'apply (t 'rv.apply (length on)) (pair? on) #t) (list 'discard (t 'rv.discard) #t #f))]
      [(applied) (list (list 'undo (t 'rv.undo) #t #f) (list 'close (t 'rv.close) #t #f))]
      [else (list (list 'close (t 'rv.close) #t #f))]))
  (for/fold ([bx (- (+ cx cw) PAD)]) ([b (in-list (reverse buttons))])
    (define tb (lay (second b) 'body 1000.0 #f))
    (define bw (+ (text-block-width tb) 24.0))
    (define x0 (- bx bw))
    (define primary? (fourth b))
    ;; 主按钮的蓝：0.25/0.40/0.75 上白字只有 4.42
    (drawlist-rect! dl x0 fy bw 30.0 (if primary? 0.20 (car BUTTON)) (if primary? 0.33 (cadr BUTTON)) (if primary? 0.66 (caddr BUTTON)) (if (third b) 1.0 0.5) #:radius 6.0)
    (paint-text! dl at tb (+ x0 12.0) (+ fy (/ (- 30.0 (text-block-line-height tb)) 2)) (if (third b) TEXT TEXT-OFF) #:scale scale)
    (when (third b) (hit! (first b) #t x0 fy bw 30.0))
    (- x0 10.0))
  (set-rv-hits! s hits)
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
