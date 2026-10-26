#lang racket/base
;; model-picker.rkt — 模型 / 推理档选择组件（G5→G6）：状态行左端一个芯片（供应商 · 模型 · 推理档），点开在芯片上方弹出菜单。
;; 菜单两栏 + 底栏：左栏路由池（「@名」，有成员不可用时标出）+ 供应商（同一供应商的多个帐号 = 内核实例 base[label]，归在供应商名下）；
;; 右栏：当前是供应商 → 它的模型；当前是路由池 → 池成员（帐号 · 模型）与各自状态（可用 / 冷却剩余 / 额度用尽 / 鉴权失败），点成员 = 手动固定；
;; 底栏推理档。高度上限 MAX-H，两栏各自滚动（滚轮按指针所在的栏）。选中后菜单不关（可以接着改别的），
;; 只有再点芯片、点菜单外、Esc 才关。只保存显示状态与命中区域；选中项是动作，发什么请求由拼装层决定。
;; needs text-layout → provides model-picker

(require racket/list
         "../compute/drawlist.rkt" "../compute/palette.rkt" "../compute/layout.rkt" "../compute/paint.rkt" "../compute/rich.rkt"
         "../compute/hub.rkt" "../compute/interfaces.rkt" "../compute/i18n.rkt")

(define WARN '(0.95 0.75 0.35 1.0))
(define OK '(0.45 0.80 0.55 1.0))
(define ROW-H 26.0)
(define HEAD-H 24.0)
(define TITLE-H 22.0)
(define MENU-W 520.0)
(define LEFT-W 200.0)
(define MAX-H 360.0)
(define PAD 10.0)

;; state：hash（provider model reasoning）；options：内核 providers 事件；scroll-l / scroll-r：两栏偏移；
;; reveal?：下一帧把选中项滚进视区；panes：(vector 左栏矩形 右栏矩形 菜单矩形)，矩形 = (vector x y w h)，滚轮路由用
(struct mp (rc [state #:mutable] [options #:mutable] [open? #:mutable] [hover #:mutable] [hits #:mutable]
               [scroll-l #:mutable] [scroll-r #:mutable] [reveal? #:mutable] [panes #:mutable]))

(define (j h k [d #f]) (if (hash? h) (hash-ref h k d) d))

;; 当前选择是路由池时，它的状态（providers 事件里的 routes 项，route_status 事件会更新）
(define (current-route s)
  (define cur (j (mp-state s) 'provider))
  (and (string? cur) (> (string-length cur) 1) (char=? (string-ref cur 0) #\@)
       (findf (lambda (r) (equal? (j r 'name) (substring cur 1))) (j (mp-options s) 'routes '()))))

(define (route-current-member r)
  (define i (j r 'current))
  (define ms (j r 'members '()))
  (and (exact-nonnegative-integer? i) (< i (length ms)) (list-ref ms i)))

(define (chip-text s)
  (define r (current-route s))
  (define st (mp-state s))
  (cond
    [r (define m (route-current-member r))
       (define base (t 'mp.chip-route (j r 'name) (j m 'instance "?") (j m 'model (j st 'model "?")) (j st 'reasoning "?")))
       (define bad (j r 'unavailable 0))
       (if (and (exact-integer? bad) (> bad 0)) (t 'mp.chip-warn base bad) base)]
    [else (t 'mp.chip (j st 'provider "?") (j st 'model "?") (j st 'reasoning "?"))]))

;; 秒 → 「45s」「3m」「2h」
(define (short-duration secs)
  (cond [(not (real? secs)) "?"]
        [(< secs 60) (format "~as" (inexact->exact (ceiling secs)))]
        [(< secs 3600) (format "~am" (inexact->exact (ceiling (/ secs 60))))]
        [else (format "~ah" (inexact->exact (ceiling (/ secs 3600))))]))

;; 成员状态 → 附注（可用 → #f）；冷却剩余按收到状态的时刻往后推
(define (member-note s m)
  (case (j m 'state "ok")
    [("cooling") (define left (j m 'seconds_left))
                 (define age (- (current-seconds) (j (mp-options s) 'routes-received (current-seconds))))
                 (cons (t 'mp.cooling (short-duration (if (real? left) (max 0 (- left age)) #f))) WARN)]
    [("exhausted") (cons (t 'mp.exhausted) WARN)]
    [("auth") (cons (t 'mp.auth-failed) WARN)]
    [else #f]))

;; 实例名 "deepseek[work]" → (values "deepseek" "work")；无标签 → (values 名 #f)
(define (split-instance n)
  (define m (regexp-match #rx"^([^[]+)\\[(.*)\\]$" n))
  (if m (values (cadr m) (caddr m)) (values n #f))
) ; end define split-instance

;; 行：(list kind payload 文字 选中? 附注 缩进?)；附注 = (cons 文字 颜色) | #f；kind 'group 是不可点的分组头
(define (left-rows s)
  (define ps (filter (lambda (p) (string? (j p 'name))) (j (mp-options s) 'providers '())))
  (define cur (j (mp-state s) 'provider))
  (define bases (remove-duplicates (for/list ([p (in-list ps)]) (let-values ([(b _l) (split-instance (j p 'name))]) b))))
  (define (note-of p) (and (eq? (j p 'has_key) #f) (cons (t 'mp.no-key) WARN)))
  (define routes (filter (lambda (r) (string? (j r 'name))) (j (mp-options s) 'routes '())))
  (append
   ;; 路由池在最上面
   (if (null? routes) '()
       (cons (list 'group #f (t 'mp.routes) #f #f #f)
             (for/list ([r (in-list routes)])
               (define ref (string-append "@" (j r 'name)))
               (define bad (j r 'unavailable 0))
               (list 'provider ref ref (equal? ref cur)
                     (if (and (exact-integer? bad) (> bad 0)) (cons (t 'mp.unavailable bad) WARN) (cons (t 'mp.members (length (j r 'members '()))) DIM))
                     #t))))
   (append*
   (for/list ([b (in-list bases)])
     (define members (filter (lambda (p) (let-values ([(b2 _l) (split-instance (j p 'name))]) (equal? b2 b))) ps))
     (cond
       [(and (= (length members) 1) (equal? (j (car members) 'name) b))      ; 只有默认帐号：一行
        (list (list 'provider b b (equal? b cur) (note-of (car members)) #f))]
       [else                                                                   ; 多帐号：供应商名做组头，帐号缩进
        (cons (list 'group #f b #f (cons (t 'mp.accounts (length members)) DIM) #f)
              (for/list ([p (in-list members)])
                (define-values (_b l) (split-instance (j p 'name)))
                (list 'provider (j p 'name) (or l (t 'mp.default-account)) (equal? (j p 'name) cur) (note-of p) #t)))]))))
) ; end define left-rows

(define (current-entry s)
  (findf (lambda (p) (equal? (j p 'name) (j (mp-state s) 'provider))) (j (mp-options s) 'providers '()))
) ; end define current-entry

(define (right-rows s)
  (define r (current-route s))
  (if r
      ;; 路由池：成员（帐号 · 模型）与状态；点成员 = 手动固定
      (for/list ([m (in-list (j r 'members '()))] [i (in-naturals)])
        (list 'member i (format "~a · ~a" (j m 'instance "?") (j m 'model "?")) (eqv? i (j r 'current)) (member-note s m) #f))
      (provider-models s)))

(define (provider-models s)
  (define e (current-entry s))
  (define cur (j (mp-state s) 'model))
  (define ms (let ([ms (filter string? (j e 'models '()))]) (if (and (string? cur) (not (member cur ms))) (cons cur ms) ms)))
  (define loaded (j e 'loaded '()))
  (for/list ([m (in-list ms)]) (list 'model m m (equal? m cur) (and (member m loaded) (cons (t 'mp.loaded) OK)) #f))
) ; end define right-rows

(define (row-h r) (if (eq? (car r) 'group) TITLE-H ROW-H))

;; 芯片画在 (x, y)（y = 状态行文字顶）；打开时菜单底边贴芯片上方。返回芯片宽度（无状态时 0，不画）。
(define (mp-render! s dl x y max-w win-h scale)
  (define rc (mp-rc s))
  (define lay (rich-ctx-layout rc))
  (define at (rich-ctx-atlas rc))
  (define hits '())
  (define (hit! kind payload hx hy hw hh) (set! hits (cons (vector kind payload hx hy hw hh) hits)))
  (cond
    [(not (mp-state s)) (set-mp-hits! s '()) (set-mp-panes! s #f) 0.0]
    [else
     (define tb (fit-line lay (chip-text s) 'small (max 60.0 (- max-w 16.0))))
     (define cw (+ (text-block-width tb) 16.0))
     (define ch (+ (text-block-height tb) 4.0))
     (define cy (- y 2.0))
     (drawlist-rect! dl x cy cw ch (if (mp-open? s) 0.24 0.17) (if (mp-open? s) 0.27 0.18) (if (mp-open? s) 0.36 0.22) 1.0 #:radius 6.0)
     (paint-text! dl at tb (+ x 8.0) y (if (mp-open? s) DIM-ON-ACCENT DIM) #:scale scale)   ; 打开时芯片底是蓝灰：DIM 只有 3.8
     (hit! 'toggle #t x cy cw ch)
     (if (mp-open? s) (render-menu! s dl lay at x cy scale hit!) (set-mp-panes! s #f))
     (set-mp-hits! s hits)
     cw])
) ; end define mp-render!

;; 选中行在栏内的 y（没有选中行 → 0）
(define (selected-offset rows)
  (let loop ([rs rows] [y 0.0])
    (cond [(null? rs) 0.0]
          [(list-ref (car rs) 3) y]
          [else (loop (cdr rs) (+ y (row-h (car rs))))]))
) ; end define selected-offset

(define (render-menu! s dl lay at x chip-y scale hit!)
  (define foot-h (* 2 (+ TITLE-H ROW-H)))                                     ; 推理档 + 适配规则
  (define mh (max (+ (* 2 PAD) foot-h HEAD-H (* 2 ROW-H)) (min MAX-H (- chip-y 6.0 8.0))))
  (define my (- chip-y 6.0 mh))
  (define body-y (+ my PAD))
  (define body-h (- mh (* 2 PAD) foot-h))
  (define lx (+ x 4.0))
  (define lw (- LEFT-W 8.0))
  (define rx (+ x LEFT-W 4.0))
  (define rw (- MENU-W LEFT-W 8.0))
  (drawlist-rect! dl (- x 1.0) (- my 1.0) (+ MENU-W 2.0) (+ mh 2.0) 0.26 0.27 0.32 1.0 #:radius 9.0)   ; 描边
  (drawlist-rect! dl x my MENU-W mh 0.14 0.15 0.18 1.0 #:radius 8.0)
  (drawlist-rect! dl (+ x LEFT-W) (+ body-y 2.0) 1.0 (- body-h 4.0) 0.24 0.25 0.30 1.0)            ; 两栏分隔
  (drawlist-rect! dl (+ x PAD) (+ body-y body-h) (- MENU-W (* 2 PAD)) 1.0 0.24 0.25 0.30 1.0)         ; 与底栏分隔
  (hit! 'menu #t x my MENU-W mh)                                                                     ; 菜单底板：点空白不关闭
  (define lrows (left-rows s))
  (define rrows (right-rows s))
  (define view-h (- body-h HEAD-H))
  ;; 刚打开 / 刚换了供应商：两栏都把选中项滚到视区中间
  (when (mp-reveal? s)
    (set-mp-reveal?! s #f)
    (set-mp-scroll-l! s (- (selected-offset lrows) (/ (- view-h ROW-H) 2)))
    (set-mp-scroll-r! s (- (selected-offset rrows) (/ (- view-h ROW-H) 2))))
  ;; 一栏：表头固定，行裁剪滚动；返回裁剪后的偏移
  (define (pane! rows px pw title scroll)
    (define tb (fit-line lay title 'small (- pw 12.0)))
    (paint-text! dl at tb (+ px 6.0) (+ body-y (- HEAD-H (text-block-height tb) 4.0)) DIM #:scale scale)
    (define vy (+ body-y HEAD-H))
    (define content-h (for/sum ([r (in-list rows)]) (row-h r)))
    (define smax (max 0.0 (- content-h view-h)))
    (define sc (max 0.0 (min smax scroll)))
    (drawlist-set-clip! dl (vector px vy pw view-h))
    (for/fold ([ry (- vy sc)]) ([r (in-list rows)])
      (define h (row-h r))
      (when (and (< ry (+ vy view-h)) (> (+ ry h) vy)) (row! r px pw ry (cons vy (+ vy view-h))))
      (+ ry h))
    (drawlist-set-clip! dl #f)
    (when (> smax 0.0)
      (define th (max 20.0 (* view-h (/ view-h content-h))))
      (drawlist-rect! dl (+ px pw -4.0) (+ vy (* (- view-h th) (/ sc smax))) 3.0 th 0.40 0.42 0.48 1.0 #:radius 1.5))
    sc)
  (define (row! r px pw ry clip)
    (define-values (kind payload text sel? note indent?) (apply values r))
    (define h (row-h r))
    (define (hit-row! k p hx hw)
      (define y0 (if clip (max ry (car clip)) ry))
      (define y1 (if clip (min (+ ry h) (cdr clip)) (+ ry h)))
      (when (> y1 y0) (hit! k p hx y0 hw (- y1 y0))))
    (case kind
      [(group)
       (define tb (fit-line lay text 'small (- pw 80.0)))
       (paint-text! dl at tb (+ px 6.0) (+ ry (- h (text-block-height tb) 3.0)) DIM #:scale scale)
       (when note
         (define nb (lay (car note) 'small 1000.0 #f))
         (paint-text! dl at nb (- (+ px pw) 8.0 (text-block-width nb)) (+ ry (- h (text-block-height nb) 3.0)) (cdr note) #:scale scale))]
      [else
       (define hov? (equal? (mp-hover s) (cons kind payload)))
       (when (or sel? hov?)
         (drawlist-rect! dl px ry (- pw 6.0) h (if sel? 0.20 (car HOVER-ON-MENU)) (if sel? 0.28 (cadr HOVER-ON-MENU)) (if sel? 0.45 (caddr HOVER-ON-MENU)) 1.0 #:radius 5.0))
       (define ind (if indent? 12.0 0.0))
       (define tb (fit-line lay text 'body (- pw 30.0 ind (if note 58.0 6.0))))   ; 长名字截断成一行
       (define ty (+ ry (/ (- h (text-block-line-height tb)) 2)))
       (when sel? (paint-text! dl at (lay "✓" 'body 100.0 #f) (+ px 6.0 ind) ty TEXT #:scale scale))   ; 选中标记占固定列
       (paint-text! dl at tb (+ px 22.0 ind) ty TEXT #:scale scale)
       (when note
         (define nb (lay (car note) 'small 1000.0 #f))
         (paint-text! dl at nb (- (+ px pw) 12.0 (text-block-width nb)) (+ ry (/ (- h (text-block-height nb)) 2))
                      (if (and sel? (equal? (cdr note) DIM)) DIM-ON-ACCENT (cdr note)) #:scale scale))   ; 选中行的蓝底上：DIM 只有 3.6
       (hit-row! kind payload px (- pw 6.0))]))
  (set-mp-scroll-l! s (pane! lrows lx lw (t 'mp.providers) (mp-scroll-l s)))
  (set-mp-scroll-r! s (pane! rrows rx rw (let ([r (current-route s)]) (if r (t 'mp.members-of (j r 'name)) (t 'mp.models-of (j (mp-state s) 'provider "?"))))
                             (mp-scroll-r s)))
  (set-mp-panes! s (vector (vector lx body-y lw body-h) (vector rx body-y rw body-h) (vector x my MENU-W mh)))
  ;; 底栏：推理档（分段按钮）
  (define fy (+ body-y body-h))
  (define ftb (lay (t 'mp.reasoning) 'small 1000.0 #f))
  (paint-text! dl at ftb (+ x PAD) (+ fy (- TITLE-H (text-block-height ftb) 2.0)) DIM #:scale scale)
  (define levels (filter string? (j (mp-options s) 'reasoning_levels '("off" "low" "medium" "high" "max"))))
  (define ly (+ fy TITLE-H))
  (define bw (/ (- MENU-W (* 2 PAD)) (max 1 (length levels))))
  (for ([l (in-list levels)] [i (in-naturals)])
    (define bx (+ x PAD (* i bw)))
    (define cur? (equal? l (j (mp-state s) 'reasoning)))
    (define hov? (equal? (mp-hover s) (cons 'reasoning l)))
    (when (or cur? hov?)
      (drawlist-rect! dl (+ bx 2.0) (+ ly 2.0) (- bw 4.0) (- ROW-H 4.0)
                      (if cur? 0.20 (car HOVER-ON-MENU)) (if cur? 0.28 (cadr HOVER-ON-MENU)) (if cur? 0.45 (caddr HOVER-ON-MENU)) 1.0 #:radius 5.0))
    (define tb (lay l 'small 1000.0 #f))
    (paint-text! dl at tb (+ bx (/ (- bw (text-block-width tb)) 2)) (+ ly (/ (- ROW-H (text-block-height tb)) 2)) (if cur? TEXT DIM) #:scale scale)
    (hit! 'reasoning l bx ly bw ROW-H))
  ;; 底栏第二行：当前目标匹配到的增强适配规则（点 = 开 / 关；开着的带 ✓）
  (define ay (+ ly ROW-H))
  (define atb (lay (t 'mp.adapt) 'small 1000.0 #f))
  (paint-text! dl at atb (+ x PAD) (+ ay (- TITLE-H (text-block-height atb) 2.0)) DIM #:scale scale)
  (define rules (filter hash? (j (mp-options s) 'adaptations '())))
  (define cy (+ ay TITLE-H))
  (cond
    [(null? rules)
     (define tb (lay (t 'mp.adapt-none) 'small 1000.0 #f))
     (paint-text! dl at tb (+ x PAD) (+ cy (/ (- ROW-H (text-block-height tb)) 2)) DIM #:scale scale)]
    [else
     (for/fold ([cx (+ x PAD)]) ([r (in-list rules)])
       (define on? (eq? (j r 'enabled #t) #t))
       (define label (string-append (if on? "✓ " "") (j r 'name "?")))
       (define tb (fit-line lay label 'small (max 40.0 (- (+ x MENU-W) PAD cx 16.0))))
       (define cw (+ (text-block-width tb) 16.0))
       (cond
         [(> (+ cx cw) (- (+ x MENU-W) PAD)) cx]                                ; 放不下的不画
         [else
          (define hov? (equal? (mp-hover s) (cons 'adapt (j r 'name))))
          (drawlist-rect! dl cx (+ cy 3.0) cw (- ROW-H 6.0)
                          (if on? 0.18 0.15) (if on? 0.30 0.16) (if on? 0.24 0.19) (if hov? 1.0 0.9) #:radius 5.0)
          (paint-text! dl at tb (+ cx 8.0) (+ cy (/ (- ROW-H (text-block-height tb)) 2)) (if on? TEXT DIM) #:scale scale)
          (hit! 'adapt (vector (j r 'name) on?) cx (+ cy 3.0) cw (- ROW-H 6.0))
          (+ cx cw 6.0)]))])
  (void)
) ; end define render-menu!

(define (in-rect? r x y)
  (and r (<= (vector-ref r 0) x (+ (vector-ref r 0) (vector-ref r 2))) (<= (vector-ref r 1) y (+ (vector-ref r 1) (vector-ref r 3)))))

(define (find-hit s x y)
  (for/first ([h (in-list (mp-hits s))]
              #:when (and (<= (vector-ref h 2) x (+ (vector-ref h 2) (vector-ref h 4)))
                          (<= (vector-ref h 3) y (+ (vector-ref h 3) (vector-ref h 5)))))
    h)
) ; end define find-hit

;; → (list 'toggle) | (list 'provider 名) | (list 'model 名) | (list 'member 下标) | (list 'reasoning 档) | (list 'adapt 名 开?)
;;   | (list 'menu) | (list 'close) | #f
;; 选中后菜单留着；菜单打开时点任何地方都被这里消化：菜单外 → close；菜单空白 / 组头 → (list 'menu)
(define (mp-hit s x y)
  (define h (find-hit s x y))
  (cond
    [h (define kind (vector-ref h 0))
       (case kind
         [(toggle) (set-mp-open?! s (not (mp-open? s))) (set-mp-reveal?! s (mp-open? s)) (list 'toggle)]
         [(menu) (list 'menu)]
         [(adapt) (list 'adapt (vector-ref (vector-ref h 1) 0) (not (vector-ref (vector-ref h 1) 1)))]   ; → 名 新的开关状态
         [else (list kind (vector-ref h 1))])]
    [(mp-open? s) (set-mp-open?! s #f) (set-mp-hover! s #f) (list 'close)]
    [else #f])
) ; end define mp-hit

(define (mp-hover! s x y)
  (define h (and (mp-open? s) (find-hit s x y)))
  (define v (and h (memq (vector-ref h 0) '(provider model member reasoning adapt))
                 (cons (vector-ref h 0) (let ([p (vector-ref h 1)]) (if (vector? p) (vector-ref p 0) p)))))
  (begin0 (not (equal? v (mp-hover s))) (set-mp-hover! s v))
) ; end define mp-hover!

;; 滚轮：指针在哪一栏滚哪一栏；→ 是否处理了（指针不在菜单上 → #f，拼装层交给下面的区域）
(define (mp-scroll! s x y dy)
  (define ps (and (mp-open? s) (mp-panes s)))
  (cond
    [(not ps) #f]
    [(in-rect? (vector-ref ps 0) x y) (set-mp-scroll-l! s (- (mp-scroll-l s) dy)) #t]
    [(in-rect? (vector-ref ps 1) x y) (set-mp-scroll-r! s (- (mp-scroll-r s) dy)) #t]
    [(in-rect? (vector-ref ps 2) x y) #t]                                                ; 菜单上别处：吞掉，不滚下面
    [else #f])
) ; end define mp-scroll!

(define (mp-hit-center s kind payload)
  (for/first ([h (in-list (mp-hits s))] #:when (and (eq? (vector-ref h 0) kind) (equal? (vector-ref h 1) payload)))
    (cons (+ (vector-ref h 2) (/ (vector-ref h 4) 2)) (+ (vector-ref h 3) (/ (vector-ref h 5) 2))))
) ; end define mp-hit-center

;; 状态合并：内核各事件只带部分字段（ready 无 reasoning、ok 只带改了的那项）；换了供应商 → 右栏重新对准当前模型
(define (merge-state! s h)
  (define old (or (mp-state s) (hasheq)))
  (define new (for/fold ([acc old]) ([k (in-list '(provider model reasoning))])
                (define v (j h k))
                (if (string? v) (hash-set acc k v) acc)))
  (unless (equal? (j old 'provider) (j new 'provider)) (set-mp-reveal?! s #t))
  (set-mp-state! s new)
  ;; state 事件也带当前目标的适配规则
  (when (list? (j h 'adaptations)) (set-mp-options! s (hash-set (or (mp-options s) (hasheq)) 'adaptations (j h 'adaptations))))
) ; end define merge-state!

(define model-picker-component
  (component 'model-picker (list text-layout) (list model-picker)
             (lambda (use config)
               (define s (mp ((use text-layout) 'rich) #f #f #f #f '() 0.0 0.0 #f #f))
               (list (implement model-picker
                                'set-state! (lambda (h) (merge-state! s h))
                                'state (lambda () (mp-state s))
                                'reset! (lambda () (set-mp-state! s #f) (set-mp-options! s #f) (set-mp-open?! s #f) (set-mp-hover! s #f))
                                'set-options! (lambda (opts)
                                                (set-mp-options! s (hash-set opts 'routes-received (current-seconds)))
                                                (merge-state! s (hash-set opts 'provider (j opts 'current))))
                                ;; route_status 事件：替换 options 里同名的池（没有就加上）
                                'set-route! (lambda (rs)
                                              (define opts (or (mp-options s) (hasheq)))
                                              (define others (filter (lambda (r) (not (equal? (j r 'name) (j rs 'name)))) (j opts 'routes '())))
                                              (set-mp-options! s (hash-set* opts 'routes (append others (list rs)) 'routes-received (current-seconds))))
                                'render! (lambda (dl x y max-w win-h scale) (mp-render! s dl x y max-w win-h scale))
                                'hit (lambda (x y) (mp-hit s x y))
                                'open? (lambda () (mp-open? s))
                                'open! (lambda () (set-mp-open?! s #t) (set-mp-reveal?! s #t))
                                'close! (lambda () (set-mp-open?! s #f) (set-mp-hover! s #f))
                                'hover! (lambda (x y) (mp-hover! s x y))
                                'scroll! (lambda (x y dy) (mp-scroll! s x y dy))
                                'hit-center (lambda (kind payload) (mp-hit-center s kind payload)))))))

(provide model-picker-component)
