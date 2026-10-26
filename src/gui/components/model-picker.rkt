#lang racket/base
;; model-picker.rkt — 模型 / 推理档选择组件（G5→G6）：状态行左端一个芯片（供应商 · 模型 · 推理档），点开在芯片上方弹出菜单。
;; 菜单两栏 + 底栏：左栏路由池（「@名」，有成员不可用时标出）+ 供应商（同一供应商的多个帐号 = 内核实例 base[label]，归在供应商名下）；
;; 右栏：当前是供应商 → 它的模型；当前是路由池 → 池成员（帐号 · 模型）与各自状态（可用 / 冷却剩余 / 额度用尽 / 鉴权失败），点成员 = 手动固定；
;; 底栏推理档。高度上限 MAX-H，两栏各自滚动（滚轮按指针所在的栏）。选中后菜单不关（可以接着改别的），
;; 只有再点芯片、点菜单外、Esc 才关。只保存显示状态与命中区域；选中项是动作，发什么请求由拼装层决定。
;; needs text-layout → provides model-picker

(require racket/list
         "../compute/drawlist.rkt" "../compute/palette.rkt" "../compute/layout.rkt" "../compute/paint.rkt" "../compute/rich.rkt"
         "../compute/hub.rkt" "../compute/interfaces.rkt" "../compute/i18n.rkt"
         "../compute/view.rkt" "../compute/tokens.rkt")                 ; 视图平面（元素树 → 布局 → drawlist + 命中）、设计参数

(define WARN '(0.95 0.75 0.35 1.0))
(define OK '(0.45 0.80 0.55 1.0))

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

;; 视图平面（compute/view.rkt）：界面是元素树（数据），坐标 / 裁剪 / 命中区由布局推出（2026-10-26 起；原来边算坐标边画、边登记命中区）。
;; 尺寸来自设计参数（compute/tokens.rkt）。
(define ROW-H (tk 'menu-row-h)) (define HEAD-H (tk 'menu-head-h)) (define TITLE-H (tk 'menu-title-h))
(define MENU-W (tk 'picker-menu-w)) (define LEFT-W (tk 'picker-left-w)) (define MAX-H (tk 'menu-max-h)) (define PAD (tk 'menu-pad))
(define MENU-BG '(0.14 0.15 0.18 1.0)) (define MENU-BORDER '(0.26 0.27 0.32 1.0)) (define SEP '(0.24 0.25 0.30 1.0))
(define SEL-FILL '(0.20 0.28 0.45 1.0)) (define BAR '(0.40 0.42 0.48 1.0))
(define HOVER-FILL (append HOVER-ON-MENU '(1.0)))
;; 响应式规则（元数据）：菜单打开 / 当前供应商变了 → 两栏把选中项滚到视区中间（reveal?）；关菜单 → 清悬停
(define REACTIVE-RULES '((reveal-selected #:on (opened provider-changed) #:panes (left right))
                         (clear-hover #:on (closed))))

(define (row-h r) (if (eq? (car r) 'group) TITLE-H ROW-H))

;; 一行 → 元素
(define (row-el s r)
  (define-values (kind payload txt sel? note indent?) (apply values r))
  (case kind
    [(group)
     (ui-box #:dir 'row #:h TITLE-H #:align 'end #:pad (vector 0.0 8.0 3.0 6.0)
          (ui-text txt #:font 'small #:color DIM #:grow 1)
          (and note (ui-text (car note) #:font 'small #:color (cdr note) #:fit? #f)))]
    [else
     (define hov? (equal? (mp-hover s) (cons kind payload)))
     (define ind (if indent? 12.0 0.0))
     (ui-box #:dir 'row #:h ROW-H #:pad (vector 0.0 6.0 0.0 0.0)            ; 右边 6：滚动条
          (ui-box #:dir 'row #:grow 1 #:align 'center #:fill (cond [sel? SEL-FILL] [hov? HOVER-FILL] [else #f]) #:radius (tk 'radius-s)
               #:hit (cons kind payload) #:pad (vector 0.0 6.0 0.0 (+ 6.0 ind))
               (ui-box #:w 16.0 (and sel? (ui-text "✓" #:font 'body #:color TEXT)))   ; 选中标记占固定列
               (ui-text txt #:font 'body #:color TEXT #:grow 1)                      ; 长名字截断成一行
               (and note (ui-text (car note) #:font 'small #:fit? #f
                               #:color (if (and sel? (equal? (cdr note) DIM)) DIM-ON-ACCENT (cdr note))))))]))   ; 选中行的蓝底上：DIM 只有 3.6

(define (pane-el s id title rows offset)
  (ui-box #:dir 'column #:grow 1
       (ui-box #:dir 'row #:h HEAD-H #:align 'end #:pad (vector 0.0 6.0 4.0 6.0) (ui-text title #:font 'small #:color DIM #:grow 1))
       (apply ui-scroll id #:offset offset #:bar BAR (for/list ([r (in-list rows)]) (row-el s r)))))

(define (footer-el s)
  (define levels (filter string? (j (mp-options s) 'reasoning_levels '("off" "low" "medium" "high" "max"))))
  (define rules (filter hash? (j (mp-options s) 'adaptations '())))
  (ui-box #:dir 'column #:pad (vector 0.0 PAD 0.0 PAD)
       (ui-box #:dir 'row #:h TITLE-H #:align 'end #:pad (vector 0.0 0.0 2.0 0.0) (ui-text (t 'mp.reasoning) #:font 'small #:color DIM #:fit? #f))
       ;; 推理档（分段按钮）
       (apply ui-box #:dir 'row #:h ROW-H
              (for/list ([l (in-list levels)])
                (define cur? (equal? l (j (mp-state s) 'reasoning)))
                (define hov? (equal? (mp-hover s) (cons 'reasoning l)))
                (ui-box #:grow 1 #:pad 2.0 #:hit (cons 'reasoning l)
                     (ui-box #:dir 'row #:grow 1 #:align 'center #:fill (cond [cur? SEL-FILL] [hov? HOVER-FILL] [else #f]) #:radius (tk 'radius-s)
                          (ui-text l #:font 'small #:color (if cur? TEXT DIM) #:align 'center #:grow 1)))))
       (ui-box #:dir 'row #:h TITLE-H #:align 'end #:pad (vector 0.0 0.0 2.0 0.0) (ui-text (t 'mp.adapt) #:font 'small #:color DIM #:fit? #f))
       ;; 当前目标匹配到的增强适配规则（点 = 开 / 关；开着的带 ✓；放不下的不画）
       (if (null? rules)
           (ui-box #:dir 'row #:h ROW-H #:align 'center (ui-text (t 'mp.adapt-none) #:font 'small #:color DIM #:fit? #f))
           (apply ui-box #:dir 'row #:h ROW-H #:gap 6.0 #:align 'center #:overflow 'hide
                  (for/list ([r (in-list rules)])
                    (define on? (eq? (j r 'enabled #t) #t))
                    (define hov? (equal? (mp-hover s) (cons 'adapt (j r 'name))))
                    (ui-box #:dir 'row #:h (- ROW-H 6.0) #:align 'center #:pad (vector 0.0 8.0 0.0 8.0) #:radius (tk 'radius-s)
                         #:fill (if on? (list 0.18 0.30 0.24 (if hov? 1.0 0.9)) (list 0.15 0.16 0.19 (if hov? 1.0 0.9)))
                         #:hit (cons 'adapt (vector (j r 'name) on?))
                         (ui-text (string-append (if on? "✓ " "") (j r 'name "?")) #:font 'small #:color (if on? TEXT DIM) #:fit? #f)))))))

;; 芯片画在 (x, y)（y = 状态行文字顶）；打开时菜单底边贴芯片上方。返回芯片宽度（无状态时 0，不画）。
(define (mp-render! s dl x y max-w win-h scale)
  (define rc (mp-rc s))
  (cond
    [(not (mp-state s)) (set-mp-hits! s '()) (set-mp-panes! s #f) 0.0]
    [else
     (define chip (ui-box #:dir 'row #:align 'center #:pad (vector 0.0 (tk 'chip-pad-x) 0.0 (tk 'chip-pad-x)) #:radius (tk 'radius-m)
                       #:fill (if (mp-open? s) '(0.24 0.27 0.36 1.0) '(0.17 0.18 0.22 1.0)) #:hit (cons 'toggle #t)
                       (ui-text (chip-text s) #:font 'small #:color (if (mp-open? s) DIM-ON-ACCENT DIM))))   ; 打开时芯片底是蓝灰：DIM 只有 3.8
     (define-values (nw nh) (measure (rich-ctx-layout rc) chip (max 60.0 max-w)))
     (define cw (min nw (max 60.0 max-w)))
     (define ch (+ nh 4.0))
     (define cy (- y 2.0))
     (define-values (chip-hits _r _s) (render-view! rc dl chip x cy cw ch scale))
     (define menu-hits (if (mp-open? s) (render-menu! s dl x cy scale) (begin (set-mp-panes! s #f) '())))
     (set-mp-hits! s (reverse (append chip-hits menu-hits)))       ; 找命中时从后画的找起
     cw])
) ; end define mp-render!

;; 选中行在栏内的 y（没有选中行 → 0）
(define (selected-offset rows)
  (let loop ([rs rows] [y 0.0])
    (cond [(null? rs) 0.0]
          [(list-ref (car rs) 3) y]
          [else (loop (cdr rs) (+ y (row-h (car rs))))]))
) ; end define selected-offset

;; 菜单：约束（宽 MENU-W；高 = 上限 MAX-H 与芯片上方可用空间的较小者，至少放得下表头 + 两行 + 底栏；底边贴芯片上方 6）
;; → 元素树 → 布局 / 绘制；返回命中区（绘制顺序）
(define (render-menu! s dl x chip-y scale)
  (define foot-h (* 2 (+ TITLE-H ROW-H)))                                     ; 推理档 + 适配规则
  (define mh (max (+ (* 2 PAD) foot-h HEAD-H (* 2 ROW-H)) (min MAX-H (- chip-y 6.0 8.0))))
  (define my (- chip-y 6.0 mh))
  (define body-h (- mh (* 2 PAD) foot-h 1.0))
  (define view-h (- body-h HEAD-H))
  (define lrows (left-rows s))
  (define rrows (right-rows s))
  (when (mp-reveal? s)                                                        ; 规则 reveal-selected
    (set-mp-reveal?! s #f)
    (set-mp-scroll-l! s (- (selected-offset lrows) (/ (- view-h ROW-H) 2)))
    (set-mp-scroll-r! s (- (selected-offset rrows) (/ (- view-h ROW-H) 2))))
  (define tree
    (ui-box #:dir 'column #:w MENU-W #:h mh #:pad (vector PAD 0.0 PAD 0.0) #:fill MENU-BG #:stroke MENU-BORDER #:radius (tk 'radius-l)
         #:hit (cons 'menu #t) #:id 'menu                                     ; 菜单底板：点空白不关闭
         (ui-box #:dir 'row #:h body-h
              (ui-box #:dir 'column #:w LEFT-W #:pad (vector 0.0 4.0 0.0 4.0) #:id 'left-pane
                   (pane-el s 'left (t 'mp.providers) lrows (mp-scroll-l s)))
              (ui-box #:w 1.0 #:fill SEP)                                        ; 两栏分隔
              (ui-box #:dir 'column #:grow 1 #:pad (vector 0.0 4.0 0.0 3.0) #:id 'right-pane
                   (pane-el s 'right (let ([r (current-route s)]) (if r (t 'mp.members-of (j r 'name)) (t 'mp.models-of (j (mp-state s) 'provider "?"))))
                            rrows (mp-scroll-r s))))
         (ui-box #:dir 'row #:h 1.0 #:pad (vector 0.0 PAD 0.0 PAD) (ui-box #:grow 1 #:fill SEP))   ; 与底栏分隔
         (footer-el s)))
  (define-values (hits regions scrolls) (render-view! (mp-rc s) dl tree x my MENU-W mh scale))
  (set-mp-scroll-l! s (vector-ref (hash-ref scrolls 'left) 0))
  (set-mp-scroll-r! s (vector-ref (hash-ref scrolls 'right) 0))
  (set-mp-panes! s (vector (hash-ref regions 'left-pane) (hash-ref regions 'right-pane) (hash-ref regions 'menu)))
  hits
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
