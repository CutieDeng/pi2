#lang racket/base
;; settings.rkt — 设置页组件（⌘, 开关；盖在整个窗口上的面板）：左栏分节，右栏内容。
;;   通用：界面语言（跟随系统 / 中文 / English）、操作日志路径
;;   帐号与密钥：按供应商分组列出帐号（实例）与 key 状态；设置 / 更换 key、添加帐号（输入框，key 显示为圆点）
;;   权限：五种模式（strict normal auto read-only yolo），对当前会话立即生效
;;   适配规则：全部规则（来源、说明、匹配条件），逐条开关；新建规则（名字 → 匹配的模型 → 追加的 system prompt），删设置页建的
;;   路由池：池与成员状态；新建池、添加成员（选帐号 → 选模型 / 输入模型 id）、调顺序、移除成员；删池（两次点击）
;;   主机（一窗多主机）：本机 + 服务器（ssh）；连接 / 断开 / 移除（两次点击）；添加服务器（ssh 目标 → serve 名字）
;;   指令：全局文件（本机）+ 目录 catalog 里每个项目 / super-workspace / 工作区的指令（多行输入；design-memory.md M1）
;;   记忆：待确认的（模型记到全局的）确认 / 拒绝；按作用域列条目：置顶、编辑（多行）、删除；模型记全局要不要先问（design-memory.md M2）
;;   项目索引：状态（找到 / 描述了 / 待描述 / 失败、正在描述哪个）、暂停 / 继续、重新扫描、描述用的模型；按主机列仓库：编辑描述（锁定）、重新生成（design-memory.md §8）
;;   费用上限：今日估算花费（按模型）；每天 / 每轮软上限（设、取消）
;;   模型别名：子 agent 用（spawn_agent 的 model 参数）；添加（别名 → 模型 id）、删除
;;   快捷键：一览
;; 只保存显示状态、输入框编辑器与命中区；点击结果是动作，发什么请求由拼装层决定。
;; needs text-layout → provides settings-view

(require racket/list racket/string
         "../compute/drawlist.rkt" "../compute/palette.rkt" "../compute/layout.rkt" "../compute/paint.rkt" "../compute/rich.rkt"
         "../compute/editor.rkt" "../compute/hub.rkt" "../compute/interfaces.rkt" "../compute/i18n.rkt"
         "../compute/gui-settings.rkt")

(define WARN '(0.95 0.75 0.35 1.0))
(define OK '(0.45 0.80 0.55 1.0))
(define HEAD '(0.55 0.70 1.0 1.0))
(define NAV-W 170.0)
(define PAD 18.0)
(define ROW 30.0)
(define SECTIONS '(general hosts accounts permissions instructions memory projects budget rules pools aliases tools chat changes advanced shortcuts))
(define NAME-RX #px"^[A-Za-z0-9_.-]{1,40}$")

;; edit：#f | (vector kind base label step editor)
;;   kind 'key / 'new-account（base 供应商，label 帐号，step 'label → 'key）
;;        'alias（step 'label → 'value，base = 别名）
;;        'new-pool（step 'label；回车后进入 pick）
;;        'member-model（base 池，label 帐号，step 'value：手输模型 id）
;;        'rule（step 'label → 'model → 'value；base = 规则名，label = 模型正则）
;;        'budget（base 'daily | 'turn，step 'value：美元数）
;;        'host（step 'label：ssh 目标 → 'value：serve 名字，空 = default）
;;        'instructions（base = catalog 实体 id，label = 名字，step 'value：多行；存空 = 清掉）
;;        'memory（base = 记忆条目 id，step 'value：多行；存空 = 删掉）
;;        'hix-desc（base = 仓库键「主机\u0001目录」，step 'value：多行，见 host-index-store.rkt desc->text）；'hix-endpoint / 'hix-model（一行）
;; pick：#f | (vector 池 帐号或#f 新池?)：给池添加成员时先选帐号、再选模型
;; notice：输入不合法时显示在输入框下面的提示
(struct sv (rc [open? #:mutable] [section #:mutable] [data #:mutable] [edit #:mutable] [confirm #:mutable]
               [hits #:mutable] [hover #:mutable] [scroll #:mutable] [content-rect #:mutable] [caret #:mutable]
               [pick #:mutable] [notice #:mutable]))

(define (j h k [d #f]) (if (hash? h) (hash-ref h k d) d))
(define (data s k [d #f]) (hash-ref (sv-data s) k d))

(define (section-label sec) (t (string->symbol (format "set.sec.~a" sec))))

(define (split-instance n)
  (define m (regexp-match #rx"^([^[]+)\\[(.*)\\]$" n))
  (if m (values (cadr m) (caddr m)) (values n #f))
) ; end define split-instance

;; ---------------------------------------------------------------- 绘制

(define (render! s dl x y w h scale)
  (define rc (sv-rc s))
  (define lay (rich-ctx-layout rc))
  (define at (rich-ctx-atlas rc))
  (define hits '())
  (define (hit! kind payload hx hy hw hh) (set! hits (cons (vector kind payload hx hy hw hh) hits)))
  (set-sv-caret! s #f)
  (drawlist-rect! dl x y w h 0.0 0.0 0.0 0.45)                                   ; 压暗背后的窗口
  (hit! 'outside #f x y w h)
  (define pw (min 820.0 (- w 60.0)))
  (define ph (min 580.0 (- h 60.0)))
  (define px (+ x (/ (- w pw) 2)))
  (define py (+ y (/ (- h ph) 2)))
  (drawlist-rect! dl (- px 1.0) (- py 1.0) (+ pw 2.0) (+ ph 2.0) 0.28 0.29 0.34 1.0 #:radius 11.0)
  (drawlist-rect! dl px py pw ph 0.12 0.12 0.15 1.0 #:radius 10.0)
  (hit! 'panel #f px py pw ph)
  ;; 左栏：标题 + 分节
  (define ttb (lay (t 'set.title) 'title 1000.0 #f))
  (paint-text! dl at ttb (+ px PAD) (+ py PAD) TEXT #:scale scale)
  ;; 多主机：设置作用于哪台（标题下一行）
  (define hosttb (let ([hl (data s 'host-label)]) (and hl (fit-line lay (t 'set.host-of hl) 'small (- NAV-W 24.0)))))
  (when hosttb (paint-text! dl at hosttb (+ px PAD) (+ py PAD (text-block-height ttb) 2.0) HEAD #:scale scale))
  ;; 分节一行的高度按可用空间收（13 节在 580 高的面板里放不下 32 的行距：最后一节压到底下的「关闭（Esc）」上——显示检查查出的）
  (define ctb (lay (t 'set.close) 'small 1000.0 #f))
  (define nav-top (+ py PAD (text-block-height ttb) (if hosttb (+ 2.0 (text-block-height hosttb)) 0.0) 14.0))
  (define nav-bottom (- (+ py ph) PAD (text-block-height ctb) 8.0))
  (define step (min (+ ROW 2.0) (/ (- nav-bottom nav-top) (length SECTIONS))))
  (define rh (- step 2.0))
  (for/fold ([ny nav-top]) ([sec (in-list SECTIONS)])
    (define cur? (eq? sec (sv-section s)))
    (define hov? (equal? (sv-hover s) (cons 'section sec)))
    (when (or cur? hov?)
      (drawlist-rect! dl (+ px 8.0) ny (- NAV-W 16.0) rh (if cur? 0.20 0.17) (if cur? 0.28 0.18) (if cur? 0.45 0.22) 1.0 #:radius 6.0))
    (define tb (fit-line lay (section-label sec) 'body (- NAV-W 36.0)))
    (paint-text! dl at tb (+ px 20.0) (+ ny (/ (- rh (text-block-line-height tb)) 2)) (if cur? TEXT DIM) #:scale scale)
    (hit! 'section sec (+ px 8.0) ny (- NAV-W 16.0) rh)
    (+ ny step))
  (paint-text! dl at ctb (+ px PAD) (- (+ py ph) PAD (text-block-height ctb)) DIM #:scale scale)
  (drawlist-rect! dl (+ px NAV-W) (+ py 12.0) 1.0 (- ph 24.0) 0.22 0.23 0.28 1.0)
  ;; 右栏：内容（裁剪、可滚动）
  (define cx (+ px NAV-W PAD))
  (define cw (- pw NAV-W (* 2 PAD)))
  (define cy (+ py PAD))
  (define ch (- ph (* 2 PAD)))
  (set-sv-content-rect! s (vector cx cy cw ch))
  (define htb (lay (section-label (sv-section s)) 'title 1000.0 #f))
  (paint-text! dl at htb cx cy TEXT #:scale scale)
  (define body-y (+ cy (text-block-height htb) 12.0))
  (define body-h (- (+ cy ch) body-y))
  (drawlist-set-clip! dl (vector (- cx 4.0) body-y (+ cw 8.0) body-h))
  (define sc (sv-scroll s))
  ;; 内容绘制器共享的小工具：y 是绝对坐标；返回下一行的 y
  (define (in-view? yy hh) (and (< yy (+ body-y body-h)) (> (+ yy hh) body-y)))
  (define (hit/clip! kind payload hx hy hw hh)
    (define y0 (max hy body-y))
    (define y1 (min (+ hy hh) (+ body-y body-h)))
    (when (> y1 y0) (hit! kind payload hx y0 hw (- y1 y0))))
  (define (text! yy str font color [ww cw] #:x [xx cx])
    (define tb (lay str font ww #f))
    (when (in-view? yy (text-block-height tb)) (paint-text! dl at tb xx yy color #:scale scale))
    (+ yy (text-block-height tb)))
  (define (button! bx yy label kind payload #:color [color TEXT] #:fill [fill '(0.20 0.21 0.26)])
    (define tb (lay label 'small 1000.0 #f))
    (define bw (+ (text-block-width tb) 18.0))
    (define bh (+ (text-block-height tb) 8.0))
    (define hov? (equal? (sv-hover s) (cons kind payload)))
    (when (in-view? yy bh)
      (drawlist-rect! dl bx yy bw bh (+ (car fill) (if hov? 0.05 0.0)) (+ (cadr fill) (if hov? 0.05 0.0)) (+ (caddr fill) (if hov? 0.05 0.0)) 1.0 #:radius 6.0)
      (paint-text! dl at tb (+ bx 9.0) (+ yy 4.0) color #:scale scale))
    (hit/clip! kind payload bx yy bw bh)
    bw)
  (define (field! yy)                                                           ; 正在编辑的输入框（key 显示为圆点）
    (define e (sv-edit s))
    (define-values (base label step ed) (values (vector-ref e 1) (vector-ref e 2) (vector-ref e 3) (vector-ref e 4)))
    (define prompt (field-prompt (vector-ref e 0) base label step))
    (define y1 (text! yy prompt 'small DIM))
    (define-values (disp caret _m) (editor-display ed))
    (define shown (if (eq? step 'key) (make-string (string-length disp) (integer->char #x2022)) disp))
    (define tb (lay (if (string=? shown "") " " shown) 'mono (- cw 24.0) #f))
    (define fh (+ (text-block-height tb) 12.0))
    (define fy (+ y1 6.0))
    (drawlist-rect! dl cx fy cw fh 0.30 0.40 0.62 1.0 #:radius 7.0)
    (drawlist-rect! dl (+ cx 1.0) (+ fy 1.0) (- cw 2.0) (- fh 2.0) 0.09 0.09 0.11 1.0 #:radius 6.0)
    (paint-text! dl at tb (+ cx 10.0) (+ fy 6.0) TEXT #:scale scale)
    (define-values (k cxp) (text-block-caret tb (min caret (string-length shown))))   ; k = 第几行（多行输入）
    (define crect (vector (+ cx 10.0 cxp) (+ fy 6.0 (* k (text-block-line-height tb))) 2.0 (text-block-line-height tb)))
    (drawlist-rect! dl (vector-ref crect 0) (vector-ref crect 1) 1.5 (vector-ref crect 3) 0.55 0.70 1.0 1.0)
    (set-sv-caret! s crect)
    (define by (+ fy fh 8.0))
    (define w1 (button! cx by (t 'set.save) 'save #f #:fill '(0.18 0.30 0.24)))
    (button! (+ cx w1 8.0) by (t 'set.cancel) 'cancel-edit #f)
    (if (sv-notice s) (+ (text! (+ by 30.0) (sv-notice s) 'small WARN) 6.0) (+ by 30.0)))
  ;; 一行放不下就换行的一组按钮：items = (list 文字 kind payload 颜色)
  (define (flow! yy items #:x [x0 cx])
    (define-values (_bx by)
      (for/fold ([bx x0] [by yy]) ([it (in-list items)])
        (define bw (+ (text-block-width (lay (car it) 'small 1000.0 #f)) 18.0))
        (define-values (nx ny) (if (and (> (+ bx bw) (+ cx cw)) (> bx x0)) (values x0 (+ by ROW)) (values bx by)))
        (button! nx ny (car it) (cadr it) (caddr it) #:color (cadddr it))
        (values (+ nx bw 8.0) ny)))
    (+ by ROW))
  (define y-end
    (let ([y0 (- body-y sc)])
      (case (sv-section s)
        [(general) (render-general s y0 text! button! field! cx cw)]
        [(accounts) (render-accounts s y0 text! button! field! cx cw dl)]
        [(permissions) (render-permissions s y0 text! button! hit/clip! cx cw dl in-view?)]
        [(instructions) (render-instructions s y0 text! button! field! cx cw)]
        [(memory) (render-memory s y0 text! button! field! cx cw)]
        [(projects) (render-projects s y0 text! button! field! cx cw)]
        [(rules) (render-rules s y0 text! button! field! cx cw)]
        [(pools) (render-pools s y0 text! button! field! flow! cx cw)]
        [(aliases) (render-aliases s y0 text! button! field! cx cw)]
        [(budget) (render-budget s y0 text! button! field! cx cw)]
        [(hosts) (render-hosts s y0 text! button! field! cx cw)]
        [(tools) (render-tools s y0 text! button! field! cx cw)]
        [(chat) (render-chat s y0 text! button! field! cx cw)]
        [(changes) (render-changes s y0 text! button! cx cw)]
        [(advanced) (render-advanced s y0 text! button! cx cw)]
        [else (render-shortcuts s y0 text! cx cw)])))
  (drawlist-set-clip! dl #f)
  ;; 滚动范围：内容高度超出视区才能滚
  (define content-h (- y-end (- body-y sc)))
  (set-sv-scroll! s (max 0.0 (min (sv-scroll s) (max 0.0 (- content-h body-h)))))
  (set-sv-hits! s hits)
) ; end define render!

(define (render-general s y text! button! field! cx cw)
  (define y1 (text! y (t 'set.lang) 'body TEXT))
  (define cur (data s 'lang 'system))
  (define y2
    (let ([by (+ y1 8.0)])
      (for/fold ([bx cx]) ([l (in-list '(system zh en))])
        (define label (case l [(system) (t 'set.lang.system)] [(zh) "中文"] [else "English"]))
        (+ bx 8.0 (button! bx by (if (eq? l cur) (string-append "✓ " label) label) 'lang l
                           #:fill (if (eq? l cur) '(0.20 0.28 0.45) '(0.20 0.21 0.26)))))
      (+ by 36.0)))
  ;; 聊天里粘贴的密钥：不识别 / 只提示 / 自动
  (define y2a (text! (+ y2 14.0) (t 'set.secret-mode) 'body TEXT))
  (define cur-sm (data s 'secret-mode 'suggest))
  (define y2b
    (let ([by (+ y2a 8.0)])
      (for/fold ([bx cx]) ([m (in-list '(off suggest auto))])
        (define label (t (string->symbol (format "set.secret-mode.~a" m))))
        (+ bx 8.0 (button! bx by (if (eq? m cur-sm) (string-append "✓ " label) label) 'secret-mode m
                           #:fill (if (eq? m cur-sm) '(0.20 0.28 0.45) '(0.20 0.21 0.26)))))
      (+ by 32.0)))
  (define y2c0 (text! y2b (t 'set.secret-note) 'small DIM))
  ;; 自定义格式（secret-patterns.rktd）：列表 + ✕；「＋ 添加格式」
  (define pats (filter string? (data s 'secret-patterns '())))
  (define y2c1 (text! (+ y2c0 8.0) (t 'set.secret-patterns) 'small DIM))
  (define y2c2 (for/fold ([yy (+ y2c1 4.0)]) ([p (in-list pats)])
                 (text! yy p 'mono TEXT (- cw 60.0) #:x (+ cx 12.0))
                 (button! (+ cx cw -36.0) (- yy 3.0) "✕" 'remove-secret-pattern p #:color DIM)
                 (+ yy ROW)))
  (define e (sv-edit s))
  (define y2c (cond [(and e (eq? (vector-ref e 0) 'secret-pattern)) (field! (+ y2c2 4.0))]
                    [else (button! (+ cx 12.0) (+ y2c2 2.0) (t 'set.add-secret-pattern) 'add-secret-pattern #f #:color HEAD) (+ y2c2 2.0 ROW)]))
  ;; 界面缩放（字号与版面一起缩放）
  (define y2d (text! (+ y2c 14.0) (t 'set.zoom) 'body TEXT))
  (define cur-z (data s 'zoom 1.0))
  (define y2e
    (let ([by (+ y2d 8.0)])
      (for/fold ([bx cx]) ([z (in-list ZOOM-LEVELS)])
        (define label (format "~a%" (inexact->exact (round (* 100 z)))))
        (+ bx 8.0 (button! bx by (if (= z cur-z) (string-append "✓ " label) label) 'zoom z
                           #:fill (if (= z cur-z) '(0.20 0.28 0.45) '(0.20 0.21 0.26)))))
      (+ by 32.0)))
  (define y2f0 (text! y2e (t 'set.zoom-note) 'small DIM))
  ;; 流式文字的检查点（决定 1）：缺省关（完成后才落盘）
  (define y2g (text! (+ y2f0 14.0) (t 'set.checkpoint) 'body TEXT))
  (define cur-cp (data s 'stream-checkpoint #f))
  (define y2h
    (let ([by (+ y2g 8.0)])
      (for/fold ([bx cx]) ([v (in-list '(#f 1 5 30))])
        (define label (if v (format "~a s" v) (t 'set.checkpoint-off)))
        (+ bx 8.0 (button! bx by (if (equal? v cur-cp) (string-append "✓ " label) label) 'stream-checkpoint v
                           #:fill (if (equal? v cur-cp) '(0.20 0.28 0.45) '(0.20 0.21 0.26)))))
      (+ by 32.0)))
  (define y2f1 (text! y2h (t 'set.checkpoint-note) 'small DIM))
  ;; 侧栏位置（宽度：拖侧栏与对话区之间的分隔线）
  (define y2i (text! (+ y2f1 14.0) (t 'set.sidebar-side) 'body TEXT))
  (define cur-side (data s 'sidebar-side 'left))
  (define y2j
    (let ([by (+ y2i 8.0)])
      (for/fold ([bx cx]) ([v (in-list '(left right))])
        (define label (t (if (eq? v 'left) 'set.sidebar-left 'set.sidebar-right)))
        (+ bx 8.0 (button! bx by (if (eq? v cur-side) (string-append "✓ " label) label) 'sidebar-side v
                           #:fill (if (eq? v cur-side) '(0.20 0.28 0.45) '(0.20 0.21 0.26)))))
      (+ by 32.0)))
  (define y2j1 (text! y2j (t 'set.sidebar-note) 'small DIM))
  ;; 侧栏材质：液态玻璃面板 / 玻璃·透出桌面（窗口透明，整列玻璃）/ 不透明（macOS 26 的 NSGlassEffectView，由系统合成）
  (define y2k (text! (+ y2j1 14.0) (t 'set.sidebar-material) 'body TEXT))
  (define cur-mat (data s 'sidebar-material 'glass))
  (define y2l
    (let ([by (+ y2k 8.0)])
      (for/fold ([bx cx]) ([v (in-list '(glass desktop opaque))])
        (define label (t (case v [(glass) 'set.material-glass] [(desktop) 'set.material-desktop] [else 'set.material-opaque])))
        (+ bx 8.0 (button! bx by (if (eq? v cur-mat) (string-append "✓ " label) label) 'sidebar-material v
                           #:fill (if (eq? v cur-mat) '(0.20 0.28 0.45) '(0.20 0.21 0.26)))))
      (+ by 32.0)))
  (define y2mat (text! y2l (t 'set.material-note) 'small DIM))
  ;; 侧栏的「更早」：超过 N 天没动的会话收进去（缺省收起；§11.4 B）
  (define y2m (text! (+ y2mat 14.0) (t 'set.older-days) 'body TEXT))
  (define cur-od (data s 'sidebar-older-days 14))
  (define y2n
    (let ([by (+ y2m 8.0)])
      (for/fold ([bx cx]) ([v (in-list '(7 14 30 90))])
        (define label (t 'set.older-days-n v))
        (+ bx 8.0 (button! bx by (if (eqv? v cur-od) (string-append "✓ " label) label) 'sidebar-older-days v
                           #:fill (if (eqv? v cur-od) '(0.20 0.28 0.45) '(0.20 0.21 0.26)))))
      (+ by 32.0)))
  (define y2o (text! y2n (t 'set.older-days-note) 'small DIM))
  ;; 自动起标题（本机模型，同「工具输出」的设置）
  (define y2p (text! (+ y2o 14.0) (t 'set.auto-title) 'body TEXT))
  (define cur-at (data s 'auto-title #t))
  (define y2q
    (let ([by (+ y2p 8.0)])
      (for/fold ([bx cx]) ([v (in-list '(#t #f))])
        (define label (t (if v 'set.on 'set.off)))
        (+ bx 8.0 (button! bx by (if (eq? v cur-at) (string-append "✓ " label) label) 'auto-title v
                           #:fill (if (eq? v cur-at) '(0.20 0.28 0.45) '(0.20 0.21 0.26)))))
      (+ by 32.0)))
  (define y2f (text! y2q (t 'set.auto-title-note) 'small DIM))
  (define y3 (text! (+ y2f 14.0) (t 'set.journal) 'body TEXT))
  (define y4 (text! (+ y3 4.0) (or (data s 'journal) (t 'dbg.memory-only)) 'mono DIM))
  (text! (+ y4 4.0) (t 'set.journal-note) 'small DIM)
) ; end define render-general

(define (render-accounts s y text! button! field! cx cw dl)
  (define ps (filter (lambda (p) (string? (j p 'name))) (j (data s 'providers) 'providers '())))
  (cond
    [(null? ps) (text! y (t 'set.loading) 'body DIM)]
    [else
     (define bases (remove-duplicates (for/list ([p (in-list ps)]) (let-values ([(b _l) (split-instance (j p 'name))]) b))))
     (define e (sv-edit s))
     (for/fold ([yy y]) ([b (in-list bases)])
       (define members (filter (lambda (p) (let-values ([(b2 _l) (split-instance (j p 'name))]) (equal? b2 b))) ps))
       (define needs? (ormap (lambda (p) (eq? (j p 'needs_key) #t)) members))
       (define y1 (text! (+ yy 6.0) b 'body TEXT))
       (define y2
         (for/fold ([ry (+ y1 2.0)]) ([p (in-list members)])
           (define-values (_b l) (split-instance (j p 'name)))
           (define status (cond [(not (eq? (j p 'needs_key) #t)) (t 'set.key-none)]
                                [(eq? (j p 'has_key) #t) (t 'set.key-set)]
                                [else (t 'set.key-missing)]))
           (define color (cond [(not (eq? (j p 'needs_key) #t)) DIM] [(eq? (j p 'has_key) #t) OK] [else WARN]))
           (text! ry (format "· ~a" (or l (t 'mp.default-account))) 'body DIM 200.0 #:x (+ cx 12.0))
           (text! ry status 'small color 140.0 #:x (+ cx 230.0))
           (when (eq? (j p 'needs_key) #t)
             (button! (+ cx 370.0) (- ry 2.0) (if (eq? (j p 'has_key) #t) (t 'set.replace-key) (t 'set.set-key)) 'edit-key (j p 'name)))
           (define ry2 (+ ry ROW))
           ;; 正在给这个帐号设 key：输入框就在这一行下面
           (if (and e (eq? (vector-ref e 0) 'key) (equal? (vector-ref e 1) b) (equal? (vector-ref e 2) (or l "default")))
               (field! ry2)
               ry2)))
       (define y3
         (cond
           [(not needs?) y2]
           [(and e (eq? (vector-ref e 0) 'new-account) (equal? (vector-ref e 1) b)) (field! y2)]
           [else (button! (+ cx 12.0) y2 (t 'set.add-account) 'new-account b #:color HEAD) (+ y2 ROW)]))
       (+ y3 8.0))])
) ; end define render-accounts

(define (render-permissions s y text! button! hit/clip! cx cw dl in-view?)
  (define cur (data s 'permission "normal"))
  (define y-end
    (for/fold ([yy y]) ([m (in-list '("strict" "normal" "auto" "read-only" "yolo"))])
      (define sel? (equal? m cur))
      (define y1 (text! yy (format "~a  ~a" (if sel? "●" "○") m) 'body (if sel? TEXT DIM)))
      (define y2 (text! (+ y1 2.0) (t (string->symbol (format "set.perm.~a" m))) 'small (if (equal? m "yolo") WARN DIM) (- cw 24.0) #:x (+ cx 22.0)))
      (hit/clip! 'permission m cx yy cw (- y2 yy))
      (+ y2 12.0)))
  (text! (+ y-end 4.0) (t 'set.perm-note) 'small DIM)
) ; end define render-permissions

(define (render-rules s y text! button! field! cx cw)
  (define rules (filter hash? (data s 'rules '())))
  (define e (sv-edit s))
  (define y0 (text! y (t 'set.rules-note) 'small DIM))
  (define y-new
    (cond [(and e (eq? (vector-ref e 0) 'rule)) (field! (+ y0 8.0))]
          [else (button! cx (+ y0 8.0) (t 'set.new-rule) 'new-rule #f #:color HEAD) (+ y0 8.0 ROW)]))
  (cond
    [(null? rules) (text! (+ y-new 10.0) (t 'set.loading) 'body DIM)]
    [else
     (for/fold ([yy (+ y-new 10.0)]) ([r (in-list rules)])
       (define on? (eq? (j r 'enabled) #t))
       (button! cx yy (if on? (t 'set.rule-on) (t 'set.rule-off)) 'rule (vector (j r 'name) on?)
                #:fill (if on? '(0.18 0.30 0.24) '(0.20 0.21 0.26)) #:color (if on? TEXT DIM))
       (text! (+ yy 2.0) (format "~a  · ~a" (j r 'name "?") (j r 'source "?")) 'body TEXT (- cw 80.0) #:x (+ cx 70.0))
       (define m (j r 'match (hasheq)))
       (define match-str (string-join (filter values (list (and (string? (j m 'model)) (format "model ~~ ~a" (j m 'model)))
                                                           (and (string? (j m 'provider)) (format "provider ~~ ~a" (j m 'provider)))
                                                           (and (list? (j m 'reasoning)) (format "reasoning ∈ ~a" (string-join (j m 'reasoning) "/")))))
                                      " · "))
       (when (equal? (j r 'source) "settings")                                  ; 设置页建的：可删（两次点击）
         (define confirming? (equal? (sv-confirm s) (vector 'rule (j r 'name))))
         (button! (+ cx cw -110.0) (- yy 2.0) (if confirming? (t 'set.confirm-delete-pool) (t 'set.delete-pool))
                  (if confirming? 'delete-rule 'ask-delete-rule) (j r 'name) #:color (if confirming? WARN DIM)))
       (define y1 (text! (+ yy ROW) (j r 'description "") 'small DIM (- cw 70.0) #:x (+ cx 70.0)))
       (define y2 (text! (+ y1 2.0) (t 'set.rule-match (if (string=? match-str "") "*" match-str)) 'small DIM (- cw 70.0) #:x (+ cx 70.0)))
       (define y3 (if (string? (j r 'system_append)) (text! (+ y2 2.0) (t 'set.rule-append (j r 'system_append)) 'small DIM (- cw 70.0) #:x (+ cx 70.0)) y2))
       (+ y3 14.0))])
) ; end define render-rules

(define (members-of s pool)
  (define r (findf (lambda (r) (equal? (j r 'name) pool)) (filter hash? (j (data s 'providers) 'routes '()))))
  (if r (for/list ([m (in-list (j r 'members '()))]) (cons (j m 'instance "?") (j m 'model "?"))) '())
) ; end define members-of

(define (render-pools s y text! button! field! flow! cx cw)
  (define routes (filter hash? (j (data s 'providers) 'routes '())))
  (define accounts (filter (lambda (p) (string? (j p 'name))) (j (data s 'providers) 'providers '())))
  (define e (sv-edit s))
  (define pk (sv-pick s))
  (define (editing? kind [base #f]) (and e (eq? (vector-ref e 0) kind) (or (not base) (equal? (vector-ref e 1) base))))
  ;; 添加成员：先选帐号，再选模型（菜单里的模型列表 + 「其他…」手输）
  (define (pick-block! yy pool)
    (define acct (vector-ref pk 1))
    (cond
      [(not acct)
       (define y1 (text! yy (t 'set.pick-account) 'small DIM))
       (flow! (+ y1 6.0) (append (for/list ([p (in-list accounts)]) (list (j p 'name) 'pick-account (vector pool (j p 'name)) TEXT))
                                 (list (list (t 'set.cancel) 'pick-cancel #f DIM))))]
      [else
       (define p (findf (lambda (p) (equal? (j p 'name) acct)) accounts))
       (define y1 (text! yy (t 'set.pick-model acct) 'small DIM))
       (flow! (+ y1 6.0) (append (for/list ([m (in-list (if p (filter string? (j p 'models '())) '()))])
                                   (list m 'pick-model (vector pool acct m) TEXT))
                                 (list (list (t 'set.pick-other) 'pick-other (vector pool acct) HEAD)
                                       (list (t 'set.cancel) 'pick-cancel #f DIM))))]))
  (define y0 (text! y (t 'set.pools-note) 'small DIM))
  (define y-list
    (cond
      [(null? routes) (text! (+ y0 10.0) (t 'set.pools-none) 'body DIM)]
      [else
       (for/fold ([yy (+ y0 10.0)]) ([r (in-list routes)])
         (define name (j r 'name "?"))
         (define y1 (text! yy (format "@~a" name) 'body TEXT))
         (define confirming? (equal? (sv-confirm s) name))
         (button! (+ cx cw -110.0) (- yy 2.0) (if confirming? (t 'set.confirm-delete-pool) (t 'set.delete-pool))
                  (if confirming? 'delete-route 'ask-delete-route) name #:color (if confirming? WARN DIM))
         (define ms (j r 'members '()))
         (define y2
           (for/fold ([my (+ y1 4.0)]) ([m (in-list ms)] [i (in-naturals)])
             (define state (j m 'state "ok"))
             (text! my (format "~a ~a · ~a~a" (if (eqv? i (j r 'current)) "▸" " ") (j m 'instance "?") (j m 'model "?")
                               (if (equal? state "ok") "" (format "  [~a]" state)))
                    'mono (if (equal? state "ok") DIM WARN) (- cw 100.0) #:x (+ cx 12.0))
             (when (> i 0) (button! (+ cx cw -72.0) (- my 3.0) "↑" 'member-up (vector name i) #:color DIM))
             (button! (+ cx cw -36.0) (- my 3.0) "✕" 'member-remove (vector name i) #:color DIM)
             (+ my ROW)))
         (define y3
           (cond [(and pk (equal? (vector-ref pk 0) name)) (pick-block! (+ y2 2.0) name)]
                 [(editing? 'member-model name) (field! (+ y2 2.0))]
                 [else (button! (+ cx 12.0) y2 (t 'set.add-member) 'add-member name #:color HEAD) (+ y2 ROW)]))
         (+ y3 12.0))]))
  ;; 新建池：池名 → 选第一个成员（池在第一个成员加进去时才建出来）
  (define new-name (cond [(and pk (vector-ref pk 2)) (vector-ref pk 0)]
                         [(and (editing? 'member-model) (not (findf (lambda (r) (equal? (j r 'name) (vector-ref e 1))) routes))) (vector-ref e 1)]
                         [else #f]))
  (cond
    [(editing? 'new-pool) (field! (+ y-list 6.0))]
    [new-name
     (define y1 (text! (+ y-list 6.0) (format "@~a  ~a" new-name (t 'set.pool-new)) 'body TEXT))
     (if pk (pick-block! (+ y1 4.0) new-name) (field! (+ y1 4.0)))]
    [else (button! cx (+ y-list 6.0) (t 'set.new-pool) 'new-pool #f #:color HEAD) (+ y-list 6.0 ROW)])
) ; end define render-pools

(define (render-hosts s y text! button! field! cx cw)
  (define hosts (filter hash? (data s 'hosts '())))
  (define e (sv-edit s))
  (define y0 (text! y (t 'set.hosts-note) 'small DIM))
  (define y1
    (for/fold ([yy (+ y0 12.0)]) ([h (in-list hosts)])
      (define id (j h 'id "?"))
      (define status (j h 'status "connecting"))
      (define y1 (text! yy (format "~a~a" (j h 'label id) (if (eq? (j h 'active) #t) "  ●" "")) 'body TEXT (- cw 230.0)))
      (text! yy (t (string->symbol (format "set.host-status.~a" status))) 'small (if (equal? status "disconnected") WARN DIM) 120.0 #:x (+ cx cw -220.0))
      (define y2 (text! (+ y1 2.0) (j h 'detail "") 'mono DIM (- cw 20.0) #:x (+ cx 12.0)))
      ;; 按钮：连接 / 断开；可移除的（非本机）两次点击移除
      (define by (+ y2 6.0))
      (define w1 (if (equal? status "disconnected")
                     (button! cx by (t 'set.host-connect) 'host-connect id #:color HEAD)
                     (button! cx by (t 'set.host-disconnect) 'host-disconnect id #:color DIM)))
      (when (eq? (j h 'removable) #t)
        (define confirming? (equal? (sv-confirm s) (vector 'host id)))
        (button! (+ cx w1 8.0) by (if confirming? (t 'set.confirm-delete-pool) (t 'set.host-remove))
                 (if confirming? 'host-remove 'ask-host-remove) id #:color (if confirming? WARN DIM)))
      ;; 服务器：跨主机子 agent 的凭证与工作目录
      (define y3 (+ by ROW 4.0))
      (cond
        [(eq? (j h 'subagent) #t)
         (define cred (j h 'credential "proxy"))
         (define y4 (text! y3 (t 'set.host-sub-cred) 'small DIM))
         (define y5
           (let ([by2 (+ y4 4.0)])
             (for/fold ([bx (+ cx 12.0)]) ([m (in-list '("proxy" "share" "remote"))])
               (define label (t (string->symbol (format "set.host-cred.~a" m))))
               (+ bx 8.0 (button! bx by2 (if (equal? m cred) (string-append "✓ " label) label) 'host-credential (vector id m)
                                  #:fill (if (equal? m cred) '(0.20 0.28 0.45) '(0.20 0.21 0.26)))))
             (+ by2 30.0)))
         (define y6 (text! y5 (t (string->symbol (format "set.host-cred-note.~a" cred))) 'small (if (equal? cred "share") WARN DIM) (- cw 24.0) #:x (+ cx 12.0)))
         (cond
           [(and e (eq? (vector-ref e 0) 'host-workdir) (equal? (vector-ref e 1) id)) (+ (field! (+ y6 6.0)) 8.0)]
           [else
            (define y7 (text! (+ y6 6.0) (t 'set.host-workdir (or (j h 'workdir) (t 'set.host-workdir-home))) 'small DIM (- cw 120.0) #:x (+ cx 12.0)))
            (button! (+ cx cw -90.0) (+ y6 4.0) (t 'set.budget-change) 'host-workdir-edit id #:color HEAD)
            (+ y7 14.0)])]
        [else (+ y3 4.0)])))
  (cond [(and e (eq? (vector-ref e 0) 'host)) (field! (+ y1 4.0))]
        [else (button! cx (+ y1 4.0) (t 'set.add-host) 'add-host #f #:color HEAD) (+ y1 4.0 ROW)])
) ; end define render-hosts

(define (money x) (if (real? x) (format "$~a" (/ (round (* x 10000)) 10000.0)) "—"))

(define (render-budget s y text! button! field! cx cw)
  (define b (data s 'budget (hasheq)))
  (define all (data s 'budget-all #f))
  (define e (sv-edit s))
  (define y00 (text! y (t 'set.budget-note) 'small DIM))
  ;; 多主机：所有主机合计 + 分台 + 合计每天上限（GUI 存）
  (define y0
    (cond
      [(hash? all)
       (define y1 (text! (+ y00 12.0) (t 'set.budget-all (money (j all 'total 0))) 'body TEXT))
       (define y2 (for/fold ([yy (+ y1 2.0)]) ([h (in-list (filter hash? (j all 'hosts '())))])
                    (text! yy (t 'set.budget-all-host (j h 'label "?") (money (j h 'spent 0))
                                 (let ([c (j h 'cap)]) (if (real? c) (format " / ~a" (money c)) "")))
                           'mono DIM (- cw 20.0) #:x (+ cx 12.0))))
       (define cap (j all 'cap))
       (define y3 (text! (+ y2 10.0) (t 'set.budget-combined) 'body TEXT))
       (define y4 (text! (+ y3 2.0) (if (real? cap) (money cap) (t 'set.budget-none)) 'mono (if (real? cap) TEXT DIM) 200.0 #:x (+ cx 12.0)))
       (define y5 (if (eq? (j all 'snoozed) #t) (text! (+ y4 2.0) (t 'set.budget-combined-snoozed) 'small WARN) y4))
       (cond
         [(and e (eq? (vector-ref e 0) 'budget) (eq? (vector-ref e 1) 'combined)) (+ (field! (+ y5 6.0)) 10.0)]
         [else
          (define w1 (button! cx (+ y5 6.0) (if (real? cap) (t 'set.budget-change) (t 'set.budget-set)) 'budget-edit 'combined #:color HEAD))
          (when (real? cap) (button! (+ cx w1 8.0) (+ y5 6.0) (t 'set.budget-clear) 'budget-clear 'combined #:color DIM))
          (+ y5 6.0 ROW 14.0)])]
      [else y00]))
  (define y1 (text! (+ y0 12.0) (t 'set.budget-today (j b 'date "?") (money (j b 'spent_today 0))) 'body TEXT))
  (define y2 (for/fold ([yy (+ y1 2.0)]) ([m (in-list (filter hash? (j b 'by_model '())))])
               (text! yy (format "· ~a  ~a" (j m 'model "?") (money (j m 'usd 0))) 'mono DIM (- cw 20.0) #:x (+ cx 12.0))))
  (define y3 (if (eq? (j b 'snoozed) #t) (text! (+ y2 4.0) (t 'set.budget-snoozed) 'small WARN) y2))
  (define (cap-row! yy kind label-key)
    (define v (j b (if (eq? kind 'daily) 'daily_usd 'turn_usd)))
    (define cap? (real? v))
    (define y1 (text! yy (t label-key) 'body TEXT))
    (define y2 (text! (+ y1 2.0) (if cap? (money v) (t 'set.budget-none)) 'mono (if cap? TEXT DIM) 200.0 #:x (+ cx 12.0)))
    (cond
      [(and e (eq? (vector-ref e 0) 'budget) (eq? (vector-ref e 1) kind)) (field! (+ y2 6.0))]
      [else
       (define w1 (button! cx (+ y2 6.0) (if cap? (t 'set.budget-change) (t 'set.budget-set)) 'budget-edit kind #:color HEAD))
       (when cap? (button! (+ cx w1 8.0) (+ y2 6.0) (t 'set.budget-clear) 'budget-clear kind #:color DIM))
       (+ y2 6.0 ROW)]))
  (define y4 (cap-row! (+ y3 16.0) 'daily 'set.budget-daily))
  (cap-row! (+ y4 10.0) 'turn 'set.budget-turn)
) ; end define render-budget

(define (render-aliases s y text! button! field! cx cw)
  (define als (filter hash? (data s 'aliases '())))
  (define e (sv-edit s))
  (define y0 (text! y (t 'set.aliases-note) 'small DIM))
  (define y1
    (if (null? als)
        (text! (+ y0 10.0) (t 'set.aliases-none) 'body DIM)
        (for/fold ([yy (+ y0 10.0)]) ([a (in-list als)])
          (text! yy (j a 'name "?") 'mono TEXT 150.0)
          (text! yy (j a 'model "?") 'mono DIM (- cw 220.0) #:x (+ cx 160.0))
          (button! (+ cx cw -36.0) (- yy 3.0) "✕" 'alias-remove (j a 'name) #:color DIM)
          (+ yy ROW))))
  (cond [(and e (eq? (vector-ref e 0) 'alias)) (field! (+ y1 8.0))]
        [else (button! cx (+ y1 8.0) (t 'set.add-alias) 'add-alias #f #:color HEAD) (+ y1 8.0 ROW)])
) ; end define render-aliases

;; 工具输出的折叠预览（transcript.rkt preview-spec 的配置；gui.rktd 'tool-preview）
(define PREVIEW-LINES '(3 5 8 12 20))
(define PREVIEW-MODES '("head" "tail" "head_tail" "match" "smart"))
(define (preview-cfg s) (let ([c (data s 'tool-preview #f)]) (if (hash? c) c (hasheq 'lines 5 'mode "head"))))

(define (render-tools s y text! button! field! cx cw)
  (define c (preview-cfg s))
  (define e (sv-edit s))
  (define (editing? k) (and e (eq? (vector-ref e 0) k)))
  (define (choice-row! yy items cur kind)
    (for/fold ([bx cx]) ([it (in-list items)])
      (define label (cdr it))
      (define on? (equal? (car it) cur))
      (+ bx 8.0 (button! bx yy (if on? (string-append "✓ " label) label) kind (car it)
                         #:fill (if on? '(0.20 0.28 0.45) '(0.20 0.21 0.26)))))
    (+ yy 32.0))
  (define y0 (text! y (t 'set.preview-note) 'small DIM))
  (define y1 (text! (+ y0 12.0) (t 'set.preview-lines) 'body TEXT))
  (define y2 (choice-row! (+ y1 8.0) (for/list ([n PREVIEW-LINES]) (cons n (number->string n))) (hash-ref c 'lines 5) 'preview-lines))
  (define y3 (text! (+ y2 6.0) (t 'set.preview-mode) 'body TEXT))
  (define y4 (choice-row! (+ y3 8.0) (for/list ([m PREVIEW-MODES]) (cons m (t (string->symbol (format "set.preview-mode.~a" m))))) (hash-ref c 'mode "head") 'preview-mode))
  ;; 正则：keep（match 模式优先）/ drop（不显示）；本地模型（smart）
  (define (value-row! yy label-key k field-kind)
    (define y-a (text! yy (t label-key) 'small DIM))
    (cond [(editing? field-kind) (field! (+ y-a 4.0))]
          [else
           (define v (hash-ref c k #f))
           (define ty (text! (+ y-a 4.0) (if (and (string? v) (not (string=? v ""))) v (t 'set.preview-unset)) 'mono (if (string? v) TEXT DIM) (- cw 140.0) #:x (+ cx 12.0)))
           (button! (+ cx cw -120.0) (+ y-a 1.0) (t 'set.preview-edit) 'preview-edit field-kind #:color HEAD)
           (when (string? v) (button! (+ cx cw -40.0) (+ y-a 1.0) "✕" 'preview-clear k #:color DIM))
           (max (+ y-a 4.0 ROW) (+ ty 6.0))]))
  (define y5 (value-row! (+ y4 4.0) 'set.preview-keep 'keep 'preview-keep))
  (define y6 (value-row! (+ y5 4.0) 'set.preview-drop 'drop 'preview-drop))
  (define y7 (value-row! (+ y6 4.0) 'set.preview-model 'model 'preview-model))
  (define y8 (value-row! (+ y7 4.0) 'set.preview-endpoint 'endpoint 'preview-endpoint))
  (define y9 (text! y8 (t 'set.preview-smart-note) 'small DIM))
  ;; 按工具覆盖：「工具  模式 行数」+ ✕；＋ 添加
  (define y10 (text! (+ y9 14.0) (t 'set.preview-tools) 'body TEXT))
  (define tools (let ([ts (hash-ref c 'tools #f)]) (if (hash? ts) ts (hash))))
  (define y11 (for/fold ([yy (+ y10 8.0)]) ([name (in-list (sort (map (lambda (k) (format "~a" k)) (hash-keys tools)) string<?))])
                (define v (or (hash-ref tools name #f) (hash-ref tools (string->symbol name) (hasheq))))
                (text! yy name 'mono TEXT 150.0 #:x (+ cx 12.0))
                (text! yy (string-join (filter values (list (let ([m (j v 'mode #f)]) (and m (t (string->symbol (format "set.preview-mode.~a" m)))))
                                                            (let ([n (j v 'lines #f)]) (and n (t 'set.preview-n-lines n)))
                                                            (let ([k (j v 'keep #f)]) (and k (format "keep /~a/" k)))
                                                            (let ([k (j v 'drop #f)]) (and k (format "drop /~a/" k)))))
                                       " · ")
                       'small DIM (- cw 220.0) #:x (+ cx 170.0))
                (button! (+ cx cw -36.0) (- yy 3.0) "✕" 'preview-tool-remove name #:color DIM)
                (+ yy ROW)))
  (cond [(editing? 'preview-tool) (field! (+ y11 4.0))]
        [else (button! (+ cx 12.0) (+ y11 2.0) (t 'set.preview-add-tool) 'preview-tool-add #f #:color HEAD) (+ y11 2.0 ROW)])
) ; end define render-tools

;; 聊天目录（匿名工作区，design-workspace.md §10）：当前主机的根目录、三个上限、占用；改了发 set_chat_config，「立即清理」发 chat_cleanup
(define (render-chat s y text! button! field! cx cw)
  (define u (data s 'chat-usage #f))                                 ; 内核的 chat_usage 事件
  (define c (if (hash? u) (j u 'config (hasheq)) (hasheq)))
  (define e (sv-edit s))
  (define (editing? k) (and e (eq? (vector-ref e 0) k)))
  (define y0 (text! y (t 'set.chat-note) 'small DIM))
  (define (row! yy label-key value field-kind)
    (define y-a (text! yy (t label-key) 'small DIM))
    (cond [(editing? field-kind) (field! (+ y-a 4.0))]
          [else (define ty (text! (+ y-a 4.0) value 'mono TEXT (- cw 140.0) #:x (+ cx 12.0)))   ; 长路径会折行：按实际高度往下排
                (button! (+ cx cw -120.0) (+ y-a 1.0) (t 'set.preview-edit) 'chat-edit field-kind #:color HEAD)
                (max (+ y-a 4.0 ROW) (+ ty 6.0))]))
  (define (gbs v) (if (real? v) (format "~a GB" v) "—"))
  (define y1 (row! (+ y0 12.0) 'set.chat-root (let ([r (and (hash? u) (j u 'root #f))]) (if (string? r) r "—")) 'chat-root))
  (define y2 (row! (+ y1 4.0) 'set.chat-max-total (gbs (j c 'max_total_gb #f)) 'chat-max-total))
  (define y3 (row! (+ y2 4.0) 'set.chat-max-chat (gbs (j c 'max_chat_gb #f)) 'chat-max-chat))
  (define y4 (row! (+ y3 4.0) 'set.chat-min-free (gbs (j c 'min_free_gb #f)) 'chat-min-free))
  (define (size n) (cond [(not (real? n)) "?"] [(< n 1048576) (format "~a KB" (round (/ n 1024)))]
                         [(< n 1073741824) (format "~a MB" (/ (round (/ n 104857.6)) 10.0))] [else (format "~a GB" (/ (round (/ n 107374182.4)) 10.0))]))
  (define y5 (text! (+ y4 14.0) (if (hash? u)
                                    (t 'set.chat-usage (j u 'count 0) (size (j u 'total 0)) (size (j u 'free #f)) (if (j u 'complete #t) "" (t 'set.chat-partial)))
                                    (t 'set.chat-loading))
                    'body TEXT))
  (button! cx (+ y5 8.0) (t 'set.chat-cleanup) 'chat-cleanup #f #:color HEAD)
  (+ y5 8.0 ROW)
) ; end define render-chat

;; 高级：送给模型的隐藏文字（Unicode tag 字符，design-gui.md §30.4）——标出 / 原样
(define (render-advanced s y text! button! cx cw)
  (define y0 (text! y (t 'set.adv-hidden) 'body TEXT))
  (define cur (data s 'hidden-text-mode 'mark))
  (define y1
    (let ([by (+ y0 8.0)])
      (for/fold ([bx cx]) ([v (in-list '(mark raw))])
        (define label (t (if (eq? v 'mark) 'set.adv-hidden-mark 'set.adv-hidden-raw)))
        (+ bx 8.0 (button! bx by (if (eq? v cur) (string-append "✓ " label) label) 'hidden-text-mode v
                           #:fill (if (eq? v cur) '(0.20 0.28 0.45) '(0.20 0.21 0.26)))))
      (+ by 32.0)))
  (text! y1 (t 'set.adv-hidden-note) 'small DIM)
) ; end define render-advanced

;; 指令（design-memory.md M1）：每轮注入系统提示词，从一般到具体（后者优先）：全局文件 → 项目 → super-workspace → 工作区 → 目录树里的 AGENTS.md
;;   data 'instr-global：(hasheq 'path 路径 'bytes 字节数或#f)（本机的全局文件；#f = 不存在）
;;   data 'instr-entities：(listof (hasheq 'id 'kind 'name 'text))（目录 catalog 里的；不含聊天工作区）
(define (instr-preview txt)
  (define line (let ([ls (filter (lambda (l) (non-empty-string? (string-trim l))) (string-split txt "\n"))]) (if (pair? ls) (string-trim (car ls)) "")))
  (define n (bytes-length (string->bytes/utf-8 txt)))
  (t 'set.instr-preview (if (> (string-length line) 60) (string-append (substring line 0 60) "…") line) n))
(define (render-instructions s y text! button! field! cx cw)
  (define e (sv-edit s))
  (define y0 (text! y (t 'set.instr-note) 'small DIM))
  (define g (data s 'instr-global #f))
  (define y1 (+ y0 14.0))
  (define y1b (text! y1 (t 'set.instr-global) 'body TEXT (- cw 110.0)))
  (button! (+ cx cw -100.0) y1 (t 'set.instr-open) 'instr-open-global #f #:color HEAD)
  (define y2 (text! (+ y1b 2.0) (cond [(not g) "—"]
                                      [(real? (j g 'bytes)) (t 'set.instr-global-file (j g 'path "?") (j g 'bytes))]
                                      [else (t 'set.instr-global-missing (j g 'path "?"))])
                    'small DIM (- cw 110.0)))
  (define ents (let ([v (data s 'instr-entities '())]) (if (list? v) v '())))
  (define y-end
    (for/fold ([yy (+ (max y2 (+ y1 ROW)) 12.0)]) ([kind (in-list '("project" "super-workspace" "workspace"))])
      (define es (filter (lambda (x) (equal? (j x 'kind) kind)) ents))
      (cond
        [(null? es) yy]
        [else
         (define yh (text! yy (t (case kind [("project") 'set.instr-projects] [("super-workspace") 'set.instr-supers] [else 'set.instr-workspaces])) 'body HEAD))
         (for/fold ([yy (+ yh 6.0)]) ([x (in-list es)])
           (define id (j x 'id))
           (define txt (let ([v (j x 'text "")]) (if (string? v) v "")))
           (cond
             [(and e (eq? (vector-ref e 0) 'instructions) (equal? (vector-ref e 1) id)) (+ (field! yy) 8.0)]
             [else
              (define r1 (text! yy (format "~a" (j x 'name "?")) 'body TEXT (- cw 110.0)))
              (button! (+ cx cw -100.0) yy (t (if (non-empty-string? (string-trim txt)) 'set.instr-edit 'set.instr-add)) 'instr-edit id #:color HEAD)
              (define r2 (text! (+ r1 2.0) (if (non-empty-string? (string-trim txt)) (instr-preview txt) (t 'set.instr-empty)) 'small DIM (- cw 110.0)))
              (+ (max r2 (+ yy ROW)) 8.0)]))])))
  (if (null? ents) (text! y-end (t 'set.instr-no-entities) 'small DIM) y-end)
) ; end define render-instructions

;; 记忆（design-memory.md M2）
;;   data 'memory：(hasheq 'pending (listof 条目) 'groups (listof (list 作用域名 (listof 条目))) 'global-auto 布尔)
;;   条目：(hasheq 'id 'text 'meta 'pinned)；meta = 已排好的一行（来源 · 时刻 · 会话）
(define (render-memory s y text! button! field! cx cw)
  (define e (sv-edit s))
  (define d (data s 'memory (hasheq)))
  (define pending (let ([v (j d 'pending '())]) (if (list? v) v '())))
  (define groups (let ([v (j d 'groups '())]) (if (list? v) v '())))
  (define auto? (eq? (j d 'global-auto #f) #t))
  (define learn? (eq? (j d 'auto #f) #t))
  (define y0* (text! y (t 'set.mem-note) 'small DIM))
  ;; 自动记忆（M3）：关 / 开 + 状态
  (define y0
    (let* ([ya (text! (+ y0* 12.0) (t 'set.mem-auto) 'body TEXT)]
           [by (+ ya 6.0)])
      (for/fold ([bx cx]) ([v (in-list '(#f #t))])
        (define label (t (if v 'set.mem-learn-on 'set.mem-learn-off)))
        (+ bx 8.0 (button! bx by (if (eq? v learn?) (string-append "✓ " label) label) 'mem-auto v
                           #:fill (if (eq? v learn?) '(0.20 0.28 0.45) '(0.20 0.21 0.26)))))
      (text! (+ by 34.0) (format "~a" (j d 'auto-status "")) 'small DIM)))
  ;; 模型记到全局：先问 / 直接记
  (define y1 (text! (+ y0 12.0) (t 'set.mem-global-auto) 'body TEXT))
  (define y2
    (let ([by (+ y1 6.0)])
      (for/fold ([bx cx] #:result (+ by 32.0)) ([v (in-list '(#f #t))])
        (define label (t (if v 'set.mem-auto-on 'set.mem-auto-off)))
        (+ bx 8.0 (button! bx by (if (eq? v auto?) (string-append "✓ " label) label) 'mem-global-auto v
                           #:fill (if (eq? v auto?) '(0.20 0.28 0.45) '(0.20 0.21 0.26)))))))
  (define BTN-W 200.0)                                                            ; 右边按钮占的宽
  (define (entry! yy x buttons)                                                   ; 一条：文字 + meta；右边按钮（(list 文字 kind payload) …）
    (define id (j x 'id))
    (cond
      [(and e (eq? (vector-ref e 0) 'memory) (equal? (vector-ref e 1) id)) (+ (field! yy) 8.0)]
      [else
       (define r1 (text! yy (string-append (if (eq? (j x 'pinned #f) #t) "📌 " "") (format "~a" (j x 'text ""))) 'body TEXT (- cw BTN-W)))
       (define r2 (text! (+ r1 2.0) (format "~a" (j x 'meta "")) 'small DIM (- cw BTN-W)))
       (for/fold ([bx (+ cx cw (- BTN-W) 10.0)]) ([b (in-list buttons)])
         (+ bx 6.0 (button! bx yy (car b) (cadr b) (caddr b) #:color HEAD)))
       (+ (max r2 (+ yy ROW)) 8.0)]))
  (define y3
    (cond
      [(null? pending) y2]
      [else
       (define yh (text! (+ y2 4.0) (t 'set.mem-pending (length pending)) 'body WARN))
       (for/fold ([yy (+ yh 6.0)]) ([x (in-list pending)])
         (entry! yy x (list (list (t 'set.mem-accept) 'mem-accept (j x 'id)) (list (t 'set.mem-reject) 'mem-reject (j x 'id)))))]))
  (cond
    [(null? groups) (text! (+ y3 8.0) (t 'set.mem-none) 'small DIM)]
    [else
     (for/fold ([yy (+ y3 8.0)]) ([g (in-list groups)])
       (define yh (text! yy (format "~a" (car g)) 'body HEAD))
       (for/fold ([yy (+ yh 6.0)]) ([x (in-list (cadr g))])
         (entry! yy x (list (list (t (if (eq? (j x 'pinned #f) #t) 'set.mem-unpin 'set.mem-pin)) 'mem-pin (j x 'id))
                            (list (t 'set.instr-edit) 'mem-edit (j x 'id))
                            (list (t 'set.mem-delete) 'mem-delete (j x 'id))))))])
) ; end define render-memory

;; 项目索引（design-memory.md §8）
;;   data 'hix：(hasheq 'paused 'endpoint 'model 'found 'described 'pending 'errors 'current（名字或 #f）
;;                     'hosts (listof (list 主机名 (listof (hasheq 'key 'name 'dir 'summary 'short 'state 'error 'text)))))
;;   state："described" | "pending" | "error" | "edited"
(define (render-projects s y text! button! field! cx cw)
  (define e (sv-edit s))
  (define d (data s 'hix (hasheq)))
  (define (editing? kind [base #f]) (and e (eq? (vector-ref e 0) kind) (or (not base) (equal? (vector-ref e 1) base))))
  (define y0 (text! y (t 'set.hix-note) 'small DIM))
  (define y1 (text! (+ y0 12.0) (t 'set.hix-status (j d 'found 0) (j d 'described 0) (j d 'pending 0) (j d 'errors 0)) 'body TEXT))
  (define y2 (if (string? (j d 'current #f)) (text! (+ y1 2.0) (t 'set.hix-current (j d 'current)) 'small HEAD) y1))
  (define y3
    (let* ([by (+ y2 8.0)]
           [w1 (button! cx by (t (if (eq? (j d 'paused #f) #t) 'set.hix-resume 'set.hix-pause)) 'hix-pause (not (eq? (j d 'paused #f) #t)))])
      (button! (+ cx w1 8.0) by (t 'set.hix-rescan) 'hix-rescan #f)
      (+ by 34.0)))
  ;; 描述用的模型（缺省同「工具输出」的本机模型）
  (define y4
    (cond
      [(editing? 'hix-endpoint) (+ (field! y3) 8.0)]
      [(editing? 'hix-model) (+ (field! y3) 8.0)]
      [else
       (define r (text! y3 (t 'set.hix-model-line (j d 'endpoint "?") (let ([m (j d 'model #f)]) (if (string? m) m (t 'set.hix-model-auto)))) 'small DIM (- cw 230.0)))
       (define w1 (button! (+ cx cw -220.0) y3 (t 'set.hix-edit-endpoint) 'hix-endpoint-edit #f #:color HEAD))
       (button! (+ cx cw -220.0 w1 6.0) y3 (t 'set.hix-edit-model) 'hix-model-edit #f #:color HEAD)
       (+ (max r (+ y3 ROW)) 10.0)]))
  (define hosts (let ([v (j d 'hosts '())]) (if (list? v) v '())))
  (cond
    [(null? hosts) (text! (+ y4 4.0) (t 'set.hix-none) 'small DIM)]
    [else
     (for/fold ([yy (+ y4 4.0)]) ([g (in-list hosts)])
       (define yh (text! yy (format "~a" (car g)) 'body HEAD))
       (for/fold ([yy (+ yh 6.0)]) ([x (in-list (cadr g))])
         (define k (j x 'key))
         (cond
           [(editing? 'hix-desc k) (+ (field! yy) 8.0)]
           [else
            (define tag (case (j x 'state "pending") [("described") ""] [("edited") (t 'set.hix-edited)] [("error") (t 'set.hix-error (j x 'error "?"))] [else (t 'set.hix-pending)]))
            (define r1 (text! yy (string-append (format "~a" (j x 'name "?")) (if (string=? tag "") "" (string-append "  · " tag))) 'body TEXT (- cw 200.0)))
            (define r2 (text! (+ r1 1.0) (format "~a" (j x 'dir "")) 'small DIM (- cw 200.0)))
            (define r3 (if (non-empty-string? (j x 'summary "")) (text! (+ r2 1.0) (j x 'summary "") 'small DIM (- cw 200.0)) r2))
            (define r4 (if (non-empty-string? (j x 'short "")) (text! (+ r3 1.0) (j x 'short "") 'small DIM (- cw 200.0)) r3))
            (define w1 (button! (+ cx cw -190.0) yy (t 'set.instr-edit) 'hix-edit k #:color HEAD))
            (button! (+ cx cw -190.0 w1 6.0) yy (t 'set.hix-regenerate) 'hix-regenerate k #:color HEAD)
            (+ (max r4 (+ yy ROW)) 8.0)])))])
) ; end define render-projects

;; 变更记录（design-workspace.md §11.7 C4）：拼装层给的清单表（新的在前）；每条一行概要 + 摘要，「查看」→ 审阅面板（待审阅的可审，处理过的只读、可撤销）
;;   data 'changesets：(listof (hasheq 'id 'when 'by 'n 'state 'summary))
(define (render-changes s y text! button! cx cw)
  (define css (let ([v (data s 'changesets '())]) (if (list? v) v '())))
  (define y0 (text! y (t 'set.changes-note) 'small DIM))
  (cond
    [(null? css) (text! (+ y0 12.0) (t 'set.changes-none) 'body DIM)]
    [else
     (for/fold ([yy (+ y0 12.0)]) ([c (in-list css)])
       (define y1 (text! yy (t 'set.changes-row (j c 'id "?") (j c 'when "") (j c 'by "?") (j c 'n 0)
                              (t (case (j c 'state 'proposed) [(applied) 'rv.s-applied] [(undone) 'rv.s-undone] [(discarded) 'rv.s-discarded] [else 'rv.s-proposed])))
                         'body TEXT (- cw 110.0)))
       (button! (+ cx cw -100.0) yy (t 'set.changes-open) 'open-changeset (j c 'id) #:color HEAD)
       (define y2 (if (non-empty-string? (j c 'summary "")) (text! (+ y1 2.0) (j c 'summary "") 'small DIM (- cw 110.0)) y1))
       (+ (max y2 (+ yy ROW)) 10.0))])
) ; end define render-changes

;; 「工具名 模式 [行数] [keep=正则] [drop=正则]」→ (cons 名 设置) 或 #f
(define (parse-tool-rule v)
  (define ws (string-split v))
  (and (>= (length ws) 2)
       (member (cadr ws) PREVIEW-MODES)
       (let loop ([rest (cddr ws)] [h (hasheq 'mode (cadr ws))])
         (cond [(null? rest) (cons (car ws) h)]
               [(string->number (car rest)) => (lambda (n) (and (exact-positive-integer? n) (loop (cdr rest) (hash-set h 'lines n))))]
               [(regexp-match #rx"^(keep|drop)=(.+)$" (car rest))
                => (lambda (m) (and (with-handlers ([exn:fail? (lambda (_e) #f)]) (pregexp (caddr m)))
                                    (loop (cdr rest) (hash-set h (string->symbol (cadr m)) (caddr m)))))]
               [else #f])))
) ; end define parse-tool-rule

(define (render-shortcuts s y text! cx cw)
  (for/fold ([yy y]) ([row (in-list (list (cons "⌘B" 'sc.sidebar) (cons "⌘," 'sc.settings) (cons "⌘= ⌘- ⌘0" 'sc.zoom) (cons "⌘⇧K" 'sc.secret-selection) (cons "⌘⌥D" 'sc.debug)
                                           (cons "Esc" 'sc.stop) (cons "⇧Enter" 'sc.newline) (cons "⌘C" 'sc.copy)
                                           (cons (t 'sc.context-key) 'sc.context)))])
    (text! yy (car row) 'mono TEXT 140.0)
    (+ (text! yy (t (cdr row)) 'body DIM (- cw 150.0) #:x (+ cx 150.0)) 8.0))
) ; end define render-shortcuts

;; ---------------------------------------------------------------- 命中 / 编辑

(define (find-hit s x y)
  (for/first ([h (in-list (sv-hits s))]
              #:when (and (<= (vector-ref h 2) x (+ (vector-ref h 2) (vector-ref h 4)))
                          (<= (vector-ref h 3) y (+ (vector-ref h 3) (vector-ref h 5)))))
    h)
) ; end define find-hit

(define (start-edit! s kind base label step #:text [init #f])                ; init：预填的原文（改已有的）
  (set-sv-notice! s #f)
  (define ed (make-editor))
  (when (and (string? init) (not (string=? init ""))) (editor-insert! ed init))
  (set-sv-edit! s (vector kind base label step ed))
) ; end define start-edit!

(define (field-prompt kind base label step)
  (case kind
    [(key new-account)
     (if (eq? step 'label) (t 'set.field-label base) (t 'set.field-key (if (and label (not (equal? label "default"))) (format "~a[~a]" base label) base)))]
    [(alias) (if (eq? step 'label) (t 'set.field-alias-name) (t 'set.field-alias-model base))]
    [(new-pool) (t 'set.field-pool-name)]
    [(budget) (t (case base [(daily) 'set.field-budget-daily] [(combined) 'set.field-budget-combined] [else 'set.field-budget-turn]))]
    [(host) (if (eq? step 'label) (t 'set.field-host-target) (t 'set.field-host-name base))]
    [(host-workdir) (t 'set.field-host-workdir base)]
    [(secret-pattern) (t 'set.field-secret-pattern)]
    [(preview-keep preview-drop) (t 'set.field-preview-regex)]
    [(preview-model) (t 'set.field-preview-model)]
    [(preview-endpoint) (t 'set.field-preview-endpoint)]
    [(preview-tool) (t 'set.field-preview-tool)]
    [(chat-root) (t 'set.field-chat-root)]
    [(chat-max-total chat-max-chat chat-min-free) (t 'set.field-chat-gb)]
    [(member-model) (t 'set.field-member-model label)]
    [(rule) (case step [(label) (t 'set.field-rule-name)] [(model) (t 'set.field-rule-model base)] [else (t 'set.field-rule-text base)])]
    [(instructions) (t 'set.field-instructions label)]
    [(memory) (t 'set.field-memory)]
    [(hix-desc) (t 'set.field-hix-desc label)]
    [(hix-endpoint) (t 'set.field-preview-endpoint)]
    [(hix-model) (t 'set.field-hix-model)]
    [else ""])
) ; end define field-prompt

;; 项目索引的仓库（显示数据里按键找）
(define (hix-item s k)
  (define d (data s 'hix (hasheq)))
  (for*/first ([g (in-list (let ([v (j d 'hosts '())]) (if (list? v) v '())))] [x (in-list (cadr g))] #:when (equal? (j x 'key) k)) x))

;; 记忆条目（显示数据里按 id 找：待确认的与各组的）
(define (memory-item s id)
  (define d (data s 'memory (hasheq)))
  (for/first ([x (in-list (append (let ([v (j d 'pending '())]) (if (list? v) v '()))
                                  (append* (for/list ([g (in-list (let ([v (j d 'groups '())]) (if (list? v) v '())))]) (cadr g)))))]
              #:when (equal? (j x 'id) id))
    x))

;; 输入框里按回车 / 点「保存」：→ 动作或 #f
(define (submit! s)
  (define e (sv-edit s))
  (cond
    [(not e) #f]
    [else
     (define-values (kind base label step ed) (values (vector-ref e 0) (vector-ref e 1) (vector-ref e 2) (vector-ref e 3) (vector-ref e 4)))
     (define v (string-trim (editor-flatten ed)))
     (define (bad! key) (set-sv-notice! s (t key)) #f)                          ; 不合法：留在这一步，提示原因
     (define (done! action) (set-sv-edit! s #f) (set-sv-notice! s #f) action)
     (cond
       [(and (string=? v "") (eq? kind 'host) (eq? step 'value)) (done! (list 'add-host base "default"))]   ; serve 名字留空 = default
       [(eq? kind 'instructions) (done! (list 'set-instructions base v))]          ; 空 = 清掉这一层的指令
       [(eq? kind 'memory) (done! (if (string=? v "") (list 'memory-delete base) (list 'memory-edit base v)))]   ; 空 = 删掉这条
       [(eq? kind 'hix-desc) (done! (list 'hix-edit base v))]
       [(eq? kind 'hix-model) (done! (list 'hix-model (if (member v '("" "-" "auto")) #f v)))]                     ; 空 / auto = 用端点上已加载的
       [(and (eq? kind 'hix-endpoint) (string=? v "")) (done! (list 'hix-endpoint #f))]
       [(string=? v "") #f]
       [else
        (case kind
          [(key new-account)
           (cond [(not (eq? step 'label)) (done! (list 'add-key base (or label "default") v))]
                 [(regexp-match? NAME-RX v) (start-edit! s kind base v 'key) (list 'none)]
                 [else (bad! 'set.bad-name)])]
          [(alias)
           (cond [(not (eq? step 'label)) (done! (list 'set-alias base v))]
                 [(regexp-match? NAME-RX v) (start-edit! s 'alias v #f 'value) (list 'none)]
                 [else (bad! 'set.bad-name)])]
          [(new-pool)
           (cond [(regexp-match? NAME-RX v) (set-sv-edit! s #f) (set-sv-pick! s (vector v #f #t)) (list 'none)]
                 [else (bad! 'set.bad-name)])]
          [(host-workdir) (done! (list 'host-workdir base v))]
          [(secret-pattern)
           (if (with-handlers ([exn:fail? (lambda (_e) #f)]) (pregexp v)) (done! (list 'add-secret-pattern v)) (bad! 'set.bad-regex))]
          [(preview-keep preview-drop)
           (if (with-handlers ([exn:fail? (lambda (_e) #f)]) (pregexp v))
               (done! (list 'set-preview (hash-set (preview-cfg s) (if (eq? kind 'preview-keep) 'keep 'drop) v)))
               (bad! 'set.bad-regex))]
          [(preview-model) (done! (list 'set-preview (hash-set (preview-cfg s) 'model v)))]
          [(chat-root)
           (cond [(member v '("-" "default")) (done! (list 'set-chat-config (hasheq 'root 'null)))]       ; 恢复缺省
                 [(absolute-path? v) (done! (list 'set-chat-config (hasheq 'root v)))]
                 [else (bad! 'set.bad-abs-path)])]
          [(chat-max-total chat-max-chat chat-min-free)
           (define n (string->number v))
           (if (and (real? n) (> n 0))
               (done! (list 'set-chat-config (hasheq (case kind [(chat-max-total) 'max_total_gb] [(chat-max-chat) 'max_chat_gb] [else 'min_free_gb]) (exact->inexact n))))
               (bad! 'set.bad-amount))]
          [(preview-endpoint)
           (if (regexp-match? #rx"^https?://" v) (done! (list 'set-preview (hash-set (preview-cfg s) 'endpoint v))) (bad! 'set.bad-endpoint))]
          [(hix-endpoint) (if (regexp-match? #rx"^https?://" v) (done! (list 'hix-endpoint v)) (bad! 'set.bad-endpoint))]
          [(preview-tool)
           (define r (parse-tool-rule v))
           (if r
               (done! (list 'set-preview (let ([c (preview-cfg s)]) (hash-set c 'tools (hash-set (let ([ts (hash-ref c 'tools #f)]) (if (hash? ts) ts (hash))) (car r) (cdr r))))))
               (bad! 'set.bad-tool-rule))]
          [(host)
           (cond [(not (eq? step 'label))
                  (if (regexp-match? NAME-RX v) (done! (list 'add-host base v)) (bad! 'set.bad-name))]
                 [(regexp-match? #px"^[A-Za-z0-9_.@:-]{1,120}$" v) (start-edit! s 'host v #f 'value) (list 'none)]
                 [else (bad! 'set.bad-host)])]
          [(budget)
           (define n (string->number (regexp-replace #rx"^[$]" v "")))
           (if (and (real? n) (> n 0)) (done! (list 'set-budget base (exact->inexact n))) (bad! 'set.bad-amount))]
          [(member-model) (set-sv-pick! s #f) (done! (list 'define-route base (append (members-of s base) (list (cons label v)))))]
          [(rule)
           (case step
             [(label) (if (regexp-match? NAME-RX v) (begin (start-edit! s 'rule v #f 'model) (list 'none)) (bad! 'set.bad-name))]
             [(model) (if (or (equal? v "*") (with-handlers ([exn:fail? (lambda (_e) #f)]) (pregexp v)))
                          (begin (start-edit! s 'rule base v 'value) (list 'none))
                          (bad! 'set.bad-regex))]
             [else (done! (list 'save-rule base (if (equal? label "*") #f label) v))])]
          [else #f])])])
) ; end define submit!

(define (list-remove-at xs i) (append (take xs i) (drop xs (add1 i))))
(define (list-swap-up xs i) (append (take xs (sub1 i)) (list (list-ref xs i) (list-ref xs (sub1 i))) (drop xs (add1 i))))

;; → (list 'close) | (list 'section s) | (list 'lang l) | (list 'permission m) | (list 'rule 名 开?) | (list 'add-key base label token)
;;   | (list 'delete-route 名) | (list 'define-route 名 ((帐号 . 模型) …)) | (list 'set-alias 名 模型或"") | (list 'save-rule 名 模型正则或#f 文字)
;;   | (list 'delete-rule 名) | (list 'set-budget 'daily|'turn 美元或#f) | (list 'zoom z)
;;   | (list 'add-host ssh目标 serve名) | (list 'host-connect 主机) | (list 'host-disconnect 主机) | (list 'host-remove 主机)
;;   | (list 'host-credential 主机 "proxy"|"share"|"remote") | (list 'host-workdir 主机 目录)
;;   | (list 'add-secret-pattern 正则) | (list 'remove-secret-pattern 正则) | (list 'set-preview 工具输出预览配置) | (list 'none)；组件自己处理：开始编辑、取消编辑、删除确认、选成员
(define (hit s x y)
  (define h (find-hit s x y))
  (define kind (and h (vector-ref h 0)))
  (define payload (and h (vector-ref h 1)))
  (unless (memq kind '(ask-delete-route delete-route ask-delete-rule delete-rule ask-host-remove host-remove)) (set-sv-confirm! s #f))
  (case kind
    [(outside) (set-sv-open?! s #f) (set-sv-edit! s #f) (set-sv-pick! s #f) (list 'close)]
    [(section) (set-sv-section! s payload) (set-sv-scroll! s 0.0) (set-sv-edit! s #f) (set-sv-pick! s #f) (list 'section payload)]
    [(lang) (list 'lang payload)]
    [(secret-mode) (list 'secret-mode payload)]
    [(zoom) (list 'zoom payload)]
    [(stream-checkpoint) (list 'stream-checkpoint payload)]
    [(sidebar-side) (list 'sidebar-side payload)]
    [(sidebar-material) (list 'sidebar-material payload)]
    [(sidebar-older-days) (list 'sidebar-older-days payload)]
    [(auto-title) (list 'auto-title payload)]
    [(hidden-text-mode) (list 'hidden-text-mode payload)]
    [(permission) (list 'permission payload)]
    [(rule) (list 'rule (vector-ref payload 0) (not (vector-ref payload 1)))]
    [(edit-key) (let-values ([(b l) (split-instance payload)]) (start-edit! s 'key b (or l "default") 'key)) (list 'none)]
    [(new-account) (start-edit! s 'new-account payload #f 'label) (list 'none)]
    [(save) (or (submit! s) (list 'none))]
    [(cancel-edit) (set-sv-edit! s #f) (set-sv-notice! s #f) (list 'none)]
    [(ask-delete-route) (set-sv-confirm! s payload) (list 'none)]
    [(delete-route) (set-sv-confirm! s #f) (list 'delete-route payload)]
    ;; 路由池成员
    [(new-pool) (set-sv-pick! s #f) (start-edit! s 'new-pool #f #f 'label) (list 'none)]
    [(add-member) (set-sv-edit! s #f) (set-sv-pick! s (vector payload #f #f)) (list 'none)]
    [(pick-account) (set-sv-pick! s (vector (vector-ref payload 0) (vector-ref payload 1) (and (sv-pick s) (vector-ref (sv-pick s) 2)))) (list 'none)]
    [(pick-model)
     (define pool (vector-ref payload 0))
     (set-sv-pick! s #f)
     (list 'define-route pool (append (members-of s pool) (list (cons (vector-ref payload 1) (vector-ref payload 2)))))]
    [(pick-other) (set-sv-pick! s #f) (start-edit! s 'member-model (vector-ref payload 0) (vector-ref payload 1) 'value) (list 'none)]
    [(pick-cancel) (set-sv-pick! s #f) (list 'none)]
    [(member-up)
     (define pool (vector-ref payload 0))
     (list 'define-route pool (list-swap-up (members-of s pool) (vector-ref payload 1)))]
    [(member-remove)
     (define pool (vector-ref payload 0))
     (define ms (members-of s pool))
     (cond [(<= (length ms) 1) (set-sv-confirm! s pool) (list 'none)]                ; 最后一个成员 = 删池：先确认
           [else (list 'define-route pool (list-remove-at ms (vector-ref payload 1)))])]
    ;; 别名、规则
    [(budget-edit) (start-edit! s 'budget payload #f 'value) (list 'none)]
    [(add-host) (start-edit! s 'host #f #f 'label) (list 'none)]
    [(add-secret-pattern) (start-edit! s 'secret-pattern #f #f 'value) (list 'none)]
    [(remove-secret-pattern) (list 'remove-secret-pattern payload)]
    [(preview-lines) (list 'set-preview (hash-set (preview-cfg s) 'lines payload))]
    [(chat-edit) (start-edit! s payload #f #f 'value) (list 'none)]
    [(chat-cleanup) (list 'chat-cleanup)]
    [(instr-edit)                                                                   ; 指令：预填原文，多行
     (define x (for/first ([x (in-list (let ([v (data s 'instr-entities '())]) (if (list? v) v '())))] #:when (equal? (j x 'id) payload)) x))
     (when x (start-edit! s 'instructions payload (format "~a" (j x 'name "?")) 'value #:text (j x 'text "")))
     (list 'none)]
    [(instr-open-global) (list 'open-global-instructions)]
    ;; 项目索引
    [(hix-pause) (list 'hix-pause payload)]
    [(hix-rescan) (list 'hix-rescan)]
    [(hix-regenerate) (list 'hix-regenerate payload)]
    [(hix-edit) (let ([x (hix-item s payload)]) (when x (start-edit! s 'hix-desc payload (j x 'name "?") 'value #:text (j x 'text "")))) (list 'none)]
    [(hix-endpoint-edit) (start-edit! s 'hix-endpoint #f #f 'value #:text (j (data s 'hix (hasheq)) 'endpoint "")) (list 'none)]
    [(hix-model-edit) (start-edit! s 'hix-model #f #f 'value #:text (let ([m (j (data s 'hix (hasheq)) 'model #f)]) (if (string? m) m ""))) (list 'none)]
    ;; 记忆
    [(mem-global-auto) (list 'memory-global-auto payload)]
    [(mem-auto) (list 'memory-auto payload)]
    [(mem-accept) (list 'memory-accept payload)]
    [(mem-reject) (list 'memory-reject payload)]
    [(mem-delete) (list 'memory-delete payload)]
    [(mem-pin)
     (define x (memory-item s payload))
     (list 'memory-pin payload (not (and x (eq? (j x 'pinned #f) #t))))]
    [(mem-edit)
     (define x (memory-item s payload))
     (when x (start-edit! s 'memory payload #f 'value #:text (j x 'text "")))
     (list 'none)]
    [(open-changeset) (set-sv-open?! s #f) (list 'open-changeset payload)]           ; 关设置页、打开审阅面板
    [(preview-mode) (list 'set-preview (hash-set (preview-cfg s) 'mode payload))]
    [(preview-edit) (start-edit! s payload #f #f 'value) (list 'none)]
    [(preview-clear) (list 'set-preview (hash-remove (preview-cfg s) payload))]
    [(preview-tool-add) (start-edit! s 'preview-tool #f #f 'value) (list 'none)]
    [(preview-tool-remove) (list 'set-preview (let ([c (preview-cfg s)]) (hash-set c 'tools (hash-remove (let ([ts (hash-ref c 'tools #f)]) (if (hash? ts) ts (hash))) payload))))]
    [(host-connect) (list 'host-connect payload)]
    [(host-credential) (list 'host-credential (vector-ref payload 0) (vector-ref payload 1))]
    [(host-workdir-edit) (start-edit! s 'host-workdir payload #f 'value) (list 'none)]
    [(host-disconnect) (list 'host-disconnect payload)]
    [(ask-host-remove) (set-sv-confirm! s (vector 'host payload)) (list 'none)]
    [(host-remove) (set-sv-confirm! s #f) (list 'host-remove payload)]
    [(budget-clear) (list 'set-budget payload #f)]
    [(add-alias) (start-edit! s 'alias #f #f 'label) (list 'none)]
    [(alias-remove) (list 'set-alias payload "")]
    [(new-rule) (start-edit! s 'rule #f #f 'label) (list 'none)]
    [(ask-delete-rule) (set-sv-confirm! s (vector 'rule payload)) (list 'none)]
    [(delete-rule) (set-sv-confirm! s #f) (list 'delete-rule payload)]
    [else (list 'none)])
) ; end define hit

(define (hover! s x y)
  (define h (find-hit s x y))
  (define v (and h (not (memq (vector-ref h 0) '(outside panel))) (cons (vector-ref h 0) (vector-ref h 1))))
  (begin0 (not (equal? v (sv-hover s))) (set-sv-hover! s v))
) ; end define hover!

(define (hit-center s kind payload)
  (for/first ([h (in-list (sv-hits s))] #:when (and (eq? (vector-ref h 0) kind) (equal? (vector-ref h 1) payload)))
    (cons (+ (vector-ref h 2) (/ (vector-ref h 4) 2)) (+ (vector-ref h 3) (/ (vector-ref h 5) 2))))
) ; end define hit-center

(define settings-component
  (component 'settings (list text-layout) (list settings-view)
             (lambda (use config)
               (define s (sv ((use text-layout) 'rich) #f 'general (hasheq) #f #f '() #f 0.0 #f #f #f #f))
               (list (implement settings-view
                                'open? (lambda () (sv-open? s))
                                'open! (lambda () (set-sv-open?! s #t))
                                'close! (lambda () (set-sv-open?! s #f) (set-sv-edit! s #f) (set-sv-confirm! s #f) (set-sv-pick! s #f))
                                'toggle! (lambda () (set-sv-open?! s (not (sv-open? s))) (set-sv-edit! s #f))
                                'set-data! (lambda (k v) (set-sv-data! s (hash-set (sv-data s) k v)))
                                'render! (lambda (dl x y w h scale) (when (sv-open? s) (render! s dl x y w h scale)))
                                'hit (lambda (x y) (hit s x y))
                                'hover! (lambda (x y) (hover! s x y))
                                'scroll! (lambda (x y dy) (set-sv-scroll! s (- (sv-scroll s) dy)) #t)
                                'editing? (lambda () (and (sv-edit s) #t))
                                'secret? (lambda () (and (sv-edit s) (eq? (vector-ref (sv-edit s) 3) 'key)))
                                ;; 多行输入（⇧Enter 换行）：规则追加的文字、指令
                                'multiline? (lambda () (and (sv-edit s) (memq (vector-ref (sv-edit s) 0) '(rule instructions memory hix-desc)) (eq? (vector-ref (sv-edit s) 3) 'value) #t))
                                'editor (lambda () (and (sv-edit s) (vector-ref (sv-edit s) 4)))
                                'submit! (lambda () (submit! s))
                                'cancel-edit! (lambda () (set-sv-edit! s #f) (set-sv-notice! s #f))
                                'caret-rect (lambda () (sv-caret s))
                                'section (lambda () (sv-section s))
                                'hit-center (lambda (kind payload) (hit-center s kind payload)))))))

(provide settings-component)
