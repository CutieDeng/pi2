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
         "../compute/editor.rkt" "../compute/hub.rkt" "../compute/interfaces.rkt" "../compute/i18n.rkt" "../compute/keys.rkt"
         "../compute/gui-settings.rkt"
         "../compute/view.rkt" "../compute/widgets.rkt")                     ; 视图平面

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

;; ---------------------------------------------------------------- 绘制（视图平面：元素树 → view.rkt 布局、画、登记命中区）
;; 外框：压暗的整块（'outside）→ 居中的面板（'panel）：左栏分节 | 分隔线 | 右栏标题 + 可滚动的内容（区域 / 滚动 id 'body）
;; 各节的内容是元素表（下面的 *-els），用这里的几个构件拼：txt ind col btn btns choice side item-row btn-line field-el

(define BTN-FILL '(0.20 0.21 0.26))
(define SEL-FILL '(0.20 0.28 0.45))

(define (sp h) (ui-spacer #:h h))
(define (txt str font color) (para str #:font font #:color color))              ; 可折行的一段
(define (ind n el #:right [r 0.0]) (ui-box #:pad (vector 0.0 r 0.0 n) el))         ; 左缩进 n（右边留 r）
(define (col #:grow [g #f] . els) (apply ui-box #:dir 'column #:grow g els))
;; 按钮：悬停时底色 +0.05
(define (btn s label kind payload #:color [color TEXT] #:fill [fill BTN-FILL])
  (define d (if (equal? (sv-hover s) (cons kind payload)) 0.05 0.0))
  (button label (cons kind payload) #:color color #:fill (list (+ (car fill) d) (+ (cadr fill) d) (+ (caddr fill) d) 1.0)))
(define (btns . bs) (apply ui-box #:dir 'row #:wrap #t #:gap 8.0 bs))             ; 一组按钮，一行放不下就换行
(define (btn-line . bs) (apply ui-box #:dir 'row #:align 'start #:min-h ROW #:gap 8.0 bs))   ; 一行按钮，占一行高（ROW）
;; 单选的一组按钮：items = (listof (cons 值 文字))；选中的前面打 ✓、底色蓝；payload 默认就是值
(define (choice s items cur kind #:same? [same? equal?] #:payload [pf values])
  (apply btns (for/list ([it (in-list items)])
                (define on? (same? (car it) cur))
                (btn s (if on? (string-append "✓ " (cdr it)) (cdr it)) kind (pf (car it)) #:fill (if on? SEL-FILL BTN-FILL)))))
;; 一组设置：标题（正文）、单选按钮、说明（小号灰，可无）
(define (choice-group s label-key items cur kind #:note [note-key #f] #:same? [same? equal?])
  (list (txt (t label-key) 'body TEXT) (sp 8.0) (choice s items cur kind #:same? same?)
        (sp 8.0) (and note-key (txt (t note-key) 'small DIM)) (sp 14.0)))
;; 左边内容（占满剩余宽、可多行）+ 右边按钮，顶对齐；至少一行高
(define (side main . rights) (apply ui-box #:dir 'row #:align 'start #:gap 10.0 #:min-h ROW (col #:grow 1 main) rights))
;; 列表里的一项：文字 + 右边的小按钮（✕ ↑），竖直居中；至少一行高
(define (item-row main . rights) (apply ui-box #:dir 'row #:align 'center #:gap 8.0 #:min-h ROW (col #:grow 1 main) rights))

;; 正在编辑的输入框（key 显示为圆点）：提示、框（编辑器是自画的叶子，记下插入点给输入法）、保存 / 取消、不合法的提示
(define (field-el s)
  (define e (sv-edit s))
  (define-values (kind base label step ed) (values (vector-ref e 0) (vector-ref e 1) (vector-ref e 2) (vector-ref e 3) (vector-ref e 4)))
  (define rc (sv-rc s))
  (define lay (rich-ctx-layout rc))
  (define-values (disp caret _m) (editor-display ed))
  (define shown (if (eq? step 'key) (make-string (string-length disp) (integer->char #x2022)) disp))
  (define str (if (string=? shown "") " " shown))
  (col (txt (field-prompt kind base label step) 'small DIM)
       (sp 6.0)
       (ui-box #:fill '(0.30 0.40 0.62 1.0) #:radius 7.0 #:pad 1.0
               (ui-box #:fill '(0.09 0.09 0.11 1.0) #:radius 6.0 #:pad (vector 5.0 9.0 5.0 9.0)
                       (ui-custom #:measure (lambda (w) (text-block-height (lay str 'mono w #f)))
                                  #:paint (lambda (dl x y w h scale)
                                            (define tb (lay str 'mono w #f))
                                            (paint-text! dl (rich-ctx-atlas rc) tb x y TEXT #:scale scale)
                                            (define-values (k cxp) (text-block-caret tb (min caret (string-length shown))))   ; k = 第几行（多行输入）
                                            (define lh (text-block-line-height tb))
                                            (drawlist-rect! dl (+ x cxp) (+ y (* k lh)) 1.5 lh 0.55 0.70 1.0 1.0)
                                            (set-sv-caret! s (vector (+ x cxp) (+ y (* k lh)) 2.0 lh))
                                            '()))))
       (sp 8.0)
       (btns (btn s (t 'set.save) 'save #f #:fill '(0.18 0.30 0.24)) (btn s (t 'set.cancel) 'cancel-edit #f))
       (and (sv-notice s) (col (sp 6.0) (txt (sv-notice s) 'small WARN)))
       (sp 6.0)))

(define (render! s dl x y w h scale)
  (define rc (sv-rc s))
  (define lay (rich-ctx-layout rc))
  (set-sv-caret! s #f)
  (define pw (min 820.0 (- w 60.0)))
  (define ph (min 580.0 (- h 60.0)))
  ;; 分节一行的高度按可用空间收（16 节在 580 高的面板里放不下 32 的行距：最后一节压到底下的「关闭（Esc）」上——显示检查查出的）
  (define host-label (data s 'host-label))                                       ; 多主机：设置作用于哪台（标题下一行）
  (define title-h (text-block-height (lay (t 'set.title) 'title 1000.0 #f)))
  (define host-h (if host-label (+ 2.0 (text-block-height (fit-line lay (t 'set.host-of host-label) 'small (- NAV-W 24.0)))) 0.0))
  (define close-h (text-block-height (lay (t 'set.close) 'small 1000.0 #f)))
  (define step (min (+ ROW 2.0) (/ (- ph (* 2 PAD) title-h host-h 14.0 close-h 8.0) (length SECTIONS))))
  (define nav
    (ui-box #:dir 'column #:w NAV-W #:pad (vector PAD 0.0 PAD 0.0)
            (ind PAD (ui-text (t 'set.title) #:font 'title #:color TEXT))
            (and host-label (ui-box #:pad (vector 2.0 6.0 0.0 PAD) (ui-text (t 'set.host-of host-label) #:font 'small #:color HEAD)))
            (sp 14.0)
            (apply ui-box #:dir 'column #:gap 2.0
                   (for/list ([sec (in-list SECTIONS)])
                     (define cur? (eq? sec (sv-section s)))
                     (define hov? (equal? (sv-hover s) (cons 'section sec)))
                     (ui-box #:h (- step 2.0) #:pad (vector 0.0 8.0 0.0 8.0)
                             (ui-box #:grow 1 #:dir 'row #:align 'center #:pad (vector 0.0 8.0 0.0 12.0) #:radius 6.0 #:hit (cons 'section sec)
                                     #:fill (cond [cur? '(0.20 0.28 0.45 1.0)] [hov? '(0.17 0.18 0.22 1.0)] [else #f])
                                     (ui-text (section-label sec) #:font 'body #:color (if cur? TEXT DIM) #:grow 1)))))
            (ui-spacer #:grow 1)
            (ind PAD (ui-text (t 'set.close) #:font 'small #:color DIM))))
  (define tree
    (ui-box #:dir 'stack #:fill '(0.0 0.0 0.0 0.45) #:hit (cons 'outside #f) #:align 'center #:valign 'center   ; 压暗背后的窗口
            (ui-box #:dir 'row #:w pw #:h ph #:fill '(0.12 0.12 0.15 1.0) #:stroke '(0.28 0.29 0.34 1.0) #:radius 10.0 #:hit (cons 'panel #f)
                    nav
                    (ui-box #:w 1.0 #:pad (vector 12.0 0.0 12.0 0.0) (ui-box #:grow 1 #:fill '(0.22 0.23 0.28 1.0)))
                    (ui-box #:dir 'column #:grow 1 #:pad (vector PAD PAD PAD (- PAD 1.0))
                            (ui-text (section-label (sv-section s)) #:font 'title #:color TEXT)
                            (sp 12.0)
                            (apply ui-scroll 'body #:offset (sv-scroll s) (section-els s))))))
  (define-values (hits regions scrolls) (render-view! rc dl tree x y w h scale))
  (set-sv-hits! s (reverse hits))                                                 ; find-hit 取第一个 = 最上面的
  (define sc (hash-ref scrolls 'body #f))                                         ; 滚动范围：内容高度超出视区才能滚（夹好的偏移）
  (when sc (set-sv-scroll! s (vector-ref sc 0)))
  (set-sv-content-rect! s (hash-ref regions 'body #f))
) ; end define render!

(define (section-els s)
  (case (sv-section s)
    [(general) (general-els s)]
    [(accounts) (accounts-els s)]
    [(permissions) (permissions-els s)]
    [(instructions) (instructions-els s)]
    [(memory) (memory-els s)]
    [(projects) (projects-els s)]
    [(rules) (rules-els s)]
    [(pools) (pools-els s)]
    [(aliases) (aliases-els s)]
    [(budget) (budget-els s)]
    [(hosts) (hosts-els s)]
    [(tools) (tools-els s)]
    [(chat) (chat-els s)]
    [(changes) (changes-els s)]
    [(advanced) (advanced-els s)]
    [else (shortcuts-els s)]))

(define (editing-kind? s kind [base #f])
  (define e (sv-edit s))
  (and e (eq? (vector-ref e 0) kind) (or (not base) (equal? (vector-ref e 1) base))))

(define (general-els s)
  (define (labeled vs f) (for/list ([v (in-list vs)]) (cons v (f v))))
  (define pats (filter string? (data s 'secret-patterns '())))
  (append
   (choice-group s 'set.lang (list (cons 'system (t 'set.lang.system)) (cons 'zh "中文") (cons 'en "English")) (data s 'lang 'system) 'lang)
   ;; 聊天里粘贴的密钥：不识别 / 只提示 / 自动；自定义格式（secret-patterns.rktd）：列表 + ✕；「＋ 添加格式」
   (choice-group s 'set.secret-mode (labeled '(off suggest auto) (lambda (m) (t (string->symbol (format "set.secret-mode.~a" m)))))
                 (data s 'secret-mode 'suggest) 'secret-mode #:note 'set.secret-note)
   (list (txt (t 'set.secret-patterns) 'small DIM) (sp 4.0))
   (for/list ([p (in-list pats)]) (ind 12.0 (item-row (txt p 'mono TEXT) (btn s "✕" 'remove-secret-pattern p #:color DIM))))
   (list (if (editing-kind? s 'secret-pattern)
             (col (sp 4.0) (field-el s))
             (ind 12.0 (btn-line (btn s (t 'set.add-secret-pattern) 'add-secret-pattern #f #:color HEAD))))
         (sp 14.0))
   ;; 界面缩放（字号与版面一起缩放）
   (choice-group s 'set.zoom (labeled ZOOM-LEVELS (lambda (z) (format "~a%" (inexact->exact (round (* 100 z))))))
                 (data s 'zoom 1.0) 'zoom #:note 'set.zoom-note #:same? (lambda (a b) (and (real? b) (= a b))))
   ;; 流式文字的检查点（决定 1）：缺省关（完成后才落盘）
   (choice-group s 'set.checkpoint (labeled '(#f 1 5 30) (lambda (v) (if v (format "~a s" v) (t 'set.checkpoint-off))))
                 (data s 'stream-checkpoint #f) 'stream-checkpoint #:note 'set.checkpoint-note)
   ;; 侧栏位置（宽度：拖侧栏与对话区之间的分隔线）
   (choice-group s 'set.sidebar-side (labeled '(left right) (lambda (v) (t (if (eq? v 'left) 'set.sidebar-left 'set.sidebar-right))))
                 (data s 'sidebar-side 'left) 'sidebar-side #:note 'set.sidebar-note)
   ;; 侧栏材质：液态玻璃面板 / 玻璃·透出桌面（窗口透明，整列玻璃）/ 不透明（macOS 26 的 NSGlassEffectView，由系统合成）
   (choice-group s 'set.sidebar-material
                 (labeled '(glass desktop opaque) (lambda (v) (t (case v [(glass) 'set.material-glass] [(desktop) 'set.material-desktop] [else 'set.material-opaque]))))
                 (data s 'sidebar-material 'glass) 'sidebar-material #:note 'set.material-note)
   ;; 侧栏的「更早」：超过 N 天没动的会话收进去（缺省收起；§11.4 B）
   (choice-group s 'set.older-days (labeled '(7 14 30 90) (lambda (v) (t 'set.older-days-n v)))
                 (data s 'sidebar-older-days 14) 'sidebar-older-days #:note 'set.older-days-note #:same? eqv?)
   ;; 自动起标题（本机模型，同「工具输出」的设置）
   (choice-group s 'set.auto-title (labeled '(#t #f) (lambda (v) (t (if v 'set.on 'set.off))))
                 (data s 'auto-title #t) 'auto-title #:note 'set.auto-title-note #:same? eq?)
   (list (txt (t 'set.journal) 'body TEXT) (sp 4.0)
         (txt (or (data s 'journal) (t 'dbg.memory-only)) 'mono DIM) (sp 4.0)
         (txt (t 'set.journal-note) 'small DIM)))
) ; end define general-els

(define (accounts-els s)
  (define ps (filter (lambda (p) (string? (j p 'name))) (j (data s 'providers) 'providers '())))
  (define e (sv-edit s))
  (cond
    [(null? ps) (list (txt (t 'set.loading) 'body DIM))]
    [else
     (define bases (remove-duplicates (for/list ([p (in-list ps)]) (let-values ([(b _l) (split-instance (j p 'name))]) b))))
     (append*
      (for/list ([b (in-list bases)])
        (define members (filter (lambda (p) (let-values ([(b2 _l) (split-instance (j p 'name))]) (equal? b2 b))) ps))
        (define needs? (ormap (lambda (p) (eq? (j p 'needs_key) #t)) members))
        (append
         (list (sp 6.0) (txt b 'body TEXT) (sp 2.0))
         (append*
          (for/list ([p (in-list members)])
            (define-values (_b l) (split-instance (j p 'name)))
            (define key? (eq? (j p 'needs_key) #t))
            (define has? (eq? (j p 'has_key) #t))
            ;; 一行：帐号 | key 状态 | 设置 / 更换 key；正在给这个帐号设 key：输入框就在这一行下面
            (list (ui-box #:dir 'row #:align 'center #:h ROW #:pad (vector 0.0 0.0 0.0 12.0)
                          (ui-text (format "· ~a" (or l (t 'mp.default-account))) #:font 'body #:color DIM #:w 218.0)
                          (ui-text (cond [(not key?) (t 'set.key-none)] [has? (t 'set.key-set)] [else (t 'set.key-missing)])
                                   #:font 'small #:color (cond [(not key?) DIM] [has? OK] [else WARN]) #:w 140.0)
                          (and key? (btn s (if has? (t 'set.replace-key) (t 'set.set-key)) 'edit-key (j p 'name))))
                  (and e (eq? (vector-ref e 0) 'key) (equal? (vector-ref e 1) b) (equal? (vector-ref e 2) (or l "default")) (field-el s)))))
         (list (cond [(not needs?) #f]
                     [(editing-kind? s 'new-account b) (field-el s)]
                     [else (ind 12.0 (btn-line (btn s (t 'set.add-account) 'new-account b #:color HEAD)))])
               (sp 8.0)))))])
) ; end define accounts-els

(define (permissions-els s)
  (define cur (data s 'permission "normal"))
  (append
   (for/list ([m (in-list '("strict" "normal" "auto" "read-only" "yolo"))])
     (define sel? (equal? m cur))
     (ui-box #:pad (vector 0.0 0.0 12.0 0.0)
             (ui-box #:hit (cons 'permission m)                                     ; 整块（名字 + 说明）可点
                     (txt (format "~a  ~a" (if sel? "●" "○") m) 'body (if sel? TEXT DIM))
                     (sp 2.0)
                     (ind 22.0 #:right 2.0 (txt (t (string->symbol (format "set.perm.~a" m))) 'small (if (equal? m "yolo") WARN DIM))))))
   (list (sp 4.0) (txt (t 'set.perm-note) 'small DIM)))
) ; end define permissions-els

(define (rules-els s)
  (define rules (filter hash? (data s 'rules '())))
  (append
   (list (txt (t 'set.rules-note) 'small DIM) (sp 8.0)
         (if (editing-kind? s 'rule) (field-el s) (btn-line (btn s (t 'set.new-rule) 'new-rule #f #:color HEAD)))
         (sp 10.0))
   (cond
     [(null? rules) (list (txt (t 'set.loading) 'body DIM))]
     [else
      (for/list ([r (in-list rules)])
        (define on? (eq? (j r 'enabled) #t))
        (define m (j r 'match (hasheq)))
        (define match-str (string-join (filter values (list (and (string? (j m 'model)) (format "model ~~ ~a" (j m 'model)))
                                                            (and (string? (j m 'provider)) (format "provider ~~ ~a" (j m 'provider)))
                                                            (and (list? (j m 'reasoning)) (format "reasoning ∈ ~a" (string-join (j m 'reasoning) "/")))))
                                       " · "))
        (define confirming? (equal? (sv-confirm s) (vector 'rule (j r 'name))))
        ;; 开关 | 名字 · 来源（设置页建的：可删，两次点击）/ 说明 / 匹配条件 / 追加的文字
        (ui-box #:dir 'row #:align 'start #:pad (vector 0.0 0.0 14.0 0.0)
                (ui-box #:dir 'row #:w 70.0 #:align 'start
                        (btn s (if on? (t 'set.rule-on) (t 'set.rule-off)) 'rule (vector (j r 'name) on?)
                             #:fill (if on? '(0.18 0.30 0.24) BTN-FILL) #:color (if on? TEXT DIM)))
                (col #:grow 1
                     (side (col (sp 2.0) (txt (format "~a  · ~a" (j r 'name "?") (j r 'source "?")) 'body TEXT))
                           (and (equal? (j r 'source) "settings")
                                (btn s (if confirming? (t 'set.confirm-delete-pool) (t 'set.delete-pool))
                                     (if confirming? 'delete-rule 'ask-delete-rule) (j r 'name) #:color (if confirming? WARN DIM))))
                     (txt (j r 'description "") 'small DIM)
                     (sp 2.0)
                     (txt (t 'set.rule-match (if (string=? match-str "") "*" match-str)) 'small DIM)
                     (and (string? (j r 'system_append)) (col (sp 2.0) (txt (t 'set.rule-append (j r 'system_append)) 'small DIM))))))]))
) ; end define rules-els

(define (members-of s pool)
  (define r (findf (lambda (r) (equal? (j r 'name) pool)) (filter hash? (j (data s 'providers) 'routes '()))))
  (if r (for/list ([m (in-list (j r 'members '()))]) (cons (j m 'instance "?") (j m 'model "?"))) '())
) ; end define members-of

(define (pools-els s)
  (define routes (filter hash? (j (data s 'providers) 'routes '())))
  (define accounts (filter (lambda (p) (string? (j p 'name))) (j (data s 'providers) 'providers '())))
  (define e (sv-edit s))
  (define pk (sv-pick s))
  ;; 添加成员：先选帐号，再选模型（菜单里的模型列表 + 「其他…」手输）；items = (list 文字 kind payload 颜色)
  (define (pick-block pool)
    (define acct (vector-ref pk 1))
    (define (choices items) (apply btns (for/list ([it (in-list items)]) (btn s (car it) (cadr it) (caddr it) #:color (cadddr it)))))
    (cond
      [(not acct)
       (col (txt (t 'set.pick-account) 'small DIM) (sp 6.0)
            (choices (append (for/list ([p (in-list accounts)]) (list (j p 'name) 'pick-account (vector pool (j p 'name)) TEXT))
                             (list (list (t 'set.cancel) 'pick-cancel #f DIM))))
            (sp 8.0))]
      [else
       (define p (findf (lambda (p) (equal? (j p 'name) acct)) accounts))
       (col (txt (t 'set.pick-model acct) 'small DIM) (sp 6.0)
            (choices (append (for/list ([m (in-list (if p (filter string? (j p 'models '())) '()))]) (list m 'pick-model (vector pool acct m) TEXT))
                             (list (list (t 'set.pick-other) 'pick-other (vector pool acct) HEAD)
                                   (list (t 'set.cancel) 'pick-cancel #f DIM))))
            (sp 8.0))]))
  ;; 新建池：池名 → 选第一个成员（池在第一个成员加进去时才建出来）
  (define new-name (cond [(and pk (vector-ref pk 2)) (vector-ref pk 0)]
                         [(and (editing-kind? s 'member-model) (not (findf (lambda (r) (equal? (j r 'name) (vector-ref e 1))) routes))) (vector-ref e 1)]
                         [else #f]))
  (append
   (list (txt (t 'set.pools-note) 'small DIM) (sp 10.0))
   (cond
     [(null? routes) (list (txt (t 'set.pools-none) 'body DIM))]
     [else
      (for/list ([r (in-list routes)])
        (define name (j r 'name "?"))
        (define confirming? (equal? (sv-confirm s) name))
        (apply col
               (append
                (list (side (txt (format "@~a" name) 'body TEXT)
                            (btn s (if confirming? (t 'set.confirm-delete-pool) (t 'set.delete-pool))
                                 (if confirming? 'delete-route 'ask-delete-route) name #:color (if confirming? WARN DIM))))
                (for/list ([m (in-list (j r 'members '()))] [i (in-naturals)])            ; 成员：▸ 当前；↑ 上移；✕ 移除
                  (define state (j m 'state "ok"))
                  (ind 12.0 (item-row (txt (format "~a ~a · ~a~a" (if (eqv? i (j r 'current)) "▸" " ") (j m 'instance "?") (j m 'model "?")
                                                   (if (equal? state "ok") "" (format "  [~a]" state)))
                                           'mono (if (equal? state "ok") DIM WARN))
                                      (and (> i 0) (btn s "↑" 'member-up (vector name i) #:color DIM))
                                      (btn s "✕" 'member-remove (vector name i) #:color DIM))))
                (list (cond [(and pk (equal? (vector-ref pk 0) name)) (col (sp 2.0) (pick-block name))]
                            [(editing-kind? s 'member-model name) (col (sp 2.0) (field-el s))]
                            [else (ind 12.0 (btn-line (btn s (t 'set.add-member) 'add-member name #:color HEAD)))])
                      (sp 12.0)))))])
   (list (sp 6.0)
         (cond [(editing-kind? s 'new-pool) (field-el s)]
               [new-name (col (txt (format "@~a  ~a" new-name (t 'set.pool-new)) 'body TEXT) (sp 4.0) (if pk (pick-block new-name) (field-el s)))]
               [else (btn-line (btn s (t 'set.new-pool) 'new-pool #f #:color HEAD))])))
) ; end define pools-els

(define (hosts-els s)
  (define hosts (filter hash? (data s 'hosts '())))
  (append
   (list (txt (t 'set.hosts-note) 'small DIM) (sp 12.0))
   (for/list ([h (in-list hosts)])
     (define id (j h 'id "?"))
     (define status (j h 'status "connecting"))
     (define confirming? (equal? (sv-confirm s) (vector 'host id)))
     (col
      ;; 名字（● = 当前）| 连接状态；细节（ssh 目标等）
      (ui-box #:dir 'row #:align 'start #:gap 10.0
              (col #:grow 1 (txt (format "~a~a" (j h 'label id) (if (eq? (j h 'active) #t) "  ●" "")) 'body TEXT))
              (ui-text (t (string->symbol (format "set.host-status.~a" status))) #:font 'small #:color (if (equal? status "disconnected") WARN DIM) #:w 120.0)
              (ui-spacer #:w 90.0))
      (sp 2.0)
      (ind 12.0 #:right 8.0 (txt (j h 'detail "") 'mono DIM))
      (sp 6.0)
      ;; 按钮：连接 / 断开；可移除的（非本机）两次点击移除
      (btns (if (equal? status "disconnected")
                (btn s (t 'set.host-connect) 'host-connect id #:color HEAD)
                (btn s (t 'set.host-disconnect) 'host-disconnect id #:color DIM))
            (and (eq? (j h 'removable) #t)
                 (btn s (if confirming? (t 'set.confirm-delete-pool) (t 'set.host-remove))
                      (if confirming? 'host-remove 'ask-host-remove) id #:color (if confirming? WARN DIM))))
      (sp 10.0)
      ;; 服务器：跨主机子 agent 的凭证与工作目录
      (cond
        [(eq? (j h 'subagent) #t)
         (define cred (j h 'credential "proxy"))
         (col (txt (t 'set.host-sub-cred) 'small DIM) (sp 4.0)
              (ind 12.0 (choice s (for/list ([m (in-list '("proxy" "share" "remote"))]) (cons m (t (string->symbol (format "set.host-cred.~a" m)))))
                                cred 'host-credential #:payload (lambda (m) (vector id m))))
              (sp 6.0)
              (ind 12.0 #:right 12.0 (txt (t (string->symbol (format "set.host-cred-note.~a" cred))) 'small (if (equal? cred "share") WARN DIM)))
              (sp 6.0)
              (if (editing-kind? s 'host-workdir id)
                  (col (field-el s) (sp 8.0))
                  (col (side (ind 12.0 (txt (t 'set.host-workdir (or (j h 'workdir) (t 'set.host-workdir-home))) 'small DIM))
                             (btn s (t 'set.budget-change) 'host-workdir-edit id #:color HEAD))
                       (sp 6.0))))]
        [else (sp 4.0)])))
   (list (sp 4.0)
         (if (editing-kind? s 'host) (field-el s) (btn-line (btn s (t 'set.add-host) 'add-host #f #:color HEAD)))))
) ; end define hosts-els

(define (money x) (if (real? x) (format "$~a" (/ (round (* x 10000)) 10000.0)) "—"))

(define (budget-els s)
  (define b (data s 'budget (hasheq)))
  (define all (data s 'budget-all #f))
  ;; 上限一项：名字、值（没设：—）、设置 / 更改 + 取消；正在改：输入框
  (define (cap-els kind label-el v)
    (define cap? (real? v))
    (list label-el (sp 2.0)
          (ind 12.0 (txt (if cap? (money v) (t 'set.budget-none)) 'mono (if cap? TEXT DIM)))
          (sp 6.0)
          (if (editing-kind? s 'budget kind)
              (field-el s)
              (btn-line (btn s (if cap? (t 'set.budget-change) (t 'set.budget-set)) 'budget-edit kind #:color HEAD)
                        (and cap? (btn s (t 'set.budget-clear) 'budget-clear kind #:color DIM))))))
  (append
   (list (txt (t 'set.budget-note) 'small DIM))
   ;; 多主机：所有主机合计 + 分台 + 合计每天上限（GUI 存）
   (cond
     [(hash? all)
      (append (list (sp 12.0) (txt (t 'set.budget-all (money (j all 'total 0))) 'body TEXT) (sp 2.0))
              (for/list ([h (in-list (filter hash? (j all 'hosts '())))])
                (ind 12.0 (txt (t 'set.budget-all-host (j h 'label "?") (money (j h 'spent 0)) (let ([c (j h 'cap)]) (if (real? c) (format " / ~a" (money c)) "")))
                               'mono DIM)))
              (list (sp 10.0))
              (cap-els 'combined (col (txt (t 'set.budget-combined) 'body TEXT)
                                      (and (eq? (j all 'snoozed) #t) (txt (t 'set.budget-combined-snoozed) 'small WARN)))
                       (j all 'cap))
              (list (sp 10.0)))]
     [else '()])
   (list (sp 12.0) (txt (t 'set.budget-today (j b 'date "?") (money (j b 'spent_today 0))) 'body TEXT) (sp 2.0))
   (for/list ([m (in-list (filter hash? (j b 'by_model '())))])
     (ind 12.0 (txt (format "· ~a  ~a" (j m 'model "?") (money (j m 'usd 0))) 'mono DIM)))
   (list (and (eq? (j b 'snoozed) #t) (col (sp 4.0) (txt (t 'set.budget-snoozed) 'small WARN)))
         (sp 16.0))
   (cap-els 'daily (txt (t 'set.budget-daily) 'body TEXT) (j b 'daily_usd))
   (list (sp 10.0))
   (cap-els 'turn (txt (t 'set.budget-turn) 'body TEXT) (j b 'turn_usd)))
) ; end define budget-els

(define (aliases-els s)
  (define als (filter hash? (data s 'aliases '())))
  (append
   (list (txt (t 'set.aliases-note) 'small DIM) (sp 10.0))
   (if (null? als)
       (list (txt (t 'set.aliases-none) 'body DIM))
       (for/list ([a (in-list als)])                                                ; 别名 | 模型 id | ✕
         (ui-box #:dir 'row #:align 'center #:min-h ROW #:gap 8.0
                 (ui-text (j a 'name "?") #:font 'mono #:color TEXT #:w 152.0)
                 (col #:grow 1 (txt (j a 'model "?") 'mono DIM))
                 (btn s "✕" 'alias-remove (j a 'name) #:color DIM))))
   (list (sp 8.0)
         (if (editing-kind? s 'alias) (field-el s) (btn-line (btn s (t 'set.add-alias) 'add-alias #f #:color HEAD)))))
) ; end define aliases-els

;; 工具输出的折叠预览（transcript.rkt preview-spec 的配置；gui.rktd 'tool-preview）
(define PREVIEW-LINES '(3 5 8 12 20))
(define PREVIEW-MODES '("head" "tail" "head_tail" "match" "smart"))
(define (preview-cfg s) (let ([c (data s 'tool-preview #f)]) (if (hash? c) c (hasheq 'lines 5 'mode "head"))))

(define (tools-els s)
  (define c (preview-cfg s))
  ;; 正则：keep（match 模式优先）/ drop（不显示）；本地模型（smart）：名字 + 值 | 修改 ✕
  (define (value-row label-key k field-kind)
    (cond
      [(editing-kind? s field-kind) (col (txt (t label-key) 'small DIM) (sp 4.0) (field-el s))]
      [else
       (define v (hash-ref c k #f))
       (side (col (txt (t label-key) 'small DIM) (sp 4.0)
                  (ind 12.0 (txt (if (and (string? v) (not (string=? v ""))) v (t 'set.preview-unset)) 'mono (if (string? v) TEXT DIM)))
                  (sp 6.0))
             (btn s (t 'set.preview-edit) 'preview-edit field-kind #:color HEAD)
             (and (string? v) (btn s "✕" 'preview-clear k #:color DIM)))]))
  (define tools (let ([ts (hash-ref c 'tools #f)]) (if (hash? ts) ts (hash))))
  (append
   (list (txt (t 'set.preview-note) 'small DIM) (sp 12.0)
         (txt (t 'set.preview-lines) 'body TEXT) (sp 8.0)
         (choice s (for/list ([n PREVIEW-LINES]) (cons n (number->string n))) (hash-ref c 'lines 5) 'preview-lines)
         (sp 14.0)
         (txt (t 'set.preview-mode) 'body TEXT) (sp 8.0)
         (choice s (for/list ([m PREVIEW-MODES]) (cons m (t (string->symbol (format "set.preview-mode.~a" m))))) (hash-ref c 'mode "head") 'preview-mode)
         (sp 12.0)
         (value-row 'set.preview-keep 'keep 'preview-keep) (sp 4.0)
         (value-row 'set.preview-drop 'drop 'preview-drop) (sp 4.0)
         (value-row 'set.preview-model 'model 'preview-model) (sp 4.0)
         (value-row 'set.preview-endpoint 'endpoint 'preview-endpoint)
         (txt (t 'set.preview-smart-note) 'small DIM)
         ;; 按工具覆盖：「工具  模式 行数」+ ✕；＋ 添加
         (sp 14.0) (txt (t 'set.preview-tools) 'body TEXT) (sp 8.0))
   (for/list ([name (in-list (sort (map (lambda (k) (format "~a" k)) (hash-keys tools)) string<?))])
     (define v (or (hash-ref tools name #f) (hash-ref tools (string->symbol name) (hasheq))))
     (ind 12.0 (ui-box #:dir 'row #:align 'center #:min-h ROW #:gap 8.0
                       (ui-text name #:font 'mono #:color TEXT #:w 150.0)
                       (col #:grow 1 (txt (string-join (filter values (list (let ([m (j v 'mode #f)]) (and m (t (string->symbol (format "set.preview-mode.~a" m)))))
                                                                            (let ([n (j v 'lines #f)]) (and n (t 'set.preview-n-lines n)))
                                                                            (let ([k (j v 'keep #f)]) (and k (format "keep /~a/" k)))
                                                                            (let ([k (j v 'drop #f)]) (and k (format "drop /~a/" k)))))
                                                       " · ")
                                          'small DIM))
                       (btn s "✕" 'preview-tool-remove name #:color DIM))))
   (list (if (editing-kind? s 'preview-tool)
             (col (sp 4.0) (field-el s))
             (ind 12.0 (col (sp 2.0) (btn-line (btn s (t 'set.preview-add-tool) 'preview-tool-add #f #:color HEAD)))))))
) ; end define tools-els

;; 聊天目录（匿名工作区，design-workspace.md §10）：当前主机的根目录、三个上限、占用；改了发 set_chat_config，「立即清理」发 chat_cleanup
(define (chat-els s)
  (define u (data s 'chat-usage #f))                                 ; 内核的 chat_usage 事件
  (define c (if (hash? u) (j u 'config (hasheq)) (hasheq)))
  (define (row label-key value field-kind)                           ; 长路径会折行：按实际高度往下排
    (if (editing-kind? s field-kind)
        (col (txt (t label-key) 'small DIM) (sp 4.0) (field-el s))
        (side (col (txt (t label-key) 'small DIM) (sp 4.0) (ind 12.0 (txt value 'mono TEXT)) (sp 6.0))
              (btn s (t 'set.preview-edit) 'chat-edit field-kind #:color HEAD))))
  (define (gbs v) (if (real? v) (format "~a GB" v) "—"))
  (define (size n) (cond [(not (real? n)) "?"] [(< n 1048576) (format "~a KB" (round (/ n 1024)))]
                         [(< n 1073741824) (format "~a MB" (/ (round (/ n 104857.6)) 10.0))] [else (format "~a GB" (/ (round (/ n 107374182.4)) 10.0))]))
  (list (txt (t 'set.chat-note) 'small DIM) (sp 12.0)
        (row 'set.chat-root (let ([r (and (hash? u) (j u 'root #f))]) (if (string? r) r "—")) 'chat-root) (sp 4.0)
        (row 'set.chat-max-total (gbs (j c 'max_total_gb #f)) 'chat-max-total) (sp 4.0)
        (row 'set.chat-max-chat (gbs (j c 'max_chat_gb #f)) 'chat-max-chat) (sp 4.0)
        (row 'set.chat-min-free (gbs (j c 'min_free_gb #f)) 'chat-min-free)
        (sp 14.0)
        (txt (if (hash? u)
                 (t 'set.chat-usage (j u 'count 0) (size (j u 'total 0)) (size (j u 'free #f)) (if (j u 'complete #t) "" (t 'set.chat-partial)))
                 (t 'set.chat-loading))
             'body TEXT)
        (sp 8.0)
        (btn-line (btn s (t 'set.chat-cleanup) 'chat-cleanup #f #:color HEAD)))
) ; end define chat-els

;; 高级：送给模型的隐藏文字（Unicode tag 字符，design-gui.md §30.4）——标出 / 原样
(define (advanced-els s)
  (choice-group s 'set.adv-hidden (list (cons 'mark (t 'set.adv-hidden-mark)) (cons 'raw (t 'set.adv-hidden-raw)))
                (data s 'hidden-text-mode 'mark) 'hidden-text-mode #:note 'set.adv-hidden-note))

;; 指令（design-memory.md M1）：每轮注入系统提示词，从一般到具体（后者优先）：全局文件 → 项目 → super-workspace → 工作区 → 目录树里的 AGENTS.md
;;   data 'instr-global：(hasheq 'path 路径 'bytes 字节数或#f)（本机的全局文件；#f = 不存在）
;;   data 'instr-entities：(listof (hasheq 'id 'kind 'name 'text))（目录 catalog 里的；不含聊天工作区）
(define (instr-preview txt)
  (define line (let ([ls (filter (lambda (l) (non-empty-string? (string-trim l))) (string-split txt "\n"))]) (if (pair? ls) (string-trim (car ls)) "")))
  (define n (bytes-length (string->bytes/utf-8 txt)))
  (t 'set.instr-preview (if (> (string-length line) 60) (string-append (substring line 0 60) "…") line) n))
(define (instructions-els s)
  (define g (data s 'instr-global #f))
  (define ents (let ([v (data s 'instr-entities '())]) (if (list? v) v '())))
  (append
   (list (txt (t 'set.instr-note) 'small DIM) (sp 14.0)
         (side (col (txt (t 'set.instr-global) 'body TEXT) (sp 2.0)
                    (txt (cond [(not g) "—"]
                               [(real? (j g 'bytes)) (t 'set.instr-global-file (j g 'path "?") (j g 'bytes))]
                               [else (t 'set.instr-global-missing (j g 'path "?"))])
                         'small DIM))
               (btn s (t 'set.instr-open) 'instr-open-global #f #:color HEAD))
         (sp 12.0))
   (append*
    (for/list ([kind (in-list '("project" "super-workspace" "workspace"))])
      (define es (filter (lambda (x) (equal? (j x 'kind) kind)) ents))
      (if (null? es)
          '()
          (cons (col (txt (t (case kind [("project") 'set.instr-projects] [("super-workspace") 'set.instr-supers] [else 'set.instr-workspaces])) 'body HEAD) (sp 6.0))
                (for/list ([x (in-list es)])
                  (define id (j x 'id))
                  (define body (let ([v (j x 'text "")]) (if (string? v) v "")))
                  (define has? (non-empty-string? (string-trim body)))
                  (col (if (editing-kind? s 'instructions id)
                           (field-el s)
                           (side (col (txt (format "~a" (j x 'name "?")) 'body TEXT) (sp 2.0)
                                      (txt (if has? (instr-preview body) (t 'set.instr-empty)) 'small DIM))
                                 (btn s (t (if has? 'set.instr-edit 'set.instr-add)) 'instr-edit id #:color HEAD)))
                       (sp 8.0)))))))
   (list (and (null? ents) (txt (t 'set.instr-no-entities) 'small DIM))))
) ; end define instructions-els

;; 记忆（design-memory.md M2）
;;   data 'memory：(hasheq 'pending (listof 条目) 'groups (listof (list 作用域名 (listof 条目))) 'global-auto 布尔)
;;   条目：(hasheq 'id 'text 'meta 'pinned)；meta = 已排好的一行（来源 · 时刻 · 会话）
(define (memory-els s)
  (define d (data s 'memory (hasheq)))
  (define pending (let ([v (j d 'pending '())]) (if (list? v) v '())))
  (define groups (let ([v (j d 'groups '())]) (if (list? v) v '())))
  (define auto? (eq? (j d 'global-auto #f) #t))
  (define learn? (eq? (j d 'auto #f) #t))
  (define (entry x buttons)                                                         ; 一条：文字 + meta | 按钮（(list 文字 kind payload) …）
    (define id (j x 'id))
    (col (if (editing-kind? s 'memory id)
             (field-el s)
             (side (col (txt (string-append (if (eq? (j x 'pinned #f) #t) "📌 " "") (format "~a" (j x 'text ""))) 'body TEXT) (sp 2.0)
                        (txt (format "~a" (j x 'meta "")) 'small DIM))
                   (apply ui-box #:dir 'row #:align 'start #:gap 6.0
                          (for/list ([b (in-list buttons)]) (btn s (car b) (cadr b) (caddr b) #:color HEAD)))))
         (sp 8.0)))
  (append
   (list (txt (t 'set.mem-note) 'small DIM) (sp 12.0)
         ;; 自动记忆（M3）：关 / 开 + 状态
         (txt (t 'set.mem-auto) 'body TEXT) (sp 6.0)
         (choice s (list (cons #f (t 'set.mem-learn-off)) (cons #t (t 'set.mem-learn-on))) learn? 'mem-auto #:same? eq?)
         (sp 10.0)
         (txt (format "~a" (j d 'auto-status "")) 'small DIM)
         ;; 模型记到全局：先问 / 直接记
         (sp 12.0) (txt (t 'set.mem-global-auto) 'body TEXT) (sp 6.0)
         (choice s (list (cons #f (t 'set.mem-auto-off)) (cons #t (t 'set.mem-auto-on))) auto? 'mem-global-auto #:same? eq?)
         (sp 8.0))
   (if (null? pending)
       '()
       (cons (col (sp 4.0) (txt (t 'set.mem-pending (length pending)) 'body WARN) (sp 6.0))
             (for/list ([x (in-list pending)])
               (entry x (list (list (t 'set.mem-accept) 'mem-accept (j x 'id)) (list (t 'set.mem-reject) 'mem-reject (j x 'id)))))))
   (list (sp 8.0))
   (if (null? groups)
       (list (txt (t 'set.mem-none) 'small DIM))
       (append*
        (for/list ([g (in-list groups)])
          (cons (col (txt (format "~a" (car g)) 'body HEAD) (sp 6.0))
                (for/list ([x (in-list (cadr g))])
                  (entry x (list (list (t (if (eq? (j x 'pinned #f) #t) 'set.mem-unpin 'set.mem-pin)) 'mem-pin (j x 'id))
                                 (list (t 'set.instr-edit) 'mem-edit (j x 'id))
                                 (list (t 'set.mem-delete) 'mem-delete (j x 'id))))))))))
) ; end define memory-els

;; 项目索引（design-memory.md §8）
;;   data 'hix：(hasheq 'paused 'endpoint 'model 'found 'described 'pending 'errors 'current（名字或 #f）
;;                     'hosts (listof (list 主机名 (listof (hasheq 'key 'name 'dir 'summary 'short 'state 'error 'text)))))
;;   state："described" | "pending" | "error" | "edited"
(define (projects-els s)
  (define d (data s 'hix (hasheq)))
  (define paused? (eq? (j d 'paused #f) #t))
  (define hosts (let ([v (j d 'hosts '())]) (if (list? v) v '())))
  (define (small-opt str) (and (non-empty-string? str) (col (sp 1.0) (txt str 'small DIM))))
  (append
   (list (txt (t 'set.hix-note) 'small DIM) (sp 12.0)
         (txt (t 'set.hix-status (j d 'found 0) (j d 'described 0) (j d 'pending 0) (j d 'errors 0)) 'body TEXT)
         (and (string? (j d 'current #f)) (col (sp 2.0) (txt (t 'set.hix-current (j d 'current)) 'small HEAD)))
         (sp 8.0)
         (btns (btn s (t (if paused? 'set.hix-resume 'set.hix-pause)) 'hix-pause (not paused?))
               (btn s (t 'set.hix-rescan) 'hix-rescan #f))
         (sp 10.0)
         ;; 描述用的模型（缺省同「工具输出」的本机模型）
         (if (or (editing-kind? s 'hix-endpoint) (editing-kind? s 'hix-model))
             (field-el s)
             (side (txt (t 'set.hix-model-line (j d 'endpoint "?") (let ([m (j d 'model #f)]) (if (string? m) m (t 'set.hix-model-auto)))) 'small DIM)
                   (btn s (t 'set.hix-edit-endpoint) 'hix-endpoint-edit #f #:color HEAD)
                   (btn s (t 'set.hix-edit-model) 'hix-model-edit #f #:color HEAD)))
         (sp 12.0))
   (if (null? hosts)
       (list (txt (t 'set.hix-none) 'small DIM))
       (append*
        (for/list ([g (in-list hosts)])
          (cons (col (txt (format "~a" (car g)) 'body HEAD) (sp 6.0))
                (for/list ([x (in-list (cadr g))])
                  (define k (j x 'key))
                  (define tag (case (j x 'state "pending") [("described") ""] [("edited") (t 'set.hix-edited)] [("error") (t 'set.hix-error (j x 'error "?"))] [else (t 'set.hix-pending)]))
                  (col (if (editing-kind? s 'hix-desc k)
                           (field-el s)
                           (side (col (txt (string-append (format "~a" (j x 'name "?")) (if (string=? tag "") "" (string-append "  · " tag))) 'body TEXT)
                                      (col (sp 1.0) (txt (format "~a" (j x 'dir "")) 'small DIM))
                                      (small-opt (j x 'summary ""))
                                      (small-opt (j x 'short "")))
                                 (btn s (t 'set.instr-edit) 'hix-edit k #:color HEAD)
                                 (btn s (t 'set.hix-regenerate) 'hix-regenerate k #:color HEAD)))
                       (sp 8.0))))))))
) ; end define projects-els

;; 变更记录（design-workspace.md §11.7 C4）：拼装层给的清单表（新的在前）；每条一行概要 + 摘要，「查看」→ 审阅面板（待审阅的可审，处理过的只读、可撤销）
;;   data 'changesets：(listof (hasheq 'id 'when 'by 'n 'state 'summary))
(define (changes-els s)
  (define css (let ([v (data s 'changesets '())]) (if (list? v) v '())))
  (append
   (list (txt (t 'set.changes-note) 'small DIM) (sp 12.0))
   (if (null? css)
       (list (txt (t 'set.changes-none) 'body DIM))
       (for/list ([c (in-list css)])
         (col (side (col (txt (t 'set.changes-row (j c 'id "?") (j c 'when "") (j c 'by "?") (j c 'n 0)
                                 (t (case (j c 'state 'proposed) [(applied) 'rv.s-applied] [(undone) 'rv.s-undone] [(discarded) 'rv.s-discarded] [else 'rv.s-proposed])))
                              'body TEXT)
                         (and (non-empty-string? (j c 'summary "")) (col (sp 2.0) (txt (j c 'summary "") 'small DIM))))
                    (btn s (t 'set.changes-open) 'open-changeset (j c 'id) #:color HEAD))
              (sp 10.0)))))
) ; end define changes-els

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

(define (shortcuts-els s)
  ;; 组合键的写法由键位表 + 平台生成（compute/keymap.rkt、keys.rkt），不写死 ⌘
  (for/list ([row (in-list (list (cons (key-hint 'toggle-sidebar) 'sc.sidebar) (cons (key-hint 'settings) 'sc.settings)
                                 (cons (string-append (key-hint 'zoom-in) " " (key-hint 'zoom-out) " " (key-hint 'zoom-reset)) 'sc.zoom)
                                 (cons (key-hint 'mark-secret) 'sc.secret-selection) (cons (key-hint 'debug-panel) 'sc.debug)
                                 (cons (chord-label '(escape)) 'sc.stop) (cons (chord-label '(shift return)) 'sc.newline) (cons (chord-label '(primary c)) 'sc.copy)
                                 (cons (t 'sc.context-key) 'sc.context)))])
    (ui-box #:dir 'row #:align 'start #:pad (vector 0.0 0.0 8.0 0.0)
            (ui-box #:w 150.0 #:pad (vector 0.0 10.0 0.0 0.0) (txt (car row) 'mono TEXT))
            (col #:grow 1 (txt (t (cdr row)) 'body DIM))))
) ; end define shortcuts-els

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
