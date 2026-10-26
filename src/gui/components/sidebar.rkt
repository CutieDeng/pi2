#lang racket/base
;; sidebar.rkt — 会话侧栏组件（G5）：会话列表（内核 list_sessions 的结果）、当前会话高亮、新建 / 分叉。
;; 右键（或 ctrl-点击）行 → 上下文菜单（design-workspace.md W1）：打开、重命名…、信息…、分叉、复制路径、在 Finder 中显示（本机）、
;;   导出 Markdown / JSON（复制）、删除…。删除不放在行上（太容易误点）：「删除会话…」→ 变成「确认删除（不可恢复）」→ 再点才删。
;;   重命名：行上换成输入框（本组件的编辑器；拼装层把键盘事件交给它），回车 → (list 'rename 键 新名)，Esc 取消。
;; 只保存显示状态与命中区域；发什么请求由拼装层决定（命中结果是动作，不直接调用内核）。
;; 临时会话的行慢慢明暗起伏（周期 PULSE-MS）：提示「没执行内容就会被移除」。画到它时 animating? 为真，拼装层据此持续重绘（只在这时）。
;; 头部：新建（当前工作区）、聊天（匿名工作区：应用管理的目录）、分叉。聊天会话的行标题前有 💬。
;; 临时会话（'ephemeral #t：本次新建、还没发消息，切走就被内核删掉）：虚线框 + 灰字 + 说明「未发消息 · 切走后不保留」。
;; 当前会话没有消息时「分叉」变灰、不可点（内核也拒绝：没有可分叉的内容）。
;; 一窗多主机：列表里可以有主机标题行（'kind "header"：'host 'label 'status 'level 'connected）——右边「＋」在该主机上新建会话，
;;   断开的主机整行可点「重新连接」；会话行用 'key 作标识（缺省 'path），'badge / 'badge_level 显示运行中 / 等待权限。
;; 分组行（'kind "group"：'key 'label 'count 'open，W2 的「已归档」，compute/session-rows.rkt 排出来）：▸ / ▾ + 文字，点 → (list 'toggle-group 键)。
;; 会话行 'pinned → 标题前 📌；菜单里置顶 / 取消置顶、归档 / 取消归档（属性存在界面的目录 catalog，拼装层处理）。
;; 少操作（design-workspace.md §11 A）：菜单第一行是四个图标格（置顶、归档、改名、信息，带单键提示），下面是文字项（带快捷键）；
;;   删除一次点击（进回收站，可撤销，不再确认）；悬停的行右边出两个图标（置顶、归档）；底部提示条「已删除 …  撤销」；
;;   键盘焦点行（拼装层管侧栏有没有焦点、按键）画一圈描边。
;; needs text-layout → provides session-list

(require racket/list racket/string
         "../compute/drawlist.rkt" "../compute/palette.rkt" "../compute/layout.rkt" "../compute/paint.rkt" "../compute/rich.rkt" "../compute/editor.rkt"
         "../compute/hub.rkt" "../compute/interfaces.rkt" "../compute/i18n.rkt"
         "../compute/view.rkt" "../compute/widgets.rkt")                ; 视图平面

(define WARN '(0.95 0.55 0.45 1.0))
(define MENU-W 220.0)
(define ITEM-H 30.0)
(define ROW-H 46.0)
(define HEAD-H 40.0)
(define HDR-H 30.0)                                     ; 主机标题行
(define ICON-ROW-H 50.0)                                ; 菜单第一行：图标格（图标 + 字）
(define NOTICE-H 40.0)                                  ; 底部提示条
(define ACCENT '(0.55 0.70 1.0 1.0))                    ; 键盘焦点描边
(define ROW-ICON-W 26.0)                                ; 悬停行上的图标按钮宽
(define PREVIEW-LINK '(0.62 0.76 1.0 1.0))              ; 预览：归入项目
(define PREVIEW-ADD '(0.55 0.85 0.62 1.0))              ; 预览：新标题

;; 横扫（§11.4 D）：右扫 ≥ SWIPE-ACT 置顶 / 取消；左扫 ≥ SWIPE-ACT 归档 / 取消、≥ SWIPE-DELETE 删除。松手（ended）时做
(define SWIPE-ACT 60.0)
(define SWIPE-DELETE 150.0)
(define (swipe-action se off)
  (cond [(>= off SWIPE-ACT) (if (eq? (j se 'pinned #f) #t) 'unpin 'pin)]
        [(<= off (- SWIPE-DELETE)) (if (j se 'current #f) #f 'delete)]
        [(<= off (- SWIPE-ACT)) (if (eq? (j se 'archived #f) #t) 'unarchive 'archive)]
        [else #f]))
;; 拖放的目标（分组行）：置顶段、已归档、项目分组（按项目时；「未归类」不算）
(define (drop-target? k)
  (and (string? k)
       (or (regexp-match? #rx"^\u0002sec\u0001.*\u0001pinned$" k)
           (regexp-match? #rx"^\u0002archived\u0001" k)
           (and (regexp-match? #rx"^\u0002grp\u0001" k) (not (regexp-match? #rx"\u0001-$" k))))))

(define BUSY '(0.55 0.70 1.0 1.0))
(define PAD 12.0)

;; menu：上下文菜单 #f | (vector 路径 x y)；confirm：菜单处于「确认删除」的会话路径；
;; rename：#f | (cons 键 编辑器)（行上的改名输入框）；caret：输入框光标矩形（输入法候选框位置）
(struct sb (rc [sessions #:mutable] [hover #:mutable] [confirm #:mutable] [scroll #:mutable] [hits #:mutable] [view-h #:mutable]
               [menu #:mutable] [rename #:mutable] [caret #:mutable]
               [animating #:mutable]                                           ; 本帧画了临时会话的行（在起伏）
               [translucent #:mutable]                                         ; 画在液态玻璃上：#f | 玻璃的圆角（铺半透明的暗色，不画不透明底色）
               [notice #:mutable]                                              ; 底部提示条：#f | 文字（带「撤销」按钮）
               [focus #:mutable]                                               ; 键盘焦点行的键（侧栏有焦点时）或 #f
               [gmenu #:mutable]                                               ; 分组方式菜单开着？（头部右端的 ⇅）
               [gmode #:mutable]                                               ; 当前分组方式（'recent 'project 'workspace 'super）
               [swipe #:mutable]                                               ; 触控板横扫中：#f | (vector 键 偏移)（§11.4 D）
               [drag #:mutable]))                                              ; 拖行中：#f | (vector 键 x y 目标键或#f)

(define (j h k [d #f]) (if (hash? h) (hash-ref h k d) d))
(define (key se) (j se 'key (j se 'path)))
(define (header? se) (equal? (j se 'kind) "header"))
(define (group? se) (equal? (j se 'kind) "group"))
(define (item-h se) (if (or (header? se) (group? se)) HDR-H ROW-H))

(define (session-title s)
  (define title (j s 'title))
  (string-append (if (eq? (j s 'pinned #f) #t) "📌 " "")                         ; 置顶
                 (if (j s 'chat #f) "💬 " "")                                    ; 聊天会话（匿名工作区）
                 (if (and (string? title) (not (string=? (string-trim title) ""))) (regexp-replace* #rx"[\n\t ]+" (string-trim title) " ") (t 'sb.untitled)))
) ; end define session-title

(define (session-subtitle s)
  (define ts (let ([v (j s 'ts "")]) (if (and (string? v) (>= (string-length v) 16)) (regexp-replace #rx"T" (substring v 0 16) " ") (format "~a" v))))
  (t 'sb.subtitle ts (j s 'messages 0) (j s 'model "?"))
) ; end define session-subtitle

;; 慢闪烁：0..1 的余弦起伏，周期 PULSE-MS；时钟可换（测试）
(define PULSE-MS 2400.0)
(define sidebar-clock (make-parameter current-inexact-milliseconds))
(define (pulse) (- 0.5 (* 0.5 (cos (* 2.0 3.141592653589793 (/ ((sidebar-clock)) PULSE-MS))))))

;; 视图平面（compute/view.rkt）：侧栏是一棵元素树——底、头部按钮、可滚动的行、底部提示条，以及锚在点上的上下文菜单 / 分组菜单 / 拖行标签。
;; 行内：横扫时后面露出的动作、内容跟着偏移、悬停的图标、键盘焦点描边，都是元素；坐标、裁剪、命中由布局推出。
(define (with-alpha c a) (list (car c) (cadr c) (caddr c) a))
(define (sb-render! s dl x y w h scale)
  (define rc (sb-rc s))
  (define lay (rich-ctx-layout rc))
  (define list-h (- h HEAD-H (if (sb-notice s) NOTICE-H 0.0)))
  (set-sb-view-h! s list-h)
  (define ss (sb-sessions s))
  (define max-scroll (max 0.0 (- (for/sum ([se (in-list ss)]) (item-h se)) list-h)))
  (set-sb-scroll! s (max 0.0 (min max-scroll (sb-scroll s))))
  ;; 临时会话的行在视区里（画出来了）→ 拼装层持续重绘（起伏）
  (set-sb-animating! s (for/fold ([ry (- (sb-scroll s))] [any? #f] #:result any?) ([se (in-list ss)])
                         (values (+ ry (item-h se)) (or any? (and (j se 'ephemeral #f) (< ry list-h) (> (+ ry (item-h se)) 0.0) #t)))))
  (define hover-c (if (sb-translucent s) HOVER-ON-GLASS '(0.17 0.18 0.22 1.0)))   ; 悬停的底：玻璃上用更亮的半透明
  (define (inset-fill c r) (ui-box #:pad (vector 2.0 6.0 2.0 6.0) (ui-box #:grow 1 #:fill c #:radius r)))
  ;; 头部：新会话 / 聊天 / 分叉（当前会话还没有消息 → 分叉不可用）+ 右端分组方式（⇅；不是「最近」时亮）
  (define cur (findf (lambda (se) (and (not (header? se)) (j se 'current #f))) ss))
  (define fork-ok? (and cur (> (let ([m (j cur 'messages 0)]) (if (real? m) m 0)) 0)))
  (define head
    (apply ui-box #:dir 'row #:h HEAD-H #:align 'center #:gap 6.0 #:pad (vector 9.0 PAD 9.0 PAD)
           (append
            (for/list ([b (in-list (list (list 'new (t 'sb.new) #t) (list 'new-chat (t 'sb.chat) #t) (list 'fork (t 'sb.fork) fork-ok?)))])
              (define on? (caddr b))
              (ui-box #:dir 'row #:align 'center #:h (- HEAD-H 18.0) #:pad (vector 0.0 7.0 0.0 7.0) #:radius 6.0   ; 玻璃上也要看得出（≥ 1.2）
                      #:fill (with-alpha BUTTON (if on? 1.0 BUTTON-OFF-ALPHA)) #:hit (and on? (cons (car b) #t))
                      (ui-text (cadr b) #:font 'small #:color (if on? TEXT TEXT-OFF) #:fit? #f)))
            (list (ui-spacer #:grow 1)
                  (let ([on? (not (eq? (sb-gmode s) 'recent))])
                    (ui-box #:dir 'stack #:w 26.0 #:h (- HEAD-H 18.0) #:radius 6.0 #:align 'center #:valign 'center
                            #:fill (if on? '(0.20 0.33 0.66 1.0) (with-alpha BUTTON 1.0)) #:hit (cons 'group-button #t)
                            (ui-text "⇅" #:font 'body #:color TEXT #:fit? #f #:w 14.0)))))))
  ;; 一行
  (define (row-el se)
    (define ih (item-h se))
    (define k (key se))
    (define sw (and (sb-swipe s) (equal? (vector-ref (sb-swipe s) 0) k) (vector-ref (sb-swipe s) 1)))   ; 这一行在横扫：偏移
    (define drop? (and (sb-drag s) (equal? (vector-ref (sb-drag s) 3) k)))
    (define behind                                                            ; 横扫时行后面露出的动作
      (and sw
           (let* ([act (swipe-action se sw)]
                  [c (case act [(pin unpin) '(0.20 0.33 0.66 1.0)] [(delete) '(0.55 0.18 0.15 1.0)] [(archive unarchive) '(0.30 0.33 0.40 1.0)] [else '(0.16 0.17 0.20 1.0)])]
                  [label (t (case (or act (if (> sw 0) 'pin 'archive)) [(pin) 'sb.m-pin] [(unpin) 'sb.m-unpin] [(archive) 'sb.m-archive]
                              [(unarchive) 'sb.m-unarchive] [else 'sb.swipe-delete]))])
             (ui-box #:pad (vector 2.0 6.0 2.0 6.0)
                     (ui-box #:dir 'row #:grow 1 #:align 'center #:fill c #:radius 7.0 #:pad (vector 0.0 6.0 0.0 6.0)
                             (if (> sw 0) #f (ui-spacer #:grow 1))
                             (ui-text label #:font 'small #:color (if act TEXT DIM-ON-ACCENT) #:fit? #f))))))
    (define content
      (cond
        [(group? se)                                                            ; 分组行（已归档等）：▸ / ▾ + 文字，整行可点
         (ui-box #:dir 'stack
                 (and (equal? (sb-hover s) k) (ui-box #:pad (vector 2.0 6.0 2.0 6.0) (ui-box #:grow 1 #:fill hover-c #:radius 6.0)))
                 (ui-box #:dir 'row #:align 'center #:pad (vector 0.0 PAD 0.0 PAD)
                         (ui-text (format "~a  ~a" (if (j se 'open #f) "▾" "▸") (j se 'label "")) #:font 'small #:color DIM #:grow 1)))]
        [(header? se)
         ;; 主机标题：名字 + 状态；连着 → 右边「＋」新建会话；断开 → 整行「重新连接」
         (define connected? (j se 'connected #t))
         (ui-box #:dir 'row #:align 'center #:pad (vector 0.0 PAD 0.0 PAD) #:gap 6.0
                 (ui-text (format "~a  ~a" (j se 'label "?") (j se 'status "")) #:font 'small #:grow 1
                          #:color (case (j se 'level) [("warn") WARN] [("busy") BUSY] [else DIM]))
                 (and connected?
                      (ui-box #:dir 'row #:align 'center #:h (- ih 8.0) #:pad (vector 0.0 7.0 0.0 7.0) #:fill (with-alpha BUTTON 1.0) #:radius 5.0
                              #:hit (cons 'host-new (j se 'host))
                              (ui-text "＋" #:font 'body #:color TEXT #:fit? #f))))]
        [else (session-row-el se k ih)]))
    (ui-box #:dir 'stack #:h ih
            #:hit (cond [(group? se) (cons 'toggle-group k)]
                        [(header? se) (and (not (j se 'connected #t)) (cons 'host-reconnect (j se 'host)))]
                        [else (cons 'resume k)])
            behind
            (and drop? (ui-box #:pad (vector 1.0 4.0 1.0 4.0) (outline ACCENT)))   ; 拖放目标：描边
            (if sw (ui-box #:dir 'stack #:dx sw content) content)))             ; 横扫的行：内容跟着手指走
  (define (session-row-el se path ih)
    (define current? (j se 'current #f))
    (define ephemeral? (and (j se 'ephemeral #f) #t))
    (define hover? (or (equal? (sb-hover s) path) (and (sb-menu s) (equal? (vector-ref (sb-menu s) 0) path))))
    (define p (if ephemeral? (pulse) 1.0))
    ;; 悬停的行：右边两个图标按钮（置顶、归档）；临时会话、菜单开着、改名中不出。悬停时角标让位
    (define renaming (and (sb-rename s) (equal? (car (sb-rename s)) path) (cdr (sb-rename s))))
    (define icons? (and hover? (not ephemeral?) (not (sb-menu s)) (not renaming)))
    (define icons-w (if icons? (+ (* 2 ROW-ICON-W) 4.0) 0.0))
    (define badge (and (not icons?) (let ([b (j se 'badge)]) (and (string? b) b))))   ; 角标（运行中 / 等待权限）
    (define warn? (equal? (j se 'badge_level) "warn"))
    (define pv (let ([v (j se 'preview #f)]) (and (pair? v) v)))
    (define accent? (or ephemeral? current? hover?))
    (define title-line
      (cond
        [renaming                                                             ; 改名输入框（编辑器是自画的叶子）
         (ui-custom #:measure (lambda (_w) (text-block-line-height (lay "M" 'body 100.0 #f)))
                    #:paint (lambda (dl tx ty tw th scale)
                              (define-values (disp caret _m) (editor-display renaming))
                              (define tb (fit-line lay (if (string=? disp "") " " disp) 'body (- tw 8.0)))
                              (define fh (+ th 4.0))
                              (drawlist-rect! dl (- tx 4.0) (- ty 2.0) (+ tw 8.0) fh 0.30 0.40 0.62 1.0 #:radius 5.0)
                              (drawlist-rect! dl (- tx 3.0) (- ty 1.0) (+ tw 6.0) (- fh 2.0) 0.09 0.09 0.11 1.0 #:radius 4.0)
                              (paint-text! dl (rich-ctx-atlas rc) tb tx ty TEXT #:scale scale)
                              (define-values (_k cxp) (text-block-caret tb (min caret (string-length disp))))
                              (define crect (vector (+ tx cxp) ty 2.0 (text-block-line-height tb)))
                              (drawlist-rect! dl (vector-ref crect 0) (vector-ref crect 1) 1.5 (vector-ref crect 3) 0.55 0.70 1.0 1.0)
                              (set-sb-caret! s crect)
                              '()))]
        [else
         (ui-box #:dir 'row #:align 'start #:gap 6.0
                 (ui-text (session-title se) #:font 'body #:color (if ephemeral? DIM-ON-ACCENT TEXT) #:grow 1)
                 (and badge (ui-box #:dir 'row #:pad (vector 1.0 6.0 1.0 6.0) #:radius 5.0 #:fill (with-alpha (if warn? BADGE-WARN BADGE-BUSY) 1.0)
                                    (ui-text badge #:font 'small #:color (if warn? BADGE-WARN-TEXT BADGE-BUSY-TEXT) #:fit? #f)))
                 (and icons? (ui-spacer #:w icons-w)))]))
    (define subtitle
      (if pv                                                                  ; 变更清单审阅中：副标题换成「会变成什么」
          (apply ui-box #:dir 'row #:gap 10.0 #:overflow 'hide
                 (for/list ([tag (in-list pv)])
                   (ui-text (car tag) #:font 'small #:fit? #f
                            #:color (if accent? TEXT (case (cdr tag) [(link) PREVIEW-LINK] [(rename) PREVIEW-ADD] [(delete) WARN] [(archive) DIM-ON-ACCENT] [else TEXT])))))
          ;; 选中 / 临时 / 悬停的行（底比列表亮）→ 更亮的次要色（DIM 在悬停底上只有 3.7）；字不跟着起伏
          (ui-text (if ephemeral? (t 'sb.ephemeral) (session-subtitle se)) #:font 'small #:color (if accent? DIM-ON-ACCENT DIM))))
    (ui-box #:dir 'stack
            (cond [ephemeral?                                                 ; 临时会话：虚线框 + 淡底，一起慢慢明暗起伏（底 0.50–0.75：最淡时与背景仍 ≥ 1.2）
                   (ui-box #:pad (vector 2.0 6.0 2.0 6.0)
                           (ui-box #:dir 'stack #:grow 1 #:fill (with-alpha EPHEMERAL-FILL (+ 0.50 (* 0.25 p))) #:radius 7.0
                                   (dashed-outline '(0.45 0.55 0.75) (+ 0.3 (* 0.65 p)))))]
                  [current? (inset-fill (list 0.20 0.28 0.45 (if (sb-translucent s) 0.9 1.0)) 7.0)]   ; 0.75 时在黑底的玻璃上只有 1.2
                  [hover? (inset-fill hover-c 7.0)]
                  [else #f])
            (ui-box #:dir 'column #:pad (vector 6.0 PAD 0.0 PAD) title-line subtitle)
            (and (equal? (sb-focus s) path) (ui-box #:pad (vector 1.0 4.0 1.0 4.0) (outline ACCENT)))   ; 键盘焦点
            (and icons?                                                       ; 后画的先命中：图标在行的命中之上
                 (apply ui-box #:dir 'row #:align 'start #:gap 4.0 #:pad (vector 5.0 PAD 0.0 0.0)
                        (ui-spacer #:grow 1)
                        (for/list ([kind (in-list '(row-pin row-archive))])
                          ;; 蓝底的行（当前）上用深色按钮：浅灰按钮在蓝底上只有 1.19
                          (ui-box #:dir 'stack #:w ROW-ICON-W #:h 22.0 #:radius 5.0 #:align 'center #:valign 'center
                                  #:fill (if current? '(0.12 0.13 0.17 1.0) (with-alpha BUTTON 1.0)) #:hit (cons kind path)
                                  (ui-text (if (eq? kind 'row-pin) "📌" "🗄") #:font 'small #:color TEXT #:fit? #f #:w 16.0)))))))
  ;; 底部提示条：「已删除 …」+「撤销」——带边框的卡片（单一填充色没法同时和不透明侧栏与最亮的玻璃都拉开 1.2）
  (define notice
    (and (sb-notice s)
         (ui-box #:h NOTICE-H #:pad (vector 2.0 5.0 4.0 5.0)
                 (ui-box #:grow 1 #:fill '(0.36 0.38 0.46 1.0) #:radius 8.0 #:pad 1.0
                         (ui-box #:dir 'row #:grow 1 #:align 'center #:fill '(0.13 0.14 0.17 1.0) #:radius 7.0 #:pad (vector 0.0 6.0 0.0 6.0) #:gap 8.0
                                 (ui-text (sb-notice s) #:font 'small #:color TEXT #:grow 1)
                                 (ui-box #:dir 'row #:align 'center #:h (- NOTICE-H 16.0) #:pad (vector 0.0 9.0 0.0 9.0) #:fill (with-alpha BUTTON 1.0) #:radius 5.0
                                         #:hit (cons 'undo #t)
                                         (ui-text (t 'sb.undo) #:font 'small #:color TEXT #:fit? #f)))))))
  ;; 上下文菜单（锚在点上，夹进侧栏）：第一行四个图标格（图标 + 字 + 右上角单键），下面是文字项（右边快捷键）
  (define menu
    (and (sb-menu s)
         (let* ([path (vector-ref (sb-menu s) 0)]
                [se (findf (lambda (se) (equal? (key se) path)) ss)])
           (define-values (icons items) (menu-items s se))
           (define mh (+ 8.0 ICON-ROW-H 9.0 (for/sum ([it (in-list items)]) (if (eq? (vector-ref it 0) 'sep) 9.0 ITEM-H))))
           (define mx (max (+ x 6.0) (min (vector-ref (sb-menu s) 1) (- (+ x w) MENU-W 6.0))))
           (define my (max (+ y 6.0) (min (vector-ref (sb-menu s) 2) (- (+ y h) mh 6.0))))
           (define (sep) (ui-box #:h 9.0 #:pad (vector 4.0 6.0 4.0 6.0) (ui-box #:grow 1 #:fill '(0.26 0.27 0.31 1.0))))
           (apply ui-box #:dir 'column #:at (vector (- mx x) (- my y)) #:w MENU-W #:h mh #:fill '(0.16 0.17 0.20 1.0) #:stroke '(0.30 0.31 0.36 1.0)
                  #:radius 7.0 #:hit (cons 'menu #t) #:pad (vector 4.0 4.0 4.0 4.0)
                  (apply ui-box #:dir 'row #:h ICON-ROW-H
                         (for/list ([it (in-list icons)])
                           (define kind (vector-ref it 0)) (define on? (vector-ref it 2))
                           (define hov? (and on? (equal? (sb-hover s) (cons 'item kind))))
                           (ui-box #:dir 'stack #:grow 1 #:hit (and on? (cons kind path))
                                   (and hov? (ui-box #:pad (vector 0.0 2.0 0.0 2.0) (ui-box #:grow 1 #:fill '(0.22 0.30 0.48 1.0) #:radius 6.0)))
                                   (ui-box #:dir 'column #:align 'center #:pad (vector 5.0 3.0 0.0 3.0) #:gap 2.0
                                           (ui-text (vector-ref it 3) #:font 'body #:color (if on? TEXT DIM) #:fit? #f)
                                           (ui-text (vector-ref it 1) #:font 'small #:color (if on? TEXT DIM) #:align 'center))
                                   (ui-box #:dir 'row #:pad (vector 2.0 5.0 0.0 0.0)
                                           (ui-spacer #:grow 1)
                                           (ui-text (vector-ref it 4) #:font 'small #:fit? #f #:color (if hov? DIM-ON-ACCENT DIM))))))   ; 悬停的蓝底上：DIM 只有 3.3
                  (sep)
                  (for/list ([it (in-list items)])
                    (define kind (vector-ref it 0))
                    (cond
                      [(eq? kind 'sep) (sep)]
                      [else
                       (define on? (vector-ref it 2))
                       (define danger? (eq? kind 'delete))
                       (define hov? (and on? (equal? (sb-hover s) (cons 'item kind))))
                       (ui-box #:dir 'stack #:h ITEM-H #:hit (and on? (cons kind path))
                               (and hov? (ui-box #:grow 1 #:fill (if danger? '(0.45 0.16 0.14 1.0) '(0.22 0.30 0.48 1.0)) #:radius 5.0))
                               (ui-box #:dir 'row #:align 'center #:pad (vector 0.0 8.0 0.0 8.0) #:gap 8.0
                                       (ui-text (vector-ref it 1) #:font 'body #:grow 1
                                                #:color (cond [(not on?) DIM] [(and danger? hov?) '(1.0 0.92 0.90 1.0)] [danger? WARN] [else TEXT]))
                                       (ui-text (vector-ref it 3) #:font 'small #:color DIM #:fit? #f)))]))))))
  ;; 分组方式菜单（头部 ⇅ 下面，右对齐）
  (define gmenu
    (and (sb-gmenu s)
         (let* ([gw 210.0] [modes '(recent project workspace super)] [gh (+ 8.0 (* ITEM-H (length modes)))])
           (apply ui-box #:dir 'column #:at (vector (- w PAD gw) (- HEAD-H 4.0)) #:w gw #:h gh #:fill '(0.16 0.17 0.20 1.0) #:stroke '(0.30 0.31 0.36 1.0)
                  #:radius 7.0 #:hit (cons 'gmenu #t) #:pad (vector 4.0 4.0 4.0 4.0)
                  (for/list ([m (in-list modes)])
                    (ui-box #:dir 'stack #:h ITEM-H #:hit (cons 'group-by m)
                            (and (equal? (sb-hover s) (cons 'gitem m)) (ui-box #:grow 1 #:fill '(0.22 0.30 0.48 1.0) #:radius 5.0))
                            (ui-box #:dir 'row #:align 'center #:pad (vector 0.0 6.0 0.0 6.0)
                                    (ui-text (string-append (if (eq? m (sb-gmode s)) "✓ " "   ") (t (string->symbol (format "sb.group-~a" m))))
                                             #:font 'body #:color TEXT #:grow 1))))))))
  ;; 拖行：光标旁边一块小标签（行的标题）
  (define ghost
    (and (sb-drag s)
         (let* ([d (sb-drag s)] [se (findf (lambda (se) (equal? (key se) (vector-ref d 0))) ss)])
           (ui-box #:at (vector (- (vector-ref d 1) x -11.0) (- (vector-ref d 2) y -5.0)) #:fill '(0.55 0.70 1.0 1.0) #:radius 6.0 #:pad 1.0
                   (ui-box #:fill '(0.13 0.14 0.17 1.0) #:radius 5.0 #:pad (vector 4.0 8.0 4.0 8.0)
                           (ui-text (if se (session-title se) "…") #:font 'small #:color TEXT #:fit? #f #:w (min 160.0 (text-block-width (lay (if se (session-title se) "…") 'small 1000.0 #f)))))))))
  (define tree
    (ui-box #:dir 'stack
            (if (sb-translucent s)                                            ; 玻璃上：底由玻璃给，上面铺一层暗色保证字的对比度
                (ui-box #:fill GLASS-SCRIM #:radius (sb-translucent s))
                (ui-box #:fill '(0.11 0.11 0.13 1.0)))
            (ui-box #:dir 'column
                    head
                    (apply ui-scroll 'list #:offset (sb-scroll s) (for/list ([se (in-list ss)]) (row-el se)))
                    notice)
            ghost menu gmenu))
  (define-values (hits _regions scrolls) (render-view! rc dl tree x y w h scale))
  (set-sb-scroll! s (vector-ref (hash-ref scrolls 'list) 0))
  (set-sb-hits! s (reverse hits))                                            ; 后画的先命中（菜单盖在行上）
) ; end define sb-render!

;; 一行的菜单：→ (values 图标格 文字项)
;;   图标格：(vector kind 文字 可用? 图标 单键)；文字项：(vector kind 文字 可用? 快捷键) 或 (vector 'sep)。
;;   分叉只对当前会话（有消息时）；Finder 只对本机主机的会话（行上 'local）；删除一次点击（进回收站、可撤销），当前会话不能删
(define MENU-KINDS '(open rename info pin unpin archive unarchive fork copy-path reveal export-md export-json delete mem-read mem-write host-index tests))
(define (menu-items s se)
  (define current? (and se (j se 'current #f)))
  (define msgs (let ([m (and se (j se 'messages 0))]) (if (real? m) m 0)))
  (define keep? (not (and se (eq? (j se 'ephemeral #f) #t))))                     ; 临时会话（切走就删）：不能置顶 / 归档
  (values
   (list (if (and se (eq? (j se 'pinned #f) #t)) (vector 'unpin (t 'sb.m-unpin) #t "📌" "P") (vector 'pin (t 'sb.m-pin) keep? "📌" "P"))
         (if (and se (eq? (j se 'archived #f) #t)) (vector 'unarchive (t 'sb.m-unarchive) #t "🗄" "E") (vector 'archive (t 'sb.m-archive) keep? "🗄" "E"))
         (vector 'rename (t 'sb.m-rename-short) #t "✏️" "R")
         (vector 'info (t 'sb.m-info-short) #t "ℹ️" "I"))
   (list (vector 'open (t 'sb.m-open) (not current?) "↩")
         (vector 'fork (t 'sb.m-fork) (and current? (> msgs 0)) "")
         (vector 'tests (t 'sb.m-tests) current? (key-hint 'tests-panel))                       ; 工作区测试流程面板（当前会话的工作目录）
         (vector 'sep)
         (vector 'copy-path (t 'sb.m-copy-path) #t "")
         (vector 'reveal (t 'sb.m-reveal) (and se (j se 'local #f) #t) "")
         (vector 'export-md (t 'sb.m-export-md) (> msgs 0) "")
         (vector 'export-json (t 'sb.m-export-json) (> msgs 0) "")
         (vector 'sep)
         ;; 记忆（design-memory.md M2）：这个会话读 / 写记忆（缺省开；打勾 = 开）
         (vector 'mem-read (string-append (if (eq? (j se 'mem_read #t) #f) "   " "✓ ") (t 'sb.m-mem-read)) #t "")
         (vector 'mem-write (string-append (if (eq? (j se 'mem_write #t) #f) "   " "✓ ") (t 'sb.m-mem-write)) #t "")
         ;; 项目索引（design-memory.md §8）：缺省（本机模型可用、云端不能）→ 开 → 关 → 缺省
         (vector 'host-index (t (case (j se 'host_index 'unset) [(#t) 'sb.m-hix-on] [(#f) 'sb.m-hix-off] [else 'sb.m-hix-auto])) #t "")
         (vector 'sep)
         (if current? (vector 'delete (t 'sb.cannot-delete) #f "") (vector 'delete (t 'sb.delete-now) #t "⌫")))))

;; 左键点击 → (list 'new) | (list 'fork) | (list 'resume 键) | (list 'delete 键) | (list 'undo)（提示条的「撤销」）
;;   | 菜单项：(list 'open 键) (list 'info 键) (list 'menu-fork 键) (list 'copy-path 键) (list 'reveal 键) (list 'export 键 "markdown"|"json")
;;     (list 'pin 键) (list 'unpin 键) (list 'archive 键) (list 'unarchive 键)
;;   | (list 'toggle-group 分组键)（展开 / 收起分组行）
;;   | (list 'rename-start 键)（组件已进入改名状态）
;;   | (list 'host-new 主机) | (list 'host-reconnect 主机)
;;   | (list 'menu)（菜单空白处）| (list 'close)（菜单开着时点了菜单外：只关菜单）| #f
(define (sb-hit s x y)
  (define h (find-hit s x y))
  (define menu? (and (sb-menu s) #t))
  (define (close-menu!) (set-sb-menu! s #f) (set-sb-confirm! s #f))
  (cond
    [(and (sb-gmenu s) (not (and h (memq (vector-ref h 0) '(gmenu group-by group-button)))))   ; 分组菜单开着：点外面只关它
     (set-sb-gmenu! s #f) (list 'close)]
    [(and menu? (not (and h (memq (vector-ref h 0) (cons 'menu MENU-KINDS))))) (close-menu!) (list 'close)]
    [(not h) (when (sb-rename s) (set-sb-rename! s #f)) #f]
    [else
     (define kind (vector-ref h 0))
     (define k (vector-ref h 1))
     (unless (eq? kind 'menu) (set-sb-rename! s #f))
     (case kind
       [(delete) (close-menu!) (list 'delete k)]
       [(undo) (list 'undo)]
       [(group-button) (set-sb-gmenu! s (not (sb-gmenu s))) (set-sb-menu! s #f) (list 'menu)]
       [(gmenu) (list 'menu)]
       [(group-by) (set-sb-gmenu! s #f) (list 'group-by k)]
       [(row-pin row-archive)                                                    ; 行上的图标：按行现在的状态取反
        (define se (findf (lambda (se) (equal? (key se) k)) (sb-sessions s)))
        (if (eq? kind 'row-pin)
            (list (if (and se (eq? (j se 'pinned #f) #t)) 'unpin 'pin) k)
            (list (if (and se (eq? (j se 'archived #f) #t)) 'unarchive 'archive) k))]
       [(menu) (list 'menu)]
       [(new fork new-chat) (close-menu!) (list kind)]                           ; 头部按钮，或菜单里的「从末尾分叉」
       [(open) (close-menu!) (list 'resume k)]
       [(rename) (close-menu!) (start-rename! s k) (list 'rename-start k)]
       [(info copy-path reveal pin unpin archive unarchive mem-read mem-write host-index tests) (close-menu!) (list kind k)]
       [(export-md) (close-menu!) (list 'export k "markdown")]
       [(export-json) (close-menu!) (list 'export k "json")]
       [else (list kind k)])])
) ; end define sb-hit

;; (x y) 下的会话行的键 / 分组行的键（命中区里找）
(define (row-at s x y)
  (for/first ([h (in-list (sb-hits s))]
              #:when (and (memq (vector-ref h 0) '(resume toggle-group))
                          (<= (vector-ref h 2) x (+ (vector-ref h 2) (vector-ref h 4))) (<= (vector-ref h 3) y (+ (vector-ref h 3) (vector-ref h 5)))))
    (vector-ref h 1)))
;; 横扫：began → 记下行；changed → 累加；ended → 按偏移给动作（或 #f）并清掉。→ #f | (list 动作 键)
(define (swipe! s k dx phase)
  (case phase
    [(began) (set-sb-swipe! s (and k (vector k 0.0))) #f]
    [(changed none) (let ([sw (sb-swipe s)]) (when sw (set-sb-swipe! s (vector (vector-ref sw 0) (+ (vector-ref sw 1) dx))))) #f]
    [(ended)
     (define sw (sb-swipe s))
     (set-sb-swipe! s #f)
     (and sw
          (let* ([k (vector-ref sw 0)] [se (findf (lambda (se) (equal? (key se) k)) (sb-sessions s))] [act (and se (not (eq? (j se 'ephemeral #f) #t)) (swipe-action se (vector-ref sw 1)))])
            (and act (list act k))))]
    [else #f]))
(define (drag-move! s x y)
  (define d (sb-drag s))
  (when d
    (define k (row-at s x y))
    (set-sb-drag! s (vector (vector-ref d 0) x y (and (drop-target? k) k)))))
;; → (list 'drop 键 目标键) | #f
(define (drag-end! s)
  (define d (sb-drag s))
  (set-sb-drag! s #f)
  (and d (vector-ref d 3) (list 'drop (vector-ref d 0) (vector-ref d 3))))

;; 行上换成改名输入框（旧标题全选）
(define (start-rename! s k)
  (define se (findf (lambda (se) (equal? (key se) k)) (sb-sessions s)))
  (define ed (make-editor))
  (let ([tt (and se (j se 'title))]) (when (string? tt) (editor-insert! ed tt) (editor-command! ed 'select-all)))
  (set-sb-rename! s (cons k ed)))

;; 键盘焦点在会话行之间移动 d 行（标题行、分组行跳过；没有焦点 → 当前会话或第一行），滚到看得见；→ 新的键
(define (move-focus! s d)
  (define rows (filter (lambda (se) (not (or (header? se) (group? se)))) (sb-sessions s)))
  (cond
    [(null? rows) (set-sb-focus! s #f) #f]
    [else
     (define keys (map key rows))
     (define i0 (or (index-of keys (sb-focus s))
                    (index-of keys (let ([c (findf (lambda (se) (j se 'current #f)) rows)]) (and c (key c))))))
     (define i (if i0 (max 0 (min (sub1 (length keys)) (+ i0 d))) 0))
     (define k (list-ref keys i))
     (set-sb-focus! s k)
     ;; 滚到看得见
     (define top (for/sum ([se (in-list (sb-sessions s))] #:break (equal? (key se) k)) (item-h se)))
     (cond [(< top (sb-scroll s)) (set-sb-scroll! s top)]
           [(> (+ top ROW-H) (+ (sb-scroll s) (sb-view-h s))) (set-sb-scroll! s (- (+ top ROW-H) (sb-view-h s)))])
     k]))

;; 改名：回车 → (list 'rename 键 新名) 或 #f（空）；Esc → 取消
(define (sb-submit-rename! s)
  (define r (sb-rename s))
  (and r
       (let ([v (string-trim (editor-flatten (cdr r)))])
         (set-sb-rename! s #f)
         (and (not (string=? v "")) (list 'rename (car r) v)))))

(define (find-hit s x y)
  (for/first ([h (in-list (sb-hits s))]
              #:when (and (<= (vector-ref h 2) x (+ (vector-ref h 2) (vector-ref h 4)))
                          (<= (vector-ref h 3) y (+ (vector-ref h 3) (vector-ref h 5)))))
    h)
) ; end define find-hit

;; 右键 / ctrl-点击：在行上打开上下文菜单（菜单左上角在点击处）。→ 是否打开了
(define (sb-context! s x y)
  (define h (for/first ([h (in-list (sb-hits s))]
                        #:when (and (eq? (vector-ref h 0) 'resume)
                                    (<= (vector-ref h 2) x (+ (vector-ref h 2) (vector-ref h 4)))
                                    (<= (vector-ref h 3) y (+ (vector-ref h 3) (vector-ref h 5)))))
              h))
  (set-sb-confirm! s #f)
  (set-sb-menu! s (and h (vector (vector-ref h 1) x y)))
  (and h #t)
) ; end define sb-context!

;; 测试 / 冒烟：命中目标的中心点（头部按钮 payload 为 #t）
(define (sb-hit-center s kind payload)
  (for/first ([h (in-list (sb-hits s))] #:when (and (eq? (vector-ref h 0) kind) (equal? (vector-ref h 1) payload)))
    (cons (+ (vector-ref h 2) (/ (vector-ref h 4) 2)) (+ (vector-ref h 3) (/ (vector-ref h 5) 2))))
) ; end define sb-hit-center

;; 悬停：只对行生效；返回是否变化（拼装层据此决定重绘）
(define (sb-hover! s x y)
  (define hit (for/first ([h (in-list (sb-hits s))]
                          #:when (and (memq (vector-ref h 0) (list* 'resume 'toggle-group 'row-pin 'row-archive 'group-by MENU-KINDS))
                                      (<= (vector-ref h 2) x (+ (vector-ref h 2) (vector-ref h 4)))
                                      (<= (vector-ref h 3) y (+ (vector-ref h 3) (vector-ref h 5)))))
                h))
  ;; 菜单开着：悬停的是菜单项（cons 'item kind）；否则是行（键）
  (define h (and hit (cond [(memq (vector-ref hit 0) '(resume toggle-group row-pin row-archive)) (vector-ref hit 1)]
                           [(eq? (vector-ref hit 0) 'group-by) (cons 'gitem (vector-ref hit 1))]
                           [else (cons 'item (vector-ref hit 0))])))
  (begin0 (not (equal? h (sb-hover s))) (set-sb-hover! s h))
) ; end define sb-hover!

(define sidebar-component
  (component 'sidebar (list text-layout) (list session-list)
             (lambda (use config)
               (define s (sb ((use text-layout) 'rich) '() #f #f 0.0 '() 0.0 #f #f #f #f #f #f #f #f 'recent #f #f))
               (list (implement session-list
                                'set-sessions! (lambda (ss0)
                                                 (define ss (if (list? ss0) (filter hash? ss0) '()))   ; 内核数据不合形状：当空表，不让每帧重绘都失败
                                                 (set-sb-sessions! s ss)
                                                 (unless (and (sb-menu s) (findf (lambda (se) (equal? (key se) (vector-ref (sb-menu s) 0))) ss))
                                                   (set-sb-menu! s #f) (set-sb-confirm! s #f)))   ; 菜单对应的会话没了：关菜单
                                'sessions (lambda () (sb-sessions s))
                                'render! (lambda (dl x y w h scale) (sb-render! s dl x y w h scale))
                                'hit (lambda (x y) (sb-hit s x y))
                                'hover! (lambda (x y) (sb-hover! s x y))
                                'scroll! (lambda (dy) (set-sb-scroll! s (- (sb-scroll s) dy)))
                                'context! (lambda (x y) (sb-context! s x y))
                                'context-open? (lambda () (and (sb-menu s) #t))
                                'close-context! (lambda () (set-sb-menu! s #f) (set-sb-confirm! s #f))
                                'hit-center (lambda (kind payload) (sb-hit-center s kind payload))
                                ;; 改名输入框
                                'editing? (lambda () (and (sb-rename s) #t))
                                'editor (lambda () (and (sb-rename s) (cdr (sb-rename s))))
                                'submit! (lambda () (sb-submit-rename! s))
                                'cancel-edit! (lambda () (set-sb-rename! s #f))
                                'caret-rect (lambda () (and (sb-rename s) (sb-caret s)))
                                'animating? (lambda () (sb-animating s))
                                'set-translucent! (lambda (on? [radius 0.0]) (set-sb-translucent! s (and on? (exact->inexact radius))))
                                ;; 少操作（§11 A）：提示条、键盘焦点、直接进入改名
                                'set-notice! (lambda (text) (set-sb-notice! s text))
                                'set-focus! (lambda (k) (set-sb-focus! s k))
                                'focus (lambda () (sb-focus s))
                                'move-focus! (lambda (d) (move-focus! s d))
                                'start-rename! (lambda (k) (start-rename! s k))
                                'context-key (lambda () (and (sb-menu s) (vector-ref (sb-menu s) 0)))
                                'set-group-mode! (lambda (m) (set-sb-gmode! s m))
                                ;; 手势与拖放（§11.4 D）
                                'row-at (lambda (x y) (row-at s x y))
                                'swipe! (lambda (k dx phase) (swipe! s k dx phase))
                                'swiping? (lambda () (and (sb-swipe s) #t))
                                'drag-start! (lambda (k x y) (set-sb-drag! s (vector k x y #f)) (set-sb-menu! s #f))
                                'drag-move! (lambda (x y) (drag-move! s x y))
                                'drag-end! (lambda () (drag-end! s))
                                'dragging? (lambda () (and (sb-drag s) #t)))))))

(provide sidebar-component sidebar-clock PULSE-MS)
