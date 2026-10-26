#lang racket/base
;; interfaces.rkt — GUI 组件之间的全部接口（计算层：只有名字与方法表，没有实现）。
;; 提供方组件与使用方组件都只依赖这里，互不认识；谁提供哪个接口由拼装层决定（hub.rkt assemble）。

(require "hub.rkt")

;; 字体后端（计算层 mac-coretext 等的实现，由拼装层直接放进 env）
;;   font (family size) → id；shape (str font) → shaped；metrics (font) → #(asc desc lead)；
;;   rasterize (font glyph scale subpx) → #f | #(w h left top rgba color?)
(define-interface font-backend font shape metrics rasterize)

;; 文字系统：排版 + glyph atlas
;;   layout (str font-key width [box-width]) → text-block；rich () → rich-ctx；atlas () → atlas
;;   build-frame! (dl thunk) → 实例数（atlas 满则重建一次）；take-dirty! () → #f | (cons 位图 rect)；glyph-count () → n
(define-interface text-layout layout rich atlas build-frame! take-dirty! glyph-count)

;; GPU 渲染
;;   layer () → CAMetalLayer 指针或 #f；resize! (w h scale)；render! (dl clear w h scale) → bool
;;   render-offscreen (dl clear pw ph scale) → BGRA bytes；upload-atlas! (bytes rect)；device-name ()
;;   overlay-layer () → 第二张透明 CAMetalLayer（懒建）；render-overlay! (dl w h scale)：画到它上面（清成全透明）
;;   set-opaque! (bool)：主层是否不透明（透出桌面时关）
(define-interface gpu-port layer resize! render! render-offscreen upload-atlas! upload-images! device-name overlay-layer render-overlay! set-opaque!)

;; 窗口
;;   size () → (values w h scale)；pump! () → 事件表；set-text-input-state! (caret-rect index)；
;;   discard-marked! ()；on-live-resize! (proc)；set-content-size! (w h)；close! ()；set-cursor! ('arrow | 'resize-h)
;;   glass? () → 有液态玻璃侧栏（窗口配置 'glass? 且系统有玻璃视图）；set-glass-rect! ((vector x y w h 圆角) | #f)：玻璃面板位置（#f = 不用）
;;   set-transparent! (bool)：窗口透明（内容层画的是什么就是什么，没画的地方透出桌面）
;;   capture (hide-overlay? below?) → #f | (vector 宽 高 RGBA 每point像素)：截内容区（系统合成后；显示效果的自动检查）。
;;     没有屏幕录制权限时 below? 截不到下面的窗口（那部分是透明的），只有本窗口自己的内容
;;   set-text-input-enabled! (bool)：#f 时按键不进输入法（只有原始按键事件）——侧栏有键盘焦点时的单键操作
(define-interface window-port size pump! set-text-input-state! discard-marked! on-live-resize! set-content-size! close! set-cursor! glass? set-glass-rect!
  set-transparent! capture set-text-input-enabled!)

;; 剪贴板：get () → 字符串或 #f；set! (s)；call-with-private (thunk) —— 测试用私有剪贴板
(define-interface clipboard get set! call-with-private)

;; 窗口测试替身输入（冒烟 / 验收脚本用）：合成鼠标、滚轮 (x y dy)、按键（含带 CGEvent 的按键）、直接投递粘贴、模拟输入法、
;; 查询输入法状态、窗口是否为 key 窗口（应用不在前台时没有 key 窗口，Edit 菜单动作无处投递）
;;   post-attach! (items)：直接投递附件事件（ev:attach，同粘贴 / 拖放文件）
;;   post-mouse-ns! (kind x y [button 0] [mods '()] [clicks 1])：造 NSEvent 交给窗口的 sendEvent:（命中测试 → 视图的 mouseDown: / rightMouseDown: …），
;;     与真实点击同一条 AppKit 路径；post-mouse! 直接入队、不经 AppKit
(define-interface window-input post-mouse! post-mouse-ns! post-scroll! post-key! post-key-cg! post-paste! post-attach! simulate-ime! ime-query ime-diagnostics key?)

;; 输入框
;;   editor () → 编辑器对象（计算层 editor.rkt，拼装层直接用其操作）；render! (dl x bottom w scale focused? caret-on?) → 顶边 y
;;   hit (x y) → 命中；move-vertical! (dir extend?) → 'handled | 'top | 'bottom；caret-rect () → #(x y w h) | #f；
;;   scroll! (x y dy) → 是否处理（指针在输入框上：内容超过可见行数时按行滚，光标一动就回到跟随光标）
(define-interface input-box editor render! hit move-vertical! caret-rect scroll!)

;; 对话区
;;   hit (x y) → (list 'perm id 决定) | (list 'tool id) | (list 'copy 文本) | (list 'code-toggle 键) | (list 'to-bottom) | (list 'minimap) | #f
;;   多主机：每个主机一份文档；use-doc! (id) 切当前文档；current-doc ()；apply-to! (id 事件)、note-to! (id 文字 [级别]) 写指定文档；
;;   reset-doc! (id) 清空（重连时守护进程会补齐）；drop-doc! (id)；doc-info (id) → #f | hash（busy pending turns）
(define-interface transcript-port
  apply! add-user! note! answer! toggle-tool! toggle-code! pending-permissions permission-info
  render! scroll! scroll-to-bottom! hit hit-center summary busy? turns last-assistant-text
  ;; 选择 / 复制（按看到的文字）：select-start! (x y)、select-extend! (x y)、clear-selection! ()、selection-text () → 字符串或 #f
  select-start! select-extend! clear-selection! selection-text
  ;; 局部加载（RPC v3）：wanted () → 本帧要发给当前主机的请求（items / item / index_summary，带 'tag）并清空；
  ;;   stats () → hash（total loaded pages content_h scroll anchor）；set-preview! (配置) 工具输出折叠预览（lines mode tools）
  ;;   minimap-jump! (y) 点 / 拖小地图（hit 给出 (list 'minimap)）：跳到对应位置
  ;;   toggle-play! (sha) 音频块的 ▶ / ❚❚（hit 给出 (list 'media-play sha)）；add-user! 的第三个参数：附件（kind media_type sha name meta bytes）
  wanted stats set-preview! minimap-jump! toggle-play!
  ;; tool-uis () → 带结构化结果的工具卡片：(listof (list 工具名 种类 条数))（冒烟流程断言用）
  tool-uis
  use-doc! current-doc apply-to! note-to! reset-doc! drop-doc! doc-info)

;; 内核连接（一窗多主机：每个 pi2 内核一个主机 id；消息协议见 src/rpc.rkt）
;;   add! (id 配置)；remove! (id [超时]) → 退出码；ids () → 加入顺序；send! (id jsexpr) → bool；
;;   poll! () → ((id . 事件) …)（非阻塞）；alive? (id)；stderr-tail (id [n]) → 行表；close-all! ([超时]) → ((id . 退出码) …)
(define-interface kernel-set add! remove! ids send! poll! alive? stderr-tail close-all!)

;; 信息面板（design-workspace.md W1，components/inspector.rkt）：open! (内核 session_stats 的结果)；close! ()；open? ()；data ()；
;;   render! (dl x y w h scale)（x y w h = 对话区）；hit (x y) → (list 'close) | #f；scroll! (dy)；hit-center (kind)
(define-interface inspector-view open! close! open? data render! hit scroll! hit-center)

;; 变更清单的审阅面板（design-workspace.md §11.7 C3，components/review.rkt）：open! (数据) close! open? data accepted（勾选的项号）
;;   render! (dl x y w h scale) hit (x y) → (list 'toggle n) | (list 'toggle-group 组) | (list 'apply) | (list 'discard) | (list 'undo) | (list 'close) | #f
;;   scroll! (dy) hit-center (kind [payload])
(define-interface review-view open! close! open? data accepted render! hit scroll! hit-center)

;; 工作区测试流程面板（design-goalmode.md §12 T2，components/tests-panel.rkt）：open! (内核 test_flow 数据) update! (数据) set-result! (test_flow_result)
;;   set-running! (真假) close! open? data render! (dl x y w h scale) hit (x y) → (list 'close) | (list 'trust sha) | (list 'run #f|步骤) | (list 'approve id) | (list 'reject id) | #f
;;   scroll! (dy) hit-center (kind [payload])
(define-interface tests-view open! update! set-result! set-running! close! open? data render! hit scroll! hit-center)

;; 媒体（P4，components/mac-media.rkt）：缩略图与音频播放；解码 / 播放在沙箱子进程里
;;   begin-frame! ()；thumb (键) → #f | (vector u0 v0 u1 v1 宽 高 原宽 原高)（图片纹理池里的位置；宽高：像素）；
;;   decode! (键 字节)；probe-audio! (键 字节) → 之后 info (键) → #f | hash（'duration）；status (键) → 'none 'pending 'ready 'failed；
;;   poll! () → 有新结果？；take-upload! () → #f | (cons 位图 (vector x y w h))；atlas-size ()；
;;   play! (键 字节) pause! (键) resume! (键) stop! () seek! (键 秒)；audio (键) → #f | (vector 在播? 位置 时长)；close! ()
(define-interface media-port begin-frame! thumb decode! probe-audio! info status poll! take-upload! atlas-size
  play! pause! resume! stop! seek! audio close!)

;; 会话侧栏（G5）：set-sessions! (内核 sessions 事件里的会话表)；sessions () → 当前列表；render! (dl x y w h scale)；
;;   多主机：表里可有主机标题行（'kind "header"），会话行以 'key 为标识（缺省 'path）；
;;   hit (x y) → (list 'new) | (list 'fork) | (list 'resume 键) | (list 'ask-delete 键) | (list 'delete 键)
;;     | (list 'host-new 主机) | (list 'host-reconnect 主机)
;;     | (list 'menu) | (list 'close) | #f（上下文菜单开着时点任何地方都被消化）；
;;   context! (x y) → 右键 / ctrl-点击处的行打开上下文菜单（删除只在这里），返回是否打开；context-open? ()；close-context! ()；
;;   hover! (x y) → 悬停是否变化；scroll! (dy)；hit-center (kind 路径或 #t) → (cons x y) | #f（测试 / 冒烟）
;;   右键菜单与改名（design-workspace.md W1）：editing? () editor () submit! () → (list 'rename 键 新名) | #f；cancel-edit! ()；caret-rect ()
;;   animating? ()：上一帧画了在起伏的临时会话行（拼装层据此持续重绘）；set-translucent! (bool [圆角])：画在液态玻璃上（铺暗色，不画不透明底色）
;;   少操作（design-workspace.md §11 A）：set-notice! (文字 | #f)：底部提示条（带「撤销」，点 → (list 'undo)）；
;;     set-focus! (键 | #f) / focus ()：键盘焦点行（描边）；move-focus! (±n) → 新的键（跳过标题 / 分组行，滚到看得见）；start-rename! (键)：直接进入改名；
;;     context-key () → 上下文菜单开着的那一行的键（菜单开着时单键作用于它）；
;;   分组方式（W3）：set-group-mode! ('recent 'project 'workspace 'super)：头部 ⇅ 的勾与高亮；hit → (list 'group-by 方式)
;;   手势与拖放（§11.4 D）：row-at (x y) → 行 / 分组行的键；swipe! (键 dx 阶段) → #f | (list 动作 键)（ended 时）；swiping? ()；
;;     drag-start! (键 x y) drag-move! (x y) drag-end! () → #f | (list 'drop 键 目标分组键)；dragging? ()
(define-interface session-list set-sessions! sessions render! hit context! context-open? close-context! hover! scroll! hit-center
  editing? editor submit! cancel-edit! caret-rect animating? set-translucent!
  set-notice! set-focus! focus move-focus! start-rename! context-key set-group-mode!
  row-at swipe! swiping? drag-start! drag-move! drag-end! dragging?)

;; 模型选择（G5）：set-state! (hash：provider model reasoning，缺的字段保留旧值)；state () → 当前 hash 或 #f；
;;   set-options! (内核 providers 事件)；set-route! (内核 route_status 事件：更新一个路由池的成员状态)；
;;   render! (dl x y max-w win-h scale) → 芯片宽度：芯片画在状态行左端，打开时菜单在芯片上方；
;;   hit (x y) → (list 'toggle) | (list 'provider 名) | (list 'model 名) | (list 'reasoning 档) | (list 'close) | (list 'menu) | #f
;;   （打开时任何点击都被消化；选中后菜单不关）；open? () / open! () / close! ()；hover! (x y) → 是否变化；
;;   scroll! (x y dy) → 是否处理（指针在哪一栏滚哪一栏；不在菜单上 → #f）；hit-center (kind payload)
;; 多主机：reset! () 清掉状态与选项（切换主机时先清再填那台的）
(define-interface model-picker set-state! state set-options! set-route! reset! render! hit open? open! close! hover! scroll! hit-center)

;; 操作日志（G6）：record! (kind payload) —— kind ∈ 'start（窗口尺寸）'win（窗口事件 prefab）'core（内核事件）
;;   'send（发给内核的请求）'frame（一次重绘）
;;   'error（hash：where message context）'note（字符串）；path () → 日志文件路径或 #f（只在内存）；
;;   tail (n) → 最近 n 条 (vector 毫秒 kind 摘要)；errors () → 最近的错误（hash 表，新的在前）；flush! ()；close! ()
;; redact! (原文 替换)：事后把日志（内存与本次文件）里的原文换掉（选中文字标记为密钥）
(define-interface journal record! path tail errors flush! close! redact!)

;; 回放源（G6）：回放结束时比对——sent () → 回放中发给内核的请求；expected () → 日志里记录的请求；
;;   progress () → (cons 已消费条数 总条数)
(define-interface replay-source sent expected progress)

;; 调试面板（G6）：toggle! ()；open? ()；render! (dl x y w h scale 统计行表)
(define-interface debug-view toggle! open? render!)

;; 设置页（⌘,）：open? / open! / close! / toggle!；set-data! (键 值)——'providers 'rules 'permission 'lang 'journal；
;;   render! (dl x y w h scale)；hit (x y) → 动作（见 settings.rkt）；hover! (x y)；scroll! (x y dy)；
;;   输入框：editing? / secret?（正在输 key：日志要打码）/ editor / submit! → 动作 / cancel-edit! / caret-rect；section ()；hit-center
(define-interface settings-view open? open! close! toggle! set-data! render! hit hover! scroll!
  editing? secret? multiline? editor submit! cancel-edit! caret-rect section hit-center)

(provide font-backend text-layout gpu-port window-port clipboard window-input input-box transcript-port kernel-set media-port inspector-view review-view tests-view
         session-list model-picker journal replay-source debug-view settings-view)
