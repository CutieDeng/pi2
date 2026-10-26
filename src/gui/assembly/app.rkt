#!/usr/bin/env racket
#lang racket/base
;; app.rkt — 拼装层：配置组件（assemble 按 needs / provides 自动发现接口）、建立组件间的联系、驱动对外接口。
;;   组件：renderer（Metal）window（窗口 / IME / 剪贴板）text-system（文字）input（输入框）transcript（对话区）
;;         sidebar（会话侧栏）model-picker（模型 / 推理档）kernel（pi2 内核，独立 place）。组件之间不互相调用；联系都在这里：
;;           内核事件 → 对话区 / 侧栏 / 模型芯片；窗口事件 → 输入框 / 对话区 / 侧栏 / 模型菜单；
;;           输入框提交、侧栏与模型菜单的动作 → 内核请求；文字系统 atlas 脏区 → 渲染
;;
;;   racket src/gui/assembly/app.rkt [-- 内核参数…]       开窗口；内核 = 本机守护进程（src/serve.rkt，按工作目录一个，没在跑就起，
;;                                                      空闲 30 分钟自动退出；关窗口不停内核）里的 pi2 main.rkt --rpc 内核参数…
;;   racket src/gui/assembly/app.rkt --in-process [-- …]  内核放在本进程的独立 place 里（随窗口退出）
;;   racket src/gui/assembly/app.rkt --core <模块.rkt>     用指定模块的 main 子模块当内核（冒烟用脚本化内核）
;;   racket src/gui/assembly/app.rkt --no-core             不启动内核
;;   racket src/gui/assembly/app.rkt --remote 主机 [--remote-name N]   连服务器上常驻的内核（src/serve.rkt，经 ssh）
;;   racket src/gui/assembly/app.rkt --remote-cmd "命令…"  用这条命令当传输（不经 ssh；如本机的 racket src/serve.rkt attach）
;;   racket src/gui/assembly/app.rkt --selftest [--png p]  离屏渲染 + 像素比对，不开窗口
;;   racket src/gui/assembly/app.rkt --snapshot p.png [--size 900x720]   典型场景离屏渲染成 PNG
;;   冒烟 / 验收：--exit-after ms  --resize-to WxH  --synth  --synth-core  --synth-sessions  --synth-picker
;;               --synth-live 提示  --allow-rx 正则
;;               --ime-real 输入源id  --snapshot-at-exit p.png  --core-cwd 目录

(require racket/cmdline racket/list racket/string racket/runtime-path (for-syntax racket/base)
         "../compute/hub.rkt" "../compute/interfaces.rkt"
         "../compute/event.rkt" "../compute/drawlist.rkt" "../compute/png.rkt" "../compute/text-backend.rkt"
         "../compute/layout.rkt" "../compute/paint.rkt" "../compute/rich.rkt" "../compute/fold.rkt" "../compute/editor.rkt"
         "../compute/mac-objc.rkt" "../compute/mac-coretext.rkt" "../compute/mac-inputsource.rkt"
         "../components/mac-renderer.rkt" "../components/mac-window.rkt" "../components/text-system.rkt"
         "../components/input.rkt" "../components/transcript.rkt" "../components/kernels.rkt"
         "../components/sidebar.rkt" "../components/model-picker.rkt"
         "../components/journal.rkt" "../components/replay-host.rkt" "../components/debug-panel.rkt"
         "../components/settings.rkt" "../compute/gui-settings.rkt" "../compute/secret-handle.rkt"
         "../compute/journal-format.rkt" "../compute/i18n.rkt" "../compute/mac-cf.rkt" "../compute/hosts-file.rkt"
         "../components/mac-media.rkt" "../components/inspector.rkt" racket/file net/base64 racket/system
         "../compute/catalog.rkt" "../compute/session-rows.rkt" "../compute/contrast.rkt" "../compute/palette.rkt"
         "../compute/changeset.rkt" "../components/review.rkt" "../components/tests-panel.rkt" "../compute/memory-store.rkt" "../compute/host-index-store.rkt" "../compute/memory-auto.rkt")

(define-runtime-path SERVE-RKT "../../serve.rkt")
(define-runtime-path FIXTURES "../../../tests/fixtures")       ; 冒烟用的媒体样本（--synth-media）
;; 启动时的环境变量（冒烟脚本里起的子进程用；内核 place 里的 putenv 是进程级的）
(define initial-env (environment-variables-copy (current-environment-variables)))

;; 一窗多主机：每个内核连接一台「主机」（"local" = 本机守护进程 / place；其余 = 服务器等）。
;; 主机自己的状态：内核报的 ready / 退出码、模型菜单与设置页要的数据（切换主机时重新填进组件）、会话列表
(struct hs (id label config
            [ready #:mutable] [core-exit #:mutable] [replayed? #:mutable] [history #:mutable]
            [providers #:mutable] [aliases #:mutable] [budget #:mutable] [wire #:mutable] [permission #:mutable]
            [rules #:mutable] [pstate #:mutable] [sessions #:mutable]
            ;; 断线重连：retry-at 毫秒时刻或 #f；attempts 连续失败次数；manual? 用户手动断开（不自动重连）
            [retry-at #:mutable] [attempts #:mutable] [manual? #:mutable]))
(define (make-hs id label config) (hs id label config #f #f #f #f #f #f #f #f #f #f #f '() #f 0 #f))
(define RECONNECT-MAX 8)                                 ; 连续失败这么多次就停（显示「点击重新连接」）
(define (reconnect-delay-ms attempts) (* 1000.0 (min 30 (expt 2 attempts))))   ; 1 2 4 8 16 30 30 30 秒
(define (hash-merge a b) (for/fold ([a (or a (hasheq))]) ([(k v) (in-hash b)]) (hash-set a k v)))
;; route_status / state.route：替换 providers 缓存里同名的池（与模型菜单 set-route! 同一规则）
(define (route-into providers rs)
  (define opts (or providers (hasheq)))
  (define others (filter (lambda (r) (not (equal? (and (hash? r) (hash-ref r 'name #f)) (hash-ref rs 'name #f)))) (hash-ref opts 'routes '())))
  (hash-set opts 'routes (append others (list rs))))

;; 服务器主机（经 ssh）：主机 id = 目标（serve 名字不是 default 时加 ":名字"）；远端命令可用 PI2_REMOTE_CMD 覆盖
(define (ssh-host-spec target name)
  (define id (if (equal? name "default") target (string-append target ":" name)))
  (define cmd (string-append (or (getenv "PI2_REMOTE_CMD") "racket -l pi2/src/serve -- attach --spawn")
                             (if (equal? name "default") "" (string-append " --name " name))))
  (list id id (hasheq 'command (list "ssh" "-T" "-o" "BatchMode=yes" target cmd) 'ssh target 'serve-name name)))

;; 本机守护进程的名字：按工作目录一个（目录名 + 路径的 FNV-1a 32 位）；名字只许 [A-Za-z0-9_.-]
(define (local-serve-name dir)
  (define p (path->string (simplify-path (path->complete-path dir))))
  (define h (for/fold ([h 2166136261]) ([b (in-bytes (string->bytes/utf-8 p))])
              (bitwise-and (* (bitwise-xor h b) 16777619) #xFFFFFFFF)))
  (define base (let-values ([(_b n _d) (split-path (string->path p))])
                 (regexp-replace* #rx"[^A-Za-z0-9_.-]" (if (path? n) (path->string n) "root") "_")))
  (format "local-~a-~a" (substring base 0 (min 24 (string-length base))) (number->string h 16)))

(define (racket-exe-path)
  (define p (find-system-path 'exec-file))
  (path->string (or (and (absolute-path? p) p) (find-executable-path p) p)))

(define CLEAR '(0.09 0.09 0.11 1.0))
(define DIM-COLOR DIM)                                  ; 次要文字（compute/palette.rkt）
(define PAD 20.0)

;; ---------------------------------------------------------------- 配置组件

;; 字体后端：macOS 用 Core Text（计算层），以 font-backend 接口放进 env
(define (mac-font-backend)
  (define be (make-coretext-backend))
  (implement font-backend 'font (text-backend-font be) 'shape (text-backend-shape be)
             'metrics (text-backend-metrics be) 'rasterize (text-backend-rasterize be))
) ; end define mac-font-backend

;; kind：'window（带窗口与可选内核）/ 'offscreen（自检、快照）/ 'replay（回放：操作日志充当窗口 + 内核，离屏渲染）。
;; kernel-config：#f = 不起内核；journal-config：日志组件配置（'dir #f = 只在内存）；replay-config：回放组件配置
(define (configure kind #:kernel [kernel-config #f] #:title [title "pi2"]   ; kernel-config：真 = 要连内核（主机表非空）
                   #:journal [journal-config (hasheq)] #:replay [replay-config (hasheq)])
  (define comps
    (append (list renderer-component text-system-component input-component transcript-component sidebar-component media-component inspector-component review-component tests-panel-component
                  model-picker-component journal-component debug-panel-component settings-component)
            (if (eq? kind 'window) (list window-component) '())
            (if (eq? kind 'replay) (list replay-host-component) '())
            (if (and kernel-config (eq? kind 'window)) (list kernels-component) '())))
  (define-values (env _order)
    (assemble comps
              #:env (hasheq 'font-backend (mac-font-backend))
              #:config (hasheq 'renderer (hasheq 'layer? (eq? kind 'window))
                               'window (hasheq 'title title 'width 960.0 'height 680.0 'glass? #t
                                                     'dark? (not (equal? (getenv "PI2_APPEARANCE") "system")))   ; 缺省强制暗色；system = 跟系统（只为对照测量）   ; 建好玻璃面板与覆盖层（用不用由设置决定）
                               'journal journal-config
                               'media (hasheq 'audio? (eq? kind 'window))     ; 回放 / 离屏不出声
                               'replay-host replay-config)))
  env
) ; end define configure

;; ---------------------------------------------------------------- 联系：一帧画面

;; 侧栏（左或右，可隐藏，可拖分隔线改宽度）+ 对话区（上）+ 输入框（下，底边锚定）+ 状态行
(define SIDEBAR-W 240.0)
(define SIDEBAR-MIN-W 180.0)
(define SIDEBAR-MAX-W 480.0)
(define MAIN-MIN-W 420.0)                               ; 对话区至少留这么宽
(define SIDEBAR-MIN-WINDOW 760.0)                       ; 窗口比这窄时自动隐藏侧栏
(define GLASS-INSET 8.0)                                ; 液态玻璃侧栏：面板与窗口边、对话区之间的间距
(define GLASS-RADIUS 16.0)                              ; 面板圆角
(define CLEAR-TRANSPARENT '(0.0 0.0 0.0 0.0))           ; 透出桌面：主层清成透明，对话区自己画底色
(define GLASS-WORST '(0.36 0.36 0.38))                  ; 暗色玻璃最亮的实测（后面是白的，§28）：逐帧检查时玻璃上的侧栏按它算

(define (clamp-sidebar-w sw w) (max SIDEBAR-MIN-W (min SIDEBAR-MAX-W (- w MAIN-MIN-W) (if (real? sw) sw SIDEBAR-W))))
(define (sidebar-width sidebar? w [sw SIDEBAR-W]) (if (and sidebar? (>= w SIDEBAR-MIN-WINDOW)) (clamp-sidebar-w sw w) 0.0))

;; → (values 侧栏 x 侧栏宽 主区 x 主区宽 分隔线 x 或 #f)
(define (layout-columns sidebar? right? sw w)
  (define sbw (sidebar-width sidebar? w sw))
  (define sbx (if right? (- w sbw) 0.0))
  (values sbx sbw (if right? 0.0 sbw) (- w sbw) (and (> sbw 0.0) (if right? sbx sbw))))

(define (paint-scene! dl tl ib tr sbp mpp w h scale status #:focused? [focused? #t] #:caret-on? [caret-on? #t] #:sidebar? [sidebar? #t]
                      #:sidebar-right? [right? #f] #:sidebar-w [sw SIDEBAR-W] #:divider-hot? [hot? #f]
                      #:sidebar-dl [sdl #f]                                ; 液态玻璃：侧栏画进这个（覆盖层）
                      #:glass-layout? [glass-layout? (and sdl #t)]          ; 按玻璃面板排（内缩 GLASS-INSET）；回放时没有玻璃也按录制时的排
                      #:hole [hole #f]                                      ; 液态玻璃（透出桌面）：(vector x y w h 圆角)——主层清成透明，
                                                                            ;   这块以外铺底色、这块（圆角洞）留空，玻璃在洞上采样窗口后面
                      #:under [under #f]                                    ; 显示检查：侧栏那一列先画这个颜色（代替窗口后面的桌面，玻璃在窗口内采样它）
                      #:debug [dv #f] #:debug-stats [debug-stats '()] #:settings [svp #f] #:inspector [insp #f] #:review [rvw #f] #:tests [tpv #f])
  (define-values (sbx sbw lx mw dx) (layout-columns sidebar? right? sw w))
  (when (and under (> sbw 0.0)) (drawlist-rect! dl sbx 0.0 sbw h (car under) (cadr under) (caddr under) 1.0))
  (when hole
    (define-values (hx hy hw hh hr) (values (vector-ref hole 0) (vector-ref hole 1) (vector-ref hole 2) (vector-ref hole 3) (vector-ref hole 4)))
    (define-values (cr cg cb) (values (car CLEAR) (cadr CLEAR) (caddr CLEAR)))
    (define (strip! x y sw sh) (when (and (> sw 0.0) (> sh 0.0)) (drawlist-rect! dl x y sw sh cr cg cb 1.0)))
    (strip! 0.0 0.0 w hy) (strip! 0.0 (+ hy hh) w (- h hy hh))                   ; 洞的上、下
    (strip! 0.0 hy hx hh) (strip! (+ hx hw) hy (- w hx hw) hh)                   ; 洞的左、右
    (when (> hr 0.0) (drawlist-hole! dl hx hy hw hh cr cg cb 1.0 #:radius hr)))  ; 洞的四个圆角外
  (when (> sbw 0.0)
    (if glass-layout?
        (sbp 'render! (or sdl dl) (+ sbx GLASS-INSET) GLASS-INSET (- sbw (* 2 GLASS-INSET)) (- h (* 2 GLASS-INSET)) scale)
        (sbp 'render! (or sdl dl) sbx 0.0 sbw h scale)))
  ;; 分隔线（拖它改侧栏宽度；悬停时亮）。玻璃面板有自己的边：只在悬停 / 拖动时画
  (when dx (cond [hot? (drawlist-rect! dl (- dx 1.5) 0.0 3.0 h 0.36 0.50 0.85 1.0)]
                 [(not glass-layout?) (drawlist-rect! dl (- dx 0.5) 0.0 1.0 h 0.20 0.21 0.25 1.0)]
                 [else (void)]))
  (define input-top (ib 'render! dl (+ lx PAD) (- h PAD 18.0) (- mw (* 2 PAD)) scale focused? caret-on?))
  (tr 'render! dl (+ lx PAD) PAD (- mw (* 2 PAD)) (max 40.0 (- input-top 12.0 PAD)) scale)
  ;; 状态行：左端模型芯片（打开时菜单盖在对话区 / 输入框上，所以最后画），右边状态文字
  (define sy (- h PAD (text-block-line-height (tl 'layout "M" 'small 100.0)) -6.0))
  (define cw (mpp 'render! dl (+ lx PAD) sy (* 0.6 (- mw (* 2 PAD))) h scale))
  (when status
    (define sx (+ lx PAD (if (> cw 0.0) (+ cw 10.0) 0.0)))
    (define tb (tl 'layout status 'small (max 40.0 (- (+ lx mw) PAD sx))))
    (paint-text! dl (tl 'atlas) tb sx sy DIM-COLOR #:scale scale))
  (when insp (insp 'render! dl lx 0.0 mw h scale))                           ; 信息面板：盖在对话区上（不盖侧栏）
  (when rvw (rvw 'render! dl lx 0.0 mw h scale))                             ; 变更清单的审阅面板：同上
  (when tpv (tpv 'render! dl lx 0.0 mw h scale))                             ; 工作区测试流程面板：同上
  (when svp (svp 'render! dl 0.0 0.0 w h scale))                              ; 设置页（⌘,）：盖在整个窗口上
  (when (and dv (dv 'open?))                                                  ; 调试面板盖在最上层（右侧）
    (define pw (min 640.0 (* 0.62 w)))
    (dv 'render! dl (- w pw 10.0) 10.0 pw (- h 20.0) scale debug-stats))
  input-top
) ; end define paint-scene!

;; 文字系统 atlas 脏区 → 渲染
(define (upload-atlas! g tl [mdp #f])
  (define d (tl 'take-dirty!))
  (when d (g 'upload-atlas! (car d) (cdr d)))
  (define u (and mdp (mdp 'take-upload!)))                ; 图片纹理池（缩略图）
  (when u (g 'upload-images! (car u) (cdr u)))
) ; end define upload-atlas!

(define TRACE-SAMPLE
  (string-append "Traceback (most recent call last):\n"
                 (apply string-append
                        (for/list ([i 30]) (format "  File \"/srv/app/handlers/step~a.py\", line ~a, in run_step~a\n    result = pipeline.process(batch, retries=~a)\n" i (+ 40 (* 7 i)) i (modulo i 4))))
                 "ValueError: batch 17 has inconsistent schema: expected 12 columns, got 11\n"))

;; ---------------------------------------------------------------- --selftest

(define (pixel-reader px pw)
  (lambda (x y) (for/list ([k 4]) (bytes-ref px (+ (* (+ (* y pw) x) 4) k))))
) ; end define pixel-reader

;; 已知场景 → 离屏读回 → 逐点比对。返回失败描述列表。
(define (selftest png-path)
  (define env (configure 'offscreen))
  (define g (env-ref env gpu-port))
  (define tl (env-ref env text-layout))
  (define clear-bgra (list (round (* 255 0.11)) (round (* 255 0.09)) (round (* 255 0.09)) 255))
  (define (near? a b) (andmap (lambda (u v) (<= (abs (- u v)) 2)) a b))
  ;; 1. 矩形（scale 1）+ 裁剪
  (define dl (make-drawlist))
  (drawlist-rect! dl 10 10 100 50 1.0 0.0 0.0 1.0)
  (drawlist-rect! dl 150 10 100 100 0.0 1.0 0.0 0.5 #:radius 30.0)
  (define pix (pixel-reader (g 'render-offscreen dl CLEAR 300 200 1.0) 300))
  (define blend (list (round (* 0.5 (first clear-bgra))) (round (+ 127.5 (* 0.5 (second clear-bgra))))
                      (round (* 0.5 (third clear-bgra))) 255))
  (define cdl (make-drawlist))
  (drawlist-set-clip! cdl (vector 0.0 0.0 100.0 200.0))
  (drawlist-rect! cdl 0 150 200 40 0.0 0.0 1.0 1.0)
  (drawlist-set-clip! cdl #f)
  (drawlist-rect! cdl 250 150 20 20 0.0 0.0 1.0 1.0)
  (drawlist-hole! cdl 200 20 80 80 1.0 1.0 1.0 1.0 #:radius 30.0)                ; 挖洞：角上填色、中间不画
  (define cpix (pixel-reader (g 'render-offscreen cdl CLEAR 300 200 1.0) 300))
  (define rect-checks
    (list (list "clip: inside kept (50,170)" (near? (cpix 50 170) '(255 0 0 255)))
          (list "clip: outside cut (150,170)" (near? (cpix 150 170) clear-bgra))
          (list "clip: after reset unclipped (260,160)" (near? (cpix 260 160) '(255 0 0 255)))
          (list "hole: corner filled (202,22)" (near? (cpix 202 22) '(255 255 255 255)))
          (list "hole: center left clear (240,60)" (near? (cpix 240 60) clear-bgra))
          (list "hole: straight edge mid stays clear (240,22)" (near? (cpix 240 22) clear-bgra))
          (list "background (5,190)" (near? (pix 5 190) clear-bgra))
          (list "red rect center" (near? (pix 60 35) '(0 0 255 255)))
          (list "red rect just outside" (near? (pix 112 35) clear-bgra))
          (list "rounded rect center blend" (near? (pix 200 60) blend))
          (list "rounded rect corner cut" (near? (pix 152 12) clear-bgra))))
  ;; 2. 文字（scale 2）：拉丁 + CJK（回退）+ 彩色 emoji
  (define s "Hello 你好 🚀")
  (define tb (tl 'layout s 'title 380.0))
  (define tdl (make-drawlist))
  (tl 'build-frame! tdl (lambda () (paint-text! tdl (tl 'atlas) tb 10.0 10.0 '(1.0 1.0 1.0 1.0) #:scale 2.0)))
  (upload-atlas! g tl)
  (define tw 800)
  (define th 200)
  (define tpx (g 'render-offscreen tdl CLEAR tw th 2.0))
  (define tpix (pixel-reader tpx tw))
  (when png-path (write-png png-path tw th tpx) (printf "wrote ~a\n" png-path))
  (define ln (first (text-block-lines tb)))
  (define (glyph-span cp)
    (define gs (line-glyphs ln))
    (define idx (for/first ([gx gs] [i (in-naturals)] #:when (= (sglyph-cluster (car gx)) cp)) i))
    (define x0 (+ 10.0 (cdr (list-ref gs idx))))
    (define x1 (if (< (add1 idx) (length gs)) (+ 10.0 (cdr (list-ref gs (add1 idx)))) (+ 10.0 (line-width ln))))
    (values (inexact->exact (floor (* 2 x0))) (inexact->exact (ceiling (* 2 x1)))))
  (define y0 (inexact->exact (floor (* 2 10.0))))
  (define y1 (inexact->exact (ceiling (* 2 (+ 10.0 (text-block-line-height tb))))))
  (define (count-in x0 x1 pred) (for*/sum ([y (in-range y0 y1)] [x (in-range x0 x1)]) (if (pred (tpix x y)) 1 0)))
  (define (ink? p) (> (apply max (take p 3)) 120))
  (define (colored? p) (> (- (apply max (take p 3)) (apply min (take p 3))) 60))
  (define-values (hx0 hx1) (glyph-span 0))
  (define-values (cx0 cx1) (glyph-span 6))
  (define-values (ex0 ex1) (glyph-span 9))
  (define below (for*/sum ([y (in-range (+ y1 4) th)] [x (in-range tw)]) (if (ink? (tpix x y)) 1 0)))
  (define text-checks
    (list (list (format "latin 'H' has white ink (~a px)" (count-in hx0 hx1 ink?)) (> (count-in hx0 hx1 ink?) 40))
          (list "latin 'H' is not colored" (zero? (count-in hx0 hx1 colored?)))
          (list (format "CJK '你' via fallback has ink (~a px)" (count-in cx0 cx1 ink?)) (> (count-in cx0 cx1 ink?) 60))
          (list (format "emoji has colored pixels (~a px)" (count-in ex0 ex1 colored?)) (> (count-in ex0 ex1 colored?) 100))
          (list (format "nothing below the text block (~a px)" below) (zero? below))))
  (printf "device: ~a; atlas glyphs: ~a; text instances: ~a\n" (g 'device-name) (tl 'glyph-count) (drawlist-count tdl))
  (for/list ([c (in-list (append rect-checks text-checks))]
             #:unless (begin (printf "  ~a ~a\n" (if (second c) "ok  " "FAIL") (first c)) (second c)))
    (first c))
) ; end define selftest

;; ---------------------------------------------------------------- --snapshot

;; 典型一屏：用户消息（带报错栈 chip）→ 助手 → 工具卡片（42 行输出，折叠）→ 助手 → 权限卡片（待答）；
;; 输入框里一条带 chip 的草稿 + 五笔组字。事件按内核协议的形状喂给对话区组件。
;; view：'top（从对话顶部看起）/ 'bottom（贴底）/ 'menu（贴底 + 模型菜单打开）/ 'settings（设置页：帐号与密钥）
(define (snapshot path w h #:view [view 'top])
  (define env (configure 'offscreen))
  (define g (env-ref env gpu-port))
  (define tl (env-ref env text-layout))
  (define ib (env-ref env input-box))
  (define tr (env-ref env transcript-port))
  (define mdp (env-ref env media-port))
  (define sbp (env-ref env session-list))
  (define mpp (env-ref env model-picker))
  (mpp 'set-options! (hasheq 'current "lmstudio" 'model "gemma-4-31b-it@6bit" 'reasoning "high"
                             'adaptations (list (hasheq 'name "gemma-tools" 'enabled #t 'source "user")
                                                (hasheq 'name "project-style" 'enabled #f 'source "project"))
                             'reasoning_levels '("off" "low" "medium" "high" "max")
                             'providers (list (hasheq 'name "lmstudio" 'has_key #t 'loaded '("gemma-4-31b-it@6bit")
                                                      'models '("qwen/qwen3.8-27b" "mellum2-12b-a2.5b-instruct-mlx" "qwen3.6-35b-a3b-abliterated-heretic-mlx"
                                                                "gemma-4-31b-it@8bit" "gemma-4-31b-it@6bit" "gemma-4-31b" "gpt-oss-120b-mlx-crack"
                                                                "glm-4.7-flash-abliterated-mxfp8" "deepseek-r1-distill-qwen-32b-abliterated"
                                                                "qwen3.5-9b-mlx" "google/gemma-3-4b" "qwen/qwen3.5-9b"))
                                              (hasheq 'name "deepseek" 'models '("deepseek-v4-flash" "deepseek-v4-pro") 'has_key #t)
                                              (hasheq 'name "deepseek[work]" 'models '("deepseek-v4-flash" "deepseek-v4-pro") 'has_key #t)
                                              (hasheq 'name "anthropic" 'models '("claude-sonnet-5") 'has_key #f))))
  (when (eq? view 'menu) (mpp 'open!))
  ;; 设置页：帐号与密钥一节，示例数据
  (define svp (env-ref env settings-view))
  (when (eq? view 'settings)
    (svp 'open!)
    (svp 'set-data! 'providers (hasheq 'providers (list (hasheq 'name "lmstudio" 'needs_key #f 'has_key #t)
                                                        (hasheq 'name "deepseek" 'needs_key #t 'has_key #t)
                                                        (hasheq 'name "deepseek[work]" 'needs_key #t 'has_key #f)
                                                        (hasheq 'name "anthropic" 'needs_key #t 'has_key #f))))
    (svp 'render! (make-drawlist) 0.0 0.0 w h 2.0)
    (svp 'hit (car (svp 'hit-center 'section 'accounts)) (cdr (svp 'hit-center 'section 'accounts)))
    (svp 'render! (make-drawlist) 0.0 0.0 w h 2.0)
    (svp 'hit (car (svp 'hit-center 'edit-key "deepseek[work]")) (cdr (svp 'hit-center 'edit-key "deepseek[work]")))
    (editor-insert! (svp 'editor) "sk-example"))
  ;; 设置页：路由池一节，添加成员选到「帐号 → 模型」那一步
  (when (eq? view 'pools)
    (svp 'open!)
    (svp 'set-data! 'providers
         (hasheq 'providers (list (hasheq 'name "lmstudio" 'needs_key #f 'has_key #t 'models '("gemma-4-31b-it@6bit" "qwen/qwen3.5-9b"))
                                  (hasheq 'name "deepseek" 'needs_key #t 'has_key #t 'models '("deepseek-v4-flash" "deepseek-v4-pro"))
                                  (hasheq 'name "deepseek[work]" 'needs_key #t 'has_key #t 'models '("deepseek-v4-flash" "deepseek-v4-pro")))
                 'routes (list (hasheq 'name "team" 'current 1
                                       'members (list (hasheq 'instance "deepseek" 'model "deepseek-v4-pro" 'state "cooling")
                                                      (hasheq 'instance "deepseek[work]" 'model "deepseek-v4-pro" 'state "ok"))))))
    (define (click! kind payload) (svp 'render! (make-drawlist) 0.0 0.0 w h 2.0)
                                  (let ([c (svp 'hit-center kind payload)]) (svp 'hit (car c) (cdr c))))
    (click! 'section 'pools)
    (click! 'add-member "team")
    (click! 'pick-account (vector "team" "lmstudio")))
  (sbp 'set-sessions! (list (if (eq? view 'sidebar-right)          ; 'sidebar-right：当前是刚新建的临时会话
                                (hasheq 'path "/d/new.rktd" 'title 'null 'ts "2026-10-25T04:30:00" 'messages 0 'model "deepseek-v4-flash" 'current #t 'ephemeral #t)
                                (hasheq 'path "/d/a0.rktd" 'title "调试 serve 守护进程" 'ts "2026-10-22T18:00:00" 'messages 3 'model "deepseek-v4-flash" 'current #f))
                            (hasheq 'path "/d/a.rktd" 'title "这个报错怎么回事" 'ts "2026-10-22T17:41:00" 'messages 8 'model "deepseek-v4-flash" 'current (not (eq? view 'sidebar-right)))
                            (hasheq 'path "/d/b.rktd" 'title "给 loader 加 region_code 列的单元测试" 'ts "2026-10-22T10:02:00" 'messages 23 'model "deepseek-v4-pro" 'current #f)
                            (hasheq 'path "/d/c.rktd" 'title "重构 transcript 组件的高度缓存" 'ts "2026-10-21T21:15:00" 'messages 41 'model "gemma-4-31b-it@6bit" 'current #f)
                            (hasheq 'path "/d/d.rktd" 'title 'null 'ts "2026-10-21T08:00:00" 'messages 0 'model "gemma-4-31b-it@6bit" 'current #f)))
  (define sbx0 (if (eq? view 'sidebar-right) (- w 300.0) 0.0))
  (sbp 'render! (make-drawlist) sbx0 0.0 (if (eq? view 'sidebar-right) 300.0 SIDEBAR-W) h 2.0)
  (sbp 'hover! (+ sbx0 100.0) (+ 40.0 46.0 10.0))                 ; 悬停第二行
  (unless (eq? view 'sidebar-right) (sbp 'context! 120.0 (+ 40.0 92.0 24.0)))   ; 第三行右键：上下文菜单（删除只在这里）
  (define sent (make-editor))
  (editor-insert! sent "这个报错怎么回事 ")
  (editor-paste! sent TRACE-SAMPLE)
  (define r (editor-submit! sent))
  (tr 'note! (t 'app.connected "deepseek" "deepseek-v4-flash" "2026-10-22-4821.rktd"))
  (tr 'add-user! (snap-text (cdr r)) (for/list ([id (snap-chips (cdr r))]) (editor-fold sent id)))
  (define md-answer (string-append
    "## 结论\n\n"
    "第 17 批少了 `region_code` 列，原因是 **loader 的列映射**没跟上 *schema v12*。修复见 [PR #418](https://example.dev/pr/418)，~~旧方案~~已废弃。\n\n"
    "### 修改步骤\n\n"
    "1. 在 `COLUMNS` 末尾补上新列：\n"
    "   ```python\n   COLUMNS = BASE + [\"region_code\"]\n   ```\n"
    "2. 重新跑校验：\n"
    "   - 单元测试 `pytest tests/test_loader.py`\n"
    "   - 回放第 17 批\n"
    "3. 观察一天的入库量\n\n"
    "| 批次 | 列数 | 状态 |\n|:---|---:|:-:|\n| 16 | 12 | ✓ 正常 |\n| 17 | **11** | ✗ 缺列 |\n| 18 | 12 | ✓ 正常 |\n\n"
    "> 注意：`region_code` 在旧数据里为空，下游报表需要容忍 `NULL`。\n\n"
    "---\n\n"
    "完整的回放脚本（较长，默认折叠）：\n\n"
    "```python\n"
    (apply string-append (for/list ([i 40]) (format "replay_batch(~a, strict=True)  # step ~a\n" (+ 100 i) i)))
    "```"))
  ;; 内核事件（RPC v3）：会话记录投影出的条目 + 流式 delta；用户消息的条目接管本地那条（保留 chip）
  (define (item id kind . kvs) (apply hasheq 'type "item" 'id id 'kind kind kvs))
  (for ([ev (list (hasheq 'type "turn_start" 'kind "prompt")
                  (item 1 "user" 'parts (list (hasheq 'type "text" 'text (string-append "这个报错怎么回事 " TRACE-SAMPLE))) 'synthetic #f)
                  (hasheq 'type "delta" 'kind "thinking" 'text "报错在 step17：列数 11 ≠ 12。\n先看 schema 定义。")
                  (item 2 "assistant" 'parts (list (hasheq 'type "thinking" 'text "报错在 step17：列数 11 ≠ 12。\n先看 schema 定义。")
                                                   (hasheq 'type "text" 'text "先确认一下第 17 批的列。")
                                                   (hasheq 'type "tool_call" 'id "t1" 'name "bash" 'input (hasheq 'command "python3 inspect_batch.py 17 --columns"))))
                  (item 3 "tool" 'call_id "t1" 'name "bash" 'ms 412 'error #f 'source "builtin")
                  (item 4 "results" 'parts (list (hasheq 'type "tool_result" 'call_id "t1" 'is_error #f 'truncated #f 'total_len 1000
                                                         'content (string-join (for/list ([i 42]) (format "col_~a  dtype=~a  nulls=~a" i (if (even? i) "int64" "string") (modulo (* i 7) 5))) "\n"))))
                  (item 5 "assistant" 'parts (list (hasheq 'type "tool_call" 'id "t2" 'name "edit_file" 'input (hasheq 'path "loader/columns.py" 'old_string "BASE" 'new_string "BASE + [...]"))))
                  (item 6 "tool" 'call_id "t2" 'name "edit_file" 'ms 3 'error #f 'source "builtin"
                        'diff (hasheq 'path "loader/columns.py" 'added 3 'removed 1 'truncated #f
                                      'text (string-join '("@@ -10,7 +10,9 @@" " BASE = [" "     \"batch_id\", \"ts\", \"amount\"," " ]"
                                                           "-COLUMNS = BASE" "+# schema v12：新增 region_code（旧数据为空）"
                                                           "+COLUMNS = BASE + [\"region_code\"]" "+NULLABLE = {\"region_code\"}" " "
                                                           " def load(path):" "     rows = read_rows(path)") "\n")))
                  (item 7 "results" 'parts (list (hasheq 'type "tool_result" 'call_id "t2" 'is_error #f 'truncated #f 'total_len 60
                                                         'content "edited loader/columns.py: 1 replacement(s) across 1 edit(s)")))
                  (item 8 "assistant" 'parts (list (hasheq 'type "text" 'text md-answer)))
                  (item 9 "usage" 'model "deepseek-v4-flash" 'input 1830 'output 412 'cost 0.0004)
                  (hasheq 'type "turn_complete" 'turn 1 'interrupted #f 'outcome "ok" 'usage (hasheq 'input 1830 'output 412) 'cost_usd 0.0004))])
    (tr 'apply! ev))
  ;; 'media：一条带图片 / 音频 / 文件的用户消息（图片字节取自 PI2_SNAPSHOT_IMAGE，缺省一张 2×2 的 PNG），等解码子进程出缩略图
  (when (eq? view 'media)
    (define img (let ([p (getenv "PI2_SNAPSHOT_IMAGE")])
                  (if (and p (file-exists? p)) (file->bytes p)
                      (base64-decode #"iVBORw0KGgoAAAANSUhEUgAAAAIAAAACCAYAAABytg0kAAAAEUlEQVR4nGP4z8DwH4QZYAwAR8oH+WdZbrcAAAAASUVORK5CYII="))))
    (define sha (apply string-append (for/list ([b (in-bytes (sha256-bytes img))]) (string-append (if (< b 16) "0" "") (number->string b 16)))))
    (tr 'add-user! "这张截图里的设置对吗？" '()
        (list (hasheq 'kind "image" 'media_type "image/png" 'sha sha 'name "settings.png" 'meta (hasheq 'width 1920 'height 1360) 'bytes img)
              (hasheq 'kind "audio" 'media_type "audio/mp4" 'sha (make-string 64 #\a) 'name "voice-memo.m4a" 'meta (hasheq 'duration 12.4))
              (hasheq 'kind "file" 'media_type "application/pdf" 'sha (make-string 64 #\b) 'name "spec.pdf" 'meta (hasheq 'bytes 482113))))
    (let loop ([k 0]) (unless (or (eq? (mdp 'status sha) 'ready) (> k 500)) (sleep 0.01) (loop (add1 k))))
    (mdp 'begin-frame!))
  (define e (ib 'editor))
  (editor-insert! e "顺便看下这个日志 ")
  (editor-paste! e (apply string-append (for/list ([i 30]) (format "2026-10-22 16:0~a:00 INFO loader batch ~a ok\n" (modulo i 10) i))))
  (editor-insert! e " ")
  (editor-set-marked! e "wq" 2)
  (define dl (make-drawlist))
  (define right? (eq? view 'sidebar-right))
  (tl 'build-frame! dl (lambda () (paint-scene! dl tl ib tr sbp mpp w h 2.0 "" #:sidebar-right? right? #:sidebar-w (if right? 300.0 SIDEBAR-W))))   ; 先量一遍：滚动范围要有内容高度
  (when (eq? view 'top) (tr 'scroll! 100000.0))
  ;; 'info：信息面板（示例统计）
  (define insp (env-ref env inspector-view))
  (when (eq? view 'info)
    (insp 'open! (hasheq 'path "/Users/me/pi2/data/20261022-1741-0815.rktd" 'title "这个报错怎么回事" 'created "2026-10-22T17:41:00" 'last_active "2026-10-22T18:05:12"
                         'workdir "/Users/me/pi2" 'model "deepseek-v4-flash" 'bytes 48213 'index_bytes 1504 'records 46 'messages 8 'user_messages 2 'compactions 0
                         'requests 5 'cost 0.0031
                         'models (list (hasheq 'model "deepseek-v4-flash" 'requests 4 'input 9120 'output 1404 'cost 0.0019)
                                       (hasheq 'model "deepseek-v4-pro" 'requests 1 'input 3010 'output 402 'cost 0.0012))
                         'tools (list (hasheq 'name "bash" 'calls 3 'ms 1480 'errors 1 'source "builtin") (hasheq 'name "edit_file" 'calls 1 'ms 3 'errors 0 'source "builtin"))
                         'permissions (hasheq 'yes 3 'always 1) 'notes (hasheq 'route_switch 1) 'media (hasheq 'count 0 'bytes 0)
                         'subagents '() 'forked_from 'null 'effects 0)))
  (tl 'build-frame! dl (lambda () (paint-scene! dl tl ib tr sbp mpp w h 2.0 (string-append (t 'app.idle) " · " (t 'app.turns 1)) #:settings svp
                                                #:sidebar-right? right? #:sidebar-w (if right? 300.0 SIDEBAR-W) #:divider-hot? right? #:inspector insp)))
  (upload-atlas! g tl mdp)
  (mdp 'close!)
  (define pw (inexact->exact (* 2 w)))
  (define ph (inexact->exact (* 2 h)))
  (write-png path pw ph (g 'render-offscreen dl CLEAR pw ph 2.0))
  (printf "wrote ~a (~ax~a px, ~a instances, ~a segments, ~a atlas glyphs)\n" path pw ph (drawlist-count dl)
          (length (drawlist-segments dl)) (tl 'glyph-count))
) ; end define snapshot

;; ---------------------------------------------------------------- 窗口：联系 + 驱动

;; 界面缩放（字号）：版面按「逻辑尺寸」= 窗口尺寸 / zoom 排，渲染与字形光栅化用 scale × zoom（字形清晰，不是位图放大）；
;; 窗口事件的坐标除以 zoom，输入法候选框位置乘回去。存 gui.rktd 'zoom；操作日志的 start 记录记下，回放用同一个。
(define ZOOMS ZOOM-LEVELS)
(define (valid-zoom z) (if (and (real? z) (memv z ZOOMS)) z 1.0))
(define (zoom-step z dir)
  (define i (or (for/first ([x (in-list ZOOMS)] [k (in-naturals)] #:when (= x z)) k) 2))
  (list-ref ZOOMS (max 0 (min (sub1 (length ZOOMS)) (+ i dir)))))

;; 输 key 时日志不记这些事件（连长度都不留）：上屏文字、组字、粘贴、不带 ⌘ 的原始按键（带字符和硬件键码，能还原 key）。
;; 改为在下一条照常记录的事件之前记一条 ev:secret（句柄文本），回放用它填输入框
;; add_key 请求进日志时 token 换成句柄文本；已经是句柄（回放时输入框里填的就是句柄）就原样
(define (secret-or-handle s)
  (if (and (string? s) (regexp-match? #rx"^⟦secret:[0-9a-f]+⟧$" s)) s (secret-text s)))

(define (secret-input-event? ev)
  (or (ev:text? ev) (ev:marked? ev) (ev:paste? ev) (and (ev:key? ev) (not (memq 'cmd (ev:key-mods ev))))))

;; 界面语言设置 → 语言：'system 看系统首选语言列表
(define (resolve-lang setting)
  (case setting
    [(zh en) setting]
    [else (or (for/or ([tag (in-list (preferred-languages))]) (lang-from-tag tag)) 'zh)]))

(define BLINK-MS 530.0)

(define (run-window #:hosts [host-specs '()] #:active [active0 #f] #:exit-after [exit-after #f] #:resize-to [resize-to #f]
                    #:synth? [synth? #f] #:synth-core? [synth-core? #f] #:synth-sessions? [synth-sessions? #f]
                    #:synth-picker? [synth-picker? #f] #:synth-route? [synth-route? #f] #:synth-settings? [synth-settings? #f]
                    #:synth-secret? [synth-secret? #f] #:synth-reattach? [synth-reattach? #f] #:synth-settings2? [synth-settings2? #f]
                    #:synth-multihost? [synth-multihost? #f] #:synth-hosts? [synth-hosts? #f] #:synth-reconnect? [synth-reconnect? #f]
                    #:synth-spend? [synth-spend? #f] #:synth-small? [synth-small? #f] #:synth-media? [synth-media? #f] #:synth-layout? [synth-layout? #f] #:synth-sessmenu? [synth-sessmenu? #f] #:synth-chat? [synth-chat? #f] #:synth-visual? [synth-visual? #f] #:synth-rightclick? [synth-rightclick? #f] #:synth-organize? [synth-organize? #f] #:synth-unicode? [synth-unicode? #f] #:synth-instructions? [synth-instructions? #f] #:synth-memory? [synth-memory? #f] #:synth-hostindex? [synth-hostindex? #f] #:synth-automem? [synth-automem? #f] #:synth-tests? [synth-tests? #f] #:synth-history? [synth-history? #f]
                    #:synth-live [live-prompt #f] #:allow-rx [allow-rx #f] #:ime-real [ime-real #f] #:snapshot-at-exit [snap-path #f]
                    #:journal-dir [journal-dir #f] #:replay [replay-path #f])
  (install-uncaught-exception-reporter!
   (lambda (name reason) (eprintf "pi2 GUI abort: uncaught NSException ~a: ~a\n" name reason)))
  ;; 回放：日志的 'start 记录给出录制时的窗口尺寸
  (define-values (replay-header replay-records)
    (if replay-path (read-journal replay-path) (values #f '())))
  (define replay-start (for/first ([r (in-list replay-records)] #:when (eq? (third r) 'start)) (fourth r)))
  ;; 界面语言影响文字宽度 → 命中位置：回放用录制时的语言
  (let ([l (and replay-start (hash-ref replay-start 'lang #f))])
    (when (and (string? l) (memq (string->symbol l) LANGS)) (current-lang (string->symbol l))))
;; 偏好：回放时从 start 记录里录制时的值开始、只在内存里改（不读写用户的偏好文件，回放才与录制一致）
  (define settings-overlay
    (and replay-path (make-hash (hash->list (hash-ref (or replay-start (hasheq)) 'settings (hasheq))))))
  (define (gsetting k [d #f]) (if settings-overlay (hash-ref settings-overlay k d) (hash-ref (load-gui-settings) k d)))
  (define (save-setting! k v) (if settings-overlay (hash-set! settings-overlay k v) (save-gui-setting! k v)))
  (define SETTINGS-IN-JOURNAL '(combined-daily-usd combined-snooze-date secret-mode tool-preview stream-checkpoint sidebar-side sidebar-width sidebar-material sidebar-older-days sidebar-group hidden-text-mode memory-global-auto host-index-paused host-index-model host-index-roots memory-auto))   ; 回放从这些值开始（影响显示 / 命中）
  (define zoom (valid-zoom (if replay-path (hash-ref (or replay-start (hasheq)) 'zoom 1.0) (hash-ref (load-gui-settings) 'zoom 1.0))))
  (define journal-config (hasheq 'dir journal-dir
                                 'header (hasheq 'app "pi2-gui" 'argv (vector->list (current-command-line-arguments))
                                                 'started (current-seconds) 'replay-of (and replay-path (format "~a" replay-path)))))
  (define env (if replay-path
                  (configure 'replay #:journal journal-config
                             #:replay (hasheq 'records replay-records 'header (or replay-start (hasheq))))
                  (configure 'window #:kernel (pair? host-specs) #:journal journal-config)))
  (define jr (env-ref env journal))
  (define dv (env-ref env debug-view))
  (define svp (env-ref env settings-view))
  (define rs (and replay-path (env-ref env replay-source)))
  (define g (env-ref env gpu-port))
  (define win (env-ref env window-port))
  (define clip (env-ref env clipboard))
  (define wi (env-ref env window-input))
  (define tl (env-ref env text-layout))
  (define ib (env-ref env input-box))
  (define tr (env-ref env transcript-port))
  (define mdp (env-ref env media-port))                           ; 缩略图 / 音频（沙箱子进程）
  (define insp (env-ref env inspector-view))                      ; 会话信息面板
  (define rvp (env-ref env review-view))                          ; 变更清单的审阅面板（§11.7 C）
  (define tpp (env-ref env tests-view))                           ; 工作区测试流程面板（design-goalmode.md §12 T2）
  (define tests-wanted? #f)                                       ; 用户刚要打开：等内核的 test_flow 数据
  (tr 'set-preview! (gsetting 'tool-preview #f))                 ; 工具输出折叠预览（设置页「工具输出」）
  ;; ---- 主机（一窗多主机）。回放：主机表取录制时的（start 记录），不连真内核
  (define specs (if replay-path
                    (for/list ([x (in-list (hash-ref (or replay-start (hasheq)) 'hosts (list (hasheq 'id "local" 'label "local"))))])
                      (list (hash-ref x 'id) (hash-ref x 'label (hash-ref x 'id)) #f))
                    host-specs))
  (define ks (and (or (pair? specs) replay-path) (env-ref env kernel-set)))   ; 回放：总有（请求要记下来比对）
  (define host-tab (make-hash))
  (define host-order '())
  (define active (or (and replay-path (hash-ref (or replay-start (hasheq)) 'active #f)) active0 (if (pair? specs) (car (car specs)) "local")))
  (define (add-host! id label cfg)
    (unless (hash-ref host-tab id #f)
      (hash-set! host-tab id (make-hs id label cfg))
      (set! host-order (append host-order (list id))))
    (when ks (ks 'add! id cfg)))
  (for ([sp (in-list specs)]) (add-host! (car sp) (cadr sp) (caddr sp)))
  (unless (hash-ref host-tab active #f) (hash-set! host-tab active (make-hs active active #f)))
  (define (A) (hash-ref host-tab active))
  (define (H id) (hash-ref host-tab id #f))
  (define (multi?) (> (length host-order) 1))
  (tr 'use-doc! active)
  ;; 原来的单内核变量 = 当前主机的字段（脚本、状态行沿用这些名字）
  (define-syntax-rule (define-host-field name getter setter)
    (define-syntax name
      (make-set!-transformer
       (lambda (stx)
         (syntax-case stx (set!)
           [(set! _ v) #'(setter (A) v)]
           [(_ . args) #'((getter (A)) . args)]
           [_ #'(getter (A))])))))
  (define-host-field ready hs-ready set-hs-ready!)
  (define-host-field core-exit hs-core-exit set-hs-core-exit!)
  (define-host-field kernel-history hs-history set-hs-history!)          ; 冒烟：最近一次 history 应答
  (define-host-field replayed? hs-replayed? set-hs-replayed?!)           ; 无头模式：守护进程补齐对话区完毕（replay_end）
  (define-host-field last-providers hs-providers set-hs-providers!)      ; 冒烟：最近一次 providers 应答
  (define-host-field last-aliases hs-aliases set-hs-aliases!)            ; 冒烟：最近一次 aliases 应答
  (define-host-field last-budget hs-budget set-hs-budget!)               ; 今日估算花费与上限（状态行、设置页）
  (define-host-field last-wire hs-wire set-hs-wire!)                     ; 调试面板：内核报的调用格式
  (define-host-field last-permission hs-permission set-hs-permission!)   ; 冒烟：内核报的权限模式
  ;; ---- 跨主机费用合计：各主机内核报的今日花费（budget），同一本账（ledger：同一台机器同一个花费文件）只算一次
  (define (money2 x) (/ (round (* x 100)) 100.0))
  (define (host-budgets)                                 ; → ((id . budget) …)，按主机顺序，有数据的
    (for*/list ([id (in-list host-order)] [b (in-value (hs-budget (H id)))] #:when (hash? b)) (cons id b)))
  (define (unique-ledgers)                               ; → ((ledger . 花费) …)
    (for/fold ([acc '()]) ([p (in-list (host-budgets))])
      (define l (hash-ref (cdr p) 'ledger (car p)))
      (if (assoc l acc) acc (append acc (list (cons l (let ([v (hash-ref (cdr p) 'spent_today 0)]) (if (real? v) v 0))))))))
  (define (spend-total) (for/sum ([p (in-list (unique-ledgers))]) (cdr p)))
  (define (spend-hosts-count) (length (host-budgets)))
  (define (combined-cap)                                 ; GUI 设的合计每天上限；今天选过「不再询问」→ #f
    (define c (gsetting 'combined-daily-usd #f))
    (and (real? c) (> c 0)
         (not (for/or ([p (in-list (host-budgets))]) (equal? (gsetting 'combined-snooze-date #f) (hash-ref (cdr p) 'date #f))))
         c))
  ;; 推给每台：「其他主机」= 合计里去掉这台那本账；没变就不发（日期用那台自己的，时区不同也对得上）
  (define pushed-external (make-hash))
  (define (push-external!)
    (when (or (multi?) (gsetting 'combined-daily-usd #f))
      (define cap (combined-cap))
      (define ledgers (unique-ledgers))
      (for ([p (in-list (host-budgets))])
        (define id (car p))
        (define mine (hash-ref (cdr p) 'ledger id))
        (define others (for/sum ([l (in-list ledgers)] #:unless (equal? (car l) mine)) (cdr l)))
        (define msg (hasheq 'type "set_external_spend" 'others_usd (/ (round (* others 10000)) 10000.0) 'combined_cap_usd (or cap 'null)
                            'date (hash-ref (cdr p) 'date "")))
        (unless (equal? (hash-ref pushed-external id #f) msg)
          (hash-set! pushed-external id msg)
          (send! msg id)))))
  (define (push-budget-all!)                             ; 设置页「所有主机合计」块（多主机或设了合计上限时）
    (svp 'set-data! 'budget-all
         (and (or (multi?) (gsetting 'combined-daily-usd #f))
              (hasheq 'total (spend-total) 'cap (or (let ([c (gsetting 'combined-daily-usd #f)]) (and (real? c) c)) 'null)
                      'snoozed (and (gsetting 'combined-daily-usd #f) (not (combined-cap)) #t)
                      'hosts (for/list ([p (in-list (host-budgets))])
                               (hasheq 'label (hs-label (H (car p))) 'spent (hash-ref (cdr p) 'spent_today 0)
                                       'cap (hash-ref (cdr p) 'daily_usd 'null) 'ledger (hash-ref (cdr p) 'ledger "")))))))
  (define sbp (env-ref env session-list))
  (define mpp (env-ref env model-picker))
  (define sidebar? #t)
  (define mouse-x 0.0)
  (define e (ib 'editor))
  (define dl (make-drawlist))
  (define-values (pw ph ps) (win 'size))                 ; 窗口（物理）尺寸，point + backing scale
  (define w (/ pw zoom))                                 ; 版面（逻辑）尺寸：组件只看这个
  (define h (/ ph zoom))
  (define scale (* ps zoom))
  (define focused? #t)
  (define frames 0)
  (define msgs-per-frame '())
  (define event-kinds (make-hasheq))
  (define resizes '())
  (define keys '())
  (define last-mods '())
  (define drag-select? #f)
  (define drag-transcript? #f)
  (define drag-minimap? #f)                             ; 拖小地图：跟着鼠标跳
  (define chat-usage-last #f)                           ; 最近一次 chat_usage 事件（'seq：第几次）
  (define last-anim-ms 0.0)                             ; 上一次为动画重绘的时刻
  (define last-synth-ms 0.0)                             ; 冒烟脚本的保底帧（见下面 synth-due?）
  ;; 侧栏材质（设置页「通用」）：glass = 内缩的玻璃面板（玻璃下面是窗口底色）；desktop = 整列玻璃、窗口透明（透出桌面）；opaque
  ;; 覆盖层的 drawlist、上一帧的玻璃位置、窗口当前是否透明
  (define material (let ([m (gsetting 'sidebar-material 'glass)]) (if (memq m '(glass desktop opaque)) m 'glass)))
  (define replay-glass? (and replay-start (hash-ref replay-start 'glass #f) #t))   ; 录制时是否用了液态玻璃（影响侧栏排版 → 命中位置）
  (define sdl (make-drawlist))
  (define last-glass-rect 'none)
  (define last-grect #f)                                ; 这一帧的玻璃位置（截图用；#f = 没用玻璃）
  (define window-transparent? #f)
  (define test-under #f)                                ; 显示检查：透出桌面时侧栏那一列下面画的颜色（截图截不到窗口后面的东西，见 §28）
  ;; 逐帧的显示检查（冒烟流程里、或 PI2_LINT=1）：每 3 帧查一次对比度，不合格的按位置 + 颜色去重，退出时打印
  (define lint-active? #f)
  (define lint-lows (make-hash))
  (define lint-overlaps (make-hash))                    ; 字叠字（布局缺陷）：(两段的位置) → #t
  (define lint-counts (vector 0 0 0))                   ; 查了几帧、几段字、几块填充
  (define (lint-frame!)
    (define glass-base (and last-grect (lambda (_x _y) GLASS-WORST)))
    (define texts (append (contrast-report dl #:clear CLEAR #:frame-area (* w h)) (if glass-base (contrast-report sdl #:base glass-base #:frame-area (* w h)) '())))
    (define fills (append (fill-report dl #:clear CLEAR #:frame-area (* w h)) (if glass-base (fill-report sdl #:base glass-base #:frame-area (* w h)) '())))
    (vector-set! lint-counts 0 (add1 (vector-ref lint-counts 0)))
    (for ([pr (in-list (text-overlaps texts))])
      (define (pos r) (list (round (hash-ref r 'x)) (round (hash-ref r 'y)) (round (hash-ref r 'w))))
      (define k (list (pos (car pr)) (pos (cdr pr))))
      (unless (hash-ref lint-overlaps k #f)
        (let ([d (getenv "PI2_LINT_DUMP")]) (when d (snapshot-now! (build-path d (format "overlap-~a-~a.png" (vector-ref lint-counts 0) (car (pos (car pr)))))))))
      (hash-set! lint-overlaps k #t))
    (for ([r (in-list (append texts fills))] #:when (memq (hash-ref r 'status) '(ok low)))
      (if (hash-ref r 'fg #f) (vector-set! lint-counts 1 (add1 (vector-ref lint-counts 1))) (vector-set! lint-counts 2 (add1 (vector-ref lint-counts 2))))
      (when (eq? (hash-ref r 'status) 'low)
        (define k (list (if (hash-ref r 'fg #f) 'text 'fill) (round (hash-ref r 'x)) (round (hash-ref r 'y)) (round (hash-ref r 'w))
                        (map round2 (or (hash-ref r 'fg #f) (hash-ref r 'color))) (map round2 (hash-ref r 'bg))))
        (unless (hash-ref lint-lows k #f)                                         ; 新的不合格：PI2_LINT_DUMP=目录 → 存这一帧（查是哪个元素）
          (let ([d (getenv "PI2_LINT_DUMP")])
            (when d (snapshot-now! (build-path d (format "lint-~a-~a-~a.png" (vector-ref lint-counts 0) (round (hash-ref r 'x)) (round (hash-ref r 'y))))))))
        (hash-set! lint-lows k (round2 (hash-ref r 'ratio))))))
  (define (print-lint-summary!)
    (define (out!)
      (printf "G10 lint ~a: ~a frames, ~a text runs, ~a fills checked; ~a low (unique); ~a overlap (unique)\n"
              (string-join (for/list ([a (in-vector (current-command-line-arguments))] #:when (regexp-match? #rx"^--synth" a)) a) " ")
              (vector-ref lint-counts 0) (vector-ref lint-counts 1) (vector-ref lint-counts 2) (hash-count lint-lows) (hash-count lint-overlaps))
      (for ([(k v) (in-hash lint-lows)] [_ (in-range 25)])
        (printf "  low ~a at ~a,~a w ~a fg ~a bg ~a ratio ~a\n" (first k) (second k) (third k) (fourth k) (fifth k) (sixth k) v))
      (for ([(k _v) (in-hash lint-overlaps)] [_ (in-range 25)])
        (printf "  overlap text at ~a with text at ~a\n" (first k) (second k))))
    (out!)
    (let ([f (getenv "PI2_LINT_FILE")])                                          ; 回归脚本收集各流程的结果
      (when f (with-output-to-file f #:exists 'append out!))))
  ;; 侧栏：位置（左 / 右，设置页）与宽度（拖分隔线；松手时存）
  (define sidebar-right? (eq? (gsetting 'sidebar-side 'left) 'right))
  (define sidebar-w (let ([v (gsetting 'sidebar-width SIDEBAR-W)]) (if (real? v) v SIDEBAR-W)))
  (define drag-divider? #f)
  (define divider-hot? #f)
  (define (columns) (layout-columns sidebar? sidebar-right? sidebar-w w))
  (define (in-sidebar? x) (let-values ([(sbx sbw _mx _mw _dx) (columns)]) (and (> sbw 0.0) (<= sbx x (+ sbx sbw)))))
  (define (on-divider? x) (let-values ([(_a _b _c _d dx) (columns)]) (and dx (< (abs (- x dx)) 4.0))))
  (define submitted '())
  (define ime-log '())
  (define toast #f)                                    ; (cons 文本 截止毫秒)：状态行短暂提示
  ;; 侧栏少操作（design-workspace.md §11 A）：键盘焦点在侧栏（单键操作，按键不进输入法）；可撤销的会话操作；底部提示条
  (define sidebar-focus? #f)
  (define text-input-on? #t)
  (define undo-stack '())                              ; (listof (cons 说明 撤销的 thunk))，最近的在前，最多 30 个
  (define notice-until #f)
  (define last-session-op 0.0)                          ; 最近一次可撤销的会话操作 / 文字编辑的时刻：⌘Z 撤销最近的那一种
  (define last-text-op 0.0)
  (define (set-sidebar-focus! k)                       ; 键 → 侧栏有焦点、焦点行；#f → 回到输入框
    (set! sidebar-focus? (and k #t))
    (sbp 'set-focus! k))
  (define (notice! text) (sbp 'set-notice! text) (set! notice-until (+ (current-inexact-milliseconds) 10000.0)))
  (define (push-undo! label thunk)
    (set! last-session-op (current-inexact-milliseconds))
    (set! undo-stack (let ([l (cons (cons label thunk) undo-stack)]) (if (> (length l) 30) (take l 30) l))))
  (define (undo!)
    (cond [(null? undo-stack) (void)]
          [else (define u (car undo-stack))
                (set! undo-stack (cdr undo-stack))
                ((cdr u))
                (notice! (t 'sb.n-undone (car u)))]))
  (define last-frame-ms 0.0)                           ; 调试面板：上一帧构建 + 提交耗时
  (define secret-dirty? #f)                            ; 输 key 的按键被省掉了、句柄还没记
  (define secret-mode (let ([m (gsetting 'secret-mode 'suggest)]) (if (memq m '(off suggest auto)) m 'suggest)))
  ;; 聊天输入框里粘贴的内容要不要当密钥（按设置：不识别 / 只提示 / 自动）
  (define secret-patterns (load-secret-patterns))      ; 自定义格式（设置页「通用」里增删）
  (define (paste-as-secret? text) (and (string? text) (not (eq? secret-mode 'off)) (looks-like-secret? text secret-patterns)))
  ;; 插入密钥 chip；待选的展开面板，直接给出「作为密钥 / 转为文本」
  (define (insert-secret-chip! value pending?)
    (define f (editor-paste-secret! e value pending?))
    (when pending? (editor-toggle-expand! e (fold-id f))))
  (define sessions-log '())                            ; 冒烟：会话 / 模型菜单流程各步的检查结果
  (define t0 (current-inexact-milliseconds))
  (define last-edit t0)
  (unless ks (tr 'note! (t 'app.no-core)))
  (define (busy?) (tr 'busy?))
  (define (status)
    (string-join
     (filter values
      (list (and (multi?) (hs-label (A)))                                                              ; 多主机：当前是哪台
           (cond [ready #f] [ks (t 'app.connecting)] [else (t 'app.no-kernel)])                     ; 供应商 · 模型在左边的芯片里
           (cond [(and toast (< (current-inexact-milliseconds) (cdr toast))) (car toast)]
                 [core-exit (t 'app.kernel-exited core-exit)] [(busy?) (t 'app.running)] [else (t 'app.idle)])
           (t 'app.turns (tr 'turns))
           ;; 今日估算花费（> 0 才显示）：多主机 = 所有主机合计（同一本账只算一次），设了合计上限时带上限；单主机 = 这台，带它的每天上限
           (cond
             [(multi?)
              (define tot (spend-total))
              (define cap (combined-cap))
              (and (> tot 0) (t 'app.spent-all (if cap (format "$~a / $~a" (money2 tot) cap) (format "$~a" (money2 tot))) (spend-hosts-count)))]
             [else
              (let ([sp (and last-budget (hash-ref last-budget 'spent_today #f))] [cap (and last-budget (hash-ref last-budget 'daily_usd #f))])
                (and (real? sp) (> sp 0)
                     (t 'app.spent (if (real? cap) (format "$~a / $~a" (money2 sp) cap) (format "$~a" (money2 sp))))))])))
     " · "))
  ;; ---- 内部错误：记进日志、状态行提示，不退出（同一处同一消息只记前 3 次，防止每帧重复刷屏）
  (define error-counts (make-hash))
  (define (report-error! where e)
    (define msg (exn-message e))
    (define n (add1 (hash-ref error-counts (cons where msg) 0)))
    (hash-set! error-counts (cons where msg) n)
    (when (<= n 3)
      (define ctx (for/list ([c (in-list (continuation-mark-set->context (exn-continuation-marks e)))] [_ (in-range 12)])
                    (format "~a ~a" (or (car c) "?")
                            (let ([sl (cdr c)]) (if sl (format "~a:~a" (srcloc-source sl) (srcloc-line sl)) "")))))
      (jr 'record! 'error (hasheq 'where (symbol->string where) 'message msg 'context ctx))
      (eprintf "pi2 GUI internal error (~a): ~a\n" where msg))
    (set! toast (cons (t 'app.internal-error) (+ (current-inexact-milliseconds) 4000.0))))
  (define (guarded where thunk [fallback #t])
    (with-handlers ([exn:fail? (lambda (e) (report-error! where e) fallback)]) (thunk)))
  ;; ---- 会话的组织目录（design-workspace.md W2）：GUI 这台机器的 {config-home}/catalog.rktd；回放从录制时的快照开始、只在内存。
  ;; 各主机的会话列表到达时同步（会话 → 主机 + 工作目录的工作区；聊天会话 → 那台的聊天工作区）；置顶 / 归档存在会话条目上
  (define cat (if replay-path
                  (catalog-from-snapshot (hash-ref (or replay-start (hasheq)) 'catalog #f))
                  (with-handlers ([exn:fail? (lambda (e) (report-error! 'catalog e) (catalog-open #f))])
                    (catalog-open (build-path (gui-config-home) "catalog.rktd")))))
;; ---- 记忆（design-memory.md M2）：GUI 这台机器的 {config-home}/memory.rktd；回放从录制时的快照开始、只在内存
  (define mem (if replay-path
                  (memory-from-snapshot (hash-ref (or replay-start (hasheq)) 'memory #f))
                  (with-handlers ([exn:fail? (lambda (e) (report-error! 'memory e) (memory-open #f))])
                    (memory-open (build-path (gui-config-home) "memory.rktd")))))
  (define (scope-name sc)                                ; 作用域 id → 显示名
    (cond [(equal? sc "global") (t 'mem.global)]
          [(catalog-entity cat sc) => (lambda (e) (let ([a (hash-ref e 'attrs)]) (if (eq? (hash-ref a 'chat #f) #t) (t 'cat.chat-ws (hash-ref a 'host "?")) (hash-ref e 'name))))]
          [else sc]))
  ;; 会话的记忆作用域链（推给内核）：global → 项目 → super-workspace → 主工作区（聊天工作区也算；还没进目录的会话按主机 + 目录找）
  (define (memory-chain host-id row)
    (define sid (session-id host-id (hash-ref row 'path "")))
    (cons (hasheq 'id "global" 'kind "global" 'name "Global")
          (for/list ([e (in-list (catalog-scope-chain cat sid #:host host-id #:dir (let ([w (hash-ref row 'workdir #f)]) (and (string? w) w))
                                                      #:chat? (eq? (hash-ref row 'chat #f) #t) #:include-chat? #t))])
            (hasheq 'id (hash-ref e 'id) 'kind (hash-ref e 'kind) 'name (scope-name (hash-ref e 'id))))))
  (define (short-text s) (let ([s (regexp-replace* #rx"[\r\n]+" s " ")]) (if (> (string-length s) 60) (string-append (substring s 0 60) "…") s)))
  ;; ---- 项目索引（design-memory.md §8）：GUI 这台机器的 {config-home}/host-index.rktd；回放从录制时的快照开始、只在内存。
  ;; 每台主机连上时扫描（内核在那台机器上找 git 仓库）；要描述的一个一个来，只在所有主机都空闲时；本机仓库 → 本机内核 describe_project；
  ;; 远端仓库 → 那台内核 describe_input 收输入 → 本机内核 describe_project（模型在本机，内容不出这台 Mac）
  (define hix (if replay-path
                  (hix-from-snapshot (hash-ref (or replay-start (hasheq)) 'host-index #f))
                  (with-handlers ([exn:fail? (lambda (e) (report-error! 'host-index e) (hix-open #f))])
                    (hix-open (build-path (gui-config-home) "host-index.rktd")))))
  (define hix-active? (if replay-path (eq? (hash-ref (or replay-start (hasheq)) 'host-index-active #f) #t) #t))
  (define hix-flight #f)                                 ; #f | (vector 主机 目录 开始时刻)
  (define mem-flight #f)                                 ; 自动记忆（M3）正在处理的会话：#f | hasheq（'stage input|extract|merge …）；与项目索引共用「后台本机模型」：一次一个
  (define mem-seq 0)
  (define last-mem-tick 0.0)                             ; 每 5 分钟看一次有没有空闲够久的会话（回放不看：按事件推进才能复现）
  (define open-groups (make-hash))                       ; 侧栏里展开了的分组行（键 → #t）
  (define group-mode (let ([m (gsetting 'sidebar-group 'recent)]) (if (memq m '(recent project workspace super)) m 'recent)))   ; 侧栏分组方式（W3）
  ;; 分组方式下一行属于哪些组：→ (listof (cons 组键 组名))（目录里的项目 / 工作区 / super-workspace）
  (define (groups-of-row r)
    (define-values (ws sws ps) (catalog-session-groups cat (row-sid r)))
    (define (name e) (let ([a (hash-ref e 'attrs)]) (if (eq? (hash-ref a 'chat #f) #t) (t 'cat.chat-ws (hash-ref a 'host "?")) (hash-ref e 'name))))
    (for/list ([e (in-list (case group-mode [(project) ps] [(workspace) ws] [else sws]))]) (cons (hash-ref e 'id) (name e))))
  (define (row-sid r) (or (hash-ref r 'key #f) (session-id active (hash-ref r 'path ""))))   ; 单主机时行上没有 'key
  (define (hidden-text-mode) (if (eq? (gsetting 'hidden-text-mode 'mark) 'raw) 'raw 'mark))
  ;; 信息面板的「归属」：工作区 / super-workspace / 项目的名字与工作区的 git 远端（已排好的文字）
  (define (catalog-groups-text host path)
    (define-values (ws sws ps) (catalog-session-groups cat (session-id host path)))
    (define (name e) (let ([a (hash-ref e 'attrs)]) (if (eq? (hash-ref a 'chat #f) #t) (t 'cat.chat-ws (hash-ref a 'host "?")) (hash-ref e 'name))))
    (define (names l) (string-join (map name l) ", "))
    (hasheq 'workspace (names ws) 'super (names sws) 'projects (names ps)
            'git_remote (string-join (filter string? (map (lambda (e) (hash-ref (hash-ref e 'attrs) 'git_remote #f)) ws)) ", ")))
  (define (debug-stats)
    (list (t 'app.dbg-window
                  w h scale frames (/ (round (* 10 last-frame-ms)) 10.0)
                  (if (pair? msgs-per-frame) (car msgs-per-frame) "-") (tl 'glyph-count))
          (t 'app.dbg-kernel (cond [replay-path (t 'app.dbg-replay (rs 'progress))] [(not ks) (t 'app.dbg-none)]
                                   [core-exit (t 'app.dbg-exited core-exit)] [else (t 'app.dbg-alive)])
             (tr 'turns) (if (busy?) (t 'app.busy) (t 'app.idle)))
          (t 'app.dbg-wire (or last-wire "?"))))
  (define (caret-on?) (even? (inexact->exact (floor (/ (- (current-inexact-milliseconds) last-edit) BLINK-MS)))))
  (define (render!)
    (define rt0 (current-inexact-milliseconds))
    (jr 'record! 'frame frames)
    ;; 侧栏有键盘焦点（且不在改名）→ 按键不进输入法（单键操作）
    (let ([on? (or (not (or sidebar-focus? (sbp 'context-open?))) (sbp 'editing?) (svp 'open?))])
      (unless (eq? on? text-input-on?) (set! text-input-on? on?) (win 'set-text-input-enabled! on?)))                       ; 回放按重绘分批：命中区在重绘时更新
    (mdp 'begin-frame!)
    ;; 液态玻璃：侧栏显示且材质是玻璃 → 侧栏画进覆盖层的 drawlist，玻璃面板放到侧栏的位置。
    ;; 覆盖层在主层上面：设置页 / 调试面板（画在主层、可能盖住侧栏）打开时不用玻璃
    (define sb-shown? (let-values ([(_x sbw _m _w _d) (columns)]) (> sbw 0.0)))
    (define glass-now? (and (memq material '(glass desktop)) (win 'glass?) sb-shown? (not (svp 'open?)) (not (dv 'open?)) #t))
    (define desktop-now? (and glass-now? (eq? material 'desktop)))
    (define glass-layout? (and sb-shown? (if replay-path replay-glass? (and glass-now? (not desktop-now?)))))   ; 回放：按录制时的排
    (sbp 'set-translucent! (or glass-layout? desktop-now?) (if glass-layout? GLASS-RADIUS 0.0))
    ;; 两种玻璃都透出桌面：窗口透明、主层清成透明，玻璃那块留洞（面板：内缩圆角；整列：侧栏那一列）
    (unless (eq? glass-now? window-transparent?)
      (set! window-transparent? glass-now?)
      (win 'set-transparent! glass-now?)
      (g 'set-opaque! (not glass-now?)))
    (define grect (and glass-now? (let-values ([(sbx sbw _m _w _d) (columns)])
                                    (if desktop-now?
                                        (vector sbx 0.0 sbw h 0.0)
                                        (vector (+ sbx GLASS-INSET) GLASS-INSET (- sbw (* 2 GLASS-INSET)) (- h (* 2 GLASS-INSET)) GLASS-RADIUS)))))
    (tl 'build-frame! dl (lambda () (drawlist-clear! sdl)          ; 整帧可能重画（字形 atlas 满了）：覆盖层的也清掉
                                     (paint-scene! dl tl ib tr sbp mpp w h scale (status) #:focused? focused? #:caret-on? (caret-on?)
                                                  #:sidebar? sidebar? #:sidebar-right? sidebar-right? #:sidebar-w sidebar-w #:divider-hot? (or divider-hot? drag-divider?)
                                                  #:sidebar-dl (and glass-now? sdl) #:glass-layout? glass-layout? #:hole grect #:under (and glass-now? test-under)
                                                  #:debug dv #:debug-stats (debug-stats) #:settings svp #:inspector insp #:review rvp #:tests tpp)))
    (set! last-grect grect)
    (unless (equal? grect last-glass-rect) (set! last-glass-rect grect) (win 'set-glass-rect! (and grect (for/vector ([v grect]) (* v zoom)))))
    (for ([rq (in-list (tr 'wanted))]) (page-send! rq))   ; 对话区要的翻页（视口附近没装载的记录）
    (define m0 (objc-msg-count))
    (upload-atlas! g tl mdp)
    (when glass-now? (g 'render-overlay! sdl w h scale))
    (when (g 'render! dl (if glass-now? CLEAR-TRANSPARENT CLEAR) w h scale)
      (when (and lint-active? (zero? (modulo frames 3))) (guarded 'lint lint-frame!))
      (set! frames (add1 frames))
      (set! msgs-per-frame (cons (- (objc-msg-count) m0) msgs-per-frame)))
    (set! last-frame-ms (- (current-inexact-milliseconds) rt0))
    ;; 输入法候选框跟着正在输入的地方：设置页的输入框 / 主输入框
    (define-values (cr cur) (cond [(svp 'editing?) (values (svp 'caret-rect) (editor-cursor (svp 'editor)))]
                                  [(sbp 'editing?) (values (sbp 'caret-rect) (editor-cursor (sbp 'editor)))]   ; 侧栏改名
                                  [else (values (ib 'caret-rect) (editor-cursor e))]))
    (when cr (win 'set-text-input-state! (for/vector ([v (in-vector cr)]) (* v zoom)) cur)))
  (define (snapshot-now! path)
    (define pw (inexact->exact (round (* w scale))))
    (define ph (inexact->exact (round (* h scale))))
    ;; 用玻璃时侧栏在覆盖层：截图 = 主层 + 一块代替玻璃的面板 + 覆盖层（系统合成的玻璃截不到）
    (define out (cond [last-grect (define c (make-drawlist))
                                  (drawlist-append! c dl)
                                  (drawlist-rect! c (vector-ref last-grect 0) (vector-ref last-grect 1) (vector-ref last-grect 2) (vector-ref last-grect 3)
                                                  0.17 0.18 0.22 1.0 #:radius (vector-ref last-grect 4))
                                  (drawlist-append! c sdl)
                                  c]
                      [else dl]))
    (write-png path pw ph (g 'render-offscreen out CLEAR pw ph scale))
    (print-lint! "snapshot" (contrast-report out #:clear CLEAR #:frame-area (* w h)) (fill-report out #:clear CLEAR #:frame-area (* w h))))
  ;; ---- 显示效果的自动检查（design-gui.md §28）：字与背景的对比度 ≥ 4.5（WCAG AA 正文）；非文字的填充与下面的背景 ≥ 1.2；
  ;; 暗色主题：侧栏 / 对话区的底的亮度 ≤ 0.08（相对亮度）。主层的背景按 drawlist 推（自己画的颜色）；
  ;; 侧栏在玻璃上：截窗口（先藏覆盖层 → 系统合成后的玻璃像素）当最底层。→ 打印一行结论 + 不合格项
  (define MAX-DARK-L 0.08)
  (define (fmt-color c) (string-join (map (lambda (v) (number->string (round2 v))) (take c 3)) ","))
  (define (print-lint! label texts fills [extra ""])
    (define (checked l) (filter (lambda (r) (memq (hash-ref r 'status) '(ok low))) l))
    (define (lows l) (filter (lambda (r) (eq? (hash-ref r 'status) 'low)) l))
    (define (min-ratio l) (if (null? (checked l)) "-" (round2 (apply min (map (lambda (r) (hash-ref r 'ratio)) (checked l))))))
    (printf "G10 ~a: text ~a checked, ~a low (min ~a); fills ~a checked, ~a low (min ~a)~a\n"
            label (length (checked texts)) (length (lows texts)) (min-ratio texts) (length (checked fills)) (length (lows fills)) (min-ratio fills) extra)
    (for ([r (in-list (append (lows texts) (lows fills)))] [_ (in-range 12)])
      (printf "  low ~a at ~a,~a ~ax~a fg ~a bg ~a ratio ~a\n" (if (hash-ref r 'fg #f) "text" "fill")
              (round2 (hash-ref r 'x)) (round2 (hash-ref r 'y)) (round2 (hash-ref r 'w)) (round2 (hash-ref r 'h))
              (fmt-color (or (hash-ref r 'fg #f) (hash-ref r 'color))) (fmt-color (hash-ref r 'bg)) (round2 (hash-ref r 'ratio))))
    (and (null? (lows texts)) (null? (lows fills))))
  ;; 后台窗口模式（PI2_OFFSCREEN）：窗口在所有屏幕外面，WindowServer 不合成玻璃——实测侧栏亮度在白 / 黑 / 灰 / 橙 / 蓝背景上都是 0.0、最低对比度都是 5.38
  ;; （屏幕上是 0.01–0.03、4.88–5.38）：玻璃上的侧栏量不到真实效果 → 只量主区（我们自己画的，截图是对的），侧栏那部分不量、在结果里写明；
  ;; 玻璃 / 透出桌面的检查（visual、desktop 两个流程）在屏幕上跑
  (define (visual-check label)
    (define cap (win 'capture #t #f))                                            ; 只截本窗口（下面的窗口截不到）
    (let ([d (getenv "PI2_VISUAL_DIR")])                                         ; 留截图（藏了覆盖层的）给人看
      (when (and d cap) (write-png (build-path d (format "visual-~a.png" label)) (vector-ref cap 0) (vector-ref cap 1) (vector-ref cap 2) #:order 'rgba)
                        (let ([full (win 'capture #f #f)])                                      ; 再截一张带侧栏内容的
                          (when full (write-png (build-path d (format "visual-~a-full.png" label)) (vector-ref full 0) (vector-ref full 1) (vector-ref full 2) #:order 'rgba)))))
    (define base
      (and cap
           (let ([cw (vector-ref cap 0)] [ch (vector-ref cap 1)] [bs (vector-ref cap 2)] [k (* zoom (vector-ref cap 3))])
             (lambda (x y)
               (define px (max 0 (min (sub1 cw) (inexact->exact (floor (* x k))))))
               (define py (max 0 (min (sub1 ch) (inexact->exact (floor (* y k))))))
               (define i (* 4 (+ (* py cw) px)))
               (define a (max 1 (bytes-ref bs (+ i 3))))                              ; 预乘 → 还原
               (list (min 1.0 (/ (bytes-ref bs i) a)) (min 1.0 (/ (bytes-ref bs (+ i 1)) a)) (min 1.0 (/ (bytes-ref bs (+ i 2)) a)))))))
    (define off-glass? (and last-grect (getenv "PI2_OFFSCREEN") #t))              ; 后台窗口 + 玻璃：侧栏不量
    (define glass? (and last-grect (not off-glass?) #t))
    (define texts (append (contrast-report dl #:clear CLEAR #:frame-area (* w h)) (if (and glass? base) (contrast-report sdl #:base base #:frame-area (* w h)) '())))
    (define fills (append (fill-report dl #:clear CLEAR #:frame-area (* w h)) (if (and glass? base) (fill-report sdl #:base base #:frame-area (* w h)) '())))
    (define-values (sbx sbw lx mw _dx) (columns))
    (define sl (and base (region-luminance (if glass? (layered-base sdl base) base) (+ sbx 12.0) 60.0 (- sbw 24.0) (- h 120.0))))   ; 玻璃 + 侧栏铺的暗色
    (define ml (and base (region-luminance base (+ lx 12.0) 60.0 (- mw 24.0) (- h 120.0))))
    (define dark-ok? (and sl ml (or off-glass? (<= sl MAX-DARK-L)) (<= ml MAX-DARK-L)))
    (define ok? (print-lint! (format "visual ~a" label) (if off-glass? (filter (lambda (r) (>= (hash-ref r 'x 0.0) (+ sbx sbw))) texts) texts)
                             (if off-glass? (filter (lambda (r) (>= (hash-ref r 'x 0.0) (+ sbx sbw))) fills) fills)
                             (format "; L sidebar ~a main ~a~a" (if off-glass? "not measured" (if sl (round2 sl) "?")) (if ml (round2 ml) "?") (if cap "" " (no capture)"))))
    (define pass? (and ok? dark-ok?))
    (printf "G10 visual ~a: ~a\n" label (cond [(not pass?) "FAIL"] [off-glass? "PASS (main area; sidebar glass not measured off screen)"] [else "PASS"]))
    pass?)
  (define (apply-size! nw nh ns)
    (set! pw nw) (set! ph nh) (set! ps ns)
    (set! w (/ nw zoom)) (set! h (/ nh zoom)) (set! scale (* ns zoom))
    (g 'resize! pw ph ps))                               ; drawable 按物理尺寸；绘制时用逻辑尺寸 + scale × zoom
  (define (set-zoom! z)
    (unless (= z zoom)
      (set! zoom z)
      (save-setting! 'zoom z)
      (svp 'set-data! 'zoom z)
      (apply-size! pw ph ps)))
  ;; 窗口事件 → 逻辑坐标
  (define (unzoom ev)
    (cond [(= zoom 1.0) ev]
          [(ev:mouse? ev) (ev:mouse (ev:mouse-kind ev) (/ (ev:mouse-x ev) zoom) (/ (ev:mouse-y ev) zoom)
                                    (ev:mouse-button ev) (ev:mouse-mods ev) (ev:mouse-clicks ev))]
          [(ev:scroll? ev) (ev:scroll (/ (ev:scroll-dx ev) zoom) (/ (ev:scroll-dy ev) zoom) (ev:scroll-precise? ev) (ev:scroll-phase ev)
                                      (/ (ev:scroll-x ev) zoom) (/ (ev:scroll-y ev) zoom))]
          [else ev]))
  ;; 发给内核（缺省发给当前主机）。日志记界面要发的（无内核时也记：回放比对用）；key 打码；非 local 主机带 '_host
  (define (send! j [id active])
    (jr 'record! 'send (host-tag id (case (hash-ref j 'type #f)
                                      [("add_key") (hash-set j 'token (secret-or-handle (hash-ref j 'token "")))]
                                      [("register_secret") (hash-set j 'value (secret-or-handle (hash-ref j 'value "")))]
                                      [else (redact-send j)])))
    (and ks (ks 'send! id j)))
  (define (script-send! j [id active]) (and ks (begin (jr 'record! 'script-send (host-tag id j)) (ks 'send! id j))))   ; 冒烟脚本直接发的：回放不比对
  ;; 对话区按视口要的翻页请求（RPC v3）：记成 'page（回放不比对）
  (define (page-send! j [id active]) (and ks (begin (jr 'record! 'page (host-tag id j)) (ks 'send! id j))))
  ;; ---- 侧栏：多主机时按主机分组（标题行 + 会话），会话键 = 主机 \u0001 路径
  (define key-map (make-hash))                          ; 侧栏键 → (cons 主机 路径)
  (define (target k) (hash-ref key-map k (lambda () (cons active k))))
  (define (local-host? id) (equal? id "local"))                       ; 本机的内核（会话文件在这台机器上）
  (define (host-busy? id) (let ([i (tr 'doc-info id)]) (and i (hash-ref i 'busy #f))))
  (define (host-pending id) (let ([i (tr 'doc-info id)]) (if i (hash-ref i 'pending 0) 0)))
  (define (host-status id)
    (define h (H id))
    (cond [(hs-retry-at h) "connecting"] [(hs-core-exit h) "disconnected"] [(hs-ready h) "connected"] [else "connecting"]))
  (define (host-detail h)
    (define c (hs-config h))
    (cond [(not c) ""]
          [(hash-ref c 'ssh #f) => (lambda (x) (format "ssh ~a · serve ~a" x (hash-ref c 'serve-name "default")))]
          [(hash-ref c 'command #f) => (lambda (cmd) (string-join cmd " "))]
          [(hash-ref c 'entry #f) => (lambda (e) (format "place · ~a" e))]
          [else "place · main.rkt"]))
  (define (push-hosts-data!)
    (define file-entries (load-hosts-file))
    (svp 'set-data! 'hosts (for/list ([id (in-list host-order)])
                             (define h (H id))
                             (define fe (findf (lambda (x) (equal? (hash-ref x 'id #f) id)) file-entries))
                             (hasheq 'id id 'label (hs-label h) 'status (host-status id) 'detail (host-detail h)
                                     'removable (not (equal? id "local")) 'active (equal? id active)
                                     ;; 主机表里有 ssh 的：可以跑跨主机子 agent
                                     'subagent (and fe (string? (hash-ref fe 'ssh #f)) #t)
                                     'credential (if fe (hash-ref fe 'credential "proxy") "proxy")
                                     'workdir (and fe (hash-ref fe 'workdir #f))))))
  ;; 主机表里一台的字段（凭证方式、子 agent 工作目录）
  (define (update-host-entry! id k v)
    (save-hosts! (for/list ([x (in-list (load-hosts-file))]) (if (equal? (hash-ref x 'id #f) id) (hash-set x k v) x)))
    (push-hosts-data!))
  (define (refresh-sidebar!)
    (push-hosts-data!)
    (hash-clear! key-map)
    (sbp 'set-sessions!
         (arrange-session-rows                               ; 置顶在前、归档收进分组行（W2）
         (cond
           [(not (multi?))
            (for/list ([se (in-list (hs-sessions (A)))]) (hash-set! key-map (hash-ref se 'path #f) (cons active (hash-ref se 'path #f))) (hash-set se 'local (local-host? active)))]
           [else
            (append*
             (for/list ([id (in-list host-order)])
               (define h (H id))
               (define busy (host-busy? id))
               (define pending (host-pending id))
               (define status (cond [(hs-retry-at h) (t 'sb.host-reconnecting (hs-attempts h))]
                                    [(hs-core-exit h) (t 'sb.host-disconnected)] [(not (hs-ready h)) (t 'app.connecting)]
                                    [(> pending 0) (t 'sb.waiting pending)] [busy (t 'app.running)] [else ""]))
               (cons (hasheq 'kind "header" 'key (string-append "\u0001" id) 'host id 'label (hs-label h) 'status status
                             'level (cond [(or (> pending 0) (hs-core-exit h)) "warn"] [busy "busy"] [else "dim"])
                             'connected (not (hs-core-exit h)))
                     (for/list ([se (in-list (hs-sessions h))])
                       (define path (hash-ref se 'path ""))
                       (define k (string-append id "\u0001" path))
                       (hash-set! key-map k (cons id path))
                       (define cur? (hash-ref se 'current #f))
                       (define badge (and cur? (cond [(> pending 0) (t 'sb.waiting pending)] [busy (t 'app.running)] [else #f])))
                       (define se2 (hash-set* se 'key k 'current (and cur? (equal? id active)) 'local (local-host? id)
                                              'mem_read (not (eq? (catalog-session-attr cat (session-id id path) 'mem_read #t) #f))      ; 右键菜单的打勾
                                              'mem_write (not (eq? (catalog-session-attr cat (session-id id path) 'mem_write #t) #f))
                                              'host_index (catalog-session-attr cat (session-id id path) 'host_index 'unset)))
                       (if badge (hash-set* se2 'badge badge 'badge_level (if (> pending 0) "warn" "busy")) se2)))))])
         #:attr (lambda (r k) (catalog-session-attr cat (row-sid r) k))
         #:open? (lambda (g d) (hash-ref open-groups g d))
         #:archived-label (lambda (n) (t 'sb.archived-group n))
         ;; 按最近活动分段（§11.4 B）；回放用录制开始时的时刻
         #:now (if replay-path (hash-ref (or replay-start (hasheq)) 'now (current-seconds)) (current-seconds))
         #:older-days (let ([v (gsetting 'sidebar-older-days 14)]) (if (exact-positive-integer? v) v 14))
         #:section-label (lambda (sec n) (case sec [(recent) (t 'sb.sec-recent (let ([v (gsetting 'sidebar-older-days 14)]) (if (exact-positive-integer? v) v 14)) n)]
                                           [else (t (string->symbol (format "sb.sec-~a" sec)) n)]))
         #:preview (let ([pm (preview-map)]) (and pm (lambda (r) (hash-ref pm (row-sid r) #f))))
         #:groups-of (and (not (eq? group-mode 'recent)) groups-of-row)
         #:ungrouped-label (lambda (n) (t 'sb.ungrouped n))
         #:group-label (lambda (name n) (t 'sb.group-row name n)))))
  ;; ---- 切换当前主机：对话区换文档；模型菜单 / 设置页换成那台的数据
  (define drafts (make-hash))                            ; 主机 → 输入框草稿
  (define (switch-host! id)
    (when (and (H id) (not (equal? id active)))
      (hash-set! drafts active (editor-draft e))
      (set! active id)
      (editor-load-draft! e (hash-ref drafts id EMPTY-DRAFT))
      (tr 'use-doc! id)
      (define h (A))
      (mpp 'reset!)
      (when (hs-providers h) (mpp 'set-options! (hs-providers h)))
      (when (hs-pstate h) (mpp 'set-state! (hs-pstate h)))
      (svp 'set-data! 'providers (or (hs-providers h) (hasheq)))
      (svp 'set-data! 'rules (or (hs-rules h) '()))
      (svp 'set-data! 'aliases (if (hs-aliases h) (hash-ref (hs-aliases h) 'aliases '()) '()))
      (svp 'set-data! 'budget (or (hs-budget h) (hasheq)))
      (svp 'set-data! 'permission (or (hs-permission h) "normal"))
      (svp 'set-data! 'host-label (and (multi?) (hs-label h)))
      (refresh-sidebar!)))
  ;; ---- 附件（P4）：粘贴的图片 / 文件、拖进来的文件 → 输入框里的附件 chip；字节按 chip 的 fold id 记着（回放时只有 sha，没有字节）
  (define attachments (make-hasheqv))
  (define (attach! items)
    (for ([it (in-list items)])
      (define-values (kind mt name bs size sha0) (apply values (vector->list it)))
      (define sha (or sha0 (and (bytes? bs) (sha256-hex bs))))
      (when sha
        (define f (editor-attach! e kind name size))
        (hash-set! attachments (fold-id f) (hasheq 'kind kind 'media_type mt 'name name 'sha sha 'size size 'bytes bs))
        (when (bytes? bs)                                         ; 先在子进程里解码 / 探测：缩略图、尺寸、时长
          (case kind [(image) (mdp 'decode! sha bs)] [(audio) (mdp 'probe-audio! sha bs)] [else (void)])))))
  ;; 附件 → (values 发给内核的 媒体表 给对话区的 媒体表)
  (define (attachment-media folds)
    (for/lists (ks ts) ([f (in-list folds)] #:when (media-fold? f)
                        #:when #t [a (in-value (hash-ref attachments (fold-id f) #f))] #:when a)
      (define sha (hash-ref a 'sha))
      (define meta
        (case (hash-ref a 'kind)
          [(image) (let ([th (mdp 'thumb sha)]) (if th (hasheq 'width (vector-ref th 6) 'height (vector-ref th 7)) (hasheq)))]
          [(audio) (let ([i (mdp 'info sha)]) (if (and i (real? (hash-ref i 'duration #f))) (hasheq 'duration (hash-ref i 'duration)) (hasheq)))]
          [else (hasheq)]))
      (define base (hasheq 'kind (symbol->string (hash-ref a 'kind)) 'media_type (hash-ref a 'media_type) 'name (hash-ref a 'name)
                           'sha sha 'meta (hash-set meta 'bytes (hash-ref a 'size))))
      (define bs (hash-ref a 'bytes))
      (values (if (bytes? bs) (hash-set base 'data (bytes->string/latin-1 (base64-encode bs #""))) base)
              (if (bytes? bs) (hash-set base 'bytes bs) base))))
  ;; 显示用：去掉附件 chip（附件在消息下面画成块）
  (define (strip-media-chips text folds)
    (define-values (out fs _k)
      (for/fold ([out '()] [fs '()] [k 0]) ([c (in-string text)])
        (cond [(not (char=? c OBJ)) (values (cons c out) fs k)]
              [(media-fold? (list-ref folds k)) (values out fs (add1 k))]
              [else (values (cons c out) (cons (list-ref folds k) fs) (add1 k))])))
    (values (string-trim (list->string (reverse out))) (reverse fs)))
  ;; 输入框提交 → 对话区 + 内核
  (define (submit!)
    (define r (editor-submit! e))
    (when (and r (remember-command? (string-trim (car r))))                      ; /remember：记一条记忆，不发给模型
      (remember! (string-trim (car r))))
    (when (and r (not (remember-command? (string-trim (car r)))))
      (set! submitted (cons (car r) submitted))
      (hash-set! last-prompts active (car r))                                     ; 变更清单的「你说」
      (define folds (for/list ([id (in-list (snap-chips (cdr r)))]) (editor-fold e id)))
      (define-values (kernel-media tr-media) (attachment-media folds))
      (define-values (shown shown-folds) (strip-media-chips (snap-text (cdr r)) folds))
      (tr 'add-user! shown shown-folds tr-media)
      ;; 密钥 chip：原值另交内核登记（消息里只有占位符）；回放时 content 已是占位符，照发（日志里也只记句柄）
      (for ([f (in-list folds)] #:when (secret-fold? f))
        (define v (fold-content f))
        ;; handle：界面上显示、写进提示的句柄；内核连在远端（盐值不同）时按它登记
        (define h (let ([m (regexp-match #rx"^⟦secret:([0-9a-f]+)⟧$" v)]) (if m (cadr m) (secret-handle v))))
        (send! (hasheq 'type "register_secret" 'value v 'label "chat" 'handle h)))
      (send! (if (null? kernel-media)
                 (hasheq 'type "prompt" 'text (car r))
                 (hasheq 'type "prompt" 'text (string-trim (car r)) 'media kernel-media)))))
  (define (interrupt!) (when (busy?) (send! (hasheq 'type "interrupt"))))
  ;; 侧栏动作 → 内核会话请求。轮进行中不切换（内核会排队，界面会对不上）
  ;; 多主机：点别的主机的会话 = 先切到那台；那台正在跑的轮不打断（它的当前会话不是点的那个时才 resume，且要等它空闲）
  ;; 侧栏有键盘焦点时的按键（原始按键：输入法已关）→ 处理了吗。↑↓ 移动焦点行，回车打开，P 置顶 / 取消，E 归档 / 取消，
  ;; R 改名，I 信息，⌫ 删除（可撤销），Esc 先关菜单、再把焦点还给输入框；别的键：焦点还给输入框（这个键不进输入框）
  (define (sidebar-key! ev)
    (define code (ev:key-code ev))
    (define ch (string-downcase (or (ev:key-chars ev) "")))
    (define menu-k (sbp 'context-key))                                           ; 菜单开着：单键作用于菜单的那一行，并关菜单
    (define k (or menu-k (sbp 'focus)))
    (when (and menu-k (not (memv code '(53 126 125))) (not (memq 'cmd (ev:key-mods ev)))) (sbp 'close-context!))
    (define row (and k (findf (lambda (se) (equal? (hash-ref se 'key (hash-ref se 'path #f)) k)) (sbp 'sessions))))
    (cond
      [(memq 'cmd (ev:key-mods ev)) #f]                                            ; ⌘ 组合键照常（⌘Z 走菜单 → undo:）
      [(eqv? code 53) (if (sbp 'context-open?) (sbp 'close-context!) (set-sidebar-focus! #f)) #t]
      [(and menu-k (memv code '(126 125))) #t]                                     ; 菜单开着：上下键不动
      [(eqv? code 126) (sbp 'move-focus! -1) #t]
      [(eqv? code 125) (sbp 'move-focus! 1) #t]
      [(not row) (set-sidebar-focus! #f) #t]
      [(eqv? code 36) (session-action! (list 'resume k)) (set-sidebar-focus! #f) #t]   ; 打开 → 回输入框接着打字
      [(memv code '(51 117)) (session-action! (list 'delete k)) #t]
      [(and (member ch '("p" "e")) (eq? (hash-ref row 'ephemeral #f) #t)) #t]       ; 临时会话不能置顶 / 归档（同菜单）
      [(equal? ch "p") (session-action! (list (if (hash-ref row 'pinned #f) 'unpin 'pin) k)) #t]
      [(equal? ch "e") (session-action! (list (if (hash-ref row 'archived #f) 'unarchive 'archive) k)) #t]
      [(equal? ch "r") (sbp 'start-rename! k) #t]
      [(equal? ch "i") (session-action! (list 'info k)) #t]
      [menu-k #t]                                                                  ; 菜单开着时别的键：只关菜单
      [else (set-sidebar-focus! #f) #t]))
  ;; ---- 变更清单（design-workspace.md §11.7 C）：agent 提议 → 记进目录 → 审阅面板 → 应用 / 放弃 / 撤销；面板开着时侧栏预览结果
  (define last-prompts (make-hash))                    ; 主机 → 最近一次发的提示（清单里的「你说」）
  (define review-id #f)                                ; 审阅面板开着的清单
  (define (all-rows) (for*/hash ([id (in-list host-order)] [r (in-list (hs-sessions (H id)))] #:when (string? (hash-ref r 'path #f)))
                       (values (session-id id (hash-ref r 'path)) r)))
  (define (local-time-string)
    (define d (seconds->date (current-seconds)))
    (define (p n) (if (< n 10) (format "0~a" n) (format "~a" n)))
    (format "~a-~a-~a ~a:~a" (date-year d) (p (date-month d)) (p (date-day d)) (p (date-hour d)) (p (date-minute d))))
  (define (review-data cid)
    (define cs (catalog-changeset cat cid))
    (define p (hash-ref cs 'proposal))
    (define state (changeset-state cs))
    (define raw (let ([v (hash-ref p 'items '())]) (if (list? v) v '())))
    (define items (if (eq? state 'proposed)
                      (changeset-validate cat raw (all-rows))                                 ; 待审阅：按现状校验
                      (for/list ([it (in-list raw)] [n (in-naturals)]) (hash-set* it 'n n 'status "ok"))))   ; 已处理：只读
    (hasheq 'id cid 'by (hash-ref p 'by "?") 'when (hash-ref p 'when "") 'summary (hash-ref p 'summary "")
            'request (hash-ref (hash-ref p 'source (hasheq)) 'request "") 'state state
            'accepted (let ([rv (hash-ref cs 'review #f)]) (if rv (hash-ref rv 'accepted '()) '())) 'items items))
  (define (on-changeset-proposed! id cev)
    (define rows (all-rows))
    (define items (for/list ([it (in-list (let ([v (hash-ref cev 'items '())]) (if (list? v) v '())))] #:when (hash? it))
                    (define p (hash-ref it 'session #f))
                    (define sid (and (string? p) (session-id id p)))
                    (define r (and sid (hash-ref rows sid #f)))
                    (let* ([it (if sid (hash-set it 'session sid) it)]
                           [it (if r (hash-set it 'title (let ([tt (hash-ref r 'title #f)]) (if (string? tt) tt (t 'sb.untitled)))) it)])   ; 当时的标题（审计）
                      it)))
    (define model (let ([ps (hs-pstate (H id))]) (and (hash? ps) (hash-ref ps 'model #f))))
    (define cid (catalog-add-changeset! cat (hasheq 'by (format "agent:~a" (or model "?")) 'when (local-time-string)
                                                  'source (hasheq 'host id 'session (hash-ref cev 'session 'null) 'request (hash-ref last-prompts id ""))
                                                  'summary (hash-ref cev 'summary "") 'items items)))
    (open-review! cid))
  ;; 工作区测试流程面板（design-goalmode.md §12 T2）：打开 = 向当前主机要 test_flow；动作 → 内核请求
  (define (open-tests!) (set! tests-wanted? #t) (send! (hasheq 'type "test_flow")))
  (define (tests-action! a)
    (when a
      (case (car a)
        [(close) (tpp 'close!) (set! tests-wanted? #f)]
        [(trust) (send! (hasheq 'type "test_flow_trust" 'sha (cadr a)))]
        [(run) (tpp 'set-running! #t) (send! (if (cadr a) (hasheq 'type "test_flow_run" 'steps (list (cadr a))) (hasheq 'type "test_flow_run")))]
        [(approve) (send! (hasheq 'type "test_flow_decide" 'id (cadr a) 'decision "apply"))]
        [(reject) (send! (hasheq 'type "test_flow_decide" 'id (cadr a) 'decision "reject"))]
        [else (void)])))
  (define (open-review! cid) (set! review-id cid) (insp 'close!) (rvp 'open! (review-data cid)) (refresh-sidebar!))
  (define (kernel-for-changeset host req) (send! req host))
  (define (review-action! a)
    (when (pair? a)
      (case (car a)
        [(close) (rvp 'close!) (set! review-id #f) (refresh-sidebar!)]
        [(toggle toggle-group) (refresh-sidebar!)]                                   ; 侧栏预览跟着勾选变
        [(apply)
         (define cid review-id)
         (define acc (rvp 'accepted))
         (changeset-apply! cat cid acc (all-rows) #:kernel kernel-for-changeset)
         (rvp 'close!) (set! review-id #f)
         (push-undo! (t 'rv.n-applied (length acc) cid)
                     (lambda () (changeset-undo! cat cid #:kernel kernel-for-changeset) (refresh-sidebar!) (push-catalog-views!)))
         (notice! (t 'rv.n-applied (length acc) cid))
         (refresh-sidebar!) (push-catalog-views!)]
        [(discard)
         (define cid review-id)
         (catalog-changeset-event! cat cid 'review (hasheq 'accepted '() 'rejected (for/list ([it (in-list (hash-ref (rvp 'data) 'items '()))]) (hash-ref it 'n)) 'discarded #t))
         (rvp 'close!) (set! review-id #f) (notice! (t 'rv.n-discarded cid)) (refresh-sidebar!)]
        [(undo)
         (define cid review-id)
         (changeset-undo! cat cid #:kernel kernel-for-changeset)
         (rvp 'close!) (set! review-id #f) (refresh-sidebar!) (push-catalog-views!)]
        [else (void)])))
  ;; 侧栏预览：审阅面板开着（待审阅）时，勾选的项在对应的行上标出结果。→ #f | (hash 会话 id → (listof (cons 文字 种类)))
  (define (preview-map)
    (and review-id (rvp 'open?) (eq? (hash-ref (rvp 'data) 'state #f) 'proposed)
         (let ([acc (rvp 'accepted)])
           (for/fold ([h (hash)]) ([it (in-list (hash-ref (rvp 'data) 'items '()))]
                                   #:when (memv (hash-ref it 'n) acc) #:when (string? (hash-ref it 'session #f)))
             (define tag (case (hash-ref it 'op)
                           [("link") (cons (format "+ ~a" (hash-ref it 'project "")) 'link)]
                           [("unlink") (cons (format "− ~a" (hash-ref it 'project "")) 'unlink)]
                           [("archive") (cons (t 'rv.c-archive) 'archive)] [("unarchive") (cons (t 'rv.c-unarchive) 'other)]
                           [("pin") (cons "📌" 'other)] [("unpin") (cons (t 'rv.c-unpin) 'other)]
                           [("rename") (cons (format "→ ~a" (hash-ref it 'new_title "")) 'rename)]
                           [("delete") (cons (t 'rv.c-delete) 'delete)]
                           [else (cons (hash-ref it 'op "?") 'other)]))
             (hash-update h (hash-ref it 'session) (lambda (l) (append l (list tag))) '())))))
  ;; 目录视图推给内核（sessions_overview 用）：项目名、这台内核每个会话的项目 / 置顶 / 归档。记成 'page（回放不比对）
  (define (push-catalog-view! id)
    (define h (H id))
    (when h
      (define projects (map (lambda (e) (hash-ref e 'name)) (catalog-entities-of cat "project")))
      (define used (make-hash))                                                 ; 用到的有指令的实体：id → 实体（指令正文只发一份）
      (define scopes (make-hash))                                               ; 会话链上的记忆作用域（推这些作用域的记忆条目）
      (define sessions (for/list ([r (in-list (hs-sessions h))] #:when (string? (hash-ref r 'path #f)))
                         (define sid (session-id id (hash-ref r 'path)))
                         (define-values (_w _s ps) (catalog-session-groups cat sid))
                         (define chain (catalog-instruction-chain cat sid #:host id #:dir (let ([w (hash-ref r 'workdir #f)]) (and (string? w) w)) #:chat? (eq? (hash-ref r 'chat #f) #t)))
                         (for ([e (in-list chain)]) (hash-set! used (hash-ref e 'id) e))
                         (define mchain (memory-chain id r))
                         (for ([c (in-list mchain)]) (hash-set! scopes (hash-ref c 'id) #t))
                         (hasheq 'path (hash-ref r 'path) 'projects (map (lambda (e) (hash-ref e 'name)) ps)
                                 'instr (map (lambda (e) (hash-ref e 'id)) chain)
                                 'mem mchain
                                 'mem_read (not (eq? (catalog-session-attr cat sid 'mem_read #t) #f))
                                 'mem_write (not (eq? (catalog-session-attr cat sid 'mem_write #t) #f))
                                 'host_index (let ([v (catalog-session-attr cat sid 'host_index 'unset)]) (if (boolean? v) v 'null))   ; 项目索引：开 / 关 / 缺省
                                 'pinned (and (catalog-session-attr cat sid 'pinned #f) #t) 'archived (and (catalog-session-attr cat sid 'archived #f) #t))))
      (define instructions (for/list ([e (in-list (sort (hash-values used) string<? #:key (lambda (e) (hash-ref e 'id))))])
                             (hasheq 'id (hash-ref e 'id) 'kind (hash-ref e 'kind) 'name (hash-ref e 'name) 'text (catalog-entity-instructions e))))
      (page-send! (hasheq 'type "set_catalog_view" 'projects projects 'sessions sessions 'instructions instructions
                          'memories (memory-view-entries mem (hash-keys scopes)) 'memory_global_auto (eq? (gsetting 'memory-global-auto #f) #t)
                          'host_projects (hix-view hix id))
                  id)))
  ;; 记忆改了：设置页开着就刷新它的数据；推给各台内核（新会话用；已开始的会话冻结着不变）
  (define (after-memory-change!) (when (svp 'open?) (push-memory-data!)) (push-catalog-views!))
  ;; 设置页「记忆」的数据：待确认的 + 按作用域分组（全局在前，其余按名字）
  (define (push-memory-data!)
    (define (session-title sid)
      (and (string? sid)
           (let* ([parts (regexp-split #rx"\u0001" sid)] [hid (car parts)] [p (if (pair? (cdr parts)) (cadr parts) "")]
                  [h (H hid)] [r (and h (findf (lambda (se) (equal? (hash-ref se 'path #f) p)) (hs-sessions h)))])
             (if (and r (string? (hash-ref r 'title #f))) (hash-ref r 'title) (let-values ([(_b n _d) (split-path p)]) (if (path? n) (path->string n) p))))))
    (define (view e)
      (hasheq 'id (hash-ref e 'id) 'text (hash-ref e 'text) 'pinned (hash-ref e 'pinned #f)
              'meta (string-join (filter values (list (t (case (hash-ref e 'source "user") [("model") 'mem.src-model] [("auto") 'mem.src-auto] [else 'mem.src-user]))
                                                       (let ([c (hash-ref e 'created "")]) (and (non-empty-string? c) c))
                                                       (session-title (hash-ref e 'session #f))
                                                       (let ([ev (hash-ref e 'evidence #f)]) (and (string? ev) (format "“~a”" (short-text ev))))
                                                       (and (equal? (hash-ref e 'state #f) "active") (memory-dormant? e (current-seconds)) (t 'mem.dormant))))
                                 " · ")))
    (define by-scope (for/fold ([h (hash)]) ([e (in-list (memory-entries mem))]) (hash-update h (hash-ref e 'scope) (lambda (l) (append l (list e))) '())))
    (define scope-order (sort (hash-keys by-scope) (lambda (a b) (cond [(equal? a "global") #t] [(equal? b "global") #f] [else (string<? (scope-name a) (scope-name b))]))))
    (svp 'set-data! 'memory
         (hasheq 'pending (map view (memory-pending mem))
                 'groups (for/list ([sc (in-list scope-order)])
                           (define es (hash-ref by-scope sc))
                           (list (scope-name sc) (map view (append (filter (lambda (e) (hash-ref e 'pinned #f)) es) (filter (lambda (e) (not (hash-ref e 'pinned #f))) es)))))
                 'global-auto (eq? (gsetting 'memory-global-auto #f) #t)
                 'auto (mem-auto-on?)
                 'auto-status (t 'set.mem-auto-status (hash-count (memory-processed mem))
                                 (if mem-flight (let ([ti (hash-ref (hash-ref mem-flight 'row) 'title #f)]) (format "~a (~a)" (if (string? ti) ti "?") (hash-ref mem-flight 'stage))) "—")))))
  ;; 模型 memory_note（内核事件）→ 记忆库：全局且没开「直接记」→ 待确认；其余直接记（可撤销）
  (define (on-memory-note! id cev)
    (define text (format "~a" (hash-ref cev 'text "")))
    (define scope (format "~a" (hash-ref cev 'scope "global")))
    (define pending? (and (equal? (hash-ref cev 'kind #f) "global") (not (eq? (gsetting 'memory-global-auto #f) #t))))
    (define mid (memory-add! mem scope text #:source "model" #:session (session-id id (format "~a" (hash-ref cev 'session ""))) #:state (if pending? "pending" "active")))
    (cond
      [pending? (notice! (t 'mem.n-pending (short-text text)))]
      [else (define label (t 'mem.n-saved (scope-name scope) (short-text text)))
            (push-undo! label (lambda () (memory-delete! mem mid) (after-memory-change!)))
            (notice! label)])
    (after-memory-change!))
  ;; 输入框「/remember [global|project|workspace] 内容」：记到当前会话链上那一层（缺省工作区；新目录还没有工作区 → 建）；可撤销
  (define (remember! text)
    (define-values (kind body) (parse-remember text))
    (define c (let ([h (A)]) (and h (findf (lambda (se) (hash-ref se 'current #f)) (hs-sessions h)))))
    (define scope
      (cond
        [(string=? body "") #f]
        [(equal? kind "global") "global"]
        [else
         (or (and c (for/last ([e (in-list (cdr (memory-chain active c)))] #:when (equal? (hash-ref e 'kind) kind)) (hash-ref e 'id)))
             (and (equal? kind "workspace") c
                  (cond [(eq? (hash-ref c 'chat #f) #t) (catalog-ensure-chat-workspace! cat active)]
                        [(string? (hash-ref c 'workdir #f)) (catalog-ensure-workspace! cat active (hash-ref c 'workdir))]
                        [else #f])))]))
    (cond
      [(string=? body "") (notice! (t 'mem.n-empty))]
      [(not scope) (notice! (t 'mem.n-no-scope kind))]
      [else
       (define mid (memory-add! mem scope body #:source "user" #:session (and c (session-id active (hash-ref c 'path "")))))
       (define label (t 'mem.n-saved (scope-name scope) (short-text body)))
       (push-undo! label (lambda () (memory-delete! mem mid) (after-memory-change!)))
       (notice! label)
       (after-memory-change!)]))
  (define (push-catalog-views!) (for ([id (in-list host-order)]) (push-catalog-view! id)))
  ;; ---- 项目索引的队列与数据
  (define (hix-model-cfg)                                ; → (values 端点 模型或#f)：自己的设置 → 「工具输出」的本机模型 → 缺省本机端点
    (define v (gsetting 'host-index-model #f))
    (define pv (let ([x (gsetting 'tool-preview #f)]) (if (hash? x) x (hasheq))))
    (values (or (and (hash? v) (string? (hash-ref v 'endpoint #f)) (hash-ref v 'endpoint)) (let ([e (hash-ref pv 'endpoint #f)]) (and (string? e) e)) "http://localhost:1234/v1")
            (or (and (hash? v) (string? (hash-ref v 'model #f)) (hash-ref v 'model)) (let ([m (hash-ref pv 'model #f)]) (and (string? m) m)))))
  (define (local-runner) (for/first ([id (in-list host-order)] #:when (and (local-host? id) (H id) (hs-ready (H id)))) id))
  (define (hix-pump!)
    (when (and hix-active? (not (eq? (gsetting 'host-index-paused #f) #t)))
      (when (and hix-flight (> (- (current-inexact-milliseconds) (vector-ref hix-flight 2)) 600000.0)) (set! hix-flight #f))   ; 10 分钟没回：放弃这次
      (when (and (not hix-flight) (not mem-flight) (not (for/or ([id (in-list host-order)]) (host-busy? id))))
        (define next (for*/first ([id (in-list host-order)] #:when (and (H id) (hs-ready (H id))) [e (in-list (hix-stale hix id))]) e))
        (when next
          (define host (hash-ref next 'host)) (define dir (hash-ref next 'dir)) (define fp (hash-ref next 'fingerprint ""))
          (define-values (ep model) (hix-model-cfg))
          (define runner (if (local-host? host) host (local-runner)))
          (cond
            [(not runner) (hix-set-error! hix host dir fp "no kernel on this Mac to run the model") (hix-pump!)]   ; 记成这个指纹失败 → 不再是待描述的
            [else
             (set! hix-flight (vector host dir (current-inexact-milliseconds)))
             (if (equal? runner host)
                 (send! (hasheq 'type "describe_project" 'dir dir 'fingerprint fp 'endpoint ep 'model (or model 'null)) host)
                 (send! (hasheq 'type "describe_input" 'dir dir) host))])))))
  (define (after-hix-change! host) (when (H host) (push-catalog-view! host)) (when (svp 'open?) (push-hix-data!)))
  ;; ---- 自动记忆（design-memory.md M3）：空闲够久的会话 → 内核给会话文字 → 本机模型提取候选 → 再一次调用与已有条目合并 → 写进记忆库
  ;; 缺省关（用户决定 1）；全局作用域的自动记忆按「模型记全局」那个设置（缺省先问我 → 待确认）；用户自己写的条目不会被改写
  (define (mem-auto-on?) (eq? (gsetting 'memory-auto #f) #t))
  (define (mem-idle-seconds) (let ([e (getenv "PI2_MEM_IDLE_SECONDS")]) (or (and e (string->number e)) (* 2 3600))))
  (define (mem-row-flags id r) (hash-set r 'mem_write (not (eq? (catalog-session-attr cat (session-id id (hash-ref r 'path "")) 'mem_write #t) #f))))
  (define (hash-union* a b) (for/fold ([a a]) ([(k v) (in-hash b)]) (hash-set a k v)))
  (define (mem-auto-pump!)
    (when (and mem-flight (> (- (current-inexact-milliseconds) (hash-ref mem-flight 'started 0.0)) 900000.0)) (set! mem-flight #f))   ; 15 分钟没走完：放弃
    (when (and (mem-auto-on?) (not mem-flight) (not hix-flight) (local-runner) (not (for/or ([id (in-list host-order)]) (host-busy? id))))
      (define next
        (for*/first ([id (in-list host-order)] #:when (and (H id) (hs-ready (H id)))
                     [r (in-list (eligible-sessions (map (lambda (r) (mem-row-flags id r)) (hs-sessions (H id)))
                                                    (for/hash ([(k v) (in-hash (memory-processed mem))] #:when (string-prefix? k (string-append id "\u0001")))
                                                      (values (substring k (add1 (string-length id))) v))
                                                    (current-seconds) (mem-idle-seconds)))])
          (cons id r)))
      (when next
        (set! mem-flight (hasheq 'stage 'input 'host (car next) 'path (hash-ref (cdr next) 'path) 'row (cdr next) 'started (current-inexact-milliseconds)))
        (send! (hasheq 'type "memory_input" 'path (hash-ref (cdr next) 'path)) (car next)))))
  (define (mem-existing chain)                           ; 链上作用域的活动条目 → 给模型看的（id 种类 文字）
    (define kind-of (for/hash ([c (in-list chain)]) (values (hash-ref c 'id) (hash-ref c 'kind))))
    (for/list ([e (in-list (memory-entries mem))] #:when (hash-ref kind-of (hash-ref e 'scope) #f))
      (hasheq 'id (hash-ref e 'id) 'kind (hash-ref kind-of (hash-ref e 'scope)) 'text (hash-ref e 'text))))
  (define (mem-finish! [added 0])                        ; 这个会话处理完（成功或失败都记，免得反复重试）→ 下一个
    (define f mem-flight)
    (set! mem-flight #f)
    (when f
      (memory-processed! mem (session-id (hash-ref f 'host) (hash-ref f 'path)) (hash-ref f 'messages (hash-ref (hash-ref f 'row) 'messages 0)))
      (when (> added 0)
        (notice! (t 'mem.n-auto added (let ([ti (hash-ref (hash-ref f 'row) 'title #f)]) (if (string? ti) ti (hash-ref f 'path))))))
      (after-memory-change!))
    (mem-auto-pump!) (hix-pump!))
  (define (mem-complete! stage prompt extra)             ; 发给本机内核：local_complete
    (set! mem-seq (add1 mem-seq))
    (define-values (ep model) (hix-model-cfg))
    (define cid (format "mem-~a-~a" stage mem-seq))
    (set! mem-flight (hash-set* (hash-union* mem-flight extra) 'stage stage 'id cid))
    (send! (hasheq 'type "local_complete" 'id cid 'prompt prompt 'endpoint ep 'model (or model 'null)) (local-runner)))
  (define (on-memory-input! id cev)
    (when (and mem-flight (eq? (hash-ref mem-flight 'stage) 'input) (equal? (hash-ref cev 'path #f) (hash-ref mem-flight 'path)) (equal? id (hash-ref mem-flight 'host)))
      (cond
        [(or (hash-ref cev 'error #f) (not (local-runner))) (mem-finish!)]
        [else
         (define chain (memory-chain id (hash-ref mem-flight 'row)))
         (define existing (mem-existing chain))
         (mem-complete! 'extract (extract-prompt (hash-ref cev 'text "") chain existing)
                        (hasheq 'chain chain 'existing existing 'messages (hash-ref cev 'messages 0)))])))
  (define (on-local-completed! cev)
    (when (and mem-flight (equal? (hash-ref cev 'id #f) (hash-ref mem-flight 'id #f)))
      (cond
        [(hash-ref cev 'error #f) (mem-finish!)]
        [(eq? (hash-ref mem-flight 'stage) 'extract)
         (define cands (parse-extract (hash-ref cev 'text "")))
         (if (null? cands)
             (mem-finish!)
             (mem-complete! 'merge (merge-prompt cands (hash-ref mem-flight 'existing)) (hasheq 'candidates cands)))]
        [else                                                                    ; merge → 写进记忆库
         (define cands (hash-ref mem-flight 'candidates))
         (define chain (hash-ref mem-flight 'chain))
         (define sid (session-id (hash-ref mem-flight 'host) (hash-ref mem-flight 'path)))
         (define (scope-for kind)                                                ; 种类 → 链上那一层（同种多个取最具体的）；没有项目 → 工作区
           (or (for/last ([c (in-list chain)] #:when (equal? (hash-ref c 'kind) kind)) (hash-ref c 'id))
               (and (equal? kind "project") (for/last ([c (in-list chain)] #:when (equal? (hash-ref c 'kind) "workspace")) (hash-ref c 'id)))))
         (define added
           (for/sum ([op (in-list (parse-merge (hash-ref cev 'text "") (length cands) (map (lambda (e) (hash-ref e 'id)) (hash-ref mem-flight 'existing))))])
             (define c (list-ref cands (hash-ref op 'candidate)))
             (define old (and (equal? (hash-ref op 'op) "update") (memory-entry mem (hash-ref op 'id))))
             (cond
               [(and old (member (hash-ref old 'source "user") '("auto" "model"))) (memory-edit! mem (hash-ref op 'id) (hash-ref op 'text)) 0]   ; 只改自动 / 模型记的
               [else
                (define sc (if (equal? (hash-ref c 'scope) "global") "global" (scope-for (hash-ref c 'scope))))
                (cond
                  [(not sc) 0]
                  [else (memory-add! mem sc (if old (hash-ref op 'text) (hash-ref c 'text)) #:source "auto" #:session sid
                                     #:evidence (let ([ev (hash-ref c 'evidence "")]) (and (non-empty-string? ev) ev))
                                     #:state (if (and (equal? sc "global") (not (eq? (gsetting 'memory-global-auto #f) #t))) "pending" "active"))
                        1])])))
         (mem-finish! added)])))
  (define (push-hix-data!)
    (define-values (ep model) (hix-model-cfg))
    (define es (hix-entries hix))
    (define (failed? e) (and (hash-ref e 'error #f) (equal? (hash-ref e 'error-fp #f) (hash-ref e 'fingerprint #f))))
    (define (view e)
      (define d (hash-ref e 'desc #f))
      (hasheq 'key (string-append (hash-ref e 'host) "\u0001" (hash-ref e 'dir)) 'name (hash-ref e 'name "?") 'dir (hash-ref e 'dir)
              'summary (if (hash? d) (hash-ref d (if (eq? (current-lang) 'zh) 'summary_zh 'summary_en) "") "")
              'short (if (hash? d) (string-join (hash-ref d 'short '()) ", ") "")
              'state (cond [(hash-ref e 'locked #f) "edited"] [(failed? e) "error"] [(hix-stale? e) "pending"] [else "described"])
              'error (or (hash-ref e 'error #f) "") 'text (desc->text d)))
    (svp 'set-data! 'hix
         (hasheq 'paused (eq? (gsetting 'host-index-paused #f) #t) 'endpoint ep 'model (or model #f)
                 'found (length es) 'described (for/sum ([e (in-list es)]) (if (and (hash-ref e 'desc #f) (not (hix-stale? e))) 1 0))
                 'pending (for/sum ([e (in-list es)]) (if (hix-stale? e) 1 0)) 'errors (for/sum ([e (in-list es)]) (if (failed? e) 1 0))
                 'current (and hix-flight (let ([e (hix-entry hix (vector-ref hix-flight 0) (vector-ref hix-flight 1))]) (and e (hash-ref e 'name #f))))
                 'hosts (for/list ([hid (in-list (remove-duplicates (map (lambda (e) (hash-ref e 'host)) es)))])
                          (list (if (H hid) (hs-label (H hid)) hid) (map view (hix-entries hix hid)))))))
  (sbp 'set-group-mode! group-mode)
  ;; 自动起标题（§11.4 B）：没有标题记录、至少 6 条消息的会话，每次运行每个会话问一次（端点 / 模型同「工具输出」的本机模型）
  (define auto-titled (make-hash))
  (define (maybe-auto-title! id)
    (when (and (not replay-path) (not (eq? (gsetting 'auto-title #t) #f)) (H id))
      (define pv (let ([v (gsetting 'tool-preview #f)]) (if (hash? v) v (hasheq))))
      (for ([r (in-list (hs-sessions (H id)))]
            #:when (and (string? (hash-ref r 'path #f)) (not (hash-ref r 'titled #f)) (not (hash-ref r 'ephemeral #f))
                        (>= (let ([m (hash-ref r 'messages 0)]) (if (real? m) m 0)) 6)
                        (not (hash-ref auto-titled (session-id id (hash-ref r 'path)) #f))))
        (hash-set! auto-titled (session-id id (hash-ref r 'path)) #t)
        (page-send! (hasheq 'type "auto_title" 'path (hash-ref r 'path)
                            'endpoint (let ([e (hash-ref pv 'endpoint #f)]) (if (string? e) e 'null))
                            'model (let ([m (hash-ref pv 'model #f)]) (if (string? m) m 'null)))
                    id))))
  (define (in-project? pid sid) (for/or ([p (in-list (catalog-parents cat sid))]) (equal? (car p) pid)))
  ;; ---- 手势与拖放（design-workspace.md §11.4 D）
  ;; 横扫：触控板手势在行上开始（began），累计移动 > 6 point 且以横向为主 → 横扫这一行（行跟着走，松手按偏移做动作）；否则照常上下滚
  (define swipe-cand #f)                                ; #f | (vector 行键 累计dx 累计dy)
  (define (sidebar-scroll! ev sx sy dy)
    (define ph (ev:scroll-phase ev))
    (define dx (if (ev:scroll-precise? ev) (ev:scroll-dx ev) 0.0))
    (cond
      [(sbp 'swiping?)
       (define a (sbp 'swipe! #f dx (if (eq? ph 'ended) 'ended 'changed)))
       (when a (session-action! a))]
      [(and (ev:scroll-precise? ev) (eq? ph 'began)) (set! swipe-cand (vector (sbp 'row-at sx sy) 0.0 0.0)) (sbp 'scroll! dy)]
      [(and swipe-cand (eq? ph 'changed))
       (define c (vector (vector-ref swipe-cand 0) (+ (vector-ref swipe-cand 1) dx) (+ (vector-ref swipe-cand 2) dy)))
       (cond [(and (string? (vector-ref c 0)) (not (regexp-match? #rx"^\u0002" (vector-ref c 0)))
                   (> (abs (vector-ref c 1)) 6.0) (> (abs (vector-ref c 1)) (* 1.5 (abs (vector-ref c 2)))))
              (set! swipe-cand #f) (sbp 'swipe! (vector-ref c 0) 0.0 'began) (sbp 'swipe! #f (vector-ref c 1) 'changed)]
             [(> (abs (vector-ref c 2)) 6.0) (set! swipe-cand #f) (sbp 'scroll! dy)]
             [else (set! swipe-cand c) (sbp 'scroll! dy)])]
      [else (when (eq? ph 'ended) (set! swipe-cand #f)) (sbp 'scroll! dy)]))
  ;; 拖行：按住行、移动 > 6 point → 拖；松手在分组行上 → 置顶（「置顶」段）/ 归档（「已归档」）/ 归入项目（按项目分组时的项目组）；可撤销
  (define drag-cand #f)                                 ; #f | (vector 行键 x y)
  (define (drop-action! k dest)                          ; dest：分组行的键（不叫 target：那是「键 → 主机 + 路径」的函数）
    (define tg (target k))
    (define sid (session-id (car tg) (cdr tg)))
    (cond
      [(regexp-match? #rx"pinned$" dest) (session-action! (list 'pin k))]
      [(regexp-match? #rx"^\u0002archived" dest) (session-action! (list 'archive k))]
      [(and (eq? group-mode 'project) (regexp-match #rx"\u0001([^\u0001]+)$" dest))
       => (lambda (m)
            (define pid (cadr m))
            (define e (catalog-entity cat pid))
            (when e
              (define already? (for/or ([p (in-list (catalog-parents cat sid))]) (equal? (car p) pid)))
              (unless already?
                (catalog-link! cat pid sid "member")
                (define label (t 'sb.n-linked (row-title k) (hash-ref e 'name)))
                (push-undo! label (lambda () (catalog-unlink! cat pid sid) (refresh-sidebar!) (push-catalog-views!)))
                (notice! label)
                (refresh-sidebar!) (push-catalog-views!))))]
      [else (void)]))
  (define (row-title k)                                  ; 侧栏行的标题（提示条与撤销用）
    (let ([r (findf (lambda (se) (equal? (hash-ref se 'key (hash-ref se 'path #f)) k)) (sbp 'sessions))])
      (let ([tt (and r (hash-ref r 'title #f))]) (if (string? tt) tt (t 'sb.untitled)))))
  (define (session-action! a)
    (define (blocked? [id active])
      (and (host-busy? id) (begin (set! toast (cons (t 'app.blocked-busy) (+ (current-inexact-milliseconds) 2000.0))) #t)))
    (define (host-current-path id) (for/first ([se (in-list (hs-sessions (H id)))] #:when (hash-ref se 'current #f)) (hash-ref se 'path #f)))
    (case (car a)
      [(new) (unless (blocked?) (send! (hasheq 'type "new_session")))]
      [(new-chat) (unless (blocked?) (send! (hasheq 'type "new_session" 'chat #t)))]          ; 聊天：匿名工作区
      [(fork) (unless (blocked?) (send! (hasheq 'type "fork")))]
      [(resume)
       (define tg (target (cadr a)))
       (switch-host! (car tg))
       (unless (or (equal? (cdr tg) (host-current-path (car tg))) (blocked? (car tg)))
         (send! (hasheq 'type "resume" 'path (cdr tg)) (car tg)))]
      ;; 删除：一次就删（进回收站），底部提示条可撤销；不再确认（§11 A）。目录里的边留着（恢复后照旧；行只在会话还在时显示）
      [(delete)
       (define tg (target (cadr a)))
       (cond
         [(equal? (cdr tg) (host-current-path (car tg))) (set! toast (cons (t 'sb.n-cannot-delete-current) (+ (current-inexact-milliseconds) 2500.0)))]
         [else
          (define title (row-title (cadr a)))
          (send! (hasheq 'type "delete_session" 'path (cdr tg)) (car tg))
          (push-undo! (t 'sb.n-deleted title) (lambda () (send! (hasheq 'type "restore_session" 'path (cdr tg)) (car tg))))
          (notice! (t 'sb.n-deleted title))])]
      [(undo) (undo!)]
      [(group-by) (set! group-mode (cadr a)) (save-setting! 'sidebar-group (cadr a)) (sbp 'set-group-mode! (cadr a)) (refresh-sidebar!)]
      ;; 置顶 / 归档：存在目录 catalog 的会话条目上（W2）；可撤销
      [(pin unpin archive unarchive)
       (define tg (target (cadr a)))
       (define sid (session-id (car tg) (cdr tg)))
       (define attr (if (memq (car a) '(pin unpin)) 'pinned 'archived))
       (define old (catalog-session-attr cat sid attr #f))
       (define title (row-title (cadr a)))
       (catalog-set-session! cat sid #:host (car tg) #:path (cdr tg) #:attrs (hasheq attr (and (memq (car a) '(pin archive)) #t)))
       (define label (t (case (car a) [(pin) 'sb.n-pinned] [(unpin) 'sb.n-unpinned] [(archive) 'sb.n-archived] [else 'sb.n-unarchived]) title))
       (push-undo! label (lambda () (catalog-set-session! cat sid #:attrs (hasheq attr old)) (refresh-sidebar!)))
       (notice! label)
       (refresh-sidebar!)
       (push-catalog-views!)]
      ;; 这个会话能不能用项目索引：缺省（本机模型能、云端不能）→ 开 → 关 → 缺省；可撤销
      [(host-index)
       (define tg (target (cadr a)))
       (define sid (session-id (car tg) (cdr tg)))
       (define old (catalog-session-attr cat sid 'host_index 'unset))
       (define new (case old [(#t) #f] [(#f) 'unset] [else #t]))
       (catalog-set-session! cat sid #:host (car tg) #:path (cdr tg) #:attrs (hasheq 'host_index new))
       (define label (t 'hix.n-mode (row-title (cadr a)) (t (case new [(#t) 'sb.m-hix-on] [(#f) 'sb.m-hix-off] [else 'sb.m-hix-auto]))))
       (push-undo! label (lambda () (catalog-set-session! cat sid #:attrs (hasheq 'host_index old)) (refresh-sidebar!) (push-catalog-views!)))
       (notice! label)
       (refresh-sidebar!)
       (push-catalog-views!)]
      ;; 这个会话读 / 写记忆（缺省开；存在目录 catalog 的会话条目上，推给内核）；可撤销
      [(mem-read mem-write)
       (define tg (target (cadr a)))
       (define sid (session-id (car tg) (cdr tg)))
       (define attr (if (eq? (car a) 'mem-read) 'mem_read 'mem_write))
       (define old (catalog-session-attr cat sid attr #t))
       (define on? (eq? old #f))                                                   ; 现在关 → 打开；否则关掉
       (catalog-set-session! cat sid #:host (car tg) #:path (cdr tg) #:attrs (hasheq attr on?))
       (define label (t (if (eq? attr 'mem_read) (if on? 'mem.n-read-on 'mem.n-read-off) (if on? 'mem.n-write-on 'mem.n-write-off)) (row-title (cadr a))))
       (push-undo! label (lambda () (catalog-set-session! cat sid #:attrs (hasheq attr old)) (refresh-sidebar!) (push-catalog-views!)))
       (notice! label)
       (refresh-sidebar!)
       (push-catalog-views!)]
      [(toggle-group)                                                              ; 按显示着的状态取反（各组缺省不同：时间段开、更早 / 已归档收）
       (define r (findf (lambda (se) (equal? (hash-ref se 'key #f) (cadr a))) (sbp 'sessions)))
       (hash-set! open-groups (cadr a) (not (and r (hash-ref r 'open #f))))
       (refresh-sidebar!)]
      [(host-new) (switch-host! (cadr a)) (unless (blocked? (cadr a)) (send! (hasheq 'type "new_session") (cadr a)))]
      ;; 右键菜单（design-workspace.md W1）
      [(rename) (let ([tg (target (cadr a))] [old (row-title (cadr a))])
                  (send! (hasheq 'type "set_title" 'path (cdr tg) 'title (caddr a)) (car tg))
                  (push-undo! (t 'sb.n-renamed (caddr a)) (lambda () (send! (hasheq 'type "set_title" 'path (cdr tg) 'title old) (car tg))))
                  (notice! (t 'sb.n-renamed (caddr a))))]
      [(info) (let ([tg (target (cadr a))]) (send! (hasheq 'type "session_stats" 'path (cdr tg)) (car tg)))]
      [(tests) (open-tests!)]                                                      ; 右键「测试…」（当前会话的工作区）
      [(export) (let ([tg (target (cadr a))]) (send! (hasheq 'type "export_session" 'path (cdr tg) 'format (caddr a)) (car tg)))]
      [(copy-path) (let ([tg (target (cadr a))])
                     (clip 'set! (cdr tg))
                     (set! toast (cons (t 'app.copied (string-length (cdr tg))) (+ (current-inexact-milliseconds) 1500.0))))]
      [(reveal) (let ([tg (target (cadr a))])
                  (cond [(not (local-host? (car tg))) (set! toast (cons (t 'app.revealed) (+ (current-inexact-milliseconds) 2000.0)))]
                        [replay-path (void)]                                         ; 回放：不开 Finder
                        [else (void (thread (lambda () (system* "/usr/bin/open" "-R" (cdr tg)))))]))]
      [(host-reconnect) (reconnect-host! (cadr a))]
      [else (void)]))
;; ---- 主机管理（设置页「主机」）。服务器主机存 {config-home}/hosts.rktd（与内核共用：spawn_agent 的 host 参数也从这里取）
  (define (save-hosts! hs) (unless replay-path (save-hosts-file! hs)))            ; 回放不写
  (define (add-ssh-host! target name)
    (define sp (ssh-host-spec target name))
    (cond [(H (car sp)) (set! toast (cons (t 'app.host-exists (car sp)) (+ (current-inexact-milliseconds) 2000.0)))]
          [else (add-host! (car sp) (cadr sp) (caddr sp))
                (save-hosts! (append (filter (lambda (x) (not (equal? (hash-ref x 'id #f) (car sp)))) (load-hosts-file))
                                     (list (hasheq 'id (car sp) 'ssh target 'serve name))))
                (refresh-sidebar!)]))
  (define (disconnect-host! id)
    (define h (H id))
    (when (and h ks)
      (ks 'remove! id 2.0)
      (set-hs-core-exit! h "disconnected")
      (set-hs-manual?! h #t)
      (set-hs-retry-at! h #f)
      (refresh-sidebar!)))
  (define (remove-host! id)
    (define h (H id))
    (when (and h (not (equal? id "local")))
      (when ks (ks 'remove! id 2.0))
      (hash-remove! host-tab id)
      (set! host-order (remove id host-order))
      (tr 'drop-doc! id)
      (when (equal? id active)
        (define next (if (pair? host-order) (car host-order) "local"))
        (unless (H next) (hash-set! host-tab next (make-hs next next #f)))
        (set! active "")                                  ; 让 switch-host! 换过去
        (switch-host! next))
      (save-hosts! (filter (lambda (x) (not (equal? (hash-ref x 'id #f) id))) (load-hosts-file)))
      (refresh-sidebar!)))
  ;; 重连（连接断了的主机）：清掉它的对话文档（守护进程会补齐），重新起传输
  ;; auto?：断线自动重连（不清手动标记与失败计数）；用户点的（侧栏标题行 / 设置页「连接」）清掉
  (define (reconnect-host! id #:auto? [auto? #f])
    (define h (H id))
    (when (and h ks (or (hs-config h) replay-path))
      (unless auto? (set-hs-manual?! h #f) (set-hs-attempts! h 0))
      (set-hs-retry-at! h #f)
      (set-hs-core-exit! h #f) (set-hs-ready! h #f) (set-hs-replayed?! h #f)
      (tr 'reset-doc! id)
      (ks 'add! id (hs-config h))
      (refresh-sidebar!)))
  ;; 到点的自动重连（实时）：先在日志里记一条 gui_reconnect（回放靠它在同一位置重连，回放没有定时器）
  (define (fire-reconnects!)
    (unless replay-path
      (for ([id (in-list host-order)])
        (define h (H id))
        (when (and h (hs-retry-at h) (>= (current-inexact-milliseconds) (hs-retry-at h)))
          (jr 'record! 'core (host-tag id (hasheq 'type "gui_reconnect" 'attempt (hs-attempts h))))
          (reconnect-host! id #:auto? #t)))))
  ;; 模型菜单动作 → 内核请求。轮进行中内核会排队到轮结束再切
  (define (picker-action! a)
    (define (later!) (when (busy?) (set! toast (cons (t 'app.after-turn) (+ (current-inexact-milliseconds) 2000.0)))))
    (case (car a)
      [(toggle) (when (mpp 'open?) (send! (hasheq 'type "list_providers")))]
      [(provider) (later!) (send! (hasheq 'type "set_provider" 'name (cadr a)))]
      [(model) (later!) (send! (hasheq 'type "set_model" 'model (cadr a)))]
      [(reasoning) (send! (hasheq 'type "set_reasoning" 'level (cadr a)))]
      [(member) (later!) (send! (hasheq 'type "route_pin" 'index (cadr a)))]
      [(adapt) (send! (hasheq 'type "set_adaptation" 'name (cadr a) 'enabled (caddr a)))]
      [else (void)]))
  ;; ---- 设置页（⌘,）
  (define (open-settings!)
    (svp 'open!)
    (push-hosts-data!)
    (svp 'set-data! 'lang (gsetting 'lang 'system))
    (svp 'set-data! 'secret-mode secret-mode)
    (svp 'set-data! 'secret-patterns secret-patterns)
    (svp 'set-data! 'zoom zoom)
    (svp 'set-data! 'tool-preview (gsetting 'tool-preview #f))
    (svp 'set-data! 'stream-checkpoint (gsetting 'stream-checkpoint #f))
    (svp 'set-data! 'sidebar-side (if sidebar-right? 'right 'left))
    (svp 'set-data! 'sidebar-material material)
    (svp 'set-data! 'auto-title (not (eq? (gsetting 'auto-title #t) #f)))
    (svp 'set-data! 'hidden-text-mode (hidden-text-mode))
    (svp 'set-data! 'sidebar-older-days (let ([v (gsetting 'sidebar-older-days 14)]) (if (exact-positive-integer? v) v 14)))
    (svp 'set-data! 'chat-usage #f)
    (push-instructions-data!)
    (push-memory-data!)
    (push-hix-data!)
    (svp 'set-data! 'changesets (for/list ([cs (in-list (catalog-changesets-list cat))])   ; 变更记录
                                  (define p (hash-ref cs 'proposal))
                                  (hasheq 'id (hash-ref p 'id) 'when (hash-ref p 'when "") 'by (hash-ref p 'by "?")
                                          'n (length (hash-ref p 'items '())) 'state (changeset-state cs) 'summary (hash-ref p 'summary ""))))
    (send! (hasheq 'type "get_chat_config"))
    (svp 'set-data! 'journal (and (jr 'path) (path->string (jr 'path))))
    (send! (hasheq 'type "list_providers"))
    (send! (hasheq 'type "list_adaptations"))
    (send! (hasheq 'type "state")))
  ;; 设置页「指令」的数据：本机全局文件（有 override 就是它）+ 目录 catalog 里的项目 / super-workspace / 工作区（不含聊天工作区）
  (define (global-instructions-path)
    (define o (build-path (gui-config-home) "AGENTS.override.md"))
    (if (file-exists? o) o (build-path (gui-config-home) "AGENTS.md")))
  (define (push-instructions-data!)
    (define p (global-instructions-path))
    (svp 'set-data! 'instr-global (hasheq 'path (path->string p) 'bytes (and (file-exists? p) (file-size p))))
    (svp 'set-data! 'instr-entities
         (for*/list ([kind (in-list '("project" "super-workspace" "workspace"))]
                     [e (in-list (sort (catalog-entities-of cat kind) string<? #:key (lambda (e) (hash-ref e 'name))))]
                     #:unless (hash-ref (hash-ref e 'attrs (hasheq)) 'chat #f))
           (hasheq 'id (hash-ref e 'id) 'kind kind 'name (hash-ref e 'name) 'text (or (catalog-entity-instructions e) "")))))
  (define (settings-action! a)
    (case (car a)
      ;; 项目索引（design-memory.md §8）
      [(hix-pause) (save-setting! 'host-index-paused (cadr a)) (push-hix-data!) (unless (cadr a) (hix-pump!))]
      [(hix-rescan) (for ([id (in-list host-order)] #:when (and (H id) (hs-ready (H id))))
                      (send! (hasheq 'type "host_scan" 'roots (let ([r (gsetting 'host-index-roots #f)]) (if (and (list? r) (pair? r)) r '("~")))) id))]
      [(hix-regenerate hix-edit)
       (define parts (regexp-split #rx"\u0001" (cadr a)))
       (when (= (length parts) 2)
         (if (eq? (car a) 'hix-regenerate)
             (hix-regenerate! hix (car parts) (cadr parts))
             (hix-edit! hix (car parts) (cadr parts) (text->desc (caddr a))))
         (after-hix-change! (car parts))
         (hix-pump!))]
      [(hix-endpoint hix-model)
       (define old (let ([v (gsetting 'host-index-model #f)]) (if (hash? v) v (hasheq))))
       (save-setting! 'host-index-model (if (cadr a) (hash-set old (if (eq? (car a) 'hix-endpoint) 'endpoint 'model) (cadr a))
                                            (hash-remove old (if (eq? (car a) 'hix-endpoint) 'endpoint 'model))))
       (push-hix-data!)]
      ;; 记忆（design-memory.md M2）
      [(memory-global-auto) (save-setting! 'memory-global-auto (cadr a)) (after-memory-change!)]
      [(memory-auto) (save-setting! 'memory-auto (cadr a)) (push-memory-data!) (when (cadr a) (mem-auto-pump!))]
      [(memory-accept) (memory-accept! mem (cadr a)) (after-memory-change!)]
      [(memory-reject) (memory-reject! mem (cadr a)) (after-memory-change!)]
      [(memory-pin) (memory-pin! mem (cadr a) (caddr a)) (after-memory-change!)]
      [(memory-edit) (memory-edit! mem (cadr a) (caddr a)) (after-memory-change!)]
      [(memory-delete)
       (define e (memory-entry mem (cadr a)))
       (memory-delete! mem (cadr a))
       (when e
         (define label (t 'mem.n-deleted (short-text (hash-ref e 'text ""))))
         (push-undo! label (lambda () (memory-restore! mem (cadr a)) (after-memory-change!)))
         (notice! label))
       (after-memory-change!)]
      [(set-instructions)                                                         ; 指令：存进目录 catalog（空 = 清掉），推给各台内核（下一轮生效）
       (catalog-update-entity! cat (cadr a) #:attrs (hasheq 'instructions (caddr a)))
       (push-instructions-data!) (push-catalog-views!)]
      [(open-global-instructions)                                                 ; 本机全局文件：没有就建空文件，用系统的文本编辑器打开
       (unless replay-path
         (define p (global-instructions-path))
         (with-handlers ([exn:fail? (lambda (e) (eprintf "pi2: open instructions failed: ~a\n" (exn-message e)))])
           (unless (file-exists? p) (make-directory* (gui-config-home)) (call-with-output-file p void))
           (define-values (sp o i er) (subprocess #f #f #f (find-executable-path "open") "-t" (path->string p)))
           (close-output-port i) (close-input-port o) (close-input-port er))
         (push-instructions-data!))]
      [(lang) (save-setting! 'lang (cadr a))
              (current-lang (resolve-lang (cadr a)))
              (svp 'set-data! 'lang (cadr a))]
      [(permission) (send! (hasheq 'type "set_permission_mode" 'mode (cadr a)))]
      [(secret-mode) (save-setting! 'secret-mode (cadr a)) (set! secret-mode (cadr a)) (svp 'set-data! 'secret-mode (cadr a))]
      [(zoom) (set-zoom! (valid-zoom (cadr a)))]
      [(set-chat-config) (send! (hash-merge (hasheq 'type "set_chat_config") (cadr a)))]
      [(chat-cleanup) (send! (hasheq 'type "chat_cleanup"))]
      [(open-changeset) (open-review! (cadr a))]                                    ; 变更记录 → 审阅面板
      [(set-preview) (save-setting! 'tool-preview (cadr a)) (tr 'set-preview! (cadr a)) (svp 'set-data! 'tool-preview (cadr a))]
      [(sidebar-side)
       (set! sidebar-right? (eq? (cadr a) 'right)) (save-setting! 'sidebar-side (cadr a)) (svp 'set-data! 'sidebar-side (cadr a))]
      [(auto-title) (save-setting! 'auto-title (cadr a)) (svp 'set-data! 'auto-title (cadr a))]
      [(hidden-text-mode)                                                         ; 高级：隐藏文字送给模型时标出 / 原样（推给每台内核）
       (save-setting! 'hidden-text-mode (cadr a)) (svp 'set-data! 'hidden-text-mode (cadr a))
       (for ([id (in-list host-order)]) (send! (hasheq 'type "set_model_input" 'hidden_text (symbol->string (cadr a))) id))]
      [(sidebar-older-days)
       (save-setting! 'sidebar-older-days (cadr a)) (svp 'set-data! 'sidebar-older-days (cadr a)) (refresh-sidebar!)]
      [(sidebar-material)
       (set! material (cadr a)) (save-setting! 'sidebar-material (cadr a)) (svp 'set-data! 'sidebar-material (cadr a))]
      [(stream-checkpoint)
       (save-setting! 'stream-checkpoint (cadr a)) (svp 'set-data! 'stream-checkpoint (cadr a))
       (for ([id (in-list host-order)]) (send! (hasheq 'type "set_stream_checkpoint" 'seconds (or (cadr a) 'null)) id))]
      [(add-host) (add-ssh-host! (cadr a) (caddr a))]
      [(add-secret-pattern remove-secret-pattern)
       (set! secret-patterns (if (eq? (car a) 'add-secret-pattern)
                                 (append (remove (cadr a) secret-patterns) (list (cadr a)))
                                 (remove (cadr a) secret-patterns)))
       (unless replay-path (save-secret-patterns! secret-patterns))
       (svp 'set-data! 'secret-patterns secret-patterns)]
      [(host-connect) (reconnect-host! (cadr a))]
      [(host-disconnect) (disconnect-host! (cadr a))]
      [(host-remove) (remove-host! (cadr a))]
      [(host-credential) (update-host-entry! (cadr a) 'credential (caddr a))]
      [(host-workdir) (update-host-entry! (cadr a) 'workdir (caddr a))]
      [(set-budget)
       (cond [(eq? (cadr a) 'combined)                                  ; 所有主机合计：GUI 自己存，推给各台
              (save-setting! 'combined-daily-usd (caddr a))
              (save-setting! 'combined-snooze-date #f)
              (push-external!) (push-budget-all!)]
             [else (send! (hasheq 'type "set_budget" (if (eq? (cadr a) 'daily) 'daily_usd 'turn_usd) (or (caddr a) 'null)))])]
      [(rule) (send! (hasheq 'type "set_adaptation" 'name (cadr a) 'enabled (caddr a)))]
      [(add-key) (send! (hasheq 'type "add_key" 'base (cadr a) 'label (caddr a) 'token (cadddr a)))]
      [(delete-route) (send! (hasheq 'type "define_route" 'name (cadr a) 'members '()))]
      [(define-route) (send! (hasheq 'type "define_route" 'name (cadr a)
                                     'members (for/list ([m (in-list (caddr a))]) (hasheq 'instance (car m) 'model (cdr m)))))]
      [(set-alias) (send! (hasheq 'type "set_alias" 'name (cadr a) 'model (caddr a)))]
      [(save-rule) (send! (let ([base (hasheq 'type "save_adaptation" 'name (cadr a) 'system_append (cadddr a))])
                            (if (caddr a) (hash-set base 'model (caddr a)) base)))]
      [(delete-rule) (send! (hasheq 'type "delete_adaptation" 'name (cadr a)))]
      [(section) (case (cadr a)
                   [(aliases) (send! (hasheq 'type "list_aliases"))]
                   [(budget) (send! (hasheq 'type "get_budget"))]
                   [(rules) (send! (hasheq 'type "list_adaptations"))]
                   [(pools accounts) (send! (hasheq 'type "list_providers"))]
                   [else (void)])]
      [else (void)]))
  ;; 设置页开着时的键盘：输入框在编辑 → 编辑器（回车提交、Esc 取消）；否则 Esc 关设置页，其它忽略
  (define (settings-edit! ev)
    (define fe (svp 'editor))
    (cond
      [(not fe)
       (when (and (ev:command? ev) (eq? (ev:command-name ev) 'cancelOperation:)) (svp 'close!))]
      [(ev:secret? ev) (editor-command! fe 'selectAll:) (editor-insert! fe (ev:secret-text ev))]   ; 回放：句柄填进输入框
      [(ev:text? ev) (editor-insert! fe (ev:text-str ev))]
      [(ev:marked? ev) (if (string=? (ev:marked-str ev) "") (editor-unmark! fe) (editor-set-marked! fe (ev:marked-str ev) (ev:marked-sel-start ev)))]
      [(ev:paste? ev) (when (ev:paste-text ev) (editor-insert! fe (string-trim (ev:paste-text ev))))]   ; 不折叠成 chip
      [(ev:command? ev)
       (case (ev:command-name ev)
         [(insertNewline:) (if (and (svp 'multiline?) (or (memq 'shift last-mods) (memq 'alt last-mods)))
                               (editor-insert! fe "\n")                                   ; ⇧Enter：多行输入里换行
                               (let ([a (svp 'submit!)]) (when a (settings-action! a))))]
         [(insertNewlineIgnoringFieldEditor: insertLineBreak:) (when (svp 'multiline?) (editor-insert! fe "\n"))]
         [(cancelOperation:) (svp 'cancel-edit!)]
         [(copy: cut:) (unless (svp 'secret?) (editor-command! fe (ev:command-name ev)))]   ; key 不进剪贴板
         [else (editor-command! fe (ev:command-name ev))])])
    #t)
  ;; 侧栏行上的改名输入框
  (define (sidebar-edit! ev)
    (define fe (sbp 'editor))
    (cond
      [(ev:text? ev) (editor-insert! fe (ev:text-str ev))]
      [(ev:marked? ev) (if (string=? (ev:marked-str ev) "") (editor-unmark! fe) (editor-set-marked! fe (ev:marked-str ev) (ev:marked-sel-start ev)))]
      [(ev:paste? ev) (when (ev:paste-text ev) (editor-insert! fe (regexp-replace* #rx"[\r\n\t]+" (ev:paste-text ev) " ")))]
      [(ev:command? ev)
       (case (ev:command-name ev)
         [(insertNewline:) (let ([a (sbp 'submit!)]) (when a (session-action! a)))]
         [(cancelOperation:) (sbp 'cancel-edit!)]
         [else (editor-command! fe (ev:command-name ev))])]
      [else (void)])
    #t)
  ;; 窗口编辑事件 → 编辑器 / 内核
  (define (handle-edit! ev)
    (set! last-edit (current-inexact-milliseconds))
    (unless (and (ev:command? ev) (memq (ev:command-name ev) '(undo: redo:))) (set! last-text-op (current-inexact-milliseconds)))
    (cond
      ;; 设置页开着：键盘归它（输入框在编辑 → 编辑器；否则只认 Esc 关闭）
      [(svp 'open?) (settings-edit! ev)]
      [(sbp 'editing?) (sidebar-edit! ev)]                                        ; 侧栏改名
      ;; ⌘Z：侧栏有焦点，或最近的是会话操作（提示条还在、之后没打过字）→ 撤销会话操作；否则撤销文字
      [(and (ev:command? ev) (eq? (ev:command-name ev) 'undo:) (pair? undo-stack)
            (or sidebar-focus? (and notice-until (> last-session-op last-text-op))))
       (undo!)]
      [(and (insp 'open?) (ev:command? ev) (eq? (ev:command-name ev) 'cancelOperation:)) (insp 'close!)]   ; Esc：关信息面板
      [(and (rvp 'open?) (ev:command? ev) (eq? (ev:command-name ev) 'cancelOperation:)) (review-action! (list 'close))]   ; Esc：关审阅面板
      [(and (tpp 'open?) (ev:command? ev) (eq? (ev:command-name ev) 'cancelOperation:)) (tests-action! (list 'close))]    ; Esc：关测试面板
      [(ev:text? ev) (editor-insert! e (ev:text-str ev)) (set! ime-log (cons (list 'text (ev:text-str ev)) ime-log))]
      [(ev:marked? ev)
       (set! ime-log (cons (list 'marked (ev:marked-str ev)) ime-log))
       (if (string=? (ev:marked-str ev) "") (editor-unmark! e) (editor-set-marked! e (ev:marked-str ev) (ev:marked-sel-start ev)))]
      [(ev:paste? ev) (when (ev:paste-text ev)
                        (if (paste-as-secret? (ev:paste-text ev))
                            (insert-secret-chip! (ev:paste-text ev) (eq? secret-mode 'suggest))
                            (editor-paste! e (ev:paste-text ev))))]
      [(ev:secret? ev) (insert-secret-chip! (ev:secret-text ev) (equal? (ev:secret-label ev) "chat-pending"))]   ; 回放
      [(ev:attach? ev) (attach! (ev:attach-items ev))]
      [(ev:command? ev)
       (define c (ev:command-name ev))
       (case c
         [(insertNewline:) (if (or (memq 'shift last-mods) (memq 'alt last-mods)) (editor-insert! e "\n") (submit!))]
         [(moveUp: moveUpAndModifySelection:)
          (when (eq? 'top (ib 'move-vertical! -1 (eq? c 'moveUpAndModifySelection:))) (editor-history-prev! e))]
         [(moveDown: moveDownAndModifySelection:)
          (when (eq? 'bottom (ib 'move-vertical! 1 (eq? c 'moveDownAndModifySelection:))) (editor-history-next! e))]
         [else
          ;; ⌘C：对话区有选区时复制对话区（按看到的文字），否则交给编辑器
          (define tsel (and (eq? c 'copy:) (tr 'selection-text)))
          (define menu-esc? (and (eq? c 'cancelOperation:) (or (mpp 'open?) (sbp 'context-open?))))
          (when menu-esc? (mpp 'close!) (sbp 'close-context!))                          ; Esc：先关菜单（模型菜单 / 侧栏上下文菜单）
          (define r (cond [menu-esc? 'handled] [tsel (cons 'copy tsel)] [else (editor-command! e c)]))
          (cond
            [(and (pair? r) (eq? (car r) 'copy))
             (clip 'set! (cdr r))
             (when tsel (set! toast (cons (t 'app.copied (string-length tsel)) (+ (current-inexact-milliseconds) 1500.0))))]
            [(and (eq? r 'ignored) (eq? c 'cancelOperation:)) (interrupt!)])])])     ; Esc：无可取消的编辑状态时停止当前轮
    #t)
  ;; P5：拖动缩放时 AppKit 在嵌套 run loop 里回调 windowDidResize:，这里同步重绘一帧（内核在独立 place，不受影响）
  (win 'on-live-resize! (lambda (nw nh ns) (apply-size! nw nh ns) (render!)))
  (apply-size! w h scale)
  (jr 'record! 'start (hasheq 'w pw 'h ph 'scale ps 'zoom zoom 'lang (symbol->string (current-lang))
                              'glass (and (eq? material 'glass) (win 'glass?) #t)   ; 内缩排版（透出桌面是整列，与不透明同排）
                              'catalog (catalog-snapshot cat)                        ; 目录的当前状态（置顶 / 归档影响侧栏的行 → 命中）
                              'memory (memory-snapshot mem)                          ; 记忆库的当前状态（推给内核的视图要一致）
                              'host-index (hix-snapshot hix) 'host-index-active hix-active?   ; 项目描述库（同上）；录制时扫不扫（回放照做）
                              'now (current-seconds)                                 ; 侧栏按时间分段：回放用这个时刻
                              'hosts (for/list ([id (in-list host-order)]) (hasheq 'id id 'label (hs-label (H id)))) 'active active
                              'settings (for/hasheq ([k (in-list SETTINGS-IN-JOURNAL)]) (values k (gsetting k #f)))))
  (svp 'set-data! 'host-label (and (multi?) (hs-label (A))))
  (define resize-at (and resize-to (+ t0 300.0)))
  ;; ---- 冒烟 / 验收脚本：(条件 . 动作)，条件满足时执行一次
  (define synth-check #f)
  (define ime-query #f)
  (define paste-path #f)                               ; menu：真 ⌘V 经 Edit 菜单；direct：无 key 窗口时直接投递
  (define (post-text! s) (for ([c (in-string s)]) (wi 'post-key! (string c) 0)))
  (define (enter!) (wi 'post-key! "\r" 36))
  (define (quit!) (wi 'post-key! "q" 12 '(cmd)))
  (define (after ms) (lambda () (>= (- (current-inexact-milliseconds) t0) ms)))
  (define (click-at! pt) (wi 'post-mouse! 'down (* zoom (car pt)) (* zoom (cdr pt))) (wi 'post-mouse! 'up (* zoom (car pt)) (* zoom (cdr pt))))
  ;; 脚本步骤条件：上一步之后至少过了一整轮（合成事件已处理、已重绘）。fire 会连续执行条件为真的步骤，
  ;; 「立刻为真」的条件会在上一步投递的事件被处理之前就判断
  (define loop-iter 0)
  (define last-fire-iter -1)
  (define (next-iteration?) (> loop-iter last-fire-iter))
  (define (snapshot-and-quit!) (when snap-path (render!) (snapshot-now! snap-path)) (quit!))
  (define synth-steps
    (cond
      [synth-core?
       (list (cons (lambda () ready) (lambda () (post-text! "please echo something") (enter!)))
             (cons (lambda () (let ([ids (tr 'pending-permissions)]) (and (pair? ids) (tr 'hit-center 'perm (cons (car ids) "yes")))))
                   (lambda () (click-at! (tr 'hit-center 'perm (cons (car (tr 'pending-permissions)) "yes")))))
             (cons (lambda () (>= (tr 'turns) 1)) (lambda () (post-text! "now hang") (enter!)))
             (cons (lambda () (and (busy?) (let ([s (tr 'last-assistant-text)]) (and s (regexp-match? #rx"partial" s)))))
                   (lambda () (wi 'post-key! (string (integer->char 27)) 53)))                    ; Esc → 停止
             (cons (lambda () (>= (tr 'turns) 2)) (lambda () (script-send! (hasheq 'type "history"))))
             (cons (lambda () kernel-history) snapshot-and-quit!))]
      [synth-reattach?
       ;; 无头模式：连上常驻内核 → 对话区补齐上一个客户端的对话 → 再发一轮
       (define (log! k v) (set! sessions-log (cons (cons k v) sessions-log)))
       (list (cons (lambda () replayed?)
                   (lambda () (log! 'replayed (tr 'summary)) (log! 'attached (and ready (hash-ref ready 'attached #f)))
                              (post-text! "after reattach") (enter!)))
             (cons (lambda () (and (next-iteration?) (not (busy?)) (let ([s (tr 'last-assistant-text)]) (and s (regexp-match? #rx"after reattach" s)))))
                   (lambda () (log! 'answer (tr 'last-assistant-text)) (snapshot-and-quit!))))]
      [synth-sessions?
       ;; 会话侧栏（G5）：会话 A 发一轮 → 新会话 B 发一轮 → 点回 A（对话区换成 A 的历史）→ 分叉 C
       ;; → ctrl-点击 B 开上下文菜单、点「删除会话…」、再点「确认删除」→ 列表剩 A、C
       (define a-path #f)
       (define b-path #f)
       (define c-path #f)
       (define (cur) (for/first ([se (in-list (sbp 'sessions))] #:when (hash-ref se 'current #f)) se))
       (define (cur-path) (let ([c (cur)]) (and c (hash-ref c 'path #f))))
       (define (cur-msgs) (let ([c (cur)]) (if c (hash-ref c 'messages 0) -1)))
       (define (paths) (for/list ([se (in-list (sbp 'sessions))] #:when (hash-ref se 'path #f)) (hash-ref se 'path)))   ; 只要会话行（分段 / 分组行没有路径）
       (define (log! k v) (set! sessions-log (cons (cons k v) sessions-log)))
       (define (click-sb! kind payload) (click-at! (sbp 'hit-center kind payload)))
       (list (cons (lambda () (and ready (cur))) (lambda () (post-text! "alpha question") (enter!)))
             (cons (lambda () (and (not (busy?)) (>= (cur-msgs) 2)))
                   (lambda () (set! a-path (cur-path)) (click-sb! 'new #t)))
             (cons (lambda () (and (cur-path) (not (equal? (cur-path) a-path)) (= (cur-msgs) 0)))
                   (lambda () (set! b-path (cur-path)) (log! 'new-empty (tr 'summary)) (post-text! "beta question") (enter!)))
             (cons (lambda () (and (not (busy?)) (equal? (cur-path) b-path) (>= (cur-msgs) 2) (sbp 'hit-center 'resume a-path)))
                   (lambda () (log! 'b-answer (tr 'last-assistant-text)) (click-sb! 'resume a-path)))
             (cons (lambda () (equal? (cur-path) a-path))
                   (lambda () (log! 'resumed-answer (tr 'last-assistant-text)) (click-sb! 'fork #t)))
             (cons (lambda () (and (cur-path) (not (member (cur-path) (list a-path b-path)))))
                   (lambda () (set! c-path (cur-path)) (log! 'fork-msgs (cur-msgs))))
             (cons (lambda () (and (= (length (paths)) 3) (sbp 'hit-center 'resume b-path)))   ; 列表里有了 C、行位置已重绘
                   (lambda ()
                     (define pt (sbp 'hit-center 'resume b-path))
                     (wi 'post-mouse! 'down (car pt) (cdr pt) '(ctrl))
                     (wi 'post-mouse! 'up (car pt) (cdr pt) '(ctrl))))
             (cons (lambda () (sbp 'hit-center 'delete b-path)) (lambda () (click-sb! 'delete b-path)))   ; 一次删除（进回收站，可撤销）
             (cons (lambda () (and (= (length (paths)) 2) (not (member b-path (paths)))))
                   (lambda ()
                     (log! 'final (list (and (member a-path (paths)) #t) (equal? (cur-path) c-path) (length (paths))))
                     (snapshot-and-quit!))))]
      [synth-picker?
       ;; 模型菜单（G5/G6）：点芯片 → 菜单（list_providers）→ 推理 high → 同一个菜单里切到 grok（只切配置，不发 prompt）
       ;; → 切回 lmstudio → 选内核后台查到的本地模型 → 点芯片关 → 再开、Esc 关（不触发中断）
       (define (st k) (let ([h (mpp 'state)]) (and h (hash-ref h k #f))))
       (define (log! k v) (set! sessions-log (cons (cons k v) sessions-log)))
       (define (click-mp! kind payload) (click-at! (mpp 'hit-center kind payload)))
       (define (menu-ready? kind payload) (and (mpp 'open?) (mpp 'hit-center kind payload)))
       (define wheel-y0 #f)
       (list (cons (lambda () (and ready (st 'reasoning) (mpp 'hit-center 'toggle #t)))
                   (lambda () (log! 'initial (list (st 'provider) (st 'reasoning))) (click-mp! 'toggle #t)))
             ;; 脚本化内核的当前供应商是 "scripted"；先切到 lmstudio（它的模型列表由内核后台查询，假列表）
             (cons (lambda () (menu-ready? 'provider "lmstudio")) (lambda () (click-mp! 'provider "lmstudio")))
             (cons (lambda () (and (equal? (st 'provider) "lmstudio") (menu-ready? 'provider "grok"))) (lambda () (click-mp! 'reasoning "high")))
             ;; 选中后菜单不关：接着在同一个打开的菜单里切供应商、切回、选模型
             ;; 滚轮跟指针走：菜单开着时在侧栏上滚，菜单不动；在模型栏上滚，模型栏动
             (cons (lambda () (and (equal? (st 'reasoning) "high") (mpp 'hit-center 'model "qwen/qwen3.5-9b")))   ; 等本地模型列表到了（可滚）
                   (lambda ()
                     (log! 'reasoning (list (st 'reasoning) (mpp 'open?)))
                     (set! wheel-y0 (cdr (mpp 'hit-center 'model (st 'model))))
                     (wi 'post-scroll! 100.0 300.0 -200.0)))
             (cons next-iteration?
                   (lambda ()
                     (define c (mpp 'hit-center 'model (st 'model)))
                     (log! 'wheel-over-sidebar-menu-unmoved (and c (= (cdr c) wheel-y0)))
                     (wi 'post-scroll! (car c) (cdr c) -200.0)))
             (cons next-iteration?
                   (lambda ()
                     (define c (mpp 'hit-center 'model (st 'model)))
                     (log! 'wheel-over-models-moved (not (and c (= (cdr c) wheel-y0))))
                     (define any (mpp 'hit-center 'provider "grok"))
                     (wi 'post-scroll! (+ (car any) 220.0) (cdr any) 100000.0)))            ; 模型栏滚回顶
             (cons (lambda () (menu-ready? 'provider "grok")) (lambda () (click-mp! 'provider "grok")))
             (cons (lambda () (and (equal? (st 'provider) "grok") (menu-ready? 'provider "lmstudio")))
                   (lambda () (log! 'switched (list (st 'provider) (st 'model) (mpp 'open?))) (click-mp! 'provider "lmstudio")))
             ;; 本地端点的模型列表由内核后台查询后补发（脚本化内核给假列表）
             (cons (lambda () (and (equal? (st 'provider) "lmstudio") (menu-ready? 'model "qwen/qwen3.5-9b")))
                   (lambda () (log! 'back (list (st 'provider) (st 'model))) (click-mp! 'model "qwen/qwen3.5-9b")))
             (cons (lambda () (equal? (st 'model) "qwen/qwen3.5-9b"))
                   (lambda () (log! 'local-model (st 'model)) (click-mp! 'toggle #t)))          ; 再点芯片：关
             (cons (lambda () (not (mpp 'open?)))
                   (lambda () (log! 'chip-closed #t) (click-mp! 'toggle #t)))                    ; 再打开，试 Esc
             (cons (lambda () (mpp 'open?)) (lambda () (wi 'post-key! (string (integer->char 27)) 53)))
             (cons (lambda () (not (mpp 'open?)))
                   (lambda () (log! 'esc-closed (list (not (mpp 'open?)) (busy?))) (snapshot-and-quit!))))]
      [synth-secret?
       ;; 聊天里粘贴密钥（私有剪贴板）→ 待选密钥 chip（只提示档）→ 点「作为密钥」→ 补几个字发送；
       ;; 脚本化内核把收到的用户文字原样回显：回显里只能有占位符。日志里不得出现原值（冒烟测试查日志文件）
       (define KEY "sk-proj-AbCdEf1234567890XyZ")
       (define (log! k v) (set! sessions-log (cons (cons k v) sessions-log)))
       (define (find-input-button kind)
         (for*/first ([y (in-range 0 (inexact->exact (round h)) 3)] [x (in-range 0 (inexact->exact (round w)) 6)]
                      #:when (let ([r (ib 'hit (exact->inexact x) (exact->inexact y))]) (and r (eq? (car r) 'button) (eq? (cadr r) kind))))
           (cons (exact->inexact x) (exact->inexact y))))
       (list (cons (lambda () ready)
                   (lambda ()
                     (set! secret-mode 'suggest)
                     (clip 'set! KEY)
                     (cond [(wi 'key?) (wi 'post-key! "v" 9 '(cmd))] [else (wi 'post-paste!)])))
             (cons (lambda () (find-input-button 'confirm))
                   (lambda () (log! 'pending-chip #t) (click-at! (find-input-button 'confirm))))
             (cons (lambda () (and (next-iteration?) (not (find-input-button 'confirm))))
                   (lambda () (post-text! " please use it") (enter!)))
             (cons (lambda () (and (>= (tr 'turns) 1) (not (busy?))))
                   (lambda ()
                     (define a (or (tr 'last-assistant-text) ""))
                     (log! 'answer-has-placeholder (regexp-match? #rx"⟦secret:[0-9a-f]+⟧ please use it" a))
                     (log! 'answer-has-key (regexp-match? #rx"AbCdEf" a))
                     (snapshot-and-quit!))))]
      [synth-unicode?
       ;; Unicode 显示策略与 ANSI（design-gui.md §30）：模型回一段带双向控制 / 零宽 / tag / 从右到左 / 控制字符 / 形近字的文字 →
       ;; 再让它跑一个打印 ANSI 颜色的 bash（允许）→ 截图检查对比度（标记、ANSI 颜色都在里面）
       (define (log! k v) (set! sessions-log (cons (cons k v) sessions-log)))
       (define (cur) (findf (lambda (se) (hash-ref se 'current #f)) (sbp 'sessions)))
       (define (answered? n) (and (not (busy?)) (let ([c (cur)]) (and c (>= (hash-ref c 'messages 0) n)))))
       (list (cons (lambda () (and ready (cur))) (lambda () (post-text! "unicode demo") (enter!)))
             (cons (lambda () (answered? 2)) (lambda () (post-text! "ansi demo") (enter!)))
             (cons (lambda () (let ([ids (tr 'pending-permissions)]) (and (pair? ids) (tr 'hit-center 'perm (cons (car ids) "yes")))))
                   (lambda () (click-at! (tr 'hit-center 'perm (cons (car (tr 'pending-permissions)) "yes")))))
             (cons (lambda () (answered? 5))
                   (lambda () (log! 'visual (visual-check "unicode")) (wi 'post-key! "," 43 '(cmd))))
             ;; 高级设置：隐藏文字送给模型时 → 原样（推给内核；回放比对这次发送）→ 换回标出
             (cons (lambda () (and (svp 'open?) (svp 'hit-center 'section 'advanced))) (lambda () (click-at! (svp 'hit-center 'section 'advanced))))
             (cons (lambda () (svp 'hit-center 'hidden-text-mode 'raw)) (lambda () (click-at! (svp 'hit-center 'hidden-text-mode 'raw))))
             (cons (lambda () (eq? (gsetting 'hidden-text-mode #f) 'raw))
                   (lambda () (log! 'hidden-raw (gsetting 'hidden-text-mode #f)) (click-at! (svp 'hit-center 'hidden-text-mode 'mark))))
             (cons (lambda () (eq? (gsetting 'hidden-text-mode #f) 'mark))
                   (lambda () (log! 'hidden-mark (gsetting 'hidden-text-mode #f)) (snapshot-and-quit!))))]
      [synth-history?
       ;; datum 提交的工具卡片（git_history；夹具 PI2_SEED_HISTORY=1，10 个提交）：让模型查历史 → 卡片画成提交行（hash、TYPE 标记、标题、标签片；
       ;; pi2 自动化的标签用强调色）→ 截图检查 → 点标题展开（真实点击）→ 再检查
       (define (log! k v) (set! sessions-log (cons (cons k v) sessions-log)))
       (define (cur) (findf (lambda (se) (hash-ref se 'current #f)) (sbp 'sessions)))
       (define (ns! kind pt [button 0]) (wi 'post-mouse-ns! kind (* zoom (car pt)) (* zoom (cdr pt)) button))
       (define (lclick! pt) (ns! 'down pt) (ns! 'up pt))
       (define frames-at 0)
       (define (after n) (lambda () (> frames (+ frames-at n))))
       (define (step pred act) (cons pred (lambda () (act) (set! frames-at frames))))
       (define (answered? n) (and (not (busy?)) (let ([c (cur)]) (and c (>= (hash-ref c 'messages 0) n)))))
       (list (step (lambda () (and ready (cur))) (lambda () (post-text! "show history") (enter!)))
             (step (lambda () (and (answered? 4) (pair? (tr 'tool-uis)))) (lambda () (log! 'cards (tr 'tool-uis))))
             (step (lambda () (and ((after 4)) (tr 'hit-center 'tool "call-git_history")))
                   (lambda () (log! 'visual (visual-check "history-card")) (lclick! (tr 'hit-center 'tool "call-git_history"))))
             (step (after 6) (lambda () (log! 'visual-expanded (visual-check "history-expanded")) (snapshot-and-quit!))))]
      [synth-tests?
       ;; 工作区测试流程面板（design-goalmode.md §12 T2；夹具 PI2_SEED_TESTFLOW=1）：⌘⇧T 打开 → 截图检查 → 信任 → 全部运行（check 失败、unit 通过）→
       ;; 展开失败的输出 → 截图检查 → Esc → 让模型 tests_propose 加一步 → 再打开 → 提议在 → 批准 → 步骤里有了 lint（真实点击 / 按键）
       (define (log! k v) (set! sessions-log (cons (cons k v) sessions-log)))
       (define (cur) (findf (lambda (se) (hash-ref se 'current #f)) (sbp 'sessions)))
       (define (ns! kind pt [button 0]) (wi 'post-mouse-ns! kind (* zoom (car pt)) (* zoom (cdr pt)) button))
       (define (lclick! pt) (ns! 'down pt) (ns! 'up pt))
       (define frames-at 0)
       (define (after n) (lambda () (> frames (+ frames-at n))))
       (define (step pred act) (cons pred (lambda () (act) (set! frames-at frames))))
       (define (answered? n) (and (not (busy?)) (let ([c (cur)]) (and c (>= (hash-ref c 'messages 0) n)))))
       (define (td k [d #f]) (let ([v (tpp 'data)]) (if (hash? v) (hash-ref v k d) d)))
       (define (cmd-shift-t!) (wi 'post-key! "t" 17 '(cmd shift)))
       (list (step (lambda () (and ready (cur))) (lambda () (cmd-shift-t!)))
             (step (lambda () (and (tpp 'open?) (td 'exists) (tpp 'hit-center 'trust)))
                   (lambda () (log! 'opened (list (map (lambda (s) (hash-ref s 'id)) (td 'steps '())) (td 'trusted)))
                              (log! 'visual (visual-check "tests-panel"))
                              (lclick! (tpp 'hit-center 'trust))))
             (step (lambda () (eq? (td 'trusted) #t)) (lambda () (lclick! (tpp 'hit-center 'run-all))))
             (step (lambda () (pair? (td 'history '())))
                   (lambda () (log! 'ran (for/list ([r (hash-ref (car (td 'history)) 'steps '())]) (list (hash-ref r 'step) (hash-ref r 'ok))))
                              (lclick! (tpp 'hit-center 'toggle "check"))))
             (step (after 4) (lambda () (log! 'visual-output (visual-check "tests-output")) (wi 'post-key! (string (integer->char 27)) 53)))
             (step (lambda () (not (tpp 'open?))) (lambda () (post-text! "propose tests") (enter!)))
             (step (lambda () (answered? 4)) (lambda () (cmd-shift-t!)))
             (step (lambda () (and (tpp 'open?) (pair? (td 'proposals '())) (tpp 'hit-center 'approve (hash-ref (car (td 'proposals)) 'id))))
                   (lambda () (log! 'proposal (hash-ref (car (td 'proposals)) 'reason))
                              (lclick! (tpp 'hit-center 'approve (hash-ref (car (td 'proposals)) 'id)))))
             (step (lambda () (member "lint" (map (lambda (s) (hash-ref s 'id)) (td 'steps '()))))
                   (lambda () (log! 'applied (list (map (lambda (s) (hash-ref s 'id)) (td 'steps '())) (td 'trusted) (null? (td 'proposals '()))))
                              (snapshot-and-quit!))))]
      [synth-automem?
       ;; 自动记忆（design-memory.md M3；PI2_MEM_IDLE_SECONDS=0）：三轮对话（自动记忆关着：什么都不做）→ 设置页「记忆」打开自动记忆（真实点击）→
       ;; 会话文字 → 提取 → 合并（假的本机模型）→ 工作区的那条直接记、全局的那条待确认 → 截图检查 → 再一轮（只多 2 条：不重复处理）→
       ;; 新会话：注入了工作区那条、没注入待确认的
       (define (log! k v) (set! sessions-log (cons (cons k v) sessions-log)))
       (define (cur) (findf (lambda (se) (hash-ref se 'current #f)) (sbp 'sessions)))
       (define (ns! kind pt [button 0]) (wi 'post-mouse-ns! kind (* zoom (car pt)) (* zoom (cdr pt)) button))
       (define (lclick! pt) (ns! 'down pt) (ns! 'up pt))
       (define frames-at 0)
       (define (after n) (lambda () (> frames (+ frames-at n))))
       (define (step pred act) (cons pred (lambda () (act) (set! frames-at frames))))
       (define (answered? n) (and (not (busy?)) (let ([c (cur)]) (and c (>= (hash-ref c 'messages 0) n)))))
       (define (answer) (or (tr 'last-assistant-text) ""))
       (define (texts [state "active"]) (map (lambda (e) (hash-ref e 'text)) (memory-entries mem #:state state)))
       (list (step (lambda () (and ready (cur))) (lambda () (post-text! "one") (enter!)))
             (step (lambda () (answered? 2)) (lambda () (post-text! "two") (enter!)))
             (step (lambda () (answered? 4)) (lambda () (post-text! "three") (enter!)))
             (step (lambda () (answered? 6))
                   (lambda () (log! 'off-nothing (and (null? (texts)) (zero? (hash-count (memory-processed mem))))) (wi 'post-key! "," 43 '(cmd))))
             (step (lambda () (and (svp 'open?) (svp 'hit-center 'section 'memory))) (lambda () (lclick! (svp 'hit-center 'section 'memory))))
             (step (lambda () (svp 'hit-center 'mem-auto #t)) (lambda () (lclick! (svp 'hit-center 'mem-auto #t))))
             (step (lambda () (and (= (hash-count (memory-processed mem)) 1) (not mem-flight)))
                   (lambda () (log! 'learned (list (texts) (texts "pending")
                                                   (map (lambda (e) (list (hash-ref e 'source) (and (hash-ref e 'evidence #f) #t))) (memory-entries mem))))))
             (step (after 4) (lambda () (log! 'visual (visual-check "memory-auto")) (wi 'post-key! (string (integer->char 27)) 53)))
             (step (lambda () (not (svp 'open?))) (lambda () (post-text! "four") (enter!)))
             (step (lambda () (answered? 8))
                   (lambda () (log! 'not-again (list (hash-count (memory-processed mem)) (length (texts)) (not mem-flight)))
                              (lclick! (sbp 'hit-center 'new #t))))
             (step (lambda () (let ([c (cur)]) (and c (hash-ref c 'ephemeral #f) (not (busy?))))) (lambda () (post-text! "show memory") (enter!)))
             (step (lambda () (answered? 2))
                   (lambda () (define a (answer))
                              (log! 'injected (list (regexp-match? #rx"AUTO-WS" a) (regexp-match? #rx"AUTO-GLOBAL" a)))
                              (snapshot-and-quit!))))]
      [synth-hostindex?
       ;; 项目索引（design-memory.md §8）：连上 → 扫描（夹具的临时目录里两个仓库）→ 空闲时一个一个描述（假的本机模型）→
       ;; 问「电商」→ 模型 host_projects 用中文查到 shop → 右键「项目索引」两下（开 → 关）→ 再查被拒 →
       ;; 设置页「项目索引」截图检查 → 编辑 shop 的描述（真实按键）→ 锁定 → 「重新生成」notes → 又描述一次
       (define (log! k v) (set! sessions-log (cons (cons k v) sessions-log)))
       (define (cur) (findf (lambda (se) (hash-ref se 'current #f)) (sbp 'sessions)))
       (define (ns! kind pt [button 0]) (wi 'post-mouse-ns! kind (* zoom (car pt)) (* zoom (cdr pt)) button))
       (define (lclick! pt) (ns! 'down pt) (ns! 'up pt))
       (define (rclick! pt) (ns! 'down pt 1) (ns! 'up pt 1))
       (define frames-at 0)
       (define (after n) (lambda () (> frames (+ frames-at n))))
       (define (step pred act) (cons pred (lambda () (act) (set! frames-at frames))))
       (define (answered? n) (and (not (busy?)) (let ([c (cur)]) (and c (>= (hash-ref c 'messages 0) n)))))
       (define (answer) (or (tr 'last-assistant-text) ""))
       (define (entry name) (for/first ([e (in-list (hix-entries hix))] #:when (equal? (hash-ref e 'name #f) name)) e))
       (define (k-of name) (let ([e (entry name)]) (and e (string-append (hash-ref e 'host) "\u0001" (hash-ref e 'dir)))))
       (define (path0) (hash-ref (cur) 'path))
       (define notes-described #f)
       (list (step (lambda () (and ready (cur) (= (length (hix-entries hix)) 2) (null? (hix-stale hix)) (not hix-flight)))
                   (lambda () (log! 'described (for/list ([e (in-list (hix-entries hix))]) (list (hash-ref e 'name) (and (hash-ref e 'desc #f) (hash-ref (hash-ref e 'desc) 'short)))))
                              (set! notes-described (hash-ref (entry "notes") 'desc-n 0))
                              (post-text! "find projects 电商") (enter!)))
             (step (lambda () (answered? 4))
                   (lambda () (log! 'found (regexp-match? #rx"1 of 2 projects" (answer)))
                              (rclick! (sbp 'hit-center 'resume (path0)))))
             (step (lambda () (sbp 'hit-center 'host-index (path0))) (lambda () (lclick! (sbp 'hit-center 'host-index (path0)))))
             (step (lambda () (eq? (catalog-session-attr cat (session-id active (path0)) 'host_index 'unset) #t)) (lambda () (rclick! (sbp 'hit-center 'resume (path0)))))
             (step (lambda () (sbp 'hit-center 'host-index (path0))) (lambda () (lclick! (sbp 'hit-center 'host-index (path0)))))
             (step (lambda () (eq? (catalog-session-attr cat (session-id active (path0)) 'host_index 'unset) #f))
                   (lambda () (log! 'switch-off #t) (post-text! "find projects shop") (enter!)))
             (step (lambda () (answered? 8))
                   (lambda () (log! 'refused (regexp-match? #rx"turned off for this session" (answer))) (wi 'post-key! "," 43 '(cmd))))
             (step (lambda () (and (svp 'open?) (svp 'hit-center 'section 'projects))) (lambda () (lclick! (svp 'hit-center 'section 'projects))))
             (step (lambda () (svp 'hit-center 'hix-edit (k-of "shop")))
                   (lambda () (log! 'visual (visual-check "projects-settings")) (lclick! (svp 'hit-center 'hix-edit (k-of "shop")))))
             (step (lambda () (svp 'editing?)) (lambda () (log! 'prefilled (regexp-match? #rx"short: e-commerce, 电商" (editor-flatten (svp 'editor)))) (post-text! " mall")))
             (step (after 4) (lambda () (enter!)))
             (step (lambda () (hash-ref (entry "shop") 'locked #f))
                   (lambda () (log! 'edited (last (hash-ref (hash-ref (entry "shop") 'desc) 'long)))
                              (lclick! (svp 'hit-center 'hix-regenerate (k-of "notes")))))
             (step (lambda () (and (> (hash-ref (entry "notes") 'desc-n 0) notes-described) (null? (hix-stale hix)) (not hix-flight)))
                   (lambda () (log! 'regenerated #t) (log! 'still-locked (hash-ref (entry "shop") 'locked #f))
                              (log! 'visual-after (visual-check "projects-settings-after")) (snapshot-and-quit!))))]
      [synth-memory?
       ;; 记忆（design-memory.md M2）：新目录里 /remember（工作区还没有 → 建）、/remember global → 模型回它收到的记忆节（全局在前）→
       ;; 模型 memory_note 到全局 → 待确认 → 设置页「记忆」点「保留」（真实点击）→ 截图检查 → 右键关掉「写记忆」→ 模型再记 → 被拒 →
       ;; 新会话：三条都在 → 右键「信息…」→ 注入的 id
       (define (log! k v) (set! sessions-log (cons (cons k v) sessions-log)))
       (define (cur) (findf (lambda (se) (hash-ref se 'current #f)) (sbp 'sessions)))
       (define (ns! kind pt [button 0]) (wi 'post-mouse-ns! kind (* zoom (car pt)) (* zoom (cdr pt)) button))
       (define (lclick! pt) (ns! 'down pt) (ns! 'up pt))
       (define (rclick! pt) (ns! 'down pt 1) (ns! 'up pt 1))
       (define frames-at 0)
       (define (after n) (lambda () (> frames (+ frames-at n))))
       (define (step pred act) (cons pred (lambda () (act) (set! frames-at frames))))
       (define (texts) (map (lambda (e) (hash-ref e 'text)) (memory-entries mem)))
       (define (answered? n) (and (not (busy?)) (let ([c (cur)]) (and c (>= (hash-ref c 'messages 0) n)))))
       (define (answer) (or (tr 'last-assistant-text) ""))
       (define (pos s k) (let ([m (regexp-match-positions (regexp-quote k) s)]) (and m (caar m))))
       (define (path0) (hash-ref (cur) 'path))
       (list (step (lambda () (and ready (cur))) (lambda () (post-text! "/remember WS-MEM: use pnpm") (enter!)))
             (step (lambda () (member "WS-MEM: use pnpm" (texts)))
                   (lambda () (log! 'ws-scope (let ([e (car (memory-entries mem))]) (hash-ref (catalog-entity cat (hash-ref e 'scope)) 'kind)))
                              (post-text! "/remember global GLOBAL-MEM: reply briefly") (enter!)))
             (step (lambda () (member "GLOBAL-MEM: reply briefly" (texts))) (lambda () (post-text! "show memory") (enter!)))
             (step (lambda () (answered? 2))
                   (lambda () (log! 'injected-order (let ([a (answer)]) (and (pos a "GLOBAL-MEM") (pos a "WS-MEM") (< (pos a "GLOBAL-MEM") (pos a "WS-MEM")))))
                              (post-text! "note pref") (enter!)))
             (step (lambda () (and (answered? 4) (pair? (memory-pending mem))))
                   (lambda () (log! 'pending (map (lambda (e) (hash-ref e 'text)) (memory-pending mem))) (wi 'post-key! "," 43 '(cmd))))
             (step (lambda () (and (svp 'open?) (svp 'hit-center 'section 'memory))) (lambda () (lclick! (svp 'hit-center 'section 'memory))))
             (step (lambda () (let ([p (memory-pending mem)]) (and (pair? p) (svp 'hit-center 'mem-accept (hash-ref (car p) 'id)))))
                   (lambda () (log! 'visual (visual-check "memory-settings"))
                              (lclick! (svp 'hit-center 'mem-accept (hash-ref (car (memory-pending mem)) 'id)))))
             (step (lambda () (null? (memory-pending mem)))
                   (lambda () (log! 'accepted (and (member "MODEL-MEM: indent with tabs" (texts)) #t)) (wi 'post-key! (string (integer->char 27)) 53)))
             (step (lambda () (and (not (svp 'open?)) (sbp 'hit-center 'resume (path0)))) (lambda () (rclick! (sbp 'hit-center 'resume (path0)))))
             (step (lambda () (sbp 'hit-center 'mem-write (path0))) (lambda () (lclick! (sbp 'hit-center 'mem-write (path0)))))
             (step (lambda () (eq? (catalog-session-attr cat (session-id active (path0)) 'mem_write #t) #f))
                   (lambda () (log! 'write-off #t) (post-text! "note pref") (enter!)))
             (step (lambda () (answered? 10))                                          ; 这一轮：用户、工具调用、工具结果、回答 = 第 7–10 条
                   (lambda () (log! 'refused (regexp-match? #rx"writing memory is turned off" (answer)))
                              (log! 'count (length (memory-entries mem)))
                              (lclick! (sbp 'hit-center 'new #t))))
             (step (lambda () (let ([c (cur)]) (and c (hash-ref c 'ephemeral #f) (not (busy?))))) (lambda () (post-text! "show memory") (enter!)))
             (step (lambda () (answered? 2))
                   (lambda () (define a (answer))
                              (log! 'new-session (map (lambda (k) (and (pos a k) #t)) '("GLOBAL-MEM" "MODEL-MEM" "WS-MEM")))
                              (rclick! (sbp 'hit-center 'resume (path0)))))
             (step (lambda () (sbp 'hit-center 'info (path0))) (lambda () (lclick! (sbp 'hit-center 'info (path0)))))
             (step (lambda () (and (insp 'open?) (hash? (hash-ref (insp 'data) 'memory #f))))
                   (lambda () (log! 'info (length (hash-ref (hash-ref (insp 'data) 'memory) 'ids '())))))
             (step (after 4) (lambda () (log! 'visual-info (visual-check "memory-info")) (snapshot-and-quit!))))]
      [synth-instructions?
       ;; 指令分层（design-memory.md M1）：内核这边有全局文件、工作目录（git 根）有 AGENTS.md（夹具种的）→ 设置页「指令」→
       ;; 给当前会话的工作区「添加指令」（真实点击；两行：⇧Enter 换行）→ Enter 存 → 截图检查输入框 → Esc → 让模型回它收到的指令节 →
       ;; 顺序 = 全局 → 工作区 → 目录；信息面板列出这三段 → 截图检查
       (define (log! k v) (set! sessions-log (cons (cons k v) sessions-log)))
       (define (cur) (findf (lambda (se) (hash-ref se 'current #f)) (sbp 'sessions)))
       (define (ns! kind pt) (wi 'post-mouse-ns! kind (* zoom (car pt)) (* zoom (cdr pt)) 0))
       (define (lclick! pt) (ns! 'down pt) (ns! 'up pt))
       (define frames-at 0)
       (define (after n) (lambda () (> frames (+ frames-at n))))
       (define (step pred act) (cons pred (lambda () (act) (set! frames-at frames))))
       (define (ws-id) (let ([c (cur)]) (and c (for/first ([pr (in-list (catalog-parents cat (session-id active (hash-ref c 'path))))] #:when (equal? (cdr pr) "primary")) (car pr)))))
       (define (ws-text) (let ([w (ws-id)]) (and w (catalog-entity-instructions (catalog-entity cat w)))))
       (define (pos s k) (let ([m (regexp-match-positions (regexp-quote k) s)]) (and m (caar m))))
       (define (answered? n) (and (not (busy?)) (let ([c (cur)]) (and c (>= (hash-ref c 'messages 0) n)))))
       ;; 先说一句：有消息的会话才进目录（工作区实体在第一次同步时建）
       (list (step (lambda () (and ready (cur))) (lambda () (post-text! "hello") (enter!)))
             (step (lambda () (and (answered? 2) (ws-id))) (lambda () (wi 'post-key! "," 43 '(cmd))))
             (step (lambda () (and (svp 'open?) (svp 'hit-center 'section 'instructions))) (lambda () (lclick! (svp 'hit-center 'section 'instructions))))
             (step (lambda () (svp 'hit-center 'instr-edit (ws-id))) (lambda () (lclick! (svp 'hit-center 'instr-edit (ws-id)))))
             (step (lambda () (svp 'editing?)) (lambda () (post-text! "WS-RULE-ONE")))
             (step (after 3) (lambda () (wi 'post-key! "\r" 36 '(shift))))
             (step (after 3) (lambda () (post-text! "WS-RULE-TWO")))
             (step (after 6) (lambda () (log! 'field (editor-flatten (svp 'editor))) (log! 'visual (visual-check "instructions-field")) (enter!)))
             (step (lambda () (or (ws-text) ((after 60))))
                   (lambda () (log! 'saved (ws-text)) (wi 'post-key! (string (integer->char 27)) 53)))
             (step (lambda () (not (svp 'open?))) (lambda () (post-text! "show instructions") (enter!)))
             (step (lambda () (answered? 4))
                   (lambda () (define a (or (tr 'last-assistant-text) ""))
                              (log! 'order (let ([ps (map (lambda (k) (pos a k)) '("GLOBAL-RULE" "WS-RULE-ONE" "WS-RULE-TWO" "REPO-RULE"))]) (and (andmap values ps) (apply < ps))))
                              (define pt (sbp 'hit-center 'resume (hash-ref (cur) 'path)))      ; 右键当前会话（真实 AppKit 路径）→ 菜单
                              (wi 'post-mouse-ns! 'down (* zoom (car pt)) (* zoom (cdr pt)) 1) (wi 'post-mouse-ns! 'up (* zoom (car pt)) (* zoom (cdr pt)) 1)))
             (step (lambda () (sbp 'hit-center 'info (hash-ref (cur) 'path))) (lambda () (lclick! (sbp 'hit-center 'info (hash-ref (cur) 'path)))))   ; 「信息…」
             (step (lambda () (and (insp 'open?) (list? (hash-ref (insp 'data) 'instructions #f))))
                   (lambda () (log! 'info (map (lambda (x) (hash-ref x 'kind)) (hash-ref (insp 'data) 'instructions)))))
             (step (after 4) (lambda () (log! 'visual-info (visual-check "instructions-info")) (snapshot-and-quit!))))]
      [synth-organize?
       ;; 变更清单（design-workspace.md §11.7 C）：三个有消息的会话 → 说「organize my sessions」→ 脚本模型调 sessions_overview、
       ;; propose_session_changes → 审阅面板（删除缺省不勾）+ 侧栏预览 → 取消改名那一项（预览跟着变）→ 截图检查面板 →
       ;; 应用所选 → 项目 Demo、当前会话归入、最旧的归档；改名没发 → ⌘Z 整份撤销 → 都回去；清单的记录齐全
       (define (log! k v) (set! sessions-log (cons (cons k v) sessions-log)))
       (define (cur) (findf (lambda (se) (hash-ref se 'current #f)) (sbp 'sessions)))
       (define (ns! kind pt) (wi 'post-mouse-ns! kind (* zoom (car pt)) (* zoom (cdr pt)) 0))
       (define (lclick! pt) (ns! 'down pt) (ns! 'up pt))
       (define frames-at 0)
       (define (after n) (lambda () (> frames (+ frames-at n))))
       (define step-no 0)
       (define (step pred act) (cons pred (lambda () (set! step-no (add1 step-no)) (when (getenv "PI2_STEP_TRACE") (eprintf "organize step ~a\n" step-no)) (act) (set! frames-at frames))))
       (define turns0 0)
       (define (ask! text) (set! turns0 (tr 'turns)) (post-text! text) (enter!))
       (define (answered?) (and (not (busy?)) (let ([c (cur)]) (and c (>= (hash-ref c 'messages 0) 2)))))   ; 当前会话有了回答（轮数按文档计，切会话会重置）
       (define (items) (hash-ref (rvp 'data) 'items '()))
       (define (item-n op) (for/first ([it (in-list (items))] #:when (equal? (hash-ref it 'op) op)) (hash-ref it 'n)))
       (define (previews) (for/list ([se (in-list (sbp 'sessions))] #:when (hash-ref se 'preview #f)) (map car (hash-ref se 'preview))))
       (define (demo) (for/first ([e (in-list (catalog-entities-of cat "project"))] #:when (equal? (hash-ref e 'name) "Demo")) (hash-ref e 'id)))
       (define oldest #f)
       (define dragged #f)
       (list (step (lambda () (and ready (cur))) (lambda () (ask! "first topic")))
             (step (lambda () (answered?)) (lambda () (click-at! (sbp 'hit-center 'new #t))))
             (step (lambda () (let ([c (cur)]) (and c (hash-ref c 'ephemeral #f)))) (lambda () (ask! "second topic")))
             (step (lambda () (answered?)) (lambda () (click-at! (sbp 'hit-center 'new #t))))
             (step (lambda () (let ([c (cur)]) (and c (hash-ref c 'ephemeral #f)))) (lambda () (ask! "third topic")))
             (step (lambda () (answered?)) (lambda () (ask! "organize my sessions")))
             (step (lambda () (rvp 'open?))
                   (lambda () (log! 'items (map (lambda (it) (list (hash-ref it 'op) (hash-ref it 'status))) (items)))
                              (log! 'default-accepted (map (lambda (n) (hash-ref (list-ref (items) n) 'op)) (rvp 'accepted)))
                              (set! oldest (for/first ([it (in-list (items))] #:when (equal? (hash-ref it 'op) "archive")) (hash-ref it 'session)))))
             (step (after 3) (lambda () (log! 'preview (previews))
                                        (lclick! (rvp 'hit-center 'toggle (item-n "rename")))))
             (step (after 3) (lambda () (log! 'preview-after-untick (previews))
                                        (log! 'visual (visual-check "review-panel"))))
             (step (lambda () (not (busy?))) (lambda () (lclick! (rvp 'hit-center 'apply))))
             (step (lambda () (not (rvp 'open?)))
                   (lambda () (define cid (hash-ref (car (catalog-changesets-list cat)) 'proposal))
                              (log! 'applied (list (and (demo) #t)
                                                   (let ([c (cur)]) (and c (demo) (in-project? (demo) (session-id active (hash-ref c 'path))) #t))
                                                   (catalog-session-attr cat oldest 'archived #f)                 ; 清单里的会话已是完整 id
                                                   (and (sbp 'hit-center 'undo #t) #t)))
                              ;; W3：分组方式 → 按项目（真实点击 ⇅ → 菜单项）→ 侧栏有「Demo（1）」组 → 换回最近
                              (lclick! (sbp 'hit-center 'group-button #t))))
             (step (lambda () (sbp 'hit-center 'group-by 'project)) (lambda () (lclick! (sbp 'hit-center 'group-by 'project))))
             (step (after 3) (lambda () (log! 'by-project (for/list ([r (in-list (sbp 'sessions))] #:when (equal? (hash-ref r 'kind #f) "group")) (hash-ref r 'label)))
                                        ;; §11.4 D：把未归类的会话拖到「Demo」组上 → 归入 Demo；⌘Z → 撤回这一下
                                        (define loose (for/first ([r (in-list (sbp 'sessions))] #:when (and (hash-ref r 'path #f) (not (hash-ref r 'current #f)) (not (hash-ref r 'archived #f)))) (hash-ref r 'path)))
                                        (set! dragged loose)
                                        (define from (sbp 'hit-center 'resume loose))
                                        (define to (sbp 'hit-center 'toggle-group (string-append "\u0002grp\u0001\u0001" (demo))))
                                        (ns! 'down from) (ns! 'drag (cons (car from) (+ (cdr from) 20.0))) (ns! 'drag to) (ns! 'up to)))
             (step (lambda () (or (in-project? (demo) (session-id active dragged)) ((after 30))))
                   (lambda () (log! 'drag-to-project (in-project? (demo) (session-id active dragged))) (wi 'post-key! "z" 6 '(cmd))))
             (step (lambda () (or (not (in-project? (demo) (session-id active dragged))) ((after 30))))
                   (lambda () (log! 'undo-drag (not (in-project? (demo) (session-id active dragged))))
                              (lclick! (sbp 'hit-center 'group-button #t))))
             (step (lambda () (sbp 'hit-center 'group-by 'recent)) (lambda () (lclick! (sbp 'hit-center 'group-by 'recent))))
             (step (after 3) (lambda () (log! 'back-to-recent (and (for/or ([r (in-list (sbp 'sessions))]) (hash-ref r 'section #f)) #t))
                                        (wi 'post-key! "z" 6 '(cmd))))
             (step (lambda () (not (demo)))
                   (lambda () (define cs (car (catalog-changesets-list cat)))
                              (log! 'undone (list (not (demo)) (catalog-session-attr cat oldest 'archived #f)
                                                  (changeset-state cs)
                                                  (and (hash-ref cs 'review #f) (hash-ref (hash-ref cs 'review) 'rejected))
                                                  (length (hash-ref (hash-ref cs 'applied) 'results))
                                                  (hash-ref (hash-ref (hash-ref cs 'proposal) 'source) 'request)))
                              (log! 'auto-title (for/list ([r (in-list (sbp 'sessions))] #:when (>= (hash-ref r 'messages 0) 6)) (list (hash-ref r 'title #f) (hash-ref r 'titled #f))))
                              (wi 'post-key! "," 43 '(cmd))))
             ;; 变更记录：设置页 →「变更记录」→「查看」→ 审阅面板（只读、已撤销）
             (step (lambda () (and (svp 'open?) (svp 'hit-center 'section 'changes))) (lambda () (lclick! (svp 'hit-center 'section 'changes))))
             (step (lambda () (let ([cs (car (catalog-changesets-list cat))]) (svp 'hit-center 'open-changeset (hash-ref (hash-ref cs 'proposal) 'id))))
                   (lambda () (lclick! (svp 'hit-center 'open-changeset (hash-ref (hash-ref (car (catalog-changesets-list cat)) 'proposal) 'id)))))
             (step (lambda () (rvp 'open?))
                   (lambda () (log! 'history (list (not (svp 'open?)) (hash-ref (rvp 'data) 'state) (rvp 'hit-center 'apply) (and (rvp 'hit-center 'close) #t)))
                              (snapshot-and-quit!))))]
      [synth-rightclick?
       ;; 侧栏右键菜单走真实的 AppKit 路径（NSEvent → 窗口 sendEvent: → 命中的视图 rightMouseDown: …；玻璃时是覆盖视图）：
       ;; 两种材质（玻璃面板 / 不透明）各做：右键一行 → 12 帧后菜单还开着？→ 鼠标移到「信息…」上 → 还开着？→ 左键点它 → 信息面板 → Esc
       (define (log! k v) (set! sessions-log (cons (cons k v) sessions-log)))
       (define (cur) (findf (lambda (se) (hash-ref se 'current #f)) (sbp 'sessions)))
       (define first-path #f)
       (define (ns! kind pt [button 0]) (wi 'post-mouse-ns! kind (* zoom (car pt)) (* zoom (cdr pt)) button))
       (define (rclick! pt) (ns! 'down pt 1) (ns! 'up pt 1))
       (define (lclick! pt) (ns! 'down pt 0) (ns! 'up pt 0))
       (define frames-at 0)
       (define (mark!) (set! frames-at frames))
       (define (after n) (lambda () (> frames (+ frames-at n))))
       (define (eph-path) (let ([r (findf (lambda (se) (hash-ref se 'ephemeral #f)) (sbp 'sessions))]) (and r (hash-ref r 'path #f))))
       (define (row-of p) (findf (lambda (se) (equal? (hash-ref se 'path #f) p)) (sbp 'sessions)))
       (define (item! kind k) (let ([c (sbp 'hit-center kind k)]) (if c (lclick! c) (log! (list 'missing-item kind) #t))))
       (define (esc!) (wi 'post-key! (string (integer->char 27)) 53))
       (define (step pred act) (cons pred (lambda () (act) (mark!))))
       (define (round-for m)
         (list (step (lambda () (next-iteration?)) (lambda () (settings-action! (list 'sidebar-material m))))
               ;; 1. 右键 → 12 帧后还开着；两指搭在触控板上的滚动（dy 0 / 0.4）不关菜单
               (step (after 4) (lambda () (rclick! (sbp 'hit-center 'resume first-path))))
               (step (after 12) (lambda () (log! (list m 'open-after-12-frames) (sbp 'context-open?))
                                       (let ([c (sbp 'hit-center 'resume first-path)])
                                         (wi 'post-scroll! (* zoom (car c)) (* zoom (cdr c)) 0.0)
                                         (wi 'post-scroll! (* zoom (car c)) (* zoom (cdr c)) 0.4))))
               (step (after 4) (lambda () (log! (list m 'open-after-finger-rest-scroll) (sbp 'context-open?))
                                      (let ([c (sbp 'hit-center 'info first-path)]) (when c (ns! 'move c)))))
               ;; 2. 悬停「信息…」→ 还开着 → 点它 → 信息面板 → Esc
               (step (after 6) (lambda () (log! (list m 'open-after-hover) (sbp 'context-open?)) (item! 'info first-path)))
               (step (lambda () (or (insp 'open?) ((after 30)))) (lambda () (log! (list m 'info-opened) (insp 'open?)) (esc!)))
               ;; 3. 右键一行，再右键另一行（临时会话）→ 菜单换到那一行；Esc 关
               (step (after 4) (lambda () (rclick! (sbp 'hit-center 'resume first-path))))
               (step (after 4) (lambda () (rclick! (sbp 'hit-center 'resume (eph-path)))))
               (step (after 4) (lambda () (log! (list m 're-anchored) (and (sbp 'hit-center 'info (eph-path)) (not (sbp 'hit-center 'info first-path)) #t)) (esc!)))
               (step (after 4) (lambda () (log! (list m 'esc-closes) (not (sbp 'context-open?))) (rclick! (sbp 'hit-center 'resume first-path))))
               ;; 4. 点对话区（菜单外）→ 关
               (step (after 4) (lambda () (lclick! (cons (- w 200.0) 200.0))))
               (step (after 4) (lambda () (log! (list m 'outside-click-closes) (not (sbp 'context-open?))) (rclick! (sbp 'hit-center 'resume first-path))))
               ;; 5. 置顶 → 行上 pinned；再取消
               (step (after 4) (lambda () (item! 'pin first-path)))
               (step (lambda () (let ([r (row-of first-path)]) (or (and r (hash-ref r 'pinned #f)) ((after 30)))))
                     (lambda () (log! (list m 'pinned) (hash-ref (row-of first-path) 'pinned #f)) (rclick! (sbp 'hit-center 'resume first-path))))
               (step (after 4) (lambda () (item! 'unpin first-path)))
               (step (lambda () (let ([r (row-of first-path)]) (or (and r (not (hash-ref r 'pinned #f))) ((after 30)))))
                     (lambda () (log! (list m 'unpinned) (not (hash-ref (row-of first-path) 'pinned #f))) (rclick! (sbp 'hit-center 'resume first-path))))
               ;; 6. 重命名：行上输入框 → 输新名回车 → 内核改名后列表里是新名
               (step (after 4) (lambda () (item! 'rename first-path)))
               (step (lambda () (or (sbp 'editing?) ((after 30)))) (lambda () (post-text! (format "renamed ~a" m)) (enter!)))
               (step (lambda () (let ([r (row-of first-path)]) (or (and r (equal? (hash-ref r 'title #f) (format "renamed ~a" m))) ((after 60)))))
                     (lambda () (log! (list m 'renamed) (hash-ref (row-of first-path) 'title #f))))))
       ;; §11 A（少操作，真实 AppKit 事件）：悬停图标置顶 → 提示条撤销；双击改名 → ⌘Z 撤销；键盘 ↓ P E ⌫ + 撤销；Esc 回输入框
       (define (key! chars code [mods '()]) (wi 'post-key! chars code mods))
       (define (title-of p) (let ([r (row-of p)]) (and r (hash-ref r 'title #f))))
       (define (in-list? p) (and (row-of p) #t))
       (define second-path #f)
       ;; 触控板手势（经队列；带阶段）：began → changed … → ended，在行中心
       (define (attr? p a) (and (catalog-session-attr cat (session-id active p) a #f) #t))
       (define (gesture! p dxs dys)
         (define c (sbp 'hit-center 'resume p))
         (for ([dx (in-list dxs)] [dy (in-list dys)] [k (in-naturals)])
           (wi 'post-scroll! (* zoom (car c)) (* zoom (cdr c)) dy dx (if (= k 0) 'began 'changed)))
         (wi 'post-scroll! (* zoom (car c)) (* zoom (cdr c)) 0.0 0.0 'ended))
       (define quick
         (list
          ;; 1. 悬停 → 行上的置顶图标 → 置顶；提示条「撤销」→ 取消置顶
          (step (after 4) (lambda () (ns! 'move (sbp 'hit-center 'resume first-path))))
          (step (lambda () (or (sbp 'hit-center 'row-pin first-path) ((after 20)))) (lambda () (lclick! (sbp 'hit-center 'row-pin first-path))))
          (step (lambda () (let ([r (row-of first-path)]) (or (and r (hash-ref r 'pinned #f) (sbp 'hit-center 'undo #t)) ((after 30)))))
                (lambda () (log! 'hover-pin (hash-ref (row-of first-path) 'pinned #f)) (lclick! (sbp 'hit-center 'undo #t))))
          (step (lambda () (let ([r (row-of first-path)]) (or (and r (not (hash-ref r 'pinned #f))) ((after 30)))))
                (lambda () (log! 'undo-button (not (hash-ref (row-of first-path) 'pinned #f)))))
          ;; 2. 双击标题 → 改名 → 回车；⌘Z（侧栏有焦点）→ 改回
          (step (after 4) (lambda () (let ([c (sbp 'hit-center 'resume first-path)])
                                       (ns! 'down c) (ns! 'up c)
                                       (wi 'post-mouse-ns! 'down (* zoom (car c)) (* zoom (cdr c)) 0 '() 2) (wi 'post-mouse-ns! 'up (* zoom (car c)) (* zoom (cdr c)) 0 '() 2))))
          (step (lambda () (or (sbp 'editing?) ((after 30)))) (lambda () (log! 'double-click-rename (sbp 'editing?)) (post-text! "quick name") (enter!)))
          (step (lambda () (or (equal? (title-of first-path) "quick name") ((after 60)))) (lambda () (log! 'renamed-quick (title-of first-path)) (key! "z" 6 '(cmd))))
          (step (lambda () (or (equal? (title-of first-path) "renamed opaque") ((after 60)))) (lambda () (log! 'cmd-z-undo-rename (title-of first-path))))
          ;; 3. 点行（不拿键盘焦点）→ 打字进输入框；右键 → P（菜单里的单键）置顶、输入框不出现 p；⌘⇧E 键盘模式 → E 归档 → ⌘Z → ↓ → ⌫ → 提示条撤销
          (step (after 4) (lambda () (lclick! (sbp 'hit-center 'resume first-path))))
          (step (after 6) (lambda () (post-text! "q")))
          ;; 清输入框的按键与右键分两步：窗口在前台时按键走应用的事件队列（晚到），鼠标是立即的——同一步里 ⌫ 会落在菜单开着时（= 删这一行）
          (step (after 4) (lambda () (log! 'row-click-keeps-typing (editor-flatten e)) (key! "a" 0 '(cmd)) (key! "\u007F" 51)))
          (step (after 6) (lambda () (rclick! (sbp 'hit-center 'resume first-path))))
          (step (after 6) (lambda () (log! 'menu-open-before-p (sbp 'context-open?)) (key! "p" 35)))
          (step (lambda () (let ([r (row-of first-path)]) (or (and r (hash-ref r 'pinned #f)) ((after 30)))))
                (lambda () (log! 'menu-key-p (list (hash-ref (row-of first-path) 'pinned #f) (editor-flatten e) (sbp 'context-open?)))
                           (key! "e" 14 '(cmd shift))))
          (step (after 4) (lambda () (log! 'cmd-shift-e-keyboard-mode sidebar-focus?) (key! "e" 14)))
          ;; 当前会话归档后照样显示（设计如此）：看行上的 archived
          (step (lambda () (let ([r (row-of first-path)]) (or (and r (hash-ref r 'archived #f)) ((after 30)))))
                (lambda () (log! 'key-e-archived (hash-ref (row-of first-path) 'archived #f)) (key! "z" 6 '(cmd))))
          (step (lambda () (let ([r (row-of first-path)]) (or (and r (not (hash-ref r 'archived #f))) ((after 30)))))
                (lambda () (log! 'cmd-z-unarchive (not (hash-ref (row-of first-path) 'archived #f)))
                           (set! second-path (for/first ([se (in-list (sbp 'sessions))]
                                                         #:when (and (hash-ref se 'path #f) (not (equal? (hash-ref se 'path) first-path))
                                                                     (not (hash-ref se 'current #f)) (not (hash-ref se 'ephemeral #f))))
                                               (hash-ref se 'path)))
                           ;; 焦点移到它：按 ↓（真实按键，回放能重做；直接调组件的话日志里没有）
                           (define keys (for/list ([se (in-list (sbp 'sessions))] #:when (hash-ref se 'path #f)) (hash-ref se 'path)))
                           (define d (and second-path (- (index-of keys second-path) (index-of keys (sbp 'focus)))))
                           (log! 'has-second (and second-path #t))
                           (when d (for ([_ (in-range (abs d))]) (key! (if (> d 0) "\uF701" "\uF700") (if (> d 0) 125 126))))))
          (step (after 4) (lambda () (log! 'focus-on-second (equal? (sbp 'focus) second-path)) (key! "\u007F" 51)))
          (step (lambda () (or (not (and second-path (in-list? second-path))) ((after 60))))
                (lambda () (log! 'key-delete (and second-path (not (in-list? second-path)))) (let ([u (sbp 'hit-center 'undo #t)]) (when u (lclick! u)))))
          (step (lambda () (or (and second-path (in-list? second-path)) ((after 60))))
                (lambda () (log! 'undo-delete (and second-path (in-list? second-path))) (key! "\u001B" 53)))
          ;; 4. Esc 退出键盘模式；⌘Z 跟着最近的操作：右键菜单删除（鼠标）后马上 ⌘Z → 恢复（之后没打字）
          (step (after 4) (lambda () (log! 'esc-unfocus (not sidebar-focus?)) (rclick! (sbp 'hit-center 'resume second-path))))
          (step (after 4) (lambda () (item! 'delete second-path)))
          (step (lambda () (or (not (in-list? second-path)) ((after 60)))) (lambda () (log! 'menu-delete (not (in-list? second-path))) (key! "z" 6 '(cmd))))
          (step (lambda () (or (in-list? second-path) ((after 60)))) (lambda () (log! 'cmd-z-after-mouse-delete (in-list? second-path)) (post-text! "x")))
          (step (after 4) (lambda () (log! 'typing-after (editor-flatten e)) (key! "a" 0 '(cmd)) (key! "\u007F" 51)))
          ;; §11.4 D 手势：右扫 → 置顶；左扫 → 归档 → ⌘Z；竖着的手势不横扫；拖行到「置顶」段 → 置顶
          (step (after 4) (lambda () (gesture! second-path '(0.0 30.0 30.0 30.0) '(0.0 0.0 0.0 0.0))))
          (step (lambda () (let ([r (row-of second-path)]) (or (and r (hash-ref r 'pinned #f)) ((after 30)))))
                (lambda () (log! 'swipe-right-pin (hash-ref (row-of second-path) 'pinned #f))
                           (gesture! second-path '(0.0 -30.0 -30.0 -30.0) '(0.0 0.0 0.0 0.0))))
          (step (lambda () (or (attr? second-path 'archived) ((after 30))))                ; 归档了：行收进「已归档」，看目录
                (lambda () (log! 'swipe-left-archive (attr? second-path 'archived)) (key! "z" 6 '(cmd))))
          (step (lambda () (or (not (attr? second-path 'archived)) ((after 30))))
                (lambda () (log! 'undo-archive (not (attr? second-path 'archived)))
                           (gesture! first-path '(0.0 2.0 2.0 2.0) '(0.0 15.0 15.0 15.0))))
          ;; 竖着的手势：不横扫，first 的置顶状态不变（它之前已被菜单的 P 置顶）
          (step (after 4) (lambda () (log! 'vertical-no-swipe (list (not (sbp 'swiping?)) (attr? first-path 'pinned)))
                                     (gesture! second-path '(0.0 30.0 30.0 30.0) '(0.0 0.0 0.0 0.0))))   ; 再右扫 second = 取消置顶
          (step (lambda () (or (not (attr? second-path 'pinned)) ((after 30))))
                (lambda () (define from (sbp 'hit-center 'resume second-path))                 ; 拖 second 到「置顶」段（first 在里面）
                           (define to (sbp 'hit-center 'toggle-group "\u0002sec\u0001\u0001pinned"))
                           (ns! 'down from)
                           (ns! 'drag (cons (car from) (+ (cdr from) 20.0)))
                           (ns! 'drag to)
                           (ns! 'up to)))
          (step (lambda () (or (attr? second-path 'pinned) ((after 30))))
                (lambda () (log! 'drag-to-pinned (attr? second-path 'pinned))))))
       (append
        (list (cons (lambda () (and ready (cur))) (lambda () (post-text! "first question") (enter!)))
              (cons (lambda () (and (>= (tr 'turns) 1) (not (busy?)) (cur) (> (hash-ref (cur) 'messages 0) 0)))
                    (lambda () (set! first-path (hash-ref (cur) 'path)) (click-at! (sbp 'hit-center 'new #t)))))
        (round-for 'glass)
        (round-for 'opaque)
        ;; 删除要有另一个非当前会话：先建一个（发一句，再切回 first）
        (list (step (after 4) (lambda () (click-at! (sbp 'hit-center 'new #t))))
              (step (lambda () (let ([c (cur)]) (and c (hash-ref c 'ephemeral #f)))) (lambda () (post-text! "second one") (enter!)))
              (step (lambda () (and (not (busy?)) (let ([c (cur)]) (and c (> (hash-ref c 'messages 0) 0) (not (equal? (hash-ref c 'path) first-path))))))
                    (lambda () (lclick! (sbp 'hit-center 'resume first-path)))))
        quick
        (list (cons (after 4) (lambda () (snapshot-and-quit!)))))]
      [synth-visual?
       ;; 显示效果（design-gui.md §28）：一个有消息的会话 + 当前的临时会话（虚线框、起伏）；
       ;; 侧栏材质 × 背后（透出桌面时：侧栏那一列下面画白 / 黑 / 灰 / 橙 / 蓝，代替桌面）→ 每种截窗口检查对比度与亮度
       (define (cur) (findf (lambda (se) (hash-ref se 'current #f)) (sbp 'sessions)))
       (define backdrops (list (cons "white" '(1.0 1.0 1.0)) (cons "black" '(0.0 0.0 0.0)) (cons "gray" '(0.5 0.5 0.5))
                               (cons "orange" '(0.95 0.6 0.2)) (cons "blue" '(0.1 0.3 0.9))))
       (define configs (append (for*/list ([m (in-list '(glass desktop))] [b (in-list backdrops)])
                                 (list (format "~a-~a" m (car b)) m (cdr b)))
                               (list (list "opaque" 'opaque #f))))
       (define frames-at 0)
       (define results '())
       (append
        (list (cons (lambda () (and ready (cur))) (lambda () (post-text! "first question") (enter!)))
              (cons (lambda () (and (>= (tr 'turns) 1) (not (busy?)) (sbp 'hit-center 'new #t))) (lambda () (click-at! (sbp 'hit-center 'new #t)))))
        (append*
         (for/list ([c (in-list configs)])
           (list (cons (lambda () (next-iteration?))
                       (lambda () (settings-action! (list 'sidebar-material (cadr c))) (set! test-under (caddr c)) (set! frames-at frames)))
                 (cons (lambda () (> frames (+ frames-at 4)))
                       (lambda () (set! results (cons (cons (car c) (visual-check (car c))) results)))))))
        ;; 交互状态（玻璃面板 + 后面是白的，最亮）：悬停行的图标、右键菜单（图标格 + 快捷键）、底部提示条
        (let* ([row (lambda () (for/first ([se (in-list (sbp 'sessions))] #:when (and (hash-ref se 'path #f) (not (hash-ref se 'current #f)))) (hash-ref se 'path)))]
               [ns! (lambda (kind pt [button 0]) (wi 'post-mouse-ns! kind (* zoom (car pt)) (* zoom (cdr pt)) button))])
          (list (cons (lambda () (next-iteration?)) (lambda () (settings-action! (list 'sidebar-material 'glass)) (set! test-under '(1.0 1.0 1.0))
                                                      (ns! 'move (sbp 'hit-center 'resume (row))) (set! frames-at frames)))
                (cons (lambda () (> frames (+ frames-at 4))) (lambda () (set! results (cons (cons "hover-icons" (visual-check "hover-icons")) results))
                                                                  (ns! 'down (sbp 'hit-center 'resume (row)) 1) (ns! 'up (sbp 'hit-center 'resume (row)) 1)
                                                                  (set! frames-at frames)))
                (cons (lambda () (> frames (+ frames-at 4))) (lambda () (set! results (cons (cons "context-menu" (visual-check "context-menu")) results))
                                                                  (sbp 'close-context!) (notice! "Deleted “first question”") (set! frames-at frames)))
                (cons (lambda () (> frames (+ frames-at 4))) (lambda () (set! results (cons (cons "notice" (visual-check "notice")) results))))))
        (list (cons (lambda () (next-iteration?))
                    (lambda () (set! test-under #f)
                               (printf "G10 visual summary: ~s\n" (reverse results))
                               (snapshot-and-quit!)))))]
      [synth-chat?
       ;; 聊天（匿名工作区）：点「💬 聊天」→ 新会话是聊天（行上 💬，工作目录在聊天根目录下）→ 发一句 → 右键「信息…」看到聊天目录 →
       ;; 设置页「聊天目录」看到占用 →「按上限立即清理」→ 收到占用（没超限：什么也不删）
       (define (log! k v) (set! sessions-log (cons (cons k v) sessions-log)))
       (define (cur) (findf (lambda (se) (hash-ref se 'current #f)) (sbp 'sessions)))
       (define usage0 #f)
       (list (cons (lambda () (and ready (cur) (sbp 'hit-center 'new-chat #t))) (lambda () (click-at! (sbp 'hit-center 'new-chat #t))))
             (cons (lambda () (let ([c (cur)]) (and c (hash-ref c 'chat #f) (next-iteration?))))
                   (lambda () (log! 'chat-row (list (hash-ref (cur) 'ephemeral #f) (regexp-match? #rx"/chats/" (format "~a" (hash-ref (cur) 'workdir "")))))
                              (post-text! "hello chat") (enter!)))
             (cons (lambda () (and (>= (tr 'turns) 1) (not (busy?)) (cur) (not (hash-ref (cur) 'ephemeral #f)) (sbp 'hit-center 'resume (hash-ref (cur) 'path))))
                   (lambda () (define c (sbp 'hit-center 'resume (hash-ref (cur) 'path)))
                              (wi 'post-mouse! 'down (* zoom (car c)) (* zoom (cdr c)) '(ctrl)) (wi 'post-mouse! 'up (* zoom (car c)) (* zoom (cdr c)) '(ctrl))))
             (cons (lambda () (and (next-iteration?) (sbp 'hit-center 'info (hash-ref (cur) 'path)))) (lambda () (click-at! (sbp 'hit-center 'info (hash-ref (cur) 'path)))))
             (cons (lambda () (insp 'open?))
                   (lambda () (log! 'info (list (hash-ref (insp 'data) 'chat #f) (real? (hash-ref (insp 'data) 'workdir_bytes #f))))
                              (wi 'post-key! (string (integer->char 27)) 53)))
             (cons (lambda () (and (not (insp 'open?)) (next-iteration?))) (lambda () (wi 'post-key! "," 43 '(cmd))))
             (cons (lambda () (and (svp 'open?) (svp 'hit-center 'section 'chat))) (lambda () (click-at! (svp 'hit-center 'section 'chat))))
             (cons (lambda () (and (svp 'hit-center 'chat-cleanup #f) (next-iteration?)))
                   (lambda () (click-at! (svp 'hit-center 'chat-cleanup #f))))
             (cons (lambda () (and (next-iteration?) (hash? chat-usage-last) (> (hash-ref chat-usage-last 'seq 0) 1)))
                   (lambda () (log! 'usage (list (hash-ref chat-usage-last 'count #f) (hash-ref chat-usage-last 'removed #f)))
                              (snapshot-and-quit!))))]                                ; 快照里是设置页的「聊天目录」
      [synth-sessmenu?
       ;; 会话右键菜单（design-workspace.md W1，私有剪贴板）：两个会话 → 右键第一个 →「信息…」面板（消息数、标题）→ Esc →
       ;; 右键 →「重命名…」→ 输新名回车 → 列表里是新名 → 右键 →「导出 Markdown」→ 剪贴板以「# 新名」开头 → 右键当前会话 →「复制路径」→
       ;; W2：右键第一个 →「置顶」→ 它排到最前、标 pinned →「归档」→ 收进「已归档（1）」→ 点分组行展开 → 它出现在分组下 →
       ;; 「信息…」的「归属」有工作区 → Esc →「取消归档」→ 分组行没了
       (define (log! k v) (set! sessions-log (cons (cons k v) sessions-log)))
       (define first-path #f)
       (define (row k) (findf (lambda (se) (equal? (hash-ref se 'path #f) k)) (sbp 'sessions)))
       (define (cur) (findf (lambda (se) (hash-ref se 'current #f)) (sbp 'sessions)))
       (define (ctx! k) (let ([c (sbp 'hit-center 'resume k)]) (wi 'post-mouse! 'down (* zoom (car c)) (* zoom (cdr c)) '(ctrl))
                          (wi 'post-mouse! 'up (* zoom (car c)) (* zoom (cdr c)) '(ctrl))))
       (define (item! kind k) (click-at! (sbp 'hit-center kind k)))
       (define turns0 0)
       (list (cons (lambda () (and ready (cur))) (lambda () (post-text! "first question") (enter!)))
             (cons (lambda () (and (>= (tr 'turns) 1) (not (busy?)) (cur) (> (hash-ref (cur) 'messages 0) 0)))
                   (lambda () (set! first-path (hash-ref (cur) 'path)) (click-at! (sbp 'hit-center 'new #t))))
             (cons (lambda () (and (cur) (not (equal? (hash-ref (cur) 'path) first-path)) (next-iteration?)))
                   (lambda () (set! turns0 (tr 'turns)) (post-text! "second question") (enter!)))
             (cons (lambda () (and (> (tr 'turns) turns0) (not (busy?)) (row first-path) (sbp 'hit-center 'resume first-path)))
                   (lambda () (ctx! first-path)))
             (cons (lambda () (and (next-iteration?) (sbp 'hit-center 'info first-path))) (lambda () (item! 'info first-path)))
             (cons (lambda () (insp 'open?))
                   (lambda () (log! 'info (list (hash-ref (insp 'data) 'title #f) (hash-ref (insp 'data) 'messages #f) (hash-ref (insp 'data) 'requests #f)))
                              (wi 'post-key! (string (integer->char 27)) 53)))
             (cons (lambda () (and (not (insp 'open?)) (next-iteration?))) (lambda () (log! 'info-closed #t) (ctx! first-path)))
             (cons (lambda () (and (next-iteration?) (sbp 'hit-center 'rename first-path))) (lambda () (item! 'rename first-path)))
             (cons (lambda () (and (next-iteration?) (sbp 'editing?))) (lambda () (post-text! "renamed one") (enter!)))
             (cons (lambda () (let ([r (row first-path)]) (and r (equal? (hash-ref r 'title #f) "renamed one"))))
                   (lambda () (log! 'renamed (hash-ref (row first-path) 'title)) (ctx! first-path)))
             (cons (lambda () (and (next-iteration?) (sbp 'hit-center 'export-md first-path))) (lambda () (clip 'set! "") (item! 'export-md first-path)))
             (cons (lambda () (let ([c (clip 'get)]) (and (string? c) (regexp-match? #rx"^# " c))))
                   (lambda () (define c (clip 'get))
                              (log! 'export (list (substring c 0 (min 13 (string-length c))) (regexp-match? #rx"## User

first question" c)))
                              (ctx! (hash-ref (cur) 'path))))
             (cons (lambda () (and (next-iteration?) (sbp 'hit-center 'copy-path (hash-ref (cur) 'path))))
                   (lambda () (item! 'copy-path (hash-ref (cur) 'path))))
             (cons (lambda () (equal? (clip 'get) (hash-ref (cur) 'path)))
                   (lambda () (log! 'copied-path #t) (ctx! first-path)))
             (cons (lambda () (and (next-iteration?) (sbp 'hit-center 'pin first-path))) (lambda () (item! 'pin first-path)))
             (cons (lambda () (let ([r (row first-path)]) (and r (hash-ref r 'pinned #f))))
                   (lambda () (log! 'pinned (list (equal? (hash-ref (car (sbp 'sessions)) 'path #f) first-path) (hash-ref (row first-path) 'pinned #f)))
                              (ctx! first-path)))
             (cons (lambda () (and (next-iteration?) (sbp 'hit-center 'archive first-path))) (lambda () (item! 'archive first-path)))
             (cons (lambda () (and (not (row first-path)) (sbp 'hit-center 'toggle-group "\u0002archived\u0001")))
                   (lambda () (log! 'archived (for/list ([r (in-list (sbp 'sessions))] #:when (equal? (hash-ref r 'kind #f) "group")) (hash-ref r 'count)))
                              (click-at! (sbp 'hit-center 'toggle-group "\u0002archived\u0001"))))
             (cons (lambda () (and (row first-path) (sbp 'hit-center 'resume first-path)))
                   (lambda () (log! 'expanded (hash-ref (row first-path) 'archived #f)) (ctx! first-path)))
             (cons (lambda () (and (next-iteration?) (sbp 'hit-center 'info first-path))) (lambda () (item! 'info first-path)))
             (cons (lambda () (insp 'open?))
                   (lambda () (log! 'groups (let ([g (hash-ref (insp 'data) 'groups (hasheq))]) (regexp-match? #rx" @ local$" (hash-ref g 'workspace ""))))
                              (wi 'post-key! (string (integer->char 27)) 53)))
             (cons (lambda () (and (not (insp 'open?)) (next-iteration?))) (lambda () (ctx! first-path)))
             (cons (lambda () (and (next-iteration?) (sbp 'hit-center 'unarchive first-path))) (lambda () (item! 'unarchive first-path)))
             (cons (lambda () (let ([r (row first-path)]) (and r (not (hash-ref r 'archived #f)))))
                   (lambda () (log! 'unarchived (not (for/or ([r (in-list (sbp 'sessions))]) (equal? (hash-ref r 'archived #f) #t))))
                              ;; §11.4 B 时间分段：「今天」段在；点它的分组行（真实事件）→ 收起（行不见了，当前会话除外）→ 再点 → 展开
                              (log! 'sections (for/list ([r (in-list (sbp 'sessions))] #:when (hash-ref r 'section #f)) (hash-ref r 'section)))
                              (let ([c (sbp 'hit-center 'toggle-group "\u0002sec\u0001\u0001today")])
                                (wi 'post-mouse-ns! 'down (* zoom (car c)) (* zoom (cdr c)) 0) (wi 'post-mouse-ns! 'up (* zoom (car c)) (* zoom (cdr c)) 0))))
             (cons (lambda () (next-iteration?))
                   (lambda () (log! 'today-folded (list (hash-ref (findf (lambda (r) (equal? (hash-ref r 'section #f) "today")) (sbp 'sessions)) 'open)
                                                         (length (filter (lambda (r) (hash-ref r 'path #f)) (sbp 'sessions)))))
                              (let ([c (sbp 'hit-center 'toggle-group "\u0002sec\u0001\u0001today")])
                                (wi 'post-mouse-ns! 'down (* zoom (car c)) (* zoom (cdr c)) 0) (wi 'post-mouse-ns! 'up (* zoom (car c)) (* zoom (cdr c)) 0))))
             (cons (lambda () (next-iteration?))
                   (lambda () (log! 'today-unfolded (length (filter (lambda (r) (hash-ref r 'path #f)) (sbp 'sessions))))
                              (snapshot-and-quit!))))]
      [synth-layout?
       ;; 侧栏布局：拖分隔线把侧栏拉到 320（松手存设置）→ 设置页改到右边 → 右边侧栏上点「新建」→ 在右边再拖到 260；
       ;; 新建出来的临时会话标 ephemeral，「分叉」不可点
       (define (log! k v) (set! sessions-log (cons (cons k v) sessions-log)))
       (define (drag! x0 x1)
         (wi 'post-mouse! 'down (* zoom x0) 300.0) (wi 'post-mouse! 'drag (* zoom (/ (+ x0 x1) 2)) 300.0)
         (wi 'post-mouse! 'drag (* zoom x1) 300.0) (wi 'post-mouse! 'up (* zoom x1) 300.0))
       (define (geom) (let-values ([(sbx sbw mx mw dx) (columns)]) (list sbx sbw dx)))
       (define (cur-row) (findf (lambda (se) (hash-ref se 'current #f)) (sbp 'sessions)))
       (list (cons (lambda () (and ready (cur-row))) (lambda () (log! 'initial (geom)) (drag! (+ sidebar-w 1.0) 320.0)))
             (cons (lambda () (and (next-iteration?) (not drag-divider?) (= sidebar-w 320.0)))
                   (lambda () (log! 'dragged (list (geom) (gsetting 'sidebar-width #f))) (wi 'post-key! "," 43 '(cmd))))
             (cons (lambda () (and (svp 'open?) (svp 'hit-center 'section 'general))) (lambda () (click-at! (svp 'hit-center 'section 'general))))
             (cons (lambda () (svp 'hit-center 'sidebar-side 'right)) (lambda () (click-at! (svp 'hit-center 'sidebar-side 'right))))
             (cons (lambda () (and (next-iteration?) sidebar-right?))
                   (lambda () (log! 'right (list (geom) (gsetting 'sidebar-side #f))) (wi 'post-key! (string (integer->char 27)) 53)))
             (cons (lambda () (and (not (svp 'open?)) (next-iteration?) (sbp 'hit-center 'new #t)))
                   (lambda () (log! 'new-button-x (car (sbp 'hit-center 'new #t))) (click-at! (sbp 'hit-center 'new #t))))
             (cons (lambda () (let ([c (cur-row)]) (and c (hash-ref c 'ephemeral #f) (next-iteration?))))
                   (lambda () (log! 'ephemeral-fork-clickable (and (sbp 'hit-center 'fork #t) #t))
                              (drag! (- w 320.0) (- w 260.0))))
             (cons (lambda () (and (next-iteration?) (not drag-divider?) (= sidebar-w 260.0)))
                   (lambda () (log! 'dragged-right (list (geom) (gsetting 'sidebar-width #f))) (snapshot-and-quit!))))]
      [synth-media?
       ;; 附件（P4）：投递一张图片 + 一段音频（同拖进窗口）→ 输入框里两个附件 chip → 补一句发送 →
       ;; 对话区的用户消息带两个媒体块（记录接管本地那条）；图片缩略图在解码子进程里解好；内核历史里是 media 块（只有 sha）
       (define (log! k v) (set! sessions-log (cons (cons k v) sessions-log)))
       (define img (file->bytes (build-path FIXTURES "sample.png")))
       (define aud (file->bytes (build-path FIXTURES "sample.m4a")))
       (define isha (sha256-hex img))
       (list (cons (lambda () ready)
                   (lambda () (wi 'post-attach! (list (vector 'image "image/png" "sample.png" img (bytes-length img) #f)
                                                      (vector 'audio "audio/mp4" "sample.m4a" aud (bytes-length aud) #f)))))
             (cons (lambda () (and (next-iteration?) (= 2 (length (editor-chips e)))))
                   (lambda () (log! 'chips (for/list ([id (editor-chips e)]) (fold-kind (editor-fold e id))))
                              (post-text! "what is in these?") (enter!)))
             (cons (lambda () (and (>= (tr 'turns) 1) (not (busy?))))
                   (lambda () (log! 'answer (tr 'last-assistant-text))
                              (log! 'thumb (and (mdp 'thumb isha) #t))
                              (script-send! (hasheq 'type "history"))))
             (cons (lambda () kernel-history)
                   (lambda ()
                     (log! 'kernel-media (for*/list ([m (in-list kernel-history)] [b (in-list (hash-ref m 'blocks '()))]
                                                     #:when (equal? (hash-ref b 'type #f) "media"))
                                           (list (hash-ref b 'kind) (hash-ref b 'name) (equal? (hash-ref b 'sha) isha) (hash-ref (hash-ref b 'meta) 'width #f)
                                                 (hash-ref (hash-ref b 'meta) 'duration #f))))
                     (snapshot-and-quit!))))]
      [synth-small?
       ;; 小项合集（私有剪贴板）：
       ;;  ① 粘贴一段不像密钥的串 → ⌘A → ⌘⇧K 标记为密钥 → 补字发送：回显里只有占位符；日志文件里没有原文（冒烟驱动查文件）
       ;;  ② 设置页加自定义格式 ^myco_[a-z0-9]{8}$ → 粘贴 myco_ab12cd34 → 识别成待选密钥 chip
       ;;  ③ 新建规则，追加文字两行（⇧Enter 换行）→ 规则里是两行
       (define V1 "zz-Private-0099xx")
       (define V2 "myco_ab12cd34")
       (define (log! k v) (set! sessions-log (cons (cons k v) sessions-log)))
       (define (click-sv! kind payload) (click-at! (svp 'hit-center kind payload)))
       (define (paste! s) (clip 'set! s) (cond [(wi 'key?) (wi 'post-key! "v" 9 '(cmd))] [else (wi 'post-paste!)]))
       (define (find-input-button kind)
         (for*/first ([y (in-range 0 (inexact->exact (round h)) 3)] [x (in-range 0 (inexact->exact (round w)) 6)]
                      #:when (let ([r (ib 'hit (exact->inexact x) (exact->inexact y))]) (and r (eq? (car r) 'button) (eq? (cadr r) kind))))
           (cons (exact->inexact x) (exact->inexact y))))
       (define turns0 0)
       (define (rule-ml) (findf (lambda (r) (equal? (hash-ref r 'name #f) "ml")) (or (hs-rules (A)) '())))
       (list (cons (lambda () ready) (lambda () (set! secret-mode 'suggest) (paste! V1)))
             (cons (lambda () (and (next-iteration?) (regexp-match? #rx"zz-Private" (editor-text e)))) (lambda () (wi 'post-key! "a" 0 '(cmd))))
             (cons (lambda () (and (next-iteration?) (has-selection? e))) (lambda () (wi 'post-key! "K" 40 '(cmd shift))))
             (cons (lambda () (and (next-iteration?) (= (length (editor-chips e)) 1)))
                   (lambda () (log! 'marked (list (fold-kind (editor-fold e (car (editor-chips e)))) (regexp-match? #rx"zz-Private" (editor-text e))))
                              (set! turns0 (tr 'turns)) (post-text! " ok") (enter!)))
             (cons (lambda () (and (> (tr 'turns) turns0) (not (busy?))))
                   (lambda () (define a (or (tr 'last-assistant-text) ""))
                              (log! 'answer (list (regexp-match? #rx"⟦secret:[0-9a-f]+⟧ ok" a) (regexp-match? #rx"zz-Private" a)))
                              (wi 'post-key! "," 43 '(cmd))))
             (cons (lambda () (and (svp 'open?) (svp 'hit-center 'section 'general))) (lambda () (click-sv! 'section 'general)))
             (cons (lambda () (svp 'hit-center 'add-secret-pattern #f)) (lambda () (click-sv! 'add-secret-pattern #f)))
             (cons (lambda () (and (next-iteration?) (svp 'editing?))) (lambda () (post-text! "^myco_[a-z0-9]{8}$") (enter!)))
             (cons (lambda () (and (next-iteration?) (member "^myco_[a-z0-9]{8}$" secret-patterns)))
                   (lambda () (log! 'pattern-saved #t) (wi 'post-key! (string (integer->char 27)) 53)))
             (cons (lambda () (and (not (svp 'open?)) (next-iteration?))) (lambda () (paste! V2)))
             (cons (lambda () (find-input-button 'confirm))
                   (lambda () (log! 'custom-detected #t) (click-at! (find-input-button 'confirm)) (wi 'post-key! "," 43 '(cmd))))
             (cons (lambda () (and (svp 'open?) (svp 'hit-center 'section 'rules))) (lambda () (click-sv! 'section 'rules)))
             (cons (lambda () (svp 'hit-center 'new-rule #f)) (lambda () (click-sv! 'new-rule #f)))
             (cons (lambda () (and (next-iteration?) (svp 'editing?))) (lambda () (post-text! "ml") (enter!)))
             (cons (lambda () (next-iteration?)) (lambda () (post-text! "*") (enter!)))
             (cons (lambda () (and (next-iteration?) (svp 'multiline?)))
                   (lambda () (post-text! "line one") (wi 'post-key! "\r" 36 '(shift)) (post-text! "line two") (enter!)))
             (cons (lambda () (rule-ml))
                   (lambda () (log! 'rule-text (hash-ref (rule-ml) 'system_append #f)) (wi 'post-key! (string (integer->char 27)) 53)))
             (cons (lambda () (not (svp 'open?))) (lambda () (snapshot-and-quit!))))]
      [synth-spend?
       ;; 跨主机费用合计（两台的内核各预置一笔今天的花费：local $0.25、srv $0.5）：状态行显示合计 → 设置页设合计上限 $0.5
       ;; → local 上发一轮：合计上限卡片（scope combined）→ 停止 → 再发一轮 → 「今天不再询问」→ 跑完；合计上限今天不再推给各台
       (define (log! k v) (set! sessions-log (cons (cons k v) sessions-log)))
       (define (click-sv! kind payload) (click-at! (svp 'hit-center kind payload)))
       (define (spent id) (let ([b (hs-budget (H id))]) (and (hash? b) (hash-ref b 'spent_today #f))))
       (define (card-scope) (let* ([ids (tr 'pending-permissions)] [info (and (pair? ids) (tr 'permission-info (car ids)))])
                              (and info (hash? (caddr info)) (hash-ref (caddr info) 'scope #f))))
       (define (perm! decision) (click-at! (tr 'hit-center 'perm (cons (car (tr 'pending-permissions)) decision))))
       (define turns0 #f)
       (list (cons (lambda () (and (H "srv") (hs-ready (H "local")) (hs-ready (H "srv"))
                                   (let ([a (spent "local")] [b (spent "srv")]) (and a b (> a 0) (> b 0)))))
                   (lambda () (log! 'spend (list (spent "local") (spent "srv") (money2 (spend-total)) (status)))
                              (wi 'post-key! "," 43 '(cmd))))
             (cons (lambda () (and (svp 'open?) (svp 'hit-center 'section 'budget))) (lambda () (click-sv! 'section 'budget)))
             (cons (lambda () (svp 'hit-center 'budget-edit 'combined)) (lambda () (click-sv! 'budget-edit 'combined)))
             (cons (lambda () (and (next-iteration?) (svp 'editing?))) (lambda () (post-text! "0.5") (enter!)))
             (cons (lambda () (let ([m (hash-ref pushed-external "local" #f)]) (and m (equal? (hash-ref m 'combined_cap_usd #f) 0.5))))
                   (lambda () (log! 'pushed (list (hash-ref (hash-ref pushed-external "local") 'others_usd) (hash-ref (hash-ref pushed-external "srv") 'others_usd)))
                              (wi 'post-key! (string (integer->char 27)) 53)))
             (cons (lambda () (and (not (svp 'open?)) (next-iteration?))) (lambda () (set! turns0 (tr 'turns)) (post-text! "hello") (enter!)))
             (cons (lambda () (and (card-scope) (tr 'hit-center 'perm (cons (car (tr 'pending-permissions)) "no"))))
                   (lambda () (log! 'card (card-scope)) (perm! "no")))
             (cons (lambda () (and (> (tr 'turns) turns0) (not (busy?))))
                   (lambda () (log! 'stopped (tr 'summary)) (set! turns0 (tr 'turns)) (post-text! "again") (enter!)))
             (cons (lambda () (and (card-scope) (tr 'hit-center 'perm (cons (car (tr 'pending-permissions)) "always"))))
                   (lambda () (perm! "always")))
             (cons (lambda () (and (> (tr 'turns) turns0) (not (busy?))))
                   (lambda () (log! 'continued (list (tr 'last-assistant-text) (combined-cap)
                                                     (hash-ref (hash-ref pushed-external "srv") 'combined_cap_usd #f)))
                              (snapshot-and-quit!))))]
      [synth-reconnect?
       ;; 断线重连 + 每主机草稿：local 输入框留草稿 → 切到 srv（输入框是空的）→ 留另一份草稿 → 切回 local（草稿还在）
       ;; → 停掉 srv 的守护进程（模拟断线）→ 自动重连（--spawn 重新起）→ 切到 srv（它的草稿还在）
       ;; → 设置页手动断开 srv → 2.5 s 后仍是断开（不自动重连）→ Esc
       (define (log! k v) (set! sessions-log (cons (cons k v) sessions-log)))
       (define (row-key id) (let ([p (for/first ([se (in-list (hs-sessions (H id)))] #:when (hash-ref se 'current #f)) (hash-ref se 'path #f))])
                              (and p (string-append id "\u0001" p))))
       (define (click-row! id) (click-at! (sbp 'hit-center 'resume (row-key id))))
       (define (click-sv! kind payload) (click-at! (svp 'hit-center kind payload)))
       (define (serve-name id) (let ([c (hash-ref (or (hs-config (H id)) (hasheq)) 'command '())]) (let ([m (member "--name" c)]) (and m (cadr m)))))
       (define t-disc #f)
       (list (cons (lambda () (and (H "srv") (hs-ready (H "local")) (hs-ready (H "srv")) (hs-replayed? (H "srv")) (row-key "srv") (row-key "local")))
                   (lambda () (post-text! "draft for local")))
             (cons (lambda () (and (next-iteration?) (equal? (editor-text e) "draft for local") (sbp 'hit-center 'resume (row-key "srv"))))
                   (lambda () (click-row! "srv")))
             (cons (lambda () (and (equal? active "srv") (next-iteration?)))
                   (lambda () (log! 'srv-input-empty (editor-text e)) (post-text! "draft for srv")))
             (cons (lambda () (and (next-iteration?) (equal? (editor-text e) "draft for srv") (sbp 'hit-center 'resume (row-key "local"))))
                   (lambda () (click-row! "local")))
             (cons (lambda () (and (equal? active "local") (next-iteration?)))
                   (lambda () (log! 'local-draft (editor-text e))
                              ;; 模拟断线：停掉 srv 的守护进程（它的内核退出 → server_exit → 传输结束）
                              (define-values (p _o _i _e)
                                (parameterize ([current-environment-variables (environment-variables-copy initial-env)])
                                  (subprocess #f #f #f (find-executable-path "racket") (path->string SERVE-RKT) "stop" "--name" (serve-name "srv"))))
                              (void)))
             (cons (lambda () (hs-retry-at (H "srv")))
                   (lambda () (log! 'retry-scheduled (list (hs-attempts (H "srv")) (host-status "srv")))))
             (cons (lambda () (and (hs-ready (H "srv")) (not (hs-retry-at (H "srv"))) (not (hs-core-exit (H "srv"))) (row-key "srv") (sbp 'hit-center 'resume (row-key "srv"))))
                   (lambda () (log! 'reconnected (host-status "srv")) (click-row! "srv")))
             ;; 刚重连时它的会话表还在刷新，行可能挪位：30 轮还没切过去就再点一次
             (cons (lambda () (or (and (equal? active "srv") (next-iteration?))
                                  (and (> loop-iter (+ last-fire-iter 30)) (row-key "srv") (sbp 'hit-center 'resume (row-key "srv"))
                                       (begin (click-row! "srv") (set! last-fire-iter loop-iter) #f))))
                   (lambda () (log! 'srv-draft (editor-text e)) (wi 'post-key! "," 43 '(cmd))))
             (cons (lambda () (and (svp 'open?) (svp 'hit-center 'section 'hosts))) (lambda () (click-sv! 'section 'hosts)))
             (cons (lambda () (svp 'hit-center 'host-disconnect "srv")) (lambda () (click-sv! 'host-disconnect "srv") (set! t-disc (current-inexact-milliseconds))))
             (cons (lambda () (and t-disc (> (- (current-inexact-milliseconds) t-disc) 2500.0)))
                   (lambda () (log! 'manual-stays-disconnected (list (host-status "srv") (and (hs-retry-at (H "srv")) #t)))
                              (wi 'post-key! (string (integer->char 27)) 53)))
             (cons (lambda () (not (svp 'open?))) (lambda () (log! 'closed #t) (snapshot-and-quit!))))]
      [synth-hosts?
       ;; 设置页「主机」：断开 srv（侧栏显示断开）→ 连接（重新补齐）→ 添加连不上的 ssh 主机（.invalid：ssh 立刻失败 → 断开，原因在 stderr）→ 移除 → Esc
       (define (log! k v) (set! sessions-log (cons (cons k v) sessions-log)))
       (define (click-sv! kind payload) (click-at! (svp 'hit-center kind payload)))
       (define BAD "nobody@pi2-test.invalid")
       (list (cons (lambda () (and (H "local") (H "srv") (hs-ready (H "local")) (hs-ready (H "srv")))) (lambda () (wi 'post-key! "," 43 '(cmd))))
             (cons (lambda () (and (svp 'open?) (svp 'hit-center 'section 'hosts))) (lambda () (click-sv! 'section 'hosts)))
             (cons (lambda () (svp 'hit-center 'host-disconnect "srv")) (lambda () (click-sv! 'host-disconnect "srv")))
             (cons (lambda () (and (equal? (host-status "srv") "disconnected") (svp 'hit-center 'host-connect "srv")))
                   (lambda () (log! 'disconnected (host-status "srv")) (click-sv! 'host-connect "srv")))
             (cons (lambda () (and (equal? (host-status "srv") "connected") (hs-replayed? (H "srv")) (svp 'hit-center 'add-host #f)))
                   (lambda () (log! 'reconnected (host-status "srv")) (click-sv! 'add-host #f)))
             (cons (lambda () (and (next-iteration?) (svp 'editing?))) (lambda () (post-text! BAD) (enter!)))
             (cons (lambda () (and (next-iteration?) (svp 'editing?))) (lambda () (enter!)))                    ; serve 名字留空 = default
             ;; ssh 立刻失败 → 自动重连已排上（连续失败 8 次才停）
             (cons (lambda () (and (H BAD) (hs-core-exit (H BAD)) (hs-retry-at (H BAD))))
                   (lambda () (log! 'bad-host (list (host-status BAD) (hs-attempts (H BAD)) (pair? (ks 'stderr-tail BAD 3)))) (click-sv! 'ask-host-remove BAD)))
             (cons (lambda () (svp 'hit-center 'host-remove BAD)) (lambda () (click-sv! 'host-remove BAD)))
             (cons (lambda () (not (H BAD)))
                   (lambda () (log! 'removed host-order) (wi 'post-key! (string (integer->char 27)) 53)))
             (cons (lambda () (not (svp 'open?))) (lambda () (log! 'closed #t) (snapshot-and-quit!))))]
      [synth-multihost?
       ;; 一窗多主机（主机 local + srv，都是脚本化内核）：local 上发一轮挂住的 → 切到 srv（标题行「＋」新会话）→ srv 上一轮带权限的跑完
       ;; （local 那轮仍在跑：两台同时跑）→ srv 再发一轮停在权限询问上 → 切回 local（侧栏 srv 的会话显示「等待权限」）→ Esc 停 local
       ;; → 切回 srv：权限卡片还在 → 允许 → 跑完
       (define (log! k v) (set! sessions-log (cons (cons k v) sessions-log)))
       (define (row-key id) (let ([p (for/first ([se (in-list (hs-sessions (H id)))] #:when (hash-ref se 'current #f)) (hash-ref se 'path #f))])
                              (and p (string-append id "\u0001" p))))
       (define (row-of id) (let ([k (row-key id)]) (and k (findf (lambda (se) (equal? (hash-ref se 'key #f) k)) (sbp 'sessions)))))
       (define (click-row! id) (click-at! (sbp 'hit-center 'resume (row-key id))))
       (define (perm-yes!) (click-at! (tr 'hit-center 'perm (cons (car (tr 'pending-permissions)) "yes"))))
       (define srv-turns0 #f)
       (list (cons (lambda () (and (H "local") (H "srv") (hs-ready (H "local")) (hs-ready (H "srv")) (row-key "local") (row-key "srv")
                                   (sbp 'hit-center 'host-new "srv")))
                   (lambda () (log! 'hosts host-order) (post-text! "now hang") (enter!)))
             (cons (lambda () (and (host-busy? "local") (let ([s (tr 'last-assistant-text)]) (and s (regexp-match? #rx"partial" s)))))
                   (lambda () (click-at! (sbp 'hit-center 'host-new "srv"))))
             (cons (lambda () (and (equal? active "srv") (next-iteration?) (not (busy?))))
                   (lambda () (log! 'switched-to active) (post-text! "please echo something") (enter!)))
             (cons (lambda () (and (pair? (tr 'pending-permissions)) (tr 'hit-center 'perm (cons (car (tr 'pending-permissions)) "yes"))))
                   (lambda () (perm-yes!)))
             (cons (lambda () (and (>= (tr 'turns) 1) (not (busy?))))
                   (lambda () (log! 'srv-done-while-local-busy (list (tr 'last-assistant-text) (host-busy? "local")))
                              (set! srv-turns0 (tr 'turns))
                              (post-text! "echo again") (enter!)))
             (cons (lambda () (and (pair? (tr 'pending-permissions)) (row-key "local") (sbp 'hit-center 'resume (row-key "local"))))
                   (lambda () (click-row! "local")))
             (cons (lambda () (and (equal? active "local") (next-iteration?) (row-of "srv") (hash-ref (row-of "srv") 'badge #f)))
                   (lambda () (log! 'srv-badge (hash-ref (row-of "srv") 'badge #f))
                              (wi 'post-key! (string (integer->char 27)) 53)))                       ; Esc：停 local 那轮
             (cons (lambda () (and (not (host-busy? "local")) (sbp 'hit-center 'resume (row-key "srv"))))
                   (lambda () (log! 'local-stopped (tr 'summary)) (click-row! "srv")))
             (cons (lambda () (and (equal? active "srv") (pair? (tr 'pending-permissions)) (tr 'hit-center 'perm (cons (car (tr 'pending-permissions)) "yes"))))
                   (lambda () (log! 'srv-card-still-there #t) (perm-yes!)))
             (cons (lambda () (and (> (tr 'turns) srv-turns0) (not (busy?))))
                   (lambda () (log! 'srv-final (tr 'summary)) (snapshot-and-quit!))))]
      [synth-settings2?
       ;; 设置页第二组：路由池加成员（选帐号 → 其他… 手输模型）、上移、移除；别名添加；规则新建再删除（两次点击）；
       ;; 界面缩放 125%（缩放后点击照样命中）→ ⌘0 复原 → Esc
       (define (log! k v) (set! sessions-log (cons (cons k v) sessions-log)))
       (define (click-sv! kind payload) (click-at! (svp 'hit-center kind payload)))
       (define (pool-members) (let ([r (and last-providers (findf (lambda (r) (equal? (hash-ref r 'name #f) "pool")) (hash-ref last-providers 'routes '())))])
                                (and r (for/list ([m (in-list (hash-ref r 'members '()))]) (format "~a/~a" (hash-ref m 'instance) (hash-ref m 'model))))))
       (define n0 #f)
       (list (cons (lambda () ready) (lambda () (wi 'post-key! "," 43 '(cmd))))
             (cons (lambda () (and (svp 'open?) (svp 'hit-center 'section 'pools))) (lambda () (click-sv! 'section 'pools)))
             (cons (lambda () (and (pool-members) (svp 'hit-center 'add-member "pool")))
                   (lambda () (set! n0 (length (pool-members))) (log! 'members0 (pool-members)) (click-sv! 'add-member "pool")))
             (cons (lambda () (svp 'hit-center 'pick-account (vector "pool" "fb"))) (lambda () (click-sv! 'pick-account (vector "pool" "fb"))))
             (cons (lambda () (svp 'hit-center 'pick-other (vector "pool" "fb"))) (lambda () (click-sv! 'pick-other (vector "pool" "fb"))))
             (cons (lambda () (and (next-iteration?) (svp 'editing?))) (lambda () (post-text! "m3") (enter!)))
             (cons (lambda () (let ([m (pool-members)]) (and m (= (length m) (add1 n0)) (svp 'hit-center 'member-up (vector "pool" n0)))))
                   (lambda () (log! 'added (pool-members)) (click-sv! 'member-up (vector "pool" n0))))
             (cons (lambda () (let ([m (pool-members)]) (and m (equal? (list-ref m (sub1 n0)) "fb/m3"))))
                   (lambda () (log! 'moved-up (pool-members)) (click-sv! 'member-remove (vector "pool" 0))))
             (cons (lambda () (let ([m (pool-members)]) (and m (= (length m) n0) (svp 'hit-center 'section 'aliases))))
                   (lambda () (log! 'removed-first (pool-members)) (click-sv! 'section 'aliases)))
             (cons (lambda () (svp 'hit-center 'add-alias #f)) (lambda () (click-sv! 'add-alias #f)))
             (cons (lambda () (and (next-iteration?) (svp 'editing?))) (lambda () (post-text! "fast") (enter!)))
             (cons (lambda () (next-iteration?)) (lambda () (post-text! "m-fast") (enter!)))
             (cons (lambda () (and last-aliases (pair? (hash-ref last-aliases 'aliases '())) (svp 'hit-center 'alias-remove "fast")))
                   (lambda () (log! 'aliases (hash-ref last-aliases 'aliases)) (click-sv! 'section 'rules)))
             (cons (lambda () (svp 'hit-center 'new-rule #f)) (lambda () (click-sv! 'new-rule #f)))
             (cons (lambda () (and (next-iteration?) (svp 'editing?))) (lambda () (post-text! "brief") (enter!)))
             (cons (lambda () (next-iteration?)) (lambda () (post-text! "*") (enter!)))
             (cons (lambda () (next-iteration?)) (lambda () (post-text! "Be brief.") (enter!)))
             (cons (lambda () (svp 'hit-center 'ask-delete-rule "brief")) (lambda () (log! 'rule-created #t) (click-sv! 'ask-delete-rule "brief")))
             (cons (lambda () (svp 'hit-center 'delete-rule "brief")) (lambda () (click-sv! 'delete-rule "brief")))
             (cons (lambda () (and (next-iteration?) (not (svp 'hit-center 'ask-delete-rule "brief")) (not (svp 'hit-center 'delete-rule "brief"))
                                   (svp 'hit-center 'section 'general)))
                   (lambda () (log! 'rule-deleted #t) (click-sv! 'section 'general)))
             (cons (lambda () (svp 'hit-center 'zoom 1.25)) (lambda () (click-sv! 'zoom 1.25)))
             (cons (lambda () (and (= zoom 1.25) (next-iteration?) (svp 'hit-center 'section 'shortcuts)))
                   (lambda () (log! 'zoom (list zoom (/ (round (* 10 w)) 10.0) (/ (round (* 10 pw)) 10.0))) (click-sv! 'section 'shortcuts)))   ; 缩放后的点击
             (cons (lambda () (eq? (svp 'section) 'shortcuts)) (lambda () (log! 'zoomed-click 'shortcuts) (wi 'post-key! "0" 29 '(cmd))))
             (cons (lambda () (= zoom 1.0)) (lambda () (log! 'zoom-reset zoom) (wi 'post-key! (string (integer->char 27)) 53)))
             (cons (lambda () (not (svp 'open?))) (lambda () (log! 'closed #t) (snapshot-and-quit!))))]
      [synth-settings?
       ;; 设置页（⌘,）：帐号与密钥里给 grok 添加帐号 team 并输入 key（真按键）→ 列表里出现 grok[team]；
       ;; 权限改成 read-only（内核 state 回报）→ Esc 关闭。日志里不得出现 key 原文（冒烟测试检查日志文件）
       (define (log! k v) (set! sessions-log (cons (cons k v) sessions-log)))
       (define (click-sv! kind payload) (click-at! (svp 'hit-center kind payload)))
       (list (cons (lambda () ready) (lambda () (wi 'post-key! "," 43 '(cmd))))
             (cons (lambda () (svp 'open?)) (lambda () (log! 'opened #t) (click-sv! 'section 'accounts)))
             (cons (lambda () (svp 'hit-center 'new-account "grok")) (lambda () (click-sv! 'new-account "grok")))
             (cons (lambda () (svp 'editing?)) (lambda () (post-text! "team") (enter!)))
             (cons (lambda () (svp 'secret?)) (lambda () (post-text! "sk-test-123456") (enter!)))
             (cons (lambda () (svp 'hit-center 'edit-key "grok[team]"))
                   (lambda () (log! 'account-added "grok[team]") (click-sv! 'section 'permissions)))
             (cons (lambda () (svp 'hit-center 'permission "read-only")) (lambda () (click-sv! 'permission "read-only")))
             (cons (lambda () (equal? last-permission "read-only"))
                   (lambda () (log! 'permission last-permission) (wi 'post-key! (string (integer->char 27)) 53)))
             (cons (lambda () (not (svp 'open?))) (lambda () (log! 'closed #t) (snapshot-and-quit!))))]
      [synth-route?
       ;; 路由池（G6，脚本化内核预置 @pool = fa(总是 429) / fb）：菜单选 @pool → 发一条 → fa 限流、自动切到 fb、
       ;; 对话区出切换提示、芯片带不可用标记、菜单右栏列成员 → 手动固定 fa → Esc
       (define (st k) (let ([h (mpp 'state)]) (and h (hash-ref h k #f))))
       (define (log! k v) (set! sessions-log (cons (cons k v) sessions-log)))
       (define (click-mp! kind payload) (click-at! (mpp 'hit-center kind payload)))
       (define (menu-ready? kind payload) (and (mpp 'open?) (mpp 'hit-center kind payload)))
       (list (cons (lambda () (and ready (mpp 'hit-center 'toggle #t))) (lambda () (click-mp! 'toggle #t)))
             (cons (lambda () (menu-ready? 'provider "@pool")) (lambda () (click-mp! 'provider "@pool")))
             (cons (lambda () (and (equal? (st 'provider) "@pool") (menu-ready? 'member 1)))
                   (lambda () (log! 'selected (list (st 'provider) (st 'model))) (click-mp! 'toggle #t)))
             (cons (lambda () (not (mpp 'open?))) (lambda () (post-text! "route me") (enter!)))
             (cons (lambda () (and (>= (tr 'turns) 1) (not (busy?)) (equal? (st 'model) "m2")))
                   (lambda ()
                     (log! 'answer (tr 'last-assistant-text))
                     (log! 'transcript (tr 'summary))
                     (click-mp! 'toggle #t)))
             (cons (lambda () (menu-ready? 'member 0)) (lambda () (click-mp! 'member 0)))   ; 手动固定 fa（冷却中）
             (cons (lambda () (equal? (st 'model) "m1"))
                   (lambda () (log! 'pinned (st 'model)) (wi 'post-key! (string (integer->char 27)) 53)))
             (cons (lambda () (not (mpp 'open?))) (lambda () (snapshot-and-quit!))))]
      [live-prompt
       ;; 真实模型验收：权限卡片只对「bash 且命令匹配 allow-rx」点允许，其余拒绝；同一张卡片只决定一次
       (define handled '())
       (define (next-pending)
         (for/first ([id (in-list (tr 'pending-permissions))]
                     #:when (and (not (memv id handled)) (tr 'hit-center 'perm (cons id "yes"))))
           id))
       (define (decide!)
         (define id (next-pending))
         (set! handled (cons id handled))
         (define info (tr 'permission-info id))
         (define cmd (and info (hash? (caddr info)) (hash-ref (caddr info) 'command #f)))
         (define ok? (and info (equal? (car info) "bash") (string? cmd) allow-rx (regexp-match? allow-rx cmd)))
         (eprintf "live: permission ~a ~s → ~a\n" (and info (car info)) cmd (if ok? "yes" "no"))
         (click-at! (tr 'hit-center 'perm (cons id (if ok? "yes" "no")))))
       (append (list (cons (lambda () ready) (lambda () (post-text! live-prompt) (enter!))))
               (for/list ([i 8]) (cons (lambda () (or (next-pending) (>= (tr 'turns) 1)))
                                       (lambda () (when (next-pending) (decide!)))))
               (list (cons (lambda () (>= (tr 'turns) 1)) snapshot-and-quit!)))]
      [synth?
       (list (cons (after 600.0) (lambda () (post-text! "a")))
             (cons (after 700.0) (lambda ()
                                   (clip 'set! TRACE-SAMPLE)
                                   (cond [(wi 'key?) (set! paste-path 'menu) (wi 'post-key! "v" 9 '(cmd))]
                                         [else (set! paste-path 'direct) (wi 'post-paste!)])))
             (cons (after 800.0) (lambda () (post-text! " fix")))
             (cons (after 900.0) (lambda () (wi 'simulate-ime! '((mark "n" 1) (mark "ni" 2)))))
             (cons (after 1000.0) (lambda ()
                                    (define-values (r marked?) (wi 'ime-query))
                                    (set! ime-query (list r marked?))
                                    (wi 'simulate-ime! '((insert "你")))))
             (cons (after 1100.0) (lambda () (wi 'post-key! "\r" 36 '(shift))))           ; ⇧Enter → 换行
             (cons (after 1150.0) (lambda () (post-text! "x")))
             (cons (after 1200.0) enter!)                                                ; Enter → 发送
             (cons (after 1400.0) (lambda ()
                                    (set! synth-check (if (equal? submitted (list (string-append "a" TRACE-SAMPLE " fix你\nx"))) 'ok 'mismatch))
                                    (snapshot-and-quit!))))]
      [ime-real
       ;; 真实输入法（P4）：五笔 86 二级简码 wq → 你，vb → 好；按键必须带 CGEvent（post-key-cg!）
       (list (cons (after 650.0) (lambda () (eprintf "ime diag: ~a\n" (wi 'ime-diagnostics))))
             (cons (after 700.0) (lambda () (wi 'post-key-cg! 13) (wi 'post-key-cg! 12)))
             (cons (after 1100.0) (lambda () (wi 'post-key-cg! 49)))
             (cons (after 1500.0) (lambda () (wi 'post-key-cg! 9) (wi 'post-key-cg! 11)))
             (cons (after 1900.0) (lambda () (wi 'post-key-cg! 49)))
             (cons (after 2400.0) quit!))]
      [else '()]))
  (set! lint-active? (and (not replay-path) (or (pair? synth-steps) (and (getenv "PI2_LINT") #t))))   ; 逐帧显示检查
  (define (run-loop)
    (let loop ([dirty? #t] [blink (caret-on?)])
      (set! loop-iter (add1 loop-iter))
      ;; 联系：内核事件 → 对话区
      (define core-dirty?
        (for/fold ([d #f]) ([pr (in-list (if ks (ks 'poll!) '()))])
          (define id (car pr))
          (define cev (cdr pr))
          (define h (or (H id) (begin (hash-set! host-tab id (make-hs id id #f)) (H id))))
          (define act? (equal? id active))
          (jr 'record! 'core (host-tag id cev))
          (define (to! j) (send! j id))
          (define (pstate! ev) (set-hs-pstate! h (hash-merge (hs-pstate h) ev)) (when act? (mpp 'set-state! ev)))
          (define (budget! b) (when (hash? b) (set-hs-budget! h b) (when act? (svp 'set-data! 'budget b)) (push-external!) (push-budget-all!)))
          (guarded 'core-event (lambda ()
          (case (hash-ref cev 'type #f)
            [("sessions") (set-hs-sessions! h (let ([ss (hash-ref cev 'sessions '())]) (if (list? ss) (filter hash? ss) '())))
                          (catalog-sync-host! cat id (hs-sessions h))                  ; 会话挂到工作区（幂等；回放时在内存里重做）
                          (push-catalog-view! id)                                      ; 给这台内核的 sessions_overview
                          (maybe-auto-title! id)]                                      ; 没标题、够长的会话：本机模型起标题（§11.4 B）
            [("session" "turn_complete") (to! (hasheq 'type "list_sessions")) (budget! (hash-ref cev 'budget #f))
                                         (when (equal? (hash-ref cev 'type #f) "turn_complete") (hix-pump!) (mem-auto-pump!))]   ; 空闲了：项目索引 / 自动记忆可以接着做
            [("ok") (case (hash-ref cev 'for #f)
                      [("delete_session" "restore_session") (to! (hasheq 'type "list_sessions"))]   ; 删除 / 撤销删除后刷新列表
                      [("add_key" "define_route") (to! (hasheq 'type "list_providers"))]
                      [("set_permission_mode") (to! (hasheq 'type "state"))]
                      [("set_alias") (to! (hasheq 'type "list_aliases"))]
                      [("set_chat_config") (to! (hasheq 'type "chat_usage"))]
                      [("set_budget") (to! (hasheq 'type "get_budget"))]
                      [("save_adaptation" "delete_adaptation") (to! (hasheq 'type "list_adaptations")) (to! (hasheq 'type "state"))]
                      [("set_provider" "set_model" "set_reasoning" "route_pin" "set_adaptation") (pstate! cev) (to! (hasheq 'type "state"))
                       (when (and act? (equal? (hash-ref cev 'for #f) "set_adaptation") (svp 'open?)) (to! (hasheq 'type "list_adaptations")))]
                      [else (void)])]
            [("state") (pstate! cev) (set-hs-wire! h (hash-ref cev 'wire #f))
                       (budget! (hash-ref cev 'budget #f))
                       (set-hs-permission! h (hash-ref cev 'permission_mode #f))
                       (when act? (svp 'set-data! 'permission (hash-ref cev 'permission_mode "normal")))
                       (when (hash? (hash-ref cev 'route #f))
                         (set-hs-providers! h (route-into (hs-providers h) (hash-ref cev 'route)))
                         (when act? (mpp 'set-route! (hash-ref cev 'route))))]
            [("providers") (set-hs-providers! h cev) (when act? (mpp 'set-options! cev) (svp 'set-data! 'providers cev))]
            [("adaptations") (set-hs-rules! h (hash-ref cev 'rules '())) (when act? (svp 'set-data! 'rules (hash-ref cev 'rules '())))]
            [("aliases") (set-hs-aliases! h cev) (when act? (svp 'set-data! 'aliases (hash-ref cev 'aliases '())))]
            [("budget") (budget! cev)]
            [("route_status") (set-hs-providers! h (route-into (hs-providers h) cev)) (when act? (mpp 'set-route! cev))]
            [("route_switch") (to! (hasheq 'type "state"))]                        ; 当前成员 / 模型变了
            [("auto") (pstate! cev)]
            [("ready") (set-hs-ready! h cev) (set-hs-attempts! h 0) (set-hs-retry-at! h #f) (pstate! cev) (to! (hasheq 'type "list_sessions")) (to! (hasheq 'type "state"))
                       (let ([cp (gsetting 'stream-checkpoint #f)]) (when cp (to! (hasheq 'type "set_stream_checkpoint" 'seconds cp))))   ; 流式检查点（设置页）
                       (to! (hasheq 'type "set_model_input" 'hidden_text (symbol->string (hidden-text-mode))))   ; 隐藏文字送给模型时（高级设置；内核可能是长跑的守护进程，每次都推）
                       (when hix-active? (to! (hasheq 'type "host_scan" 'roots (let ([r (gsetting 'host-index-roots #f)]) (if (and (list? r) (pair? r)) r '("~"))))))   ; 项目索引：扫这台主机
                       (tr 'note-to! id (t 'app.connected (hash-ref cev 'provider "?") (hash-ref cev 'model "?")
                                          (hash-ref cev 'session "?")))]
            [("history") (set-hs-history! h (hash-ref cev 'messages '()))]
            [("changeset_proposed") (on-changeset-proposed! id cev)]                ; agent 提交的会话整理清单 → 审阅
            [("memory_note") (on-memory-note! id cev)]                              ; 模型记了一条记忆 → 记忆库（全局：待确认）
            ;; 工作区测试流程面板：数据到了（刚要打开 / 开着就刷新）；面板里跑的结果 → 显示、再要一次数据（历史）
            [("test_flow") (when (equal? id active)
                             (cond [tests-wanted? (set! tests-wanted? #f) (insp 'close!) (tpp 'open! cev)]
                                   [(tpp 'open?) (tpp 'update! cev)]))]
            [("test_flow_result") (when (and (equal? id active) (tpp 'open?)) (tpp 'set-result! cev) (to! (hasheq 'type "test_flow")))]
            [("memory_used") (memory-used! mem (filter string? (let ([v (hash-ref cev 'ids '())]) (if (list? v) v '()))) (hash-ref cev 'how "?"))]   ; 最近用过（老化）
            [("memory_input") (on-memory-input! id cev)]                             ; 自动记忆：会话文字到了 → 提取
            [("local_completed") (on-local-completed! cev)]                          ; 自动记忆：提取 / 合并的回答
            ;; 项目索引：扫描结果 → 库；远端仓库的输入 → 交给本机内核描述；描述结果 → 库 → 下一个
            [("host_repos") (unless (hash-ref cev 'error #f)
                              (hix-set-repos! hix id (filter hash? (let ([v (hash-ref cev 'repos '())]) (if (list? v) v '()))))
                              (after-hix-change! id))
                            (hix-pump!)]
            [("describe_input")
             (define dir (format "~a" (hash-ref cev 'dir "")))
             (define runner (local-runner))
             (define-values (ep model) (hix-model-cfg))
             (cond
               [(or (hash-ref cev 'error #f) (not runner))
                (define e (hix-entry hix id dir))
                (hix-set-error! hix id dir (if e (hash-ref e 'fingerprint "") "") (or (hash-ref cev 'error #f) "no kernel on this Mac to run the model"))
                (set! hix-flight #f) (after-hix-change! id) (hix-pump!)]
               [else (send! (hasheq 'type "describe_project" 'dir dir 'host id 'input (hash-ref cev 'input "") 'fingerprint (hash-ref cev 'fingerprint "")
                                    'endpoint ep 'model (or model 'null))
                            runner)])]
            [("project_described")
             (define host (let ([h (hash-ref cev 'host 'null)]) (if (string? h) h id)))
             (define dir (format "~a" (hash-ref cev 'dir "")))
             (define e (hix-entry hix host dir))
             (cond [(hash-ref cev 'error #f) => (lambda (msg) (hix-set-error! hix host dir (if e (hash-ref e 'fingerprint "") "") msg))]
                   [(hash? (hash-ref cev 'desc #f)) (hix-set-desc! hix host dir (hash-ref cev 'fingerprint "") (hash-ref cev 'desc) (hash-ref cev 'model #f))])
             (set! hix-flight #f) (after-hix-change! host) (hix-pump!)]
            [("session_stats") (insp 'open! (hash-set* cev 'groups (catalog-groups-text id (format "~a" (hash-ref cev 'path "")))   ; 右键「信息…」
                                                       'memory-text (for*/hash ([e (in-list (append (memory-entries mem) (memory-entries mem #:state "deleted")))])   ; 记忆 id → 文字
                                                                      (values (hash-ref e 'id) (short-text (hash-ref e 'text ""))))))]
            [("chat_usage") (set! chat-usage-last (hash-set cev 'seq (add1 (if (hash? chat-usage-last) (hash-ref chat-usage-last 'seq 0) 0))))
                            (when act? (svp 'set-data! 'chat-usage cev))           ; 聊天目录占用（设置页）；自动清理删了目录 → 提示
                            (let ([rm (hash-ref cev 'removed '())])
                              (when (pair? rm)
                                (set! toast (cons (t 'app.chat-cleaned (length rm) (/ (round (/ (for/sum ([x rm]) (hash-ref x 'bytes 0)) 107374182.4)) 10.0))
                                                  (+ (current-inexact-milliseconds) 4000.0)))
                                (to! (hasheq 'type "list_sessions"))))]
            [("export") (let ([txt (hash-ref cev 'text "")])                       ; 右键「导出」：复制
                          (clip 'set! txt)
                          (set! toast (cons (t 'app.exported (if (equal? (hash-ref cev 'format #f) "json") "JSON" "Markdown") (string-length txt))
                                            (+ (current-inexact-milliseconds) 2000.0))))]
            [("replay_end") (set-hs-replayed?! h #t)
                            (define rd (hs-ready h))
                            (when (and rd (hash-ref rd 'attached #f))
                              (tr 'note-to! id (t 'app.attached (hash-ref rd 'server "?") (if (hash-ref rd 'busy #f) (t 'app.running) (t 'app.idle)))))]
            [("core_exit") (set-hs-core-exit! h (hash-ref cev 'code "?"))
                           (unless (and (eqv? (hs-core-exit h) 0) (not (hash-ref cev 'remote #f)))   ; 远端：连接断了也给出原因
                             (for ([l (in-list (ks 'stderr-tail id 8))]) (tr 'note-to! id l 'error)))
                           ;; 传输子进程断了（守护进程 / ssh）：按退避自动重连；手动断开的、连续失败太多次的不重连
                           (when (and (hash-ref cev 'remote #f) (not (hs-manual? h)) (< (hs-attempts h) RECONNECT-MAX))
                             (set-hs-retry-at! h (+ (current-inexact-milliseconds) (reconnect-delay-ms (hs-attempts h))))
                             (set-hs-attempts! h (add1 (hs-attempts h)))
                             (tr 'note-to! id (t 'app.reconnecting (hs-attempts h)) 'warn))]
            [("gui_reconnect") (when replay-path (reconnect-host! id #:auto? #t))]   ; 回放：录制时这里自动重连过
            [else (void)])
          (tr 'apply-to! id cev)))
          #t))
      ;; 侧栏：会话表 / 运行中 / 等待权限都可能变了
      (when core-dirty? (guarded 'sidebar refresh-sidebar!))
      (define media-dirty? (mdp 'poll!))                 ; 缩略图解好了 / 播放进度变了
      (guarded 'reconnect fire-reconnects!)
      ;; 联系：窗口事件 → 输入框 / 对话区
      (define evs (win 'pump!))
      (define quit? #f)
      (define dirty*
        (for/fold ([d (or dirty? core-dirty? media-dirty?)]) ([ev0 (in-list evs)])
          ;; 日志记窗口给的原始事件（物理坐标）；处理用逻辑坐标
          (cond
            [(and (svp 'secret?) (secret-input-event? ev0)) (set! secret-dirty? #t)]   ; 输 key：不记
            [(and (ev:paste? ev0) (not (svp 'open?)) (paste-as-secret? (ev:paste-text ev0)))   ; 聊天里粘贴的密钥：只记句柄
             (jr 'record! 'win (ev:secret (secret-or-handle (string-trim (ev:paste-text ev0))) (if (eq? secret-mode 'suggest) "chat-pending" "chat")))]
            [(ev:attach? ev0) (jr 'record! 'win (ev:attach (attach-for-journal (ev:attach-items ev0))))]   ; 附件：只记 sha 与大小
            [else
             (when (and secret-dirty? (svp 'secret?))                                  ; 先记句柄，再记触发提交的事件
               (jr 'record! 'win (ev:secret (secret-text (editor-flatten (svp 'editor))) "settings-key")))
             (set! secret-dirty? #f)
             (jr 'record! 'win ev0)])
          (define ev (unzoom ev0))
          (hash-update! event-kinds (vector-ref (struct->vector ev) 0) add1 0)
          (guarded 'window-event (lambda ()
          (cond
            [(ev:close? ev) (set! quit? #t) d]
            [(ev:resize? ev)
             (set! resizes (cons (list (ev:resize-w ev) (ev:resize-h ev)) resizes))
             (define changed? (not (and (= pw (ev:resize-w ev)) (= ph (ev:resize-h ev)) (= ps (ev:resize-scale ev)))))
             (when changed? (apply-size! (ev:resize-w ev) (ev:resize-h ev) (ev:resize-scale ev)))
             (or d changed?)]
            [(ev:focus? ev) (set! focused? (ev:focus-on? ev)) #t]
            [(ev:key? ev)
             (when (ev:key-down? ev) (set! last-mods (ev:key-mods ev)) (set! keys (cons (ev:key-chars ev) keys)))
             (cond [(and (ev:key-down? ev) (or sidebar-focus? (sbp 'context-open?)) (not (svp 'open?)) (not (sbp 'editing?)) (sidebar-key! ev)) #t]   ; 侧栏单键
                   [(and (ev:key-down? ev) (memq 'cmd (ev:key-mods ev)) (memq 'shift (ev:key-mods ev)) (eqv? (ev:key-code ev) 14) (not (svp 'open?)))
                    ;; ⌘⇧E：侧栏键盘模式（从当前会话开始）开 / 关
                    (if sidebar-focus?
                        (set-sidebar-focus! #f)
                        (set-sidebar-focus! (let ([c (findf (lambda (se) (hash-ref se 'current #f)) (sbp 'sessions))])
                                              (and c (hash-ref c 'key (hash-ref c 'path #f))))))
                    #t]
                   [(and (ev:key-down? ev) (memq 'cmd (ev:key-mods ev)) (memq 'alt (ev:key-mods ev)) (eqv? (ev:key-code ev) 2))
                    (dv 'toggle!) #t]                                                     ; ⌘⌥D：调试面板（按 keyCode：⌥ 会改字符）
                   [(and (ev:key-down? ev) (memq 'cmd (ev:key-mods ev)) (memq 'shift (ev:key-mods ev)) (eqv? (ev:key-code ev) 17) (not (svp 'open?)))
                    (if (tpp 'open?) (tests-action! (list 'close)) (open-tests!)) #t]        ; ⌘⇧T：工作区测试流程面板
                   [(and (ev:key-down? ev) (memq 'cmd (ev:key-mods ev)) (equal? (ev:key-chars ev) ","))
                    (if (svp 'open?) (svp 'close!) (open-settings!)) #t]                 ; ⌘,：设置页
                   [(and (ev:key-down? ev) (memq 'cmd (ev:key-mods ev)) (memq 'shift (ev:key-mods ev)) (member (ev:key-chars ev) '("v" "V"))
                         (not (svp 'open?)))
                    ;; ⌘⇧V：以密钥粘贴（不看格式）。日志在这条按键之后补记句柄，回放时据此插 chip（回放没有剪贴板）
                    (define txt (clip 'get))
                    (when (and (string? txt) (not (string=? (string-trim txt) "")))
                      (insert-secret-chip! txt #f)
                      (jr 'record! 'win (ev:secret (secret-or-handle (string-trim txt)) "chat")))
                    #t]
                   [(and (ev:key-down? ev) (memq 'cmd (ev:key-mods ev)) (memq 'shift (ev:key-mods ev)) (eqv? (ev:key-code ev) 40) (not (svp 'open?)))
                    ;; ⌘⇧K：选中的文字 → 密钥 chip；操作日志里整段出现的原文换成句柄（逐字手打的那些按键补不回来）
                    (define r (editor-secret-selection! e))
                    (cond [r (jr 'redact! (string-trim (cdr r)) (secret-text (string-trim (cdr r))))
                             (set! toast (cons (t 'app.secret-marked) (+ (current-inexact-milliseconds) 2500.0)))]
                          [else (set! toast (cons (t 'app.secret-select-hint) (+ (current-inexact-milliseconds) 2500.0)))])
                    #t]
                   [(and (ev:key-down? ev) (memq 'cmd (ev:key-mods ev)) (equal? (ev:key-chars ev) "b"))
                    (set! sidebar? (not sidebar?)) #t]                                    ; ⌘B：侧栏开 / 关
                   [(and (ev:key-down? ev) (memq 'cmd (ev:key-mods ev)) (memv (ev:key-code ev) '(24 27 29)))
                    ;; ⌘= / ⌘+ 放大，⌘- 缩小，⌘0 复原（按 keyCode：⇧ 会改字符）
                    (set-zoom! (case (ev:key-code ev) [(24) (zoom-step zoom 1)] [(27) (zoom-step zoom -1)] [else 1.0])) #t]
                   [else d])]
            [(or (ev:text? ev) (ev:marked? ev) (ev:paste? ev) (ev:command? ev) (ev:secret? ev) (ev:attach? ev)) (handle-edit! ev)]
            [(ev:scroll? ev)
             ;; 滚轮按指针所在区域分派（事件自带位置），不跟焦点：菜单（哪一栏）> 侧栏 > 输入框 > 对话区。
             ;; 非像素级滚轮（普通鼠标）给的是行数，换成 point
             (define sx (ev:scroll-x ev))
             (define sy (ev:scroll-y ev))
             (define dy (if (ev:scroll-precise? ev) (ev:scroll-dy ev) (* 20.0 (ev:scroll-dy ev))))
             (set! mouse-x sx)
             (cond [(svp 'open?) (svp 'scroll! sx sy dy)]                               ; 设置页：模态
                   [(mpp 'scroll! sx sy dy) (void)]
                   ;; 侧栏的上下文菜单开着：吞掉滚动（与系统菜单一样是模态的）。原来这里关菜单：触控板双指点按（右键）时
                   ;; 两指搭在板上会发 dy≈0 的带阶段的滚动事件 → 菜单一闪就没了（用户 2026-10-25 报告）
                   [(sbp 'context-open?) (void)]
                   [(in-sidebar? sx) (sidebar-scroll! ev sx sy dy)]                ; 侧栏：上下滚，或横扫一行（§11.4 D）
                   [(tpp 'open?) (tpp 'scroll! dy)]                                      ; 测试面板盖着对话区
                   [(rvp 'open?) (rvp 'scroll! dy)]                                      ; 审阅面板盖着对话区
                   [(insp 'open?) (insp 'scroll! dy)]                                    ; 信息面板盖着对话区
                   [(ib 'scroll! sx sy dy) (void)]
                   [else (tr 'scroll! dy)])
             #t]
            [(ev:mouse? ev)
             (define x (ev:mouse-x ev))
             (define y (ev:mouse-y ev))
             (if (svp 'open?)
             ;; 设置页开着：模态，鼠标都归它
             (case (ev:mouse-kind ev)
               [(down) (settings-action! (svp 'hit x y)) (set! last-edit (current-inexact-milliseconds)) #t]
               [(move) (or (svp 'hover! x y) d)]
               [else d])
             (cond
              ;; 分隔线：按下开始拖（改侧栏宽度），拖动中跟着鼠标，松手存进设置
              [(and (eq? (ev:mouse-kind ev) 'down) (on-divider? x) (eqv? (ev:mouse-button ev) 0) (not (mpp 'open?)) (not (sbp 'context-open?)))
               (set! drag-divider? #t) #t]
              [(and drag-divider? (eq? (ev:mouse-kind ev) 'drag))
               (set! sidebar-w (clamp-sidebar-w (if sidebar-right? (- w x) x) w)) #t]
              [(and drag-divider? (eq? (ev:mouse-kind ev) 'up))
               (set! drag-divider? #f) (save-setting! 'sidebar-width sidebar-w) #t]
              [(and (eq? (ev:mouse-kind ev) 'move) (not (eq? (on-divider? x) divider-hot?)))
               (set! divider-hot? (on-divider? x))
               (win 'set-cursor! (if divider-hot? 'resize-h 'arrow))
               #t]
              [else
             (case (ev:mouse-kind ev)
               [(down)
                ;; 右键 / ctrl-点击：只开侧栏行的上下文菜单（或关掉已开的菜单），不做别的
                (define context? (or (eqv? (ev:mouse-button ev) 1) (memq 'ctrl (ev:mouse-mods ev))))
                (define ctx-done?
                  (and context?
                       (begin (cond [(mpp 'open?) (mpp 'close!)]
                                    [(in-sidebar? x) (sbp 'context! x y)]
                                    [else (sbp 'close-context!)])
                              #t)))
                (when (and (sbp 'editing?) (not (in-sidebar? x))) (sbp 'cancel-edit!))   ; 在侧栏外点：放弃改名
                ;; 信息面板开着：对话区上的点击都归它（点外面 / ✕ 关闭）；侧栏照常
                (define tq (and (not ctx-done?) (tpp 'open?) (not (in-sidebar? x)) (begin (tests-action! (tpp 'hit x y)) #t)))   ; 测试面板：对话区上的点击归它
                (define rp (or tq (and (not ctx-done?) (rvp 'open?) (not (in-sidebar? x)) (begin (review-action! (rvp 'hit x y)) #t))))   ; 审阅面板：对话区上的点击归它
                (define ip (or rp (and (not ctx-done?) (insp 'open?) (not (in-sidebar? x)) (begin (insp 'hit x y) #t))))
                (define ph (and (not ctx-done?) (not ip) (mpp 'hit x y)))                        ; 模型芯片 / 菜单优先（菜单打开时消化所有点击）
                (when ph (picker-action! ph))
                (define sh (and (not ctx-done?) (not ip) (not ph) (or (sbp 'context-open?) (in-sidebar? x)) (sbp 'hit x y)))
                ;; 双击行 = 改名（第一下已经打开了它）
                ;; 点行：松手时才打开（按下后拖动 = 拖行，不切会话）；双击 = 改名
                (cond [(and sh (eq? (car sh) 'resume) (>= (ev:mouse-clicks ev) 2)) (sbp 'start-rename! (cadr sh)) (set! drag-cand #f)]
                      [(and sh (eq? (car sh) 'resume)) (set! drag-cand (vector (cadr sh) x y))]
                      [sh (set! drag-cand #f) (session-action! sh)]
                      [else (set! drag-cand #f)])
                ;; 点行不拿键盘焦点（否则接着打字会被当成单键操作）；键盘模式里点行 = 焦点移到那行；点侧栏外 = 退出键盘模式
                (cond [(and sidebar-focus? sh (eq? (car sh) 'resume)) (sbp 'set-focus! (cadr sh))]
                      [(not (in-sidebar? x)) (set-sidebar-focus! #f)])
                (define th (and (not ctx-done?) (not ip) (not ph) (not sh) (not (in-sidebar? x)) (tr 'hit x y)))
                (define ih (and (not ctx-done?) (not ip) (not ph) (not sh) (not th) (ib 'hit x y)))
                (set! drag-select? #f)
                (set! drag-transcript? #f)
                (set! drag-minimap? #f)
                (tr 'clear-selection!)
                (cond
                  [(and th (eq? (car th) 'perm)) (define req (tr 'answer! (cadr th) (caddr th))) (when req (send! req))
                   ;; 合计上限的「今天不再询问」：对所有主机生效（GUI 记下今天，推给各台的合计上限变成不设）
                   (define info (tr 'permission-info (cadr th)))
                   (when (and req info (equal? (car info) "budget") (hash? (caddr info)) (equal? (hash-ref (caddr info) 'scope #f) "combined")
                              (equal? (caddr th) "always"))
                     (save-setting! 'combined-snooze-date (let ([b (hs-budget (A))]) (and (hash? b) (hash-ref b 'date #f))))
                     (push-external!) (push-budget-all!))]
                  [(and th (eq? (car th) 'tool)) (tr 'toggle-tool! (cadr th))]
                  [(and th (eq? (car th) 'copy))
                   (clip 'set! (cadr th))
                   (set! toast (cons (t 'app.copied (string-length (cadr th))) (+ (current-inexact-milliseconds) 1500.0)))]
                  [(and th (eq? (car th) 'code-toggle)) (tr 'toggle-code! (cadr th))]
                  [(and th (eq? (car th) 'to-bottom)) (tr 'scroll-to-bottom!)]
                  [(and th (eq? (car th) 'minimap)) (tr 'minimap-jump! y) (set! drag-minimap? #t)]
                  [(and th (eq? (car th) 'media-play)) (tr 'toggle-play! (cadr th))]
                  [(and ih (eq? (car ih) 'button))
                   (case (cadr ih)
                     [(unfold) (editor-unfold! e (caddr ih))]
                     [(confirm) (editor-confirm-secret! e (caddr ih))]                 ; 待选密钥 → 作为密钥
                     [else (editor-remove-chip! e (caddr ih))])]
                  [(and ih (eq? (car ih) 'chip))
                   (if (>= (ev:mouse-clicks ev) 2) (editor-unfold! e (cadr ih)) (editor-toggle-expand! e (cadr ih)))]
                  [(and ih (eq? (car ih) 'text))
                   (unless (editor-marked e)
                     (editor-move-to! e (cadr ih) #:extend? (and (memq 'shift (ev:mouse-mods ev)) #t))
                     (set! drag-select? #t))]
                  [(and (not ctx-done?) (not ip) (not ph) (not sh) (not th) (not ih) (not (in-sidebar? x)))
                   (tr 'select-start! x y) (set! drag-transcript? #t)]                          ; 对话区文字拖选
                  [else (void)])
                (set! last-edit (current-inexact-milliseconds))
                #t]
               [(drag)
                (cond [(sbp 'dragging?) (sbp 'drag-move! x y) #t]
                      [(and drag-cand (> (+ (abs (- x (vector-ref drag-cand 1))) (abs (- y (vector-ref drag-cand 2)))) 6.0))
                       (sbp 'drag-start! (vector-ref drag-cand 0) x y) (set! drag-cand #f) (sbp 'drag-move! x y) #t]
                      [drag-minimap? (tr 'minimap-jump! y) #t]
                      [drag-transcript? (tr 'select-extend! x y) #t]
                      [drag-select?
                       (define hitr (ib 'hit x y))
                       (when (and hitr (eq? (car hitr) 'text)) (editor-move-to! e (cadr hitr) #:extend? #t))
                       #t]
                      [else d])]
               [(up)
                (define cand drag-cand)
                (set! drag-cand #f)
                (cond [(sbp 'dragging?) (let ([a (sbp 'drag-end!)]) (when a (drop-action! (cadr a) (caddr a)))) #t]
                      [cand (session-action! (list 'resume (vector-ref cand 0))) #t]          ; 没拖：点了这一行 → 打开
                      [else d])]
               [(move) (set! mouse-x x)
                       (or (and (mpp 'open?) (mpp 'hover! x y))
                           (and (in-sidebar? x) (sbp 'hover! x y))
                           d)]
               [else d])]))]
            [else d])))))
      (when (and resize-at (>= (current-inexact-milliseconds) resize-at))
        (set! resize-at #f)
        (win 'set-content-size! (car resize-to) (cdr resize-to)))
      (define blink* (caret-on?))
      (define notice-expired? (and notice-until (>= (current-inexact-milliseconds) notice-until)))   ; 侧栏提示条 10 s 后收起
      (when notice-expired? (set! notice-until #f) (sbp 'set-notice! #f))
      (define toast-expired? (and toast (>= (current-inexact-milliseconds) (cdr toast))))
      (when toast-expired? (set! toast #f))
      ;; 动画：侧栏有在起伏的临时会话行 → 约 25 帧 / 秒重绘（只在这时；回放不需要）
      (define anim-due? (and (not replay-path)
                             (let-values ([(_sbx sbw _mx _mw _dx) (columns)]) (> sbw 0.0))   ; 侧栏没显示：它上一帧的状态不算
                             (sbp 'animating?) (>= (- (current-inexact-milliseconds) last-anim-ms) 40.0)))
      (when anim-due? (set! last-anim-ms (current-inexact-milliseconds)))
      (when (and (not replay-path) (> (- (current-inexact-milliseconds) last-mem-tick) 300000.0))   ; 自动记忆：每 5 分钟看一次空闲够久的会话
        (set! last-mem-tick (current-inexact-milliseconds))
        (guarded 'memory-auto mem-auto-pump!))
      ;; 冒烟脚本跑着：至少每 50 ms 画一帧——按帧数等的步骤不能靠光标闪烁（窗口失焦——有人在用这台 Mac、点了别的窗口——光标不闪，帧就停了；2026-10-25 实测）
      (define synth-due? (and (pair? synth-steps) (not replay-path) (>= (- (current-inexact-milliseconds) last-synth-ms) 50.0)))
      (when synth-due? (set! last-synth-ms (current-inexact-milliseconds)))
      (when (or dirty* toast-expired? notice-expired? anim-due? synth-due? (and focused? (not (eq? blink blink*)))) (guarded 'render render!))
      ;; 冒烟脚本在重绘之后判断条件：命中区（hit-center）与下一次点击时用的是同一帧
      (let fire ()
        (when (and (pair? synth-steps) ((car (first synth-steps))))
          (guarded 'synth-step (cdr (first synth-steps)))
          (set! last-fire-iter loop-iter)
          (set! synth-steps (rest synth-steps))
          (fire)))
      (define timed-out? (and exit-after (>= (- (current-inexact-milliseconds) t0) exit-after)))
      (cond
        [(or quit? timed-out?)
         (win 'close!)
         (mdp 'close!)
         (define closes (if ks (ks 'close-all!) '()))
         (define code (let ([r (assoc active closes)]) (and r (cdr r))))
         (printf "G3 stats: quit=~a frames=~a events=~a objc-msgs/frame=~a resizes=~a size=~ax~a scale=~a keys=~a atlas-glyphs=~a\n"
                 (if quit? 'close 'timeout) frames
                 (sort (hash->list event-kinds) string<? #:key (lambda (p) (format "~a" (car p))))
                 (remove-duplicates msgs-per-frame) (reverse resizes) w h scale (reverse keys) (tl 'glyph-count))
         (printf "G3 input: submitted=~a submitted-chars=~a synth-check=~a paste-path=~a ime-query=~a ime-log=~a\n"
                 (length submitted) (map string-length submitted) synth-check paste-path ime-query (reverse ime-log))
         (printf "G3 core: ready=~a turns=~a transcript=~a core-exit=~a last-assistant=~s\n"
                 (and ready #t) (tr 'turns) (tr 'summary) code (tr 'last-assistant-text))
         (when synth-sessions?
           (printf "G5 sessions: ~s\n" (reverse sessions-log)))
         (when synth-picker?
           (printf "G5 picker: ~s\n" (reverse sessions-log)))
         (when synth-route?
           (printf "G6 route: ~s\n" (reverse sessions-log)))
         (when synth-settings?
           (printf "G6 settings: ~s\n" (reverse sessions-log)))
         (when synth-secret?
           (printf "G6 secret: ~s\n" (reverse sessions-log)))
         (when synth-reattach?
           (printf "G7 reattach: ~s\n" (reverse sessions-log)))
         (when synth-settings2?
           (printf "G7 settings2: ~s\n" (reverse sessions-log)))
         (when synth-multihost?
           (printf "G7 multihost: ~s\n" (reverse sessions-log)))
         (when synth-hosts?
           (printf "G7 hosts: ~s\n" (reverse sessions-log)))
         (when synth-reconnect?
           (printf "G7 reconnect: ~s\n" (reverse sessions-log)))
         (when synth-spend?
           (printf "G7 spend: ~s\n" (reverse sessions-log)))
         (when synth-small?
           (printf "G7 small: ~s\n" (reverse sessions-log)))
         (when synth-media?
           (printf "G8 media: ~s\n" (reverse sessions-log)))
         (when synth-layout?
           (printf "G9 layout: ~s\n" (reverse sessions-log)))
         (when synth-sessmenu?
           (printf "G9 sessmenu: ~s\n" (reverse sessions-log)))
         (when synth-chat?
           (printf "G9 chat: ~s\n" (reverse sessions-log)))
         (when synth-rightclick?
           (printf "G11 rightclick: ~s\n" (reverse sessions-log)))
         (when synth-organize?
           (printf "G12 organize: ~s\n" (reverse sessions-log)))
         (when synth-unicode?
           (printf "G13 unicode: ~s\n" (reverse sessions-log)))
         (when synth-instructions?
           (printf "G14 instructions: ~s\n" (reverse sessions-log)))
         (when synth-memory?
           (printf "G15 memory: ~s\n" (reverse sessions-log)))
         (when synth-hostindex?
           (printf "G16 hostindex: ~s\n" (reverse sessions-log)))
         (when synth-automem?
           (printf "G17 automem: ~s\n" (reverse sessions-log)))
         (when synth-tests?
           (printf "G18 tests: ~s\n" (reverse sessions-log)))
         (when synth-history?
           (printf "G19 history: ~s\n" (reverse sessions-log)))
         (when lint-active? (print-lint-summary!))
         (when kernel-history
           (printf "G3 kernel-history-tail=~s\n"
                   (for/list ([m (in-list (take-right kernel-history (min 3 (length kernel-history))))])
                     (list (hash-ref m 'role) (let ([txt (hash-ref m 'text "")]) (substring txt 0 (min 40 (string-length txt))))))))
         (when (and ks (not (eqv? code 0)))
           (printf "core stderr tail:\n~a\n" (string-join (ks 'stderr-tail active 20) "\n")))
         (when rs
           ;; 回放比对：回放中界面发出的请求与录制时是否一致（不一致 = 界面状态在某处走岔了）
           (define sent (rs 'sent))
           (define expected (rs 'expected))
           (define diverge (for/first ([a (in-list sent)] [b (in-list expected)] [i (in-naturals)] #:unless (equal? a b)) i))
           (when snap-path (render!) (snapshot-now! snap-path))
           (printf "G6 replay: progress=~a errors=~s sends=~a/~a match=~a~a\n"
                   (rs 'progress) (map (lambda (e) (hash-ref e 'message)) (reverse (jr 'errors)))
                   (length sent) (length expected) (and (not diverge) (= (length sent) (length expected)))
                   (if diverge (format " first-divergence=~a got=~s want=~s" diverge (list-ref sent diverge) (list-ref expected diverge)) "")))
         (printf "G6 journal: path=~a errors=~a\n" (jr 'path) (length (jr 'errors)))
         (jr 'close!)]
        [else (unless rs (sleep 0.004)) (loop (and dirty* #f) blink*)])))
  ;; 守卫之外的异常（主循环自身）：先落盘再退出，日志路径打到 stderr
  (define (run-loop/fatal)
    (with-handlers ([exn:fail? (lambda (e)
                                 (report-error! 'fatal e)
                                 (jr 'close!)
                                 (eprintf "pi2 GUI: fatal error, journal: ~a\n" (jr 'path))
                                 (raise e))])
      (run-loop)))
  (cond
    [(or synth? synth-secret? synth-small? synth-sessmenu?) (clip 'call-with-private run-loop/fatal)]          ; 私有剪贴板：不碰系统剪贴板
    [ime-real (call-with-input-source ime-real run-loop/fatal)
              (printf "input source restore requested (applied asynchronously)\n")]
    [else (run-loop/fatal)])
) ; end define run-window

(define (parse-size s)
  (define p (map string->number (string-split s "x")))
  (cons (exact->inexact (first p)) (exact->inexact (second p)))
) ; end define parse-size

(module+ main
  (define selftest? #f)
  (define png #f)
  (define snapshot-path #f)
  (define size (cons 900.0 720.0))
  (define snapshot-view 'top)
  (define exit-after #f)
  (define resize-to #f)
  (define synth? #f)
  (define synth-core? #f)
  (define synth-sessions? #f)
  (define synth-picker? #f)
  (define synth-route? #f)
  (define synth-settings? #f)
  (define synth-secret? #f)
  (define synth-reattach? #f)
  (define synth-settings2? #f)
  (define synth-multihost? #f)
  (define synth-hosts? #f)
  (define synth-reconnect? #f)
  (define synth-spend? #f)
  (define synth-small? #f)
  (define synth-media? #f)
  (define synth-layout? #f)
  (define synth-sessmenu? #f)
  (define synth-chat? #f)
  (define synth-visual? #f)
  (define synth-rightclick? #f)
  (define synth-organize? #f)
  (define synth-unicode? #f)
  (define synth-instructions? #f)
  (define synth-memory? #f)
  (define synth-hostindex? #f)
  (define synth-automem? #f)
  (define synth-tests? #f)
  (define synth-history? #f)
  (define remote-host #f)
  (define remote-cmd #f)
  (define remote-name #f)
  (define in-process? #f)
  (define no-local? #f)
  (define no-saved-hosts? #f)
  (define host-cmds '())                               ; ((编号 . 命令) …)
  (define ime-real #f)
  (define core-module #f)
  (define core-cwd #f)
  (define live-prompt #f)
  (define allow-rx #f)
  (define no-core? #f)
  (define snap-at-exit #f)
  (define replay-path #f)
  (define journal-dir 'default)
  (define lang-flag #f)
  (define core-args
    (command-line
     #:once-each
     [("--selftest") "offscreen render + pixel readback, no window" (set! selftest? #t)]
     [("--png") path "selftest: also write the text render to <path>" (set! png path)]
     [("--snapshot") path "render a typical scene offscreen to <path> (PNG, scale 2)" (set! snapshot-path path)]
     [("--size") wxh "snapshot size in points (default 900x720)" (set! size (parse-size wxh))]
     [("--snapshot-view") v "snapshot: top | bottom | menu | settings | pools | media | sidebar-right | info (default top)" (set! snapshot-view (string->symbol v))]
     [("--core") module "use <module>.rkt's main submodule as the core instead of pi2 main.rkt" (set! core-module module)]
     [("--core-cwd") dir "working directory for the core" (set! core-cwd dir)]
     [("--no-core") "do not start a core" (set! no-core? #t)]
     [("--exit-after") ms "close the window after <ms> milliseconds" (set! exit-after (string->number ms))]
     [("--synth") "smoke: input-box flow without a core" (set! synth? #t) (set! no-core? #t)]
     [("--synth-core") "smoke: two turns through the core (permission, tool, interrupt)" (set! synth-core? #t)]
     [("--synth-sessions") "smoke: session sidebar (new, resume, fork, two-click delete) through the core" (set! synth-sessions? #t)]
     [("--synth-picker") "smoke: model menu (reasoning, provider switch and back, Esc closes) through the core" (set! synth-picker? #t)]
     [("--synth-route") "smoke: routing pool (select @pool, failover on 429, member pin) through the scripted core" (set! synth-route? #t)]
     [("--synth-settings") "smoke: settings page (add account + key, permission mode, Esc) through the scripted core" (set! synth-settings? #t)]
     [("--synth-small") "smoke: mark selection as secret (journal redacted), custom secret format, multi-line rule text (scripted core)" (set! synth-small? #t)]
     [("--synth-media") "smoke: attach an image and an audio file, send, thumbnail decoded, kernel keeps media references (scripted core)" (set! synth-media? #t)]
     [("--synth-layout") "smoke: drag the sidebar divider, move the sidebar to the right, new session there (scripted core)" (set! synth-layout? #t)]
     [("--synth-sessmenu") "smoke: session context menu: info panel, rename, export, copy path (scripted core, private pasteboard)" (set! synth-sessmenu? #t)]
     [("--synth-chat") "smoke: chat session (anonymous workspace), its folder in the info panel, chat directory settings and clean-up (scripted core)" (set! synth-chat? #t)]
     [("--synth-visual") "check: contrast / brightness of the composited window for each sidebar material and backdrop (scripted core)" (set! synth-visual? #t)]
     [("--synth-rightclick") "smoke: sidebar context menu through real AppKit mouse events (right click, hover, click items) on glass and opaque sidebars" (set! synth-rightclick? #t)]
     [("--synth-organize") "smoke: the agent proposes a changeset; review panel, sidebar preview, apply selected, undo (scripted core)" (set! synth-organize? #t)]
     [("--synth-unicode") "check: Unicode display policy markers and ANSI colors in the transcript, with contrast checks (scripted core)" (set! synth-unicode? #t)]
     [("--synth-tests") "smoke: workspace test flow panel — open (⌘⇧T), trust, run all, failing output, the agent proposes a change, approve (scripted core; PI2_SEED_TESTFLOW=1)" (set! synth-tests? #t)]
   [("--synth-history") "smoke: git_history tool card — datum commits drawn as rows with TYPE badges and label chips, collapsed and expanded (scripted core; PI2_SEED_HISTORY=1)" (set! synth-history? #t)]
     [("--synth-automem") "smoke: automatic memory — off does nothing; turned on in Settings: extract + merge with the (fake) local model, global ones pending, no reprocessing, a new session gets them (scripted core; PI2_MEM_IDLE_SECONDS=0)" (set! synth-automem? #t)]
     [("--synth-hostindex") "smoke: project index — scan, local-model descriptions, host_projects (Chinese query), session switch, edit / regenerate in Settings (scripted core; PI2_SEED_REPOS=1)" (set! synth-hostindex? #t)]
     [("--synth-memory") "smoke: memory — /remember (workspace, global), the model's memory_note to global waits for Keep in Settings, write switch, a new session gets them, info panel (scripted core)" (set! synth-memory? #t)]
     [("--synth-instructions") "smoke: layered instructions — add a workspace's instructions in Settings, the model receives global → workspace → directory, the info panel lists them (scripted core; PI2_SEED_INSTRUCTIONS=1)" (set! synth-instructions? #t)]
     [("--synth-spend") "smoke: cross-host spend total and combined cap (local --core + a --host daemon, both seeded with --seed-spend)" (set! synth-spend? #t)]
     [("--synth-reconnect") "smoke: per-host drafts; auto-reconnect after the --host daemon stops; manual disconnect does not reconnect" (set! synth-reconnect? #t)]
     [("--synth-hosts") "smoke: settings hosts section (disconnect / connect a --host daemon, add an unreachable ssh host, remove it)" (set! synth-hosts? #t)]
     [("--synth-multihost") "smoke: two hosts in one window (local core + a --host daemon): concurrent turns, background permission badge, switching" (set! synth-multihost? #t)]
     [("--synth-settings2") "smoke: settings page part 2 (pool members, alias, rule create/delete, zoom) through the scripted core" (set! synth-settings2? #t)]
     [("--synth-reattach") "smoke: attach to a running pi2 serve, check the replayed conversation, send one more turn" (set! synth-reattach? #t)]
     [("--remote") host "attach to the pi2 kernel kept running on <host> by src/serve.rkt (over ssh -T)" (set! remote-host host)]
     [("--remote-cmd") cmd "transport command for the remote kernel (with --remote: run on the host; alone: run locally)" (set! remote-cmd cmd)]
     [("--in-process") "run the kernel in a place inside this process (stops with the window) instead of the local pi2 serve daemon" (set! in-process? #t)]
     [("--no-saved-hosts") "do not connect the servers in the hosts table ({config-home}/hosts.rktd)" (set! no-saved-hosts? #t)]
     [("--no-local") "do not connect the local kernel (only --remote / --host hosts)" (set! no-local? #t)]
     [("--remote-name") n "name of the pi2 serve instance to attach to (default: default)" (set! remote-name n)]
     [("--synth-secret") "smoke: paste a key into the chat (secret chip, confirm, send) through the scripted core" (set! synth-secret? #t)]
     [("--synth-live") prompt "live acceptance: send <prompt>, answer permissions, quit when the turn ends" (set! live-prompt prompt)]
     [("--allow-rx") rx "live: only allow bash permission requests whose command matches <rx>" (set! allow-rx (pregexp rx))]
     [("--ime-real") source-id "switch to input source <id> temporarily and type through it" (set! ime-real source-id) (set! no-core? #t)]
     [("--snapshot-at-exit") path "smoke / replay: write the last frame to <path> before quitting" (set! snap-at-exit path)]
     [("--replay") path "replay an operation journal headless (no window, no core); compare requests; report errors" (set! replay-path path)]
     [("--journal-dir") dir "write the operation journal into <dir> (default ~/Library/Logs/pi2; replays: memory only)" (set! journal-dir dir)]
     [("--no-journal") "keep the operation journal in memory only" (set! journal-dir #f)]
     [("--lang") lang "UI language: zh | en (default: PI2_LANG, else the system's preferred languages, else zh)" (set! lang-flag lang)]
     [("--resize-to") wxh "300ms after start, set content size to <w>x<h> points" (set! resize-to (parse-size wxh))]
     #:multi
     [("--host") spec "add a host: <id>=<transport command> (e.g. srv=ssh -T srv racket -l pi2/src/serve -- attach --spawn); repeatable"
                 (let ([m (regexp-match #rx"^([A-Za-z0-9_.@-]+)=(.+)$" spec)])
                   (if m (set! host-cmds (cons (cons (cadr m) (caddr m)) host-cmds)) (begin (eprintf "bad --host ~s (want id=command)\n" spec) (exit 64))))]
     #:args core-args core-args))
  ;; 界面语言：--lang > PI2_LANG > 设置页存的（gui.rktd）> 系统首选语言里第一个支持的 > zh
  (current-lang (or (lang-from-tag lang-flag) (lang-from-tag (getenv "PI2_LANG"))
                    (resolve-lang (hash-ref (load-gui-settings) 'lang 'system))))   ; 设置页存的 > 系统首选语言 > zh
  (cond
    [selftest?
     (define failures (selftest png))
     (cond [(null? failures) (printf "SELFTEST ok\n")]
           [else (printf "SELFTEST FAIL: ~a\n" failures) (exit 1)])]
    [snapshot-path (snapshot snapshot-path (car size) (cdr size) #:view snapshot-view)]
    [else
     ;; 主机表：本机（守护进程 / --core 脚本化内核 / --in-process）+ --remote / --remote-cmd + --host 编号=命令（可多个）
     (define local-config
       (cond [no-core? #f]
             [core-module (hasheq 'entry (path->complete-path core-module) 'args core-args 'cwd core-cwd)]
             [in-process? (hasheq 'args (cons "--rpc" core-args) 'cwd core-cwd)]
             [else
              ;; 本机守护进程：按工作目录一个；没在跑就起（空闲 30 分钟退出）；内核参数只在新起时生效
              (define dir (or core-cwd (current-directory)))
              (hasheq 'command (append (list (racket-exe-path) (path->string SERVE-RKT) "attach" "--name" (local-serve-name dir)
                                             "--spawn" "--idle-exit" "1800" "--")
                                       core-args)
                      'cwd dir)]))
     (define remote-spec
       (cond [remote-host (ssh-host-spec remote-host (or remote-name "default"))]   ; 经 ssh 在主机上跑 serve.rkt attach --spawn
             [remote-cmd (list "remote" "remote" (hasheq 'command (string-split (string-append remote-cmd (if remote-name (string-append " --name " remote-name) "")))))]
             [else #f]))
     ;; 主机表里的服务器（{config-home}/hosts.rktd，设置页加的）：启动时一起连（只连有 ssh 目标的）
     (define saved-specs
       (if (or no-saved-hosts? replay-path) '()
           (for/list ([x (in-list (load-hosts-file))] #:when (string? (hash-ref x 'ssh #f)))
             (ssh-host-spec (hash-ref x 'ssh) (hash-ref x 'serve "default")))))
     (define host-specs
       (append (if (and local-config (not no-local?)) (list (list "local" (t 'app.host-local) local-config)) '())
               (for/list ([hc (in-list (reverse host-cmds))])
                 (list (car hc) (car hc) (hasheq 'command (string-split (cdr hc)))))
               (filter (lambda (sp) (not (and remote-spec (equal? (car sp) (car remote-spec))))) saved-specs)
               (if remote-spec (list remote-spec) '())))
     (define jdir
       (cond [(not journal-dir) #f]
             [(string? journal-dir) journal-dir]
             [replay-path #f]
             [(getenv "PI2_JOURNAL_DIR") => values]
             [else (build-path (find-system-path 'home-dir) "Library" "Logs" "pi2")]))
     (run-window #:hosts (if replay-path '() host-specs) #:active (and remote-spec (car remote-spec)) #:journal-dir jdir #:replay replay-path #:exit-after exit-after #:resize-to resize-to
                 #:synth? synth? #:synth-core? synth-core? #:synth-sessions? synth-sessions? #:synth-picker? synth-picker? #:synth-route? synth-route? #:synth-settings? synth-settings? #:synth-secret? synth-secret? #:synth-reattach? synth-reattach? #:synth-settings2? synth-settings2? #:synth-multihost? synth-multihost? #:synth-hosts? synth-hosts? #:synth-reconnect? synth-reconnect? #:synth-spend? synth-spend? #:synth-small? synth-small? #:synth-media? synth-media? #:synth-layout? synth-layout? #:synth-sessmenu? synth-sessmenu? #:synth-chat? synth-chat? #:synth-visual? synth-visual? #:synth-rightclick? synth-rightclick? #:synth-organize? synth-organize? #:synth-unicode? synth-unicode? #:synth-instructions? synth-instructions? #:synth-memory? synth-memory? #:synth-hostindex? synth-hostindex? #:synth-automem? synth-automem? #:synth-tests? synth-tests? #:synth-history? synth-history? #:synth-live live-prompt #:allow-rx allow-rx
                 #:ime-real ime-real #:snapshot-at-exit snap-at-exit)])
) ; end module+ main
