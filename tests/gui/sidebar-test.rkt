#lang racket/base
;; tests/gui/sidebar-test.rkt — 会话侧栏组件（单个组件 + 假 text-layout）

(require rackunit racket/list
         "../../src/gui/compute/drawlist.rkt" "../../src/gui/compute/interfaces.rkt"
         "../../src/gui/components/sidebar.rkt" "../../src/gui/compute/editor.rkt" "fakes.rkt"
         (only-in "../../src/gui/compute/paint.rkt" current-text-sink) (only-in "../../src/gui/compute/i18n.rkt" t))

(define (new-sb) (instantiate-with sidebar-component session-list))
(define SESSIONS
  (list (hasheq 'path "/d/a.rktd" 'title "修复 loader 缺列" 'ts "2026-10-22T17:41:00" 'messages 12 'model "deepseek-v4-flash" 'current #t)
        (hasheq 'path "/d/b.rktd" 'title 'null 'ts "2026-10-21T09:00:00" 'messages 0 'model "gemma" 'current #f)
        (hasheq 'path "/d/c.rktd" 'title "写周报" 'ts "2026-10-20T08:30:00" 'messages 4 'model "gemma" 'current #f)))

(define (render s) (define dl (make-drawlist)) (s 'render! dl 0.0 0.0 240.0 500.0 2.0) dl)

(test-case "header buttons and rows hit-test to actions"
  (define s (new-sb))
  (s 'set-sessions! SESSIONS)
  (render s)
  (check-equal? (s 'hit 20.0 20.0) '(new))
  (check-equal? (s 'hit 20.0 (+ 40.0 46.0 10.0)) '(resume "/d/b.rktd"))          ; 第二行
  (check-false (s 'hit 20.0 499.0)))                                              ; 空白处

(test-case "hovered row shows pin / archive icons (no delete on the row); delete is one click in the menu (undoable, no confirm step)"
  (define s (new-sb))
  (s 'set-sessions! SESSIONS)
  (render s)
  (s 'hover! 100.0 (+ 40.0 46.0 10.0))                                           ; 悬停第二行
  (render s)
  (check-false (s 'hit-center 'delete "/d/b.rktd"))                               ; 行上没有删除
  (define pin (s 'hit-center 'row-pin "/d/b.rktd"))
  (define arc (s 'hit-center 'row-archive "/d/b.rktd"))
  (check-equal? (s 'hit (car pin) (cdr pin)) '(pin "/d/b.rktd"))                  ; 行上的图标：一次点击
  (check-equal? (s 'hit (car arc) (cdr arc)) '(archive "/d/b.rktd"))
  (check-equal? (s 'hit 60.0 (+ 40.0 46.0 23.0)) '(resume "/d/b.rktd"))           ; 行的其余部分：切换
  (check-true (s 'context! 100.0 (+ 40.0 46.0 10.0)))                            ; 右键第二行
  (render s)
  (check-false (s 'hit-center 'row-pin "/d/b.rktd"))                              ; 菜单开着：行上不出图标
  (define d (s 'hit-center 'delete "/d/b.rktd"))
  (check-equal? (s 'hit (car d) (cdr d)) '(delete "/d/b.rktd"))                   ; 一次就删
  (check-false (s 'context-open?)))

(test-case "context menu: current session cannot be deleted; outside click and close-context! dismiss without action; icon row on top"
  (define s (new-sb))
  (s 'set-sessions! SESSIONS)
  (render s)
  (check-true (s 'context! 100.0 (+ 40.0 10.0)))                                  ; 当前会话
  (render s)
  (check-false (s 'hit-center 'delete "/d/a.rktd"))                               ; 只有「当前会话不能删除」（灰）
  (define cells (for/list ([k '(pin archive rename info)]) (s 'hit-center k "/d/a.rktd")))
  (check-true (andmap pair? cells))                                               ; 第一行四个图标格
  (check-equal? (length (remove-duplicates (map cdr cells))) 1)                   ; 同一行
  (check-true (< (cdr (car cells)) (cdr (s 'hit-center 'copy-path "/d/a.rktd"))) "icons above text items")
  (check-equal? (s 'hit 20.0 20.0) '(close))                                      ; 菜单外（头部按钮处）：只关菜单，不新建
  (check-false (s 'context-open?))
  (render s)
  (check-equal? (s 'hit 20.0 20.0) '(new))
  (check-false (s 'context! 100.0 499.0))                                         ; 空白处：不开
  (s 'context! 100.0 (+ 40.0 92.0 10.0))
  (render s)
  (s 'close-context!)                                                              ; Esc
  (check-false (s 'context-open?)))

(test-case "the context menu closes when its session disappears from the list"
  (define s (new-sb))
  (s 'set-sessions! SESSIONS)
  (render s)
  (s 'context! 100.0 (+ 40.0 46.0 10.0))
  (s 'set-sessions! (list (car SESSIONS)))
  (check-false (s 'context-open?)))

(test-case "long lists scroll inside the list area"
  (define s (new-sb))
  (s 'set-sessions! (for/list ([i 40]) (hasheq 'path (format "/d/~a" i) 'title (format "t~a" i) 'ts "2026-10-22T00:00:00" 'messages 1 'model "m" 'current #f)))
  (render s)
  (check-equal? (s 'hit 20.0 (+ 40.0 5.0)) '(resume "/d/0"))
  (s 'scroll! -460.0)                                                              ; 往下翻 10 行
  (render s)
  (check-equal? (s 'hit 20.0 (+ 40.0 5.0)) '(resume "/d/10")))

(test-case "hit-center finds header buttons and rows; rows scrolled under the header do not cover the buttons"
  (define s (new-sb))
  (s 'set-sessions! (for/list ([i 40]) (hasheq 'path (format "/d/~a" i) 'title (format "t~a" i) 'ts "2026-10-22T00:00:00" 'messages 1 'model "m" 'current #f)))
  (render s)
  (define c (s 'hit-center 'new #t))
  (check-equal? (s 'hit (car c) (cdr c)) '(new))
  (define r (s 'hit-center 'resume "/d/3"))
  (check-equal? (s 'hit (car r) (cdr r)) '(resume "/d/3"))
  (s 'scroll! -20.0)                                                               ; 第 0 行上半截滚到头部下面
  (render s)
  (check-equal? (s 'hit (car c) (cdr c)) '(new))
  (check-false (s 'hit-center 'resume "/d/39")))                                   ; 不可见的行没有命中区

(test-case "multi-host: header rows (+ = new session on that host; disconnected = reconnect), rows keyed by 'key, badges"
  (define s (new-sb))
  (s 'set-sessions!
     (list (hasheq 'kind "header" 'key "\u0001local" 'host "local" 'label "This Mac" 'status "" 'level "dim" 'connected #t)
           (hasheq 'path "/d/a.rktd" 'key "local\u0001/d/a.rktd" 'title "a" 'ts "2026-10-22T17:41:00" 'messages 2 'model "m" 'current #t)
           (hasheq 'kind "header" 'key "\u0001srv" 'host "srv" 'label "srv" 'status "Needs permission (1)" 'level "warn" 'connected #t)
           (hasheq 'path "/d/a.rktd" 'key "srv\u0001/d/a.rktd" 'title "same path on srv" 'ts "2026-10-22T17:41:00" 'messages 4 'model "m"
                   'current #f 'badge "Needs permission (1)" 'badge_level "warn")
           (hasheq 'kind "header" 'key "\u0001box" 'host "box" 'label "box" 'status "Disconnected" 'level "warn" 'connected #f)))
  (render s)
  (define (click kind payload) (let ([c (s 'hit-center kind payload)]) (and c (s 'hit (car c) (cdr c)))))
  (check-equal? (click 'host-new "srv") '(host-new "srv"))
  (check-equal? (click 'resume "srv\u0001/d/a.rktd") '(resume "srv\u0001/d/a.rktd"))   ; 两台上同一路径：按键区分
  (check-equal? (click 'resume "local\u0001/d/a.rktd") '(resume "local\u0001/d/a.rktd"))
  (check-false (s 'hit-center 'host-new "box"))                                   ; 断开的主机：没有「＋」
  (check-equal? (click 'host-reconnect "box") '(host-reconnect "box"))
  (check-true (s 'context! (car (s 'hit-center 'resume "srv\u0001/d/a.rktd")) (cdr (s 'hit-center 'resume "srv\u0001/d/a.rktd")))))

(test-case "context menu items (W1): open / rename / info / fork / copy path / Finder / export / delete; disabled items have no hit"
  (define s (new-sb))
  (s 'set-sessions! (list (hash-set (car SESSIONS) 'local #t) (cadr SESSIONS) (caddr SESSIONS)))
  (render s)
  (s 'context! 100.0 (+ 40.0 92.0 10.0))                                          ; 第三行（不是当前、有消息、不是本机）
  (render s)
  (define (click kind k) (let ([c (s 'hit-center kind k)]) (and c (s 'hit (car c) (cdr c)))))
  (check-false (s 'hit-center 'fork "/d/c.rktd"))                                  ; 分叉只对当前会话
  (check-false (s 'hit-center 'reveal "/d/c.rktd"))                                ; 不是本机的会话
  (check-equal? (click 'info "/d/c.rktd") '(info "/d/c.rktd"))
  (check-false (s 'context-open?))
  (s 'context! 100.0 (+ 40.0 92.0 10.0)) (render s)
  (check-equal? (click 'export-md "/d/c.rktd") '(export "/d/c.rktd" "markdown"))
  (s 'context! 100.0 (+ 40.0 92.0 10.0)) (render s)
  (check-equal? (click 'copy-path "/d/c.rktd") '(copy-path "/d/c.rktd"))
  (s 'context! 100.0 (+ 40.0 92.0 10.0)) (render s)
  (check-equal? (click 'open "/d/c.rktd") '(resume "/d/c.rktd"))
  ;; 当前会话（本机、有消息）：分叉、Finder 可用；「打开」不可用
  (s 'context! 100.0 (+ 40.0 10.0)) (render s)
  (check-false (s 'hit-center 'open "/d/a.rktd"))
  (check-equal? (click 'reveal "/d/a.rktd") '(reveal "/d/a.rktd"))
  (s 'context! 100.0 (+ 40.0 10.0)) (render s)
  (check-equal? (click 'fork "/d/a.rktd") '(fork))
  ;; 空会话：导出不可用
  (s 'context! 100.0 (+ 40.0 46.0 10.0)) (render s)
  (check-false (s 'hit-center 'export-md "/d/b.rktd")))

(test-case "rename: the row becomes an input with the old title selected; Enter gives the new name; Esc / empty cancels"
  (define s (new-sb))
  (s 'set-sessions! SESSIONS)
  (render s)
  (s 'context! 100.0 (+ 40.0 92.0 10.0)) (render s)
  (define c (s 'hit-center 'rename "/d/c.rktd"))
  (check-equal? (s 'hit (car c) (cdr c)) '(rename-start "/d/c.rktd"))
  (check-true (s 'editing?))
  (render s)
  (check-true (vector? (s 'caret-rect)))
  (editor-insert! (s 'editor) "周报（第 43 周）")                                   ; 旧名全选 → 替换
  (check-equal? (s 'submit!) '(rename "/d/c.rktd" "周报（第 43 周）"))
  (check-false (s 'editing?))
  (s 'context! 100.0 (+ 40.0 92.0 10.0)) (render s)
  (let ([c (s 'hit-center 'rename "/d/c.rktd")]) (s 'hit (car c) (cdr c)))
  (editor-command! (s 'editor) 'select-all) (editor-command! (s 'editor) 'delete-backward)
  (check-false (s 'submit!))                                                       ; 空：不改
  (let ([c (s 'hit-center 'rename "/d/c.rktd")]) (void))
  (s 'context! 100.0 (+ 40.0 92.0 10.0)) (render s)
  (let ([c (s 'hit-center 'rename "/d/c.rktd")]) (s 'hit (car c) (cdr c)))
  (s 'cancel-edit!)
  (check-false (s 'editing?)))

(test-case "the temporary (ephemeral) row pulses slowly: animating? while it is drawn; its outline alpha follows the clock (period PULSE-MS)"
  (define s (new-sb))
  (define (outline-alpha t)
    (parameterize ([sidebar-clock (lambda () t)])
      (define dl (make-drawlist))
      (s 'render! dl 0.0 0.0 240.0 500.0 2.0)
      ;; 虚线框的短横：1 point 高、6 point 宽的矩形，颜色 (0.45 0.55 0.75)
      (for/first ([i (in-range (drawlist-count dl))]
                  #:when (let ([v (drawlist-ref dl i)]) (and (< (abs (- (list-ref v 4) 0.45)) 0.002) (< (abs (- (list-ref v 5) 0.55)) 0.002) (< (abs (- (list-ref v 3) 1.0)) 0.01))))
        (list-ref (drawlist-ref dl i) 7))))
  (s 'set-sessions! SESSIONS)
  (render s)
  (check-false (s 'animating?))                                                      ; 没有临时会话：不动
  (s 'set-sessions! (cons (hash-set* (car SESSIONS) 'ephemeral #t 'messages 0) (cdr SESSIONS)))
  (define a0 (outline-alpha 0.0))
  (define a-half (outline-alpha (/ PULSE-MS 2)))
  (define a-full (outline-alpha PULSE-MS))
  (check-true (s 'animating?))
  (check-true (< a0 0.35))                                                           ; 最暗
  (check-true (> a-half 0.9))                                                        ; 最亮
  (check-true (< (abs (- a0 a-full)) 0.01)))                                         ; 一个周期回到原处

(test-case "W2: pin / archive menu items follow the row's flags (temporary rows cannot be kept); 📌 rows; archived group row toggles"
  (define s (new-sb))
  (s 'set-sessions! (list (hash-set (car SESSIONS) 'pinned #t)
                          (hash-set (cadr SESSIONS) 'ephemeral #t)
                          (hasheq 'kind "group" 'key "\u0002archived\u0001" 'label "Archived (1)" 'count 1 'open #t)
                          (hash-set (caddr SESSIONS) 'archived #t)))
  (render s)
  (define (click kind k) (let ([c (s 'hit-center kind k)]) (and c (s 'hit (car c) (cdr c)))))
  (s 'context! 100.0 (+ 40.0 10.0)) (render s)                                    ; 置顶的当前会话
  (check-false (s 'hit-center 'pin "/d/a.rktd"))
  (check-equal? (click 'unpin "/d/a.rktd") '(unpin "/d/a.rktd"))
  (s 'context! 100.0 (+ 40.0 10.0)) (render s)
  (check-equal? (click 'archive "/d/a.rktd") '(archive "/d/a.rktd"))
  (s 'context! 100.0 (+ 40.0 46.0 10.0)) (render s)                              ; 临时会话：置顶 / 归档不可用
  (check-false (s 'hit-center 'pin "/d/b.rktd"))
  (check-false (s 'hit-center 'archive "/d/b.rktd"))
  (s 'close-context!) (render s)
  (check-equal? (s 'hit 20.0 (+ 40.0 92.0 10.0)) '(toggle-group "\u0002archived\u0001"))   ; 分组行（30 高）
  (s 'context! 100.0 (+ 40.0 92.0 30.0 10.0)) (render s)                         ; 归档的行
  (check-equal? (click 'unarchive "/d/c.rktd") '(unarchive "/d/c.rktd")))

(test-case "§11 A: notice bar with Undo; keyboard focus moves over session rows only (skips headers / groups) and scrolls into view; start-rename!"
  (define s (new-sb))
  (define many (for/list ([i 30]) (hasheq 'path (format "/d/~a.rktd" i) 'title (format "s~a" i) 'messages 1 'model "m" 'ts "2026-10-25T10:00:00" 'current (= i 0))))
  (s 'set-sessions! (append (list (hasheq 'kind "header" 'key "\u0001local" 'host "local" 'label "local" 'connected #t))
                            (take many 3)
                            (list (hasheq 'kind "group" 'key "\u0002archived\u0001" 'label "Archived (1)" 'count 1 'open #t))
                            (drop many 3)))
  (render s)
  ;; 提示条：「撤销」可点；列表区让出提示条的高度
  (s 'set-notice! "Deleted “x”")
  (render s)
  (define u (s 'hit-center 'undo #t))
  (check-true (> (cdr u) 440.0))                                                   ; 在底部
  (check-equal? (s 'hit (car u) (cdr u)) '(undo))
  (s 'set-notice! #f) (render s)
  (check-false (s 'hit-center 'undo #t))
  ;; 焦点：没有焦点 → 从当前会话开始；跳过分组行；到底不越界；滚到看得见
  (check-equal? (s 'move-focus! 1) "/d/1.rktd")
  (check-equal? (s 'move-focus! 2) "/d/3.rktd")                                    ; 2 → 3：中间的分组行不算
  (check-equal? (s 'move-focus! 100) "/d/29.rktd")
  (render s)
  (check-true (pair? (s 'hit-center 'resume "/d/29.rktd")))                       ; 滚到看得见了
  (check-equal? (s 'move-focus! -100) "/d/0.rktd")
  (render s)
  (check-equal? (s 'focus) "/d/0.rktd")
  ;; 直接进入改名（双击 / R 键）：旧标题全选
  (s 'start-rename! "/d/0.rktd")
  (check-true (s 'editing?))
  (editor-insert! (s 'editor) "new name")
  (check-equal? (s 'submit!) '(rename "/d/0.rktd" "new name")))

(test-case "§11 A: hover icons do not appear on the temporary row; menu pin / archive cells are disabled there"
  (define s (new-sb))
  (s 'set-sessions! (list (hash-set (car SESSIONS) 'ephemeral #t) (cadr SESSIONS)))
  (render s)
  (s 'hover! 100.0 (+ 40.0 10.0)) (render s)
  (check-false (s 'hit-center 'row-pin "/d/a.rktd"))
  (s 'context! 100.0 (+ 40.0 10.0)) (render s)
  (check-false (s 'hit-center 'pin "/d/a.rktd"))
  (check-false (s 'hit-center 'archive "/d/a.rktd"))
  (check-true (pair? (s 'hit-center 'rename "/d/a.rktd"))))

(test-case "W3: ⇅ opens the group-by menu (current mode checked); choosing a mode closes it; a click outside only closes it"
  (define s (new-sb))
  (s 'set-sessions! SESSIONS)
  (s 'set-group-mode! 'recent)
  (render s)
  (define gb (s 'hit-center 'group-button #t))
  (check-equal? (s 'hit (car gb) (cdr gb)) '(menu))
  (render s)
  (define p (s 'hit-center 'group-by 'project))
  (check-true (pair? p))
  (check-equal? (s 'hit (car p) (cdr p)) '(group-by project))
  (render s)
  (check-false (s 'hit-center 'group-by 'project))                                 ; 关了
  (s 'hit (car gb) (cdr gb)) (render s)
  (check-equal? (s 'hit 20.0 300.0) '(close))                                      ; 点外面：只关菜单
  (render s)
  (check-false (s 'hit-center 'group-by 'recent)))

(test-case "§11.4 D swipe: right ≥ 60 pin, left ≥ 60 archive, left ≥ 150 delete (not the current row), short = nothing; temporary rows do nothing"
  (define s (new-sb))
  (s 'set-sessions! (list (car SESSIONS) (cadr SESSIONS) (caddr SESSIONS) (hasheq 'path "/d/e.rktd" 'title "temp" 'messages 0 'ephemeral #t)))
  (render s)
  (define (swipe k dx) (s 'swipe! k 0.0 'began) (s 'swipe! #f (/ dx 2) 'changed) (render s) (s 'swipe! #f (/ dx 2) 'changed) (s 'swipe! #f 0.0 'ended))
  (check-equal? (swipe "/d/c.rktd" 80.0) '(pin "/d/c.rktd"))
  (check-false (s 'swiping?))
  (check-equal? (swipe "/d/c.rktd" -80.0) '(archive "/d/c.rktd"))
  (check-equal? (swipe "/d/c.rktd" -200.0) '(delete "/d/c.rktd"))
  (check-false (swipe "/d/c.rktd" 30.0))
  (check-false (swipe "/d/a.rktd" -200.0))                                         ; 当前会话不能删
  (check-false (swipe "/d/e.rktd" 80.0))
  (check-equal? (s 'row-at 20.0 (+ 40.0 46.0 10.0)) "/d/b.rktd"))

(test-case "§11.4 D drag: onto the pinned section / archived group → drop; onto a plain row or ungrouped → nothing"
  (define s (new-sb))
  (s 'set-sessions! (list (hasheq 'kind "group" 'key "\u0002sec\u0001\u0001pinned" 'label "Pinned (1)" 'count 1 'open #t 'section "pinned")
                          (hash-set (car SESSIONS) 'pinned #t)
                          (hasheq 'kind "group" 'key "\u0002grp\u0001\u0001-" 'label "Ungrouped (2)" 'count 2 'open #t)
                          (cadr SESSIONS) (caddr SESSIONS)
                          (hasheq 'kind "group" 'key "\u0002archived\u0001" 'label "Archived (0)" 'count 0 'open #f)))
  (render s)
  (define (drag-to k target)
    (define from (s 'hit-center 'resume k))
    (s 'drag-start! k (car from) (cdr from))
    (define c (s 'hit-center (if (regexp-match? #rx"^\u0002" target) 'toggle-group 'resume) target))
    (s 'drag-move! (car c) (cdr c)) (render s)
    (s 'drag-end!))
  (check-equal? (drag-to "/d/c.rktd" "\u0002sec\u0001\u0001pinned") (list 'drop "/d/c.rktd" "\u0002sec\u0001\u0001pinned"))
  (check-false (s 'dragging?))
  (check-equal? (drag-to "/d/c.rktd" "\u0002archived\u0001") (list 'drop "/d/c.rktd" "\u0002archived\u0001"))
  (check-false (drag-to "/d/c.rktd" "/d/b.rktd"))
  (check-false (drag-to "/d/c.rktd" "\u0002grp\u0001\u0001-")))

(test-case "context menu (M2): Read memory / Write memory — checked when on (default), unchecked when the row says off; click gives the toggle action"
  (define s (new-sb))
  (s 'set-sessions! (list (car SESSIONS) (cadr SESSIONS) (hash-set (caddr SESSIONS) 'mem_write #f)))
  (render s)
  (s 'context! 100.0 (+ 40.0 92.0 10.0)) (render s)
  (define acc '())
  (parameterize ([current-text-sink (lambda (str xs y h soft?) (set! acc (cons str acc)))]) (render s))
  (check-not-false (member (string-append "✓ " (t 'sb.m-mem-read)) acc))
  (check-not-false (member (string-append "   " (t 'sb.m-mem-write)) acc))
  (define (click kind k) (let ([c (s 'hit-center kind k)]) (and c (s 'hit (car c) (cdr c)))))
  (check-equal? (click 'mem-write "/d/c.rktd") '(mem-write "/d/c.rktd"))
  (check-false (s 'context-open?))
  (s 'context! 100.0 (+ 40.0 92.0 10.0)) (render s)
  (check-equal? (click 'mem-read "/d/c.rktd") '(mem-read "/d/c.rktd")))

(test-case "context menu (§8): Project index — label follows the row (default / on / off); click gives the action"
  (define s (new-sb))
  (s 'set-sessions! (list (car SESSIONS) (cadr SESSIONS) (hash-set (caddr SESSIONS) 'host_index #f)))
  (render s)
  (s 'context! 100.0 (+ 40.0 92.0 10.0)) (render s)
  (define acc '())
  (parameterize ([current-text-sink (lambda (str xs y h soft?) (set! acc (cons str acc)))]) (render s))
  (check-not-false (member (t 'sb.m-hix-off) acc))
  (define c (s 'hit-center 'host-index "/d/c.rktd"))
  (check-equal? (s 'hit (car c) (cdr c)) '(host-index "/d/c.rktd"))
  (s 'set-sessions! (list (car SESSIONS) (cadr SESSIONS) (caddr SESSIONS)))
  (render s) (s 'context! 100.0 (+ 40.0 92.0 10.0))
  (define acc2 '())
  (parameterize ([current-text-sink (lambda (str xs y h soft?) (set! acc2 (cons str acc2)))]) (render s))
  (check-not-false (member (t 'sb.m-hix-auto) acc2)))
