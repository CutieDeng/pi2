#lang racket/base
;; keymap.rkt — 应用快捷键表（GUI 元数据平面：数据，与平台无关）。组合键用 keys.rkt 的逻辑写法：primary = macOS ⌘ / Windows Ctrl。
;;   表：(上下文 (组合键 动作) …) …；拼装层按当前上下文查动作，再按自己的状态决定做不做（做不了返回 #f → 照常往下分派）。
;;   上下文：global（随时）；sidebar-focus（侧栏有键盘焦点：单键操作，输入法已关）。
;;   显示文字（菜单提示、帮助）用 keys.rkt 的 chord-label 按平台生成，不手写。

(require racket/list "keys.rkt")

(define APP-KEYMAP
  '((global
     ((primary shift e) sidebar-keyboard)          ; 侧栏键盘模式开 / 关
     ((primary alt d) debug-panel)                 ; 调试面板
     ((primary shift t) tests-panel)               ; 工作区测试流程面板
     ((primary comma) settings)                    ; 设置页
     ((primary shift v) paste-secret)              ; 以密钥粘贴
     ((primary shift k) mark-secret)               ; 选中的文字 → 密钥
     ((primary b) toggle-sidebar)                  ; 侧栏开 / 关
     ((primary equal) zoom-in) ((primary shift equal) zoom-in)   ; ⌘= 与 ⌘+（⇧=）
     ((primary minus) zoom-out)
     ((primary digit-0) zoom-reset))
    (sidebar-focus
     ((escape) sidebar-escape)                     ; 先关菜单，再把焦点还给输入框
     ((up) focus-up) ((down) focus-down)
     ((return) open)
     ((backspace) delete) ((delete) delete)
     ((p) pin) ((e) archive) ((r) rename) ((i) info))))

;; 表的形状检查（测试 / 加载时）：每条都是合法组合键 + 符号动作；同一上下文里组合键不重复
(define (keymap-problems km)
  (append*
   (for/list ([ctx (in-list km)])
     (define binds (cdr ctx))
     (append
      (for/list ([b (in-list binds)] #:unless (and (list? b) (= (length b) 2) (chord? (car b)) (symbol? (cadr b))))
        (format "~a: bad binding ~s" (car ctx) b))
      (let ([cs (map (lambda (b) (cons (chord-key (car b)) (chord-mods (car b)))) (filter (lambda (b) (and (list? b) (pair? b) (chord? (car b)))) binds))])
        (for/list ([c (in-list (remove-duplicates cs))] #:when (> (count (lambda (x) (equal? x c)) cs) 1))
          (format "~a: chord bound twice ~s" (car ctx) c)))))))

;; 查动作：上下文 + 逻辑键 + 修饰键 → 动作或 #f
(define (keymap-lookup km ctx key mods)
  (define binds (cond [(assq ctx km) => cdr] [else '()]))
  (for/first ([b (in-list binds)] #:when (chord-matches? (car b) key mods)) (cadr b)))

;; 某动作的组合键（显示用）
(define (keymap-chords km ctx action)
  (define binds (cond [(assq ctx km) => cdr] [else '()]))
  (for/list ([b (in-list binds)] #:when (eq? (cadr b) action)) (car b)))

(provide APP-KEYMAP keymap-problems keymap-lookup keymap-chords)
