#lang racket/base
;; win-keys.rkt — Windows 原生输入 → 逻辑词汇（keys.rkt）的对照表：给 Windows 后端用的数据（本机是 macOS，**没有在 Windows 上验证过**）。
;; 状态标注：documented = 取自微软公开文档的常量 / 惯例，未在设备上实测。Windows 组接手后按实测改 status。
;;   VK_*：learn.microsoft.com「Virtual-Key Codes」。字母 / 数字的 VK 跟随键盘布局（与 macOS 按字符取字母一致）；
;;   OEM 标点键按 US 布局列出（其他布局下 VK_OEM_* 对应的字符不同——与 macOS 的「标点按物理位置」同义）。
;;   修饰键：Ctrl → primary，Alt → alt，Shift → shift，Win → super（macOS 的 ctrl 在 Windows 不出现）。
;;   Windows 没有 AppKit 那样的「按键绑定 → 编辑命令」层：WIN-TEXT-KEYMAP 是按 Windows 惯例从按键推出编辑命令的表（同 keys.rkt 的命令名）。

(define WIN-STATUS 'documented)

;; VK → 逻辑键
(define WIN-VK
  (append
   '((#x08 . backspace) (#x09 . tab) (#x0D . return) (#x1B . escape) (#x20 . space) (#x21 . page-up) (#x22 . page-down)
     (#x23 . end) (#x24 . home) (#x25 . left) (#x26 . up) (#x27 . right) (#x28 . down) (#x2D . insert) (#x2E . delete)
     (#xBA . semicolon) (#xBB . equal) (#xBC . comma) (#xBD . minus) (#xBE . period) (#xBF . slash) (#xC0 . grave)
     (#xDB . bracket-left) (#xDC . backslash) (#xDD . bracket-right) (#xDE . quote))
   (for/list ([i 10]) (cons (+ #x30 i) (string->symbol (format "digit-~a" i))))
   (for/list ([c (in-string "abcdefghijklmnopqrstuvwxyz")] [i (in-naturals)]) (cons (+ #x41 i) (string->symbol (string c))))
   (for/list ([i 12]) (cons (+ #x70 i) (string->symbol (format "f~a" (add1 i)))))))
(define (win-logical-key vk) (cond [(assv vk WIN-VK) => cdr] [else #f]))

;; 修饰键：GetKeyState 的 VK（Windows 后端在 WM_KEYDOWN 时查这几个键的状态）
(define WIN-MOD-VK '((#x11 . primary) (#x10 . shift) (#x12 . alt) (#x5B . super) (#x5C . super)))

;; 鼠标按键（WM_*BUTTON* / XBUTTON1/2）→ ev:mouse 的 button：0 左 1 右 2 中；3 4 = 侧键（后退 / 前进）
(define WIN-MOUSE-BUTTONS '((left . 0) (right . 1) (middle . 2) (xbutton1 . 3) (xbutton2 . 4)))

;; 文字编辑键位（Windows 惯例）：组合键 → 编辑命令。同一命令可有多个组合键（Ctrl+Insert 也是复制）。
(define WIN-TEXT-KEYMAP
  '(((return) insert-newline) ((shift return) insert-line-break) ((tab) insert-tab)
    ((backspace) delete-backward) ((delete) delete-forward)
    ((primary backspace) delete-word-backward) ((primary delete) delete-word-forward)
    ((left) move-left) ((right) move-right) ((up) move-up) ((down) move-down)
    ((primary left) move-word-left) ((primary right) move-word-right)
    ((home) move-line-start) ((end) move-line-end) ((primary home) move-doc-start) ((primary end) move-doc-end)
    ((shift left) select-left) ((shift right) select-right) ((shift up) select-up) ((shift down) select-down)
    ((primary shift left) select-word-left) ((primary shift right) select-word-right)
    ((shift home) select-line-start) ((shift end) select-line-end)
    ((primary shift home) select-doc-start) ((primary shift end) select-doc-end)
    ((primary a) select-all)
    ((primary c) copy) ((primary insert) copy)
    ((primary x) cut) ((shift delete) cut)
    ((primary v) paste) ((shift insert) paste)
    ((primary z) undo) ((primary y) redo) ((primary shift z) redo)
    ((escape) cancel)))

;; macOS 有、Windows 没有惯例组合键的编辑命令：显式声明（不发明组合键），一致性检查据此放行；Windows 组可以改成绑定
(define WIN-TEXT-UNBOUND
  '((delete-to-line-start "macOS ⌘⌫; no Windows convention")
    (delete-to-line-end "macOS ⌃K / ⌘⌦; no Windows convention")))

(provide WIN-STATUS WIN-VK win-logical-key WIN-MOD-VK WIN-MOUSE-BUTTONS WIN-TEXT-KEYMAP WIN-TEXT-UNBOUND)
