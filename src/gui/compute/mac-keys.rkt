#lang racket/base
;; mac-keys.rkt — macOS 原生输入 → 逻辑词汇（keys.rkt）的对照表：纯数据 + 纯函数（mac-window 用它把 NSEvent 翻成逻辑事件；
;; 拼装层用它升级旧日志）。Windows 那边的同类表在 win-keys.rkt。
;;   键码：Carbon kVK_*（Events.h；ANSI 键位）。逻辑键的取法（与改造前的快捷键判定一致）：
;;     命名键（Esc、方向键、回车 …）按键码；字母 / 数字按「不计修饰键的字符」（随键盘布局，与菜单快捷键同）；标点按物理位置（键码）。
;;   修饰键位：NSEventModifierFlags（shift 1<<17 ctrl 1<<18 alt 1<<19 cmd 1<<20），⌘ → primary。
;;   编辑命令：AppKit 的按键绑定发 doCommandBySelector: / 菜单项动作 → 中性命令名。

(require racket/list racket/string "keys.rkt")

;; 键码 → 逻辑键（命名键与标点；字母数字也列上，供合成按键与没有字符时用）
(define MAC-KEYCODES
  '((0 . a) (1 . s) (2 . d) (3 . f) (4 . h) (5 . g) (6 . z) (7 . x) (8 . c) (9 . v) (11 . b) (12 . q) (13 . w) (14 . e) (15 . r)
    (16 . y) (17 . t) (18 . digit-1) (19 . digit-2) (20 . digit-3) (21 . digit-4) (22 . digit-6) (23 . digit-5) (24 . equal)
    (25 . digit-9) (26 . digit-7) (27 . minus) (28 . digit-8) (29 . digit-0) (30 . bracket-right) (31 . o) (32 . u)
    (33 . bracket-left) (34 . i) (35 . p) (36 . return) (37 . l) (38 . j) (39 . quote) (40 . k) (41 . semicolon)
    (42 . backslash) (43 . comma) (44 . slash) (45 . n) (46 . m) (47 . period) (48 . tab) (49 . space) (50 . grave)
    (51 . backspace) (53 . escape) (76 . return) (96 . f5) (97 . f6) (98 . f7) (99 . f3) (100 . f8) (101 . f9) (103 . f11)
    (109 . f10) (111 . f12) (114 . insert) (115 . home) (116 . page-up) (117 . delete) (118 . f4) (119 . end) (120 . f2)
    (121 . page-down) (122 . f1) (123 . left) (124 . right) (125 . down) (126 . up)))
(define CODE->KEY (for/hasheqv ([p (in-list MAC-KEYCODES)]) (values (car p) (cdr p))))
(define KEY->CODE (for/hasheq ([p (in-list (reverse MAC-KEYCODES))]) (values (cdr p) (car p))))   ; return → 36（不是小键盘 76）
(define BY-CODE-ONLY '(escape return tab space backspace delete insert home end page-up page-down left right up down
                       f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12
                       equal minus comma period slash semicolon quote bracket-left bracket-right backslash grave))

;; NSEvent → 逻辑键：code = keyCode；base = charactersIgnoringModifiers（字符串或 #f）
(define (mac-logical-key code base)
  (define by-code (hash-ref CODE->KEY code #f))
  (cond
    [(and by-code (memq by-code BY-CODE-ONLY)) by-code]
    [(and (string? base) (= (string-length base) 1) (char-alphabetic? (string-ref base 0)) (char<=? (string-ref base 0) #\z)
          (memq (string->symbol (string-downcase base)) LETTERS))
     (string->symbol (string-downcase base))]
    [(and (string? base) (= (string-length base) 1) (char-numeric? (string-ref base 0)) (char<=? (string-ref base 0) #\9))
     (string->symbol (format "digit-~a" base))]
    [else by-code]))

;; 修饰键位 → 逻辑修饰键（与 mac-window 原来的 decode-mods 同一组位，⌘ 换名 primary）
(define MAC-MOD-BITS '((#x20000 . shift) (#x40000 . ctrl) (#x80000 . alt) (#x100000 . primary)))
(define (mac-logical-mods flags)
  (for/list ([p (in-list MAC-MOD-BITS)] #:unless (zero? (bitwise-and flags (car p)))) (cdr p)))
(define (mac-mod-flags mods)                              ; 逆向：合成事件用
  (for/sum ([p (in-list MAC-MOD-BITS)] #:when (memq (cdr p) mods)) (car p)))

;; AppKit selector → 中性命令名（同义的几个 selector 归到同一个命令）
(define MAC-SELECTORS
  '((insertNewline: . insert-newline) (insertNewlineIgnoringFieldEditor: . insert-line-break) (insertLineBreak: . insert-line-break)
    (insertTab: . insert-tab)
    (deleteBackward: . delete-backward) (deleteBackwardByDecomposingPreviousCharacter: . delete-backward)
    (deleteForward: . delete-forward) (deleteWordBackward: . delete-word-backward) (deleteWordForward: . delete-word-forward)
    (deleteToBeginningOfLine: . delete-to-line-start) (deleteToEndOfLine: . delete-to-line-end) (deleteToEndOfParagraph: . delete-to-line-end)
    (moveLeft: . move-left) (moveRight: . move-right) (moveUp: . move-up) (moveDown: . move-down)
    (moveWordLeft: . move-word-left) (moveWordRight: . move-word-right)
    (moveToBeginningOfLine: . move-line-start) (moveToLeftEndOfLine: . move-line-start) (moveToBeginningOfParagraph: . move-line-start)
    (moveToEndOfLine: . move-line-end) (moveToRightEndOfLine: . move-line-end) (moveToEndOfParagraph: . move-line-end)
    (moveToBeginningOfDocument: . move-doc-start) (moveToEndOfDocument: . move-doc-end)
    (moveLeftAndModifySelection: . select-left) (moveRightAndModifySelection: . select-right)
    (moveUpAndModifySelection: . select-up) (moveDownAndModifySelection: . select-down)
    (moveWordLeftAndModifySelection: . select-word-left) (moveWordRightAndModifySelection: . select-word-right)
    (moveToBeginningOfLineAndModifySelection: . select-line-start) (moveToLeftEndOfLineAndModifySelection: . select-line-start)
    (moveToEndOfLineAndModifySelection: . select-line-end) (moveToRightEndOfLineAndModifySelection: . select-line-end)
    (moveToBeginningOfDocumentAndModifySelection: . select-doc-start) (moveToEndOfDocumentAndModifySelection: . select-doc-end)
    (selectAll: . select-all) (copy: . copy) (cut: . cut) (paste: . paste) (undo: . undo) (redo: . redo)
    (cancelOperation: . cancel)))
(define SEL->CMD (for/hasheq ([p (in-list MAC-SELECTORS)]) (values (car p) (cdr p))))
;; 认不出的 selector：mac: 前缀保留原名（没有组件处理它；日志里看得到是什么）
(define (mac-command sel) (hash-ref SEL->CMD sel (lambda () (string->symbol (format "mac:~a" sel)))))

;; 合成按键（冒烟脚本的 post-key!）：逻辑键 + 修饰键 → (values 字符 键码 mac修饰键表)
(define NAMED-CHARS (hasheq 'escape (string (integer->char 27)) 'return "\r" 'tab "\t" 'space " " 'backspace (string (integer->char 127))
                            'delete (string (integer->char #xF728)) 'left (string (integer->char #xF702)) 'right (string (integer->char #xF703))
                            'up (string (integer->char #xF700)) 'down (string (integer->char #xF701))
                            'home (string (integer->char #xF729)) 'end (string (integer->char #xF72B))
                            'page-up (string (integer->char #xF72C)) 'page-down (string (integer->char #xF72D))))
(define PUNCT (hasheq 'equal "=" 'minus "-" 'comma "," 'period "." 'slash "/" 'semicolon ";" 'quote "'"
                      'bracket-left "[" 'bracket-right "]" 'backslash "\\" 'grave "`"))
(define (mac-synthetic-key key mods)
  (define code (hash-ref KEY->CODE key (lambda () (error 'mac-synthetic-key "no macOS key code for ~s" key))))
  (define s (symbol->string key))
  (define chars
    (cond [(hash-ref NAMED-CHARS key #f) => values]
          [(hash-ref PUNCT key #f) => values]
          [(regexp-match #rx"^digit-([0-9])$" s) => cadr]
          [(memq 'shift mods) (string-upcase s)]
          [else s]))
  (values chars code (for/list ([m (in-list mods)]) (if (eq? m 'primary) 'cmd m))))

;; ---------------------------------------------------------------- 旧日志（版本 1：原生键码、cmd、AppKit selector）→ 逻辑事件
;; 记录里的窗口事件是 prefab：按 prefab 键名与字段数认，不依赖新的结构体定义
(define (upgrade-mods ms) (if (list? ms) (for/list ([m (in-list ms)]) (if (eq? m 'cmd) 'primary m)) ms))
(define (mac-upgrade-event ev)
  (define v (and (struct? ev) (struct->vector ev)))
  (define key (and (prefab-struct-key ev) (prefab-struct-key ev)))
  (cond
    [(and (eq? key 'ev:key) (= (vector-length v) 6))                ; (down? code chars mods repeat?)
     (define code (vector-ref v 2)) (define chars (vector-ref v 3))
     (define base (and (string? chars) (string-downcase chars)))
     (make-prefab-struct 'ev:key (vector-ref v 1) code chars (upgrade-mods (vector-ref v 4)) (vector-ref v 5) (mac-logical-key code base))]
    [(and (eq? key 'ev:mouse) (= (vector-length v) 7))
     (make-prefab-struct 'ev:mouse (vector-ref v 1) (vector-ref v 2) (vector-ref v 3) (vector-ref v 4) (upgrade-mods (vector-ref v 5)) (vector-ref v 6))]
    [(and (eq? key 'ev:command) (= (vector-length v) 2) (symbol? (vector-ref v 1))
          (regexp-match? #rx":$" (symbol->string (vector-ref v 1))))
     (make-prefab-struct 'ev:command (mac-command (vector-ref v 1)))]
    [else ev]))

(provide MAC-KEYCODES mac-logical-key mac-logical-mods mac-mod-flags MAC-SELECTORS mac-command mac-synthetic-key mac-upgrade-event)
