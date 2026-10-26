#lang racket/base
;; keys.rkt — 输入事件的逻辑词汇（GUI 元数据平面：各平台共用；平台只提供「原生 → 逻辑」的对照表，见 mac-keys.rkt / win-keys.rkt）。
;;
;; 逻辑键名（ev:key 的 key 字段）：
;;   字母 a … z；数字 digit-0 … digit-9；
;;   命名键 escape return tab space backspace delete insert home end page-up page-down left right up down f1 … f12；
;;   标点（按 US 布局的物理位置）equal minus comma period slash semicolon quote bracket-left bracket-right backslash grave；
;;   认不出的键：#f（原生键码仍在 ev:key 的 code 字段里，只供诊断）。
;; 逻辑修饰键（ev:key / ev:mouse 的 mods）：
;;   primary —— 应用快捷键的主修饰键：macOS ⌘，Windows Ctrl
;;   shift alt —— 两边同名（macOS 的 ⌥ 是 alt）
;;   ctrl    —— 只在 macOS 出现（⌃：与 ⌘ 是两个键；⌃点击 = 右键）
;;   super   —— 只在 Windows 出现（Win 键）
;; 编辑命令（ev:command 的 name）：COMMANDS 里的中性名字；macOS 由系统按键绑定产生（mac-keys.rkt 对照 AppKit selector），
;;   Windows 没有这层，由 win-keys.rkt 的文字编辑键位表从按键推出。

(require racket/list racket/string)

(define LETTERS (for/list ([c (in-string "abcdefghijklmnopqrstuvwxyz")]) (string->symbol (string c))))
(define DIGITS (for/list ([i 10]) (string->symbol (format "digit-~a" i))))
(define NAMED-KEYS '(escape return tab space backspace delete insert home end page-up page-down left right up down
                     f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12))
(define PUNCT-KEYS '(equal minus comma period slash semicolon quote bracket-left bracket-right backslash grave))
(define LOGICAL-KEYS (append LETTERS DIGITS NAMED-KEYS PUNCT-KEYS))
(define LOGICAL-MODS '(primary shift alt ctrl super))

;; 编辑命令：名字 → 说明（输入框 / 设置页 / 侧栏改名的编辑器都认这一套）
(define COMMANDS
  '((insert-newline "Enter: submit (input box) / confirm")
    (insert-line-break "new line without submitting")
    (insert-tab "insert a tab")
    (delete-backward "delete the grapheme before the caret (or the selection)")
    (delete-forward "delete the grapheme after the caret (or the selection)")
    (delete-word-backward "delete to the start of the word")
    (delete-word-forward "delete to the end of the word")
    (delete-to-line-start "delete to the start of the line")
    (delete-to-line-end "delete to the end of the line")
    (move-left "caret one grapheme left (collapses a selection)") (move-right "caret one grapheme right")
    (move-up "caret one line up") (move-down "caret one line down")
    (move-word-left "caret to the previous word start") (move-word-right "caret to the next word end")
    (move-line-start "caret to the line start") (move-line-end "caret to the line end")
    (move-doc-start "caret to the start of the text") (move-doc-end "caret to the end of the text")
    (select-left "extend the selection one grapheme left") (select-right "extend the selection one grapheme right")
    (select-up "extend the selection one line up") (select-down "extend the selection one line down")
    (select-word-left "extend the selection to the previous word start") (select-word-right "extend the selection to the next word end")
    (select-line-start "extend the selection to the line start") (select-line-end "extend the selection to the line end")
    (select-doc-start "extend the selection to the start of the text") (select-doc-end "extend the selection to the end of the text")
    (select-all "select everything")
    (copy "copy the selection") (cut "cut the selection") (paste "paste (the window turns it into ev:paste with the clipboard text)")
    (undo "undo") (redo "redo")
    (cancel "Esc: close the panel / menu / cancel")))
(define COMMAND-NAMES (map car COMMANDS))

;; ---------------------------------------------------------------- 组合键（chord）
;; 写法：(修饰键 … 键)，如 (primary shift t)、(escape)。匹配要求修饰键集合完全相同。
(define (chord-key c) (last c))
(define (chord-mods c) (sort-mods (drop-right c 1)))
(define (sort-mods ms) (filter (lambda (m) (memq m ms)) LOGICAL-MODS))
(define (chord? c)
  (and (pair? c) (memq (last c) LOGICAL-KEYS) (andmap (lambda (m) (memq m LOGICAL-MODS)) (drop-right c 1)) #t))
(define (chord-matches? c key mods)
  (and (eq? (chord-key c) key) (equal? (chord-mods c) (sort-mods mods))))

;; 组合键的显示文字：平台的写法是数据（macOS 符号紧排 ⌃⌥⇧⌘T；Windows Ctrl+Alt+Shift+T）
(define LABEL-STYLES
  (hasheq 'macos (hasheq 'order '(ctrl alt shift primary) 'sep ""
                         'mods (hasheq 'primary "⌘" 'shift "⇧" 'alt "⌥" 'ctrl "⌃" 'super "◆")
                         'keys (hasheq 'escape "⎋" 'return "↩" 'tab "⇥" 'space "Space" 'backspace "⌫" 'delete "⌦"
                                       'left "←" 'right "→" 'up "↑" 'down "↓" 'home "↖" 'end "↘" 'page-up "⇞" 'page-down "⇟"))
          'windows (hasheq 'order '(primary alt shift super) 'sep "+"
                           'mods (hasheq 'primary "Ctrl" 'shift "Shift" 'alt "Alt" 'ctrl "Ctrl" 'super "Win")
                           'keys (hasheq 'escape "Esc" 'return "Enter" 'tab "Tab" 'space "Space" 'backspace "Backspace" 'delete "Del"
                                         'insert "Ins" 'left "Left" 'right "Right" 'up "Up" 'down "Down" 'home "Home" 'end "End"
                                         'page-up "PgUp" 'page-down "PgDn"))))
(define PUNCT-CHARS (hasheq 'equal "=" 'minus "-" 'comma "," 'period "." 'slash "/" 'semicolon ";" 'quote "'"
                            'bracket-left "[" 'bracket-right "]" 'backslash "\\" 'grave "`"))
(define (key-label k style)
  (define s (symbol->string k))
  (cond
    [(hash-ref (hash-ref style 'keys) k #f) => values]
    [(hash-ref PUNCT-CHARS k #f) => values]
    [(regexp-match #rx"^digit-([0-9])$" s) => cadr]
    [(regexp-match? #rx"^f[0-9]+$" s) (string-upcase s)]
    [else (string-upcase s)]))
;; 显示快捷键用哪个平台的写法：拼装层按选用的平台组件设（macOS 组件 → 'macos）
(define current-key-platform (make-parameter 'macos))
(define (chord-label c [platform (current-key-platform)])
  (define style (hash-ref LABEL-STYLES platform))
  (define ms (for/list ([m (in-list (hash-ref style 'order))] #:when (memq m (chord-mods c))) (hash-ref (hash-ref style 'mods) m)))
  (string-join (append ms (list (key-label (chord-key c) style))) (hash-ref style 'sep)))

(provide LETTERS DIGITS NAMED-KEYS PUNCT-KEYS LOGICAL-KEYS LOGICAL-MODS COMMANDS COMMAND-NAMES
         chord? chord-key chord-mods sort-mods chord-matches? LABEL-STYLES chord-label current-key-platform)
