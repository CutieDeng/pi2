#lang racket/base
;; keys-test.rkt — GUI 元数据平面：事件词汇（keys.rkt）、应用键位表（keymap.rkt）、平台对照表（mac-keys.rkt、win-keys.rkt）的一致性。
;;   这些表是 macOS / Windows 共用的数据；这里的检查不需要窗口，Windows 组改表时同样能跑。

(require rackunit racket/list
         "../../src/gui/compute/keys.rkt" "../../src/gui/compute/keymap.rkt"
         "../../src/gui/compute/mac-keys.rkt" "../../src/gui/compute/win-keys.rkt" "../../src/gui/compute/event.rkt")

(test-case "vocabulary: logical keys / modifiers / commands are distinct symbols"
  (check-equal? (length LOGICAL-KEYS) (length (remove-duplicates LOGICAL-KEYS)))
  (check-equal? (length COMMAND-NAMES) (length (remove-duplicates COMMAND-NAMES)))
  (check-true (andmap symbol? LOGICAL-KEYS))
  (check-equal? LOGICAL-MODS '(primary shift alt ctrl super)))

(test-case "chords: parse, match regardless of modifier order, labels per platform"
  (check-true (chord? '(primary shift t)))
  (check-false (chord? '(cmd t)) "cmd is a macOS name, not a logical modifier")
  (check-false (chord? '(primary tee)))
  (check-true (chord-matches? '(primary shift t) 't '(shift primary)))
  (check-false (chord-matches? '(primary t) 't '(primary shift)) "modifier sets must be equal")
  (check-equal? (chord-label '(primary shift t) 'macos) "⇧⌘T")
  (check-equal? (chord-label '(primary shift t) 'windows) "Ctrl+Shift+T")
  (check-equal? (chord-label '(primary comma) 'macos) "⌘,")
  (check-equal? (chord-label '(primary alt d) 'windows) "Ctrl+Alt+D")
  (check-equal? (chord-label '(escape) 'windows) "Esc"))

(test-case "app keymap: well formed, no chord bound twice in a context, lookups"
  (check-equal? (keymap-problems APP-KEYMAP) '())
  (check-equal? (keymap-lookup APP-KEYMAP 'global 't '(primary shift)) 'tests-panel)
  (check-equal? (keymap-lookup APP-KEYMAP 'global 'equal '(primary shift)) 'zoom-in "⌘+ is ⇧=")
  (check-false (keymap-lookup APP-KEYMAP 'global 't '(primary)))
  (check-equal? (keymap-lookup APP-KEYMAP 'sidebar-focus 'backspace '()) 'delete)
  (check-equal? (keymap-chords APP-KEYMAP 'global 'zoom-in) '((primary equal) (primary shift equal)))
  (check-equal? (keymap-problems '((global ((primary t) a) ((shift primary t) b) ((primary t) c)))) '("global: chord bound twice (t primary)"))
  (check-equal? (keymap-problems '((global ((t primary) b)))) '("global: bad binding ((t primary) b)") "the key comes last")
  (check-equal? (length (keymap-problems '((global ((cmd t) a))))) 1))

(test-case "macOS table: key codes cover the logical keys the keymap uses; synthetic keys round-trip"
  (for ([p (in-list MAC-KEYCODES)]) (check-not-false (memq (cdr p) LOGICAL-KEYS) (format "~s" p)))
  (define used (remove-duplicates (for*/list ([ctx (in-list APP-KEYMAP)] [b (in-list (cdr ctx))]) (chord-key (car b)))))
  (for ([k (in-list used)])
    (define-values (chars code mods) (mac-synthetic-key k '()))
    (check-equal? (mac-logical-key code (and (= (string-length chars) 1) (char-alphabetic? (string-ref chars 0)) chars)) k (format "~s" k)))
  ;; 字母按「不计修饰键的字符」：⇧T 的字符是 "T"，逻辑键还是 t；⌥D 的字符是 ∂（不认），按键码回落到 d
  (check-equal? (mac-logical-key 17 "T") 't)
  (check-equal? (mac-logical-key 2 (string (integer->char #x2202))) 'd)
  (check-equal? (mac-logical-key 53 (string (integer->char 27))) 'escape)
  (check-equal? (mac-logical-key 24 "+") 'equal "punctuation by position")
  (check-equal? (mac-logical-mods (+ #x100000 #x20000)) '(shift primary))
  (check-equal? (mac-mod-flags '(primary shift)) (+ #x100000 #x20000))
  (let-values ([(chars code mods) (mac-synthetic-key 'k '(primary shift))])
    (check-equal? (list chars code mods) '("K" 40 (cmd shift)))))

(test-case "macOS selectors map onto the neutral command vocabulary; unknown ones are kept with a mac: prefix"
  (for ([p (in-list MAC-SELECTORS)]) (check-not-false (memq (cdr p) COMMAND-NAMES) (format "~s" p)))
  (check-equal? (mac-command 'deleteBackward:) 'delete-backward)
  (check-equal? (mac-command 'transpose:) '|mac:transpose:|))

(test-case "Windows table (documented, not verified on a device): VKs map to logical keys; the text keymap uses known commands"
  (check-eq? WIN-STATUS 'documented)
  (for ([p (in-list WIN-VK)]) (check-not-false (memq (cdr p) LOGICAL-KEYS) (format "~s" p)))
  (check-equal? (length WIN-VK) (length (remove-duplicates (map car WIN-VK))))
  (check-equal? (win-logical-key #x41) 'a)
  (check-equal? (win-logical-key #x1B) 'escape)
  (check-equal? (keymap-problems (list (cons 'text WIN-TEXT-KEYMAP))) '())
  (for ([b (in-list WIN-TEXT-KEYMAP)]) (check-not-false (memq (cadr b) COMMAND-NAMES) (format "~s" b)))
  ;; 每个 macOS 由系统按键绑定产生的编辑命令，Windows 键位表里都有对应，或在 WIN-TEXT-UNBOUND 里显式声明没有惯例（不静默缺失）
  (define mac-cmds (remove-duplicates (map cdr MAC-SELECTORS)))
  (define win-cmds (append (remove-duplicates (map cadr WIN-TEXT-KEYMAP)) (map car WIN-TEXT-UNBOUND)))
  (check-equal? (filter (lambda (c) (not (memq c win-cmds))) mac-cmds) '())
  (for ([u (in-list WIN-TEXT-UNBOUND)]) (check-false (assq (car u) (map (lambda (b) (list (cadr b))) WIN-TEXT-KEYMAP)) (format "~s is both bound and declared unbound" (car u)))))

(test-case "old journals (version 1): native key events, cmd and AppKit selectors are upgraded to logical events"
  (define old-key (make-prefab-struct 'ev:key #t 17 "t" '(cmd shift) #f))
  (define k (mac-upgrade-event old-key))
  (check-true (ev:key? k))
  (check-equal? (list (ev:key-key k) (ev:key-mods k) (ev:key-code k)) '(t (primary shift) 17))
  (define old-mouse (make-prefab-struct 'ev:mouse 'down 1.0 2.0 0 '(cmd) 1))
  (check-equal? (ev:mouse-mods (mac-upgrade-event old-mouse)) '(primary))
  (check-equal? (ev:command-name (mac-upgrade-event (ev:command 'moveWordLeftAndModifySelection:))) 'select-word-left)
  (check-equal? (mac-upgrade-event (ev:text "x")) (ev:text "x"))
  (check-equal? (mac-upgrade-event (ev:command 'copy)) (ev:command 'copy) "already neutral → unchanged"))
