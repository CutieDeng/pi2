#lang racket/base
;; tests/gui/editor-test.rkt — 输入框编辑器模型 + 粘贴折叠（纯逻辑，无平台）

(require rackunit racket/list racket/string
         "../../src/gui/compute/fold.rkt"
         "../../src/gui/compute/editor.rkt")

;; 实际场景的粘贴样本
(define TRACE
  (string-append "Traceback (most recent call last):\n"
                 (apply string-append (for/list ([i 40]) (format "  File \"/app/mod~a.py\", line ~a, in f~a\n    call()\n" i (* 3 i) i)))
                 "ValueError: bad input\n"))
(define DIFF "diff --git a/x.rkt b/x.rkt\n--- a/x.rkt\n+++ b/x.rkt\n@@ -1,3 +1,3 @@\n-(define a 1)\n+(define a 2)\n (provide a)\n context\n more\n")
(define JS-TRACE (string-append "TypeError: x is undefined\n" (apply string-append (for/list ([i 10]) (format "    at fn~a (/src/app.js:~a:7)\n" i (+ 10 i))))))
(define LOG (apply string-append (for/list ([i 12]) (format "2026-10-22 16:0~a:00 INFO worker ~a started\n" (modulo i 10) i))))
(define JSON (string-append "{\n" (apply string-append (for/list ([i 10]) (format "  \"k~a\": ~a,\n" i i))) "  \"end\": true\n}"))
(define CODE (apply string-append (for/list ([i 10]) (format "function f~a(x) {\n  return x + ~a;\n}\n" i i))))
(define LONG-LINE (make-string 1200 #\x))
(define SHORT "line one\nline two\nline three")

(define (type! e s) (editor-insert! e s))
(define (cmd! e . cs) (for/last ([c cs]) (editor-command! e c)))

;; ---------------------------------------------------------------- fold

(test-case "fold thresholds: short snippets stay inline, big pastes fold"
  (check-false (fold-worthy? SHORT))
  (check-true (fold-worthy? TRACE))
  (check-true (fold-worthy? LONG-LINE))                ; 单行超长（压缩 JSON、base64）
  (check-equal? (line-count "a\nb\n") 2)                ; 末尾换行不算一行
  (check-equal? (line-count "") 1))

(test-case "classify real paste kinds"
  (check-eq? (classify TRACE) 'trace)
  (check-eq? (classify JS-TRACE) 'trace)
  (check-eq? (classify DIFF) 'diff)
  (check-eq? (classify LOG) 'log)
  (check-eq? (classify JSON) 'json)
  (check-eq? (classify CODE) 'code)
  (check-eq? (classify "just some prose\nwith lines") 'text))

(test-case "fold label shows kind, id, first line, line count"
  (define f (make-fold 3 TRACE))
  (check-equal? (fold-label f) "报错栈 #3 · Traceback (most recent call last): · 82 行")
  (check-regexp-match #rx"^报错栈 #3 · 82 行 · [0-9.]+ KB$" (fold-summary f))
  (check-equal? (fold-preview (make-fold 1 (string-append "\n\n   " (make-string 50 #\a) "\nb"))) (string-append (make-string 36 #\a) "…")))

(test-case "normalize: CRLF / CR → LF, U+FFFC stripped (cannot forge a chip)"
  (check-equal? (normalize-paste (string-append "a\r\nb\rc" OBJ-STR "d")) "a\nb\ncd"))

;; ---------------------------------------------------------------- 粘贴进编辑器

(test-case "paste: big text becomes one atomic chip between typed instructions"
  (define e (make-editor))
  (type! e "看下这个报错 ")
  (define f (editor-paste! e TRACE))
  (check-true (fold? f))
  (type! e " 怎么修？")
  (check-equal? (editor-text e) (string-append "看下这个报错 " OBJ-STR " 怎么修？"))
  (check-equal? (editor-chips e) (list (fold-id f)))
  (check-equal? (editor-flatten e) (string-append "看下这个报错 " TRACE " 怎么修？")))

(test-case "paste: short snippet is inserted as editable text"
  (define e (make-editor))
  (check-false (editor-paste! e SHORT))
  (check-equal? (editor-text e) SHORT)
  (check-equal? (editor-chips e) '()))

(test-case "one backspace deletes the whole chip; undo restores it with its content"
  (define e (make-editor))
  (type! e "a")
  (editor-paste! e TRACE)
  (type! e "b")
  (cmd! e 'moveLeft:)
  (cmd! e 'deleteBackward:)
  (check-equal? (editor-text e) "ab")
  (check-equal? (editor-chips e) '())
  (cmd! e 'undo:)
  (check-equal? (editor-flatten e) (string-append "a" TRACE "b"))
  (cmd! e 'redo:)
  (check-equal? (editor-text e) "ab"))

(test-case "cursor steps over a chip in one move; selection across chips flattens on copy"
  (define e (make-editor))
  (type! e "x")
  (editor-paste! e DIFF)
  (editor-paste! e LOG)
  (type! e "y")
  (editor-move-to! e 1)
  (cmd! e 'moveRight:)
  (check-equal? (editor-cursor e) 2)
  (cmd! e 'moveRightAndModifySelection: 'moveRightAndModifySelection:)
  (check-equal? (editor-command! e 'copy:) (cons 'copy (string-append LOG "y")))
  (check-equal? (editor-command! e 'cut:) (cons 'copy (string-append LOG "y")))
  (check-equal? (editor-flatten e) (string-append "x" DIFF)))

(test-case "multiple chips keep their order through deletes in the middle"
  (define e (make-editor))
  (define f1 (editor-paste! e TRACE))
  (type! e " and ")
  (define f2 (editor-paste! e DIFF))
  (type! e " and ")
  (define f3 (editor-paste! e LOG))
  (editor-remove-chip! e (fold-id f2))
  (check-equal? (editor-chips e) (list (fold-id f1) (fold-id f3)))
  (check-equal? (editor-flatten e) (string-append TRACE " and  and " LOG)))

(test-case "unfold: chip turns into editable text (undoable)"
  (define e (make-editor))
  (type! e "see: ")
  (define f (editor-paste! e LOG))
  (editor-toggle-expand! e (fold-id f))
  (check-equal? (editor-expanded e) (fold-id f))
  (editor-unfold! e (fold-id f))
  (check-equal? (editor-text e) (string-append "see: " LOG))
  (check-equal? (editor-chips e) '())
  (check-false (editor-expanded e))
  (cmd! e 'undo:)
  (check-equal? (editor-text e) (string-append "see: " OBJ-STR)))

;; ---------------------------------------------------------------- 打字 / 撤销合并

(test-case "consecutive typing is one undo step; a cursor move breaks it"
  (define e (make-editor))
  (for ([c "hello"]) (type! e (string c)))
  (cmd! e 'moveLeft:)
  (for ([c "XY"]) (type! e (string c)))
  (check-equal? (editor-text e) "hellXYo")
  (cmd! e 'undo:)
  (check-equal? (editor-text e) "hello")
  (cmd! e 'undo:)
  (check-equal? (editor-text e) ""))

(test-case "typing replaces the selection"
  (define e (make-editor))
  (type! e "hello world")
  (cmd! e 'moveWordLeftAndModifySelection:)
  (type! e "racket")
  (check-equal? (editor-text e) "hello racket"))

;; ---------------------------------------------------------------- 移动 / 删除

(test-case "grapheme-aware delete and move (combining mark, ZWJ emoji)"
  (define e (make-editor))
  (define fam "👨‍👩‍👧")
  (type! e (string-append "e" (string (integer->char #x301)) fam "z"))
  (cmd! e 'moveLeft:)
  (cmd! e 'deleteBackward:)
  (check-equal? (editor-text e) (string-append "e" (string (integer->char #x301)) "z"))
  (cmd! e 'deleteBackward:)
  (check-equal? (editor-text e) "z"))

(test-case "word moves: Latin words, CJK one char per step, chip one step"
  (define e (make-editor))
  (type! e "foo bar 你好")
  (editor-paste! e LOG)
  (check-equal? (begin (cmd! e 'moveWordLeft:) (editor-cursor e)) 10)
  (check-equal? (begin (cmd! e 'moveWordLeft:) (editor-cursor e)) 9)
  (check-equal? (begin (cmd! e 'moveWordLeft:) (editor-cursor e)) 8)
  (check-equal? (begin (cmd! e 'moveWordLeft:) (editor-cursor e)) 4)
  (cmd! e 'deleteWordBackward:)
  (check-equal? (editor-text e) (string-append "bar 你好" OBJ-STR)))

(test-case "line commands on multi-line text"
  (define e (make-editor))
  (type! e "first\nsecond line\nthird")
  (editor-move-to! e 9)                                   ; second 里
  (cmd! e 'moveToBeginningOfLine:)
  (check-equal? (editor-cursor e) 6)
  (cmd! e 'moveToEndOfLine:)
  (check-equal? (editor-cursor e) 17)
  (cmd! e 'deleteToBeginningOfLine:)
  (check-equal? (editor-text e) "first\n\nthird")
  (cmd! e 'deleteToEndOfLine:)                             ; 行尾处删掉换行本身
  (check-equal? (editor-text e) "first\nthird"))

;; ---------------------------------------------------------------- 提交 / 历史

(test-case "Enter asks the caller; Option-Enter inserts a newline"
  (define e (make-editor))
  (type! e "a")
  (check-eq? (cmd! e 'insertNewline:) 'submit)
  (cmd! e 'insertNewlineIgnoringFieldEditor:)
  (type! e "b")
  (check-equal? (editor-text e) "a\nb"))

(test-case "submit returns flattened text, clears, history restores chips"
  (define e (make-editor))
  (type! e "fix ")
  (define f (editor-paste! e TRACE))
  (define r (editor-submit! e))
  (check-equal? (car r) (string-append "fix " TRACE))
  (check-true (editor-empty? e))
  (check-false (editor-submit! e))                         ; 空不提交
  (type! e "draft")
  (editor-history-prev! e)
  (check-equal? (editor-text e) (string-append "fix " OBJ-STR))
  (check-equal? (editor-chips e) (list (fold-id f)))
  (editor-history-next! e)
  (check-equal? (editor-text e) "draft"))                  ; 草稿被暂存并恢复

(test-case "whitespace-only input does not submit"
  (define e (make-editor))
  (type! e "  \n ")
  (check-false (editor-submit! e)))

;; ---------------------------------------------------------------- IME

(test-case "marked text shows at the cursor, is not committed until insertText"
  (define e (make-editor))
  (type! e "说")
  (editor-set-marked! e "ni hao" 6)
  (define-values (disp caret mr) (editor-display e))
  (check-equal? disp "说ni hao")
  (check-equal? caret 7)
  (check-equal? mr (cons 1 7))
  (check-equal? (editor-text e) "说")
  (check-false (editor-submit! e))                         ; 组字中不提交
  (editor-insert! e "你好")
  (check-false (editor-marked e))
  (check-equal? (editor-text e) "说你好"))

(test-case "starting composition replaces the selection; empty marked string cancels"
  (define e (make-editor))
  (type! e "abc")
  (cmd! e 'selectAll:)
  (editor-set-marked! e "wq" 2)
  (check-equal? (editor-text e) "")
  (editor-set-marked! e "" 0)
  (check-false (editor-marked e)))

(test-case "drafts (one per host): save / load keeps text, chips, cursor and undo; loading the empty draft clears"
  (define e (make-editor))
  (editor-insert! e "on host a ")
  (editor-paste! e (string-join (for/list ([i 10]) (format "line ~a" i)) "\n"))   ; 折叠成 chip
  (editor-insert! e " tail")
  (define d (editor-draft e))
  (editor-load-draft! e EMPTY-DRAFT)
  (check-equal? (editor-text e) "")
  (check-equal? (editor-chips e) '())
  (editor-insert! e "b")
  (editor-load-draft! e d)
  (check-equal? (length (editor-chips e)) 1)
  (check-regexp-match #rx"^on host a .* tail$" (editor-text e))
  (check-equal? (editor-cursor e) (string-length (editor-text e)))
  (editor-command! e 'undo:)                                                  ; 撤销栈跟着草稿
  (check-false (regexp-match? #rx"tail$" (editor-text e))))

(test-case "mark the selection as a secret: becomes a confirmed secret chip; empty / whitespace / chip-containing selections are refused; undo restores"
  (define e (make-editor))
  (editor-insert! e "token=abcd1234efgh5678 please")
  (editor-move-to! e 6)
  (editor-move-to! e 22 #:extend? #t)                                   ; 选中 abcd1234efgh5678
  (define r (editor-secret-selection! e))
  (check-equal? (cdr r) "abcd1234efgh5678")
  (check-equal? (fold-kind (car r)) 'secret)
  (check-equal? (length (editor-chips e)) 1)
  (check-false (regexp-match? #rx"abcd1234" (editor-text e)))
  (check-regexp-match #rx"⟦secret:[0-9a-f]+⟧" (editor-flatten e))       ; 发出去的是占位符
  (check-false (editor-secret-selection! e))                            ; 没有选区
  (editor-command! e 'selectAll:)
  (check-false (editor-secret-selection! e))                            ; 选区里有 chip
  (editor-move-to! e 0)
  (editor-command! e 'undo:)
  (check-regexp-match #rx"abcd1234efgh5678" (editor-text e)))

(test-case "attachment chips: one atomic chip per file; sent text has nothing for it; label shows icon, name and size"
  (define e (make-editor))
  (editor-insert! e "see ")
  (define f (editor-attach! e 'image "screenshot 2026-10-25.png" 812345))
  (editor-insert! e " please")
  (check-true (media-fold? f))
  (check-equal? (fold-label f) "🖼 screenshot 2026-10-25.png · 793.3 KB")
  (check-equal? (editor-flatten e) "see  please")
  (editor-command! e 'deleteBackward:) (for ([i 6]) (editor-command! e 'deleteBackward:))   ; 删掉 " please"
  (editor-command! e 'deleteBackward:)                                                       ; 一次退格删掉整个 chip
  (check-equal? (editor-chips e) '())
  (check-equal? (editor-text e) "see "))
