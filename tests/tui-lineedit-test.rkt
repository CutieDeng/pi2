#lang tstring racket
;; tui-lineedit-test.rkt — 行编辑器状态迁移与渲染单测（纯离线）

(require
 rackunit
 (file "../src/tui/keys.rkt")
 (file "../src/tui/lineedit.rkt")
) ; end require

;; 把一串字节（含转义序列）依次施加到编辑器，返回 (values st last-action)
(define (drive st0 bs)
  (for/fold ([st st0] [act 'edit]) ([k (in-list (parse-keys bs))])
    (define-values (st* a) (ledit-apply st k))
    (values st* a)
  ) ; end for/fold
) ; end define drive

(define (type bs) (let-values ([(st _a) (drive (make-ledit) bs)]) st))

;; ---------------------------------------------------------------- 插入与光标

(test-case "basic insertion"
  (define st (type "hello"))
  (check-equal? (ledit-value st) "hello")
  (check-equal? (ledit-cursor st) 5)
) ; end test-case

(test-case "cursor movement: left/right/home/end"
  (define st0 (type "abcde"))
  ;; 左移两次
  (define-values (st1 _1) (drive st0 "\e[D\e[D"))
  (check-equal? (ledit-cursor st1) 3)
  ;; 在中间插入
  (define-values (st2 _2) (drive st1 "X"))
  (check-equal? (ledit-value st2) "abcXde")
  (check-equal? (ledit-cursor st2) 4)
  ;; Home / End
  (define-values (st3 _3) (drive st2 (bytes 1)))     ; Ctrl-A
  (check-equal? (ledit-cursor st3) 0)
  (define-values (st4 _4) (drive st3 (bytes 5)))     ; Ctrl-E
  (check-equal? (ledit-cursor st4) 6)
) ; end test-case

(test-case "backspace and delete"
  (define-values (st1 _1) (drive (type "hello") (bytes 127)))   ; 退格
  (check-equal? (ledit-value st1) "hell")
  ;; 光标回到开头，Delete 删首字符
  (define-values (st2 _2) (drive st1 (bytes-append (bytes 1) #"\e[3~")))
  (check-equal? (ledit-value st2) "ell")
) ; end test-case

;; ---------------------------------------------------------------- kill / yank

(test-case "Ctrl-K kills to end, Ctrl-Y yanks"
  (define st0 (type "hello world"))
  ;; 光标移到第 6 位（"hello "后），Ctrl-K 杀掉 "world"
  (define-values (st1 _1) (drive st0 "\e[D\e[D\e[D\e[D\e[D"))  ; 左移 5 -> cursor=6
  (check-equal? (ledit-cursor st1) 6)
  (define-values (st2 _2) (drive st1 (bytes 11)))   ; Ctrl-K
  (check-equal? (ledit-value st2) "hello ")
  ;; Ctrl-A 回首，Ctrl-Y 粘贴
  (define-values (st3 _3) (drive st2 (bytes-append (bytes 1) (bytes 25))))  ; Ctrl-A, Ctrl-Y
  (check-equal? (ledit-value st3) "worldhello ")
) ; end test-case

(test-case "Ctrl-U kills to start"
  (define-values (st1 _1) (drive (type "hello world") (bytes 21)))   ; Ctrl-U at end
  (check-equal? (ledit-value st1) "")
) ; end test-case

(test-case "Ctrl-W kills previous word"
  (define-values (st1 _1) (drive (type "foo bar baz") (bytes 23)))   ; Ctrl-W
  (check-equal? (ledit-value st1) "foo bar ")
  (define-values (st2 _2) (drive st1 (bytes 23)))                     ; 再 Ctrl-W
  (check-equal? (ledit-value st2) "foo ")
) ; end test-case

(test-case "Alt-B / Alt-F word motion, Alt-D kill word forward"
  (define st0 (type "alpha beta gamma"))
  ;; Alt-B 三次退到词首
  (define-values (st1 _1) (drive st0 "\eb\eb"))
  (check-equal? (ledit-cursor st1) 6)                ; "beta" 词首
  ;; Alt-D 删掉 "beta"
  (define-values (st2 _2) (drive st1 "\ed"))
  (check-equal? (ledit-value st2) "alpha  gamma")
) ; end test-case

;; ---------------------------------------------------------------- Unicode

(test-case "unicode insertion and cursor by character"
  (define st (type "中a文"))
  (check-equal? (ledit-value st) "中a文")
  (check-equal? (ledit-cursor st) 3)                 ; 3 个字符
  ;; 退格删掉 "文"（一个字符，尽管显示 2 列）
  (define-values (st1 _1) (drive st (bytes 127)))
  (check-equal? (ledit-value st1) "中a")
) ; end test-case

(test-case "Ctrl-W on unicode words"
  (define-values (st1 _1) (drive (type "你好 世界") (bytes 23)))
  (check-equal? (ledit-value st1) "你好 ")
) ; end test-case

;; ---------------------------------------------------------------- 动作信号

(test-case "enter submits, Ctrl-C cancels, Ctrl-D on empty is eof"
  (define-values (_st1 a1) (drive (type "hi") "\r"))
  (check-equal? a1 'submit)
  (define-values (st2 a2) (drive (type "hi") (bytes 3)))    ; Ctrl-C
  (check-equal? a2 'cancel)
  (check-equal? (ledit-value st2) "")                        ; 取消清行
  (define-values (_st3 a3) (drive (make-ledit) (bytes 4)))  ; Ctrl-D on empty
  (check-equal? a3 'eof)
  (define-values (_st4 a4) (drive (make-ledit) (bytes 12))) ; Ctrl-L
  (check-equal? a4 'clear-screen)
) ; end test-case

;; ---------------------------------------------------------------- 历史

(test-case "history up/down navigation"
  (define st0 (make-ledit #:history '("third" "second" "first")))  ; 最新在前
  ;; 键入草稿
  (define-values (sd _d) (drive st0 "draft"))
  ;; Up -> "third"
  (define-values (s1 _1) (drive sd "\e[A"))
  (check-equal? (ledit-value s1) "third")
  ;; Up -> "second"
  (define-values (s2 _2) (drive s1 "\e[A"))
  (check-equal? (ledit-value s2) "second")
  ;; Down -> "third"
  (define-values (s3 _3) (drive s2 "\e[B"))
  (check-equal? (ledit-value s3) "third")
  ;; Down -> 回到草稿
  (define-values (s4 _4) (drive s3 "\e[B"))
  (check-equal? (ledit-value s4) "draft")
) ; end test-case

;; ---------------------------------------------------------------- 渲染

(test-case "render places cursor at correct display column"
  (define st (make-ledit #:text "ab"))
  (define out (ledit-render st "> "))
  ;; 含清行、prompt、文本、以及移到第 4 列（prompt=2 + text=2）
  (check-true (string-contains? out "\e[K"))
  (check-true (string-contains? out "> ab"))
  (check-true (string-contains? out "\e[4C"))
) ; end test-case

(test-case "render cursor column accounts for CJK width and ANSI prompt"
  ;; 光标在 "中" 之后（1 字符，2 列），带颜色 prompt（可见宽度 2）
  (define st0 (make-ledit #:text "中x"))
  (define-values (st _a) (ledit-apply st0 (kchar #\a '(ctrl))))  ; Ctrl-A 到行首
  (define-values (st2 _2) (ledit-apply st (knamed 'right)))       ; 右移过 "中"
  (define colored-prompt "\e[32m> \e[0m")                          ; 显示宽度 2
  (define out (ledit-render st2 colored-prompt))
  ;; target col = prompt(2) + width("中")(2) = 4
  (check-true (string-contains? out "\e[4C"))
) ; end test-case

(test-case "visible-width strips ANSI"
  (check-equal? (visible-width "\e[32m> \e[0m") 2)
  (check-equal? (visible-width "中文") 4)
) ; end test-case

(test-case "Shift+Enter inserts newline; multi-line value + row metrics"
  (define st
    (for/fold ([s (make-ledit)]) ([k (parse-keys (bytes-append #"ab" #"\e[13;2u" #"cd"))])
      (define-values (s* _a) (ledit-apply s k))
      s*))
  (check-equal? (ledit-value st) "ab\ncd")
  (check-equal? (ledit-line-count st) 2)
  (check-equal? (ledit-cursor-row st) 1)          ; 光标在第 2 行
  ;; 渲染含分行与末行光标定位
  (define r (ledit-render st "> "))
  (check-true (string-contains? r "> ab"))
  (check-true (string-contains? r "cd"))
) ; end test-case

(test-case "plain Enter submits, not newline"
  (define-values (s* a) (ledit-apply (make-ledit #:text "hi") (car (parse-keys #"\r"))))
  (check-eq? a 'submit)
) ; end test-case

(displayln "tui-lineedit-test: all passed")
