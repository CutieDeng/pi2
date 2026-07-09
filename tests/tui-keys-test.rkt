#lang tstring racket
;; tui-keys-test.rkt — 按键/转义序列解析单测

(require
 rackunit
 (file "../src/tui/keys.rkt")
) ; end require

;; 解析单个按键（从字节串取首个事件）
(define (one bs)
  (define ks (parse-keys bs))
  (if (pair? ks) (car ks) key-eof)
) ; end define one

(test-case "plain ascii chars"
  (check-equal? (one "a") (kchar #\a))
  (check-equal? (one "Z") (kchar #\Z))
  (check-equal? (one " ") (kchar #\space))
  (check-equal? (parse-keys "abc") (list (kchar #\a) (kchar #\b) (kchar #\c)))
) ; end test-case

(test-case "enter / tab / backspace"
  (check-equal? (one "\r") (knamed 'enter))
  (check-equal? (one "\n") (knamed 'enter))
  (check-equal? (one "\t") (knamed 'tab))
  (check-equal? (one (bytes 127)) (knamed 'backspace))
  (check-equal? (one (bytes 8)) (knamed 'backspace '(ctrl)))
) ; end test-case

(test-case "ctrl combos"
  (check-equal? (one (bytes 1)) (kchar #\a '(ctrl)))     ; Ctrl-A
  (check-equal? (one (bytes 5)) (kchar #\e '(ctrl)))     ; Ctrl-E
  (check-equal? (one (bytes 3)) (kchar #\c '(ctrl)))     ; Ctrl-C
  (check-equal? (one (bytes 11)) (kchar #\k '(ctrl)))    ; Ctrl-K
  (check-equal? (one (bytes 23)) (kchar #\w '(ctrl)))    ; Ctrl-W
) ; end test-case

(test-case "arrow keys (CSI)"
  (check-equal? (one "\e[A") (knamed 'up))
  (check-equal? (one "\e[B") (knamed 'down))
  (check-equal? (one "\e[C") (knamed 'right))
  (check-equal? (one "\e[D") (knamed 'left))
  (check-equal? (one "\e[H") (knamed 'home))
  (check-equal? (one "\e[F") (knamed 'end))
) ; end test-case

(test-case "home/end/del/pgup via tilde"
  (check-equal? (one "\e[1~") (knamed 'home))
  (check-equal? (one "\e[3~") (knamed 'delete))
  (check-equal? (one "\e[4~") (knamed 'end))
  (check-equal? (one "\e[5~") (knamed 'pgup))
  (check-equal? (one "\e[6~") (knamed 'pgdn))
) ; end test-case

(test-case "modified arrows (CSI 1;mod)"
  (check-equal? (one "\e[1;5C") (knamed 'right '(ctrl)))  ; Ctrl-Right
  (check-equal? (one "\e[1;5D") (knamed 'left '(ctrl)))   ; Ctrl-Left
  (check-equal? (one "\e[1;2C") (knamed 'right '(shift))) ; Shift-Right
  (check-equal? (one "\e[1;3C") (knamed 'right '(alt)))   ; Alt-Right
) ; end test-case

(test-case "SS3 sequences (application cursor mode)"
  (check-equal? (one "\eOA") (knamed 'up))
  (check-equal? (one "\eOF") (knamed 'end))
  (check-equal? (one "\eOP") (knamed 'f1))
) ; end test-case

(test-case "alt + char and bare escape"
  (check-equal? (one "\eb") (kchar #\b '(alt)))          ; Alt-B (word back)
  (check-equal? (one "\ef") (kchar #\f '(alt)))          ; Alt-F (word fwd)
  (check-equal? (one "\e") (knamed 'escape))             ; 孤立 ESC
  (check-equal? (one (bytes 27 127)) (knamed 'backspace '(alt)))  ; Alt-Backspace
) ; end test-case

(test-case "utf-8 multibyte chars"
  (check-equal? (one "中") (kchar #\中))
  (check-equal? (one "é") (kchar #\é))
  (check-equal? (parse-keys "a中b") (list (kchar #\a) (kchar #\中) (kchar #\b)))
  ;; emoji (4-byte)
  (check-equal? (one "😀") (kchar (integer->char #x1F600)))
) ; end test-case

(test-case "mixed realistic input stream"
  ;; 键入 "hi", 左移, 退格, 回车
  (define ks (parse-keys (bytes-append #"hi" #"\e[D" (bytes 127) #"\r")))
  (check-equal? ks (list (kchar #\h) (kchar #\i)
                         (knamed 'left) (knamed 'backspace) (knamed 'enter)))
) ; end test-case

(test-case "modifier predicates"
  (check-true (kev-ctrl? (kchar #\a '(ctrl))))
  (check-true (kev-alt? (kchar #\b '(alt))))
  (check-false (kev-ctrl? (kchar #\a)))
) ; end test-case

(test-case "Shift/Alt+Enter parse to enter with modifiers (multi-line)"
  ;; CSI-u（kitty 键协议）
  (define su (car (parse-keys #"\e[13;2u")))
  (check-eq? (kev-name su) 'enter)
  (check-true (kev-shift? su))
  ;; modifyOtherKeys 形式
  (define mk (car (parse-keys #"\e[27;2;13~")))
  (check-eq? (kev-name mk) 'enter)
  (check-true (kev-shift? mk))
  ;; Alt/Option+Enter（可移植回退）
  (define ae (car (parse-keys #"\e\r")))
  (check-eq? (kev-name ae) 'enter)
  (check-true (kev-alt? ae))
  ;; 普通 Enter 无修饰
  (define pe (car (parse-keys #"\r")))
  (check-eq? (kev-name pe) 'enter)
  (check-false (kev-shift? pe))
  (check-false (kev-alt? pe))
) ; end test-case

(displayln "tui-keys-test: all passed")
