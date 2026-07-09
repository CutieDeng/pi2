#lang tstring racket
;; tui-width-test.rkt — Unicode 显示宽度单测

(require
 rackunit
 (file "../src/tui/width.rkt")
) ; end require

(test-case "ascii width 1"
  (check-equal? (char-width #\a) 1)
  (check-equal? (char-width #\Z) 1)
  (check-equal? (char-width #\space) 1)
  (check-equal? (string-width "hello") 5)
) ; end test-case

(test-case "cjk width 2"
  (check-equal? (char-width #\中) 2)
  (check-equal? (char-width #\文) 2)
  (check-equal? (char-width #\あ) 2)          ; 平假名
  (check-equal? (char-width #\한) 2)          ; 谚文
  (check-equal? (string-width "中文") 4)
  (check-equal? (string-width "a中b") 4)
) ; end test-case

(test-case "fullwidth and emoji width 2"
  (check-equal? (char-width #\Ａ) 2)          ; 全角 A (U+FF21)
  (check-equal? (char-width (integer->char #x1F600)) 2)   ; 😀
  (check-equal? (char-width (integer->char #x1F680)) 2)   ; 🚀
) ; end test-case

(test-case "combining and zero-width are 0"
  (check-equal? (char-width (integer->char #x0301)) 0)    ; 组合锐音符
  (check-equal? (char-width (integer->char #x200B)) 0)    ; 零宽空格
  (check-equal? (char-width (integer->char #xFE0F)) 0)    ; 变体选择符
  ;; e + 组合符 = 1 列
  (check-equal? (string-width (string #\e (integer->char #x0301))) 1)
) ; end test-case

(test-case "control chars are 0"
  (check-equal? (char-width #\nul) 0)
  (check-equal? (char-width #\tab) 0)         ; 控制符本身 0（tab 展开由渲染层处理）
  (check-equal? (char-width (integer->char #x7F)) 0)
) ; end test-case

(test-case "string-width-upto for cursor positioning"
  (check-equal? (string-width-upto "中a文" 0) 0)
  (check-equal? (string-width-upto "中a文" 1) 2)
  (check-equal? (string-width-upto "中a文" 2) 3)
  (check-equal? (string-width-upto "中a文" 3) 5)
) ; end test-case

(displayln "tui-width-test: all passed")
