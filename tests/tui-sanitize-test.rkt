#lang tstring racket
;; tui-sanitize-test.rkt — 不可信文本消毒（终端转义注入防护）离线验证。

(require
 rackunit
 (file "../src/tui/sanitize.rkt")
) ; end require

(test-case "strips ESC-based ANSI/CSI sequences"
  ;; 模型若吐出颜色/光标转义，应被剥离（ESC 移除后残留可打印字符无害）
  (check-equal? (sanitize-untrusted "\e[31mhi\e[0m") "[31mhi[0m")
  (check-false (string-contains? (sanitize-untrusted "\e[2J\e[H") "\e"))
) ; end test-case

(test-case "strips OSC / title-setting and bell"
  (define evil "\e]0;pwned\a done")     ; OSC 设标题 + BEL
  (define clean (sanitize-untrusted evil))
  (check-false (string-contains? clean "\e"))
  (check-false (string-contains? clean "\a"))
  (check-true (string-contains? clean "done"))
) ; end test-case

(test-case "strips carriage return (prevents overwrite) and other C0"
  (check-false (string-contains? (sanitize-untrusted "a\rb") "\r"))
  (check-equal? (sanitize-untrusted "a\rb") "ab")
  (check-equal? (sanitize-untrusted (string #\a (integer->char 8) #\b)) "ab")  ; backspace
) ; end test-case

(test-case "strips C1 control block and DEL"
  (check-equal? (sanitize-untrusted (string #\x (integer->char #x9B) #\y)) "xy")  ; CSI(C1)
  (check-equal? (sanitize-untrusted (string #\x (integer->char 127) #\y)) "xy")   ; DEL
) ; end test-case

(test-case "preserves newlines, tabs and Unicode"
  (check-equal? (sanitize-untrusted "line1\nline2") "line1\nline2")
  (check-equal? (sanitize-untrusted "a\tb") "a\tb")
  (check-equal? (sanitize-untrusted "你好，世界 🌍") "你好，世界 🌍")
) ; end test-case

(test-case "safe-display-char? classification"
  (check-true (safe-display-char? #\A))
  (check-true (safe-display-char? #\newline))
  (check-true (safe-display-char? #\tab))
  (check-false (safe-display-char? (integer->char 27)))   ; ESC
  (check-false (safe-display-char? (integer->char 13)))   ; CR
  (check-true (safe-display-char? (integer->char #x4F60))); 你
) ; end test-case

(displayln "tui-sanitize-test: all passed")
