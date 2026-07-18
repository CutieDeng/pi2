#lang tstring racket
;; repl-complete-test.rkt — '/' 命令 Tab 补全逻辑（离线）。

(require
 rackunit
 (file "../src/repl.rkt")
) ; end require

(test-case "unique prefix completes to the full command + space"
  (check-equal? (command-complete "/mo") "/model ")
  (check-equal? (command-complete "/q") "/quit ")
  (check-equal? (command-complete "/cl") "/clear ")
  (check-equal? (command-complete "/com") "/compact ")   ; /co 现与 /cost 分歧
  (check-equal? (command-complete "/cos") "/cost ")
  (check-equal? (command-complete "/he") "/help ")
  (check-equal? (command-complete "/hi") "/history ")
  (check-equal? (command-complete "/t") "/tail ")
  (check-equal? (command-complete "/res") "/resume ")
  (check-equal? (command-complete "/rea") "/reasoning ")
  (check-equal? (command-complete "/f") "/fallback ")     ; 唯一以 /f 起头
  (check-equal? (command-complete "/au") "/auto ")
  (check-equal? (command-complete "/e") "/escalate ")     ; 唯一以 /e 起头
) ; end test-case

(test-case "ambiguous prefix extends to common prefix, then stops"
  (check-equal? (command-complete "/r") "/re")   ; /resume、/reasoning 共享 "/re"
  (check-false (command-complete "/re"))    ; /resume vs /reasoning 在此分叉
  (check-false (command-complete "/h"))    ; /help、/history 仅共享 "/h"
  (check-false (command-complete "/c"))    ; /clear、/compact、/cost 仅共享 "/c"
  (check-false (command-complete "/co"))   ; /compact vs /cost 在此分叉
) ; end test-case

(test-case "already-complete / args-region / non-command → no completion"
  (check-false (command-complete "/model"))       ; 已是完整命令名
  (check-false (command-complete "/model gpt"))   ; 已进入参数区
  (check-false (command-complete "/model "))      ; 命令名后有空格
  (check-false (command-complete "hello"))        ; 非 '/' 起头
  (check-false (command-complete ""))             ; 空
) ; end test-case

(test-case "hint lines only appear for '/'-prefixed input"
  (check-true (pair? (command-hint-lines "/")))
  (check-equal? (command-hint-lines "hello") '())
  (check-true (ormap (lambda (l) (string-contains? l "/model")) (command-hint-lines "/mo")))
) ; end test-case

(displayln "repl-complete-test: all passed")
