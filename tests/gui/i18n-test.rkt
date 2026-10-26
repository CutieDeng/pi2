#lang racket/base
;; tests/gui/i18n-test.rkt — 界面文字多语言：表完整、参数个数一致；英文界面下组件画出的文字里没有漏翻的中文

(require rackunit racket/list racket/string
         "../../src/gui/compute/i18n.rkt" "../../src/gui/compute/drawlist.rkt" "../../src/gui/compute/paint.rkt"
         "../../src/gui/compute/interfaces.rkt" "../../src/gui/compute/hub.rkt"
         "../../src/gui/components/sidebar.rkt" "../../src/gui/components/model-picker.rkt"
         "../../src/gui/components/input.rkt" "../../src/gui/components/transcript.rkt" "fakes.rkt")

(define (placeholders s) (length (regexp-match* #rx"~a" s)))

(test-case "every key has every language with the same number of ~a arguments"
  (for ([(k e) (in-hash CATALOG)])
    (for ([l (in-list LANGS)])
      (check-true (string? (hash-ref e l #f)) (format "~a missing ~a" k l)))
    (check-equal? (placeholders (hash-ref e 'en)) (placeholders (hash-ref e 'zh)) (format "~a: argument count differs" k))))

(test-case "lookup: current language, fallback to the key name; language tags"
  (parameterize ([current-lang 'en]) (check-equal? (t 'app.turns 3) "3 turn(s)"))
  (parameterize ([current-lang 'zh]) (check-equal? (t 'app.turns 3) "3 轮"))
  (check-equal? (t 'no.such.key) "no.such.key")
  (check-equal? (map lang-from-tag '("zh-Hans-CN" "en-CN" "zh_CN.UTF-8" "EN" "fr-FR" #f)) '(zh en zh en #f #f)))

;; 收集绘制的文字（paint.rkt 的 current-text-sink）
(define (painted-text thunk)
  (define acc '())
  (parameterize ([current-text-sink (lambda (str xs y h soft?) (set! acc (cons str acc)))])
    (thunk))
  (string-join (reverse acc) "\n"))

(define CJK #px"[一-鿿　-〿！-｠]")

(test-case "English UI: sidebar, model menu, input box and transcript draw no untranslated Chinese"
  (parameterize ([current-lang 'en])
    (define tl (make-fake-text-layout))
    (define sb (instantiate-with sidebar-component session-list #:text-layout tl))
    (define mp (instantiate-with model-picker-component model-picker #:text-layout tl))
    (define ib (instantiate-with input-component input-box #:text-layout tl))
    (define tr (instantiate-with transcript-component transcript-port #:text-layout tl))
    (sb 'set-sessions! (list (hasheq 'path "/a" 'title 'null 'ts "2026-10-23T10:00:00" 'messages 0 'model "m" 'current #t)
                             (hasheq 'path "/b" 'title "fix the loader" 'ts "2026-10-23T09:00:00" 'messages 4 'model "m" 'current #f)))
    (mp 'set-options! (hasheq 'current "lmstudio" 'model "gemma" 'reasoning "off" 'reasoning_levels '("off" "high")
                              'providers (list (hasheq 'name "lmstudio" 'models '("gemma") 'loaded '("gemma") 'has_key #t)
                                               (hasheq 'name "anthropic" 'models '("c") 'has_key #f))))
    (mp 'open!)
    ;; 会话记录投影的条目（RPC v3）：回答（推理 + 文字 + 工具调用）、工具结束、结果、用量、权限询问、提示、中断标记、压缩
    (define (it id kind . kvs) (apply hasheq 'type "item" 'id id 'kind kind kvs))
    (for ([ev (list (hasheq 'type "turn_start")
                    (it 1 "assistant" 'parts (list (hasheq 'type "thinking" 'text (string-join (for/list ([i 12]) "think") "\n"))
                                                   (hasheq 'type "text" 'text "Here is a long enough answer to get a copy link, more than sixty characters in total.")
                                                   (hasheq 'type "tool_call" 'id "x" 'name "bash" 'input (hasheq 'command "ls"))))
                    (it 2 "tool" 'call_id "x" 'name "bash" 'ms 3 'error #f 'source "builtin")
                    (it 3 "results" 'parts (list (hasheq 'type "tool_result" 'call_id "x" 'is_error #f 'content (string-join (for/list ([i 20]) "line") "\n")
                                                         'total_len 99999 'truncated #t)))
                    (it 4 "usage" 'model "m" 'input 1 'output 1 'cost 0.001)
                    (it 5 "permission" 'perm_id 1 'tool "bash" 'level "dangerous" 'input (hasheq 'command "rm x") 'prompt "")
                    (it 6 "note" 'level "error" 'code "error" 'text "boom")
                    (it 7 "partial" 'text "half" 'reasoning "")
                    (it 8 "user" 'parts (list (hasheq 'type "text" 'text "<turn_aborted>")) 'synthetic "aborted")
                    (it 9 "compact" 'count 3 'summary "s")
                    (it 10 "user" 'parts (list (hasheq 'type "text" 'text "Tool budget exhausted.")) 'synthetic "notice")
                    (hasheq 'type "turn_complete" 'turn 1 'interrupted #t))])
      (tr 'apply! ev))
    (define text
      (painted-text
       (lambda ()
         (define dl (make-drawlist))
         (sb 'render! dl 0.0 0.0 240.0 600.0 2.0)
         (sb 'context! 50.0 (+ 40.0 46.0 10.0))
         (sb 'render! dl 0.0 0.0 240.0 600.0 2.0)
         (mp 'render! dl 260.0 650.0 600.0 680.0 2.0)
         (ib 'render! dl 260.0 640.0 600.0 2.0 #t #t))))
    ;; 对话区自己设 current-text-sink（选择用）：经它的选择接口取画出的文字——全选
    (tr 'render! (make-drawlist) 260.0 10.0 600.0 2000.0 2.0)
    (tr 'select-start! 261.0 11.0)
    (tr 'select-extend! 859.0 2009.0)
    (tr 'render! (make-drawlist) 260.0 10.0 600.0 2000.0 2.0)
    (define tr-text (or (tr 'selection-text) ""))
    (check-true (> (string-length tr-text) 100) tr-text)
    (check-false (regexp-match CJK text) (format "untranslated text: ~s" (regexp-match CJK text)))
    (check-false (regexp-match CJK tr-text) (format "untranslated transcript text: ~s in ~s" (regexp-match CJK tr-text) tr-text))
    (set! text (string-append text "\n" tr-text))
    (check-true (string-contains? text "Delete (undoable)"))
    (check-true (string-contains? text "Message pi2"))
    (check-true (string-contains? text "Permission needed"))
    (check-true (string-contains? text "reasoning off"))))
