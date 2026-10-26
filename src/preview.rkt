#lang tstring racket
;; preview.rkt — 长工具输出的折叠预览：让本机的小模型挑出最有信息的几行（design-content.md §8.5 决定：本地模型，输出不离开本机）。
;; 界面发 select_lines（src/rpc.rkt）→ 这里拼提示、调模型、解析行号。模型调用可注入（测试不碰真实 LM Studio）。

(require (file "model.rkt") (file "provider.rkt"))

(define DEFAULT-LOCAL-ENDPOINT "http://localhost:1234/v1")
(define MAX-HEAD-LINES 400)                            ; 交给模型的行数上限：前 400 行 + 后 100 行
(define MAX-TAIL-LINES 100)
(define MAX-LINE-CHARS 200)                           ; 每行截到这么长

(define SYSTEM
  (string-join
   '("You pick lines from a tool's output for a collapsed preview in a coding agent's UI."
     "Reply with only a JSON array of line numbers (as shown before the | sign), ascending, at most the requested count."
     "Prefer: errors, warnings, failed checks, final results, summaries and totals, the lines that answer what the command was run for."
     "Skip: progress bars, blank lines, repeated boilerplate.")
   " "))

;; → (values 消息表 发给模型的行号表)；行号从 1 起
(define (select-lines-messages tool summary output k)
  (define ls (string-split output "\n" #:trim? #f))
  (define n (length ls))
  (define numbered (for/list ([l (in-list ls)] [i (in-naturals 1)]) (cons i l)))
  (define shown (if (<= n (+ MAX-HEAD-LINES MAX-TAIL-LINES)) numbered
                    (append (take numbered MAX-HEAD-LINES) (take-right numbered MAX-TAIL-LINES))))
  (define body (string-join (for/list ([pr (in-list shown)])
                              (define l (cdr pr))
                              (format "~a| ~a" (car pr) (if (> (string-length l) MAX-LINE-CHARS) (string-append (substring l 0 MAX-LINE-CHARS) "…") l)))
                            "\n"))
  (values (list (text-msg 'system SYSTEM)
                (text-msg 'user (format "Tool: ~a\nInput: ~a\nOutput has ~a lines~a. Pick at most ~a lines.\n\n~a"
                                        tool summary n (if (< (length shown) n) (format " (lines ~a–~a omitted here)" (add1 MAX-HEAD-LINES) (- n MAX-TAIL-LINES)) "")
                                        k body)))
          (map car shown))
) ; end define select-lines-messages

;; 模型的回答 → 行号表（只要发给它的行里的、去重、升序、最多 k 个）；解析不出 → #f
(define (parse-line-numbers text allowed k)
  (define m (regexp-match #px"\\[([^\\]]*)\\]" text))
  (define nums (and m (filter-map string->number (regexp-match* #px"\\d+" (cadr m)))))
  (define ok (and nums (sort (remove-duplicates (filter (lambda (x) (memv x allowed)) nums)) <)))
  (and ok (pair? ok) (take ok (min k (length ok))))
) ; end define parse-line-numbers

;; 真实的本地模型调用：OpenAI 兼容端点（LM Studio），温度 0，非流式取全文。项目描述（host-index）要长一些：max-tokens / timeout 可调
(define (complete-local endpoint model msgs #:max-tokens [max-tokens 200] #:timeout [timeout 60])
  (define cfg (struct-copy config (default-config) [endpoint endpoint] [api-key #f] [model model] [max-tokens max-tokens] [temperature 0.0]))
  (define p (make-openai-provider cfg))
  (define ch (parameterize ([current-config cfg]) (provider-stream! p msgs '())))
  (let loop ()
    (define e (sync/timeout timeout ch))
    (cond [(not e) (provider-cancel! p) (error 'complete-local "local model did not answer in ~a s" timeout)]
          [(evt:message? e) (message-text (evt:message-msg e))]
          [(evt:error? e) (raise (evt:error-exn e))]
          [(evt:turn-end? e) ""]
          [else (loop)]))
) ; end define complete-local

;; → 行号表（1 起）；失败 raise
(define (select-lines tool summary output k #:endpoint [endpoint DEFAULT-LOCAL-ENDPOINT] #:model model #:complete [complete complete-local])
  (define-values (msgs allowed) (select-lines-messages tool summary output k))
  (define reply (complete endpoint model msgs))
  (or (parse-line-numbers reply allowed k)
      (error 'select-lines "could not read line numbers from the model's reply: ~a" (let ([s (string-trim reply)]) (if (> (string-length s) 80) (substring s 0 80) s))))
) ; end define select-lines

(provide select-lines select-lines-messages parse-line-numbers complete-local DEFAULT-LOCAL-ENDPOINT)
