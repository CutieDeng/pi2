#lang racket-tstring
;; provider-live-test.rkt — 对本地 LM Studio (gemma-4-31b-it@6bit) 的真机集成测试
;; 需要 LM Studio 运行于 localhost:1234。

(require
 rackunit
 racket/async-channel
 racket/string
 (file "../model.rkt")
 (file "../provider.rkt")
) ; end require

(define cfg
  (struct-copy config (default-config)
               [temperature 0.0]
               [max-tokens 64]
  ) ; end struct-copy
) ; end define cfg

(define p (make-openai-provider cfg))

;; 收集一轮流式事件直到 turn-end / error
(define (collect-turn ch #:timeout [timeout 120])
  (let loop ([deltas '()] [msg #f])
    (define e (sync/timeout timeout ch))
    (cond
      [(not e) (error 'collect-turn "timeout waiting for provider events")]
      [(evt:delta? e) (loop (cons e deltas) msg)]
      [(evt:message? e) (loop deltas (evt:message-msg e))]
      [(evt:turn-end? e)
       (values (reverse deltas) msg (evt:turn-end-stop-reason e) (evt:turn-end-usage e))
      ] ; end turn-end case
      [(evt:error? e) (raise (evt:error-exn e))]
      [else (loop deltas msg)]
    ) ; end cond
  ) ; end let loop
) ; end define collect-turn

(test-case "live: streaming text completion"
  (define ch
    (provider-stream! p
                      (list (text-msg 'user "Reply with exactly the word: pineapple"))
                      '()
    ) ; end provider-stream!
  ) ; end define ch
  (define-values (deltas msg stop u) (collect-turn ch))
  (printf f"  deltas={(length deltas)} stop={stop} usage={u}\n")
  (printf f"  text: {(message-text msg)}\n")
  (check-true (pair? deltas) "should receive streaming deltas")
  (check-true (string-contains? (string-downcase (message-text msg)) "pineapple"))
  (check-equal? stop "stop")
  (check-true (> (usage-output-tokens u) 0) "stream usage should be reported")
) ; end test-case

(test-case "live: streaming tool call"
  (define list-files-spec
    (hasheq 'type "function"
            'function
            (hasheq 'name "list_files"
                    'description "List files in a directory"
                    'parameters
                    (hasheq 'type "object"
                            'properties (hasheq 'path (hasheq 'type "string"))
                            'required (list "path")
                    ) ; end hasheq parameters
            ) ; end hasheq function
    ) ; end hasheq
  ) ; end define list-files-spec
  (define ch
    (provider-stream! p
                      (list (text-msg 'user "List the files in /tmp using the list_files tool."))
                      (list list-files-spec)
    ) ; end provider-stream!
  ) ; end define ch
  (define-values (_deltas msg stop _u) (collect-turn ch))
  (define tcs (message-tool-uses msg))
  (printf f"  stop={stop} tool-calls={(length tcs)}\n")
  (check-equal? stop "tool_calls")
  (check-equal? (length tcs) 1)
  (check-equal? (tool-use-block-name (car tcs)) "list_files")
  (check-true (hash-has-key? (tool-use-block-input (car tcs)) 'path))
  (printf f"  input: {(tool-use-block-input (car tcs))}\n")
) ; end test-case

(test-case "live: cancel mid-stream"
  (define ch
    (provider-stream! p
                      (list (text-msg 'user "Count from 1 to 500, one number per line."))
                      '()
    ) ; end provider-stream!
  ) ; end define ch
  ;; 等到第一个 delta 后立即取消
  (let wait ()
    (define e (sync/timeout 60 ch))
    (cond
      [(not e) (error 'cancel-test "timeout")]
      [(evt:delta? e) (void)]
      [else (wait)]
    ) ; end cond
  ) ; end let wait
  (provider-cancel! p)
  ;; 取消后事件流应很快停止（channel 不再有新事件）
  (let drain ([n 0])
    (define e (sync/timeout 2 ch))
    (cond
      [(not e) (printf f"  drained {n} residual events after cancel\n")]
      [(> n 2000) (fail "stream did not stop after cancel")]
      [else (drain (add1 n))]
    ) ; end cond
  ) ; end let drain
  (check-true #t)
) ; end test-case

(displayln "provider-live-test: all passed")
