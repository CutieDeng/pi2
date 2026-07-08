#lang racket-tstring
;; stream-test.rkt — SSE 解析与 accumulator 单测（离线）

(require
 rackunit
 json
 racket/string
 (file "../model.rkt")
 (file "../stream.rkt")
) ; end require

(define (sse-string . chunks)
  (string-append
   (apply string-append
          (for/list ([c (in-list chunks)])
            f"data: {(jsexpr->string c)}\n\n"
          ) ; end for/list
   ) ; end apply
   "data: [DONE]\n\n"
  ) ; end string-append
) ; end define sse-string

(define (chunk #:content [content #f] #:reasoning [reasoning #f]
               #:tool-calls [tcs #f] #:finish [finish #f] #:usage [u #f])
  (define delta
    (let* ([d (hasheq)]
           [d (if content (hash-set d 'content content) d)]
           [d (if reasoning (hash-set d 'reasoning_content reasoning) d)]
           [d (if tcs (hash-set d 'tool_calls tcs) d)])
      d
    ) ; end let*
  ) ; end define delta
  (define base
    (hasheq 'choices
            (list (hasheq 'index 0
                          'delta delta
                          'finish_reason (or finish 'null)
                  ) ; end hasheq
            ) ; end list
    ) ; end hasheq
  ) ; end define base
  (if u (hash-set base 'usage u) base)
) ; end define chunk

(test-case "sse-pump + accumulator: plain text stream"
  (define in
    (open-input-string
     (sse-string (chunk #:content "hello")
                 (chunk #:content " world")
                 (chunk #:finish "stop"
                        #:usage (hasheq 'prompt_tokens 5 'completion_tokens 2)
                 ) ; end chunk
     ) ; end sse-string
    ) ; end open-input-string
  ) ; end define in
  (define acc (make-accumulator))
  (define delta-texts (box '()))
  (sse-pump! in
    (lambda (_t data)
      (when data
        (for ([d (in-list (accumulator-feed! acc data))])
          (when (eq? (evt:delta-kind d) 'text)
            (set-box! delta-texts (cons (evt:delta-text d) (unbox delta-texts)))
          ) ; end when
        ) ; end for
      ) ; end when
    ) ; end lambda
  ) ; end sse-pump!
  (define-values (msg stop u) (accumulator-finish acc))
  (check-equal? (message-text msg) "hello world")
  (check-equal? (reverse (unbox delta-texts)) '("hello" " world"))
  (check-equal? stop "stop")
  (check-equal? u (usage 5 2))
) ; end test-case

(test-case "accumulator: fragmented tool call args via intmap index"
  (define acc (make-accumulator))
  (define (feed! c) (accumulator-feed! acc c))
  (feed! (chunk #:tool-calls
                (list (hasheq 'index 0 'id "call_a"
                              'function (hasheq 'name "bash" 'arguments "{\"comm")
                      ) ; end hasheq
                ) ; end list
         ) ; end chunk
  ) ; end feed!
  (feed! (chunk #:tool-calls
                (list (hasheq 'index 0
                              'function (hasheq 'arguments "and\":\"ls\"}")
                      ) ; end hasheq
                ) ; end list
         ) ; end chunk
  ) ; end feed!
  (feed! (chunk #:tool-calls
                (list (hasheq 'index 1 'id "call_b"
                              'function (hasheq 'name "read_file"
                                                'arguments "{\"path\":\"x\"}")
                      ) ; end hasheq
                ) ; end list
         ) ; end chunk
  ) ; end feed!
  (feed! (chunk #:finish "tool_calls"))
  (define-values (msg stop _u) (accumulator-finish acc))
  (check-equal? stop "tool_calls")
  (define tcs (message-tool-uses msg))
  (check-equal? (length tcs) 2)
  (check-equal? (tool-use-block-name (car tcs)) "bash")
  (check-equal? (tool-use-block-input (car tcs)) (hasheq 'command "ls"))
  (check-equal? (tool-use-block-name (cadr tcs)) "read_file")
) ; end test-case

(test-case "accumulator: reasoning_content becomes thinking-block"
  (define acc (make-accumulator))
  (accumulator-feed! acc (chunk #:reasoning "let me think"))
  (accumulator-feed! acc (chunk #:content "answer"))
  (define-values (msg _s _u) (accumulator-finish acc))
  (check-true (thinking-block? (car (message-blocks msg))))
  (check-equal? (message-text msg) "answer")
) ; end test-case

(test-case "sse-pump tolerates comments, event lines and bad json"
  (define in
    (open-input-string
     (string-append
      ": keepalive comment\n\n"
      "event: ping\ndata: {\"x\":1}\n\n"
      "data: {not json}\n\n"
      "data: [DONE]\n\n"
     ) ; end string-append
    ) ; end open-input-string
  ) ; end define in
  (define seen (box '()))
  (sse-pump! in
    (lambda (t data)
      (set-box! seen (cons (cons t data) (unbox seen)))
    ) ; end lambda
  ) ; end sse-pump!
  (define evs (reverse (unbox seen)))
  (check-equal? (length evs) 2)                  ; ping + done（坏 json 被丢弃）
  (check-equal? (caar evs) "ping")
  (check-equal? (car (cadr evs)) "done")
) ; end test-case

(displayln "stream-test: all passed")
