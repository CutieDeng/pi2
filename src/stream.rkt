#lang tstring racket
;; stream.rkt — SSE 流式解析 + OpenAI 兼容 accumulator（design.md §4.2 / §5.2）

(require
 json
 racket/string
 racket/intmap
 (file "model.rkt")
) ; end require

;; ---------------------------------------------------------------- SSE 泵

;; SSE 行协议：`event: <type>` / `data: <payload>` / 空行 dispatch。
;; OpenAI 兼容端点只用 data 行；`data: [DONE]` 以 ("done" . #f) 递给 sink。
;; sink : (-> string? (or/c jsexpr? #f) void?)
(define (sse-pump! in sink)
  (let loop ([evt-type "message"] [data-lines '()])
    (define line (read-line in 'any))
    (cond
      [(eof-object? line)
       (unless (null? data-lines)
         (dispatch-sse! evt-type data-lines sink)
       ) ; end unless
       (void)
      ] ; end eof case
      [(string=? line "")                                   ; 空行 = 事件结束
       (unless (null? data-lines)
         (dispatch-sse! evt-type data-lines sink)
       ) ; end unless
       (loop "message" '())
      ] ; end blank case
      [(string-prefix? line "event:")
       (loop (string-trim (substring line 6)) data-lines)
      ] ; end event case
      [(string-prefix? line "data:")
       (loop evt-type (cons (string-trim (substring line 5)) data-lines))
      ] ; end data case
      [(string-prefix? line ":") (loop evt-type data-lines)] ; SSE 注释
      [else (loop evt-type data-lines)]                     ; 容忍未知行
    ) ; end cond
  ) ; end let loop
) ; end define sse-pump!

(define (dispatch-sse! evt-type data-lines sink)
  (define payload (string-join (reverse data-lines) "\n"))
  (if (string=? payload "[DONE]")
      (sink "done" #f)
      (with-handlers ([exn:fail?
                       (lambda (e)
                         (log-warning f"sse: bad json payload: {(exn-message e)}")
                       ) ; end lambda
                      ]) ; end handlers
        (sink evt-type (string->jsexpr payload))
      ) ; end with-handlers
  ) ; end if
) ; end define dispatch-sse!

;; ------------------------------------------------- OpenAI chunk accumulator

;; 累积中的 tool call 槽
(struct tc-slot
  (id       ; string
   name     ; string
   args     ; output-string port — arguments 分片累积
  ) ; end fields
) ; end struct tc-slot

(struct accumulator
  (content        ; output-string port — 正文
   reasoning      ; output-string port — thinking/reasoning_content
   tool-calls     ; box of intmap: index -> tc-slot
   finish-reason  ; box of (or/c #f string)
   usage          ; box of usage
  ) ; end fields
) ; end struct accumulator

(define (make-accumulator)
  (accumulator (open-output-string)
               (open-output-string)
               (box intmap-empty)
               (box #f)
               (box usage-zero)
  ) ; end accumulator
) ; end define make-accumulator

;; json null 在 jsexpr 中是符号 'null（truthy）——统一归一为 dflt
(define (jsref j key [dflt #f])
  (define v (if (hash? j) (hash-ref j key dflt) dflt))
  (if (eq? v 'null) dflt v)
) ; end define jsref

(define (jslist v)
  (if (list? v) v '())
) ; end define jslist

(define (jsnum v dflt)
  (if (number? v) v dflt)
) ; end define jsnum

;; 喂一个 chat.completion.chunk；返回 (listof evt:delta) 供调用方转发渲染
(define (accumulator-feed! acc chunk)
  (define deltas '())
  (define choices (jsref chunk 'choices '()))
  (define choice (if (pair? choices) (car choices) #f))
  (when choice
    (define delta (jsref choice 'delta (hasheq)))
    ;; 正文增量
    (define ctext (jsref delta 'content))
    (when (and (string? ctext) (non-empty-string? ctext))
      (write-string ctext (accumulator-content acc))
      (set! deltas (cons (evt:delta (now-ms) 'text ctext) deltas))
    ) ; end when
    ;; reasoning 增量（LM Studio: reasoning_content）
    (define rtext (jsref delta 'reasoning_content))
    (when (and (string? rtext) (non-empty-string? rtext))
      (write-string rtext (accumulator-reasoning acc))
      (set! deltas (cons (evt:delta (now-ms) 'thinking rtext) deltas))
    ) ; end when
    ;; tool_calls 增量：按 index 寻址 intmap 槽
    (for ([tc (in-list (jslist (jsref delta 'tool_calls)))])
      (define idx (jsref tc 'index 0))
      (define fn (jsref tc 'function (hasheq)))
      (define m (unbox (accumulator-tool-calls acc)))
      (define slot
        (or (intmap-ref m idx #f)
            (tc-slot (or (jsref tc 'id) f"call_{idx}")
                     (or (jsref fn 'name) "")
                     (open-output-string)
            ) ; end tc-slot
        ) ; end or
      ) ; end define slot
      ;; id / name 可能迟到：以首个非空为准，args 持续累积
      (define slot*
        (tc-slot (if (jsref tc 'id) (jsref tc 'id) (tc-slot-id slot))
                 (if (and (jsref fn 'name) (non-empty-string? (or (jsref fn 'name) "")))
                     (jsref fn 'name)
                     (tc-slot-name slot)
                 ) ; end if
                 (tc-slot-args slot)
        ) ; end tc-slot
      ) ; end define slot*
      (define frag (jsref fn 'arguments))
      (when (string? frag)
        (write-string frag (tc-slot-args slot*))
        (set! deltas (cons (evt:delta (now-ms) 'tool-json frag) deltas))
      ) ; end when
      (set-box! (accumulator-tool-calls acc) (intmap-set m idx slot*))
    ) ; end for
    ;; finish_reason
    (define fr (jsref choice 'finish_reason))
    (when (string? fr)
      (set-box! (accumulator-finish-reason acc) fr)
    ) ; end when
  ) ; end when choice
  ;; usage（stream_options include_usage 的final chunk，或非流式响应）
  (define u (jsref chunk 'usage))
  (when (hash? u)
    (set-box! (accumulator-usage acc)
              (usage (jsnum (jsref u 'prompt_tokens 0) 0)
                     (jsnum (jsref u 'completion_tokens 0) 0)
              ) ; end usage
    ) ; end set-box!
  ) ; end when
  (reverse deltas)
) ; end define accumulator-feed!

;; 收尾：拼装完整 assistant message
(define (accumulator-finish acc)
  (define text (get-output-string (accumulator-content acc)))
  (define reasoning (get-output-string (accumulator-reasoning acc)))
  (define tcs
    (for/list ([kv (in-list (intmap-range->list (unbox (accumulator-tool-calls acc))))])
      (define slot (cdr kv))
      (define args-str (get-output-string (tc-slot-args slot)))
      (tool-use-block (tc-slot-id slot)
                      (tc-slot-name slot)
                      (with-handlers ([exn:fail? (lambda (_e) (hasheq '_raw args-str))])
                        (if (string=? args-str "")
                            (hasheq)
                            (string->jsexpr args-str)
                        ) ; end if
                      ) ; end with-handlers
      ) ; end tool-use-block
    ) ; end for/list
  ) ; end define tcs
  (define blocks
    (append
     (if (non-empty-string? reasoning) (list (thinking-block reasoning #f)) '())
     (if (non-empty-string? text) (list (text-block text)) '())
     tcs
    ) ; end append
  ) ; end define blocks
  (values
   (message 'assistant (if (null? blocks) (list (text-block "")) blocks))
   (or (unbox (accumulator-finish-reason acc)) "stop")
   (unbox (accumulator-usage acc))
  ) ; end values
) ; end define accumulator-finish

;; 处理非流式 chat.completion 响应（重试路径 / count 场景复用）
(define (completion->message resp)
  (define acc (make-accumulator))
  (define choices (jsref resp 'choices '()))
  (define msg (if (pair? choices) (jsref (car choices) 'message (hasheq)) (hasheq)))
  ;; 归一化成 chunk 形状复用 feed 逻辑
  (define pseudo-chunk
    (hasheq 'choices
            (list (hasheq 'delta
                          (hasheq 'content (or (jsref msg 'content) "")
                                  'reasoning_content (or (jsref msg 'reasoning_content) "")
                                  'tool_calls
                                  (for/list ([tc (in-list (jslist (jsref msg 'tool_calls)))]
                                             [i (in-naturals)])
                                    (hasheq 'index i
                                            'id (jsref tc 'id)
                                            'function (jsref tc 'function (hasheq))
                                    ) ; end hasheq
                                  ) ; end for/list
                          ) ; end hasheq delta
                          'finish_reason (if (pair? choices)
                                             (jsref (car choices) 'finish_reason)
                                             "stop"
                                         ) ; end if
                  ) ; end hasheq choice
            ) ; end list
            'usage (jsref resp 'usage)
    ) ; end hasheq
  ) ; end define pseudo-chunk
  (accumulator-feed! acc pseudo-chunk)
  (accumulator-finish acc)
) ; end define completion->message

;; ---------------------------------------------------------------- provide

(provide
 sse-pump!
 make-accumulator
 accumulator-feed!
 accumulator-finish
 completion->message
) ; end provide
