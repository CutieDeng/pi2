#lang tstring racket
;; context.rkt — 上下文与 token 预算管理（design.md §4.5 / §5.5）
;; estimate: 本地启发式（CJK 加权），intmap memo；fit: 三级裁剪的前两级。
;; compact!（模型总结压缩）在 loop 之上实现，见 compact.rkt / M4。

(require
 racket/pvector
 racket/intmap
 racket/list
 racket/string
 racket/math
 racket/async-channel
 (file "model.rkt")
 (file "provider.rkt")
) ; end require

;; ------------------------------------------------------------ token 估算

;; 字符级启发式：ASCII 字母数字 ~4 chars/token；CJK ~1.5 chars/token；
;; 其余（空白/标点）~3 chars/token。误差配合 80% 预算边际吸收。
(define (estimate-string-tokens s)
  (define-values (ascii cjk other)
    (for/fold ([a 0] [c 0] [o 0]) ([ch (in-string s)])
      (define code (char->integer ch))
      (cond
        [(or (char-alphabetic? ch) (char-numeric? ch))
         (if (< code 128)
             (values (add1 a) c o)
             (values a (add1 c) o)          ; 非 ASCII 字母类按 CJK 计
         ) ; end if
        ] ; end alnum case
        [(>= code #x2E80) (values a (add1 c) o)]   ; CJK 及以上区段
        [else (values a c (add1 o))]
      ) ; end cond
    ) ; end for/fold
  ) ; end define-values
  (+ (quotient ascii 4)
     (exact-ceiling (* cjk 0.67))
     (quotient other 3)
     1
  ) ; end +
) ; end define estimate-string-tokens

(define (estimate-block-tokens b)
  (cond
    [(text-block? b) (estimate-string-tokens (text-block-text b))]
    [(thinking-block? b) (estimate-string-tokens (thinking-block-text b))]
    [(tool-use-block? b)
     (+ 8 (estimate-string-tokens (format "~a" (tool-use-block-input b))))
    ] ; end tool-use case
    [(tool-result-block? b)
     (define c (tool-result-block-content b))
     (+ 4 (estimate-string-tokens (if (string? c) c (format "~a" c))))
    ] ; end tool-result case
    [else 16]
  ) ; end cond
) ; end define estimate-block-tokens

(define (estimate-message-tokens m)
  (+ 6                                        ; role 与消息边界固定开销
     (for/sum ([b (in-list (message-blocks m))])
       (estimate-block-tokens b)
     ) ; end for/sum
  ) ; end +
) ; end define estimate-message-tokens

(define (estimate-tokens msgs)
  (for/sum ([m (in-list msgs)])
    (estimate-message-tokens m)
  ) ; end for/sum
) ; end define estimate-tokens

;; memo：history 不可变 → (index . message) 的估值终身有效。
;; cache = box of intmap: index -> tokens
(define (make-estimate-cache)
  (box intmap-empty)
) ; end define make-estimate-cache

(define (history-tokens hist cache)
  (for/sum ([i (in-range (pvector-length hist))])
    (define memo (unbox cache))
    (cond
      [(intmap-ref memo i #f) => values]
      [else
       (define v (estimate-message-tokens (pvector-ref hist i)))
       (set-box! cache (intmap-set memo i v))
       v
      ] ; end else
    ) ; end cond
  ) ; end for/sum
) ; end define history-tokens

;; ---------------------------------------------------------------- 裁剪

;; 真实用户轮的起点：role=user 且不含 tool-result（工具回填也是 user role）
(define (genuine-user-msg? m)
  (and (eq? (message-role m) 'user)
       (not (ormap tool-result-block? (message-blocks m)))
  ) ; end and
) ; end define genuine-user-msg?

;; history (pvector) -> 发送窗口 (list)
;; 级别1：< 80% 预算 → 透传。
;; 级别2：保留任务锚点（首条真实 user）+ 从真实用户轮边界起的尾部，
;;        锚点文本附加省略说明。
(define (context-fit hist cfg #:cache [cache (make-estimate-cache)])
  (define msgs (pvector->list hist))
  (define budget (config-context-budget cfg))
  (define total (history-tokens hist cache))
  (cond
    [(< total (* 0.8 budget)) msgs]
    [else
     (define tail-budget (* 0.6 budget))
     ;; 从尾部向前累积，记录最近一个不超预算的真实用户轮边界
     (define n (length msgs))
     (define rev (reverse msgs))
     (define cut-idx                       ; 窗口从此下标（含）开始
       (let loop ([rest rev] [acc 0] [i n] [best n])
         (cond
           [(null? rest) best]
           [else
            (define m (car rest))
            (define acc* (+ acc (estimate-message-tokens m)))
            (cond
              [(> acc* tail-budget) best]
              [(genuine-user-msg? m) (loop (cdr rest) acc* (sub1 i) (sub1 i))]
              [else (loop (cdr rest) acc* (sub1 i) best)]
            ) ; end cond
           ] ; end else
         ) ; end cond
       ) ; end let loop
     ) ; end define cut-idx
     (define suffix
       (if (>= cut-idx n)
           ;; 连最后一轮都放不下：硬取最后一个真实用户轮到结尾
           (let ([idx (or (for/last ([m (in-list msgs)] [i (in-naturals)]
                                     #:when (genuine-user-msg? m))
                            i
                          ) ; end for/last
                          0
                      ) ; end or
                 ]) ; end bindings
             (drop msgs idx)
           ) ; end let
           (drop msgs cut-idx)
       ) ; end if
     ) ; end define suffix
     (define anchor (findf genuine-user-msg? msgs))
     (define elided (- n (length suffix)))
     (if (and anchor (not (memq anchor suffix)) (> elided 0))
         (cons
          (text-msg 'user
                    f"{(message-text anchor)}\n\n[note: {elided} earlier messages elided to fit context]"
          ) ; end text-msg
          suffix
         ) ; end cons
         suffix
     ) ; end if
    ] ; end else
  ) ; end cond
) ; end define context-fit

;; ---------------------------------------------------------------- compact!

(define COMPACT-PROMPT
  (string-join
   '("Summarize the conversation so far into a compact state description that lets"
     "work continue. Preserve: the task goal, what has been done, key file paths,"
     "important findings, and any open questions. Be concise but complete."
    ) ; end list
   " "
  ) ; end string-join
) ; end define COMPACT-PROMPT

;; 收集一次非工具 provider 流的完整文本（compact 内部同步调用）
(define (provider-complete-text p msgs)
  (define ch (provider-stream! p msgs '()))
  (let loop ()
    (define e (sync ch))
    (cond
      [(evt:message? e) (message-text (evt:message-msg e))]
      [(evt:error? e) (raise (evt:error-exn e))]
      [(evt:turn-end? e) ""]                ; 无 message 兜底
      [else (loop)]
    ) ; end cond
  ) ; end let loop
) ; end define provider-complete-text

;; compact!：用模型把前部历史总结为一条 summary user 消息，替换被删段。
;; 保留最近 keep-recent 条消息；返回压缩后的 agent-state。
;; 需传入 provider（避免与 loop.rkt 的循环依赖）。
(define (compact! st p #:keep-recent [keep-recent 4])
  (define msgs (state-history-list st))
  (define n (length msgs))
  (cond
    [(<= n (add1 keep-recent)) st]          ; 太短，无需压缩
    [else
     (define split (- n keep-recent))
     (define head (take msgs split))
     (define tail (drop msgs split))
     (define summary-text
       (provider-complete-text p
         (append head
                 (list (text-msg 'user COMPACT-PROMPT))
         ) ; end append
       ) ; end provider-complete-text
     ) ; end define summary-text
     (define summary-msg
       (text-msg 'user f"[compacted context summary]\n{summary-text}")
     ) ; end define summary-msg
     (define new-hist
       (for/fold ([pv (pvector)]) ([m (in-list (cons summary-msg tail))])
         (pvector-cons-right pv m)
       ) ; end for/fold
     ) ; end define new-hist
     (struct-copy agent-state st [history new-hist])
    ] ; end else
  ) ; end cond
) ; end define compact!

;; ---------------------------------------------------------------- provide

(provide
 estimate-string-tokens
 estimate-message-tokens
 estimate-tokens
 make-estimate-cache
 history-tokens
 genuine-user-msg?
 context-fit
 compact!
 COMPACT-PROMPT
) ; end provide
