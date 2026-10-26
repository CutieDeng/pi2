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
    [(media-block? b) (if (eq? (media-block-kind b) 'image) 1000 32)]   ; 图片按 ~1000 token 估；其余投影成一行占位
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

;; ---------------------------------------------------------------- 压缩（三级；2026-10-25 重写）
;; 原来的 compact! 按「最近 4 条」切：切点会落在工具调用与它的结果之间（已验证：user / 调用 c1 / 结果 c1 / 调用 c2 / 结果 c2 / 回答 →
;; 新历史里结果 c1 没有调用；总结请求里调用 c1 没有结果、带工具块却没声明工具），总结请求失败又被吞掉（maybe-compact 的 with-handlers）。现在：
;;   1. prune-tool-outputs：不调模型——保护最近 PROTECT-FRAC × 预算的消息与当前这一轮，更早的大工具输出换成占位（开头几行 + 字数）
;;   2. compact!：只在安全切点切（真实用户消息 = 轮边界；退而求其次：紧跟在工具结果后面的 assistant 消息）；被总结的那段渲染成纯文字
;;      （没有工具块：不要求声明工具、不会有落单的调用），超过 SUMMARY-INPUT-FRAC × 预算就分段滚动总结；
;;      总结并进切点后的第一条用户消息（不出现两条相连的 user：本机模型的聊天模板——如 Gemma——要求角色交替）
;;   3. 失败不吞：调用方记提示（loop.rkt maybe-compact）

(define PRUNE-AT 0.7)                 ; 历史估算 > 0.7 × 预算：先删旧工具输出
(define COMPACT-AT 1.0)               ; 删完仍 > 1.0 × 预算：总结
(define PROTECT-FRAC 0.3)             ; 删工具输出时保护最近这么多（× 预算）
(define KEEP-FRAC 0.25)               ; 总结时原样保留的尾部（× 预算）
(define SUMMARY-INPUT-FRAC 0.5)       ; 每次总结请求的输入上限（× 预算）
(define PRUNE-MIN-CHARS 2000)         ; 小于这个的工具输出不换

(define COMPACT-PROMPT
  (string-join
   '("You are writing a handoff note so that work on this conversation can continue in a fresh context."
     "Summarize the conversation below. Preserve: the task goal and the user's requirements and constraints; decisions made and why;"
     "what has been done and what is in progress; key file paths, commands, identifiers and values; errors seen and how they were resolved;"
     "open questions and the next steps. Be concise but complete; do not invent anything.")
   " "))

(define (result-text c)
  (cond [(string? c) c]
        [(list? c) (string-join (for/list ([x (in-list c)]) (cond [(text-block? x) (text-block-text x)] [(string? x) x] [else "[media]"])) "\n")]
        [else (format "~a" c)]))
(define (clip s n) (if (> (string-length s) n) (string-append (substring s 0 n) (format " … [+~a chars]" (- (string-length s) n))) s))

;; ---- 1. 删旧工具输出
;; → (values 新消息表 换了几个 省了多少字)。保护：从尾往前累计到 protect-tokens 的消息，以及最后一条真实用户消息起的当前这一轮
(define (prune-tool-outputs msgs #:protect-tokens protect #:min-chars [mn PRUNE-MIN-CHARS])
  (define v (list->vector msgs))
  (define n (vector-length v))
  (define last-turn (or (for/last ([i (in-range n)] #:when (genuine-user-msg? (vector-ref v i))) i) n))
  (define first-protected
    (let loop ([i (sub1 n)] [acc 0])
      (cond [(< i 0) 0]
            [else (define acc* (+ acc (estimate-message-tokens (vector-ref v i))))
                  (if (> acc* protect) (add1 i) (loop (sub1 i) acc*))])))
  (define cut (min first-protected last-turn))
  (define names (for*/hash ([m (in-list msgs)] [b (in-list (message-blocks m))] #:when (tool-use-block? b)) (values (tool-use-block-id b) (tool-use-block-name b))))
  (define n-pruned 0) (define saved 0)
  (define out
    (for/list ([m (in-list msgs)] [i (in-naturals)])
      (cond
        [(>= i cut) m]
        [else
         (define bs (for/list ([b (in-list (message-blocks m))])
                      (cond
                        [(and (tool-result-block? b) (let ([t (result-text (tool-result-block-content b))]) (>= (string-length t) mn)))
                         (define t (result-text (tool-result-block-content b)))
                         (set! n-pruned (add1 n-pruned))
                         (define stub (format "[tool output trimmed to save context (~a, ~a characters). It began:\n~a\n… Run the tool again if you need it.]"
                                              (hash-ref names (tool-result-block-tool-use-id b) "tool") (string-length t)
                                              (substring t 0 (min 300 (string-length t)))))
                         (set! saved (+ saved (- (string-length t) (string-length stub))))
                         (tool-result-block (tool-result-block-tool-use-id b) stub (tool-result-block-is-error? b))]
                        [else b])))
         (if (andmap eq? bs (message-blocks m)) m (message (message-role m) bs))])))
  (values out n-pruned saved))

;; ---- 2. 总结
;; 安全切点：(listof (cons 下标 'turn|'step))，下标 ≥ 1。'turn：真实用户消息；'step：assistant 消息、前一条是 user（工具结果或用户话）——
;; pi2 的循环里每条带工具调用的 assistant 后面紧跟它的结果，所以在这两种地方切都不会拆开调用与结果
(define (split-points msgs)
  (define v (list->vector msgs))
  (for/list ([i (in-range 1 (vector-length v))]
             #:when (or (genuine-user-msg? (vector-ref v i))
                        (and (eq? (message-role (vector-ref v i)) 'assistant) (eq? (message-role (vector-ref v (sub1 i))) 'user))))
    (cons i (if (genuine-user-msg? (vector-ref v i)) 'turn 'step))))
;; 选切点：尾部 ≤ keep-tokens 的最早的轮边界；最后一轮自己就超了 → 这一轮里尾部 ≤ keep-tokens 的最早的 step；都不行 → 最后一个切点；没有 → #f
;; keep-recent（条数，兼容旧接口）：给了就按「最多留这么多条」选
(define (choose-split msgs #:keep-tokens keep #:keep-recent [keep-n #f])
  (define pts (split-points msgs))
  (define n (length msgs))
  (define suffix (let ([v (list->vector msgs)])                                    ; 下标 → 从它到尾的估算
                   (for/fold ([acc (hasheqv n 0)] [sum 0] #:result acc) ([i (in-range (sub1 n) -1 -1)])
                     (define s* (+ sum (estimate-message-tokens (vector-ref v i)))) (values (hash-set acc i s*) s*))))
  (define (fits? i) (if keep-n (<= (- n i) keep-n) (<= (hash-ref suffix i) keep)))
  (define turns (filter (lambda (p) (eq? (cdr p) 'turn)) pts))
  (define last-turn (and (pair? turns) (car (last turns))))
  (cond
    [(null? pts) #f]
    [(for/first ([p (in-list turns)] #:when (fits? (car p))) (car p)) => values]
    [(for/first ([p (in-list pts)] #:when (and (or (not last-turn) (>= (car p) last-turn)) (fits? (car p)))) (car p)) => values]
    [else (car (last pts))]))

;; 被总结的消息 → 纯文字记录（工具结果截短）
(define (transcript-lines msgs #:tool-chars [tc 600] #:msg-chars [mc 6000])
  (define names (for*/hash ([m (in-list msgs)] [b (in-list (message-blocks m))] #:when (tool-use-block? b)) (values (tool-use-block-id b) (tool-use-block-name b))))
  (append*
   (for/list ([m (in-list msgs)])
     (define role (if (eq? (message-role m) 'assistant) "ASSISTANT" "USER"))
     (for/list ([b (in-list (message-blocks m))]
                #:when (or (text-block? b) (tool-use-block? b) (tool-result-block? b)))
       (cond
         [(text-block? b) (format "~a: ~a" role (clip (text-block-text b) mc))]
         [(tool-use-block? b) (format "ASSISTANT called ~a ~a" (tool-use-block-name b) (clip (format "~s" (tool-use-block-input b)) 300))]
         [else (format "TOOL RESULT (~a~a): ~a" (hash-ref names (tool-result-block-tool-use-id b) "tool")
                       (if (tool-result-block-is-error? b) ", error" "") (clip (result-text (tool-result-block-content b)) tc))])))))
;; 行 → 段（每段 ≤ limit token）
(define (chunk-lines lines limit)
  (let loop ([ls lines] [cur '()] [cur-t 0] [acc '()])
    (cond
      [(null? ls) (reverse (if (null? cur) acc (cons (string-join (reverse cur) "\n") acc)))]
      [else
       (define t (estimate-string-tokens (car ls)))
       (if (and (pair? cur) (> (+ cur-t t) limit))
           (loop ls '() 0 (cons (string-join (reverse cur) "\n") acc))
           (loop (cdr ls) (cons (car ls) cur) (+ cur-t t) acc))])))

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

;; 分段滚动总结：每段一个请求（只有一条 user 文字消息）；后一段带上前面的总结
(define (summarize-lines p lines limit)
  (for/fold ([summary #f]) ([chunk (in-list (chunk-lines lines limit))])
    (define reply
      (provider-complete-text p (list (text-msg 'user (string-append COMPACT-PROMPT
                                                                     (if summary (string-append "\n\nSummary of the earlier part of the conversation:\n" summary) "")
                                                                     "\n\nConversation" (if summary " (continued)" "") ":\n" chunk)))))
    (when (string=? (string-trim reply) "") (error 'compact! "the model returned an empty summary"))
    reply))

;; compact!：在安全切点前面的那段 → 总结；总结并进尾部第一条（是用户消息就合进去，是 assistant 就放在它前面单独一条 user）。
;; 返回压缩后的 agent-state；不能压（没有切点）→ 原样返回。失败 → raise（调用方记提示）
(define (compact! st p #:keep-recent [keep-recent #f] #:keep-tokens [keep-tokens #f])
  (define msgs (state-history-list st))
  (define budget (config-context-budget (agent-state-config st)))
  (define split (choose-split msgs #:keep-tokens (or keep-tokens (* KEEP-FRAC budget)) #:keep-recent keep-recent))
  (cond
    [(or (not split) (<= split 0)) st]
    [else
     (define head (take msgs split))
     (define tail (drop msgs split))
     (define summary-text
       (parameterize ([current-config (agent-state-config st)])   ; 用 st 的 config（model 等）
         (summarize-lines p (transcript-lines head) (max 2000 (* SUMMARY-INPUT-FRAC budget)))))
     (define summary-block (text-block (string-append "[compacted context summary]\n" summary-text)))
     (define new-msgs
       (if (and (pair? tail) (genuine-user-msg? (car tail)))
           (cons (message 'user (cons summary-block (message-blocks (car tail)))) (cdr tail))   ; 合进去：角色交替
           (cons (message 'user (list summary-block)) tail)))
     (define new-hist (for/fold ([pv (pvector)]) ([m (in-list new-msgs)]) (pvector-cons-right pv m)))
     (record! 'compact (hasheq 'history new-msgs 'usage (agent-state-token-usage st)    ; 压缩后的历史落盘（原消息仍在会话文件里）
                               'kind "summary" 'summarized (length head)))
     (struct-copy agent-state st [history new-hist])]))

;; prune!：删旧工具输出（不调模型）→ (values 新 st 换了几个 省了多少字)；换了才落盘
(define (prune! st #:protect-tokens [protect #f])
  (define msgs (state-history-list st))
  (define budget (config-context-budget (agent-state-config st)))
  (define-values (out n saved) (prune-tool-outputs msgs #:protect-tokens (or protect (* PROTECT-FRAC budget))))
  (cond
    [(zero? n) (values st 0 0)]
    [else
     (record! 'compact (hasheq 'history out 'usage (agent-state-token-usage st) 'kind "prune" 'pruned n 'saved saved))
     (values (struct-copy agent-state st [history (for/fold ([pv (pvector)]) ([m (in-list out)]) (pvector-cons-right pv m))]) n saved)]))

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
 prune! prune-tool-outputs split-points choose-split transcript-lines chunk-lines
 PRUNE-AT COMPACT-AT PROTECT-FRAC KEEP-FRAC SUMMARY-INPUT-FRAC
 COMPACT-PROMPT
) ; end provide
