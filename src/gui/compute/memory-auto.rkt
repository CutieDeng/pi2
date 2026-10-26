#lang racket/base
;; memory-auto.rkt — 自动记忆（design-memory.md §2.2 生成 / M3）：纯函数。
;;   eligible-sessions   哪些会话该提取：消息数够（缺省 ≥ 6 = 约 3 轮）、空闲够久（缺省 2 小时）、不是临时会话、没关「写记忆」、
;;                       上次处理之后又多了 ≥ 6 条消息（或从没处理过）
;;   extract-prompt / parse-extract   第一次调用：从会话文字里提取候选（≤ 5 条，带建议作用域与证据）
;;   merge-prompt / parse-merge       第二次调用：候选与已有条目合并（新增 / 改写已有的 / 跳过）
;; 模型的回答形状不稳（本机 gemma 实测：有时带 ```json 围栏）→ 解析都容忍。

(require racket/list racket/string json)

(define MIN-MESSAGES 6)
(define MIN-NEW-MESSAGES 6)
(define MAX-CANDIDATES 5)

;; rows：(listof (hasheq 'path 'messages 'mtime 'ephemeral 'mem_write …))；processed：path → 处理时的消息数；now、idle-seconds：秒
;; → 合格的行，最近的在前
(define (eligible-sessions rows processed now idle-seconds)
  (sort
   (for/list ([r (in-list rows)]
              #:when (string? (hash-ref r 'path #f))
              #:when (not (eq? (hash-ref r 'ephemeral #f) #t))
              #:when (not (eq? (hash-ref r 'mem_write #t) #f))
              #:when (let ([m (hash-ref r 'messages 0)]) (and (real? m) (>= m MIN-MESSAGES)))
              #:when (let ([t (hash-ref r 'mtime #f)]) (and (real? t) (>= (- now t) idle-seconds)))
              #:when (let ([done (hash-ref processed (hash-ref r 'path) #f)]) (or (not done) (>= (- (hash-ref r 'messages 0) done) MIN-NEW-MESSAGES))))
     r)
   > #:key (lambda (r) (hash-ref r 'mtime 0))))

(define (fmt-chain chain)                                ; chain：(listof (hasheq 'id 'kind 'name))
  (string-join (for/list ([c (in-list chain)])
                 (case (hash-ref c 'kind "global")
                   [("global") "- global: about the user, applies to every session"]
                   [else (format "- ~a: ~a" (hash-ref c 'kind) (hash-ref c 'name "?"))]))
               "\n"))
(define (fmt-existing entries)                           ; entries：(listof (hasheq 'id 'kind 'text))
  (if (null? entries) "(none)"
      (string-join (for/list ([e (in-list entries)]) (format "- [~a] (~a) ~a" (hash-ref e 'id) (hash-ref e 'kind "?") (hash-ref e 'text ""))) "\n")))

(define (extract-prompt transcript chain existing)
  (string-append
   "From the conversation below, extract durable facts worth remembering for future sessions with this user:\n"
   "user preferences (language, style, tools), project conventions, build / test / run commands, environment facts, pitfalls and how they were fixed.\n"
   "Do NOT extract: task progress, temporary state, anything only true right now, secrets or keys, or facts already in the existing memories.\n"
   "Each fact is one or two self-contained sentences, in the conversation's language.\n"
   "Scope — where the fact applies:\n" (fmt-chain chain) "\n"
   "Existing memories:\n" (fmt-existing existing) "\n\n"
   (format "Reply with JSON only: {\"memories\": [{\"text\": \"…\", \"scope\": \"global|project|workspace\", \"evidence\": \"a short quote from the conversation\"}]} — at most ~a; an empty list is fine.\n\n" MAX-CANDIDATES)
   "Conversation:\n" transcript))

(define (json-in text)
  (define m (regexp-match #px"(?s:\\{.*\\})" (or text "")))
  (and m (with-handlers ([exn:fail? (lambda (_e) #f)]) (string->jsexpr (car m)))))
(define (str v [n 400]) (and (string? v) (let ([s (string-trim v)]) (and (not (string=? s "")) (if (> (string-length s) n) (substring s 0 n) s)))))
(define SCOPES '("global" "project" "workspace"))

;; → (listof (hasheq 'text 'scope 'evidence))，最多 MAX-CANDIDATES；坏的项丢掉；作用域不认识 → workspace
(define (parse-extract text)
  (define j (json-in text))
  (define items (and (hash? j) (hash-ref j 'memories #f)))
  (if (list? items)
      (for/list ([x (in-list items)] [_ (in-range MAX-CANDIDATES)]
                 #:when (and (hash? x) (str (hash-ref x 'text #f))))
        (hasheq 'text (str (hash-ref x 'text)) 'scope (let ([s (hash-ref x 'scope "workspace")]) (if (member s SCOPES) s "workspace"))
                'evidence (or (str (hash-ref x 'evidence #f) 200) "")))
      '()))

(define (merge-prompt candidates existing)
  (string-append
   "Merge new candidate memories into the existing memories. For each candidate decide one of:\n"
   "  add — a new fact;  update — it refines or corrects an existing memory (give that memory's id and the full new text);  skip — already covered or not worth keeping.\n"
   "Existing memories:\n" (fmt-existing existing) "\n\n"
   "Candidates:\n"
   (string-join (for/list ([c (in-list candidates)] [i (in-naturals)]) (format "~a. (~a) ~a" i (hash-ref c 'scope) (hash-ref c 'text))) "\n")
   "\n\nReply with JSON only: {\"ops\": [{\"candidate\": 0, \"op\": \"add\"}, {\"candidate\": 1, \"op\": \"update\", \"id\": \"m3\", \"text\": \"…\"}, {\"candidate\": 2, \"op\": \"skip\"}]}\n"))

;; → (listof (hasheq 'op "add"|"update" 'candidate i 'id? 'text?))；skip 与坏的项不返回；同一候选只取第一个
(define (parse-merge text n-candidates existing-ids)
  (define j (json-in text))
  (define ops (and (hash? j) (hash-ref j 'ops #f)))
  (define seen (make-hasheqv))
  (if (list? ops)
      (for/list ([o (in-list ops)]
                 #:when (hash? o)
                 #:when (let ([c (hash-ref o 'candidate #f)]) (and (exact-nonnegative-integer? c) (< c n-candidates) (not (hash-ref seen c #f))))
                 #:when (case (hash-ref o 'op #f)
                          [("add") #t]
                          [("update") (and (member (hash-ref o 'id #f) existing-ids) (str (hash-ref o 'text #f)) #t)]
                          [else #f]))
        (hash-set! seen (hash-ref o 'candidate) #t)
        (if (equal? (hash-ref o 'op) "add")
            (hasheq 'op "add" 'candidate (hash-ref o 'candidate))
            (hasheq 'op "update" 'candidate (hash-ref o 'candidate) 'id (hash-ref o 'id) 'text (str (hash-ref o 'text)))))
      ;; 合并那次没回成 JSON：全部当新增（宁多勿漏；用户可删）
      (for/list ([i (in-range n-candidates)]) (hasheq 'op "add" 'candidate i))))

(provide MIN-MESSAGES MIN-NEW-MESSAGES MAX-CANDIDATES eligible-sessions extract-prompt parse-extract merge-prompt parse-merge)
