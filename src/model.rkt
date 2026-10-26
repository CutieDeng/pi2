#lang tstring racket
;; model.rkt — pi++ 核心数据模型（design.md §3）
;; 所有需持久化的 struct 一律 #:prefab，write/read 直接往返。

(require
 racket/pvector
 racket/list
 racket/string
) ; end require

;; ---------------------------------------------------------------- 内容块

(struct text-block (text) #:prefab)
(struct thinking-block (text signature) #:prefab)
(struct tool-use-block (id name input) #:prefab)               ; input : jsexpr
(struct tool-result-block (tool-use-id content is-error?) #:prefab)
(struct image-block (media-type data) #:prefab)
;; 媒体（design-content.md §3.3）：内容在 blob 存储里（按 sha256 寻址，src/media.rkt），记录里只有引用。
;; kind：'image | 'audio | 'file；name：文件名或 #f；meta：hasheq（bytes width height duration transcript …）
(struct media-block (kind media-type sha name meta) #:prefab)

(define (content-block? v)
  (or (text-block? v)
      (thinking-block? v)
      (tool-use-block? v)
      (tool-result-block? v)
      (image-block? v)
      (media-block? v)
  ) ; end or
) ; end define content-block?

;; ---------------------------------------------------------------- 消息

(struct message
  (role        ; 'user | 'assistant | 'system
   blocks      ; (listof content-block?)
  ) ; end fields
  #:prefab
) ; end struct message

(define (text-msg role s)
  (message role (list (text-block s)))
) ; end define text-msg

(define (message-tool-uses m)
  (filter tool-use-block? (message-blocks m))
) ; end define message-tool-uses

(define (message-text m)
  (string-join
   (for/list ([b (in-list (message-blocks m))]
              #:when (text-block? b))
     (text-block-text b)
   ) ; end for/list
   ""
  ) ; end string-join
) ; end define message-text

;; ---------------------------------------------------------------- 用量

(struct usage (input-tokens output-tokens) #:prefab)

(define usage-zero (usage 0 0))

(define (usage-add a b)
  (usage (+ (usage-input-tokens a) (usage-input-tokens b))
         (+ (usage-output-tokens a) (usage-output-tokens b))
  ) ; end usage
) ; end define usage-add

;; ---------------------------------------------------------------- 配置

(struct config
  (endpoint         ; string  — OpenAI 兼容 base url，如 "http://localhost:1234/v1"
   api-key          ; (or/c #f string)
   model            ; string
   max-tokens       ; exact-positive-integer
   temperature      ; real
   system-prompt    ; (or/c #f string)
   context-budget   ; exact-positive-integer — 发送窗口 token 预算
   turn-max-calls   ; exact-positive-integer — 单用户轮最大工具调用数
   permission-mode  ; 'strict | 'normal | 'yolo | 'auto（作用域自动批准）
   workdir          ; path-string
  ) ; end fields
  #:prefab
) ; end struct config

(define (default-config)
  (config "http://localhost:1234/v1"
          #f
          "gemma-4-31b-it@6bit"
          4096
          0.7
          #f
          8192
          16
          'normal
          (path->string (current-directory))
  ) ; end config
) ; end define default-config

;; 当前请求生效的 config：loop/compact 每次调用前 parameterize 之，provider 读取它取
;; model/endpoint/key（而非创建时闭包的旧 cfg）——故 /model 等运行时切换即时生效。
(define current-config (make-parameter #f))

;; 运行时推理强度：'default | 'off | 'low | 'medium | 'high | 'max。
;; 'default = 没人选过：按模型取（effective-reasoning-effort）——DeepSeek 服务端缺省就思考（官方文档：缺省开、effort high）→ 'high；
;; 其余 → 'off。选了 'off 就是真关：DeepSeek 发 thinking disabled（2026-10-26 实测：没有思考块）。界面显示解析后的档位。
;; **刻意不入 prefab config**（加字段会破坏旧 .rktd 重放，参照 provider 选用的做法），
;; **parameter**（非 box）——故 thread-local：主线程 `/reasoning`、`--reasoning`、RPC set_reasoning 直接设；
;; 并行 goal worker 各自 `(parameterize ([current-reasoning-effort …]) …)` 隔离，互不干扰(P4.2 v2)。
;; provider 每次请求读取：OpenAI 兼容 → reasoning_effort（'max 无对应，钳到 high）；
;; Anthropic → thinking budget（'max 给最大预算，供 DeepSeek 等 high/max 两档场景）。
(define current-reasoning-effort (make-parameter 'default))
(define (set-reasoning-effort! level) (current-reasoning-effort level))
(define (deepseek-model? m) (and (string? m) (regexp-match? #rx"^deepseek" m)))   ; deepseek-v4-flash / -v4-pro / -chat…
(define (effective-reasoning-effort [model (let ([c (current-config)]) (and c (config-model c)))] [eff (current-reasoning-effort)])
  (if (eq? eff 'default) (if (deepseek-model? model) 'high 'off) eff))
(define (valid-reasoning-effort? v) (and (memq v '(off low medium high max)) #t))

;; ---------------------------------------------------------------- Agent 状态

(struct agent-state
  (history       ; pvector of message
   pending       ; (listof tool-use-block?)
   turn-count    ; exact-nonnegative-integer
   token-usage   ; usage
   config        ; config
  ) ; end fields
  #:transparent
) ; end struct agent-state

(define (make-initial-state cfg)
  (agent-state (pvector) '() 0 usage-zero cfg)
) ; end define make-initial-state

(define (state-append st msg)
  (struct-copy agent-state st
               [history (pvector-cons-right (agent-state-history st) msg)]
  ) ; end struct-copy
) ; end define state-append

(define (state-add-usage st u)
  (struct-copy agent-state st
               [token-usage (usage-add (agent-state-token-usage st) u)]
  ) ; end struct-copy
) ; end define state-add-usage

(define (state-history-list st)
  (pvector->list (agent-state-history st))
) ; end define state-history-list

;; ---------------------------------------------------------------- 事件

(struct evt-base (timestamp) #:transparent)                     ; 毫秒
(struct evt:delta evt-base (kind text) #:transparent)           ; kind: 'text | 'thinking | 'tool-json
(struct evt:message evt-base (msg) #:transparent)
(struct evt:tool-start evt-base (block) #:transparent)
(struct evt:tool-end evt-base (block result ms) #:transparent)
(struct evt:turn-end evt-base (stop-reason usage) #:transparent)
(struct evt:error evt-base (exn recoverable?) #:transparent)

(define (now-ms)
  (current-inexact-milliseconds)
) ; end define now-ms

;; ---------------------------------------------------------------- provide

(provide
 (struct-out text-block)
 (struct-out thinking-block)
 (struct-out tool-use-block)
 (struct-out tool-result-block)
 (struct-out image-block)
 (struct-out media-block)
 content-block?
 (struct-out message)
 text-msg
 message-tool-uses
 message-text
 (struct-out usage)
 usage-zero
 usage-add
 (struct-out config)
 default-config
 current-config
 current-reasoning-effort set-reasoning-effort! valid-reasoning-effort? effective-reasoning-effort deepseek-model?
 (struct-out agent-state)
 make-initial-state
 state-append
 state-add-usage
 state-history-list
 (struct-out evt-base)
 (struct-out evt:delta)
 (struct-out evt:message)
 (struct-out evt:tool-start)
 (struct-out evt:tool-end)
 (struct-out evt:turn-end)
 (struct-out evt:error)
 now-ms
 current-record-sink record! current-session-id
) ; end provide

;; ---------------------------------------------------------------- 会话记录的去向
;; 内核在「历史真正变化的那一刻」把记录交给它：(kind payload)
;;   'msg 消息（追加进历史）| 'compact 压缩后的完整历史（摘要 + 保留的消息；清空 = '()）| 'turn 'begin / 'revert（轮开始 / 出错撤回）
;; 由前端（RPC / TUI）设成写会话文件；#f = 不记（测试、子 agent）
(define current-record-sink (make-parameter #f))
;; 当前会话的 id（会话文件名去掉 .rktd，如 "20261026-1050-9463"）：自动化提交的元数据 (pi2 种类 (session id)) 用；#f = 不知道
(define current-session-id (make-parameter #f))
(define (record! kind payload) (let ([s (current-record-sink)]) (when s (s kind payload))))
