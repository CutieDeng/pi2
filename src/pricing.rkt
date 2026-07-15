#lang tstring racket
;; pricing.rkt — 记费：按模型单价估算 token 开销（USD）。非侵入，独立于内核。
;;
;; token 用量（usage: input/output）内核已逐轮累计；本模块只把用量乘以「每百万 token 单价」
;; 得到**估算**美元开销，供 /usage、/cost、每轮页脚、RPC state 展示。
;;
;; 单价随供应商调价而变，故：
;;   * 内置 DEFAULT-PRICES 是**近似默认**（USD / 1M tokens），仅作估算起点；
;;   * 用户可在 {config-home}/pricing.rktd 覆盖/新增（hash: model名 → (list 输入价 输出价)）；
;;   * 未知模型（如本地 lmstudio/gemma 未列）→ estimate-cost 返回 #f，UI 显示 n/a（本地免费）。
;; 匹配：先精确命中 model 名，否则取「最长前缀命中」（如 "claude-sonnet-5-20xx" 命中 "claude-sonnet"）。

(require
 racket/string
 racket/list
 (file "model.rkt")                     ; usage 结构访问器
 (file "credentials.rkt")               ; config-home（复用配置根）
) ; end require

;; 每百万 token 单价（USD）。input / output 分列。
(struct model-price (input output) #:transparent)

;; 近似公开价（USD/1M）——请按实际账单在 pricing.rktd 覆盖。本地模型价 0。
(define DEFAULT-PRICES
  (hash
   ;; 本地：免费
   "gemma"                (model-price 0.0    0.0)
   "lmstudio"             (model-price 0.0    0.0)
   ;; DeepSeek（Anthropic 兼容线路）
   "deepseek-chat"        (model-price 0.28   0.42)
   "deepseek-reasoner"    (model-price 0.28   0.42)
   ;; Anthropic
   "claude-haiku"         (model-price 1.0    5.0)
   "claude-sonnet"        (model-price 3.0    15.0)
   "claude-opus"          (model-price 15.0   75.0)
   ;; OpenAI
   "gpt-5"                (model-price 1.25   10.0)
   "gpt-4o"               (model-price 2.5    10.0)
   ;; Google
   "gemini-2.0-flash"     (model-price 0.10   0.40)
   "gemini-1.5-pro"       (model-price 1.25   5.0)
   ;; xAI
   "grok-4"               (model-price 3.0    15.0)
   "grok"                 (model-price 3.0    15.0)
  ) ; end hash
) ; end define DEFAULT-PRICES

;; 从 {config-home}/pricing.rktd 载入覆盖表：hash model→(list in out)。缺/损 → 空。
(define (load-price-overrides)
  (define path (build-path (config-home) "pricing.rktd"))
  (cond
    [(not (file-exists? path)) (hash)]
    [else
     (with-handlers ([exn:fail? (lambda (_e) (hash))])
       (define v (call-with-input-file path read))
       (if (hash? v)
           (for/hash ([(k val) (in-hash v)]
                      #:when (and (string? k) (list? val) (= 2 (length val))
                                  (real? (first val)) (real? (second val))))
             (values k (model-price (first val) (second val))))
           (hash)))]
  ) ; end cond
) ; end define load-price-overrides

;; 合并价目表（覆盖优先）。每次查询重载覆盖文件——量小，且便于用户改价即生效。
(define (price-table) (for/fold ([h DEFAULT-PRICES]) ([(k v) (in-hash (load-price-overrides))])
                        (hash-set h k v)))

;; model 名 → model-price：精确命中优先，否则最长前缀命中；无 → #f。
(define (model-price-for model)
  (define table (price-table))
  (cond
    [(not (string? model)) #f]
    [(hash-ref table model #f) => values]
    [else
     (define hits (for/list ([k (in-hash-keys table)]
                             #:when (string-prefix? model k)) k))
     (and (pair? hits)
          (hash-ref table (argmax string-length hits)))]
  ) ; end cond
) ; end define model-price-for

;; 估算某 usage 在 model 下的美元开销；未知模型 → #f。
(define (estimate-cost model u)
  (define p (model-price-for model))
  (and p
       (+ (* (/ (usage-input-tokens u) 1000000.0) (model-price-input p))
          (* (/ (usage-output-tokens u) 1000000.0) (model-price-output p))))
) ; end define estimate-cost

;; 美元 → 展示串。<$0.01 用 6 位小数（微额可见），否则 4 位。
(define (format-cost d)
  (cond
    [(not (real? d)) "n/a"]
    [(< d 0.01) (string-append "$" (real->decimal-string d 6))]
    [else       (string-append "$" (real->decimal-string d 4))]
  ) ; end cond
) ; end define format-cost

;; 紧凑 token 数：1234 → "1.2k"，1200000 → "1.2M"。
(define (fmt-tok n)
  (cond
    [(>= n 1000000) (string-append (real->decimal-string (/ n 1000000.0) 1) "M")]
    [(>= n 1000)    (string-append (real->decimal-string (/ n 1000.0) 1) "k")]
    [else (number->string n)]
  ) ; end cond
) ; end define fmt-tok

;; 一行开销说明：model 已知 → "≈ $x  (in … @$a/M · out … @$b/M)"；未知 → 本地/未定价提示。
(define (cost-line model u)
  (define p (model-price-for model))
  (cond
    [(not p) f"cost — n/a (本地或未定价模型 {model}；可在 pricing.rktd 配置)"]
    [else
     (define c (estimate-cost model u))
     f"cost — ≈ {(format-cost c)}  (in {(fmt-tok (usage-input-tokens u))} @${(model-price-input p)}/M · out {(fmt-tok (usage-output-tokens u))} @${(model-price-output p)}/M)"]
  ) ; end cond
) ; end define cost-line

(provide
 (struct-out model-price)
 DEFAULT-PRICES
 model-price-for
 estimate-cost
 format-cost
 fmt-tok
 cost-line
) ; end provide
