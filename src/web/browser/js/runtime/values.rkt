#lang racket/base
;; browser/js/runtime/values.rkt — L6 · JS 对象模型核心（dict 模式）
;; 对标: ECMAScript 2023 §10.1 普通对象内部方法 + §6.1.7.1 描述符（design-jsobj.md §2/§3）
;; 职责: jso 值表示（dict 模式，shape/IC 后续）· 11 内部方法 · 描述符校验
;;       · 枚举顺序（§1.5 强约束）· 原型链 · 最小 native function（供访问器测试）
;; 存储（design-jsobj.md §2.2）: 三段分治，「索引」与「插入顺序」各用最优结构解耦——
;;   整数索引键 → intmap（值即序，天然升序 §10.1.11；own-keys 整数段免排序）
;;   字符串键   → swisstable 管键值(key→{seq,desc}) + intmap 管插入序(seq→key)
;;   symbol 键  → swisstable-eq 管键值 + intmap 管插入序
;; 序号 seq 单调递增（jso-seq）。查找走 swisstable（原生，实测 ~2x 于 hash）；
;; 枚举走 intmap-range->list（升序=插入序，免 reverse/tombstone）；删除经 swisstable 查
;; 到 seq 再从 intmap 移除，O(log n)——正对 dict 模式「频繁增删」的兜底负载。
;; 快字典 = 特性探测（design-chrome.md L0.3）：有 racket/swisstable 用之，stock 回退 hash。
;;   swisstable 是运行时既有通用设施，非为 JS 而加——不违背「不为 JS 给 host 开洞」。
;; 不做: shape/hidden-class/IC（B3）· 数组/String/Proxy 奇异（§5，后续文件）
;;       · 类型转换表（convert.rkt）· realm 内建根（realm.rkt）
;; 依赖: racket/base racket/list racket/intmap（swisstable 经 dynamic-require 门控，缺则回退）

(require racket/list racket/intmap)

;; 快字典门控：绑定 6 op（make-str/make-sym/ref/set!/remove!/has?）到 swisstable 或 hash
(define-values (fd-make-str fd-make-sym fd-ref fd-set! fd-remove! fd-has?)
  (with-handlers ([exn:fail? (lambda (_e)
                    (values make-hash make-hasheq hash-ref hash-set! hash-remove! hash-has-key?))])
    (define (dr s) (dynamic-require 'racket/swisstable s))
    (values (dr 'make-swisstable) (dr 'make-swisstable-eq)
            (dr 'swisstable-ref) (dr 'swisstable-set!)
            (dr 'swisstable-remove!) (dr 'swisstable-has-key?))))

;; ---------------------------------------------------------------- 原始值哨兵
(struct js-const (name))
(define the-undefined (js-const 'undefined))
(define the-null      (js-const 'null))
(define (js-undefined? v) (eq? v the-undefined))
(define (js-null? v)      (eq? v the-null))

;; Symbol：不透明 struct → equal? 退化为 eq?，故可直接做 hash 键（按身份）
(struct jsym (desc))
;; well-known symbols（跨 realm 共享单例，§6.1.5.1）
(define sym:iterator      (jsym "Symbol.iterator"))
(define sym:to-primitive  (jsym "Symbol.toPrimitive"))
(define sym:has-instance  (jsym "Symbol.hasInstance"))
(define sym:to-string-tag (jsym "Symbol.toStringTag"))

;; ---------------------------------------------------------------- 属性描述符
(struct prop  (value w e c) #:mutable)   ; 数据: value + writable/enumerable/configurable
(struct aprop (get set e c) #:mutable)   ; 访问器: get/set(js 值; undefined=缺) + e/c

(define (desc-configurable? d) (if (prop? d) (prop-c d) (aprop-c d)))
(define (desc-enumerable? d)   (if (prop? d) (prop-e d) (aprop-e d)))

;; ---------------------------------------------------------------- 对象（三段存储）
(struct pent ([seq #:mutable] [desc #:mutable]))   ; 键值项: 插入序号 + 描述符
;; ikeys: intmap 整数键→描述符（值即序）
;; stbl/sord: 字符串键 swisstable(key→pent) + intmap(seq→key)；ytbl/yord: symbol 同构
(struct jso ([proto #:mutable] exotic [ext? #:mutable]
             [ikeys #:mutable] stbl [sord #:mutable] ytbl [yord #:mutable]
             [seq #:mutable] [xdata #:mutable]))

(define (new-object [proto the-null])
  (jso proto #f #t intmap-empty (fd-make-str) intmap-empty (fd-make-sym) intmap-empty 0 #f))

;; 建带 exotic 标签的空对象（Array/String/…；exotic 字段不可变，须建时定）
(define (new-exotic exotic [proto the-null] [xdata #f])
  (jso proto exotic #t intmap-empty (fd-make-str) intmap-empty (fd-make-sym) intmap-empty 0 xdata))

(define (next-seq! o) (define s (jso-seq o)) (set-jso-seq! o (add1 s)) s)

;; 数组下标键: 规范数字串（无前导零），值 < 2^32-1（§10.1.11 整数索引判定）
(define (array-index? s)
  (and (string? s)
       (regexp-match? #px"^(0|[1-9][0-9]*)$" s)
       (< (string->number s) 4294967295)))

;; ---------------------------------------------------------------- 低层 dict（三段分派）
(define (own-desc o p)
  (cond [(jsym? p) (let ([e (fd-ref (jso-ytbl o) p #f)]) (and e (pent-desc e)))]
        [(array-index? p) (intmap-ref (jso-ikeys o) (string->number p) #f)]
        [else (let ([e (fd-ref (jso-stbl o) p #f)]) (and e (pent-desc e)))]))

;; 字符串/symbol 段通用增删（tbl: swisstable; ord-get/ord-set!: 该段的 intmap 存取器）
(define (seg-put! o p d tbl ord-get ord-set!)
  (define e (fd-ref tbl p #f))
  (if e
      (set-pent-desc! e d)                         ; 已存在：保序，仅替换描述符
      (let ([s (next-seq! o)])
        (fd-set! tbl p (pent s d))
        (ord-set! o (intmap-set (ord-get o) s p)))))
(define (seg-remove! o p tbl ord-get ord-set!)
  (define e (fd-ref tbl p #f))
  (when e
    (fd-remove! tbl p)
    (ord-set! o (intmap-remove (ord-get o) (pent-seq e)))))

(define (dict-put! o p d)
  (cond
    [(jsym? p) (seg-put! o p d (jso-ytbl o) jso-yord set-jso-yord!)]
    [(array-index? p) (set-jso-ikeys! o (intmap-set (jso-ikeys o) (string->number p) d))]
    [else (seg-put! o p d (jso-stbl o) jso-sord set-jso-sord!)]))

(define (dict-remove! o p)
  (cond
    [(jsym? p) (seg-remove! o p (jso-ytbl o) jso-yord set-jso-yord!)]
    [(array-index? p) (set-jso-ikeys! o (intmap-remove (jso-ikeys o) (string->number p)))]
    [else (seg-remove! o p (jso-stbl o) jso-sord set-jso-sord!)]))

;; OrdinaryOwnPropertyKeys §10.1.11: 整数键升序 → 字符串插入序 → symbol 插入序（三段各 range->list）
(define (js-own-keys o)
  (append
   (for/list ([kv (in-list (intmap-range->list (jso-ikeys o)))]) (number->string (car kv)))
   (for/list ([kv (in-list (intmap-range->list (jso-sord o)))]) (cdr kv))
   (for/list ([kv (in-list (intmap-range->list (jso-yord o)))]) (cdr kv))))

;; ---------------------------------------------------------------- SameValue（§7.2.11）
;; flonum: (eqv? +nan.0 +nan.0)=#t、(eqv? -0.0 0.0)=#f —— 与 SameValue 逐字一致
(define (same-value a b)
  (if (and (string? a) (string? b)) (string=? a b) (eqv? a b)))

;; ---------------------------------------------------------------- 可调用/可构造协议
;; 函数对象 exotic='function，xdata=callable。call: (this args→val)；
;; construct: #f(不可构造) | 'ordinary(默认 [[Construct]]，function.rkt 实现) | proc(自定义构造)
(struct callable (call construct))
(define (make-callable call construct [proto the-null])
  (jso proto 'function #t intmap-empty (fd-make-str) intmap-empty (fd-make-sym) intmap-empty 0
       (callable call construct)))
(define (make-native proc [proto the-null]) (make-callable proc #f proto))
(define (js-callable? v) (and (jso? v) (eq? (jso-exotic v) (quote function))))
(define (js-call f this args)
  (unless (js-callable? f) (error 'js-call "not a function"))
  ((callable-call (jso-xdata f)) this args))

;; ---------------------------------------------------------------- 内部方法（§10.1）

(define (js-get-proto o) (jso-proto o))

(define (proto-cycle? o v)   ; 沿 v 上溯遇到 o → 成环
  (let loop ([p v])
    (cond [(eq? p the-null) #f]
          [(eq? p o) #t]
          [(jso? p) (loop (jso-proto p))]
          [else #f])))

(define (js-set-proto! o v)   ; §10.1.2
  (cond [(same-value v (jso-proto o)) #t]
        [(not (jso-ext? o)) #f]
        [(proto-cycle? o v) #f]
        [else (set-jso-proto! o v) #t]))

(define (js-extensible? o)  (jso-ext? o))
(define (js-prevent-ext! o) (set-jso-ext?! o #f) #t)

;; OrdinaryGet §10.1.8.1
(define (js-get o p [receiver o])
  (define d (own-desc o p))
  (cond
    [(not d)
     (define par (jso-proto o))
     (if (eq? par the-null) the-undefined (js-get par p receiver))]
    [(prop? d) (prop-value d)]
    [else (let ([g (aprop-get d)])       ; 访问器：以 receiver 为 this
            (if (js-undefined? g) the-undefined (js-call g receiver '())))]))

(define (create-data-property o p v)   ; CreateDataProperty §7.3.5（三 true 数据属性）
  (cond [(not (jso-ext? o)) #f]
        [else (dict-put! o p (prop v #t #t #t)) #t]))

;; OrdinarySet / OrdinarySetWithOwnDescriptor §10.1.9
(define (js-set! o p v [receiver o])
  (define cur (own-desc o p))
  (cond
    [(not cur)
     (define par (jso-proto o))
     (cond [(not (eq? par the-null)) (js-set! par p v receiver)]   ; 委托原型找 setter
           [(jso? receiver) (create-data-property receiver p v)]
           [else #f])]
    [(prop? cur)
     (cond
       [(not (prop-w cur)) #f]
       [(not (jso? receiver)) #f]
       [else
        (define ex (own-desc receiver p))
        (cond [(and ex (aprop? ex)) #f]
              [(and ex (not (prop-w ex))) #f]
              [ex (set-prop-value! ex v) #t]
              [else (create-data-property receiver p v)])])]
    [else (let ([s (aprop-set cur)])     ; 访问器 setter
            (if (js-undefined? s) #f (begin (js-call s receiver (list v)) #t)))]))

(define (js-has? o p)   ; §10.1.7.1
  (cond [(own-desc o p) #t]
        [(eq? (jso-proto o) the-null) #f]
        [else (js-has? (jso-proto o) p)]))

(define (js-delete! o p)   ; §10.1.10
  (define d (own-desc o p))
  (cond [(not d) #t]
        [(desc-configurable? d) (dict-remove! o p) #t]
        [else #f]))

;; ---------------------------------------------------------------- DefineOwnProperty（§10.1.6，最硬）
;; Desc: immutable hash，可选键 'value 'w 'e 'c 'get 'set（缺=未指定）
(define (dhas? D k) (hash-has-key? D k))
(define (dget D k)  (hash-ref D k #f))
(define (dor D k default) (if (dhas? D k) (dget D k) default))
(define (accessor-desc? D) (or (dhas? D 'get) (dhas? D 'set)))
(define (data-desc? D)     (or (dhas? D 'value) (dhas? D 'w)))
(define (generic-desc? D)  (and (not (accessor-desc? D)) (not (data-desc? D))))

;; 奇异对象覆写 [[DefineOwnProperty]]（如 Array 的 length 耦合，§10.4.2）：
;; 按 exotic 标签注册（开放递归，避免 values 反向依赖 array.rkt）。
(define exotic-define-hooks (make-hasheq))
(define (register-exotic-define! tag proc) (hash-set! exotic-define-hooks tag proc))
(define (ordinary-define! o p D)   ; OrdinaryDefineOwnProperty §10.1.6.1
  (validate-apply! o p (jso-ext? o) D (own-desc o p)))
(define (js-define! o p D)
  (define h (and (jso-exotic o) (hash-ref exotic-define-hooks (jso-exotic o) #f)))
  (if h (h o p D) (ordinary-define! o p D)))

;; ValidateAndApplyPropertyDescriptor §10.1.6.3 —— 返回 #t=已应用 / #f=拒绝
(define (validate-apply! o p ext D current)
  (cond
    [(not current)                                   ; 新建
     (cond
       [(not ext) #f]
       [(accessor-desc? D)
        (dict-put! o p (aprop (dor D 'get the-undefined) (dor D 'set the-undefined)
                              (dor D 'e #f) (dor D 'c #f))) #t]
       [else
        (dict-put! o p (prop (dor D 'value the-undefined) (dor D 'w #f)
                             (dor D 'e #f) (dor D 'c #f))) #t])]
    [(zero? (hash-count D)) #t]                       ; 空描述符恒真
    [(not (desc-configurable? current))              ; 不可配 → 一串禁止
     (cond
       [(and (dhas? D 'c) (dget D 'c)) #f]            ; configurable:false→true 禁
       [(and (dhas? D 'e) (not (eq? (and (dget D 'e) #t) (desc-enumerable? current)))) #f]
       [(generic-desc? D) (apply-fields! o p current D) #t]
       [(not (eq? (aprop? current) (accessor-desc? D))) #f]   ; data↔accessor 互转禁
       [(prop? current)
        (cond
          [(not (prop-w current))                    ; 不可写数据
           (cond [(and (dhas? D 'w) (dget D 'w)) #f]  ; writable:false→true 禁
                 [(and (dhas? D 'value)
                       (not (same-value (dget D 'value) (prop-value current)))) #f]  ; 改值禁
                 [else (apply-fields! o p current D) #t])]
          [else (apply-fields! o p current D) #t])]   ; 可写数据：允许改值/降 writable
       [else                                          ; 访问器：不可改 get/set
        (cond [(and (dhas? D 'get) (not (same-value (dget D 'get) (aprop-get current)))) #f]
              [(and (dhas? D 'set) (not (same-value (dget D 'set) (aprop-set current)))) #f]
              [else (apply-fields! o p current D) #t])])]
    [else (apply-fields! o p current D) #t]))         ; 可配：合并（含互转）

(define (apply-fields! o p current D)
  (cond
    [(and (accessor-desc? D) (prop? current))         ; data → accessor
     (dict-put! o p (aprop (dor D 'get the-undefined) (dor D 'set the-undefined)
                           (if (dhas? D 'e) (dget D 'e) (prop-e current))
                           (if (dhas? D 'c) (dget D 'c) (prop-c current))))]
    [(and (data-desc? D) (aprop? current))            ; accessor → data
     (dict-put! o p (prop (dor D 'value the-undefined) (dor D 'w #f)
                          (if (dhas? D 'e) (dget D 'e) (aprop-e current))
                          (if (dhas? D 'c) (dget D 'c) (aprop-c current))))]
    [(prop? current)
     (when (dhas? D 'value) (set-prop-value! current (dget D 'value)))
     (when (dhas? D 'w) (set-prop-w! current (dget D 'w)))
     (when (dhas? D 'e) (set-prop-e! current (dget D 'e)))
     (when (dhas? D 'c) (set-prop-c! current (dget D 'c)))]
    [else
     (when (dhas? D 'get) (set-aprop-get! current (dget D 'get)))
     (when (dhas? D 'set) (set-aprop-set! current (dget D 'set)))
     (when (dhas? D 'e) (set-aprop-e! current (dget D 'e)))
     (when (dhas? D 'c) (set-aprop-c! current (dget D 'c)))]))

;; ---------------------------------------------------------------- provide
(provide
 the-undefined the-null js-undefined? js-null?
 (struct-out jsym)
 sym:iterator sym:to-primitive sym:has-instance sym:to-string-tag
 (struct-out prop) (struct-out aprop)
 desc-configurable? desc-enumerable?
 (struct-out jso) new-object new-exotic next-seq!
 own-desc array-index? same-value
 (struct-out callable) make-callable make-native js-callable? js-call
 js-get-proto js-set-proto! js-extensible? js-prevent-ext!
 js-get js-set! js-has? js-delete! js-own-keys
 js-define! ordinary-define! register-exotic-define! create-data-property)
