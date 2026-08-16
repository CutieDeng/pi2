#lang racket/base
;; tests/browser/l6-values-test.rkt — L6 JS 对象模型核心（dict 模式）
;; 对标 ECMAScript §10.1；逐条验证内部方法、描述符校验、枚举顺序、原型链

(require rackunit racket/list
         (file "../../src/web/browser/js/runtime/values.rkt"))

(define (desc . kvs) (apply hasheq kvs))
;; 确定性打乱：后半反序拼前半 → 明显非升序，用于证明 intmap 与插入序无关
(define (shuffle-det lst)
  (define-values (a b) (split-at lst (quotient (length lst) 2)))
  (append (reverse b) a))

;; ------------------------------------------------------------ 基础读写
(test-case "数据属性：put/get；缺失键返回 undefined"
  (define o (new-object))
  (check-true (js-define! o "x" (desc 'value 1 'w #t 'e #t 'c #t)))
  (check-equal? (js-get o "x") 1)
  (check-true (js-undefined? (js-get o "missing"))))

;; ------------------------------------------------------------ 原型链
(test-case "js-get 沿原型链上溯；js-has? 含原型"
  (define proto (new-object))
  (js-define! proto "a" (desc 'value 'A 'e #t 'c #t))
  (define o (new-object proto))
  (js-define! o "b" (desc 'value 'B 'e #t 'c #t))
  (check-equal? (js-get o "a") 'A)           ; 从原型取
  (check-equal? (js-get o "b") 'B)
  (check-true (js-has? o "a"))
  (check-false (js-has? o "missing")))

;; ------------------------------------------------------------ 访问器 this 绑定
(test-case "访问器 getter/setter 以 Receiver 为 this（§10.1.8/10.1.9）"
  (define proto (new-object))
  ;; getter 返回 this.stored；setter 写 this.stored
  (define getter (make-native (lambda (this args) (js-get this "stored"))))
  (define setter (make-native (lambda (this args) (js-define! this "stored"
                                                     (desc 'value (car args) 'w #t 'e #t 'c #t))
                                       the-undefined)))
  (js-define! proto "prop" (desc 'get getter 'set setter 'e #t 'c #t))
  (define o (new-object proto))
  (js-set! o "prop" 42)                       ; 触发原型上的 setter，this=o
  (check-equal? (js-get o "prop") 42)         ; getter 读 o.stored
  ;; stored 落在 o 上（Receiver），不在 proto 上
  (check-not-false (own-desc o "stored"))
  (check-false (own-desc proto "stored")))

;; ------------------------------------------------------------ js-set! 的 Receiver 创建
(test-case "写不存在属性：到顶在 Receiver 上建三 true 数据属性"
  (define o (new-object))
  (check-true (js-set! o "y" 7))
  (define d (own-desc o "y"))
  (check-true (and (prop? d) (prop-w d) (prop-e d) (prop-c d)))
  (check-equal? (prop-value d) 7))

(test-case "写不可写数据属性静默失败（strict 报错在 L7）"
  (define o (new-object))
  (js-define! o "ro" (desc 'value 1 'w #f 'e #t 'c #t))
  (check-false (js-set! o "ro" 2))
  (check-equal? (js-get o "ro") 1))

;; ------------------------------------------------------------ DefineOwnProperty 守卫（§10.1.6.3）
(test-case "不可配属性：禁改 configurable/enumerable、禁 data↔accessor、禁改不可写值"
  (define o (new-object))
  (js-define! o "k" (desc 'value 1 'w #f 'e #f 'c #f))
  (check-false (js-define! o "k" (desc 'c #t)))                 ; c:false→true 禁
  (check-false (js-define! o "k" (desc 'e #t)))                 ; 改 enumerable 禁
  (check-false (js-define! o "k" (desc 'w #t)))                 ; w:false→true 禁
  (check-false (js-define! o "k" (desc 'value 2)))              ; 改不可写值 禁
  (check-false (js-define! o "k" (desc 'get (make-native (lambda (t a) 0))))) ; data→accessor 禁
  (check-true  (js-define! o "k" (desc 'value 1)))              ; 同值允许（no-op）
  (check-true  (js-define! o "k" (desc 'w #f)))                 ; 同 writable 允许
  (check-equal? (js-get o "k") 1))

(test-case "可写但不可配数据：允许改值、允许降 writable"
  (define o (new-object))
  (js-define! o "k" (desc 'value 1 'w #t 'e #t 'c #f))
  (check-true (js-define! o "k" (desc 'value 9)))               ; 可写 → 改值 OK
  (check-equal? (js-get o "k") 9)
  (check-true (js-define! o "k" (desc 'w #f)))                  ; writable true→false OK
  (check-false (js-define! o "k" (desc 'value 10))))            ; 之后不可写 → 改值禁

(test-case "可配属性：data↔accessor 互转"
  (define o (new-object))
  (js-define! o "k" (desc 'value 1 'w #t 'e #t 'c #t))
  (define g (make-native (lambda (t a) 'via-getter)))
  (check-true (js-define! o "k" (desc 'get g)))                 ; data → accessor
  (check-true (aprop? (own-desc o "k")))
  (check-equal? (js-get o "k") 'via-getter)
  (check-true (js-define! o "k" (desc 'value 5)))              ; accessor → data
  (check-true (prop? (own-desc o "k")))
  (check-equal? (js-get o "k") 5))

(test-case "新建属性默认值：defineProperty 缺省布尔为 false"
  (define o (new-object))
  (js-define! o "k" (desc 'value 1))                            ; 只给 value
  (define d (own-desc o "k"))
  (check-false (prop-w d)) (check-false (prop-e d)) (check-false (prop-c d)))

;; ------------------------------------------------------------ 枚举顺序（§10.1.11，强约束）
(test-case "own-keys 顺序：整数键升序 → 字符串插入序 → symbol 插入序"
  (define o (new-object))
  (define (d! k) (js-define! o k (desc 'value #t 'e #t 'c #t)))
  (d! "b") (d! "2") (d! "a") (d! "10") (d! "0") (d! "c")
  (define s1 (jsym 'sym1)) (define s2 (jsym 'sym2))
  (js-define! o s2 (desc 'value #t 'e #t 'c #t))
  (js-define! o s1 (desc 'value #t 'e #t 'c #t))
  (check-equal? (js-own-keys o)
                (list "0" "2" "10"        ; 整数键数值升序（非字典序！"10">"2"）
                      "b" "a" "c"          ; 字符串键插入序
                      s2 s1)))             ; symbol 键插入序

;; ------------------------------------------------------------ 三段存储（intmap+hash）专项
(test-case "大量整数键：intmap 升序，非字典序，插入序无关"
  (define o (new-object))
  ;; 乱序插入 0..999
  (for ([k (in-list (shuffle-det (build-list 1000 values)))])
    (js-define! o (number->string k) (desc 'value k 'e #t 'c #t)))
  (define keys (js-own-keys o))
  (check-equal? (length keys) 1000)
  (check-equal? (map string->number (list (car keys) (cadr keys) (caddr keys))) '(0 1 2))
  (check-equal? (string->number (last keys)) 999)
  ;; 整段严格升序
  (check-true (apply < (map string->number keys))))

(test-case "删后重加：字符串键回到末尾（新插入序）；整数键仍升序"
  (define o (new-object))
  (for ([k '("a" "b" "c")]) (js-define! o k (desc 'value #t 'e #t 'c #t)))
  (js-delete! o "a")
  (js-define! o "a" (desc 'value #t 'e #t 'c #t))     ; 重加 → 末尾
  (check-equal? (js-own-keys o) '("b" "c" "a"))
  ;; 整数键删后重加仍按数值,非插入序
  (define n (new-object))
  (js-define! n "5" (desc 'value #t 'e #t 'c #t))
  (js-define! n "1" (desc 'value #t 'e #t 'c #t))
  (js-delete! n "5")
  (js-define! n "5" (desc 'value #t 'e #t 'c #t))
  (check-equal? (js-own-keys n) '("1" "5")))

(test-case "intmap 管序 + swisstable 管值：大规模交错增删后顺序仍正确"
  (define o (new-object))
  ;; 加 str-0..str-499
  (for ([i (in-range 500)]) (js-define! o (format "str-~a" i) (desc 'value i 'e #t 'c #t)))
  ;; 删所有偶数号（O(log n) 删除路径压力）
  (for ([i (in-range 0 500 2)]) (js-delete! o (format "str-~a" i)))
  (define keys (js-own-keys o))
  (check-equal? (length keys) 250)
  ;; 剩奇数号，且保持插入序（1,3,5,...）
  (check-equal? (list (car keys) (cadr keys) (caddr keys)) '("str-1" "str-3" "str-5"))
  (check-equal? (last keys) "str-499")
  ;; 删掉的键确实不在
  (check-false (js-has? o "str-0"))
  (check-true (js-has? o "str-1"))
  ;; 重加一个被删的 → 追加到末尾（新 seq）
  (js-define! o "str-0" (desc 'value 'back 'e #t 'c #t))
  (check-equal? (last (js-own-keys o)) "str-0")
  (check-equal? (js-get o "str-0") 'back))

;; ------------------------------------------------------------ delete / extensibility
(test-case "delete：可配删除、不可配失败"
  (define o (new-object))
  (js-define! o "x" (desc 'value 1 'c #t))
  (js-define! o "y" (desc 'value 2 'c #f))
  (check-true (js-delete! o "x"))
  (check-false (own-desc o "x"))
  (check-false (js-delete! o "y"))
  (check-not-false (own-desc o "y"))
  (check-true (js-delete! o "absent")))     ; 删不存在 → #t

(test-case "preventExtensions：禁新属性；已有仍可改"
  (define o (new-object))
  (js-define! o "x" (desc 'value 1 'w #t 'e #t 'c #t))
  (js-prevent-ext! o)
  (check-false (js-extensible? o))
  (check-false (js-set! o "new" 1))          ; 新属性禁
  (check-false (own-desc o "new"))
  (check-true (js-set! o "x" 2))             ; 已有可写属性仍可改
  (check-equal? (js-get o "x") 2))

;; ------------------------------------------------------------ 原型环 / SameValue
(test-case "setPrototypeOf 防环；SameValue 的 -0/NaN 语义"
  (define a (new-object)) (define b (new-object a))
  (check-false (js-set-proto! a b))          ; a 的原型设为 b(其原型是 a) → 成环，拒绝
  (check-true  (js-set-proto! a the-null))
  (check-true  (same-value +nan.0 +nan.0))   ; NaN = NaN
  (check-false (same-value -0.0 0.0))        ; -0 ≠ +0
  (check-true  (same-value 1.0 1.0)))
