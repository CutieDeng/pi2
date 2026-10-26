#lang racket/base
;; hub.rkt — 组件接口与装配（计算层，design-gui.md §2）。
;;
;; 组件之间不互相 require，也不「认识」对方：
;;   接口   = 名字 + 方法名表（定义在计算层 interfaces.rkt，提供方与使用方共同依赖它）
;;   组件   = 名字 + needs（要用的接口）+ provides（提供的接口）+ make（构造）
;;   装配   = 拼装层把组件描述与配置交给 assemble：按 needs / provides 自动发现依赖、决定实例化顺序、注入实现
;; 组件在 make 里经 use 取得接口实现（只能取自己声明过的），之后像调用过程一样用：(impl 'method arg ...)。
;; 单元测试：instantiate 单个组件，只为它声明的 needs 提供假实现。

(require racket/list)

;; ---------------------------------------------------------------- 接口

(struct iface (name methods) #:transparent)

(define-syntax-rule (define-interface id method ...)
  (define id (iface 'id '(method ...))))

;; 接口实现：可直接调用 (impl 'method arg ...)
(struct impl (iface table)
  #:property prop:procedure
  (lambda (self m . args)
    (define f (hash-ref (impl-table self) m
                        (lambda () (error (iface-name (impl-iface self)) "no method ~a; interface has ~a"
                                          m (iface-methods (impl-iface self))))))
    (apply f args)))

;; (implement 接口 'm1 f1 'm2 f2 ...)：方法必须恰好覆盖接口声明（不多不少）
(define (implement ifc . kvs)
  (unless (even? (length kvs)) (error 'implement "~a: odd method/procedure list" (iface-name ifc)))
  (define table
    (let loop ([kvs kvs] [h (hasheq)])
      (cond
        [(null? kvs) h]
        [else
         (unless (procedure? (cadr kvs)) (error 'implement "~a: ~a is not a procedure" (iface-name ifc) (car kvs)))
         (loop (cddr kvs) (hash-set h (car kvs) (cadr kvs)))])))
  (define declared (iface-methods ifc))
  (define missing (filter (lambda (m) (not (hash-has-key? table m))) declared))
  (define extra (filter (lambda (m) (not (memq m declared))) (hash-keys table)))
  (unless (and (null? missing) (null? extra))
    (error 'implement "~a: missing ~a, extra ~a" (iface-name ifc) missing extra))
  (impl ifc table)
) ; end define implement

(define (impl-of? i ifc) (and (impl? i) (eq? (iface-name (impl-iface i)) (iface-name ifc))))

;; ---------------------------------------------------------------- 组件

;; make : (use config) → (listof impl)；use : iface → impl（只能取 needs 里声明过的）
(struct component (name needs provides make))

;; env：不可变 hasheq，接口名 → impl
(define (env-ref env ifc)
  (hash-ref env (iface-name ifc) (lambda () (error 'env-ref "no implementation of ~a" (iface-name ifc))))
) ; end define env-ref

;; 实例化单个组件：env 里必须有它声明的全部 needs；返回加入其 provides 后的新 env
(define (instantiate comp env #:config [config (hasheq)])
  (define name (component-name comp))
  (define missing (for/list ([n (in-list (component-needs comp))] #:unless (hash-has-key? env (iface-name n))) (iface-name n)))
  (unless (null? missing) (error 'instantiate "~a: missing needs ~a" name missing))
  (define (use ifc)
    (unless (memq ifc (component-needs comp))
      (error 'use "~a uses ~a without declaring it in needs" name (iface-name ifc)))
    (env-ref env ifc))
  (define impls ((component-make comp) use config))
  (define got (map (lambda (i) (iface-name (impl-iface i))) impls))
  (define want (map iface-name (component-provides comp)))
  (unless (equal? (sort got symbol<?) (sort want symbol<?))
    (error 'instantiate "~a: declared provides ~a but made ~a" name want got))
  (for/fold ([e env]) ([i (in-list impls)])
    (define k (iface-name (impl-iface i)))
    (when (hash-has-key? e k) (error 'instantiate "~a: interface ~a already provided" name k))
    (hash-set e k i))
) ; end define instantiate

;; 装配：comps 顺序任意，按 needs / provides 自动排序。env0：拼装层直接给出的接口（平台后端等）。
;; configs：组件名 → 配置 hasheq。返回 (values env 实例化顺序)。
(define (assemble comps #:env [env0 (hasheq)] #:config [configs (hasheq)])
  (let loop ([pending comps] [env env0] [order '()])
    (cond
      [(null? pending) (values env (reverse order))]
      [else
       (define ready
         (findf (lambda (c) (andmap (lambda (n) (hash-has-key? env (iface-name n))) (component-needs c))) pending))
       (unless ready
         (error 'assemble "unresolved needs: ~a"
                (for/list ([c (in-list pending)])
                  (cons (component-name c)
                        (for/list ([n (in-list (component-needs c))] #:unless (hash-has-key? env (iface-name n)))
                          (iface-name n))))))
       (loop (remq ready pending)
             (instantiate ready env #:config (hash-ref configs (component-name ready) (hasheq)))
             (cons (component-name ready) order))]))
) ; end define assemble

(provide (struct-out iface) define-interface implement impl? impl-of? impl-iface
         (struct-out component) env-ref instantiate assemble)
