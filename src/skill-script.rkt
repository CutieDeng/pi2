#lang tstring racket
;; skill-script.rkt — skill.rktd 受控脚本：沙箱执行 + 分级结构化返回
;; 设计（据用户）：skill.rktd 内联 Racket 受控函数，参数一般是路径；在 racket/sandbox 里跑，
;;   只放行「声明为 reads 的路径参数」的读 IO，禁写/禁网/禁子进程，限内存/时间。
;;   返回不是裸字符串，而是分级对象（brief/summary/detail），供 LLM 按需取用（结果的渐进披露）。
;; 信任模型：脚本可信因为用户装在 ~/.pi2/skills（可信来源）；沙箱化因为它是通用运行时+限爆炸半径。
;; 与「受控执行平面」一脉：框架中介+沙箱执行，绝不把裸解释器交给 agent（同 cmdscan / 浏览器 cell）。

(require racket/sandbox (file "tool.rkt"))

;; skill.rktd 形状（datum）：
;;   (skill (name "commit") (desc "…")
;;     (fn <name> (params p...) (reads p...) (limits (memory MB) (time S))
;;         (body <top-level-form> ...)))    ; body 定义名为 <name> 的函数，参数即 params
;; body 里可调注入的 (result ok brief summary detail) 构造分级返回。

(struct skill-result (ok brief summary detail) #:transparent)
;; 发现分级（教给模型的 4 级）：group(L0 组) · brief(L1 一行) · whenuse(L2 卡片:何时用) · desc(L3 全文引子)。
;; 运行契约：prompt(驱动 agent) · writes(agent 写作用域) · fns/entry(native 函数 + 校验入口)。
(struct rktd-skill (name group brief whenuse desc prompt writes fns entry) #:transparent)
(struct fn-spec (name params reads mem-mb time-s body) #:transparent)

;; ---------------------------------------------------------------- 解析 skill.rktd
(define (kv sexps key [dflt #f])
  (cond [(assq key sexps) => cadr] [else dflt]))
(define (kv* sexps key) (let ([p (assq key sexps)]) (if p (cdr p) '())))

;; 软配置原则：缺字段一律走「自动安全缺省」（fail-closed）——
;;   writes 缺省 '() ⇒ agent 无写作用域；fn 缺 reads ⇒ 沙箱不放行任何读；缺 limits ⇒ 64MB/5s；
;;   缺 entry/prompt ⇒ 该 skill 惰性（不自动跑、不驱动 agent）。畸形的 fn/entry 条目跳过而非崩。
(define (parse-skill datum)
  (unless (and (pair? datum) (eq? (car datum) 'skill)) (error 'skill "not a (skill …) datum"))
  (define body (filter pair? (cdr datum)))            ; 容忍非列表噪声条目
  ;; 只收形如 (fn <sym> …) 的良构条目；畸形者静默跳过（软配置）
  (define fns
    (for/hash ([f (in-list body)]
               #:when (and (eq? (car f) 'fn) (>= (length f) 2) (symbol? (cadr f))))
      (let* ([name (cadr f)] [spec (cddr f)] [l (kv* spec 'limits)])
        (values name
                (fn-spec name (kv* spec 'params) (kv* spec 'reads)
                         (or (kv l 'memory) 64) (or (kv l 'time) 5)
                         (kv* spec 'body))))))
  ;; (entry <fn> (<default-arg> ...)) — 仅当良构（entry 名是符号且该 fn 存在）才生效
  (define entry
    (let ([e (assq 'entry body)])
      (and e (>= (length e) 2) (symbol? (cadr e)) (hash-has-key? fns (cadr e))
           (cons (cadr e) (if (>= (length e) 3) (caddr e) '())))))
  (define desc (kv body 'desc ""))
  (rktd-skill (kv body 'name "?") (kv body 'group "misc")
              (kv body 'brief desc) (kv body 'when-to-use "") desc
              (kv body 'prompt #f) (kv* body 'writes) fns entry))

(define (load-rktd-skill path)
  (parse-skill (call-with-input-file path read)))

;; 发现给定目录下的 .rktd skill：<dir>/*/skill.rktd → hash name→(cons skill skill-dir)
;; 软配置：坏 skill.rktd 只告警并跳过（绝不塞 #f 进表污染下游）；name 缺失时用目录名兜底。
(define (discover-rktd-skills dirs)
  (for*/fold ([h (hash)])
             ([d (in-list dirs)] #:when (directory-exists? d)
              [sub (in-list (directory-list d))]
              [f (in-value (build-path d sub "skill.rktd"))]
              #:when (file-exists? f))
    (define sk (with-handlers ([exn:fail? (lambda (e) (eprintf "[skill] 跳过 ~a：~a\n" f (exn-message e)) #f)])
                 (load-rktd-skill f)))
    (cond
      [(not sk) h]
      [else
       ;; name 缺失（"?"）→ 用目录名，避免多个无名 skill 互相覆盖
       (define nm (let ([n (rktd-skill-name sk)]) (if (equal? n "?") (path->string sub) n)))
       (hash-set h nm (cons (struct-copy rktd-skill sk [name nm])
                            (path->string (build-path d sub))))])))

;; 跑 skill 的 entry（$<skill> 用）：无覆盖 args 则用 entry 默认；返回 skill-result。
(define (run-entry skill #:workdir [wd (current-directory)] #:skill-dir [sd #f] #:args [override '()])
  (define e (rktd-skill-entry skill))
  (unless e (error 'skill "skill ~a has no entry" (rktd-skill-name skill)))
  (run-fn skill (car e) (if (pair? override) override (cdr e)) #:workdir wd #:skill-dir sd))

;; ---------------------------------------------------------------- 沙箱执行
;; args：与 params 一一对应的实参；reads 声明的参数（路径）自动获读权限。skill-dir 也放行读。
(define (run-fn skill fn-name args #:skill-dir [skill-dir #f] #:workdir [wd (current-directory)])
  (define fs (hash-ref (rktd-skill-fns skill) fn-name
                       (lambda () (error 'skill "no function ~a" fn-name))))
  (unless (= (length args) (length (fn-spec-params fs)))
    (error 'skill "~a expects ~a args, got ~a" fn-name (length (fn-spec-params fs)) (length args)))
  ;; 把「reads 声明的路径参数」解析成绝对路径 → 读权限白名单
  (define param->arg (map cons (fn-spec-params fs) args))
  (define read-paths
    (for/list ([p (in-list (fn-spec-reads fs))])
      (path->string (simplify-path (path->complete-path (cdr (assq p param->arg)) wd)))))
  (define perms
    (append (for/list ([p (in-list read-paths)]) (list 'read p))
            (if skill-dir (list (list 'read (path->string (path->complete-path skill-dir)))) '())))
  ;; 传给沙箱的实参：路径参数用其绝对路径（与读权限一致）
  (define call-args
    (for/list ([p (in-list (fn-spec-params fs))] [a (in-list args)])
      (if (memq p (fn-spec-reads fs))
          (path->string (simplify-path (path->complete-path a wd))) a)))
  (with-handlers ([exn:fail:resource? (lambda (_e) (skill-result #f "resource limit hit" "沙箱内存/时间预算超限" '()))]
                  [exn:fail? (lambda (e) (skill-result #f "script error"
                                                       (exn-message e) '()))])
    (define ev
      (parameterize ([sandbox-path-permissions perms]
                     [sandbox-memory-limit (fn-spec-mem-mb fs)]
                     [sandbox-eval-limits (list (fn-spec-time-s fs) (fn-spec-mem-mb fs))]
                     [sandbox-output 'string] [sandbox-error-output 'string])
        (make-evaluator 'racket/base)))
    ;; 注入 result 构造器（唯一注入的便利绑定；其余能力靠 security-guard 兜住）
    (ev '(define (result ok brief summary detail) (list '__skill-result ok brief summary detail)))
    (for ([form (in-list (fn-spec-body fs))]) (ev form))   ; 定义函数
    (define raw (ev (cons fn-name call-args)))              ; 调用
    (kill-evaluator ev)
    (normalize-result raw)))

;; 把 body 返回值归一成 skill-result（认 (result …) 形态；否则整体塞 detail）
(define (normalize-result raw)
  (cond
    [(and (list? raw) (= 5 (length raw)) (eq? (car raw) '__skill-result))
     (skill-result (cadr raw) (caddr raw) (cadddr raw) (list-ref raw 4))]
    [else (skill-result #t "ok" "" raw)]))

;; ---------------------------------------------------------------- 分级取用（结果的渐进披露）
;; 默认给 agent brief + ok；agent 需要时再取 summary / detail，省 token。
(define (result->brief r)   f"[{(if (skill-result-ok r) "ok" "fail")}] {(skill-result-brief r)}")
(define (result->summary r) (skill-result-summary r))
(define (result->detail r)  (skill-result-detail r))

;; ---------------------------------------------------------------- skill 函数 → agent 工具（「受控的洞」）
;; 把 skill 声明的 native 函数暴露成一个 read-only 级工具，agent 可主动调用来验证自己的产出。
;; 内部经 racket/sandbox 跑（只读声明路径、禁写/网/子进程）——故是「特定沙箱函数」，非任意 racket。
;; permission-level='read-only ⇒ read-only 模式也放行（危险的 `racket -e` 仍被 cmdscan 拒）。
(struct skill-tool (tname skill fn-name skill-dir)
  #:methods gen:tool
  [(define (tool-name t) (skill-tool-tname t))
   (define (tool-permission-level t) 'read-only)
   (define (tool-spec t)
     (define fs (hash-ref (rktd-skill-fns (skill-tool-skill t)) (skill-tool-fn-name t)))
     (function-spec
      (skill-tool-tname t)
      (format "Skill '~a' validator (sandboxed native function). Args are file paths; returns graded ok/brief/summary/detail."
              (rktd-skill-name (skill-tool-skill t)))
      (for/hasheq ([p (in-list (fn-spec-params fs))])
        (values (if (symbol? p) p (string->symbol (format "~a" p)))
                (hasheq 'type "string" 'description "file path")))
      (map (lambda (p) (format "~a" p)) (fn-spec-params fs))))
   (define (tool-run t input ctx)
     (define fs (hash-ref (rktd-skill-fns (skill-tool-skill t)) (skill-tool-fn-name t)))
     (define args (map (lambda (p) (input-str input (if (symbol? p) p (string->symbol (format "~a" p)))))
                       (fn-spec-params fs)))
     (cond
       [(memf not args) (err-outcome "missing required path argument(s)")]
       [else
        (define r (run-fn (skill-tool-skill t) (skill-tool-fn-name t) args
                          #:workdir (tool-ctx-workdir ctx) #:skill-dir (skill-tool-skill-dir t)))
        (define body (format "~a\n~a\ndetail: ~s" (result->brief r) (result->summary r) (result->detail r)))
        (if (skill-result-ok r) (ok-outcome body) (err-outcome body))]))])

;; 把 skill 的全部函数做成工具（工具名 <skill>_<fn>），供注册进 registry。
(define (skill->tools skill skill-dir)
  (for/list ([fn (in-list (hash-keys (rktd-skill-fns skill)))])
    (skill-tool (string-append (rktd-skill-name skill) "_" (format "~a" fn)) skill fn skill-dir)))

;; ---------------------------------------------------------------- 发现分级渲染（教给模型）
;; registry: name → (cons rktd-skill dir)（discover-rktd-skills 的产物）
;; L0 组索引：每组一行「group (N): $a $b …」——常驻系统提示词，极小，模型据此下钻。
(define (skill-groups-index registry)
  (define by-group (make-hash))
  (for ([(name v) (in-hash registry)])
    (define g (rktd-skill-group (car v)))
    (hash-update! by-group g (lambda (l) (cons name l)) '()))
  (for/list ([g (in-list (sort (hash-keys by-group) string<?))])
    (define names (sort (hash-ref by-group g) string<?))
    (format "~a (~a): ~a" g (length names)
            (string-join (map (lambda (n) (string-append "$" n)) names) " "))))

;; L1 组内列表：某组下 「$skill — brief」。
(define (skill-group-listing registry group)
  (for/list ([(name v) (in-hash registry)] #:when (string=? (rktd-skill-group (car v)) group))
    (format "$~a — ~a" name (rktd-skill-brief (car v)))))

;; L2 skill 卡片：何时用 + 输入 + 开的洞（工具）+ 全文位置。常常够 agent 决定与调用，不必读全文。
(define (skill-card registry name)
  (define v (hash-ref registry name #f))
  (cond
    [(not v) (format "no skill: ~a" name)]
    [else
     (define sk (car v)) (define dir (cdr v))
     (define tools (map (lambda (fn) (string-append name "_" (format "~a" fn))) (hash-keys (rktd-skill-fns sk))))
     (string-join
      (filter values
       (list (format "skill: ~a  [group: ~a]" name (rktd-skill-group sk))
             (format "  brief: ~a" (rktd-skill-brief sk))
             (and (non-empty-string? (rktd-skill-whenuse sk)) (format "  when: ~a" (rktd-skill-whenuse sk)))
             (and (rktd-skill-entry sk) (format "  run: $~a  (writes: ~a)" name (rktd-skill-writes sk)))
             (and (pair? tools) (format "  tools it opens: ~a" (string-join tools ", ")))
             (format "  full: ~a/skill.rktd" dir)))
      "\n")]))

(provide (struct-out skill-result) (struct-out rktd-skill) (struct-out fn-spec)
         parse-skill load-rktd-skill run-fn discover-rktd-skills run-entry
         skill->tools
         skill-groups-index skill-group-listing skill-card
         result->brief result->summary result->detail)
