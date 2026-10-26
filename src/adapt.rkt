#lang tstring racket
;; adapt.rkt — 增强适配（design-gui.md §15.2）：按模型 / 供应商 / 推理档匹配的「特调」规则，请求时改写 system prompt、参数、工具。
;; 用户感知：模型菜单里列出当前目标生效的规则，可逐条开关。规则只作用于发出去的请求，不改会话状态与存档。
;;
;; 规则来源（同名后者覆盖前者）：内置 → {config-home}/adapt.rktd → {config-home}/adapt-settings.rktd（设置页建的）→ {workdir}/.pi/adapt.rktd。
;; 设置页只改 adapt-settings.rktd（整文件重写）；手写的 adapt.rktd 不动（保留其中的注释）。
;; 文件里每条规则一个表，键值对形式：
;;   ((name "qwen3-no-think")
;;    (description "Qwen3：推理关闭时在 system prompt 末尾加 /no_think（模型卡里的软开关）")
;;    (model "qwen3")                 ; 正则（不分大小写），匹配模型名
;;    (provider "lmstudio")           ; 正则，匹配供应商 / 帐号名（可省）
;;    (reasoning off)                 ; 只在这些推理档生效（可省）
;;    (system-append "/no_think"))
;; 动作键：system-append / system-prepend / system-replace（文字）；system-into-user（#t：system prompt 并入第一条用户消息）；
;;   temperature / max-tokens（数）；tools-only（工具名…）；tool-description（工具名 文字）。
;; 开关状态：{config-home}/adapt-state.rktd（名 → #t/#f）；没记录的按规则自己的 enabled（缺省 #t）。

(require
 racket/file
 racket/list
 racket/string
 (file "model.rkt")
 (file "credentials.rkt")               ; config-home
) ; end require

;; ---------------------------------------------------------------- 规则

(struct rule (name description source props) #:transparent)   ; props：hasheq 键 → 值（已解析）

;; 内置规则：只放模型发布方文档里写明的用法（不臆造 prompt）
(define BUILTIN-RULES
  (list
   (rule "qwen3-no-think"
         "Qwen3：推理关闭时在 system prompt 末尾加 /no_think（Qwen3 模型卡的软开关，关闭思考输出）"
         'builtin
         (hasheq 'model "qwen3" 'reasoning '(off) 'system-append "/no_think"))
   (rule "deepseek-r1-usage"
         "DeepSeek-R1 系列：temperature 0.6，不用 system prompt、把它并入用户消息（DeepSeek-R1 模型卡的使用建议）"
         'builtin
         (hasheq 'model "deepseek-r1" 'temperature 0.6 'system-into-user #t))))

(define (parse-rule-datum d source)
  (and (list? d)
       (andmap (lambda (e) (and (pair? e) (symbol? (car e)))) d)
       (let* ([h (for/hasheq ([e (in-list d)])
                   (values (car e) (if (and (pair? (cdr e)) (null? (cddr e))) (cadr e) (cdr e))))]
              [name (hash-ref h 'name #f)])
         (and (string? name)
              (rule name (let ([s (hash-ref h 'description "")]) (if (string? s) s "")) source
                    (hash-remove (hash-remove h 'name) 'description)))))
) ; end define parse-rule-datum

(define (read-rules-file path source)
  (with-handlers ([exn:fail? (lambda (e) (log-warning f"adapt: cannot read {path}: {(exn-message e)}") '())])
    (if (file-exists? path)
        (call-with-input-file path
          (lambda (in)
            (parameterize ([read-accept-reader #f] [read-accept-lang #f])
              (let loop ([acc '()])
                (define d (read in))
                (if (eof-object? d) (reverse acc) (loop (let ([r (parse-rule-datum d source)]) (if r (cons r acc) acc))))))))
        '()))
) ; end define read-rules-file

(define (user-rules-path) (build-path (config-home) "adapt.rktd"))
(define (project-rules-path workdir) (build-path workdir ".pi" "adapt.rktd"))
(define (settings-rules-path) (build-path (config-home) "adapt-settings.rktd"))

;; 全部规则（同名后者覆盖前者，保持首次出现的顺序）
(define (all-rules [workdir #f])
  (define rs (append BUILTIN-RULES
                     (read-rules-file (user-rules-path) 'user)
                     (read-rules-file (settings-rules-path) 'settings)
                     (if workdir (read-rules-file (project-rules-path workdir) 'project) '())))
  (define last-by-name (for/fold ([h (hash)]) ([r (in-list rs)]) (hash-set h (rule-name r) r)))
  (for/list ([n (in-list (remove-duplicates (map rule-name rs)))]) (hash-ref last-by-name n))
) ; end define all-rules

;; ---------------------------------------------------------------- 开关

(define (state-path) (build-path (config-home) "adapt-state.rktd"))

(define (load-state)
  (with-handlers ([exn:fail? (lambda (_e) (hash))])
    (define v (if (file-exists? (state-path))
                  (call-with-input-file (state-path) (lambda (in) (parameterize ([read-accept-reader #f]) (read in))))
                  (hash)))
    (if (hash? v) v (hash)))
) ; end define load-state

(define (rule-enabled? r [st (load-state)])
  (hash-ref st (rule-name r) (lambda () (not (eq? (hash-ref (rule-props r) 'enabled #t) #f))))
) ; end define rule-enabled?

(define (set-rule-enabled! name on?)
  (make-directory* (config-home))
  (define st (hash-set (load-state) name (and on? #t)))
  (define tmp (path-add-extension (state-path) #".tmp"))
  (call-with-output-file tmp #:exists 'truncate (lambda (out) (write st out)))
  (rename-file-or-directory tmp (state-path) #t)
) ; end define set-rule-enabled!

;; ---------------------------------------------------------------- 匹配与应用

(define (rx-match? pat s)
  (and (string? pat) (string? s)
       (with-handlers ([exn:fail? (lambda (_e) #f)]) (regexp-match? (pregexp (string-append "(?i:" pat ")")) s)))
) ; end define rx-match?

(define (rule-matches? r provider-name model effort)
  (define p (rule-props r))
  (and (or (not (hash-ref p 'model #f)) (rx-match? (hash-ref p 'model) model))
       (or (not (hash-ref p 'provider #f)) (rx-match? (hash-ref p 'provider) provider-name))
       (let ([lv (hash-ref p 'reasoning #f)])
         (or (not lv) (memq effort (if (list? lv) lv (list lv))))))
) ; end define rule-matches?

;; 当前目标匹配到的规则（含关掉的，界面要显示）
(define (matching-rules provider-name model effort [workdir #f])
  (filter (lambda (r) (rule-matches? r provider-name model effort)) (all-rules workdir))
) ; end define matching-rules

(define (join-nonempty a b) (cond [(and a (not (string=? a ""))) (if (and b (not (string=? b ""))) (string-append a "\n\n" b) a)] [else b]))

(define (spec-name spec) (let ([f (hash-ref spec 'function spec)]) (hash-ref f 'name #f)))

;; 一条规则作用于 (cfg msgs tools)
(define (apply-rule r cfg msgs tools)
  (define p (rule-props r))
  (define (get k) (hash-ref p k #f))
  (define sys0 (config-system-prompt cfg))
  (define sys1 (cond [(string? (get 'system-replace)) (get 'system-replace)]
                     [else (join-nonempty (join-nonempty (and (string? (get 'system-prepend)) (get 'system-prepend)) sys0)
                                          (and (string? (get 'system-append)) (get 'system-append)))]))
  ;; system prompt 并入第一条用户消息（R1 的建议）
  (define-values (sys2 msgs2)
    (cond
      [(and (get 'system-into-user) (string? sys1) (not (string=? sys1 "")))
       (define i (index-where msgs (lambda (m) (and (eq? (message-role m) 'user) (ormap text-block? (message-blocks m))))))
       (values #f (if i
                      (list-update msgs i (lambda (m) (struct-copy message m [blocks (cons (text-block (string-append sys1 "\n\n")) (message-blocks m))])))
                      msgs))]
      [else (values sys1 msgs)]))
  (define cfg2 (struct-copy config cfg
                            [system-prompt sys2]
                            [temperature (if (real? (get 'temperature)) (get 'temperature) (config-temperature cfg))]
                            [max-tokens (if (exact-positive-integer? (get 'max-tokens)) (get 'max-tokens) (config-max-tokens cfg))]))
  (define only (let ([o (get 'tools-only)]) (cond [(list? o) o] [(or (string? o) (symbol? o)) (list o)] [else #f])))   ; 单个名字也行
  (define tools2 (if (list? only) (filter (lambda (s) (member (spec-name s) (map (lambda (x) (format "~a" x)) only))) tools) tools))
  (define td (get 'tool-description))
  (define tools3
    (if (and (list? td) (= (length td) 2))
        (for/list ([s (in-list tools2)])
          (if (equal? (spec-name s) (format "~a" (car td)))
              (if (hash-has-key? s 'function)
                  (hash-set s 'function (hash-set (hash-ref s 'function) 'description (cadr td)))
                  (hash-set s 'description (cadr td)))
              s))
        tools2))
  (values cfg2 msgs2 tools3)
) ; end define apply-rule

;; 请求前调用：→ (values cfg msgs tools 生效的规则名表)
(define (apply-adaptations provider-name cfg msgs tools [effort (current-reasoning-effort)])
  (define st (load-state))
  (for/fold ([cfg cfg] [msgs msgs] [tools tools] [applied '()]
             #:result (values cfg msgs tools (reverse applied)))
            ([r (in-list (matching-rules provider-name (config-model cfg) (effective-reasoning-effort (config-model cfg) effort) (config-workdir cfg)))]
             #:when (rule-enabled? r st))
    (define-values (c m t) (apply-rule r cfg msgs tools))
    (values c m t (cons (rule-name r) applied)))
) ; end define apply-adaptations

;; 界面描述
(define (rules->jsexpr provider-name model effort workdir)
  (define st (load-state))
  (for/list ([r (in-list (matching-rules provider-name model effort workdir))])
    (hasheq 'name (rule-name r) 'description (rule-description r) 'source (symbol->string (rule-source r))
            'enabled (rule-enabled? r st)))
) ; end define rules->jsexpr

;; ---------------------------------------------------------------- 设置页建的规则

;; 允许的键（与文件格式同名）；值已校验
(define SETTINGS-RULE-KEYS '(model provider reasoning system-append system-prepend temperature))

(define (rule->datum r)
  (append (list (list 'name (rule-name r)))
          (if (string=? (rule-description r) "") '() (list (list 'description (rule-description r))))
          (for/list ([k (in-list SETTINGS-RULE-KEYS)] #:when (hash-ref (rule-props r) k #f))
            (define v (hash-ref (rule-props r) k))
            (if (list? v) (cons k v) (list k v))))
) ; end define rule->datum

(define (write-settings-rules! rs)
  (make-directory* (config-home))
  (define tmp (path-add-extension (settings-rules-path) #".tmp"))
  (call-with-output-file tmp #:exists 'truncate
    (lambda (out) (for ([r (in-list rs)]) (write (rule->datum r) out) (newline out))))
  (rename-file-or-directory tmp (settings-rules-path) #t)
) ; end define write-settings-rules!

;; props：hasheq，键在 SETTINGS-RULE-KEYS 里；正则不合法 / 值类型不对 → raise。同名 → 替换
(define (save-settings-rule! name description props)
  (unless (and (string? name) (regexp-match? #px"^[A-Za-z0-9_.-]{1,60}$" name)) (error 'save-settings-rule! "bad rule name: ~s" name))
  (for ([(k v) (in-hash props)])
    (unless (memq k SETTINGS-RULE-KEYS) (error 'save-settings-rule! "unknown key: ~a" k))
    (case k
      [(model provider) (unless (string? v) (error 'save-settings-rule! "~a must be a string" k))
                        (with-handlers ([exn:fail? (lambda (_e) (error 'save-settings-rule! "~a is not a valid regex: ~s" k v))]) (pregexp v))]
      [(system-append system-prepend) (unless (string? v) (error 'save-settings-rule! "~a must be a string" k))]
      [(temperature) (unless (and (real? v) (<= 0 v 2)) (error 'save-settings-rule! "temperature must be a number in [0, 2]"))]
      [(reasoning) (unless (and (list? v) (andmap symbol? v)) (error 'save-settings-rule! "reasoning must be a list of levels"))]))
  (define r (rule name (or description "") 'settings props))
  (define cur (read-rules-file (settings-rules-path) 'settings))
  (write-settings-rules! (if (ormap (lambda (x) (equal? (rule-name x) name)) cur)
                             (for/list ([x (in-list cur)]) (if (equal? (rule-name x) name) r x))
                             (append cur (list r))))
) ; end define save-settings-rule!

;; → #t 删了 / #f 没有这条（只删设置页建的）
(define (delete-settings-rule! name)
  (define cur (read-rules-file (settings-rules-path) 'settings))
  (and (ormap (lambda (x) (equal? (rule-name x) name)) cur)
       (begin (write-settings-rules! (filter (lambda (x) (not (equal? (rule-name x) name))) cur)) #t))
) ; end define delete-settings-rule!

(provide save-settings-rule! delete-settings-rule! settings-rules-path)
(provide (struct-out rule) BUILTIN-RULES all-rules matching-rules apply-adaptations apply-rule
         rule-enabled? set-rule-enabled! rules->jsexpr user-rules-path project-rules-path)
