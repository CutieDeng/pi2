#lang racket/base
;; commit-datum.rkt — 提交消息的 Racket datum：解析与查询（git_history 工具用；界面以后也可用）。
;;   消息形状：(TYPE "title" (元数据 …) "detail")。用户 2026-10-26：结构化的是第一项 TYPE 与第三项元数据；
;;   标题、说明是自由文字，不要求列全改动文件 → 改动文件取 git 的实际记录，不从说明里解析。
;;   旧的 3 项形状 (TYPE "title" "detail") 当作元数据为空；读不成这两种形状 → 纯文字（type #f，标题 = 第一行）。
;;   元数据不定形状（commit skill：扩展点，不强加内部结构）：每一项可以是
;;     符号            → 标记，如 breaking
;;     以符号开头的表  → 标签，如 (scope gui kernel)、(CI github)
;;     别的            → 原样保留，只能按 equal? 查到
;;   安全：提交消息是不可信输入——只 read、从不 eval；关掉 #reader / #lang / 图记号（#0= 能造出环，遍历会卡住）/ 编译代码。

(require racket/string racket/list)

(struct commit (hash date type title meta detail files shape) #:transparent)   ; shape：'datum | 'legacy | 'text

;; 安全地读恰好一个 datum；读不了 / 后面还有东西 → #f
(define (safe-read-one s)
  (parameterize ([read-accept-reader #f] [read-accept-lang #f] [read-accept-graph #f] [read-accept-compiled #f]
                 [read-accept-quasiquote #t] [read-on-demand-source #f])
    (with-handlers ([exn:fail? (lambda (_e) #f)])
      (define in (open-input-string s))
      (define d (read in))
      (and (not (eof-object? d))
           (eof-object? (with-handlers ([exn:fail? (lambda (_e) 'extra)]) (read in)))
           d))))

;; 消息文字 → (values shape type title meta detail)
(define (parse-commit-message s)
  (define d (safe-read-one s))
  (cond
    [(and (list? d) (= (length d) 4) (symbol? (first d)) (string? (second d)) (list? (third d)) (string? (fourth d)))
     (values 'datum (first d) (second d) (third d) (fourth d))]
    [(and (list? d) (= (length d) 3) (symbol? (first d)) (string? (second d)) (string? (third d)))
     (values 'legacy (first d) (second d) '() (third d))]
    [else
     (define lines (string-split (string-trim s) "\n" #:trim? #f))
     (values 'text #f (if (pair? lines) (string-trim (car lines)) "")
             (trailer-meta s) (if (pair? lines) (string-trim (string-join (cdr lines) "\n")) ""))]))

;; 纯文字消息里的「Pi2-Automation: 种类; k=v; …」trailer → 同样的元数据 (pi2 种类 (k "v") …)：两种惯例的自动化提交一样查、一样归节
(define (trailer-meta s)
  (define m (regexp-match #px"(?m:^Pi2-Automation: ([^;\n]+)((?:; [^;\n=]+=[^;\n]*)*)[ \t]*$)" s))
  (if m
      (list (append (list 'pi2 (string->symbol (string-trim (cadr m))))
                    (for/list ([kv (regexp-match* #px"; ([^;\n=]+)=([^;\n]*)" (caddr m) #:match-select cdr)])
                      (list (string->symbol (string-trim (car kv))) (string-trim (cadr kv))))))
      '()))

;; 元数据项的「头」：标记 → 它自己；(头 参数…) → 头；别的 → #f
(define (meta-head item) (cond [(symbol? item) item] [(and (pair? item) (symbol? (car item))) (car item)] [else #f]))
(define (meta-args item) (if (and (pair? item) (list? item)) (cdr item) '()))

;; 查询串 → datum（同样安全地读）；读不了 → #f
(define (parse-meta-query s) (and (string? s) (safe-read-one s)))

;; 一个查询是否命中某提交的元数据：
;;   符号 S        → 有标记 S 或标签 (S …)
;;   (H a …)       → 有标签 (H …)，且 a … 都在它的参数里（equal?）
;;   别的          → 有一项与它 equal?
(define (meta-match? q meta)
  (for/or ([item (in-list meta)])
    (cond
      [(symbol? q) (eq? (meta-head item) q)]
      [(and (pair? q) (list? q) (symbol? (car q)))
       (and (eq? (meta-head item) (car q))
            (for/and ([a (in-list (cdr q))]) (and (member a (meta-args item)) #t)))]
      [else (equal? item q)])))

;; TYPE 比较不分大小写（feat 与 FEAT 一样）
(define (type-in? t types)
  (and (symbol? t) (for/or ([x (in-list types)]) (string-ci=? (symbol->string t) (format "~a" x)))))

;; 过滤：types（#f = 不限）、metas（查询 datum 表，全部要命中）、grep（标题 + 说明里不分大小写的子串）
(define (commit-matches? c #:types [types #f] #:metas [metas '()] #:grep [grep #f])
  (and (or (not types) (null? types) (type-in? (commit-type c) types))
       (for/and ([q (in-list metas)]) (meta-match? q (commit-meta c)))
       (or (not grep) (string=? grep "")
           (let ([g (string-downcase grep)])
             (or (string-contains? (string-downcase (commit-title c)) g)
                 (string-contains? (string-downcase (commit-detail c)) g))))))

;; git log 的输出（--format=%x1e%H%x1f%aI%x1f%B%x1f --name-only）→ (listof commit)
(define (parse-git-log out)
  (for/list ([rec (in-list (string-split out "\x1e"))]
             #:when (regexp-match? #rx"\x1f" rec))
    (define parts (string-split rec "\x1f" #:trim? #f))
    (define-values (hash date body tail)
      (values (string-trim (list-ref parts 0)) (string-trim (list-ref parts 1))
              (if (> (length parts) 2) (list-ref parts 2) "")
              (if (> (length parts) 3) (list-ref parts 3) "")))
    (define-values (shape type title meta detail) (parse-commit-message body))
    (commit hash date type title meta detail
            (filter (lambda (l) (not (string=? l ""))) (map string-trim (string-split tail "\n")))
            shape)))

;; 统计：TYPE 与元数据头各出现几次（看这个仓库用了哪些标签）
(define (history-stats cs)
  (define (tally xs) (sort (hash->list (for/fold ([h (hash)]) ([x (in-list xs)]) (hash-update h x add1 0))) > #:key cdr))
  (values (tally (map (lambda (c) (or (commit-type c) '|(text)|)) cs))
          (tally (filter values (append-map (lambda (c) (map meta-head (commit-meta c))) cs)))))

;; ---------------------------------------------------------------- 标签规则（.pi/commit-meta.rktd）
;; 用户 2026-10-26：元数据标签 → 界面上显示成标签；changelog 里单独成节，或不进 changelog。
;;   (commit-meta
;;     (tag breaking #:label "breaking" #:changelog (section "Breaking changes"))
;;     (tag CI #:changelog exclude)
;;     (tag wip #:label #f))                        ; #:label #f = 不显示
;; 没配的标签：显示它的头（+ 简单参数），changelog 按 TYPE 归类。内置：pi2（自动化提交）单独成节。
;; 规则的先后 = changelog 归节的优先级（一个提交命中多条 section 规则时取最先的；exclude 优先于一切）。
(struct tag-rule (head label changelog) #:transparent)   ; label：字符串 | #f（不显示）| 'default；changelog：'default | 'exclude | (cons 'section 名)

(define BUILTIN-RULES (list (tag-rule 'pi2 "pi2" (cons 'section "Automated (pi2)"))))

;; datum → (listof tag-rule)；形状不对 → raise（说明哪里不对）
(define (parse-commit-meta d)
  (unless (and (pair? d) (eq? (car d) 'commit-meta)) (error 'commit-meta "must be (commit-meta (tag HEAD #:label … #:changelog …) …)"))
  (for/list ([c (in-list (cdr d))])
    (unless (and (list? c) (>= (length c) 2) (eq? (car c) 'tag) (symbol? (cadr c))) (error 'commit-meta "bad entry ~s: expected (tag HEAD #:label \"…\" #:changelog exclude|(section \"…\"))" c))
    (let loop ([kv (cddr c)] [label 'default] [cl 'default])
      (cond
        [(null? kv) (tag-rule (cadr c) label cl)]
        [(and (eq? (car kv) '#:label) (pair? (cdr kv)) (or (string? (cadr kv)) (eq? (cadr kv) #f))) (loop (cddr kv) (cadr kv) cl)]
        [(and (eq? (car kv) '#:changelog) (pair? (cdr kv)) (eq? (cadr kv) 'exclude)) (loop (cddr kv) label 'exclude)]
        [(and (eq? (car kv) '#:changelog) (pair? (cdr kv)) (pair? (cadr kv)) (eq? (car (cadr kv)) 'section) (= (length (cadr kv)) 2) (string? (cadr (cadr kv))))
         (loop (cddr kv) label (cons 'section (cadr (cadr kv))))]
        [else (error 'commit-meta "bad option in ~s at ~s" c kv)]))))

;; 合并：后面的覆盖前面同头的规则（内置 < 用户 < 项目）；结果里越具体的层越靠前（项目的节排在内置的「Automated (pi2)」前面，
;; 一个提交命中多条 section 规则时也先取项目的）
(define (merge-rules . layers)
  (for/fold ([acc '()]) ([layer (in-list layers)])
    (append layer (filter (lambda (r) (not (memq (tag-rule-head r) (map tag-rule-head layer)))) acc))))

;; 文件 → (values rules 错误或#f)：缺文件 = '()；读不了 / 形状不对 → '() + 错误
(define (read-commit-meta-file f)
  (cond
    [(not (and f (file-exists? f))) (values '() #f)]
    [else
     (define d (safe-read-one (call-with-input-file f (lambda (in) (let ([o (open-output-string)]) (copy-port* in o) (get-output-string o))))))
     (if (not d)
         (values '() (format "~a: not a readable Racket datum" f))
         (with-handlers ([exn:fail? (lambda (e) (values '() (format "~a: ~a" f (exn-message e))))])
           (values (parse-commit-meta d) #f)))]))
(define (copy-port* in out) (let loop () (define s (read-string 65536 in)) (unless (eof-object? s) (write-string s out) (loop))))

(define (rule-for head rules) (for/first ([r (in-list rules)] #:when (eq? (tag-rule-head r) head)) r))

;; 一个提交的标签文字（元数据顺序）：配置的 label（或头的名字）+ 简单参数（符号 / 字符串 / 数字）；#:label #f 的不出现
(define (commit-labels c rules)
  (for*/list ([item (in-list (commit-meta c))]
              [h (in-value (meta-head item))] #:when h
              [r (in-value (rule-for h rules))]
              #:unless (and r (eq? (tag-rule-label r) #f)))
    (define base (if (and r (string? (tag-rule-label r))) (tag-rule-label r) (symbol->string h)))
    (define simple (for/list ([a (in-list (meta-args item))] #:when (or (symbol? a) (string? a) (number? a))) (format "~a" a)))
    (if (null? simple) base (string-append base " " (string-join simple " ")))))

;; 提交归哪一节：'exclude | 节名
(define TYPE-SECTIONS '((FEAT . "Features") (FIX . "Fixes") (REFACTOR . "Refactoring") (TEST . "Tests") (DOCS . "Documentation") (BUILD . "Build")))
(define (commit-section c rules)
  (define heads (filter values (map meta-head (commit-meta c))))
  (define hit (filter (lambda (r) (memq (tag-rule-head r) heads)) rules))
  (cond
    [(for/or ([r hit]) (eq? (tag-rule-changelog r) 'exclude)) 'exclude]
    [(for/first ([r hit] #:when (pair? (tag-rule-changelog r))) (cdr (tag-rule-changelog r))) => values]
    [else (let ([t (and (commit-type c) (string->symbol (string-upcase (symbol->string (commit-type c)))))])
            (or (and t (cdr (or (assq t TYPE-SECTIONS) '(#f . #f)))) "Other"))]))

;; markdown：先按规则顺序的那些节，再 TYPE 各节，最后 Other；每条「- 标题 (hash) [标签] …」
(define (changelog-markdown cs rules #:title [title "Changelog"])
  (define groups (make-hash))
  (define excluded 0)
  (for ([c (in-list cs)])
    (define s (commit-section c rules))
    (if (eq? s 'exclude) (set! excluded (add1 excluded)) (hash-update! groups s (lambda (l) (cons c l)) '())))
  (define order (remove-duplicates (append (for/list ([r rules] #:when (pair? (tag-rule-changelog r))) (cdr (tag-rule-changelog r)))
                                           (map cdr TYPE-SECTIONS) (list "Other"))))
  (string-append
   "## " title "\n"
   (apply string-append
          (for/list ([sec (in-list order)] #:when (hash-ref groups sec #f))
            (string-append "\n### " sec "\n"
                           (apply string-append
                                  (for/list ([c (in-list (reverse (hash-ref groups sec)))])
                                    (define ls (commit-labels c rules))
                                    (format "- ~a (~a)~a\n" (commit-title c) (substring (commit-hash c) 0 (min 7 (string-length (commit-hash c))))
                                            (apply string-append (map (lambda (l) (format " [~a]" l)) ls))))))))
   (if (> excluded 0) (format "\n(~a commit(s) left out by #:changelog exclude rules)\n" excluded) "")))

;; ---------------------------------------------------------------- 自动化提交的元数据
;; 用户 2026-10-26：自动化提交必须写元数据，以区分。标签 (pi2 种类 (session "会话id") (model "模型"))，
;;   种类：agent（模型经 git 工具提交）/ goal-worker / goal-merge / goal-driver。
;; 仓库用 datum 提交消息 → 标签进第三项；不用 → 纯文字末尾加 trailer「Pi2-Automation: …」（git 的惯例写法）。
(define (automation-tag kind #:session [sid #f] #:model [model #f])
  (append (list 'pi2 kind) (if sid (list (list 'session sid)) '()) (if model (list (list 'model model)) '())))
(define (has-pi2-tag? meta) (for/or ([i (in-list meta)]) (eq? (meta-head i) 'pi2)))
(define (automation-trailer tag)
  (string-append "Pi2-Automation: " (format "~a" (cadr tag))
                 (apply string-append (for/list ([a (in-list (cddr tag))] #:when (and (pair? a) (= (length a) 2)))
                                        (format "; ~a=~a" (car a) (cadr a))))))

;; 已有的消息加上自动化标签，尽量不动原文：datum → 只替换第三项的文字；3 项旧形状 → 在标题后插入元数据；纯文字 → 末尾加 trailer。
;; 已带 pi2 标签 / trailer 的不重复加。
(define (add-automation-meta msg tag)
  (define-values (shape _t _ti meta _d) (parse-commit-message msg))
  (case shape
    [(datum legacy)
     (cond
       [(has-pi2-tag? meta) msg]
       [else
        (define stx (parameterize ([read-accept-reader #f] [read-accept-lang #f] [read-accept-graph #f] [read-accept-compiled #f])
                      (let ([in (open-input-string msg)]) (port-count-lines! in) (read-syntax 'commit in))))   ; 计行 → 位置按字符算（否则是字节，中文标题会切错）
        (define parts (syntax->list stx))
        (define (span-of x) (values (sub1 (syntax-position x)) (+ (sub1 (syntax-position x)) (syntax-span x))))
        (cond
          [(eq? shape 'datum)
           (define-values (a b) (span-of (list-ref parts 2)))
           (string-append (substring msg 0 a) (format "~s" (append meta (list tag))) (substring msg b))]
          [else
           (define-values (_a b) (span-of (list-ref parts 1)))
           (string-append (substring msg 0 b) "\n\n" (format "~s" (list tag)) (substring msg b))])])]
    [else
     (if (regexp-match? #rx"(?m:^Pi2-Automation: )" msg)
         msg
         (string-append (string-trim msg #:left? #f) "\n\n" (automation-trailer tag)))]))

;; 仓库是否用 datum 提交消息：最近的消息里 datum / 旧 3 项形状占一半以上（没有提交 → #f）
(define (datum-convention? msgs)
  (define ms (filter (lambda (m) (not (string=? (string-trim m) ""))) msgs))
  (and (pair? ms)
       (>= (* 2 (count (lambda (m) (let-values ([(sh _a _b _c _d) (parse-commit-message m)]) (memq sh '(datum legacy)))) ms)) (length ms))))

;; pi2 自己生成的提交消息（goal 的 driver / worker / merge）：按仓库惯例
(define (automation-message datum? type title tag #:detail [detail ""])
  (if datum?
      (format "(~a ~s\n\n~s\n~s\n)\n" type title (list tag) (if (string=? detail "") title detail))
      (string-append title (if (string=? detail "") "" (string-append "\n\n" detail)) "\n\n" (automation-trailer tag) "\n")))

(provide (struct-out commit) safe-read-one parse-commit-message meta-head meta-args parse-meta-query meta-match?
         type-in? commit-matches? parse-git-log history-stats
         (struct-out tag-rule) BUILTIN-RULES parse-commit-meta merge-rules read-commit-meta-file commit-labels commit-section changelog-markdown
         automation-tag add-automation-meta automation-trailer datum-convention? automation-message has-pi2-tag?)
