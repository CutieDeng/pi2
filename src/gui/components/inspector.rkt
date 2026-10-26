#lang racket/base
;; inspector.rkt — 信息面板组件（design-workspace.md W1 §4.4）：会话的信息（内核 session_stats 的结果）按节显示，
;; 盖在对话区上的一张卡片（不盖侧栏），可滚动；右上角 ✕、Esc、点卡片外面关闭。只显示，不发请求。
;; needs text-layout → provides inspector-view

(require racket/list racket/string
         "../compute/drawlist.rkt" "../compute/palette.rkt" "../compute/layout.rkt" "../compute/paint.rkt" "../compute/rich.rkt"
         "../compute/hub.rkt" "../compute/interfaces.rkt" "../compute/i18n.rkt"
         "../compute/view.rkt" "../compute/widgets.rkt")                ; 视图平面：元素树 → 布局 → drawlist + 命中

(define HEAD '(0.55 0.70 1.0 1.0))
(define KEY-W 150.0)

(struct iv (rc [data #:mutable] [scroll #:mutable] [hits #:mutable] [content-h #:mutable] [view-h #:mutable]))

(define (j h k [d #f]) (if (hash? h) (hash-ref h k d) d))
(define (money c) (if (real? c) (format "$~a" (/ (round (* c 10000)) 10000.0)) "—"))
(define (num n) (if (real? n) (number->string (if (integer? n) (inexact->exact n) (/ (round (* 10 n)) 10.0))) "—"))
(define (bytes-str n) (cond [(not (real? n)) "—"] [(< n 1024) (format "~a B" n)] [(< n 1048576) (format "~a KB" (/ (round (/ n 102.4)) 10.0))]
                            [else (format "~a MB" (/ (round (/ n 104857.6)) 10.0))]))
(define (ts s) (if (string? s) (regexp-replace #rx"T" (if (>= (string-length s) 19) (substring s 0 19) s) " ") "—"))

;; 统计 → 节：(list 标题 (list (cons 键 值) …))
(define (sections d)
  (define tools (let ([v (j d 'tools '())]) (if (list? v) (sort v > #:key (lambda (x) (j x 'calls 0))) '())))
  (define models (let ([v (j d 'models '())]) (if (list? v) v '())))
  (define perms (let ([v (j d 'permissions (hasheq))]) (if (hash? v) v (hasheq))))
  (define notes (let ([v (j d 'notes (hasheq))]) (if (hash? v) v (hasheq))))
  (define subs (let ([v (j d 'subagents '())]) (if (list? v) v '())))
  (define groups (let ([v (j d 'groups (hasheq))]) (if (hash? v) v (hasheq))))   ; 拼装层从目录 catalog 填（W2）：已排好的文字
  (define instr (j d 'instructions 'null))                                       ; 最后一次注入的指令清单（design-memory.md M1）；'null = 还没记
  (define kib (let ([m (j d 'instructions_max_bytes)]) (if (real? m) (quotient m 1024) 32)))
  (define mem (j d 'memory 'null))                                                ; 会话开始时注入的记忆（ids bytes）；'null = 还没记
  (define used (let ([v (j d 'memory_used '())]) (if (list? v) v '())))           ; memory_search 查到过的 id
  (define mem-text (let ([v (j d 'memory-text (hash))]) (if (hash? v) v (hash)))) ; 拼装层从记忆库填：id → 文字（删掉了的没有）
  (define (mem-line id) (let ([tx (hash-ref mem-text id #f)]) (if tx (format "[~a] ~a" id tx) (format "[~a]" id))))
  (filter
   (lambda (sec) (pair? (cadr sec)))
   (list
    (list (t 'in.general)
          (list (cons (t 'in.title) (format "~a" (j d 'title "—")))
                (cons (t 'in.path) (format "~a" (j d 'path "—")))
                (cons (t 'in.workdir) (format "~a~a" (if (j d 'chat #f) "💬 " "") (let ([w (j d 'workdir)]) (if (string? w) w "—"))))
                (cons (t 'in.chat-dir) (let ([b (j d 'workdir_bytes)]) (if (real? b) (bytes-str b) "—")))
                (cons (t 'in.created) (ts (j d 'created)))
                (cons (t 'in.last-active) (ts (j d 'last_active)))
                (cons (t 'in.forked-from) (let ([f (j d 'forked_from)]) (if (string? f) f "—")))))
    (list (t 'in.groups)
          (for/list ([k (in-list '(workspace super projects git_remote))]
                     [label (in-list (list (t 'in.workspace) (t 'in.super-workspace) (t 'in.projects) (t 'in.git-remote)))]
                     #:when (let ([v (j groups k)]) (and (string? v) (not (string=? v "")))))
            (cons label (j groups k))))
    (list (t 'in.instructions)
          (cond
            [(not (list? instr)) (list (cons "—" (t 'in.instr-none)))]
            [(null? instr) (list (cons "—" (t 'in.instr-empty)))]
            [else
             (for/list ([x (in-list instr)] #:when (hash? x))
               (define k (j x 'kind "dir"))
               (cons (t (case k [("global") 'in.instr-k-global] [("project") 'in.instr-k-project] [("super-workspace" "super_workspace") 'in.instr-k-super]
                                [("workspace") 'in.instr-k-workspace] [else 'in.instr-k-dir]))
                     (format "~a · ~a" (j x 'label "?")
                             (cond [(eq? (j x 'omitted) #t) (t 'in.instr-omitted (num (j x 'bytes)) kib)]
                                   [(eq? (j x 'truncated) #t) (t 'in.instr-truncated (num (j x 'bytes)) kib)]
                                   [else (t 'in.instr-bytes (num (j x 'bytes)))]))))]))
    (list (t 'in.memory)
          (append
           (cond
             [(not (hash? mem)) (list (cons (t 'in.mem-injected) (t 'in.mem-none)))]
             [else (define ids (let ([v (j mem 'ids '())]) (if (list? v) v '())))
                   (cons (cons (t 'in.mem-injected) (t 'in.mem-n (num (length ids)) (num (j mem 'bytes 0))))
                         (for/list ([id (in-list ids)]) (cons "" (mem-line id))))])
           (if (null? used) '()
               (cons (cons (t 'in.mem-used) (t 'in.mem-used-n (num (length used))))
                     (for/list ([id (in-list used)]) (cons "" (mem-line id)))))))
    (list (t 'in.content)
          (list (cons (t 'in.messages) (format "~a（~a）" (num (j d 'messages)) (t 'in.user-n (num (j d 'user_messages)))))
                (cons (t 'in.records) (num (j d 'records)))
                (cons (t 'in.compactions) (num (j d 'compactions)))
                (cons (t 'in.media) (let ([m (j d 'media (hasheq))]) (format "~a · ~a" (num (j m 'count 0)) (bytes-str (j m 'bytes 0)))))
                (cons (t 'in.file) (format "~a + ~a" (bytes-str (j d 'bytes)) (t 'in.index (bytes-str (j d 'index_bytes)))))
                (cons (t 'in.effects) (num (j d 'effects)))))
    (list (t 'in.usage)
          (cons (cons (t 'in.total) (format "~a · ~a" (t 'in.requests (num (j d 'requests))) (money (j d 'cost))))
                (for/list ([m (in-list models)])
                  (cons (format "~a" (j m 'model "?"))
                        (format "~a · in ~a / out ~a · ~a" (t 'in.requests (num (j m 'requests))) (num (j m 'input)) (num (j m 'output)) (money (j m 'cost)))))))
    (list (t 'in.tools)
          (for/list ([x (in-list tools)])
            (cons (format "~a~a" (j x 'name "?") (let ([s (j x 'source "builtin")]) (if (equal? s "builtin") "" (format " [~a]" s))))
                  (format "~a · ~a ms~a" (t 'in.calls (num (j x 'calls))) (num (j x 'ms))
                          (let ([e (j x 'errors 0)]) (if (and (real? e) (> e 0)) (format " · ~a" (t 'in.errors (num e))) ""))))))
    (list (t 'in.permissions)
          (for/list ([(k v) (in-hash perms)]) (cons (format "~a" k) (num v))))
    (list (t 'in.notes)
          (for/list ([(k v) (in-hash notes)]) (cons (format "~a" k) (num v))))
    (list (t 'in.subagents)
          (for/list ([x (in-list subs)]) (cons (format "~a" (j x 'call "?")) (t 'in.records-n (num (j x 'records)))))))))

(define (render! s dl x y w h scale)
  ;; 视图平面：模态卡片（compute/widgets.rkt）；正文 = 各节（标题 + 键值行）
  (define body
    (for/list ([sec (in-list (sections (iv-data s)))])
      (apply ui-box #:dir 'column #:gap 6.0 #:pad (vector 0.0 0.0 10.0 0.0)
             (ui-text (car sec) #:font 'small #:color HEAD #:fit? #f)
             (for/list ([kv (in-list (cadr sec))]) (kv-row (car kv) (cdr kv) #:key-w KEY-W)))))
  (define-values (hits regions scrolls)
    (render-view! (iv-rc s) dl (modal-card #:title (t 'in.heading) #:body body #:offset (iv-scroll s)) x y w h scale))
  (define sc (hash-ref scrolls 'body))
  (set-iv-scroll! s (vector-ref sc 0))
  (set-iv-content-h! s (vector-ref sc 1))
  (set-iv-view-h! s (vector-ref sc 2))
  (set-iv-hits! s (reverse hits))                                         ; 找命中时从最上面（后画的）找起
) ; end define render!

;; → (list 'close) | #f（点在卡片上）
(define (hit s x y)
  (define h (for/first ([h (in-list (iv-hits s))]
                        #:when (and (<= (vector-ref h 2) x (+ (vector-ref h 2) (vector-ref h 4)))
                                    (<= (vector-ref h 3) y (+ (vector-ref h 3) (vector-ref h 5)))))
              h))
  (cond [(not h) #f]
        [(memq (vector-ref h 0) '(close outside)) (set-iv-data! s #f) (list 'close)]
        [else #f])
) ; end define hit

(define inspector-component
  (component 'inspector (list text-layout) (list inspector-view)
             (lambda (use config)
               (define s (iv ((use text-layout) 'rich) #f 0.0 '() 0.0 0.0))
               (list (implement inspector-view
                                'open! (lambda (data) (set-iv-data! s data) (set-iv-scroll! s 0.0))
                                'close! (lambda () (set-iv-data! s #f))
                                'open? (lambda () (and (iv-data s) #t))
                                'data (lambda () (iv-data s))
                                'render! (lambda (dl x y w h scale) (when (iv-data s) (render! s dl x y w h scale)))
                                'hit (lambda (x y) (hit s x y))
                                'scroll! (lambda (dy) (set-iv-scroll! s (- (iv-scroll s) dy)))
                                'hit-center (lambda (kind)
                                              (for/first ([h (in-list (iv-hits s))] #:when (eq? (vector-ref h 0) kind))
                                                (cons (+ (vector-ref h 2) (/ (vector-ref h 4) 2)) (+ (vector-ref h 3) (/ (vector-ref h 5) 2))))))))))

(provide inspector-component sections)
