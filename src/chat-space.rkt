#lang tstring racket
;; chat-space.rkt — 聊天会话（匿名工作区，design-workspace.md §10）的目录：每个聊天会话一个临时目录，放在应用管理的根目录下，
;; 自动管理占用（防止生成的内容占满磁盘）。
;;
;; 配置 {config-home}/chat.rktd（每台主机各自一份，界面经 RPC set_chat_config 改）：
;;   'root          根目录（#f = 会话目录旁的 chats/）
;;   'max_total_gb  所有聊天目录合计上限（缺省 20）
;;   'max_chat_gb   单个聊天目录上限（缺省 5：超了只提示，不删正在用的）
;;   'min_free_gb   磁盘至少留这么多空闲（缺省 5）
;; 清理：合计超上限或磁盘空闲低于下限 → 按最近活动（会话文件的修改时间）从旧到新删聊天目录（跳过正在用的、置顶的），
;;   删到合计 ≤ 上限的 80% 且空闲 ≥ 下限为止；每删一个，调用方往那个会话里记一条提示（会话记录本身不删）。
;; 目录名 = 会话文件名（去掉 .rktd）：聊天目录 ↔ 会话文件一一对应。

(require racket/file racket/system racket/port (file "credentials.rkt"))

(define GB (* 1024 1024 1024))
(define DEFAULTS (hasheq 'root #f 'max_total_gb 20 'max_chat_gb 5 'min_free_gb 5))

(define (chat-config-path) (build-path (config-home) "chat.rktd"))

(define (load-chat-config)
  (define v (with-handlers ([exn:fail? (lambda (_e) #f)])
              (and (file-exists? (chat-config-path))
                   (call-with-input-file (chat-config-path) (lambda (in) (parameterize ([read-accept-reader #f]) (read in)))))))
  (for/fold ([c DEFAULTS]) ([(k v) (in-hash (if (hash? v) v (hasheq)))] #:when (hash-has-key? DEFAULTS k)) (hash-set c k v))
) ; end define load-chat-config

;; 合并、校验、写盘（临时文件 + 改名）→ 新配置；不合法的字段 raise
(define (save-chat-config! changes)
  (define c (for/fold ([c (load-chat-config)]) ([(k v) (in-hash changes)] #:when (hash-has-key? DEFAULTS k))
              (case k
                [(root) (unless (or (not v) (and (string? v) (absolute-path? v))) (error 'chat "root must be an absolute path"))
                        (hash-set c k (and v (not (string=? v "")) v))]
                [else (unless (and (real? v) (> v 0)) (error 'chat "~a must be a positive number" k)) (hash-set c k v)])))
  (make-directory* (config-home))
  (define tmp (path-add-extension (chat-config-path) #".tmp"))
  (call-with-output-file tmp #:exists 'truncate (lambda (out) (write c out)))
  (rename-file-or-directory tmp (chat-config-path) #t)
  c
) ; end define save-chat-config!

(define (chat-root cfg session-dir)
  (define r (hash-ref cfg 'root #f))
  (if (string? r) (string->path r) (build-path session-dir "chats")))

;; 会话文件 → 它的聊天目录
(define (chat-dir-for cfg session-dir session-path)
  (define-values (_b name _d) (split-path session-path))
  (build-path (chat-root cfg session-dir) (path->string (path-replace-extension name #""))))

;; 目录大小（字节）：不跟符号链接；超过 deadline（毫秒时刻）就停 → (values 字节 完整?)
(define (dir-size dir #:deadline [deadline +inf.0])
  (define total 0)
  (define complete? #t)
  (let walk ([d dir])
    (cond
      [(> (current-inexact-milliseconds) deadline) (set! complete? #f)]
      [(link-exists? d) (void)]
      [(file-exists? d) (set! total (+ total (with-handlers ([exn:fail? (lambda (_e) 0)]) (file-size d))))]
      [(directory-exists? d)
       (for ([f (in-list (with-handlers ([exn:fail? (lambda (_e) '())]) (directory-list d #:build? #t)))]) (walk f))]
      [else (void)]))
  (values total complete?)
) ; end define dir-size

;; 目录所在卷的空闲字节（df -Pk；拿不到 → #f）
(define (free-bytes dir)
  (with-handlers ([exn:fail? (lambda (_e) #f)])
    (define df (or (find-executable-path "df") "/bin/df"))
    (define-values (p out in err) (subprocess #f #f #f df "-Pk" (path->string (path->complete-path dir))))
    (close-output-port in)
    (define s (port->string out))
    (close-input-port err)
    (unless (sync/timeout 5 p) (subprocess-kill p #t))
    (define lines (string-split s "\n"))
    (and (>= (length lines) 2)
         (let ([cols (string-split (cadr lines))]) (and (>= (length cols) 4) (string->number (list-ref cols 3)) (* 1024 (string->number (list-ref cols 3))))))))

;; 用量：→ hasheq 'root 'dirs ((名字 字节 最近活动秒) …) 'total 'free 'complete
;; last-active：(名字 → 秒或 #f)：会话文件的修改时间（没有会话文件的目录 = 孤儿，按目录自己的修改时间）
(define (chat-usage root last-active #:budget-ms [budget 3000])
  (define deadline (+ (current-inexact-milliseconds) budget))
  (define complete? #t)
  (define dirs
    (for/list ([d (in-list (if (directory-exists? root) (directory-list root #:build? #t) '()))] #:when (directory-exists? d))
      (define-values (_b n _x) (split-path d))
      (define-values (bytes ok?) (dir-size d #:deadline deadline))
      (unless ok? (set! complete? #f))
      (list (path->string n) bytes (or (last-active (path->string n)) (file-or-directory-modify-seconds d)))))
  (hasheq 'root (path->string root) 'dirs dirs 'total (for/sum ([x dirs]) (cadr x)) 'free (free-bytes (if (directory-exists? root) root (current-directory)))
          'complete complete?)
) ; end define chat-usage

;; 要删哪些：→ (listof (list 名字 字节))。keep：不删的名字（正在用的、置顶的）
(define (cleanup-plan usage cfg keep)
  (define max-total (* GB (hash-ref cfg 'max_total_gb)))
  (define min-free (* GB (hash-ref cfg 'min_free_gb)))
  (define free0 (hash-ref usage 'free #f))
  (define total0 (hash-ref usage 'total))
  (define need? (or (> total0 max-total) (and free0 (< free0 min-free))))
  (cond
    [(not need?) '()]
    [else
     (define target (* 0.8 max-total))
     (let loop ([cands (sort (filter (lambda (x) (not (member (car x) keep))) (hash-ref usage 'dirs)) < #:key caddr)]
                [total total0] [free (or free0 +inf.0)] [acc '()])
       (cond
         [(or (null? cands) (and (<= total target) (>= free min-free))) (reverse acc)]
         [else (define x (car cands))
               (loop (cdr cands) (- total (cadr x)) (+ free (cadr x)) (cons (list (car x) (cadr x)) acc))]))])
) ; end define cleanup-plan

(define (gb n) (/ (round (* 10 (/ n GB))) 10.0))

(provide load-chat-config save-chat-config! chat-root chat-dir-for dir-size free-bytes chat-usage cleanup-plan gb GB DEFAULTS)
