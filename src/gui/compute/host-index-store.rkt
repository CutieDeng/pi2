#lang racket/base
;; host-index-store.rkt — 项目索引的描述库（design-memory.md §8）：GUI 这台机器的 {config-home}/host-index.rktd；只追加记录、读时重放（同 catalog.rkt）。
;; 键 = (主机 . 仓库目录)。记录：
;;   (hasheq 'op "repos" 'host 'repos ((hasheq 'dir 'name 'fingerprint 'remote 'mtime) …))   一台主机的一次扫描（集合变了才写）；不在里面的 = 不见了
;;   (hasheq 'op "desc" 'host 'dir 'fingerprint 'desc 'model)       模型写的描述（desc：short long names summary_en summary_zh）
;;   (hasheq 'op "edit" 'host 'dir 'desc)                             用户改的 → 锁定（重新生成不覆盖）
;;   (hasheq 'op "regenerate" 'host 'dir)                             用户要重新生成：解锁、作废旧描述的指纹
;;   (hasheq 'op "error" 'host 'dir 'fingerprint 'error)              这个指纹描述失败（指纹变了才再试）
;; 不能写文件（回放）时：path #f → 只在内存里；回放从录制时的快照开始。

(require racket/list racket/string)

(struct hix (path [repos #:mutable]))            ; repos：(cons 主机 目录) → hasheq

(define (now-iso)
  (define d (seconds->date (current-seconds)))
  (define (p n) (if (< n 10) (format "0~a" n) (format "~a" n)))
  (format "~a-~a-~a ~a:~a" (date-year d) (p (date-month d)) (p (date-day d)) (p (date-hour d)) (p (date-minute d))))

(define (key host dir) (cons host dir))
(define (upd! h k f) (let ([e (hash-ref (hix-repos h) k #f)]) (when e (set-hix-repos! h (hash-set (hix-repos h) k (f e))))))

(define (apply-rec! h r)
  (define host (hash-ref r 'host #f))
  (define k (key host (hash-ref r 'dir #f)))
  (case (hash-ref r 'op #f)
    [("repos")
     (define now (for/hash ([x (in-list (hash-ref r 'repos '()))] #:when (and (hash? x) (string? (hash-ref x 'dir #f)))) (values (key host (hash-ref x 'dir)) x)))
     (set-hix-repos! h
       (for/fold ([m (for/hash ([(k e) (in-hash (hix-repos h))])                         ; 这台主机的旧条目：不在新集合里 → 不见了（描述留着）
                       (values k (if (and (equal? (car k) host) (not (hash-ref now k #f))) (hash-set e 'present #f) e)))])
                 ([(k x) (in-hash now)])
         (define old (hash-ref m k (hasheq 'host host 'dir (cdr k) 'desc #f 'desc-fp #f 'model #f 'locked #f 'error #f 'error-fp #f 'described "")))
         (hash-set m k (hash-set* old 'name (hash-ref x 'name (cdr k)) 'fingerprint (hash-ref x 'fingerprint "")
                                  'remote (hash-ref x 'remote 'null) 'mtime (hash-ref x 'mtime 0) 'present #t))))]
    [("desc") (upd! h k (lambda (e) (if (hash-ref e 'locked #f) e
                                         (hash-set* e 'desc (hash-ref r 'desc #f) 'desc-fp (hash-ref r 'fingerprint #f) 'model (hash-ref r 'model #f)
                                                    'error #f 'error-fp #f 'described (hash-ref r 'ts "") 'desc-n (add1 (hash-ref e 'desc-n 0))))))]   ; desc-n：模型写过几次
    [("edit") (upd! h k (lambda (e) (hash-set* e 'desc (hash-ref r 'desc #f) 'locked #t 'described (hash-ref r 'ts ""))))]
    [("regenerate") (upd! h k (lambda (e) (hash-set* e 'locked #f 'desc-fp #f 'error #f 'error-fp #f)))]
    [("error") (upd! h k (lambda (e) (hash-set* e 'error (hash-ref r 'error "?") 'error-fp (hash-ref r 'fingerprint #f))))]
    [else (void)]))

(define (append! h r0)
  (define r (hash-set r0 'ts (now-iso)))
  (apply-rec! h r)
  (when (hix-path h)
    (with-output-to-file (hix-path h) #:exists 'append (lambda () (write r) (newline)))))

(define (hix-open path)
  (define h (hix path (hash)))
  (when (and path (file-exists? path))
    (call-with-input-file path
      (lambda (in)
        (let loop ()
          (define r (with-handlers ([exn:fail? (lambda (_e) 'bad)]) (read in)))
          (cond [(eof-object? r) (void)]
                [(hash? r) (apply-rec! h r) (loop)]
                [(eq? r 'bad) (void)]
                [else (loop)])))))
  h)

(define (hix-snapshot h) (hasheq 'repos (hix-repos h)))
(define (hix-from-snapshot snap) (hix #f (let ([r (and (hash? snap) (hash-ref snap 'repos #f))]) (if (hash? r) r (hash)))))

;; ---------------------------------------------------------------- 写
;; 一次扫描的结果：集合（目录 + 指纹）跟现在的一样就不写
(define (hix-set-repos! h host repos)
  (define cur (for/list ([(k e) (in-hash (hix-repos h))] #:when (and (equal? (car k) host) (hash-ref e 'present #f)))
                (cons (cdr k) (hash-ref e 'fingerprint ""))))
  (define new (for/list ([x (in-list repos)] #:when (hash? x)) (cons (hash-ref x 'dir "") (hash-ref x 'fingerprint ""))))
  (unless (equal? (sort cur string<? #:key car) (sort new string<? #:key car))
    (append! h (hasheq 'op "repos" 'host host 'repos repos))))
(define (hix-set-desc! h host dir fp desc model) (append! h (hasheq 'op "desc" 'host host 'dir dir 'fingerprint fp 'desc desc 'model model)))
(define (hix-edit! h host dir desc) (append! h (hasheq 'op "edit" 'host host 'dir dir 'desc desc)))
(define (hix-regenerate! h host dir) (append! h (hasheq 'op "regenerate" 'host host 'dir dir)))
(define (hix-set-error! h host dir fp msg) (append! h (hasheq 'op "error" 'host host 'dir dir 'fingerprint fp 'error msg)))

;; ---------------------------------------------------------------- 读
(define (hix-entry h host dir) (hash-ref (hix-repos h) (key host dir) #f))
;; 一台主机（或全部）现有的仓库，最近提交的在前
(define (hix-entries h [host #f])
  (sort (for/list ([(k e) (in-hash (hix-repos h))] #:when (and (hash-ref e 'present #f) (or (not host) (equal? (car k) host)))) e)
        > #:key (lambda (e) (let ([m (hash-ref e 'mtime 0)]) (if (real? m) m 0)))))
;; 要描述的：在、没锁、描述的指纹不是现在的、这个指纹也没失败过
(define (hix-stale? e)
  (and (hash-ref e 'present #f) (not (hash-ref e 'locked #f))
       (not (equal? (hash-ref e 'desc-fp #f) (hash-ref e 'fingerprint #f)))
       (not (equal? (hash-ref e 'error-fp #f) (hash-ref e 'fingerprint #f)))))
(define (hix-stale h [host #f]) (filter hix-stale? (hix-entries h host)))
;; 推给内核：这台主机的项目（desc 没有 → 'null）
(define (hix-view h host)
  (for/list ([e (in-list (hix-entries h host))])
    (hasheq 'dir (hash-ref e 'dir) 'name (hash-ref e 'name "?") 'remote (hash-ref e 'remote 'null) 'mtime (hash-ref e 'mtime 0)
            'desc (or (hash-ref e 'desc #f) 'null))))

;; ---------------------------------------------------------------- 用户编辑：描述 ↔ 文字
;;   names: a, b
;;   short: a, b, c
;;   summary_en: …
;;   summary_zh: …
;;   long:
;;   一行一句
(define (desc->text d)
  (define (j k) (if (hash? d) (hash-ref d k '()) '()))
  (define (s k) (if (hash? d) (hash-ref d k "") ""))
  (string-append "names: " (string-join (j 'names) ", ") "\n"
                 "short: " (string-join (j 'short) ", ") "\n"
                 "summary_en: " (s 'summary_en) "\n"
                 "summary_zh: " (s 'summary_zh) "\n"
                 "long:\n" (string-join (j 'long) "\n")))
(define (text->desc txt)
  (define lines (string-split txt "\n" #:trim? #f))
  (define (csv s) (filter (lambda (x) (not (string=? x ""))) (map string-trim (regexp-split #rx"[,，、]" s))))
  (let loop ([ls lines] [d (hasheq 'names '() 'short '() 'long '() 'summary_en "" 'summary_zh "")] [in-long? #f])
    (cond
      [(null? ls) d]
      [else
       (define l (string-trim (car ls)))
       (define m (regexp-match #px"^(names|short|summary_en|summary_zh|long)\\s*[:：]\\s*(.*)$" l))
       (cond
         [m (define k (string->symbol (cadr m)))
            (case k
              [(names short) (loop (cdr ls) (hash-set d k (csv (caddr m))) #f)]
              [(long) (loop (cdr ls) (if (string=? (caddr m) "") d (hash-set d 'long (list (caddr m)))) #t)]
              [else (loop (cdr ls) (hash-set d k (caddr m)) #f)])]
         [(and in-long? (not (string=? l ""))) (loop (cdr ls) (hash-set d 'long (append (hash-ref d 'long) (list l))) #t)]
         [else (loop (cdr ls) d in-long?)])])))

(provide hix-open hix-snapshot hix-from-snapshot hix-set-repos! hix-set-desc! hix-edit! hix-regenerate! hix-set-error!
         hix-entry hix-entries hix-stale hix-stale? hix-view desc->text text->desc)
