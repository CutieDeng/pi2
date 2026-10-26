#lang racket/base
;; journal-format.rkt — GUI 操作日志的文件格式（G6）：一行一个 datum，write / read 往返。
;;   首行：(pi2-gui-journal 1 头信息hash)          头信息：w h scale argv started
;;   其后：(毫秒 序号 kind payload)                kind ∈ start win core send script-send page frame error note
;;   script-send：冒烟脚本直接发给内核的请求（不是界面操作产生的，回放不比对）
;;   page：对话区按视口发的请求（items / item / index_summary / select_lines；回放不比对）
;;   frame：一次重绘（回放时窗口事件按它分批，命中区与录制时一致）
;; 窗口事件是 prefab（compute/event.rkt），内核事件 / 请求是 jsexpr（hasheq），都能直接 write / read。

(require racket/list racket/string)

(define JOURNAL-VERSION 1)

(define (journal-header-datum header) (list 'pi2-gui-journal JOURNAL-VERSION header))

;; → (values 头信息 记录表)；记录 = (list 毫秒 序号 kind payload)。残缺尾行（进程被杀时写了一半）视为结束。
(define (read-journal path)
  (call-with-input-file path
    (lambda (in)
      (parameterize ([read-accept-reader #f] [read-accept-lang #f])
        (define head (with-handlers ([exn:fail:read? (lambda (_e) eof)]) (read in)))
        (unless (and (list? head) (= (length head) 3) (eq? (car head) 'pi2-gui-journal))
          (error 'read-journal "not a pi2 GUI journal: ~a" path))
        (define recs
          (let loop ([acc '()])
            (define d (with-handlers ([exn:fail:read? (lambda (_e) eof)]) (read in)))
            (if (or (eof-object? d) (not (and (list? d) (= (length d) 4))))
                (reverse acc)
                (loop (cons d acc)))))
        (values (third head) recs))))
) ; end define read-journal

;; 一条记录的单行摘要（调试面板 / 打印）
(define (record-summary kind payload)
  (define (clip s) (let ([s (regexp-replace* #rx"[\r\n\t]+" s " ")]) (if (> (string-length s) 90) (string-append (substring s 0 90) "…") s)))
  (case kind
    [(core send) (clip (if (hash? payload)
                           (string-join (for/list ([k '(type for kind id name path)] #:when (hash-ref payload k #f))
                                          (format "~a" (hash-ref payload k)))
                                        " ")
                           (format "~a" payload)))]
    [(error) (clip (if (hash? payload) (format "~a: ~a" (hash-ref payload 'where "?") (hash-ref payload 'message "?")) (format "~a" payload)))]
    [else (clip (format "~a" payload))])
) ; end define record-summary

(provide JOURNAL-VERSION journal-header-datum read-journal record-summary host-tag host-untag redact-datum
         sha256-hex attach-for-journal redact-send)

;; ---------------------------------------------------------------- 附件（P4）：日志里不记媒体内容

(define (sha256-hex bs)
  (apply string-append (for/list ([b (in-bytes (sha256-bytes bs))]) (string-append (if (< b 16) "0" "") (number->string b 16)))))

;; ev:attach 的 items（(vector kind 类型 名字 字节 大小 sha)）→ 字节换成 #f、补上 sha
(define (attach-for-journal items)
  (for/list ([it (in-list items)])
    (define bs (vector-ref it 3))
    (vector (vector-ref it 0) (vector-ref it 1) (vector-ref it 2) #f (vector-ref it 4)
            (or (vector-ref it 5) (and (bytes? bs) (sha256-hex bs))))))

;; 发给内核的请求 → 日志里记的样子：prompt 的媒体只留 kind media_type name sha（不记数据与探测出的 meta）
(define (redact-send j)
  (if (and (hash? j) (equal? (hash-ref j 'type #f) "prompt") (list? (hash-ref j 'media #f)))
      (hash-set j 'media (for/list ([m (in-list (hash-ref j 'media))] #:when (hash? m))
                           (for/hasheq ([k (in-list '(kind media_type name sha))] #:when (hash-has-key? m k)) (values k (hash-ref m k)))))
      j))

;; 多主机：日志里内核事件 / 请求带主机 id（键 '_host）；主机 "local" 不带（与单主机时的日志一致）
(define (host-tag id j) (if (or (equal? id "local") (not (hash? j))) j (hash-set j '_host id)))
(define (host-untag j)
  (if (and (hash? j) (hash-has-key? j '_host)) (cons (hash-ref j '_host) (hash-remove j '_host)) (cons "local" j)))

;; 日志记录里所有字符串中的 needle 换成 rep（列表、向量、hash、prefab 结构体逐层）：事后标记为密钥时清掉日志里的原文
(define (redact-datum v needle rep)
  (define (r x)
    (cond [(string? x) (if (string-contains? x needle) (string-replace x needle rep) x)]
          [(pair? x) (cons (r (car x)) (r (cdr x)))]
          [(vector? x) (for/vector #:length (vector-length x) ([y (in-vector x)]) (r y))]
          [(hash? x) (for/fold ([h (cond [(hash-eq? x) (hasheq)] [(hash-eqv? x) (hasheqv)] [else (hash)])]) ([(k y) (in-hash x)]) (hash-set h k (r y)))]
          [(prefab-struct-key x) => (lambda (k) (apply make-prefab-struct k (map r (cdr (vector->list (struct->vector x))))))]
          [else x]))
  (r v))
