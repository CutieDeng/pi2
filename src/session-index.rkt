#lang tstring racket
;; session-index.rkt — 会话记录的索引（<会话>.rktd.idx；design-content.md §4.1）：每条记录一个 32 字节的定长条目，
;; 用于随机访问与局部加载（只读需要的记录，不全量解析会话文件）。
;; 文件头 16 字节："PI2IDX01" + 8 字节保留。条目（little-endian）：
;;   偏移 u64 | 字节数 u32 | 类型 u8 | 角色 u8 | 标志 u16 | 文字长度 u32 | 行数 u32 | 时间（秒）u32 | 保留 u32
;; 索引由会话文件派生：没有 / 头不对 → 重建；只覆盖到文件中间（旧版本在后面追加过）→ 从覆盖处往后补；
;; 覆盖超过文件长度（文件被替换过）→ 重建。只有写会话的那个进程写索引文件；只读的打开在内存里补，不写。

(require racket/date (file "model.rkt") (file "rktd.rkt"))

(define MAGIC #"PI2IDX01")
(define HEADER 16)
(define ENTRY 32)

;; type：记录类型码；role：msg 的角色 / turn 的 begin end revert；flags：msg 里有 thinking / 工具调用 / 媒体 / 出错的工具结果
(struct ientry (offset len type role flags text-len lines ts) #:transparent)

(define TYPES '#(other meta msg usage compact turn diff fork route effect perm perm-answer note req tool partial sub title workspace instructions memory memory_used tests))
(define (type-code sym) (or (for/first ([s (in-vector TYPES)] [i (in-naturals)] #:when (eq? s sym)) i) 0))
(define (type-name code) (if (< code (vector-length TYPES)) (vector-ref TYPES code) 'other))

(define ROLE-USER 1) (define ROLE-ASSISTANT 2) (define ROLE-RESULTS 3) (define ROLE-SYNTHETIC 4)
(define FLAG-THINKING 1) (define FLAG-TOOL-USE 2) (define FLAG-MEDIA 4) (define FLAG-TOOL-ERROR 8)

;; 程序合成的用户消息（中断标记、压缩摘要、工具额度用完的提示）：界面当提示显示
(define (synthetic-text? s)
  (regexp-match? #rx"^(<turn_aborted>|\\[compacted context summary\\]|Tool budget exhausted)" s))

(define (iso->secs ts)
  (define m (and (string? ts) (regexp-match #px"^(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})" ts)))
  (if m
      (with-handlers ([exn:fail? (lambda (_e) 0)])
        (apply find-seconds (map string->number (list (list-ref m 6) (list-ref m 5) (list-ref m 4) (list-ref m 3) (list-ref m 2) (list-ref m 1)))))
      0))

(define (count-lines s) (add1 (for/sum ([c (in-string s)]) (if (char=? c #\newline) 1 0))))

;; 记录 → 条目（偏移、字节数由调用方给）
(define (classify r offset len)
  (define-values (type data ts)
    (if (and (prefab-struct-key r) (eq? (prefab-struct-key r) 'rec))
        (let ([v (struct->vector r)]) (values (vector-ref v 1) (vector-ref v 3) (vector-ref v 2)))
        (values 'other #f #f)))
  (define code (type-code type))
  (define-values (role flags tlen lines)
    (case type
      [(msg)
       (cond
         [(message? data)
          (define bs (message-blocks data))
          (define texts (for/list ([b (in-list bs)])
                          (cond [(text-block? b) (text-block-text b)]
                                [(tool-result-block? b) (let ([c (tool-result-block-content b)]) (if (string? c) c ""))]
                                [else ""])))
          (define all (apply string-append texts))
          (define results? (and (pair? bs) (andmap tool-result-block? bs)))
          (define role (case (message-role data)
                         [(assistant) ROLE-ASSISTANT]
                         [else (cond [results? ROLE-RESULTS] [(synthetic-text? all) ROLE-SYNTHETIC] [else ROLE-USER])]))
          (define flags (bitwise-ior (if (ormap thinking-block? bs) FLAG-THINKING 0)
                                     (if (ormap tool-use-block? bs) FLAG-TOOL-USE 0)
                                     (if (ormap (lambda (b) (or (image-block? b) (media-block? b))) bs) FLAG-MEDIA 0)
                                     (if (ormap (lambda (b) (and (tool-result-block? b) (tool-result-block-is-error? b))) bs) FLAG-TOOL-ERROR 0)))
          (values role flags (string-length all) (count-lines all))]
         [else (values 0 0 0 0)])]
      [(turn) (values (case data [(begin) 1] [(end) 2] [(revert) 3] [else 0]) 0 0 0)]
      [(note partial)
       (define t (or (and (hash? data) (hash-ref data 'text #f)) ""))
       (values 0 0 (string-length t) (count-lines t))]
      [else (values 0 0 0 0)]))
  (ientry offset len code role flags (min tlen #xFFFFFFFF) (min lines #xFFFFFFFF) (min (iso->secs ts) #xFFFFFFFF))
) ; end define classify

(define (entry->bytes e)
  (define b (make-bytes ENTRY 0))
  (integer->integer-bytes (ientry-offset e) 8 #f #f b 0)
  (integer->integer-bytes (ientry-len e) 4 #f #f b 8)
  (bytes-set! b 12 (ientry-type e))
  (bytes-set! b 13 (ientry-role e))
  (integer->integer-bytes (ientry-flags e) 2 #f #f b 14)
  (integer->integer-bytes (ientry-text-len e) 4 #f #f b 16)
  (integer->integer-bytes (ientry-lines e) 4 #f #f b 20)
  (integer->integer-bytes (ientry-ts e) 4 #f #f b 24)
  b
) ; end define entry->bytes

(define (bytes->entry b i)
  (define o (+ HEADER (* i ENTRY)))
  (ientry (integer-bytes->integer b #f #f o (+ o 8))
          (integer-bytes->integer b #f #f (+ o 8) (+ o 12))
          (bytes-ref b (+ o 12)) (bytes-ref b (+ o 13))
          (integer-bytes->integer b #f #f (+ o 14) (+ o 16))
          (integer-bytes->integer b #f #f (+ o 16) (+ o 20))
          (integer-bytes->integer b #f #f (+ o 20) (+ o 24))
          (integer-bytes->integer b #f #f (+ o 24) (+ o 28)))
) ; end define bytes->entry

;; ---------------------------------------------------------------- 索引对象

;; entries：可增长向量（vec + n）；port：可写时的追加口（只读打开为 #f）
(struct sidx (log-path [vec #:mutable] [n #:mutable] [port #:mutable]))

(define (index-path log-path) (string-append (if (path? log-path) (path->string log-path) log-path) ".idx"))

(define (push! ix e)
  (when (= (sidx-n ix) (vector-length (sidx-vec ix)))
    (define nv (make-vector (max 64 (* 2 (vector-length (sidx-vec ix)))) #f))
    (vector-copy! nv 0 (sidx-vec ix) 0 (sidx-n ix))
    (set-sidx-vec! ix nv))
  (vector-set! (sidx-vec ix) (sidx-n ix) e)
  (set-sidx-n! ix (add1 (sidx-n ix)))
) ; end define push!

(define (index-count ix) (sidx-n ix))
(define (index-ref ix i) (vector-ref (sidx-vec ix) i))
(define (index-covered ix) (if (zero? (sidx-n ix)) 0 (let ([e (index-ref ix (sub1 (sidx-n ix)))]) (+ (ientry-offset e) (ientry-len e)))))

;; 从日志的 start 字节处往后顺序读，逐条建条目（字节数含其后的空白，覆盖处 = 下一条的开头）
(define (scan-from! ix start)
  (define path (sidx-log-path ix))
  (when (file-exists? path)
    (call-with-input-file path
      (lambda (in)
        (file-position in start)
        (let loop ()
          (skip-ws! in)
          (define off (file-position in))
          (define d (datum-log-read-at in off))
          (unless (eof-object? d)
            (skip-ws! in)
            (define e (classify d off (- (file-position in) off)))
            (push! ix e)
            (when (sidx-port ix) (write-bytes (entry->bytes e) (sidx-port ix)))
            (loop))))))
  (when (sidx-port ix) (flush-output (sidx-port ix)))
) ; end define scan-from!

(define (skip-ws! in)
  (let loop () (define c (peek-char in)) (when (and (char? c) (char-whitespace? c)) (read-char in) (loop))))

;; 打开（必要时建 / 补 / 重建）。writable?：写会话的进程（同时更新索引文件）
(define (index-open log-path #:writable? [writable? #f])
  (define ipath (index-path log-path))
  (define log-size (if (file-exists? log-path) (file-size log-path) 0))
  (define existing
    (and (file-exists? ipath)
         (let ([b (file->bytes ipath)])
           (and (>= (bytes-length b) HEADER) (equal? (subbytes b 0 8) MAGIC)
                (zero? (remainder (- (bytes-length b) HEADER) ENTRY))
                b))))
  (define ix (sidx log-path (make-vector 64 #f) 0 #f))
  (when existing
    (for ([i (in-range (quotient (- (bytes-length existing) HEADER) ENTRY))]) (push! ix (bytes->entry existing i))))
  (define stale-long? (> (index-covered ix) log-size))           ; 文件被替换过
  (when (or (not existing) stale-long?)
    (set-sidx-vec! ix (make-vector 64 #f)) (set-sidx-n! ix 0))
  (when writable?
    (cond
      [(or (not existing) stale-long?)
       (define p (open-output-file ipath #:exists 'truncate))
       (write-bytes MAGIC p) (write-bytes (make-bytes 8 0) p) (flush-output p)
       (set-sidx-port! ix p)]
      [else (set-sidx-port! ix (open-output-file ipath #:exists 'append))]))
  (when (< (index-covered ix) log-size) (scan-from! ix (index-covered ix)))  ; 补上没进索引的记录
  ix
) ; end define index-open

;; 写会话时：记录刚写进日志（偏移、字节数）→ 追加条目
(define (index-append! ix r offset len)
  (define e (classify r offset len))
  (push! ix e)
  (when (sidx-port ix) (write-bytes (entry->bytes e) (sidx-port ix)) (flush-output (sidx-port ix)))
  e
) ; end define index-append!

(define (index-close! ix) (when (sidx-port ix) (close-output-port (sidx-port ix)) (set-sidx-port! ix #f)))

;; 按条目序号读记录：→ (listof (cons 序号 记录))；只打开一次文件，按偏移跳读
(define (index-read ix positions)
  (if (null? positions) '()
      (call-with-input-file (sidx-log-path ix)
        (lambda (in)
          (for/list ([i (in-list positions)])
            (cons i (datum-log-read-at in (ientry-offset (index-ref ix i)))))))))

;; 某类型的条目序号（如只读 effect / route / diff 记录）
(define (index-positions ix pred)
  (for/list ([i (in-range (index-count ix))] #:when (pred (index-ref ix i))) i))

(provide (struct-out ientry) type-code type-name index-path index-open index-append! index-close!
         index-count index-ref index-covered index-read index-positions classify
         ROLE-USER ROLE-ASSISTANT ROLE-RESULTS ROLE-SYNTHETIC FLAG-THINKING FLAG-TOOL-USE FLAG-MEDIA FLAG-TOOL-ERROR)
