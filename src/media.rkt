#lang tstring racket
;; media.rkt — 消息里的媒体（design-content.md §3.3）：blob 存储、定期垃圾回收、按模型能力投影。
;;
;; blob 存储：会话目录下 blobs/ab/cdef…（sha256 十六进制，前两位分目录）；同一内容只存一份。写入先写临时文件再改名（原子）。
;; 垃圾回收（决定 3：定期扫描，不用引用计数）：列出会话目录里所有会话文件引用的 sha，删掉没被引用、且修改时间早于宽限期的 blob；
;;   上次回收的时刻记在 blobs/.last-gc，间隔不到 GC-INTERVAL-S 不回收。
;; 能力：按模型名判断能不能收图片 / 音频（缺省表 + {config-home}/media-caps.rktd 覆盖：((模型正则 . (hasheq 'image #t 'audio #f)) …)）。
;;   不能收的媒体投影成文字占位（「[image: a.png, 1024×768]」「[audio: memo.m4a, 12.3 s] 转写…」）；原件始终在 blob 里，界面显示原件。

(require file/sha1 net/base64 racket/file (file "model.rkt") (file "credentials.rkt"))

(define GC-INTERVAL-S (* 24 3600))
(define GC-GRACE-S 3600)                               ; 刚写入、还没进会话记录的 blob（发送途中）不删

;; 当前会话的 blob 根目录（rpc 轮线程里 parameterize；provider 投影时读 blob 用）
(define current-blob-root (make-parameter #f))

(define (blob-root-of session-dir) (build-path session-dir "blobs"))
(define (valid-sha? s) (and (string? s) (regexp-match? #px"^[0-9a-f]{64}$" s)))
(define (blob-file root sha) (build-path root (substring sha 0 2) (substring sha 2)))

(define (blob-put! root bs)
  (define sha (sha256-hex bs))
  (define f (blob-file root sha))
  (unless (file-exists? f)
    (make-directory* (build-path root (substring sha 0 2)))
    (define tmp (path-add-extension f (string->bytes/utf-8 (format ".tmp~a" (random 1000000)))))
    (call-with-output-file tmp #:exists 'truncate (lambda (out) (write-bytes bs out)))
    (rename-file-or-directory tmp f #t))
  sha
) ; end define blob-put!

(define (sha256-hex bs) (bytes->hex-string (sha256-bytes bs)))

(define (blob-bytes root sha)
  (and root (valid-sha? sha) (let ([f (blob-file root sha)]) (and (file-exists? f) (file->bytes f)))))
(define (blob-size root sha)
  (and root (valid-sha? sha) (let ([f (blob-file root sha)]) (and (file-exists? f) (file-size f)))))

;; 分块读（RPC blob 请求）：→ (values 字节 全长) 或 (values #f #f)
(define (blob-read-range root sha offset len)
  (define f (and (valid-sha? sha) (blob-file root sha)))
  (cond
    [(and f (file-exists? f))
     (define total (file-size f))
     (define o (max 0 (min offset total)))
     (define n (max 0 (min len (- total o))))
     (values (call-with-input-file f (lambda (in) (file-position in o) (let ([b (read-bytes n in)]) (if (eof-object? b) #"" b)))) total)]
    [else (values #f #f)])
) ; end define blob-read-range

;; ---------------------------------------------------------------- 引用与垃圾回收

;; 一个值里引用的 sha（消息、压缩记录的历史、media meta 里的缩略图 'thumb）
(define (media-refs v)
  (define acc '())
  (let walk ([v v])
    (cond [(media-block? v)
           (set! acc (cons (media-block-sha v) acc))
           (let ([t (and (hash? (media-block-meta v)) (hash-ref (media-block-meta v) 'thumb #f))]) (when (valid-sha? t) (set! acc (cons t acc))))]
          [(message? v) (for-each walk (message-blocks v))]
          [(pair? v) (walk (car v)) (walk (cdr v))]
          [(hash? v) (for ([x (in-hash-values v)]) (walk x))]
          [(and (prefab-struct-key v) (struct? v)) (for ([x (in-vector (struct->vector v))]) (walk x))]
          [else (void)]))
  acc
) ; end define media-refs

;; 会话目录里所有会话文件引用的 sha 集合。read-records：(路径 → 记录表)（session.rkt 注入：经索引只读带媒体的记录）
(define (referenced-shas session-dir read-records)
  (define h (make-hash))
  (for ([f (in-list (if (directory-exists? session-dir) (directory-list session-dir #:build? #t) '()))]
        #:when (regexp-match? #rx"[.]rktd$" (path->string f)))
    (for ([r (in-list (with-handlers ([exn:fail? (lambda (_e) '())]) (read-records f)))])
      (for ([sha (in-list (media-refs r))]) (hash-set! h sha #t))))
  h
) ; end define referenced-shas

;; → 删掉的 blob 数。force?：不看间隔
(define (blobs-gc! session-dir read-records #:force? [force? #f] #:grace-s [grace GC-GRACE-S] #:now [now (current-seconds)])
  (define root (blob-root-of session-dir))
  (define stamp (build-path root ".last-gc"))
  (define last (and (file-exists? stamp) (file-or-directory-modify-seconds stamp)))
  (cond
    [(not (directory-exists? root)) 0]
    [(and (not force?) last (< (- now last) GC-INTERVAL-S)) 0]
    [else
     (define refs (referenced-shas session-dir read-records))
     (define n
       (for*/sum ([d (in-list (directory-list root #:build? #t))] #:when (directory-exists? d)
                  [f (in-list (directory-list d #:build? #t))])
         (define-values (_b name _d) (split-path d))
         (define-values (_b2 rest _d2) (split-path f))
         (define sha (string-append (path->string name) (path->string rest)))
         (cond [(and (valid-sha? sha) (not (hash-ref refs sha #f)) (< (file-or-directory-modify-seconds f) (- now grace)))
                (delete-file f) 1]
               [(and (regexp-match? #rx"[.]tmp[0-9]+$" (path->string f)) (< (file-or-directory-modify-seconds f) (- now grace)))
                (delete-file f) 0]
               [else 0])))
     (call-with-output-file stamp #:exists 'truncate (lambda (out) (write now out)))
     n])
) ; end define blobs-gc!

;; ---------------------------------------------------------------- 能力与投影

;; 缺省：能收图片的模型（名字匹配）；音频缺省都不收
(define DEFAULT-CAPS
  (list (cons #px"(?i:claude|gpt-4o|gpt-4\\.1|gpt-5|\\bo3\\b|\\bo4|gemini|grok-4|vision|gemma-3|gemma-4|qwen[^/]*vl|llava|pixtral|minicpm-v|glm-4\\.[0-9]+v|kimi-vl)"
              (hasheq 'image #t 'audio #f))
        (cons #px"(?i:gpt-4o-audio|gpt-realtime)" (hasheq 'image #t 'audio #t))))

(define (caps-file) (build-path (config-home) "media-caps.rktd"))

(define (load-caps-overrides)
  (with-handlers ([exn:fail? (lambda (_e) '())])
    (define v (if (file-exists? (caps-file)) (call-with-input-file (caps-file) (lambda (in) (parameterize ([read-accept-reader #f]) (read in)))) '()))
    (for/list ([pr (in-list (if (list? v) v '()))] #:when (and (pair? pr) (string? (car pr)) (hash? (cdr pr))))
      (cons (pregexp (car pr)) (cdr pr)))))

;; → (hasheq 'image bool 'audio bool)；覆盖表里先匹配的优先，然后缺省表（最后一条匹配的胜出：音频那条更具体）
(define (media-caps model)
  (define m (if (string? model) model ""))
  (or (for/first ([pr (in-list (load-caps-overrides))] #:when (regexp-match? (car pr) m)) (cdr pr))
      (for/fold ([c (hasheq 'image #f 'audio #f)]) ([pr (in-list DEFAULT-CAPS)])
        (if (regexp-match? (car pr) m) (cdr pr) c)))
) ; end define media-caps

(define (fmt-seconds s) (format "~a s" (/ (round (* 10 s)) 10.0)))

;; 收不了的媒体 → 文字占位（给模型的）
(define (media-placeholder b)
  (define meta (if (hash? (media-block-meta b)) (media-block-meta b) (hasheq)))
  (define name (or (media-block-name b) (media-block-media-type b)))
  (define dims (let ([w (hash-ref meta 'width #f)] [h (hash-ref meta 'height #f)]) (and (real? w) (real? h) (format "~a×~a" w h))))
  (define dur (let ([d (hash-ref meta 'duration #f)]) (and (real? d) (fmt-seconds d))))
  (define tr (let ([x (hash-ref meta 'transcript #f)]) (and (string? x) (not (string=? x "")) x)))
  (string-append "[" (symbol->string (media-block-kind b)) ": " name
                 (if dims (string-append ", " dims) "") (if dur (string-append ", " dur) "") "]"
                 (if tr (string-append " transcript: " tr) ""))
) ; end define media-placeholder

;; 能收的图片：base64 数据（blob 读不到 → #f，调用方退回占位）
(define (media-base64 b)
  (define bs (blob-bytes (current-blob-root) (media-block-sha b)))
  (and bs (bytes->string/latin-1 (base64-encode bs #"")))
) ; end define media-base64

;; 消息里的媒体按能力分组：→ (values 能直接发的图片表 占位文字表)
(define (split-media blocks model)
  (define caps (media-caps model))
  (for/fold ([imgs '()] [phs '()] #:result (values (reverse imgs) (reverse phs)))
            ([b (in-list blocks)] #:when (media-block? b))
    (define data (and (eq? (media-block-kind b) 'image) (hash-ref caps 'image #f) (media-base64 b)))
    (if data (values (cons (cons b data) imgs) phs) (values imgs (cons (media-placeholder b) phs))))
) ; end define split-media

(define (has-media? m) (ormap media-block? (message-blocks m)))

(provide current-blob-root blob-root-of valid-sha? blob-file blob-put! blob-bytes blob-size blob-read-range sha256-hex
         media-refs referenced-shas blobs-gc! GC-INTERVAL-S
         media-caps media-placeholder media-base64 split-media has-media?)
