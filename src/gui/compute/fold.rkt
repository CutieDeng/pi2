#lang racket/base
;; fold.rkt — 大段粘贴折叠（design-gui.md S13）。
;; 实际场景：用户往输入框里粘日志、报错栈、diff、整个文件。展开铺满输入框既看不清自己的指令，
;; 也难以编辑；折成一个 chip（原子单元：一次退格删掉、光标一步跨过），发送时展开原文。
;; 在编辑器字符串里，每个 chip 占一个 U+FFFC（OBJECT REPLACEMENT CHARACTER，富文本附件的标准占位符）。

(require racket/string racket/list "i18n.rkt" "secret-handle.rkt")

(define OBJ (integer->char #xFFFC))
(define OBJ-STR (string OBJ))

;; 达到任一阈值就折叠。短片段（几行代码、一条命令）留在原地以便直接改。
(define FOLD-MIN-LINES 8)
(define FOLD-MIN-CHARS 1000)

;; id：会话内递增编号（显示为 #id）；kind ∈ trace diff json log code text；
;; lines / bytes：原文行数与 UTF-8 字节数；preview：首个非空行（截断）
(struct fold (id content kind lines bytes preview) #:transparent)

;; 粘贴文本规范化：统一换行；去掉 U+FFFC（防止伪造 chip 占位符）
(define (normalize-paste s)
  (string-replace (regexp-replace* #rx"\r\n?" s "\n") OBJ-STR "")
) ; end define normalize-paste

(define (line-count s)
  (define t (if (and (> (string-length s) 0) (char=? (string-ref s (sub1 (string-length s))) #\newline))
                (substring s 0 (sub1 (string-length s)))
                s))
  (add1 (for/sum ([c (in-string t)]) (if (char=? c #\newline) 1 0)))
) ; end define line-count

(define (fold-worthy? s)
  (or (>= (line-count s) FOLD-MIN-LINES) (>= (string-length s) FOLD-MIN-CHARS))
) ; end define fold-worthy?

(define (ratio pred lines)
  (if (null? lines) 0 (/ (for/sum ([l (in-list lines)]) (if (pred l) 1 0)) (length lines)))
) ; end define ratio

;; 内容分类（启发式，只用于 chip 上的类型标签，不影响发送内容）
(define (classify s)
  (define lines (filter (lambda (l) (not (string=? (string-trim l) ""))) (string-split s "\n")))
  (define (rx r) (lambda (l) (regexp-match? r l)))
  (define head (string-trim s))
  (cond
    [(or (regexp-match? #rx"(?m:^diff --git )" s)
         (and (regexp-match? #rx"(?m:^@@ .* @@)" s) (regexp-match? #rx"(?m:^[-+])" s)))
     'diff]
    [(or (regexp-match? #rx"Traceback \\(most recent call last\\)" s)
         (> (ratio (rx #rx"^[ \t]+at .+[(:][^)]*[0-9]+\\)?$") lines) 0.2)
         (regexp-match? #rx"(?m:^[ \t]+File \".+\", line [0-9]+)" s)
         (regexp-match? #rx"panicked at " s))
     'trace]
    [(and (> (string-length head) 1)
          (memv (string-ref head 0) '(#\{ #\[))
          (memv (string-ref head (sub1 (string-length head))) '(#\} #\]))
          (regexp-match? #rx"\"[^\"]*\"[ \t]*:" s))
     'json]
;; 注意：{n} 计数量词只有 #px 支持
    [(> (ratio (rx #px"^\\[?[0-9]{4}-[0-9]{2}-[0-9]{2}|^\\[?[0-9]{2}:[0-9]{2}:[0-9]{2}|(^|[ \\[])(INFO|WARN|WARNING|ERROR|DEBUG|TRACE)([ \\]:]|$)") lines) 0.3)
     'log]
    [(> (ratio (rx #rx"[;{}]$|^[ \t]*(def|fn|func|function|class|import|from|#include|\\(define|let|const|var|pub|return|if|for)[ (]") lines) 0.3)
     'code]
    [else 'text])
) ; end define classify

(define (truncate-graphemes s n)
  (let loop ([i 0] [k 0])
    (cond
      [(>= i (string-length s)) s]
      [(= k n) (string-append (substring s 0 i) "…")]
      [else (loop (+ i (max 1 (string-grapheme-span s i))) (add1 k))]))
) ; end define truncate-graphemes

(define PREVIEW-MAX 36)

(define (make-fold id text)
  (define first-line (or (for/first ([l (in-list (string-split text "\n"))] #:unless (string=? (string-trim l) ""))
                           (string-trim l))
                         ""))
  (fold id text (classify text) (line-count text) (bytes-length (string->bytes/utf-8 text))
        (truncate-graphemes (regexp-replace* #rx"[ \t]+" first-line " ") PREVIEW-MAX))
) ; end define make-fold

;; ---------------------------------------------------------------- 密钥 chip
;; kind 'secret（确认是密钥）/ 'secret-pending（像密钥，等用户选）。content 是原值（只在 GUI 进程内存里）；preview 是句柄。
;; 发送时换成占位符 ⟦secret:句柄⟧（原值经 register_secret 另交内核）；展开面板不显示原值。
;; 回放时 content 本身就是占位符（日志里只有句柄）：占位符原样当占位符。

(define (placeholder? s) (and (string? s) (regexp-match? #rx"^⟦secret:[0-9a-f]+⟧$" s)))
(define (placeholder-handle s) (cadr (regexp-match #rx"^⟦secret:([0-9a-f]+)⟧$" s)))

(define (make-secret-fold id value pending?)
  (define v (string-trim value))
  (fold id v (if pending? 'secret-pending 'secret) 1 (bytes-length (string->bytes/utf-8 v))
        (if (placeholder? v) (placeholder-handle v) (secret-handle v)))
) ; end define make-secret-fold

(define (secret-fold? f) (and (memq (fold-kind f) '(secret secret-pending)) #t))

;; 发送 / 复制用的文本：密钥 → 占位符；其它 → 原文
(define (fold-send-text f)
  (if (secret-fold? f)
      (if (placeholder? (fold-content f)) (fold-content f) (string-append "⟦secret:" (fold-preview f) "⟧"))
      (fold-content f))
) ; end define fold-send-text

;; 粘贴内容像不像 key：去掉首尾空白后是一个不含空白的串，且符合常见格式（可在 {config-home}/secret-patterns.rktd 里加正则）
(define BUILTIN-SECRET-PATTERNS
  '("^sk-[A-Za-z0-9_-]{16,}$" "^sk-ant-[A-Za-z0-9_-]{20,}$" "^AKIA[0-9A-Z]{16}$" "^gh[pousr]_[A-Za-z0-9]{30,}$"
    "^github_pat_[A-Za-z0-9_]{30,}$" "^xox[abprs]-[A-Za-z0-9-]{10,}$" "^AIza[0-9A-Za-z_-]{30,}$" "^glpat-[A-Za-z0-9_-]{20,}$"))

(define (looks-like-secret? s [extra-patterns '()])
  (define v (string-trim s))
  (and (not (regexp-match? #rx"[ 	
]" v))
       (for/or ([p (in-list (append BUILTIN-SECRET-PATTERNS extra-patterns))])
         (with-handlers ([exn:fail? (lambda (_e) #f)]) (regexp-match? (pregexp p) v))))
) ; end define looks-like-secret?

;; ---------------------------------------------------------------- 附件 chip（P4）
;; kind 'image / 'audio / 'file；content ""（发送的文字里没有它，媒体经 prompt 的 'media 另交）；bytes = 文件大小；preview = 文件名。
;; 字节、sha、类型在拼装层的附件表里（按 fold id）。

(define (make-media-fold id kind name size)
  (fold id "" kind 0 size (truncate-graphemes name PREVIEW-MAX))
) ; end define make-media-fold

(define (media-fold? f) (and (memq (fold-kind f) '(image audio file)) #t))

;; chip 种类名（界面文字，按当前语言；diff / JSON 不翻译）
(define (kind-label k)
  (case k [(trace) (t 'fold.trace)] [(log) (t 'fold.log)] [(code) (t 'fold.code)] [(diff) "diff"] [(json) "JSON"] [else (t 'fold.text)])
) ; end define kind-label

(define (human-bytes n)
  (cond [(< n 1024) (format "~a B" n)]
        [(< n (* 1024 1024)) (format "~a KB" (/ (round (* 10 (/ n 1024.0))) 10))]
        [else (format "~a MB" (/ (round (* 10 (/ n 1048576.0))) 10))])
) ; end define human-bytes

;; chip 上的文字：「报错栈 #1 · Traceback (most recent call last): · 214 行」；密钥只显示句柄
(define (fold-label f)
  (case (fold-kind f)
    [(secret) (t 'fold.secret (fold-preview f))]
    [(secret-pending) (t 'fold.secret-pending (fold-preview f))]
    [(image audio file) (format "~a ~a · ~a" (case (fold-kind f) [(image) "🖼"] [(audio) "🔊"] [else "📎"]) (fold-preview f) (human-bytes (fold-bytes f)))]
    [else (t 'fold.label (kind-label (fold-kind f)) (fold-id f) (fold-preview f) (fold-lines f))])
) ; end define fold-label

(define (fold-summary f)
  (cond
    [(secret-fold? f) (t 'fold.secret-summary (fold-preview f))]
    [(media-fold? f) (format "~a · ~a" (fold-preview f) (human-bytes (fold-bytes f)))]
    [else
      (t 'fold.label-chars (kind-label (fold-kind f)) (fold-id f) (fold-lines f) (human-bytes (fold-bytes f)))])
) ; end define fold-summary

(provide OBJ OBJ-STR FOLD-MIN-LINES FOLD-MIN-CHARS
         (struct-out fold) normalize-paste line-count fold-worthy? classify make-fold
         make-secret-fold secret-fold? make-media-fold media-fold? fold-send-text looks-like-secret? placeholder? BUILTIN-SECRET-PATTERNS
         fold-label fold-summary human-bytes truncate-graphemes)
