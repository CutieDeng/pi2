#lang racket/base
;; markdown.rkt — Markdown 解析（计算层，CommonMark 的实用子集，按模型回答的实际写法取舍）。
;; 块：段落、ATX 标题、围栏代码（未闭合 = 流式中，一直延伸到末尾）、引用、有序 / 无序列表（列表项内容递归解析，
;;     支持嵌套与项内代码块）、GFM 表格、分隔线。
;; 行内：代码 span（反引号数匹配）、链接 [文字](url)、裸 URL、**粗** / __粗__、*斜* / _斜_（下划线只在词边界）、
;;       ~~删除线~~、反斜杠转义。段内单个换行 = 空格。
;; 不做：setext 标题、HTML、脚注、引用式链接、缩进式代码块（模型几乎只用围栏）。

(require racket/list racket/string)

;; ---------------------------------------------------------------- AST

;; 行内片段：styles ⊆ '(bold italic code link strike)
(struct span (text styles url) #:transparent)

(struct md-para (inlines) #:transparent)
(struct md-heading (level inlines) #:transparent)
(struct md-code (lang text closed?) #:transparent)
(struct md-list (ordered? start items) #:transparent)          ; items：(listof (listof block))
(struct md-quote (blocks) #:transparent)
(struct md-table (aligns header rows) #:transparent)           ; aligns：'left|'center|'right；header：(listof inlines)
(struct md-hr () #:transparent)

;; ---------------------------------------------------------------- 行内

(define (add-style sp s) (span (span-text sp) (if (memq s (span-styles sp)) (span-styles sp) (cons s (span-styles sp))) (span-url sp)))

;; 反斜杠转义：返回去掉转义符的文本（转义的标点不参与强调识别：替换为私用区占位再还原）
(define ESC-BASE #xE100)
(define (protect-escapes s)
  (regexp-replace* #px"\\\\([!-/:-@\\[-`{-~])" s
                   (lambda (all c) (string (integer->char (+ ESC-BASE (char->integer (string-ref c 0))))))))
(define (restore-escapes s)
  (list->string (for/list ([c (in-string s)])
                  (define n (char->integer c))
                  (if (<= ESC-BASE n (+ ESC-BASE 127)) (integer->char (- n ESC-BASE)) c))))

;; 强调：基于栈的定界符匹配（** / __ / * / _ / ~~）。输入：无代码 / 链接的纯文本片段。
(define (emphasis text base-styles url)
  (define toks   ; (list 'text str) | (list 'delim char count can-open can-close)
    (let loop ([i 0] [acc '()] [buf '()])
      (define (flush) (if (null? buf) acc (cons (list 'text (list->string (reverse buf))) acc)))
      (cond
        [(>= i (string-length text)) (reverse (flush))]
        [else
         (define c (string-ref text i))
         (cond
           [(memv c '(#\* #\_ #\~))
            (define j (let run ([j i]) (if (and (< j (string-length text)) (char=? (string-ref text j) c)) (run (add1 j)) j)))
            (define n (- j i))
            (define before (if (> i 0) (string-ref text (sub1 i)) #\space))
            (define after (if (< j (string-length text)) (string-ref text j) #\space))
            (define (ws? ch) (or (char-whitespace? ch)))
            (define (punct? ch) (and (not (char-alphabetic? ch)) (not (char-numeric? ch)) (not (ws? ch))))
            ;; CommonMark 左 / 右侧定界判定（简化）
            (define left (and (not (ws? after)) (or (not (punct? after)) (ws? before) (punct? before))))
            (define right (and (not (ws? before)) (or (not (punct? before)) (ws? after) (punct? after))))
            (define-values (open? close?)
              (cond [(char=? c #\_) (values (and left (or (not right) (punct? before))) (and right (or (not left) (punct? after))))]
                    [(char=? c #\~) (values (and left (= n 2)) (and right (= n 2)))]
                    [else (values left right)]))
            (if (or open? close?)
                (loop j (cons (list 'delim c n open? close?) (flush)) '())
                (loop j acc (append (reverse (string->list (substring text i j))) buf)))]
           [else (loop (add1 i) acc (cons c buf))])])))
  ;; 匹配：从左到右，遇到可闭合的定界符向栈里找同字符可开启的
  (define v (list->vector toks))
  (define n (vector-length v))
  (define pairs (make-hash))                                          ; 开 idx → (闭 idx . 样式)
  (let loop ([i 0] [stack '()])
    (when (< i n)
      (define t (vector-ref v i))
      (cond
        [(eq? (car t) 'delim)
         (define c (cadr t))
         (define cnt (caddr t))
         (define opener (and (list-ref t 4)
                             (for/first ([s (in-list stack)] #:when (and (char=? (cadr (vector-ref v s)) c)
                                                                      (or (char=? c #\~) (= (caddr (vector-ref v s)) cnt)
                                                                          (and (>= (caddr (vector-ref v s)) 2) (>= cnt 2)))))
                               s)))
         (cond
           [opener
            (define style (cond [(char=? c #\~) 'strike] [(>= cnt 3) 'bold-italic] [(= cnt 2) 'bold] [else 'italic]))
            (hash-set! pairs opener (cons i style))
            (loop (add1 i) (cdr (memv opener stack)))]
           [(list-ref t 3) (loop (add1 i) (cons i stack))]
           [else (loop (add1 i) stack)])]
        [else (loop (add1 i) stack)])))
  ;; 生成片段：未配对的定界符按原文输出
  (define closers (for/hash ([(o p) (in-hash pairs)]) (values (car p) #t)))
  (let gen ([i 0] [end n] [styles base-styles])
    (cond
      [(>= i end) '()]
      [else
       (define t (vector-ref v i))
       (cond
         [(and (eq? (car t) 'delim) (hash-ref pairs i #f))
          => (lambda (p)
               (define st (cdr p))
               (define st* (if (eq? st 'bold-italic) (list* 'bold 'italic styles) (cons st styles)))
               (append (gen (add1 i) (car p) st*) (gen (add1 (car p)) end styles)))]
         [(eq? (car t) 'delim)
          (cons (span (make-string (caddr t) (cadr t)) styles url) (gen (add1 i) end styles))]
         [else (cons (span (cadr t) styles url) (gen (add1 i) end styles))])]))
) ; end define emphasis

(define URL-RX #px"https?://[^\\s<>()\\[\\]]*[^\\s<>()\\[\\].,;:!?'\"]")

;; 行内解析：先切代码 span，再在非代码文本里切链接与裸 URL，其余做强调
(define (parse-inlines text0)
  (define text (protect-escapes (regexp-replace* #rx"[ \t]*\n[ \t]*" text0 " ")))
  (define (non-code s styles)
    ;; 链接 [t](u)、自动链接 <url>、裸 URL
    (let loop ([s s])
      (define m-first (let ([a (regexp-match-positions #px"\\[([^\\]\n]+)\\]\\(([^)\\s]+)\\)|<(https?://[^>\\s]+)>" s)]
                            [b (regexp-match-positions URL-RX s)])
                        (cond [(and a b) (if (<= (caar a) (caar b)) a b)] [else (or a b)])))
      (cond
        [(not m-first) (emphasis s styles #f)]
        [else
         (define start (caar m-first))
         (define end (cdar m-first))
         (define-values (label url)
           (cond [(and (> (length m-first) 2) (list-ref m-first 1))
                  (values (substring s (car (list-ref m-first 1)) (cdr (list-ref m-first 1)))
                          (substring s (car (list-ref m-first 2)) (cdr (list-ref m-first 2))))]
                 [(and (> (length m-first) 3) (list-ref m-first 3))
                  (define u (substring s (car (list-ref m-first 3)) (cdr (list-ref m-first 3)))) (values u u)]
                 [else (define u (substring s start end)) (values u u)]))
         (append (emphasis (substring s 0 start) styles #f)
                 (map (lambda (sp) (add-style (span (span-text sp) (span-styles sp) url) 'link)) (emphasis label styles url))
                 (loop (substring s end)))])))
  (define out
    (let loop ([s text])
      (define m (regexp-match-positions #px"(`+)" s))
      (cond
        [(not m) (non-code s '())]
        [else
         (define ticks (- (cdar m) (caar m)))
         (define open-end (cdar m))
         ;; 找同样长度的闭合反引号串
         (define close
           (let find ([from open-end])
             (define c (regexp-match-positions #px"`+" s from))
             (cond [(not c) #f]
                   [(= (- (cdar c) (caar c)) ticks) (car c)]
                   [else (find (cdar c))])))
         (cond
           [(not close) (append (non-code (substring s 0 open-end) '()) (loop (substring s open-end)))]
           [else
            (define code (let ([raw (substring s open-end (car close))])
                           (if (and (>= (string-length raw) 2) (char=? (string-ref raw 0) #\space)
                                    (char=? (string-ref raw (sub1 (string-length raw))) #\space)
                                    (regexp-match? #rx"[^ ]" raw))
                               (substring raw 1 (sub1 (string-length raw)))
                               raw)))
            (append (non-code (substring s 0 (caar m)) '())
                    (list (span code '(code) #f))
                    (loop (substring s (cdr close))))])])))
  ;; 还原转义；合并相邻同样式片段；去掉空片段
  (define restored (for/list ([sp (in-list out)] #:unless (string=? (span-text sp) ""))
                     (span (if (memq 'code (span-styles sp)) (restore-escapes-in-code (span-text sp)) (restore-escapes (span-text sp)))
                           (span-styles sp) (span-url sp))))
  (merge-spans restored)
) ; end define parse-inlines

;; 代码 span 里的反斜杠不是转义：把占位还原成「\ + 字符」
(define (restore-escapes-in-code s)
  (apply string-append
         (for/list ([c (in-string s)])
           (define n (char->integer c))
           (if (<= ESC-BASE n (+ ESC-BASE 127)) (string #\\ (integer->char (- n ESC-BASE))) (string c))))
) ; end define restore-escapes-in-code

(define (merge-spans sps)
  (let loop ([sps sps] [acc '()])
    (cond
      [(null? sps) (reverse acc)]
      [(and (pair? acc) (equal? (sort (span-styles (car acc)) symbol<?) (sort (span-styles (car sps)) symbol<?))
            (equal? (span-url (car acc)) (span-url (car sps))))
       (loop (cdr sps) (cons (span (string-append (span-text (car acc)) (span-text (car sps))) (span-styles (car acc)) (span-url (car acc))) (cdr acc)))]
      [else (loop (cdr sps) (cons (car sps) acc))]))
) ; end define merge-spans

(define (inline-text inlines) (apply string-append (map span-text inlines)))

;; ---------------------------------------------------------------- 块

(define FENCE-RX #px"^( {0,3})(`{3,}|~{3,})[ \t]*([^`\\s]*)")
(define HEADING-RX #px"^ {0,3}(#{1,6})(?:[ \t]+(.*?))?[ \t]*#*[ \t]*$")
(define HR-RX #px"^ {0,3}([-*_])([ \t]*\\1){2,}[ \t]*$")
(define QUOTE-RX #px"^ {0,3}>[ ]?(.*)$")
(define LIST-RX #px"^( *)([-*+]|[0-9]{1,9}[.)])([ \t]+(.*))?$")
(define TABLE-DELIM-RX #px"^[ \t]*\\|?[ \t]*:?-+:?[ \t]*(\\|[ \t]*:?-+:?[ \t]*)*\\|?[ \t]*$")

(define (blank? l) (regexp-match? #px"^[ \t]*$" l))
(define (indent-of l) (let loop ([i 0]) (if (and (< i (string-length l)) (char=? (string-ref l i) #\space)) (loop (add1 i)) i)))
(define (strip-indent l n) (substring l (min n (indent-of l))))

(define (split-row l)
  (define t (string-trim l))
  (define t1 (if (and (> (string-length t) 0) (char=? (string-ref t 0) #\|)) (substring t 1) t))
  (define t2 (if (and (> (string-length t1) 0) (char=? (string-ref t1 (sub1 (string-length t1))) #\|)) (substring t1 0 (sub1 (string-length t1))) t1))
  (map string-trim (regexp-split #px"(?<!\\\\)\\|" t2))
) ; end define split-row

(define (starts-block? l)
  (or (regexp-match? FENCE-RX l) (regexp-match? HEADING-RX l) (regexp-match? HR-RX l)
      (regexp-match? QUOTE-RX l) (regexp-match? LIST-RX l))
) ; end define starts-block?

;; 行表 → 块表
(define (parse-blocks lines)
  (let loop ([ls lines] [acc '()])
    (cond
      [(null? ls) (reverse acc)]
      [(blank? (car ls)) (loop (cdr ls) acc)]
      [else
       (define l (car ls))
       (cond
         ;; 围栏代码
         [(regexp-match FENCE-RX l)
          => (lambda (m)
               (define ind (string-length (cadr m)))
               (define fence (caddr m))
               (define close-rx (pregexp (format "^ {0,3}~a~a*[ \t]*$" (regexp-quote (substring fence 0 1)) (regexp-quote fence))))
               (let scan ([rest (cdr ls)] [body '()])
                 (cond
                   [(null? rest) (loop '() (cons (md-code (cadddr m) (string-join (reverse body) "\n") #f) acc))]
                   [(regexp-match? close-rx (car rest)) (loop (cdr rest) (cons (md-code (cadddr m) (string-join (reverse body) "\n") #t) acc))]
                   [else (scan (cdr rest) (cons (strip-indent (car rest) ind) body))])))]
         ;; 标题
         [(regexp-match HEADING-RX l)
          => (lambda (m) (loop (cdr ls) (cons (md-heading (string-length (cadr m)) (parse-inlines (or (caddr m) ""))) acc)))]
         ;; 分隔线（须在列表之前判断：「- - -」「***」）
         [(regexp-match? HR-RX l) (loop (cdr ls) (cons (md-hr) acc))]
         ;; 引用：收集连续的 > 行（懒续行：非空非块起始行并入）
         [(regexp-match? QUOTE-RX l)
          (let scan ([rest ls] [body '()])
            (cond
              [(and (pair? rest) (regexp-match QUOTE-RX (car rest))) => (lambda (m) (scan (cdr rest) (cons (cadr m) body)))]
              [(and (pair? rest) (pair? body) (not (blank? (car rest))) (not (starts-block? (car rest))) (not (blank? (car body))))
               (scan (cdr rest) (cons (car rest) body))]
              [else (loop rest (cons (md-quote (parse-blocks (reverse body))) acc))]))]
         ;; 列表
         [(regexp-match LIST-RX l) (define-values (blk rest) (parse-list ls)) (loop rest (cons blk acc))]
         ;; 表格：表头行 + 分隔行
         [(and (pair? (cdr ls)) (regexp-match? #rx"\\|" l) (regexp-match? TABLE-DELIM-RX (cadr ls)))
          (define header (split-row l))
          (define aligns (for/list ([d (in-list (split-row (cadr ls)))])
                           (define left? (regexp-match? #rx"^:" d))
                           (define right? (regexp-match? #rx":$" d))
                           (cond [(and left? right?) 'center] [right? 'right] [else 'left])))
          (let scan ([rest (cddr ls)] [rows '()])
            (cond
              [(and (pair? rest) (not (blank? (car rest))) (regexp-match? #rx"\\|" (car rest)))
               (scan (cdr rest) (cons (split-row (car rest)) rows))]
              [else
               (define ncol (length header))
               (define (fit cells) (for/list ([i (in-range ncol)]) (parse-inlines (if (< i (length cells)) (list-ref cells i) ""))))
               (loop rest (cons (md-table (take (append aligns (make-list ncol 'left)) ncol) (fit header) (map fit (reverse rows))) acc))]))]
         ;; 段落：到空行或其它块开始为止
         [else
          (let scan ([rest (cdr ls)] [body (list l)])
            (cond
              [(and (pair? rest) (not (blank? (car rest))) (not (starts-block? (car rest)))
                    (not (and (pair? (cdr rest)) (regexp-match? #rx"\\|" (car rest)) (regexp-match? TABLE-DELIM-RX (cadr rest)))))
               (scan (cdr rest) (cons (car rest) body))]
              [else (loop rest (cons (md-para (parse-inlines (string-join (reverse body) "\n"))) acc))]))])]))
) ; end define parse-blocks

;; 列表：同一层的项（缩进与首项相同的项标记）；项内容 = 首行 + 缩进 ≥ 内容缩进的后续行（去掉该缩进后递归解析）
(define (parse-list ls)
  (define m0 (regexp-match LIST-RX (car ls)))
  (define base (string-length (cadr m0)))
  (define ordered? (regexp-match? #px"[0-9]" (caddr m0)))
  (define start (if ordered? (string->number (regexp-replace #px"[.)]" (caddr m0) "")) 1))
  (let items ([ls ls] [acc '()])
    (define m (and (pair? ls) (regexp-match LIST-RX (car ls))))
    (cond
      [(and m (= (string-length (cadr m)) base) (eq? ordered? (regexp-match? #px"[0-9]" (caddr m))))
       (define content-indent (+ base (string-length (caddr m)) (if (cadddr m) (- (string-length (cadddr m)) (string-length (or (list-ref m 4) ""))) 1)))
       (define first-line (or (list-ref m 4) ""))
       (let body ([rest (cdr ls)] [lines (list first-line)] [pending-blank 0])
         (cond
           [(null? rest) (items '() (cons (parse-blocks (reverse lines)) acc))]
           [(blank? (car rest)) (body (cdr rest) lines (add1 pending-blank))]
           [(>= (indent-of (car rest)) content-indent)
            (body (cdr rest) (cons (strip-indent (car rest) content-indent) (append (make-list pending-blank "") lines)) 0)]
           [(and (zero? pending-blank) (not (starts-block? (car rest))) (> (indent-of (car rest)) base))
            (body (cdr rest) (cons (string-trim (car rest)) lines) 0)]                 ; 懒续行
           [(and (zero? pending-blank) (not (starts-block? (car rest))) (not (regexp-match? LIST-RX (car rest))))
            (body (cdr rest) (cons (string-trim (car rest)) lines) 0)]
           [else (items rest (cons (parse-blocks (reverse lines)) acc))]))]
      [else (values (md-list ordered? start (reverse acc)) ls)]))
) ; end define parse-list

;; 整篇：→ 块表
(define (parse-markdown text)
  (parse-blocks (regexp-split #rx"\n" (regexp-replace* #rx"\r\n?" text "\n")))
) ; end define parse-markdown

(provide (struct-out span) (struct-out md-para) (struct-out md-heading) (struct-out md-code) (struct-out md-list)
         (struct-out md-quote) (struct-out md-table) (struct-out md-hr)
         parse-markdown parse-inlines inline-text)
