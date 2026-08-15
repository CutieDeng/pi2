#lang tstring racket
;; shparse.rkt — 内置 bash 子集解析器（design-execguard.md §1，v2）。
;;
;; 纯 Racket 递归下降，零外挂进程。目标不是复刻 bash 全语义，而是给 cmdscan
;; 提供一棵**可安全遍历**的 AST：引号/展开在词内结构化（$(…)、``、${…}、$((…))、
;; <(…)/>(…) 的内嵌命令体收集为 subs 字符串，由判定器递归判级）；heredoc 正文
;; 被正确吞掉并保留（未引号定界符时正文里的展开也会被收集）；复合命令
;; （if/while/until/for/case/{ }/( )/[[ ]]/(( ))）与函数定义成结构节点。
;; 解析失败一律抛 exn:fail（'shparse）——调用方按 opaque 处理，fail-closed。
;;
;; 已知子集边界（超出即报错→opaque）：select/coproc 无专门支持（coproc 落普通
;; 词→默认拒绝）、扩展 glob、历史展开（非交互 bash 本就不启用）。

(require
 racket/string
 racket/list
) ; end require

;; tstring reader 下 #\" / #\` 字面量会破坏字符串扫描，经常量引用。
(define DQUOTE (integer->char 34))
(define BTICK  (integer->char 96))

;; ---------------------------------------------------------------- AST

;; word：text = 静态部分拼接（展开处贡献空串）；static? = 无任何运行期展开；
;; glob? = 含未引号 * ?；param? = 含参数展开；quoted? = 含引号（heredoc 定界符用）；
;; subs = 内嵌命令体字符串（$(…) `…` <(…) >(…) 与 ${…}/$((…)) 内部的命令替换）。
(struct sh-word (text static? glob? param? quoted? subs) #:transparent)

;; redir：op ∈ ">" ">>" ">|" ">&" "&>" "&>>" "<" "<&" "<<" "<<-" "<<<"
;; target = sh-word（heredoc 时为定界符词）；hbody = box（heredoc 正文，延迟填充）。
(struct sh-redir (fd op target hbody) #:transparent)

(struct sh-simple (assigns words redirs) #:transparent)  ; assigns: listof sh-word（值）
(struct sh-pipe (cmds) #:transparent)
(struct sh-seq (cmds) #:transparent)
;; compound：kind ∈ 'subshell 'group 'if 'while 'until 'for 'case 'cond 'arith
;; seqs = 内部命令序列；words = 附属词（for 的 in 列表 / case 主词与模式 / cond 词 /
;; arith 用零文本词携带 subs）。
(struct sh-compound (kind seqs words redirs) #:transparent)
(struct sh-fundef (name body) #:transparent)

;; ---------------------------------------------------------------- 解析器状态

(struct pst (s [pos #:mutable] [pending #:mutable]))
;; pending: (listof (list hbody-box op delim-text quoted?))，行尾按序吞正文。

(define (perr st msg)
  (error 'shparse "~a (at offset ~a)" msg (pst-pos st))
) ; end define perr

(define (at-end? st) (>= (pst-pos st) (string-length (pst-s st))))
(define (pk st [k 0])
  (define i (+ (pst-pos st) k))
  (and (< i (string-length (pst-s st))) (string-ref (pst-s st) i))
) ; end define pk
(define (adv! st [n 1]) (set-pst-pos! st (+ (pst-pos st) n)))

(define (looking-at? st prefix)
  (define s (pst-s st)) (define p (pst-pos st))
  (and (<= (+ p (string-length prefix)) (string-length s))
       (string=? (substring s p (+ p (string-length prefix))) prefix))
) ; end define looking-at?

(define (meta-char? ch)
  (and ch (memv ch (list #\space #\tab #\newline #\; #\& #\| #\( #\) #\< #\>)))
) ; end define meta-char?

;; 空白（不含换行）；`\`+换行 续行；词首 # 注释吞到行尾。
(define (skip-blanks! st)
  (let loop ()
    (define ch (pk st))
    (cond
      [(and ch (memv ch (list #\space #\tab))) (adv! st) (loop)]
      [(and (eqv? ch #\\) (eqv? (pk st 1) #\newline)) (adv! st 2) (loop)]
      [(eqv? ch #\#)
       (let skip ()
         (define c2 (pk st))
         (when (and c2 (not (char=? c2 #\newline))) (adv! st) (skip)))
       (loop)]
      [else (void)]
    ) ; end cond
  ) ; end let loop
) ; end define skip-blanks!

;; 读一行（吞换行），返回不含换行的内容；EOF 返回 #f（内容空时）。
(define (read-line! st)
  (if (at-end? st)
      #f
      (let loop ([acc '()])
        (define ch (pk st))
        (cond
          [(not ch) (list->string (reverse acc))]
          [(char=? ch #\newline) (adv! st) (list->string (reverse acc))]
          [else (adv! st) (loop (cons ch acc))]
        ) ; end cond
      ) ; end let loop
  ) ; end if
) ; end define read-line!

;; 消费一个换行：吞掉全部待处理 heredoc 正文。
(define (consume-newline! st)
  (adv! st)                                            ; 换行本身
  (define pend (pst-pending st))
  (set-pst-pending! st '())
  (for ([h (in-list pend)])
    (define hbox (first h)) (define op (second h)) (define delim (third h))
    (let gather ([lines '()])
      (define ln (read-line! st))
      (define ln* (if (and ln (string=? op "<<-")) (string-trim ln "\t" #:right? #f) ln))
      (cond
        [(not ln*) (perr st f"heredoc delimiter not found: {delim}")]
        [(string=? ln* delim) (set-box! hbox (string-join (reverse lines) "\n"))]
        [else (gather (cons (or ln "") lines))]
      ) ; end cond
    ) ; end let gather
  ) ; end for
) ; end define consume-newline!

(define (skip-linebreaks! st)
  (let loop ()
    (skip-blanks! st)
    (when (eqv? (pk st) #\newline) (consume-newline! st) (loop))
  ) ; end let loop
) ; end define skip-linebreaks!

;; ---------------------------------------------------------------- 平衡扫描

;; pos 处于开括号之后；吃到与之配对的 `)`（不含）并前进过它。返回内容。
;; 引号内的括号不计数；反斜杠转义跳过。
(define (read-balanced-paren! st)
  (define out (open-output-string))
  (let loop ([depth 1] [mode 'plain])
    (define ch (pk st))
    (cond
      [(not ch) (perr st "unbalanced parenthesis")]
      [else
       (case mode
         [(single)
          (adv! st) (write-char ch out)
          (loop depth (if (char=? ch #\') 'plain 'single))]
         [(double)
          (adv! st)
          (cond
            [(char=? ch #\\)
             (write-char ch out)
             (define nx (pk st))
             (when nx (adv! st) (write-char nx out))
             (loop depth 'double)]
            [else (write-char ch out)
                  (loop depth (if (char=? ch DQUOTE) 'plain 'double))])]
         [else
          (cond
            [(char=? ch #\\)
             (adv! st) (write-char ch out)
             (define nx (pk st))
             (when nx (adv! st) (write-char nx out))
             (loop depth 'plain)]
            [(char=? ch #\') (adv! st) (write-char ch out) (loop depth 'single)]
            [(char=? ch DQUOTE) (adv! st) (write-char ch out) (loop depth 'double)]
            [(char=? ch #\() (adv! st) (write-char ch out) (loop (add1 depth) 'plain)]
            [(char=? ch #\))
             (adv! st)
             (if (= depth 1)
                 (void)                               ; 完成，不写入
                 (begin (write-char ch out) (loop (sub1 depth) 'plain)))]
            [else (adv! st) (write-char ch out) (loop depth 'plain)])]
       ) ; end case
      ] ; end else
    ) ; end cond
  ) ; end let loop
  (get-output-string out)
) ; end define read-balanced-paren!

;; pos 处于 `$((` 之后；吃到配对的 `))`。返回内容。
(define (read-balanced-arith! st)
  (define c1 (read-balanced-paren! st))                ; 到内层 ( 的配对 )
  (skip-blanks! st)
  (unless (eqv? (pk st) #\)) (perr st "expected )) closing arithmetic"))
  (adv! st)
  c1
) ; end define read-balanced-arith!

;; 反引号命令替换：pos 在开 ` 之后；到未转义的 ` 为止。
(define (read-backtick! st)
  (define out (open-output-string))
  (let loop ()
    (define ch (pk st))
    (cond
      [(not ch) (perr st "unbalanced backtick")]
      [(char=? ch BTICK) (adv! st)]
      [(char=? ch #\\)
       (adv! st)
       (define nx (pk st))
       (cond
         [(not nx) (perr st "dangling backslash in backtick")]
         [(or (char=? nx BTICK) (char=? nx #\\) (char=? nx #\$))
          (adv! st) (write-char nx out) (loop)]
         [else (write-char ch out) (adv! st) (write-char nx out) (loop)])]
      [else (adv! st) (write-char ch out) (loop)]
    ) ; end cond
  ) ; end let loop
  (get-output-string out)
) ; end define read-backtick!

;; ${…}：pos 在 `${` 之后；配对花括号（引号内不计数）。
(define (read-balanced-brace! st)
  (define out (open-output-string))
  (let loop ([depth 1] [mode 'plain])
    (define ch (pk st))
    (cond
      [(not ch) (perr st "unbalanced ${")]
      [else
       (case mode
         [(single) (adv! st) (write-char ch out)
                   (loop depth (if (char=? ch #\') 'plain 'single))]
         [(double) (adv! st)
                   (cond
                     [(char=? ch #\\) (write-char ch out)
                      (define nx (pk st)) (when nx (adv! st) (write-char nx out))
                      (loop depth 'double)]
                     [else (write-char ch out)
                           (loop depth (if (char=? ch DQUOTE) 'plain 'double))])]
         [else
          (cond
            [(char=? ch #\\) (adv! st) (write-char ch out)
             (define nx (pk st)) (when nx (adv! st) (write-char nx out))
             (loop depth 'plain)]
            [(char=? ch #\') (adv! st) (write-char ch out) (loop depth 'single)]
            [(char=? ch DQUOTE) (adv! st) (write-char ch out) (loop depth 'double)]
            [(char=? ch #\{) (adv! st) (write-char ch out) (loop (add1 depth) 'plain)]
            [(char=? ch #\})
             (adv! st)
             (if (= depth 1) (void)
                 (begin (write-char ch out) (loop (sub1 depth) 'plain)))]
            [else (adv! st) (write-char ch out) (loop depth 'plain)])]
       ) ; end case
      ] ; end else
    ) ; end cond
  ) ; end let loop
  (get-output-string out)
) ; end define read-balanced-brace!

;; 在任意内容串里提取顶层命令替换（$(…)、`…`；$((…)) 深入其内部继续找）。
(define (extract-subs content)
  (define st (pst content 0 '()))
  (let loop ([acc '()] [mode 'plain])
    (define ch (pk st))
    (cond
      [(not ch) (reverse acc)]
      [(eq? mode 'single)
       (adv! st) (loop acc (if (char=? ch #\') 'plain 'single))]
      [(char=? ch #\\) (adv! st) (when (pk st) (adv! st)) (loop acc mode)]
      [(eq? mode 'double)
       (cond
         [(char=? ch DQUOTE) (adv! st) (loop acc 'plain)]
         [(char=? ch BTICK) (adv! st) (loop (cons (read-backtick! st) acc) 'double)]
         [(and (char=? ch #\$) (eqv? (pk st 1) #\())
          (if (eqv? (pk st 2) #\()
              (begin (adv! st 3)
                     (let ([inner (read-balanced-arith! st)])
                       (loop (append (reverse (extract-subs inner)) acc) 'double)))
              (begin (adv! st 2) (loop (cons (read-balanced-paren! st) acc) 'double)))]
         [else (adv! st) (loop acc 'double)])]
      [(char=? ch #\') (adv! st) (loop acc 'single)]
      [(char=? ch DQUOTE) (adv! st) (loop acc 'double)]
      [(char=? ch BTICK) (adv! st) (loop (cons (read-backtick! st) acc) 'plain)]
      [(and (char=? ch #\$) (eqv? (pk st 1) #\())
       (if (eqv? (pk st 2) #\()
           (begin (adv! st 3)
                  (let ([inner (read-balanced-arith! st)])
                    (loop (append (reverse (extract-subs inner)) acc) 'plain)))
           (begin (adv! st 2) (loop (cons (read-balanced-paren! st) acc) 'plain)))]
      [else (adv! st) (loop acc mode)]
    ) ; end cond
  ) ; end let loop
) ; end define extract-subs

;; ---------------------------------------------------------------- 词

(define (param-start? ch)
  (and ch (or (char-alphabetic? ch) (char-numeric? ch)
              (memv ch (list #\_ #\@ #\* #\# #\? #\$ #\! #\-))))
) ; end define param-start?

;; 解析一个词；无词返回 #f。
(define (parse-word st)
  (define out (open-output-string))
  (define any? (box #f)) (define static? (box #t)) (define glob? (box #f))
  (define param? (box #f)) (define quoted? (box #f)) (define subs (box '()))
  (define (emit! ch) (write-char ch out) (set-box! any? #t))
  (define (sub! s) (set-box! subs (cons s (unbox subs))) (set-box! static? #f) (set-box! any? #t))
  (define (dollar! st mode)
    ;; pos 在 $ 上。mode ∈ 'plain 'double。
    (define nx (pk st 1))
    (cond
      [(eqv? nx #\()
       (cond
         [(eqv? (pk st 2) #\()
          (adv! st 3)
          (define inner (read-balanced-arith! st))
          (for ([s (in-list (extract-subs inner))]) (sub! s))
          (set-box! static? #f) (set-box! any? #t)]
         [else (adv! st 2) (sub! (read-balanced-paren! st))])]
      [(eqv? nx #\{)
       (adv! st 2)
       (define inner (read-balanced-brace! st))
       (for ([s (in-list (extract-subs inner))]) (sub! s))
       (set-box! param? #t) (set-box! static? #f) (set-box! any? #t)]
      [(and (eq? mode 'plain) (eqv? nx #\'))
       ;; $'…' ANSI-C 引号：字面量
       (adv! st 2) (set-box! quoted? #t) (set-box! any? #t)
       (let loop ()
         (define ch (pk st))
         (cond
           [(not ch) (perr st "unbalanced $'")]
           [(char=? ch #\\) (adv! st)
            (define n2 (pk st)) (when n2 (adv! st) (emit! n2)) (loop)]
           [(char=? ch #\') (adv! st)]
           [else (adv! st) (emit! ch) (loop)]))]
      [(and (eq? mode 'plain) (eqv? nx DQUOTE))
       (adv! st 1)]                                    ; $"…" 本地化：当普通双引号
      [(param-start? nx)
       (adv! st 2)
       (when (or (char-alphabetic? nx) (char=? nx #\_))
         (let eat ()
           (define ch (pk st))
           (when (and ch (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_)))
             (adv! st) (eat))))
       (set-box! param? #t) (set-box! static? #f) (set-box! any? #t)]
      [else (adv! st) (emit! #\$)]
    ) ; end cond
  ) ; end define dollar!
  (let loop ()
    (define ch (pk st))
    (cond
      [(not ch) (void)]
      [(meta-char? ch)
       ;; <( / >( 是进程替换，属于词
       (cond
         [(and (memv ch (list #\< #\>)) (eqv? (pk st 1) #\())
          (adv! st 2) (sub! (read-balanced-paren! st)) (loop)]
         [else (void)])]
      [(char=? ch #\\)
       (define nx (pk st 1))
       (cond
         [(eqv? nx #\newline) (adv! st 2) (loop)]
         [nx (adv! st 2) (emit! nx) (loop)]
         [else (adv! st) (loop)])]
      [(char=? ch #\')
       (adv! st) (set-box! quoted? #t) (set-box! any? #t)
       (let q ()
         (define c2 (pk st))
         (cond
           [(not c2) (perr st "unbalanced single quote")]
           [(char=? c2 #\') (adv! st)]
           [else (adv! st) (emit! c2) (q)]))
       (loop)]
      [(char=? ch DQUOTE)
       (adv! st) (set-box! quoted? #t) (set-box! any? #t)
       (let dq ()
         (define c2 (pk st))
         (cond
           [(not c2) (perr st "unbalanced double quote")]
           [(char=? c2 DQUOTE) (adv! st)]
           [(char=? c2 #\\)
            (define n2 (pk st 1))
            (cond
              [(and n2 (memv n2 (list #\$ DQUOTE #\\ BTICK)))
               (adv! st 2) (emit! n2) (dq)]
              [(eqv? n2 #\newline) (adv! st 2) (dq)]
              [else (adv! st) (emit! c2) (dq)])]
           [(char=? c2 BTICK) (adv! st) (sub! (read-backtick! st)) (dq)]
           [(char=? c2 #\$) (dollar! st 'double) (dq)]
           [else (adv! st) (emit! c2) (dq)]))
       (loop)]
      [(char=? ch BTICK) (adv! st) (sub! (read-backtick! st)) (loop)]
      [(char=? ch #\$) (dollar! st 'plain) (loop)]
      [(memv ch (list #\* #\?)) (adv! st) (set-box! glob? #t) (emit! ch) (loop)]
      [else (adv! st) (emit! ch) (loop)]
    ) ; end cond
  ) ; end let loop
  (if (unbox any?)
      (sh-word (get-output-string out) (unbox static?) (unbox glob?)
               (unbox param?) (unbox quoted?) (reverse (unbox subs)))
      #f)
) ; end define parse-word

;; ---------------------------------------------------------------- 重定向

;; pos 是否处在重定向起点（可选 fd 数字 + < >，或 &> / &>>）。
;; 注意 <( / >( 是进程替换、数字后随 ( 亦然，不算重定向。
(define (redir-ahead? st)
  (define (io-at k)
    (define ch (pk st k))
    (and (memv ch (list #\< #\>)) (not (eqv? (pk st (add1 k)) #\())))
  (or (io-at 0)
      (and (looking-at? st "&>") #t)
      (let digits ([k 0])
        (define ch (pk st k))
        (cond
          [(and ch (char-numeric? ch)) (digits (add1 k))]
          [(> k 0) (io-at k)]
          [else #f]))
  ) ; end or
) ; end define redir-ahead?

(define REDIR-OPS (list "&>>" "&>" "<<<" "<<-" "<<" "<&" ">>" ">|" ">&" ">" "<"))

(define (parse-redir! st)
  (define fd
    (let digits ([acc '()])
      (define ch (pk st))
      (if (and ch (char-numeric? ch))
          (begin (adv! st) (digits (cons ch acc)))
          (if (null? acc) #f (list->string (reverse acc))))))
  (define op (for/first ([o (in-list REDIR-OPS)] #:when (looking-at? st o)) o))
  (unless op (perr st "expected redirection operator"))
  (adv! st (string-length op))
  (skip-blanks! st)
  (define target (parse-word st))
  (unless target (perr st f"missing redirection target after {op}"))
  (define hbox (box #f))
  (when (member op (list "<<" "<<-"))
    (set-pst-pending! st (append (pst-pending st)
                                 (list (list hbox op (sh-word-text target)
                                             (sh-word-quoted? target))))))
  (sh-redir fd op target hbox)
) ; end define parse-redir!

;; ---------------------------------------------------------------- 语句

(define RESERVED-OPENERS (list "if" "while" "until" "for" "case" "{" "[[" "function"))

;; 偷看下一个词（不消费）。返回 (values word pos-after) 或 (values #f _)。
(define (peek-word st)
  (define save (pst-pos st))
  (define w (with-handlers ([exn:fail? (lambda (_e) #f)]) (parse-word st)))
  (define after (pst-pos st))
  (set-pst-pos! st save)
  (values w after)
) ; end define peek-word

;; 若下一词是给定静态词则消费之并返回 #t。
(define (eat-word! st text)
  (define-values (w after) (peek-word st))
  (cond
    [(and w (sh-word-static? w) (not (sh-word-quoted? w)) (string=? (sh-word-text w) text))
     (set-pst-pos! st after) #t]
    [else #f]
  ) ; end cond
) ; end define eat-word!

(define (parse-trailing-redirs! st)
  (let loop ([acc '()])
    (skip-blanks! st)
    (if (redir-ahead? st)
        (loop (cons (parse-redir! st) acc))
        (reverse acc))
  ) ; end let loop
) ; end define parse-trailing-redirs!

;; 简单命令（或函数定义 name ()）。已确认不是复合开头。
(define (parse-simple* st)
  (define assigns '()) (define words '()) (define redirs '())
  (define result
    (let loop ()
      (skip-blanks! st)
      (define ch (pk st))
      (cond
        [(or (not ch) (char=? ch #\newline) (char=? ch #\)) (char=? ch #\;) (char=? ch #\})
             (char=? ch #\|))
         'done]
        [(char=? ch #\&)
         (if (looking-at? st "&>")
             (begin (set! redirs (cons (parse-redir! st) redirs)) (loop))
             'done)]
        [(redir-ahead? st)
         (set! redirs (cons (parse-redir! st) redirs)) (loop)]
        [(char=? ch #\()
         (cond
           [(and (= (length words) 1) (null? assigns) (null? redirs)
                 (sh-word-static? (car words)))
            (adv! st) (skip-blanks! st)
            (unless (eqv? (pk st) #\)) (perr st "expected ) in function definition"))
            (adv! st) (skip-linebreaks! st)
            (list 'fundef (sh-fundef (sh-word-text (car words)) (parse-command st)))]
           [else (perr st "unexpected (")])]
        [else
         (define w (parse-word st))
         (cond
           [(not w) 'done]
           [(and (null? words)
                 (regexp-match? #px"^[A-Za-z_][A-Za-z0-9_]*\\+?=" (sh-word-text w))
                 (not (sh-word-quoted? w)))
            (cond
              [(and (eqv? (pk st) #\() (regexp-match? #px"=$" (sh-word-text w)))
               (adv! st)
               (define arr-subs
                 (let arr ([acc (sh-word-subs w)])
                   (skip-linebreaks! st)
                   (cond
                     [(eqv? (pk st) #\)) (adv! st) acc]
                     [else
                      (define aw (parse-word st))
                      (unless aw (perr st "unbalanced array literal"))
                      (arr (append acc (sh-word-subs aw)))])))
               (set! assigns (cons (sh-word (sh-word-text w) #f #f (sh-word-param? w) #f arr-subs)
                                   assigns))
               (loop)]
              [else (set! assigns (cons w assigns)) (loop)])]
           [else (set! words (cons w words)) (loop)])]
      ) ; end cond
    ) ; end let loop
  ) ; end define result
  (cond
    [(and (pair? result) (eq? (car result) 'fundef)) (cadr result)]
    [(and (null? assigns) (null? words) (null? redirs)) (perr st "expected command")]
    [else (sh-simple (reverse assigns) (reverse words) (reverse redirs))]
  ) ; end cond
) ; end define parse-simple*

;; if/while/until/for/case/{/[[ 复合；否则简单命令。
(define (parse-command st)
  (skip-blanks! st)
  (define ch (pk st))
  (cond
    [(eqv? ch #\()
     (cond
       [(eqv? (pk st 1) #\()
        ;; (( … )) 算术命令
        (adv! st 2)
        (define inner (read-balanced-arith! st))
        (define redirs (parse-trailing-redirs! st))
        (sh-compound 'arith '()
                     (list (sh-word "" #f #f #f #f (extract-subs inner))) redirs)]
       [else
        (adv! st)
        (define body (parse-seq st #:stop-char #\)))
        (unless (eqv? (pk st) #\)) (perr st "expected )"))
        (adv! st)
        (sh-compound 'subshell (list body) '() (parse-trailing-redirs! st))])]
    [else
     (define-values (w after) (peek-word st))
     (define txt (and w (sh-word-static? w) (not (sh-word-quoted? w)) (sh-word-text w)))
     (cond
       [(and txt (member txt RESERVED-OPENERS))
        (set-pst-pos! st after)
        (case txt
          [("{")
           (define body (parse-seq st #:stop-words (list "}")))
           (unless (eat-word! st "}") (perr st "expected }"))
           (sh-compound 'group (list body) '() (parse-trailing-redirs! st))]
          [("if") (parse-if st)]
          [("while") (parse-loop st 'while)]
          [("until") (parse-loop st 'until)]
          [("for") (parse-for st)]
          [("case") (parse-case st)]
          [("[[") (parse-cond st)]
          [("function")
           (skip-blanks! st)
           (define nm (parse-word st))
           (unless (and nm (sh-word-static? nm)) (perr st "bad function name"))
           (skip-blanks! st)
           (when (eqv? (pk st) #\()
             (adv! st) (skip-blanks! st)
             (unless (eqv? (pk st) #\)) (perr st "expected )"))
             (adv! st))
           (skip-linebreaks! st)
           (sh-fundef (sh-word-text nm) (parse-command st))]
          [else (perr st "unreachable")])]
       [else (parse-simple* st)]
     ) ; end cond
    ] ; end else
  ) ; end cond
) ; end define parse-command

(define (parse-if st)
  (define cnd (parse-seq st #:stop-words (list "then")))
  (unless (eat-word! st "then") (perr st "expected then"))
  (let branches ([seqs (list cnd)])
    (define body (parse-seq st #:stop-words (list "elif" "else" "fi")))
    (cond
      [(eat-word! st "fi")
       (sh-compound 'if (reverse (cons body seqs)) '() (parse-trailing-redirs! st))]
      [(eat-word! st "elif")
       (define c2 (parse-seq st #:stop-words (list "then")))
       (unless (eat-word! st "then") (perr st "expected then"))
       (branches (cons c2 (cons body seqs)))]
      [(eat-word! st "else")
       (define e (parse-seq st #:stop-words (list "fi")))
       (unless (eat-word! st "fi") (perr st "expected fi"))
       (sh-compound 'if (reverse (cons e (cons body seqs))) '() (parse-trailing-redirs! st))]
      [else (perr st "expected elif/else/fi")]
    ) ; end cond
  ) ; end let branches
) ; end define parse-if

(define (parse-loop st kind)
  (define cnd (parse-seq st #:stop-words (list "do")))
  (unless (eat-word! st "do") (perr st "expected do"))
  (define body (parse-seq st #:stop-words (list "done")))
  (unless (eat-word! st "done") (perr st "expected done"))
  (sh-compound kind (list cnd body) '() (parse-trailing-redirs! st))
) ; end define parse-loop

(define (parse-for st)
  (skip-blanks! st)
  (cond
    [(and (eqv? (pk st) #\() (eqv? (pk st 1) #\())
     (adv! st 2)
     (define inner (read-balanced-arith! st))
     (skip-blanks! st)
     (when (eqv? (pk st) #\;) (adv! st))
     (skip-linebreaks! st)
     (unless (eat-word! st "do") (perr st "expected do"))
     (define body (parse-seq st #:stop-words (list "done")))
     (unless (eat-word! st "done") (perr st "expected done"))
     (sh-compound 'for (list body)
                  (list (sh-word "" #f #f #f #f (extract-subs inner)))
                  (parse-trailing-redirs! st))]
    [else
     (define var (parse-word st))
     (unless var (perr st "expected for variable"))
     (skip-blanks! st)
     (when (eqv? (pk st) #\;) (adv! st))
     (skip-linebreaks! st)
     (define in-words
       (if (eat-word! st "in")
           (let iw ([acc '()])
             (skip-blanks! st)
             (define c2 (pk st))
             (cond
               [(or (not c2) (char=? c2 #\;) (char=? c2 #\newline))
                (when (eqv? c2 #\;) (adv! st))
                (skip-linebreaks! st)
                (reverse acc)]
               [else
                (define w (parse-word st))
                (unless w (perr st "bad word in for list"))
                (iw (cons w acc))]))
           '()))
     (unless (eat-word! st "do") (perr st "expected do"))
     (define body (parse-seq st #:stop-words (list "done")))
     (unless (eat-word! st "done") (perr st "expected done"))
     (sh-compound 'for (list body) in-words (parse-trailing-redirs! st))]
  ) ; end cond
) ; end define parse-for

(define (parse-case st)
  (skip-blanks! st)
  (define subject (parse-word st))
  (unless subject (perr st "expected case subject"))
  (skip-linebreaks! st)
  (unless (eat-word! st "in") (perr st "expected in"))
  (let clauses ([seqs '()] [pwords (list subject)])
    (skip-linebreaks! st)
    (cond
      [(eat-word! st "esac")
       (sh-compound 'case (reverse seqs) (reverse pwords) (parse-trailing-redirs! st))]
      [else
       (when (eqv? (pk st) #\() (adv! st) (skip-blanks! st))
       (define pats
         (let pat ([acc '()])
           (define w (parse-word st))
           (unless w (perr st "expected case pattern"))
           (skip-blanks! st)
           (cond
             [(and (eqv? (pk st) #\|) (not (eqv? (pk st 1) #\|)))
              (adv! st) (skip-blanks! st) (pat (cons w acc))]
             [else (cons w acc)])))
       (unless (eqv? (pk st) #\)) (perr st "expected ) after case pattern"))
       (adv! st)
       (define body (parse-seq st #:stop-words (list "esac") #:case-mode? #t))
       ;; 终结符：;; / ;& / ;;& 已被 seq 消费；或直接 esac
       (cond
         [(eat-word! st "esac")
          (sh-compound 'case (reverse (cons body seqs))
                       (reverse (append pats pwords)) (parse-trailing-redirs! st))]
         [else (clauses (cons body seqs) (append pats pwords))])]
    ) ; end cond
  ) ; end let clauses
) ; end define parse-case

;; [[ … ]]：词收集（< > & | ( ) ! 作为比较符/括号跳过），到 ]] 止。
(define (parse-cond st)
  (let loop ([ws '()])
    (skip-blanks! st)
    (define ch (pk st))
    (cond
      [(not ch) (perr st "expected ]]")]
      [(eqv? ch #\newline) (consume-newline! st) (loop ws)]
      [(memv ch (list #\< #\> #\& #\| #\( #\) #\!))
       (adv! st) (loop ws)]
      [else
       (define w (parse-word st))
       (cond
         [(not w) (perr st "bad token in [[ ]]")]
         [(and (sh-word-static? w) (string=? (sh-word-text w) "]]"))
          (sh-compound 'cond '() (reverse ws) (parse-trailing-redirs! st))]
         [else (loop (cons w ws))])]
    ) ; end cond
  ) ; end let loop
) ; end define parse-cond

;; 管道：cmd (| 或 |& cmd)*
(define (parse-pipeline st)
  (let loop ([cmds (list (parse-command st))])
    (skip-blanks! st)
    (cond
      [(and (eqv? (pk st) #\|) (not (eqv? (pk st 1) #\|)))
       (adv! st)
       (when (eqv? (pk st) #\&) (adv! st))
       (skip-linebreaks! st)
       (loop (cons (parse-command st) cmds))]
      [else (if (= 1 (length cmds)) (car cmds) (sh-pipe (reverse cmds)))]
    ) ; end cond
  ) ; end let loop
) ; end define parse-pipeline

;; and-or：pipeline ((&& / ||) pipeline)* —— 判级上全部可能执行，压平为 seq。
(define (parse-andor st)
  (let loop ([cmds (list (parse-pipeline st))])
    (skip-blanks! st)
    (cond
      [(or (looking-at? st "&&") (looking-at? st "||"))
       (adv! st 2) (skip-linebreaks! st)
       (loop (cons (parse-pipeline st) cmds))]
      [else (if (= 1 (length cmds)) (car cmds) (sh-seq (reverse cmds)))]
    ) ; end cond
  ) ; end let loop
) ; end define parse-andor

;; 序列：直到 EOF / stop-char / stop-words / (case-mode 下 ;; ;& ;;&)。
;; 停止词只在**命令起始位置**生效；停止词/;;… 会被消费（stop-char 不消费）。
(define (parse-seq st #:stop-words [stop-words '()] #:stop-char [stop-char #f]
                   #:case-mode? [case-mode? #f])
  (let loop ([cmds '()])
    (skip-blanks! st)
    (define ch (pk st))
    (cond
      [(not ch) (sh-seq (reverse cmds))]
      [(char=? ch #\newline) (consume-newline! st) (loop cmds)]
      [(and stop-char (char=? ch stop-char)) (sh-seq (reverse cmds))]
      [(char=? ch #\;)
       (cond
         [(and case-mode? (eqv? (pk st 1) #\;))
          (adv! st 2) (when (eqv? (pk st) #\&) (adv! st))
          (sh-seq (reverse cmds))]
         [(and case-mode? (eqv? (pk st 1) #\&)) (adv! st 2) (sh-seq (reverse cmds))]
         [else (adv! st) (loop cmds)])]
      [(char=? ch #\&)
       (if (or (looking-at? st "&&") (looking-at? st "&>"))
           (perr st "unexpected &&/&> at command start")
           (begin (adv! st) (loop cmds)))]
      [else
       (define stop?
         (and (pair? stop-words)
              (let-values ([(w _after) (peek-word st)])
                (and w (sh-word-static? w) (not (sh-word-quoted? w))
                     (member (sh-word-text w) stop-words)))))
       (if stop?
           (sh-seq (reverse cmds))                     ; 停止词留给调用方 eat-word!
           (loop (cons (parse-andor st) cmds)))]
    ) ; end cond
  ) ; end let loop
) ; end define parse-seq

;; 主入口：整串 → sh-seq；尾部残留或 heredoc 未闭合 → 报错。
(define (parse-shell s)
  (define st (pst s 0 '()))
  (define ast (parse-seq st))
  (skip-linebreaks! st)
  ;; 结尾无换行但有待处理 heredoc → 视作 EOF 前正文缺失
  (when (pair? (pst-pending st)) (perr st "heredoc delimiter not found"))
  (unless (at-end? st) (perr st f"trailing input at offset {(pst-pos st)}"))
  ast
) ; end define parse-shell

(provide
 (struct-out sh-word)
 (struct-out sh-redir)
 (struct-out sh-simple)
 (struct-out sh-pipe)
 (struct-out sh-seq)
 (struct-out sh-compound)
 (struct-out sh-fundef)
 parse-shell
 extract-subs
) ; end provide
