#lang tstring racket
;; cmdscan.rkt — shell 命令静态判级 v2：AST 遍历（design-execguard.md）。
;;
;; v1 是词法保守扫描；v2 改走内置解析器 shparse.rkt 的 AST（all-in-one，零外挂
;; 进程），在**不放松 fail-closed** 的前提下大幅降误拒：
;;   * $(…)/`…`/<(…)/heredoc 正文里的命令体 → 递归判级（而非一律 opaque）；
;;   * 子 shell/分组/if/while/for/case/[[ ]]/(( )) → 结构化遍历；
;;   * 函数定义不执行不计；调用点按定义体判级，且**同名遮蔽白名单**（ls(){…};ls）；
;;   * 包装命令（time/timeout/nohup/command/…）剥壳判内层；find -exec 判载荷。
;; 判不了的照旧 'opaque：解析失败、动态命令名、eval/xargs/通用解释器、动态
;; 重定向目标、特判命令（git/find/sed/…）里出现动态参数（可走私危险旗标）等。
;;
;; 判级：'read-only | 'mutating | 'opaque（同 v1；opaque 按最危险处理）。
;; verdict.scripts = 递归检查读到的脚本快照 (path . content)（审计取证）。
;; TOCTOU 声明同 v1：检查后执行仍走原路径。

(require
 racket/string
 racket/list
 racket/set
 (file "shparse.rkt")
) ; end require

(struct cmd-verdict (level reason scripts) #:transparent)

(define MAX-DEPTH 6)                                   ; 脚本/替换总递归深度
(define MAX-SCRIPT-BYTES 262144)

;; ---------------------------------------------------------------- 名单

(define SIMPLE-READ-ONLY
  '("ls" "cat" "head" "tail" "wc" "pwd" "echo" "printf" "true" "false" "date"
    "whoami" "id" "uname" "hostname" "which" "type" "file" "stat" "du" "df"
    "basename" "dirname" "realpath" "readlink" "cut" "tr" "rev"
    "nl" "paste" "column" "comm" "diff" "cmp" "od" "xxd" "hexdump" "strings"
    "md5" "shasum" "sha1sum" "sha256sum" "cksum" "sum" "test" "[" "seq"
    "expr" "sleep" "grep" "egrep" "fgrep" "rg" "tree" "printenv" "wait"
    "cd" "export" "set" "unset" "umask" "local" "read" "shift" "return" "exit"
    "break" "continue" ":" "jobs" "declare" "typeset" "let"))

(define OPAQUE-CMDS
  '("eval" "exec" "trap" "alias" "unalias" "xargs" "script" "expect"
    "python" "python2" "python3" "perl" "ruby" "node" "deno" "bun" "racket"
    "osascript" "make" "cmake" "ninja"))

;; 包装命令：剥掉自身（与旗标）后判内层。
(define WRAPPER-CMDS '("time" "nohup" "nice" "stdbuf" "watch" "timeout" "command" "builtin" "env" "!"))

(define SHELLS '("bash" "sh" "zsh" "dash" "ksh"))

(define GIT-READ-ONLY-SUBCMDS
  '("status" "log" "diff" "show" "ls-files" "ls-tree" "ls-remote" "rev-parse"
    "rev-list" "blame" "shortlog" "describe" "grep" "show-ref" "count-objects"
    "reflog" "var" "check-ignore" "cat-file"))

(define (git-args-read-only? args)
  (define as (filter (lambda (a) (and (string? a) (non-empty-string? a))) args))
  (define pos (filter (lambda (a) (not (string-prefix? a "-"))) as))
  (define sub (and (pair? pos) (car pos)))
  (cond
    [(not sub) #f]
    [(member sub GIT-READ-ONLY-SUBCMDS) #t]
    [(string=? sub "branch")
     (for/and ([a (in-list (remove sub as))])
       (member a '("-a" "-r" "-v" "-vv" "--list" "--show-current" "--merged" "--no-merged")))]
    [(string=? sub "tag")
     (for/and ([a (in-list (remove sub as))]) (member a '("-l" "--list" "-n")))]
    [(string=? sub "remote") (for/and ([a (in-list (remove sub as))]) (member a '("-v")))]
    [(string=? sub "stash") (equal? (remove sub pos) '("list"))]
    [(string=? sub "config")
     (for/or ([a (in-list as)]) (member a '("--get" "--list" "-l" "--get-all")))]
    [else #f]
  ) ; end cond
) ; end define git-args-read-only?

;; ---------------------------------------------------------------- verdict 代数

(define RO (cmd-verdict 'read-only #f '()))
(define (opaque why) (cmd-verdict 'opaque why '()))
(define (mut why) (cmd-verdict 'mutating why '()))

(define (worse a b)
  (cond [(or (eq? a 'opaque) (eq? b 'opaque)) 'opaque]
        [(or (eq? a 'mutating) (eq? b 'mutating)) 'mutating]
        [else 'read-only]))

(define (v+ a b)
  (cmd-verdict (worse (cmd-verdict-level a) (cmd-verdict-level b))
               (or (cmd-verdict-reason a) (cmd-verdict-reason b))
               (append (cmd-verdict-scripts a) (cmd-verdict-scripts b)))
) ; end define v+

;; 面包屑：非只读判决在递归边界处把上下文 frame 拼进 reason，形成因果链
;; （如 "find -exec ▸ sh -c ▸ command `rm` not in read-only allowlist"），
;; 供模型精确定位是命令树哪一层被否。只读判决无需归因，原样返回。
(define (within frame v)
  (if (and (not (eq? (cmd-verdict-level v) 'read-only)) (cmd-verdict-reason v))
      (cmd-verdict (cmd-verdict-level v)
                   (string-append frame " ▸ " (cmd-verdict-reason v))
                   (cmd-verdict-scripts v))
      v)
) ; end define within

(define (vfold f xs) (for/fold ([v RO]) ([x (in-list xs)]) (v+ v (f x))))

;; ---------------------------------------------------------------- 上下文

;; ctx：workdir + 递归深度 + 已检查脚本集合（防循环）+ 函数定义（不可变 hash）。
(struct sctx (workdir depth seen funs) #:transparent)

(define (ctx-deeper c) (struct-copy sctx c [depth (add1 (sctx-depth c))]))

;; ---------------------------------------------------------------- 词与重定向

;; 词内嵌命令体（$(…)、`…`、<(…) 等）递归判级。
(define (walk-word-subs w c)
  (vfold (lambda (s) (within "command substitution $(…)" (classify-sub s c)))
         (sh-word-subs w))
) ; end define walk-word-subs

(define OUT-OPS '(">" ">>" ">|" "&>" "&>>"))

(define (sink-target? t)
  (member t '("/dev/null" "/dev/stderr" "/dev/stdout")))

(define (walk-redir r c)
  (define op (sh-redir-op r))
  (define tgt (sh-redir-target r))
  (define tv (walk-word-subs tgt c))                   ; 目标词里的替换也要查
  (v+ tv
      (cond
        [(member op OUT-OPS)
         (cond
           [(not (sh-word-static? tgt)) (opaque f"redirect target with expansion ({op})")]
           [(sink-target? (sh-word-text tgt)) RO]
           [else (mut f"output redirect to file: {(sh-word-text tgt)}")])]
        [(string=? op ">&")
         (define t (sh-word-text tgt))
         (cond
           [(not (sh-word-static? tgt)) (opaque "redirect target with expansion (>&)")]
           [(or (regexp-match? #px"^[0-9]+$" t) (string=? t "-")) RO]
           [(sink-target? t) RO]
           [else (mut f"output redirect to file: {t}")])]
        [(member op '("<<" "<<-"))
         ;; 未引号定界的 heredoc 正文会做命令替换：提取并递归判级。
         (define body (unbox (sh-redir-hbody r)))
         (cond
           [(sh-word-quoted? tgt) RO]
           [(not (string? body)) (opaque "heredoc body unavailable")]
           [else (vfold (lambda (s) (within "heredoc body" (classify-sub s c)))
                        (extract-subs body))])]
        [else RO]                                      ; < <& <<< 皆输入
      ) ; end cond
  ) ; end v+
) ; end define walk-redir

;; ---------------------------------------------------------------- 简单命令

(define (basename-of s)
  (define m (regexp-match #px"([^/]+)$" s))
  (if m (cadr m) s)
) ; end define basename-of

;; words 的 text 列表；任一非静态返回 #f（供特判命令拒绝动态参数走私）。
(define (static-texts ws)
  (for/fold ([acc '()] #:result (and acc (reverse acc)))
            ([w (in-list ws)])
    (and acc (sh-word-static? w) (not (sh-word-glob? w)) (cons (sh-word-text w) acc)))
) ; end define static-texts

;; 按名分派（words 非空；name 词已保证 static 无 glob）。
(define (dispatch-simple words c)
  (define name-w (car words))
  (define name0 (sh-word-text name-w))
  (define name (basename-of name0))
  (define args (cdr words))
  (define (arg-texts) (map sh-word-text (filter sh-word-static? args)))
  (cond
    ;; 函数遮蔽最优先（ls(){ rm x; }; ls）
    [(hash-ref (sctx-funs c) name0 #f)
     => (lambda (body)
          (walk body (struct-copy sctx c [funs (hash-remove (sctx-funs c) name0)])))]
    ;; 相对路径调用：本地脚本/伪装，一律递归检查
    [(and (string-contains? name0 "/") (not (string-prefix? name0 "/")))
     (classify-file name0 c)]
    [(member name0 WRAPPER-CMDS)
     (dispatch-wrapper name0 args c)]
    [(member name SHELLS)
     (define texts (static-texts args))
     (cond
       [(not texts) (opaque f"dynamic argument to {name}")]
       [(member "-c" texts)
        (define body (let lp ([ts texts])
                       (cond [(null? ts) #f]
                             [(string=? (car ts) "-c") (and (pair? (cdr ts)) (cadr ts))]
                             [else (lp (cdr ts))])))
        (if body (within f"{name} -c" (classify-sub body c)) (opaque f"{name} -c without body"))]
       [else
        (define pos (filter (lambda (t) (not (string-prefix? t "-"))) texts))
        (if (pair? pos) (classify-file (car pos) c) (opaque f"bare {name} (interactive shell)"))])]
    [(or (string=? name0 "source") (string=? name0 "."))
     (define texts (static-texts args))
     (cond
       [(not texts) (opaque "dynamic argument to source")]
       [(pair? texts) (classify-file (car texts) c)]
       [else (opaque "bare source")])]
    [(string=? name "git")
     (define texts (static-texts args))
     (cond
       [(not texts) (mut "git with dynamic argument")]
       [(git-args-read-only? texts) RO]
       [else (let ([shown (string-join (take texts (min 3 (length texts))) " ")])
               (mut f"git subcommand not in read-only set: {shown}"))])]
    [(string=? name "find") (dispatch-find args c)]
    [(string=? name "sed")
     (define texts (static-texts args))
     (cond
       [(not texts) (opaque "sed with dynamic argument")]
       [(for/or ([a (in-list texts)])
          (or (regexp-match? #px"^-[a-zA-Z]*i" a) (string-prefix? a "--in-place")))
        (mut "sed -i (in-place edit)")]
       ;; -f/--file：程序体在外部文件,不建模其语言 → 拒（可含 w 写命令）
       [(for/or ([a (in-list texts)])
          (or (regexp-match? #px"^-[a-zA-Z]*f" a) (string-prefix? a "--file")))
        (opaque "sed -f (script from file, not analyzed)")]
       [(for/or ([a (in-list texts)])
          (regexp-match? #px"(^|[^a-zA-Z\\\\])[wW][ \t]+[^ \t]" a))
        (opaque "sed script may contain w (write-to-file) command")]
       [else RO])]
    [(string=? name "sort")
     (define texts (static-texts args))
     (cond
       [(not texts) (opaque "sort with dynamic argument")]
       [(for/or ([a (in-list texts)])
          (or (regexp-match? #px"^-o" a) (string-prefix? a "--output")))
        (mut "sort -o (writes output file)")]
       [else RO])]
    [(string=? name "uniq")
     (define texts (static-texts args))
     (cond
       [(not texts) (opaque "uniq with dynamic argument")]
       [(> (length (filter (lambda (a) (not (string-prefix? a "-"))) texts)) 1)
        (mut "uniq with output-file operand")]
       [else RO])]
    [(member name '("awk" "gawk" "mawk"))
     (define texts (static-texts args))
     (cond
       [(not texts) (opaque "awk with dynamic argument")]
       ;; -f progfile：程序体外置 → 拒（可含 system()/print>）
       [(for/or ([a (in-list texts)])
          (or (regexp-match? #px"^-[a-zA-Z]*f" a) (string-prefix? a "--file")))
        (opaque "awk -f (program from file, not analyzed)")]
       [(for/or ([a (in-list texts)])
          (or (string-contains? a "system(") (string-contains? a ">")
              (string-contains? a "print |") (string-contains? a "printf |")))
        (opaque "awk with system()/redirect/pipe")]
       [else RO])]
    [(member name0 OPAQUE-CMDS) (opaque f"unanalyzable command: {name0}")]
    [(string-prefix? name0 "/") (classify-file name0 c)] ; 绝对路径：脚本递归/二进制白名单
    [(string-suffix? name0 ".sh") (classify-file name0 c)]
    [(member name0 SIMPLE-READ-ONLY) RO]
    [else (mut f"command not in read-only allowlist: {name0}")]
  ) ; end cond
) ; end define dispatch-simple

;; 包装命令：剥名与旗标后判内层；timeout 再剥时长；command -v/-V 仅查询。
(define (dispatch-wrapper name args c)
  (cond
    [(and (string=? name "command")
          (for/or ([w (in-list args)])
            (and (sh-word-static? w) (member (sh-word-text w) '("-v" "-V")))))
     RO]
    [else
     (define rest0 (dropf args (lambda (w) (and (sh-word-static? w)
                                                (string-prefix? (sh-word-text w) "-")))))
     (define rest1
       (cond
         [(and (string=? name "timeout") (pair? rest0)) (cdr rest0)]  ; 时长
         [(string=? name "env")
          (dropf rest0 (lambda (w) (and (sh-word-static? w)
                                        (regexp-match? #px"^[A-Za-z_][A-Za-z0-9_]*="
                                                       (sh-word-text w)))))]
         [else rest0]))
     (cond
       [(null? rest1) RO]
       [(not (and (sh-word-static? (car rest1)) (not (sh-word-glob? (car rest1)))))
        (opaque f"dynamic command under {name}")]
       [else (dispatch-simple rest1 c)])]
  ) ; end cond
) ; end define dispatch-wrapper

;; find 的**只读谓词/选项**白名单——判据是「不写盘、不执行外部命令」。
;; 刻意从严：任何不在此集合的 dash-token（含 -exec/-ok/-delete/-fprint* 与一切
;; 未知谓词）都令整条命令判 opaque。这是有意的架构取舍：不去建模 find 的执行/
;; 写语义（那是随 find 版本增长的攻击面），只维护一张小而稳定的「安全谓词」表；
;; 认不出就拒（fail-closed），宁可误伤 exotic 用法。非 dash token（路径/模式/
;; 数值/文件名等参数）一律跳过——不解读参数含义，故也不受参数注入影响。
(define FIND-READ-ONLY-TOKENS
  (list "-name" "-iname" "-path" "-ipath" "-wholename" "-iwholename" "-lname"
        "-ilname" "-regex" "-iregex" "-type" "-xtype" "-size" "-empty" "-perm"
        "-user" "-group" "-uid" "-gid" "-nouser" "-nogroup" "-readable"
        "-writable" "-executable" "-newer" "-anewer" "-cnewer" "-newermt"
        "-newerat" "-newerct" "-mtime" "-atime" "-ctime" "-mmin" "-amin"
        "-cmin" "-used" "-inum" "-samefile" "-links" "-fstype" "-context"
        "-true" "-false" "-print" "-print0" "-printf" "-ls" "-quit" "-prune"
        "-maxdepth" "-mindepth" "-depth" "-mount" "-xdev" "-follow" "-daystart"
        "-noleaf" "-ignore_readdir_race" "-noignore_readdir_race" "-warn"
        "-nowarn" "-regextype" "-a" "-and" "-o" "-or" "-not" "!" "(" ")" ","))

;; find 的写谓词（明确写盘）：mutating。-fprint/-fprint0/-fls 后随 1 个目标文件，
;; -fprintf 后随 2 个（文件 + 格式）；跳过其目标以免被当谓词误判。
(define FIND-WRITE-PREDS '("-delete" "-fprint" "-fprint0" "-fls" "-fprintf"))

;; find 忠实模型（对齐 GNU findutils find/parser.c: insert_exec_ok）：
;;  - 只读谓词/非-dash 参数（路径/模式/数值）→ 跳过；
;;  - 写谓词 → mutating；
;;  - -exec/-execdir/-ok/-okdir → 按 GNU 规则切出载荷命令，递归判级
;;    （`grep {} +` → 只读；`rm {} ;` → mutating；`sh -c '…'` → 深入 -c 体）；
;;  - {} 在命令名位（执行匹配到的文件本身）/ 载荷无终止符 / 未知谓词 → opaque（fail-closed）。
(define (dispatch-find args c)
  (let scan ([ws args] [v RO])
    (cond
      [(null? ws) v]
      [(not (and (sh-word-static? (car ws)) (not (sh-word-glob? (car ws)))))
       (opaque "find with dynamic argument")]     ; 动态参数可走私谓词/旗标
      [else
       (define t (sh-word-text (car ws)))
       (cond
         [(member t '("-exec" "-execdir" "-ok" "-okdir"))
          (define-values (cmd-words rest ok?) (split-exec-clause (cdr ws) t))
          (if (not ok?)
              (opaque "find -exec/-ok: no terminator, or executes matched files themselves")
              (scan rest (v+ v (within "find -exec" (dispatch-simple cmd-words c)))))]
         [(string=? t "-fprintf") (scan (drop-n (cdr ws) 2) (v+ v (mut "find -fprintf (writes file)")))]
         [(member t '("-fprint" "-fprint0" "-fls"))
          (scan (drop-n (cdr ws) 1) (v+ v (mut f"find {t} (writes file)")))]
         [(string=? t "-delete") (scan (cdr ws) (v+ v (mut "find -delete")))]
         [(and (string-prefix? t "-") (not (string=? t "-")))
          (if (member t FIND-READ-ONLY-TOKENS)
              (scan (cdr ws) v)
              (opaque f"find predicate not in read-only set: {t}"))]
         [else (scan (cdr ws) v)])]   ; 非-dash 参数（路径/模式/数值/文件名）
    ) ; end cond
  ) ; end let scan
) ; end define dispatch-find

(define (drop-n lst n) (if (or (= n 0) (null? lst)) lst (drop-n (cdr lst) (sub1 n))))

;; 切出一个 -exec/-ok 子句的命令词（ws 起于关键字之后）。对齐 GNU：
;;  - `+` 仅当**紧邻前一 arg 恰为 {}** 且允许（-exec/-execdir，非 -ok*）时才是终止符；
;;  - `;` 须为独立整词；
;;  - 无终止符 → find 报错 → ok?=#f；
;;  - {} 出现在命令名位（首词含 {}）→ 执行匹配文件本身 → ok?=#f（opaque）；
;;  - 参数位含 {} 的词是文件占位数据 → 从命令中剔除（不影响命令判级）。
;; 返回 (values 命令词表 剩余词表 ok?)。
(define (split-exec-clause ws kw)
  (define allow-plus (member kw '("-exec" "-execdir")))
  (let loop ([ws ws] [acc '()] [prev-braces? #f])
    (cond
      [(null? ws) (values '() '() #f)]              ; 无终止符
      [else
       (define w (car ws))
       (define st (and (sh-word-static? w) (sh-word-text w)))
       (cond
         [(and allow-plus prev-braces? (equal? st "+")) (finish-exec acc (cdr ws))]
         [(equal? st ";") (finish-exec acc (cdr ws))]
         [else (loop (cdr ws) (cons w acc) (equal? st "{}"))])]
    ) ; end cond
  ) ; end let loop
) ; end define split-exec-clause

(define (finish-exec acc rest)
  (define cmd-words (reverse acc))
  (cond
    [(null? cmd-words) (values '() rest #f)]
    [(let ([n (car cmd-words)]) (and (sh-word-static? n) (string-contains? (sh-word-text n) "{}")))
     (values '() rest #f)]                          ; {} 在命令名位 → 执行找到的文件
    [else
     (define arg-words                              ; 剔除参数位的 {} 占位（纯数据）
       (filter (lambda (w) (not (and (sh-word-static? w) (string-contains? (sh-word-text w) "{}"))))
               (cdr cmd-words)))
     (values (cons (car cmd-words) arg-words) rest #t)]
  ) ; end cond
) ; end define finish-exec

;; ---------------------------------------------------------------- AST 遍历

(define (walk node c)
  (cond
    [(> (sctx-depth c) MAX-DEPTH) (opaque f"nesting deeper than {MAX-DEPTH}")]
    [(sh-seq? node) (walk-seq-with-funs node c)]       ; 序列内函数定义进作用域
    [(sh-pipe? node) (vfold (lambda (x) (walk x c)) (sh-pipe-cmds node))]
    [(sh-fundef? node) RO]                             ; 定义不执行；调用点判体
    [(sh-compound? node)
     (v+ (vfold (lambda (sq) (walk sq c)) (sh-compound-seqs node))
         (v+ (vfold (lambda (w) (walk-word-subs w c)) (sh-compound-words node))
             (vfold (lambda (r) (walk-redir r c)) (sh-compound-redirs node))))]
    [(sh-simple? node)
     (define base
       (v+ (vfold (lambda (w) (walk-word-subs w c)) (sh-simple-assigns node))
           (v+ (vfold (lambda (w) (walk-word-subs w c)) (sh-simple-words node))
               (vfold (lambda (r) (walk-redir r c)) (sh-simple-redirs node)))))
     (define words (sh-simple-words node))
     (cond
       [(null? words) (v+ base RO)]
       [(not (and (sh-word-static? (car words)) (not (sh-word-glob? (car words)))))
        (v+ base (opaque f"dynamic command name"))]
       [else (v+ base (dispatch-simple words c))])]
    [else (opaque "unknown AST node")]
  ) ; end cond
) ; end define walk

;; 序列级遍历（函数定义按出现顺序进作用域，遮蔽后续同名调用）。
(define (walk-seq-with-funs sq c)
  (let loop ([cmds (sh-seq-cmds sq)] [c c] [v RO])
    (cond
      [(null? cmds) v]
      [(sh-fundef? (car cmds))
       (loop (cdr cmds)
             (struct-copy sctx c
                          [funs (hash-set (sctx-funs c)
                                          (sh-fundef-name (car cmds))
                                          (sh-fundef-body (car cmds)))])
             v)]
      [else (loop (cdr cmds) c (v+ v (walk (car cmds) c)))]
    ) ; end cond
  ) ; end let loop
) ; end define walk-seq-with-funs

;; ---------------------------------------------------------------- 递归入口

;; 子命令体（$(…)、bash -c、heredoc 内嵌等）：字符串 → 判级。
(define (classify-sub s c)
  (classify-str s (ctx-deeper c))
) ; end define classify-sub

(define (classify-str s c)
  (cond
    [(not (string? s)) (opaque "non-string command")]
    [(> (sctx-depth c) MAX-DEPTH) (opaque f"nesting deeper than {MAX-DEPTH}")]
    [else
     (define ast
       (with-handlers ([exn:fail? (lambda (e) e)])
         (parse-shell s)))
     (if (exn:fail? ast)
         (opaque f"parse error: {(exn-message ast)}")
         (walk-seq-with-funs ast c))]
  ) ; end cond
) ; end define classify-str

;; 剥 shebang：shell 系 → 正文；其它解释器 → #f。无 shebang 视作 shell。
(define (script-body text)
  (cond
    [(string-prefix? text "#!")
     (define nl (for/first ([ch (in-string text)] [i (in-naturals)]
                            #:when (char=? ch #\newline)) i))
     (define shebang (if nl (substring text 0 nl) text))
     (if (regexp-match? #px"#!\\s*(/usr)?/bin/(env +)?(ba|z|da|k)?sh\\b" shebang)
         (if nl (substring text (add1 nl)) "")
         #f)]
    [else text]
  ) ; end cond
) ; end define script-body

;; 脚本/可执行文件引用：读内容递归判级（快照留证）；二进制仅白名单绝对路径放行。
(define (classify-file ref c)
  (define p (with-handlers ([exn:fail? (lambda (_e) #f)])
              (path->string (simplify-path (path->complete-path ref (sctx-workdir c)) #f))))
  (cond
    [(not p) (opaque f"unresolvable script path: {ref}")]
    [(set-member? (sctx-seen c) p) RO]                 ; 已检查（防循环）
    [(not (file-exists? p)) (opaque f"script not found (cannot inspect): {ref}")]
    [(> (file-size p) MAX-SCRIPT-BYTES) (opaque f"script too large to inspect: {ref}")]
    [else
     (define text (with-handlers ([exn:fail? (lambda (_e) #f)]) (file->string p)))
     (define binary?
       (or (not text) (string-contains? text (string (integer->char 0)))))
     (cond
       [binary?
        (define base (basename-of ref))
        (if (and (string-prefix? ref "/") (member base SIMPLE-READ-ONLY))
            RO
            (opaque f"binary or unreadable executable: {ref}"))]
       [else
        (define body (script-body text))
        (cond
          [(not body) (opaque f"non-shell script (shebang): {ref}")]
          [else
           (define sub
             (within f"script {ref}"
                     (classify-str body
                                   (struct-copy sctx (ctx-deeper c)
                                                [seen (set-add (sctx-seen c) p)]))))
           (cmd-verdict (cmd-verdict-level sub) (cmd-verdict-reason sub)
                        (cons (cons p text) (cmd-verdict-scripts sub)))])])]
  ) ; end cond
) ; end define classify-file

;; 主入口。
(define (classify-command s #:workdir [workdir (current-directory)])
  (classify-str s (sctx workdir 0 (set) (hash)))
) ; end define classify-command

;; 判决调试串：级别 + 因果链 reason + 递归检查过的脚本清单（供 --mode read-only
;; 调试与 agent 归因）。PI_CMDSCAN_DEBUG 置位时 classify 调用点可 eprintf 之。
(define (verdict->debug v)
  (define lv (cmd-verdict-level v))
  (define why (or (cmd-verdict-reason v) "—"))
  (define snap-lines
    (for/list ([s (in-list (cmd-verdict-scripts v))])
      f"    - {(car s)} [{(string-length (cdr s))} bytes]"))
  (define head (list f"verdict: {lv}" f"  reason: {why}"))
  (define body (if (null? snap-lines) head (append head (list "  inspected scripts:") snap-lines)))
  (string-append (string-join body "\n") "\n")
) ; end define verdict->debug

(provide
 (struct-out cmd-verdict)
 classify-command
 verdict->debug
 git-args-read-only?
 GIT-READ-ONLY-SUBCMDS
) ; end provide
