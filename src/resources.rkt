#lang tstring racket
;; resources.rkt — 技能/提示词资源发现（design-plugins.md M5，对标 pi 的 .pi/skills、.pi/prompts）
;; 读带 YAML 前置元数据的 markdown：
;;   ---
;;   name: brave-search
;;   description: 用 Brave 搜索网页
;;   ---
;;   # 全文说明…
;; 技能：渐进披露——名称/描述列入系统提示词，模型按需 read_file 全文。
;; 提示词：可经 /prompt <name> 激活（其正文追加进系统提示词）。

(require racket/file racket/string)

(struct resource (name description body path) #:transparent)

;; 解析前置元数据：返回 (values alist body)。无 `---` 头则元数据空、正文即全文。
(define (parse-front-matter text)
  (define lines (string-split text "\n" #:trim? #f))
  (cond
    [(and (pair? lines) (string=? (string-trim (car lines)) "---"))
     (let loop ([rest (cdr lines)] [fm '()])
       (cond
         [(null? rest) (values (reverse fm) "")]
         [(string=? (string-trim (car rest)) "---")
          (values (reverse fm) (string-join (cdr rest) "\n"))]
         [else
          (define m (regexp-match #rx"^([^:]+):[ \t]*(.*)$" (car rest)))
          (loop (cdr rest) (if m (cons (cons (string-trim (cadr m)) (string-trim (caddr m))) fm) fm))]))
    ] ; end has-front-matter
    [else (values '() text)]
  ) ; end cond
) ; end define parse-front-matter

(define (fm-ref fm key [dflt #f]) (cond [(assoc key fm) => cdr] [else dflt]))

(define (base-name path)
  (define-values (_d n _q) (split-path (if (path? path) path (string->path path))))
  (regexp-replace #rx"\\.md$" (path->string n) "")
) ; end define base-name

(define (read-resource path)
  (define text (file->string path))
  (define-values (fm body) (parse-front-matter text))
  (resource (or (fm-ref fm "name") (base-name path))
            (or (fm-ref fm "description") "")
            body
            (if (string? path) path (path->string path)))
) ; end define read-resource

;; 目录下的技能 → resource 列表。技能入口两种形态（对齐 Claude Code 约定）：
;;   1. 顶层散文件 skills/<name>.md
;;   2. 技能目录 skills/<name>/SKILL.md —— 该目录内**只有 SKILL.md 是技能入口**，
;;      同目录其它 .md（README/README.zh 等）是配套文档，不单独列为技能。
(define (discover-resources dir)
  (cond
    [(not (directory-exists? dir)) '()]
    [else
     ;; 含 SKILL.md 的目录集合：这些目录内的非 SKILL.md 文件被排除。
     (define skill-dirs
       (for/set ([f (in-directory dir)]
                 #:when (and (file-exists? f)
                             (string=? (let-values ([(_d n _?) (split-path f)]) (path->string n))
                                       "SKILL.md")))
         (let-values ([(d _n _?) (split-path f)]) (path->string (path->complete-path d)))))
     (for/list ([f (in-directory dir)]
                #:when (and (file-exists? f) (regexp-match? #rx"\\.md$" (path->string f)))
                #:when (let-values ([(d n _?) (split-path f)])
                         (or (string=? (path->string n) "SKILL.md")           ; 入口保留
                             (not (set-member? skill-dirs                      ; 非技能目录的散文件保留
                                               (path->string (path->complete-path d)))))))
       (read-resource f))]
  ) ; end cond
) ; end define discover-resources

;; 技能渐进披露：把可用技能列入系统提示词（名称/描述/路径），模型按需 read_file 取全文。
(define (skills-addendum skills)
  (if (null? skills)
      ""
      (string-append
       "\n\n## Available skills\nWhen a task matches one of these, read its file for the full instructions before proceeding.\n"
       (apply string-append
              (for/list ([s (in-list skills)])
                f"- {(resource-name s)}: {(resource-description s)} (read: {(resource-path s)})\n"))))
) ; end define skills-addendum

;; ---------------------------------------------------------------- 项目指令自动加载
;; 对标 Claude Code 的 CLAUDE.md / AGENTS.md：在工作目录放一份项目约定，启动即注入系统提示词，
;; 让 agent 每个 session 不再对项目规范「失忆」。按优先级取**首个存在**的文件（避免重复注入）。

(define PROJECT-INSTRUCTION-FILES '("AGENTS.md" "CLAUDE.md" ".pi/AGENTS.md" "PI.md"))

;; 在 dir 下按优先级找项目指令文件，返回 (values 路径字符串 正文) 或 (values #f "")。
(define (find-project-instructions dir)
  (let loop ([cs PROJECT-INSTRUCTION-FILES])
    (cond
      [(null? cs) (values #f "")]
      [else
       (define fp (build-path dir (car cs)))
       (if (file-exists? fp)
           (values (path->string fp) (file->string fp))
           (loop (cdr cs)))]))
) ; end define find-project-instructions

;; 项目指令注入片段（空正文 → 空串，不注入）。
(define (project-instructions-addendum body [path #f])
  (if (and (string? body) (non-empty-string? (string-trim body)))
      (string-append "\n\n## Project instructions"
                     (if path f" (from {path})" "")
                     "\nFollow these project-specific conventions.\n\n" body)
      "")
) ; end define project-instructions-addendum

(provide
 (struct-out resource)
 parse-front-matter read-resource discover-resources skills-addendum
 PROJECT-INSTRUCTION-FILES find-project-instructions project-instructions-addendum
) ; end provide
