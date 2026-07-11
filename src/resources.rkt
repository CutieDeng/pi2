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

;; 目录下全部 .md（含子目录，支持 skills/<name>/SKILL.md 结构）→ resource 列表
(define (discover-resources dir)
  (if (directory-exists? dir)
      (for/list ([f (in-directory dir)]
                 #:when (and (file-exists? f) (regexp-match? #rx"\\.md$" (path->string f))))
        (read-resource f))
      '())
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

(provide
 (struct-out resource)
 parse-front-matter read-resource discover-resources skills-addendum
) ; end provide
