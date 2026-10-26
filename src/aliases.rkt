#lang tstring racket
;; aliases.rkt — 模型别名：{config-home}/model-aliases.rktd，形如 (("fast" "deepseek-v4-flash") ("strong" "deepseek-v4-pro"))。
;; 父代理用别名或模型名给子 agent 选模型（subagent.rkt）；设置页列出 / 增删（RPC list_aliases / set_alias）。

(require racket/file (file "credentials.rkt"))

(define (aliases-path) (build-path (config-home) "model-aliases.rktd"))

(define (model-aliases)
  (with-handlers ([exn:fail? (lambda (_e) '())])
    (define p (aliases-path))
    (define v (if (file-exists? p) (call-with-input-file p (lambda (in) (parameterize ([read-accept-reader #f]) (read in)))) '()))
    (if (list? v)
        (for/list ([e (in-list v)] #:when (and (list? e) (= (length e) 2) (string? (car e)) (string? (cadr e)))) (cons (car e) (cadr e)))
        '()))
) ; end define model-aliases

(define (resolve-model-alias name)
  (cond [(assoc name (model-aliases)) => cdr] [else name])
) ; end define resolve-model-alias

(define (valid-alias-name? n) (and (string? n) (regexp-match? #px"^[A-Za-z0-9_.-]{1,40}$" n)))

;; model 为 #f / "" → 删除；已有同名 → 原位替换；否则追加
(define (set-model-alias! name model)
  (unless (valid-alias-name? name) (error 'set-model-alias! "bad alias name: ~s" name))
  (define del? (or (not model) (string=? (string-trim model) "")))
  (define cur (model-aliases))
  (define next
    (cond [del? (filter (lambda (a) (not (equal? (car a) name))) cur)]
          [(assoc name cur) (for/list ([a (in-list cur)]) (if (equal? (car a) name) (cons name (string-trim model)) a))]
          [else (append cur (list (cons name (string-trim model))))]))
  (make-directory* (config-home))
  (define tmp (path-add-extension (aliases-path) #".tmp"))
  (call-with-output-file tmp #:exists 'truncate
    (lambda (out) (write (for/list ([a (in-list next)]) (list (car a) (cdr a))) out) (newline out)))
  (rename-file-or-directory tmp (aliases-path) #t)
  next
) ; end define set-model-alias!

(provide model-aliases resolve-model-alias set-model-alias! valid-alias-name?)
