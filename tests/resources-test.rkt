#lang tstring racket
;; resources-test.rkt — 技能/提示词资源读取与渲染（离线）。

(require
 rackunit
 racket/file
 (file "../src/resources.rkt")
) ; end require

(test-case "parse-front-matter extracts fields and body"
  (define-values (fm body)
    (parse-front-matter "---\nname: foo\ndescription: bar baz\n---\n# heading\ncontent line"))
  (check-equal? (cdr (assoc "name" fm)) "foo")
  (check-equal? (cdr (assoc "description" fm)) "bar baz")
  (check-true (string-contains? body "content line"))
  (check-false (string-contains? body "name: foo"))          ; 前置元数据不入正文
) ; end test-case

(test-case "no front matter → whole text is body"
  (define-values (fm body) (parse-front-matter "just plain text\nmore"))
  (check-equal? fm '())
  (check-true (string-contains? body "just plain text"))
) ; end test-case

(test-case "read-resource + skills-addendum (progressive disclosure)"
  (define tmp (make-temporary-file "res-~a.md"))
  (with-output-to-file tmp #:exists 'replace
    (lambda () (display "---\nname: sk1\ndescription: does a thing\n---\nfull instructions here")))
  (define r (read-resource tmp))
  (check-equal? (resource-name r) "sk1")
  (check-equal? (resource-description r) "does a thing")
  (check-true (string-contains? (resource-body r) "full instructions here"))
  (define add (skills-addendum (list r)))
  (check-true (string-contains? add "Available skills"))
  (check-true (string-contains? add "sk1"))
  (check-true (string-contains? add "does a thing"))
  (check-true (string-contains? add (resource-path r)))       ; 含路径供 read_file
  (delete-file tmp)
) ; end test-case

(test-case "read-resource falls back to filename when no name field"
  (define tmp (make-temporary-file "no-fm-~a.md"))
  (with-output-to-file tmp #:exists 'replace (lambda () (display "body only, no front matter")))
  (define r (read-resource tmp))
  (check-true (regexp-match? #rx"no-fm-" (resource-name r)))   ; 用文件名
  (delete-file tmp)
) ; end test-case

(test-case "empty skills → empty addendum"
  (check-equal? (skills-addendum '()) "")
) ; end test-case

(displayln "resources-test: all passed")
