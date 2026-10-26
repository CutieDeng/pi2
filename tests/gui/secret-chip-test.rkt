#lang racket/base
;; tests/gui/secret-chip-test.rkt — 聊天输入框里的密钥 chip：格式识别（命中 / 不误判）、chip 与展开面板不露原值、
;; 发送时是占位符、「作为密钥」确认、回放（content 本身是占位符）

(require rackunit racket/string racket/file
         "../../src/gui/compute/fold.rkt" "../../src/gui/compute/editor.rkt" "../../src/gui/compute/drawlist.rkt"
         "../../src/gui/compute/paint.rkt" "../../src/gui/compute/interfaces.rkt" "../../src/gui/compute/secret-handle.rkt"
         "../../src/gui/components/input.rkt" "fakes.rkt")

(define dir (make-temporary-file "pi2-chip-~a" 'directory))
(void (putenv "PI_CONFIG_HOME" (path->string dir)))
(define KEY "sk-proj-AbCdEf1234567890XyZ")

(test-case "detection: common key formats hit; ordinary text, paths, URLs and multi-word text do not"
  (for ([s (list KEY "  sk-ant-api03-abcdefghijklmnopqrstuv \n" "AKIAIOSFODNN7EXAMPLE" "ghp_abcdefghijklmnopqrstuvwxyz0123456789")])
    (check-true (looks-like-secret? s) s))
  (for ([s (list "hello world" "sk-short" "/usr/local/bin/racket" "https://example.com/a/b" "sk-proj-abc def ghi jklmnopqrstu" "")])
    (check-false (looks-like-secret? s) s))
  (check-true (looks-like-secret? "mytoken-0123456789abcdef" (list "^mytoken-[0-9a-f]{16}$"))))   ; 规则文件里加的格式

(test-case "secret chip: label / summary show only the handle; sending gives the placeholder; confirm"
  (define e (make-editor))
  (editor-insert! e "use ")
  (define f (editor-paste-secret! e KEY #t))
  (check-eq? (fold-kind f) 'secret-pending)
  (define h (secret-handle KEY))
  (check-false (regexp-match? #rx"AbCdEf" (fold-label f)))
  (check-true (string-contains? (fold-label f) h))
  (check-false (regexp-match? #rx"AbCdEf" (fold-summary f)))
  (check-equal? (editor-flatten e) (string-append "use ⟦secret:" h "⟧"))
  (editor-confirm-secret! e (fold-id f))
  (check-eq? (fold-kind (editor-fold e (fold-id f))) 'secret)
  ;; 回放：content 就是占位符 → 原样
  (define e2 (make-editor))
  (editor-paste-secret! e2 (string-append "⟦secret:" h "⟧") #f)
  (check-equal? (editor-flatten e2) (string-append "⟦secret:" h "⟧")))

(test-case "the expanded panel of a secret chip never draws the value, and offers 'treat as secret' for pending ones"
  (define iv (instantiate-with input-component input-box))
  (define e (iv 'editor))
  (define f (editor-paste-secret! e KEY #t))
  (editor-toggle-expand! e (fold-id f))
  (define acc '())
  (parameterize ([current-text-sink (lambda (str xs y h soft?) (set! acc (cons str acc)))])
    (iv 'render! (make-drawlist) 0.0 400.0 600.0 2.0 #t #t))
  (define drawn (string-join acc "\n"))
  (check-false (regexp-match? #rx"AbCdEf" drawn) drawn)
  (check-true (string-contains? drawn "⟦secret:"))
  (define c (iv 'caret-rect))
  (check-true (pair? (for*/or ([y (in-range 0 400 4)] [x (in-range 0 600 8)])
                       (let ([r (iv 'hit (exact->inexact x) (exact->inexact y))]) (and r (eq? (car r) 'button) (eq? (cadr r) 'confirm) r))))))

(delete-directory/files dir)
