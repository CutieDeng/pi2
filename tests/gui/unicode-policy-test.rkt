#lang racket/base
;; tests/gui/unicode-policy-test.rkt — 显示时的 Unicode 策略（design-gui.md §30）：双向控制 / 不可见 / tag / 从右到左 / 控制字符 /
;; 组合符堆叠 / 形近字的分类；emoji 序列里的 ZWJ、变体选择符放行；显示串与原串等长（下标一一对应）

(require rackunit racket/list "../../src/gui/compute/unicode-policy.rkt")

(define (scan s) (define-values (d m o) (unicode-scan s)) (list d (for/list ([v m]) (list (vector-ref v 0) (vector-ref v 1) (vector-ref v 2))) o))
(define (tags s) (list->string (for/list ([c (in-string s)]) (integer->char (+ #xE0000 (char->integer c))))))

(test-case "bidi controls and invisible format characters become markers"
  (check-equal? (cadr (scan "file ‮txt.exe‬")) '((5 bidi "RLO") (13 bidi "PDF")))
  (check-equal? (cadr (scan "a⁦b⁩")) '((1 bidi "LRI") (3 bidi "PDI")))
  (check-equal? (cadr (scan "pass​word­x﻿")) '((4 invisible "ZWSP") (9 invisible "SHY") (11 invisible "BOM"))))

(test-case "tag characters: one marker with the decoded hidden text, the rest zero-width"
  (define r (scan (string-append "hi" (tags "ignore me") "!")))
  (check-equal? (car (cadr r)) '(2 tag "ignore me"))
  (check-equal? (length (cadr r)) 9)
  (check-true (andmap (lambda (m) (equal? (caddr m) "")) (cdr (cadr r))))
  (check-equal? (cadr (scan (string-append "🏴" (tags "gbsct") (string (integer->char #xE007F))))) '()))   ; 地区旗 emoji：不标

(test-case "emoji sequences keep ZWJ and variation selectors; elsewhere they are marked"
  (check-equal? (cadr (scan "👨‍👩‍👧 👍🏽 🏳️‍🌈 1️⃣ ☺︎")) '())
  (check-equal? (cadr (scan "a‍b x️")) '((1 invisible "ZWJ") (5 vs "VS16"))))

(test-case "C0 controls → control pictures, U+2028 / 2029 → newline, C1 → marker; same length"
  (define r (scan "\u001b[31mred\u0007\tok\u007fx y\u0085"))
  (check-equal? (car r) "␛[31mred␇\tok␡x\ny\u0085")
  (check-equal? (string-length (car r)) (string-length "\u001b[31mred\u0007\tok\u007fx y\u0085"))
  (check-equal? (cadr r) '((16 c1 "U+0085"))))

(test-case "RTL letters (not supported) become code-point markers"
  (check-equal? (cadr (scan "שלום x")) '((0 rtl "U+05E9") (1 rtl "U+05DC") (2 rtl "U+05D5") (3 rtl "U+05DD")))
  (check-equal? (length (cadr (scan "مرحبا"))) 5))

(test-case "combining pile-up: first 4 marks kept, the rest folded into one marker"
  (define s (string-append "Z" (make-string 10 (integer->char #x0301)) "é"))
  (define m (cadr (scan s)))
  (check-equal? (car m) '(5 combining "+6"))
  (check-equal? (length m) 6))

(test-case "mixed-script words: the minority letters are flagged; single-script words and other scripts are not"
  (check-equal? (caddr (scan "раураl.com paypal.com")) '(0 1 2 3 4 5))      ; раура（西里尔）+ l（拉丁）→ 整个词
  (check-equal? (caddr (scan "pаypal")) '(0 1 2 3 4 5))                      ; 一个西里尔 а 混在拉丁词里 → 整个词
  (check-equal? (caddr (scan "Привет hello 你好 Ελλάδα")) '()))
