#lang racket/base
;; tests/gui/tests-panel-test.rkt — 工作区测试流程面板（design-goalmode.md §12 T2）：定义、信任、每步结果（失败的可展开输出）、修改提议（diff、批准 / 拒绝）、
;; 历史；没有流程时的说明；英文界面不漏中文

(require rackunit racket/list racket/string
         "../../src/gui/compute/drawlist.rkt" "../../src/gui/compute/interfaces.rkt" "../../src/gui/compute/i18n.rkt"
         (only-in "../../src/gui/compute/paint.rkt" current-text-sink)
         "../../src/gui/components/tests-panel.rkt" "fakes.rkt")

(define DATA
  (hasheq 'exists #t 'root "/w/shop" 'file "/w/shop/.pi/tests.rktd" 'name "shop" 'sha "abc" 'trusted #f
          'steps (list (hasheq 'id "check" 'cmd "grep -q READY app.txt" 'parse "exit" 'timeout 60)
                       (hasheq 'id "unit" 'cmd "python3 -m unittest" 'parse "unittest" 'timeout 600))
          'protect '("tests/**")
          'history (list (hasheq 'ts "2026-10-26 10:00:00" 'kind "run" 'ok #f 'steps (list (hasheq 'step "check" 'ok #f) (hasheq 'step "unit" 'ok #t 'passed 2 'failed 0))))
          'proposals (list (hasheq 'id "p1" 'reason "add a lint step" 'diff (hasheq 'text "@@ -1,2 +1,3 @@\n (step unit \"x\")\n+(step lint \"make lint\")")))
          'error 'null))
(define RESULT (hasheq 'ok #f 'results (list (hasheq 'step "check" 'ok #f 'exit 1 'ms 12 'passed 'null 'failed 'null 'failures '() 'tail "grep: app.txt: No such file")
                                             (hasheq 'step "unit" 'ok #t 'exit 0 'ms 340 'passed 2 'failed 0 'failures '() 'tail ""))))

(define (new-tp) (instantiate-with tests-panel-component tests-view))
(define (render p) (define dl (make-drawlist)) (p 'render! dl 240.0 0.0 720.0 680.0 2.0) dl)
(define (texts p) (define acc '()) (parameterize ([current-text-sink (lambda (str xs y h soft?) (set! acc (cons str acc)))]) (render p)) (string-join (reverse acc) "\n"))
(define (click p kind [payload #f]) (define c (p 'hit-center kind payload)) (and c (p 'hit (car c) (cdr c))))

(test-case "definition, trust, results, proposal, history; actions"
 (parameterize ([current-lang 'en])
  (define p (new-tp))
  (check-false (p 'open?))
  (p 'open! DATA)
  (define tx (texts p))
  (for ([s '("Tests · shop" "$ grep -q READY app.txt" "Protected: tests/**" "add a lint step" "+(step lint \"make lint\")" "2026-10-26 10:00:00 · run · FAIL")])
    (check-true (string-contains? tx s) s))
  (check-true (string-contains? tx "Not trusted yet"))                         ; 长句会折行：只查开头
  (check-equal? (click p 'trust "abc") '(trust "abc"))
  (check-equal? (click p 'run-all) '(run #f))
  (check-equal? (click p 'run "unit") '(run "unit"))
  (check-equal? (click p 'approve "p1") '(approve "p1"))
  (check-equal? (click p 'reject "p1") '(reject "p1"))
  ;; 面板里跑的结果：失败那步可展开输出
  (p 'set-result! RESULT)
  (check-false (string-contains? (texts p) "No such file"))
  (check-false (click p 'toggle "check"))                                          ; 只是展开
  (check-true (string-contains? (texts p) "grep: app.txt: No such file"))
  (check-true (string-contains? (texts p) "✓ unit  2 passed · 0 failed · 0.3 s"))
  (check-false (p 'hit-center 'toggle "unit"))                                     ; 通过的步骤没有输出按钮
  ;; 信任以后：不再有「信任」按钮
  (p 'update! (hash-set DATA 'trusted #t))
  (render p)
  (check-false (p 'hit-center 'trust))
  ;; 关闭
  (check-equal? (click p 'close) '(close))
  (check-false (p 'open?))))

(test-case "no flow: explains how to get one; proposals still reviewable"
  (define p (new-tp))
  (p 'open! (hasheq 'exists #f 'root "/w/new" 'error 'null 'history '() 'proposals (list (hasheq 'id "p9" 'reason "first flow" 'diff (hasheq 'text "+(test-flow)")))))
  (define tx (texts p))
  (check-true (string-contains? tx "/w/new/.pi/tests.rktd"))
  (check-equal? (click p 'approve "p9") '(approve "p9")))

(test-case "English: no untranslated Chinese"
  (parameterize ([current-lang 'en])
    (define p (new-tp))
    (p 'open! DATA) (p 'set-result! RESULT) (click p 'toggle "check")
    (define tx (texts p))
    (check-false (regexp-match #px"[一-鿿]" tx) (format "~s" (regexp-match #px"[一-鿿].{0,10}" tx)))))
