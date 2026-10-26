#lang racket/base
;; tests/gui/review-test.rkt — 变更清单的审阅面板（design-workspace.md §11.7 C3）：缺省勾选（能做的、删除不勾）、逐项 / 整组勾选、
;; 按状态的按钮（待审阅：应用所选 / 全部放弃；已应用：撤销整份 / 关闭）、已应用的只读（没有勾选框）

(require rackunit racket/list
         "../../src/gui/compute/drawlist.rkt" "../../src/gui/compute/interfaces.rkt"
         "../../src/gui/components/review.rkt" "fakes.rkt")

(define ITEMS
  (list (hasheq 'n 0 'op "new-project" 'project "Market" 'status "ok" 'reason "two sessions")
        (hasheq 'n 1 'op "link" 'project "Market" 'title "market tool" 'status "ok" 'reason "about market")
        (hasheq 'n 2 'op "link" 'project "Market" 'title "prices" 'status "ok")
        (hasheq 'n 3 'op "archive" 'title "old chat" 'status "ok")
        (hasheq 'n 4 'op "rename" 'title "gui" 'new_title "GUI sidebar" 'status "ok")
        (hasheq 'n 5 'op "delete" 'title "scratch" 'status "ok")
        (hasheq 'n 6 'op "pin" 'title "gone" 'status "stale" 'why "session no longer exists")))
(define DATA (hasheq 'id "c3" 'by "agent:test" 'when "2026-10-25 15:20" 'request "organize last week" 'summary "group market work" 'state 'proposed 'items ITEMS))

(define (new-rv) (instantiate-with review-component review-view))
(define (render r) (define dl (make-drawlist)) (r 'render! dl 240.0 0.0 720.0 900.0 2.0) dl)
(define (click r kind [payload #t]) (let ([c (r 'hit-center kind payload)]) (and c (r 'hit (car c) (cdr c)))))

(test-case "default selection, item / group toggles, buttons for a proposed changeset"
  (define r (new-rv))
  (r 'open! DATA)
  (render r)
  (check-equal? (r 'accepted) '(0 1 2 3 4))                                       ; 删除（5）不勾、过期（6）不能勾
  (check-false (r 'hit-center 'toggle 6))
  (check-equal? (click r 'toggle 5) '(toggle 5))
  (check-equal? (r 'accepted) '(0 1 2 3 4 5))
  (render r)
  (check-equal? (click r 'toggle-group '(link . "Market")) '(toggle-group (link . "Market")))   ; 整组：全勾着 → 全去掉
  (check-equal? (r 'accepted) '(0 3 4 5))
  (render r)
  (click r 'toggle-group '(link . "Market"))
  (check-equal? (r 'accepted) '(0 1 2 3 4 5))
  (render r)
  (check-equal? (click r 'apply) '(apply))
  (check-equal? (click r 'discard) '(discard))
  (check-false (r 'hit-center 'undo))
  (check-equal? (click r 'close) '(close)))

(test-case "an applied changeset is read-only: no checkboxes; Undo all / Close"
  (define r (new-rv))
  (r 'open! (hash-set* DATA 'state 'applied 'accepted '(0 1 3)))
  (render r)
  (check-equal? (r 'accepted) '(0 1 3))
  (check-false (r 'hit-center 'toggle 1))
  (check-false (r 'hit-center 'apply))
  (check-equal? (click r 'undo) '(undo)))
