#lang racket/base
;; tests/gui/input-test.rkt — 输入框组件（只实例化这一个组件，text-layout 用假实现，无 GPU / 窗口）：chip 排版与命中、预览面板按钮、
;; 整行滚动、上下移动

(require rackunit racket/list racket/string
         "../../src/gui/compute/drawlist.rkt" "../../src/gui/compute/interfaces.rkt"
         "../../src/gui/compute/fold.rkt" "../../src/gui/compute/editor.rkt" "../../src/gui/compute/rich.rkt"
         "../../src/gui/components/input.rkt" "fakes.rkt")

;; 单个组件 + 假 text-layout
(define (new-ib) (instantiate-with input-component input-box))

(define LOG (apply string-append (for/list ([i 12]) (format "2026-10-22 16:00:0~a INFO worker ~a\n" (modulo i 10) i))))

(define (render ib #:w [w 600.0])
  (define dl (make-drawlist))
  (values (ib 'render! dl 0.0 400.0 w 2.0 #t #t) dl))

(test-case "chip is laid out inline; click on it hits the fold, click on text hits an index"
  (define iv (new-ib))
  (define e (iv 'editor))
  (editor-insert! e "ab ")
  (define f (editor-paste! e LOG))
  (editor-insert! e " cd")
  (define-values (top dl) (render iv))
  (check-true (< top 400.0))
  ;; 文字起点 = 框左 12；"ab " 宽 30 → chip 框从 x=42 开始，chip 本体再缩进 CHIP-GAP
  (define chip-x (+ 12.0 30.0 CHIP-GAP 5.0))
  (define line-mid (+ (- 400.0 12.0 12.5) 6.0))
  (check-equal? (iv 'hit chip-x line-mid) (list 'chip (fold-id f)))
  (check-equal? (iv 'hit 13.0 line-mid) (list 'text 0))
  (check-equal? (iv 'hit 33.0 line-mid) (list 'text 2))    ; "a|b" 之后靠近 x=20…30 → 2
  (check-false (iv 'hit 300.0 10.0)))                       ; 框外

(test-case "expanded chip shows a peek panel with unfold / remove buttons that hit-test"
  (define iv (new-ib))
  (define e (iv 'editor))
  (define f (editor-paste! e LOG))
  (define-values (top0 _d0) (render iv))
  (editor-toggle-expand! e (fold-id f))
  (define-values (top1 _d1) (render iv))
  (check-true (< top1 (- top0 50.0)))                             ; 面板在框上方
  ;; 在面板顶部那一行横扫，找到两个按钮
  (define hits (remove-duplicates
                (filter values (for*/list ([y (in-range (+ top1 8.0) (+ top1 24.0) 2.0)] [x (in-range 0.0 600.0 3.0)])
                                 (let ([h (iv 'hit x y)]) (and h (eq? (car h) 'button) h))))))
  (check-equal? (sort (map cadr hits) symbol<?) '(remove unfold))
  (check-true (andmap (lambda (h) (= (caddr h) (fold-id f))) hits)))

(test-case "long input: box grows to MAX-LINES then scrolls by whole lines to keep the caret visible"
  (define iv (new-ib))
  (define e (iv 'editor))
  (editor-insert! e "x")
  (define-values (top1 _a) (render iv))
  (for ([i 19]) (editor-insert! e "\nline"))
  (define-values (top20 _b) (render iv))
  (check-= (- 400.0 top20) (+ (* 8 12.5) 24.0) 1e-9)             ; 8 行 + 上下内边距
  (check-true (> (- 400.0 top20) (- 400.0 top1)))
  ;; 光标在末行 → 最后一行可见：点框内最底部一行得到末行的下标
  (define hit (iv 'hit 15.0 (- 400.0 12.0 6.0)))
  (check-equal? (car hit) 'text)
  (check-true (> (cadr hit) (- (string-length (editor-text e)) 5))))

(test-case "up/down move by visual lines; top / bottom edges are reported for history"
  (define iv (new-ib))
  (define e (iv 'editor))
  (editor-insert! e "first line\nsecond line")
  (render iv)
  (editor-move-to! e 14)                                           ; second 的 "c" 附近
  (render iv)
  (check-eq? (iv 'move-vertical! -1 #f) 'handled)
  (check-equal? (editor-cursor e) 3)                               ; 同一 x 的上一行位置
  (render iv)
  (check-eq? (iv 'move-vertical! -1 #f) 'top)
  (editor-move-to! e (string-length (editor-text e)))
  (render iv)
  (check-eq? (iv 'move-vertical! 1 #f) 'bottom))

(test-case "empty editor renders the placeholder and the caret sits at 0"
  (define iv (new-ib))
  (define e (iv 'editor))
  (define-values (top dl) (render iv))
  (check-true (> (drawlist-count dl) 10))                          ; 占位提示的字形
  (check-equal? (vector-ref (iv 'caret-rect) 0) 12.0))

(test-case "regression: clicking on the placeholder of an empty box hits index 0 (was vector-ref out of range)"
  (define iv (new-ib))
  (define-values (top dl) (render iv))
  (define cr (iv 'caret-rect))
  (for ([dx '(5.0 60.0 200.0 500.0)])                              ; 占位提示文字上的各处
    (check-equal? (iv 'hit (+ (vector-ref cr 0) dx) (+ (vector-ref cr 1) 4.0)) '(text 0))))

(test-case "wheel over a long input box scrolls by lines and stays there until the caret or the text changes"
  (define iv (new-ib))
  (define e (iv 'editor))
  (editor-insert! e (string-join (for/list ([i 20]) (format "line ~a" i)) "\n"))   ; 光标在末尾：显示最后 8 行
  (render iv)
  (define cr0 (iv 'caret-rect))
  (check-false (iv 'scroll! 5.0 5.0 100.0))                                     ; 指针不在输入框上
  (check-true (iv 'scroll! (vector-ref cr0 0) (vector-ref cr0 1) 1000.0))       ; 往上滚到顶
  (render iv)
  (check-equal? (iv 'hit 13.0 (- (vector-ref cr0 1) (* 7 12.5) -4.0)) '(text 0))   ; 第一行可见
  (render iv)
  (check-equal? (iv 'hit 13.0 (- (vector-ref cr0 1) (* 7 12.5) -4.0)) '(text 0))   ; 重绘不弹回
  (editor-insert! e "x")                                                          ; 一编辑：回到跟随光标
  (render iv)
  (check-false (equal? (iv 'hit 13.0 (- (vector-ref cr0 1) (* 7 12.5) -4.0)) '(text 0))))
