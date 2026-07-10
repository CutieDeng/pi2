#lang tstring racket
;; tui-picker-test.rkt — 选择器纯状态机 + 渲染 + 脚本化运行（离线）。

(require
 rackunit
 (file "../src/tui/keys.rkt")
 (file "../src/tui/terminal.rkt")
 (file "../src/tui/picker.rkt")
) ; end require

(define (step st bytes)
  (pick-step st (car (parse-keys bytes))))

;; ---------------------------------------------------------------- 状态机

(test-case "navigation clamps at both ends"
  (define st0 (pick-init 5))
  (check-equal? (pick-state-idx st0) 0)
  (define-values (s1 _1) (step st0 #"\e[B")) (check-equal? (pick-state-idx s1) 1)  ; down
  (define-values (s2 _2) (step s1  #"\e[A")) (check-equal? (pick-state-idx s2) 0)  ; up
  (define-values (s3 _3) (step s2  #"\e[A")) (check-equal? (pick-state-idx s3) 0)  ; up clamp
  (define-values (s4 _4) (step s3  #"\e[F")) (check-equal? (pick-state-idx s4) 4)  ; end
  (define-values (s5 _5) (step s4  #"\e[B")) (check-equal? (pick-state-idx s5) 4)  ; down clamp
) ; end test-case

(test-case "home/end jump; vim j/k move"
  (define st (pick-init 5))
  (define-values (se _e) (step st #"\e[F")) (check-equal? (pick-state-idx se) 4)   ; end
  (define-values (sh _h) (step se #"\e[H")) (check-equal? (pick-state-idx sh) 0)   ; home
  (define-values (sj _j) (step sh #"j"))    (check-equal? (pick-state-idx sj) 1)   ; vim down
  (define-values (sk _k) (step sj #"k"))    (check-equal? (pick-state-idx sk) 0)   ; vim up
) ; end test-case

(test-case "enter chooses current index; esc/q/ctrl-c cancel"
  (define st (pick-init 5))
  (define-values (sd _d) (step st #"\e[B"))
  (define-values (_s r) (step sd #"\r"))
  (check-equal? r (list 'chosen 1))
  (define-values (_s2 r2) (step st #"\e")) (check-eq? r2 'cancel)    ; ESC
  (define-values (_s3 r3) (step st #"q"))  (check-eq? r3 'cancel)    ; q
  (define-values (_s4 r4) (step st (bytes 3))) (check-eq? r4 'cancel); Ctrl-C
) ; end test-case

(test-case "empty list cancels immediately"
  (define-values (_s r) (step (pick-init 0) #"\r"))
  (check-eq? r 'cancel)
) ; end test-case

;; ---------------------------------------------------------------- 渲染

(test-case "render produces exactly H lines with title, items, hint, highlight"
  (define lines (pick-render-lines (pick-init 3) '("apple" "banana" "cherry")
                                   "Pick one" (lambda (x) x) 40 10))
  (check-equal? (length lines) 10)
  (check-true (string-contains? (car lines) "Pick one"))
  (check-true (ormap (lambda (l) (string-contains? l "banana")) lines))
  (check-true (string-contains? (last lines) "1/3"))          ; 计数提示
  ;; 选中项（apple）反显
  (check-true (ormap (lambda (l) (and (string-contains? l "apple")
                                      (string-contains? l "\e[7m"))) lines))
) ; end test-case

(test-case "windowing keeps the selection visible in a small viewport"
  ;; 50 项、H=6（→ 4 项可见），选中第 40 项应在窗口内
  (define st (let loop ([s (pick-init 50)] [k 0]) (if (>= k 40) s
                  (let-values ([(s* _r) (pick-step s (car (parse-keys #"\e[B")))]) (loop s* (add1 k))))))
  (define lines (pick-render-lines st (build-list 50 (lambda (i) f"item{i}")) "t" (lambda (x) x) 30 6))
  (check-true (ormap (lambda (l) (string-contains? l "item40")) lines))
) ; end test-case

;; ---------------------------------------------------------------- 脚本化运行

(test-case "run-picker returns the chosen index"
  (define-values (term st) (make-scripted-terminal (bytes-append #"\e[B" #"\e[B" #"\r")))
  (check-equal? (run-picker term '("a" "b" "c" "d")) 2)     ; down down enter
) ; end test-case

(test-case "run-picker returns #f on cancel"
  (define-values (term st) (make-scripted-terminal #"\e"))
  (check-false (run-picker term '("a" "b" "c")))
) ; end test-case

(test-case "run-picker on empty list is #f"
  (define-values (term st) (make-scripted-terminal ""))
  (check-false (run-picker term '()))
) ; end test-case

(displayln "tui-picker-test: all passed")
