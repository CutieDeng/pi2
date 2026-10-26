#lang racket/base
;; gesture.rkt — 交互识别（GUI 元数据平面 3）：逻辑事件 + 平台交互配置的常数 → 意图。纯函数，状态由调用方保存。
;;   规则与改造前 app.rkt 里写死的相同（数值来自 interaction.rkt 的配置常数，不再散在代码里）：
;;     按下：右键（button 1）或按着 ctrl → context；中键 → auxiliary；侧键 3 / 4 → navigate；连击 ≥ 2 → activate-double；否则 activate
;;     滚轮：非像素级（经典鼠标）一格 = wheel-line-points
;;     横扫：像素级滚动 began 时记下指针下的行；累计横向 > swipe-start 且 > swipe-dominance × 纵向 → 开始横扫；纵向 > swipe-cancel → 普通滚动
;;     拖行：按下后 |dx| + |dy| > drag-start → 开始拖

(require "interaction.rkt")

;; 平台的识别常数（数值常数；"system" 的由平台层给，这里用 macOS 的实测值兜底）
(struct gesture-consts (wheel-line swipe-start swipe-dominance swipe-cancel drag-start) #:transparent)
(define (consts-for platform)
  (define (num name fallback) (let ([v (profile-constant platform name)]) (if (real? v) v fallback)))
  (gesture-consts (num 'wheel-line-points 20.0) (num 'swipe-start 6.0) (num 'swipe-dominance 1.5)
                  (num 'swipe-cancel 6.0) (num 'drag-start 6.0)))

;; 按下 → 意图
(define (press-intent button mods clicks)
  (cond [(or (eqv? button 1) (memq 'ctrl mods)) 'context]
        [(eqv? button 2) 'auxiliary]
        [(eqv? button 3) 'navigate-back]
        [(eqv? button 4) 'navigate-forward]
        [(and (exact-integer? clicks) (>= clicks 2)) 'activate-double]
        [else 'activate]))

;; 滚动量 → point
(define (scroll-points c precise? d) (if precise? d (* (gesture-consts-wheel-line c) d)))

;; 横扫识别的一步。cand：#f | (vector 行 累计dx 累计dy)；row：began 时指针下的行（可横扫的才给，否则 #f）
;; → (values 新的 cand 结果)，结果：'scroll（照常滚动）| (list 'swipe 行 累计dx)（开始横扫）| 'pending（还在判断，照常滚动）
(define (swipe-step c cand phase precise? row dx dy)
  (cond
    [(and precise? (eq? phase 'began)) (values (vector row 0.0 0.0) 'scroll)]
    [(and cand (eq? phase 'changed))
     (define n (vector (vector-ref cand 0) (+ (vector-ref cand 1) dx) (+ (vector-ref cand 2) dy)))
     (cond [(and (vector-ref n 0)
                 (> (abs (vector-ref n 1)) (gesture-consts-swipe-start c))
                 (> (abs (vector-ref n 1)) (* (gesture-consts-swipe-dominance c) (abs (vector-ref n 2)))))
            (values #f (list 'swipe (vector-ref n 0) (vector-ref n 1)))]
           [(> (abs (vector-ref n 2)) (gesture-consts-swipe-cancel c)) (values #f 'scroll)]
           [else (values n 'pending)])]
    [(eq? phase 'ended) (values #f 'scroll)]
    [else (values cand 'scroll)]))

;; 拖起：按下点 (x0 y0) → 现在 (x y)
(define (drag-started? c x0 y0 x y) (> (+ (abs (- x x0)) (abs (- y y0))) (gesture-consts-drag-start c)))

(provide (struct-out gesture-consts) consts-for press-intent scroll-points swipe-step drag-started?)
