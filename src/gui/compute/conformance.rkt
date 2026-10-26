#lang racket/base
;; conformance.rkt — 渲染器一致性的判据（数据）：平台渲染器与参考光栅器（softraster.rkt）对同一 drawlist 的像素差。
;;   通道差 > CONFORMANCE-CHANNEL-TOLERANCE 的像素占比 ≤ CONFORMANCE-MAX-FRACTION，且最大通道差 ≤ CONFORMANCE-MAX-DIFF。
;;   实测（2026-10-26，macOS Metal vs 参考光栅器，app.rkt --conformance）：
;;     text@2 最大差 1、ui-chips@2 最大差 2（都是 0% 超差）；rects 最大差 58、超差 0.01%；clip+hole 最大差 57、超差 0.0%。
;;     差异只在很小 / 小数坐标矩形的抗锯齿边缘（GPU 的 fwidth 按 2×2 像素块求导，参考光栅器用前向差分）。
;;   阈值 = 实测 + 余量。

(define CONFORMANCE-CHANNEL-TOLERANCE 8)       ; 8/255：内部像素逐值一致，边缘允许这么多
(define CONFORMANCE-MAX-FRACTION 0.005)        ; 超差像素占比上限（实测 ≤ 0.0001）
(define CONFORMANCE-MAX-DIFF 64)               ; 单像素最大通道差上限（实测 58）
(define (conformance-pass? max-diff fraction)
  (and (<= max-diff CONFORMANCE-MAX-DIFF) (<= fraction CONFORMANCE-MAX-FRACTION)))

(provide CONFORMANCE-CHANNEL-TOLERANCE CONFORMANCE-MAX-FRACTION CONFORMANCE-MAX-DIFF conformance-pass?)
