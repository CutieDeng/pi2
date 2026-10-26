#lang racket/base
;; tests/gui/contrast-test.rkt — 显示效果的自动检查（compute/contrast.rkt）：WCAG 对比度；字形合成文字段；
;; 段的背景 = 之前画的矩形按顺序合成；被后画的盖住 / 在图片上 / 淡出的字不判；对比度不够的标 low

(require rackunit racket/list "../../src/gui/compute/drawlist.rkt" "../../src/gui/compute/contrast.rkt" "../../src/gui/compute/palette.rkt")

(define (glyphs! dl x y n r g b [a 1.0] #:w [w 7.0] #:h [h 14.0])
  (for ([i n]) (drawlist-glyph! dl (+ x (* i (+ w 1.0))) y w h 0.0 0.0 0.1 0.1 r g b a)))

(test-case "WCAG numbers"
  (check-= (contrast-ratio '(1.0 1.0 1.0) '(0.0 0.0 0.0)) 21.0 0.01)
  (check-= (contrast-ratio '(0.5 0.5 0.5) '(0.5 0.5 0.5)) 1.0 1e-9)
  (check-= (relative-luminance '(1.0 1.0 1.0)) 1.0 1e-9)
  (check-equal? (over '(1.0 1.0 1.0 0.5) '(0.0 0.0 0.0)) '(0.5 0.5 0.5)))

(test-case "glyphs group into runs: same color, same line, adjacent"
  (define dl (make-drawlist))
  (glyphs! dl 10.0 10.0 4 0.9 0.9 0.9)
  (glyphs! dl 60.0 10.0 2 0.5 0.5 0.5)                                          ; 同一行、换色 → 新段
  (glyphs! dl 10.0 40.0 3 0.9 0.9 0.9)                                          ; 下一行 → 新段
  (glyphs! dl 200.0 10.0 2 0.9 0.9 0.9)                                         ; 同色同行但隔得远 → 新段
  (define rs (text-runs (instances dl)))
  (check-equal? (length rs) 4)
  (check-equal? (map (lambda (r) (list (vector-ref r 2) (vector-ref r 3))) rs) '((10.0 10.0) (60.0 10.0) (10.0 40.0) (200.0 10.0))))

(test-case "backgrounds compose in draw order; low contrast, hidden, over image, faint"
  (define dl (make-drawlist))
  (drawlist-rect! dl 0.0 0.0 300.0 200.0 0.11 0.11 0.13 1.0)                   ; 底
  (glyphs! dl 10.0 10.0 5 0.88 0.89 0.92)                                       ; 正文在暗底上：ok
  (drawlist-rect! dl 0.0 40.0 300.0 30.0 0.20 0.28 0.45 1.0)                    ; 选中行的蓝底
  (glyphs! dl 10.0 48.0 5 0.55 0.57 0.62)                                       ; 次要字在蓝底上：low
  (glyphs! dl 10.0 90.0 5 0.88 0.89 0.92)                                       ; 之后被菜单盖住：hidden
  (drawlist-image! dl 0.0 120.0 300.0 40.0 0.0 0.0 1.0 1.0)
  (glyphs! dl 10.0 130.0 5 0.88 0.89 0.92)                                      ; 在图片上：image
  (glyphs! dl 10.0 170.0 5 0.88 0.89 0.92 0.3)                                  ; 淡出：faint
  (drawlist-rect! dl 0.0 80.0 300.0 30.0 0.16 0.17 0.20 1.0)                    ; 菜单（后画）
  (define rep (contrast-report dl))
  (check-equal? (map (lambda (r) (hash-ref r 'status)) rep) '(ok low hidden image faint))
  (check-true (> (hash-ref (first rep) 'ratio) 10.0))
  (check-true (< (hash-ref (second rep) 'ratio) 3.0))
  (check-equal? (map round2 (hash-ref (second rep) 'bg)) '(0.2 0.28 0.45)))   ; drawlist 是 f32

(test-case "clips and rounded corners decide coverage; a caller-given base (e.g. a captured glass pixel) is the bottom"
  (define dl (make-drawlist))
  (drawlist-set-clip! dl (vector 0.0 0.0 50.0 50.0))
  (drawlist-rect! dl 0.0 0.0 300.0 300.0 1.0 1.0 1.0 1.0)                       ; 裁剪到左上 50×50
  (drawlist-set-clip! dl #f)
  (drawlist-rect! dl 100.0 100.0 40.0 40.0 1.0 1.0 1.0 1.0 #:radius 20.0)       ; 圆：角上不算
  (glyphs! dl 60.0 10.0 1 0.9 0.9 0.9 #:w 4.0 #:h 4.0)                         ; 裁剪外：底色
  (glyphs! dl 100.0 100.0 1 0.9 0.9 0.9 #:w 4.0 #:h 4.0)                       ; 圆的角上：底色
  (glyphs! dl 118.0 118.0 1 0.9 0.9 0.9 #:w 4.0 #:h 4.0)                       ; 圆心：白
  (define rep (contrast-report dl #:base (lambda (_x _y) '(0.6 0.6 0.6))))
  (check-equal? (map (lambda (r) (map round2 (hash-ref r 'bg))) rep) '((0.6 0.6 0.6) (0.6 0.6 0.6) (1.0 1.0 1.0)))
  (check-equal? (map (lambda (r) (hash-ref r 'status)) rep) '(low low low)))

(test-case "a card scrolled half out of the clip: sampled where it is visible (inside its border), fully clipped ones are skipped"
  (define dl (make-drawlist))
  (drawlist-set-clip! dl (vector 0.0 0.0 300.0 100.0))                            ; 视口
  (drawlist-rect! dl 10.0 60.0 200.0 120.0 0.55 0.42 0.18 1.0)                    ; 卡片的边（中心 y=120 在视口外）
  (drawlist-rect! dl 11.5 61.5 197.0 117.0 0.15 0.13 0.10 1.0)                    ; 卡片的底
  (glyphs! dl 20.0 70.0 4 0.93 0.94 0.96)                                         ; 卡片里看得见的字
  (drawlist-rect! dl 10.0 200.0 200.0 50.0 0.5 0.5 0.5 1.0)                       ; 完全在视口外的卡片
  (glyphs! dl 20.0 210.0 4 0.93 0.94 0.96)
  (drawlist-set-clip! dl #f)
  (define fills (fill-report dl #:frame-area 1e6))
  (check-equal? (length fills) 1)                                                  ; 边 + 底 = 一张卡片；第三块被裁掉，不判
  (check-equal? (map round2 (hash-ref (first fills) 'bg)) '(0.09 0.09 0.11))      ; 取样在看得见的部分：卡片下面是清屏色（不是视口外的空处）
  (check-equal? (hash-ref (first fills) 'status) 'ok)
  (check-equal? (length (contrast-report dl)) 1))                                 ; 视口外的字不判

(test-case "hole (kind 4): the corners outside the rounded rect are filled, the inside is left to the base (glass over the desktop)"
  (define dl (make-drawlist))
  (drawlist-hole! dl 0.0 0.0 100.0 100.0 0.09 0.09 0.11 1.0 #:radius 30.0)
  (glyphs! dl 1.0 1.0 1 0.9 0.9 0.9 #:w 3.0 #:h 3.0)                            ; 角上：洞外，底是填的颜色
  (glyphs! dl 48.0 48.0 1 0.9 0.9 0.9 #:w 3.0 #:h 3.0)                          ; 中间：洞里，底是 base
  (define rep (contrast-report dl #:base (lambda (_x _y) '(0.5 0.5 0.5))))
  (check-equal? (map (lambda (r) (map round2 (hash-ref r 'bg))) rep) '((0.09 0.09 0.11) (0.5 0.5 0.5))))

(test-case "bordered card (ring + inset body) is judged as one: passes if the ring or the body stands out from what is under it"
  (define dl (make-drawlist))
  (drawlist-rect! dl 0.0 0.0 300.0 200.0 0.20 0.28 0.45 1.0)                      ; 蓝底（选中行）
  (drawlist-rect! dl 10.0 10.0 100.0 60.0 0.30 0.31 0.36 1.0)                      ; 环：和蓝底差不多
  (drawlist-rect! dl 11.0 11.0 98.0 58.0 0.16 0.17 0.20 1.0)                       ; 主体：比蓝底暗得多
  (drawlist-rect! dl 150.0 10.0 100.0 60.0 0.30 0.31 0.36 1.0)                     ; 同样的环 + 和蓝底一样的主体 → 不够
  (drawlist-rect! dl 151.0 11.0 98.0 58.0 0.20 0.28 0.45 1.0)
  (define rep (fill-report dl #:frame-area 1e6 #:max-frac 0.05))                   ; 蓝底（60000）算整块底色，不判
  (check-equal? (length rep) 2)                                                    ; 两张卡片，内层不单独判
  (check-equal? (map (lambda (r) (hash-ref r 'status)) rep) '(ok low)))

(test-case "text and fills under a large translucent modal overlay (Settings dims the window) are not judged"
  (define dl (make-drawlist))
  (drawlist-rect! dl 0.0 0.0 1000.0 600.0 0.09 0.09 0.11 1.0)
  (drawlist-rect! dl 10.0 40.0 220.0 40.0 0.20 0.28 0.45 1.0)                     ; 侧栏的蓝色当前行
  (glyphs! dl 20.0 50.0 5 0.62 0.64 0.69)                                         ; 上面的次要字（单看 < 4.5）
  (drawlist-rect! dl 0.0 0.0 1000.0 600.0 0.0 0.0 0.0 0.45)                       ; 设置页的压暗遮罩
  (check-equal? (map (lambda (r) (hash-ref r 'status)) (contrast-report dl #:frame-area 600000.0)) '(hidden))
  (check-equal? (map (lambda (r) (hash-ref r 'status)) (contrast-report dl)) '(low))           ; 不给整帧面积：照旧判
  (check-equal? (map (lambda (r) (hash-ref r 'status)) (fill-report dl #:frame-area 600000.0)) '(hidden)))

(test-case "text overlapping other text is reported; text under an opaque menu is not"
  (define dl (make-drawlist))
  (drawlist-rect! dl 0.0 0.0 500.0 300.0 0.09 0.09 0.11 1.0)
  (glyphs! dl 10.0 100.0 6 0.9 0.9 0.9)                                           ; 两段字压在一起
  (glyphs! dl 14.0 104.0 6 0.62 0.64 0.69)
  (glyphs! dl 10.0 200.0 6 0.9 0.9 0.9)                                           ; 被菜单盖住的字 + 菜单上的字：不算
  (drawlist-rect! dl 0.0 190.0 200.0 40.0 0.16 0.17 0.20 1.0)
  (glyphs! dl 12.0 202.0 6 0.9 0.9 0.9)
  (glyphs! dl 300.0 100.0 6 0.9 0.9 0.9)                                          ; 不挨着
  (check-equal? (length (text-overlaps (contrast-report dl))) 1))

(test-case "overlap uses the visible (clipped) box: a row scrolled half under a header is not an overlap"
  (define dl (make-drawlist))
  (drawlist-rect! dl 0.0 0.0 500.0 300.0 0.09 0.09 0.11 1.0)
  (glyphs! dl 10.0 100.0 6 0.9 0.9 0.9)                                           ; 栏标题
  (drawlist-set-clip! dl (vector 0.0 112.0 500.0 100.0))                           ; 列表区从标题下面开始
  (glyphs! dl 10.0 106.0 6 0.9 0.9 0.9)                                           ; 滚到一半的行：没裁时与标题相交
  (drawlist-set-clip! dl #f)
  (check-equal? (length (text-overlaps (contrast-report dl))) 0)
  ;; 组合符叠在底字上：同一段
  (define dl2 (make-drawlist))
  (glyphs! dl2 10.0 10.0 1 0.9 0.9 0.9 #:w 10.0)
  (glyphs! dl2 12.0 6.0 1 0.9 0.9 0.9 #:w 4.0)
  (glyphs! dl2 12.0 4.0 1 0.9 0.9 0.9 #:w 4.0)
  (check-equal? (length (text-overlaps (contrast-report dl2))) 0))

(test-case "palette: menu hover fill is visible on the menu (≥ 1.2) and DIM / TEXT on it stay readable (≥ 4.5) — not left to frame sampling"
  (define menu-bg '(0.14 0.15 0.18))                                              ; model-picker.rkt 菜单底
  (define hov (take HOVER-ON-MENU 3))
  (check-true (>= (contrast-ratio hov menu-bg) 1.2))
  (check-true (>= (contrast-ratio (take DIM 3) hov) 4.5))
  (check-true (>= (contrast-ratio (take TEXT 3) hov) 4.5)))
