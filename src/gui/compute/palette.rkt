#lang racket/base
;; palette.rkt — 界面的颜色（只有暗色主题；窗口强制 DarkAqua）。各组件共用这一份，显示效果的检查（compute/contrast.rkt，design-gui.md §28）按实际画出来的值算：
;;   字与背景 ≥ 4.5（WCAG AA 正文），非文字的填充与下面的背景 ≥ 1.2，底的相对亮度 ≤ 0.08。
;; 颜色：(r g b a)，sRGB 0..1，非预乘。

(define TEXT '(0.90 0.91 0.93 1.0))             ; 正文
(define DIM '(0.62 0.64 0.69 1.0))              ; 次要文字：在常用的最亮的底（按钮 0.27 灰）上仍 ≥ 4.5（0.55 时在芯片 / 按钮上只有 4.2–4.3）
(define DIM-ON-ACCENT '(0.80 0.83 0.90 1.0))    ; 强调底（选中行的蓝、临时会话的淡蓝）上的次要文字（DIM 在蓝底上只有 2.6）
(define BUTTON '(0.32 0.33 0.40 1.0))           ; 侧栏的按钮：玻璃上铺了暗色后最亮的底约 0.21；0.20 的按钮与它 1.03、0.27 的不可用态 1.19
(define BUTTON-OFF-ALPHA 0.6)                    ; 不可用的按钮：同色、半透明
(define TEXT-OFF '(0.74 0.76 0.81 1.0))         ; 不可用的按钮上的字：比正文暗（看得出不可用），在最亮的玻璃上仍 ≥ 4.5（DIM 只有 4.1，0.70 在 0.32 的按钮下 4.48）
(define HOVER-ON-MENU '(0.21 0.22 0.27))         ; 菜单（底 0.14 0.15 0.18）上悬停的行：原来 0.19 0.20 0.24 只有 1.198（填充下限 1.2）；现在 1.30，上面的 DIM 字 4.61、TEXT 9.47（0.22 0.23 0.28 时 DIM 只有 4.44）
(define HOVER-ON-GLASS '(0.30 0.31 0.37 0.6))   ; 玻璃上悬停的行：原来 0.17 × 0.55 在最亮的玻璃上只有 1.06–1.08
(define EPHEMERAL-FILL '(0.24 0.34 0.55))       ; 临时会话的行（不透明度起伏 0.50–0.75）：0.20/0.28/0.45 在最亮的玻璃上最淡时只有 1.17
;; 角标（运行中 / 等待权限）：实心浅色 + 深色字。原来深底浅字：警告色字在棕底上 3.9，运行中的蓝底与选中行的蓝几乎一样（1.0x）
(define BADGE-WARN '(0.95 0.55 0.45)) (define BADGE-WARN-TEXT '(0.18 0.08 0.05 1.0))
(define BADGE-BUSY '(0.55 0.70 1.00)) (define BADGE-BUSY-TEXT '(0.06 0.10 0.20 1.0))
;; 玻璃上的侧栏先铺的暗色（盖在玻璃上、字下面）：「透出桌面」时窗口后面可能是白的——暗色玻璃在白底上实测亮度 0.106，
;; 次要文字只有 2.7；铺 0.55 后最亮约 0.035，次要文字 ≥ 4.5（design-gui.md §28）
(define GLASS-SCRIM '(0.08 0.08 0.10 0.55))

(provide TEXT DIM DIM-ON-ACCENT BUTTON BUTTON-OFF-ALPHA TEXT-OFF HOVER-ON-MENU HOVER-ON-GLASS GLASS-SCRIM EPHEMERAL-FILL BADGE-WARN BADGE-WARN-TEXT BADGE-BUSY BADGE-BUSY-TEXT)
