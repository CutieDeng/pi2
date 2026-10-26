#lang racket/base
;; text-backend.rkt — 文字后端接口（L0，纯数据）。平台层（Core Text）实现它，text/ 与 render/ 只依赖这里，
;; 因而排版 / atlas 可以用假后端做无窗口测试，阶段 2 的自写光栅化（S1）也按同一接口替换。
;;
;; 字符串下标一律是 Racket 码点下标（不是 UTF-16）。

;; 一次 shaping 的结果（单行、不断行）。
;;   glyphs : vector of sglyph，按视觉顺序
;;   width  : 总前进宽度（point）
(struct shaped (glyphs width) #:prefab)

;; font：后端分配的字体 id（含回退字体）；glyph：字形 id；x：相对行首的笔位置（point）；
;; cluster：该字形所属 cluster 的首码点下标（同一 cluster 的多个字形共享）。
(struct sglyph (font glyph x cluster) #:prefab)

;; 后端（全部为过程）：
;;   (font family size)                    → font-id
;;   (shape str font-id)                   → shaped（回退字体由后端自行选择）
;;   (metrics font-id)                     → (vector ascent descent leading)，point
;;   (rasterize font-id glyph scale subpx) → #f（空字形）或 (vector w h left top rgba-bytes color?)
;;        像素单位；位图左上角放在 (floor(pen_px) + left, baseline_px - top)；
;;        rgba-bytes 为 w*h*4 预乘 RGBA，行 0 = 顶部；color? = 彩色字形（emoji），否则只用 alpha
(struct text-backend (font shape metrics rasterize))

(provide (struct-out shaped) (struct-out sglyph) (struct-out text-backend))
