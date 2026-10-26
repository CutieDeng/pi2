#lang racket/base
;; tokens.rkt — 设计参数（GUI 元数据平面 4）：字号表、尺寸、圆角、各平台的字体族。纯数据，各平台共用；
;; 组件按名字取（tk 'menu-row-h），不在自己文件里写死数字。颜色在 palette.rkt（有对比度检查约束），不在这里。
;;   字体是「角色」：system / system-bold / system-italic / system-bold-italic / monospace；平台的文字后端把角色映射到实际字体
;;   （macOS：Core Text 的 UI 字体，CJK 自动回退 PingFang SC——已验证；Windows：FONT-FAMILIES 里的草案，status documented）。

;; 字号表：字体键 → (角色 . 字号 point)（原来在 components/text-system.rkt）
(define TYPE-SCALE
  (hasheq 'body '("system" . 14) 'title '("system-bold" . 17) 'mono '("monospace" . 13) 'small '("system" . 11)
          'bold '("system-bold" . 14) 'italic '("system-italic" . 14) 'bold-italic '("system-bold-italic" . 14)
          'code '("monospace" . 12.5) 'h1 '("system-bold" . 21) 'h2 '("system-bold" . 18) 'h3 '("system-bold" . 15.5)))

;; 尺寸（point）：名字 → 值。菜单类（模型选择）先迁过来；其余组件迁移时补
(define METRICS
  (hasheq 'menu-row-h 26.0          ; 菜单里一行
          'menu-head-h 24.0         ; 菜单栏的表头
          'menu-title-h 22.0        ; 菜单里的分组标题 / 底栏标题
          'menu-pad 10.0            ; 菜单内边距
          'menu-max-h 360.0         ; 菜单高度上限
          'picker-menu-w 520.0      ; 模型菜单宽
          'picker-left-w 200.0      ; 模型菜单左栏宽
          'radius-s 5.0 'radius-m 6.0 'radius-l 8.0
          'scrollbar-w 3.0 'scrollbar-min 20.0
          'chip-pad-x 8.0))
(define (tk name) (hash-ref METRICS name (lambda () (error 'tk "no design token ~s" name))))

;; 各平台把字体角色映射到哪些字体族（按顺序回退）。macOS 一行是实际做法的说明；Windows 是按微软文档写的草案
(define FONT-FAMILIES
  '((macos (status verified)
           ("system" "CTFontCreateUIFontForLanguage(kCTFontUIFontSystem); CJK falls back to PingFang SC automatically")
           ("system-bold" "kCTFontUIFontEmphasizedSystem")
           ("monospace" "kCTFontUIFontUserFixedPitch (Menlo / SF Mono)"))
    (windows (status documented)
             ("system" "Segoe UI Variable" "Segoe UI" "Microsoft YaHei UI")
             ("system-bold" "Segoe UI Variable (weight 600)" "Segoe UI Semibold" "Microsoft YaHei UI Bold")
             ("monospace" "Cascadia Mono" "Consolas" "Microsoft YaHei UI"))))

(provide TYPE-SCALE METRICS tk FONT-FAMILIES)
