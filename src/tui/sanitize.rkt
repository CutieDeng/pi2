#lang tstring racket
;; tui/sanitize.rkt — 不可信文本消毒（design.md §11.6 安全）
;;
;; 来自模型与工具的文本是不可信的：其中可能夹带终端转义/控制序列，用于劫持光标、
;; 改窗口标题(OSC)、清屏、写剪贴板、甚至在个别终端里触发命令回放——即「文本格式注入」。
;; 默认策略：只放行常规可打印 Unicode 与换行/制表，其余 C0/C1/DEL/ESC 一律移除。
;; 移除 ESC(0x1B) 即可瓦解一切 CSI/SS3/OSC/DCS 转义；移除 CR(0x0D) 防止「回车覆盖」把
;; 已落屏内容改写。我们自己的颜色样式在渲染层单独加，不经此函数，故不受影响。

(provide sanitize-untrusted safe-display-char?)

;; 是否允许直接显示：\n \t 与普通可打印字符放行；控制字符/ESC/DEL/C1 拒绝。
(define (safe-display-char? ch)
  (define c (char->integer ch))
  (cond
    [(or (= c 10) (= c 9)) #t]               ; \n \t
    [(< c 32) #f]                            ; 其余 C0（含 ESC=27 / CR=13 / BEL=7）
    [(= c 127) #f]                           ; DEL
    [(and (>= c #x80) (<= c #x9F)) #f]       ; C1 控制区
    [else #t]                                ; 其余 Unicode 码位放行
  ) ; end cond
) ; end define safe-display-char?

;; 过滤掉不安全字符，返回可安全显示的文本（多行/Unicode 保留）。
(define (sanitize-untrusted s)
  (define out (open-output-string))
  (for ([ch (in-string s)] #:when (safe-display-char? ch))
    (write-char ch out)
  ) ; end for
  (get-output-string out)
) ; end define sanitize-untrusted
