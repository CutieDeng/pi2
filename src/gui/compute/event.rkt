#lang racket/base
;; event.rkt — 与平台无关的输入事件（L0，纯数据）。平台层把 NSEvent 翻译成这些 prefab。
;; 坐标单位：point，原点在内容区左上角；backing scale 单独给出。
;; mods：符号表，取自 '(shift ctrl alt cmd)。

(struct ev:resize (w h scale) #:prefab)              ; 内容区尺寸（point）+ backing scale
(struct ev:close () #:prefab)                        ; 用户请求关窗（Cmd-Q / 红点），由 app 决定是否退出
(struct ev:focus (on?) #:prefab)
(struct ev:key (down? code chars mods repeat?) #:prefab)   ; code = 硬件 keyCode；chars = 字符串或 #f
(struct ev:text (str) #:prefab)                      ; G2：上屏文本（IME 提交 / 普通输入）
(struct ev:marked (str sel-start sel-len) #:prefab)  ; G2：IME 组字中
(struct ev:mouse (kind x y button mods clicks) #:prefab)   ; kind ∈ down up drag move；button ∈ 0 左 1 右 2 中
(struct ev:scroll (dx dy precise? phase x y) #:prefab)   ; precise? = 触控板像素级增量；phase ∈ none began changed ended momentum；
                                                         ; x y = 指针位置（滚轮按指针所在区域分派，不跟焦点）
(struct ev:command (name) #:prefab)                  ; 编辑命令，名字沿用 AppKit selector（'deleteBackward: 'copy: …）
(struct ev:paste (text) #:prefab)                    ; 粘贴（剪贴板字符串；无字符串内容时为 #f）
;; 附件（粘贴的图片 / 文件、拖进窗口的文件）：items = 表 of (vector kind 类型 名字 字节 大小 sha)
;;   kind ∈ image audio file；窗口给出字节（sha #f）；操作日志里字节换成 #f、记 sha（不记内容）
(struct ev:attach (items) #:prefab)
(struct ev:secret (text label) #:prefab)             ; 只出现在操作日志里：输 key 时的按键不记，改记一条句柄（⟦secret:…⟧）；
                                                     ; 回放时用它填输入框。窗口本身不产生这种事件

(provide
 (struct-out ev:resize) (struct-out ev:close) (struct-out ev:focus) (struct-out ev:key)
 (struct-out ev:text) (struct-out ev:marked) (struct-out ev:mouse) (struct-out ev:scroll)
 (struct-out ev:command) (struct-out ev:paste) (struct-out ev:secret) (struct-out ev:attach)
) ; end provide
