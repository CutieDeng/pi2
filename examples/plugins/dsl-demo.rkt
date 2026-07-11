#lang s-exp "../../src/pi-plugin-lang.rkt"
;; dsl-demo.rkt — 用声明式插件语言写的插件（design-plugins.md M5）。
;; 演示 deftool / defcommand / defshortcut / on，无样板、无 require。

;; 局部辅助（内部 define，闭包可捕获）
(define prefix "dsl")

;; 一个工具：模型可调用
(deftool shout
  #:desc "Uppercase and exclaim the given text"
  #:params (hasheq 'text (hasheq 'type "string" 'description "text"))
  (lambda (in ctx)
    (ok-outcome (format "~a: ~a!" prefix (string-upcase (input-str in 'text))))))

;; 一个斜杠命令
(defcommand "/dsl" #:desc "say hi from the DSL plugin"
  (lambda (args ctx) ((plugin-ctx-notify ctx) (format "~a plugin here, args=~a" prefix args))))

;; 一个快捷键 Ctrl-Y（编辑器默认粘贴，但插件优先——仅示意，用 Ctrl-J 免冲突）
(defshortcut (kchar #\j '(ctrl))
  (lambda (ctx) ((plugin-ctx-notify ctx) "Ctrl-J from the DSL plugin")))

;; 一个观测钩子
(on 'tool-start (lambda (_e) (void)))
