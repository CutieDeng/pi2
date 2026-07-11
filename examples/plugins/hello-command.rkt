#lang tstring racket
;; hello-command.rkt — 受信插件：注册一个斜杠命令 + 一个观测钩子（演示命令/钩子扩展面）。

(require (file "../../src/plugin.rkt"))

(provide plugin)

(define (plugin api)
  ;; 斜杠命令 /hello：经 ctx.notify 向用户显示消息。
  ((plugin-api-register-command! api) "/hello"
   (hasheq 'desc "greet from a plugin" 'args ""
           'handler (lambda (args ctx)
                      ((plugin-ctx-notify ctx) f"Hello from the plugin! (args: {args})"))))
  ;; 观测钩子：每次工具开始执行时可做副作用（此处仅示意，不打扰）。
  ((plugin-api-on! api) 'tool-start (lambda (_e) (void))))
