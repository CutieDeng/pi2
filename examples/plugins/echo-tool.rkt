#lang tstring racket
;; echo-tool.rkt — 受信（trusted）示例插件。
;; 经 dynamic-require 载入：provide 一个 `plugin` 注册函数，用 SDK 注册一个 gen:tool。

(require (file "../../src/plugin.rkt"))

(provide plugin)

(define (plugin api)
  ((plugin-api-register-tool! api)
   (make-simple-tool
    #:name "echo"
    #:desc "Echo back the given text"
    #:params (hasheq 'text (hasheq 'type "string" 'description "text to echo"))
    #:run (lambda (input _ctx)
            (ok-outcome f"echo: {(input-str input 'text)}"))))
) ; end define plugin
