#lang tstring racket
;; echo-provider.rkt — 受信插件：注册一个自定义 LLM 供应商 "echollm"（演示 register-provider!）。
;; 不联网：回复即回显最后一条 user 消息。用 `--provider echollm` 选用。

(require (file "../../src/plugin.rkt") (file "../../src/model.rkt"))

(provide plugin)

(define (last-user-text msgs)
  (define users (filter (lambda (m) (eq? (message-role m) 'user)) msgs))
  (if (null? users) "" (message-text (last users))))

(define (plugin api)
  ((plugin-api-register-provider! api) "echollm"
   ;; 工厂：(config -> provider)。此处忽略 cfg，返回一个回显供应商。
   (lambda (_cfg)
     (make-simple-provider
      #:name "echollm"
      #:reply (lambda (msgs) f"echo-llm reply: {(last-user-text msgs)}")))))
