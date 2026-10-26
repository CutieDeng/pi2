#lang racket/base
;; dump-first-request.rkt — 用 pi2 真实装配层 dump dsv4-b 首请求整包 JSON。
;; 不猜、不手搓：复用 build-anthropic-body + 真实 bash/editor tool-spec + ANCHOR-SYSTEM。
;;   racket tests/dsv4b-probes/dump-first-request.rkt > /tmp/dsv4b-first-request.json
(require json
         (file "../../src/model.rkt")
         (file "../../src/tool.rkt")
         (file "../../src/provider-anthropic.rkt")
         (file "../../src/dsv4b.rkt")
         (file "../../src/tools/bash-persistent.rkt")
         (file "../../src/tools/editor.rkt"))

(current-reasoning-effort 'max)                 ; dsv4-b boot 的默认
(define reg (make-registry (list (make-persistent-bash-tool) (make-editor-tool))))
(define cfg (struct-copy config (default-config)
                         [model "deepseek-v4-pro"]
                         [system-prompt ANCHOR-SYSTEM]))
(define msgs (list (text-msg 'user "Fix the failing test in ./foo.py")))  ; ← 用户内容（变量）
(define body (build-anthropic-body cfg msgs (registry-specs reg)))
(write-json body)
(newline)
