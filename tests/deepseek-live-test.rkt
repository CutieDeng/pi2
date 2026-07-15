#lang tstring racket
;; deepseek-live-test.rkt — 真机：验证 DeepSeek（Anthropic 兼容线路）远端推理能力。
;; 需密钥：env DEEPSEEK_API_KEY 或 `racket main.rkt --set-key DEEPSEEK_API_KEY`。
;; 从离线 raco test 排除（见 tests/info.rkt）；仅显式运行：
;;   racket tests/deepseek-live-test.rkt
;; 无密钥 → 跳过（退出 0），不算失败。

(require
 racket/async-channel
 (file "../src/model.rkt")
 (file "../src/event.rkt")
 (file "../src/provider.rkt")
 (file "../src/provider-anthropic.rkt")
 (file "../src/providers.rkt")
 (file "../src/credentials.rkt")
 (file "../src/pricing.rkt")
) ; end require

(define key (resolve-key "DEEPSEEK_API_KEY"))

(cond
  [(not key)
   (displayln "deepseek-live-test: SKIP (set DEEPSEEK_API_KEY or run: racket main.rkt --set-key DEEPSEEK_API_KEY)")]
  [else
   ;; 用内置档案组装 deepseek config（endpoint/model/key 全部就位），走 anthropic 线路。
   (define cfg
     (struct-copy config
                  (apply-provider-profile
                   (struct-copy config (default-config)
                                [system-prompt "You are a terse assistant. Answer in one short sentence."]
                                [max-tokens 256])
                   "deepseek")))
   (printf "endpoint: ~a  model: ~a  key: ~a\n"
           (config-endpoint cfg) (config-model cfg) (mask-key (config-api-key cfg)))
   (define prov (make-anthropic-provider cfg))
   ;; provider 每请求读 current-config，故 parameterize 之。
   (parameterize ([current-config cfg])
     (define ch (provider-stream! prov
                 (list (text-msg 'user "Reply with exactly: pong. Then stop."))
                 '()))
     (let loop ()
       (define e (async-channel-get ch))
       (cond
         [(evt:delta? e) (display (evt:delta-text e)) (flush-output) (loop)]
         [(evt:message? e) (loop)]
         [(evt:turn-end? e)
          (define u (evt:turn-end-usage e))
          (newline)
          (printf "usage — in:~a out:~a  |  ~a\n"
                  (usage-input-tokens u) (usage-output-tokens u)
                  (cost-line (config-model cfg) u))
          (displayln "deepseek-live-test: OK")]
         [(evt:error? e)
          (eprintf "deepseek-live-test: ERROR ~a\n" (exn-message (evt:error-exn e)))
          (exit 1)]
         [else (loop)])))]
) ; end cond
