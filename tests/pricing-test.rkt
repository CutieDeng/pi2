#lang tstring racket
;; pricing-test.rkt — 记费估算（离线）：单价匹配、成本计算、格式化、覆盖文件。

(require
 rackunit
 racket/file
 (file "../src/model.rkt")
 (file "../src/pricing.rkt")
) ; end require

;; 隔离配置目录（pricing 覆盖文件从 config-home 读）
(define tmp (make-temporary-file "pi2-pricetest-~a" 'directory))
(putenv "PI_CONFIG_HOME" (path->string tmp))

(test-case "exact model name resolves price"
  (define p (model-price-for "deepseek-chat"))
  (check-equal? (model-price-input p) 0.28)
  (check-equal? (model-price-output p) 0.42)
) ; end test-case

(test-case "longest-prefix match for versioned model ids"
  ;; "claude-sonnet-5-20991231" 应命中 "claude-sonnet"（而非更短的其它前缀）
  (define p (model-price-for "claude-sonnet-5-20991231"))
  (check-equal? (model-price-input p) 3.0)
  (check-equal? (model-price-output p) 15.0)
) ; end test-case

(test-case "unknown model → #f (cost n/a)"
  (check-false (model-price-for "totally-unknown-model"))
  (check-false (estimate-cost "totally-unknown-model" (usage 1000 1000)))
) ; end test-case

(test-case "estimate-cost computes input*in + output*out per 1M"
  ;; deepseek-chat: 1M input @0.28 + 0.5M output @0.42 = 0.28 + 0.21 = 0.49
  (define c (estimate-cost "deepseek-chat" (usage 1000000 500000)))
  (check-= c 0.49 1e-9)
) ; end test-case

(test-case "local model priced 0"
  (check-= (estimate-cost "gemma-4-31b-it@6bit" (usage 100000 100000)) 0.0 1e-12)
) ; end test-case

(test-case "format-cost: sub-cent uses 6 decimals, else 4"
  (check-equal? (format-cost 0.0000123) "$0.000012")
  (check-equal? (format-cost 1.23456789) "$1.2346")
  (check-equal? (format-cost "n/a") "n/a")
) ; end test-case

(test-case "fmt-tok compacts thousands/millions"
  (check-equal? (fmt-tok 512) "512")
  (check-equal? (fmt-tok 1500) "1.5k")
  (check-equal? (fmt-tok 2300000) "2.3M")
) ; end test-case

(test-case "cost-line reflects known vs unknown"
  (check-true (string-contains? (cost-line "deepseek-chat" (usage 1000 1000)) "≈ $"))
  (check-true (string-contains? (cost-line "unknown-x" (usage 1000 1000)) "n/a"))
) ; end test-case

(test-case "pricing.rktd override wins over defaults"
  (with-output-to-file (build-path tmp "pricing.rktd") #:exists 'replace
    (lambda () (write (hash "deepseek-chat" (list 9.99 19.99)))))
  (define p (model-price-for "deepseek-chat"))
  (check-equal? (model-price-input p) 9.99)
  (check-equal? (model-price-output p) 19.99)
) ; end test-case

(delete-directory/files tmp)
(displayln "pricing-test: all passed")
