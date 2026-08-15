#lang racket/base
;; src/web/dev/gen-entities.rkt — 构建期生成器（design-chrome.md §7.4 数据文件级迁移）
;; 从 vendored 的 WHATWG entities.json 生成 browser/html/entities-data.rkt。
;; 上游升级流程：更新 entities.json（https://html.spec.whatwg.org/entities.json）
;;   → racket src/web/dev/gen-entities.rkt → 跑测试。
;; 注意：本文件在内核 collection 之外（可用 json 等非 racket/* 库）。

(require json racket/runtime-path racket/string)

(define-runtime-path JSON-PATH "../browser/html/entities.json")
(define-runtime-path OUT-PATH "../browser/html/entities-data.rkt")

(define j (call-with-input-file JSON-PATH read-json))

(define named (make-hash))    ; "amp"  → "&"（带分号形态,键去掉 & 与 ;）
(define legacy (make-hash))   ; "amp"  → "&"（无分号历史形态,键去掉 &）
(for ([(k v) (in-hash j)])
  (define s (symbol->string k))                 ; "&AElig;" / "&AElig"
  (define name (substring s 1))
  (define chars (hash-ref v 'characters))
  (if (string-suffix? name ";")
      (hash-set! named (substring name 0 (sub1 (string-length name))) chars)
      (hash-set! legacy name chars))
) ; end for

(define (emit-hash out id h)
  (fprintf out "(define ~a\n  (hash\n" id)
  (for ([k (in-list (sort (hash-keys h) string<?))])
    (fprintf out "   ~s ~s\n" k (hash-ref h k))
  ) ; end for
  (fprintf out "  ) ; end hash\n) ; end define ~a\n\n" id)
) ; end define emit-hash

(call-with-output-file OUT-PATH #:exists 'replace
  (lambda (out)
    (fprintf out "#lang racket/base\n")
    (fprintf out ";; browser/html/entities-data.rkt — L4 · 命名实体全量表（生成文件，勿手改）\n")
    (fprintf out ";; 生成器: src/web/dev/gen-entities.rkt  上游: 同目录 entities.json (WHATWG pin)\n")
    (fprintf out ";; NAMED = 带分号规范形态(键不含 & ;)  LEGACY = 无分号历史形态(键不含 &)\n\n")
    (fprintf out "(provide NAMED-ENTITIES LEGACY-ENTITIES)\n\n")
    (emit-hash out "NAMED-ENTITIES" named)
    (emit-hash out "LEGACY-ENTITIES" legacy)
  ) ; end lambda
) ; end call-with-output-file

(printf "entities-data.rkt: ~a named, ~a legacy\n" (hash-count named) (hash-count legacy))
