#lang racket/base
;; tests/gui/host-index-store-test.rkt — 项目描述库（design-memory.md §8）：扫描结果（集合没变不写）、要描述的（指纹 / 锁 / 失败）、
;; 用户编辑锁定、重新生成解锁、不见了的不列；重开一致；快照；编辑文字往返

(require rackunit racket/file racket/list "../../src/gui/compute/host-index-store.rkt")

(define dir (make-temporary-file "pi2-hixstore-~a" 'directory))
(define path (build-path dir "host-index.rktd"))
(define (R d fp [mt 1]) (hasheq 'dir d 'name (car (reverse (regexp-split #rx"/" d))) 'fingerprint fp 'remote 'null 'mtime mt))
(define D (hasheq 'short '("go" "Go语言") 'long '("Online shop" "在线商店") 'names '("shop") 'summary_en "A shop." 'summary_zh "商店。"))

(test-case "scan → stale list; describe; fingerprint change → stale again; failures wait for a new fingerprint; edits lock; regenerate unlocks"
  (define h (hix-open path))
  (hix-set-repos! h "local" (list (R "/w/shop" "a1" 20) (R "/w/pi2" "b1" 30)))
  (define size1 (file-size path))
  (hix-set-repos! h "local" (list (R "/w/pi2" "b1" 30) (R "/w/shop" "a1" 20)))       ; 同一个集合：不写
  (check-equal? (file-size path) size1)
  (check-equal? (map (lambda (e) (hash-ref e 'dir)) (hix-stale h)) '("/w/pi2" "/w/shop"))
  (hix-set-desc! h "local" "/w/shop" "a1" D "m")
  (hix-set-error! h "local" "/w/pi2" "b1" "timeout")
  (check-equal? (hix-stale h) '())
  (hix-set-repos! h "local" (list (R "/w/shop" "a2" 40) (R "/w/pi2" "b1" 30)))
  (check-equal? (map (lambda (e) (hash-ref e 'dir)) (hix-stale h)) '("/w/shop"))   ; 指纹变了；pi2 失败的还是那个指纹
  (hix-edit! h "local" "/w/shop" (hash-set D 'short '("mine")))
  (check-equal? (hix-stale h) '())                                                   ; 锁了
  (hix-set-desc! h "local" "/w/shop" "a2" D "m")                                     ; 锁着：模型的不覆盖
  (check-equal? (hash-ref (hash-ref (hix-entry h "local" "/w/shop") 'desc) 'short) '("mine"))
  (hix-regenerate! h "local" "/w/shop")
  (check-equal? (map (lambda (e) (hash-ref e 'dir)) (hix-stale h)) '("/w/shop"))
  ;; 不见了的：不列、不推
  (hix-set-repos! h "local" (list (R "/w/pi2" "b1" 30)))
  (check-equal? (map (lambda (e) (hash-ref e 'dir)) (hix-entries h)) '("/w/pi2"))
  (check-equal? (map (lambda (x) (hash-ref x 'desc)) (hix-view h "local")) '(null))
  ;; 另一台主机互不影响
  (hix-set-repos! h "srv" (list (R "/home/me/x" "c1" 5)))
  (check-equal? (map (lambda (e) (hash-ref e 'dir)) (hix-entries h "srv")) '("/home/me/x"))
  (check-equal? (length (hix-entries h)) 2)
  ;; 重开：一样
  (define h2 (hix-open path))
  (check-equal? (map (lambda (e) (list (hash-ref e 'host) (hash-ref e 'dir) (hash-ref e 'desc-fp) (hash-ref e 'locked))) (hix-entries h2))
                (map (lambda (e) (list (hash-ref e 'host) (hash-ref e 'dir) (hash-ref e 'desc-fp) (hash-ref e 'locked))) (hix-entries h)))
  (define s (hix-from-snapshot (hix-snapshot h2)))
  (check-equal? (length (hix-entries s)) 2))

(test-case "edit text round trip (commas incl. Chinese ones; long = one line each)"
  (define d (text->desc (desc->text D)))
  (check-equal? d D)
  (define d2 (text->desc "names: shop，商店\nshort: go、电商, web\nlong:\nfirst\n\nsecond\nsummary_en: x"))
  (check-equal? (list (hash-ref d2 'names) (hash-ref d2 'short) (hash-ref d2 'long) (hash-ref d2 'summary_en))
                '(("shop" "商店") ("go" "电商" "web") ("first" "second") "x")))

(delete-directory/files dir)
