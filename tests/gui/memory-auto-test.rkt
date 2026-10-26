#lang racket/base
;; tests/gui/memory-auto-test.rkt — 自动记忆（design-memory.md M3）的纯函数：哪些会话合格、提取 / 合并的提示与解析（容忍围栏、坏项、没回成 JSON）

(require rackunit racket/list racket/string "../../src/gui/compute/memory-auto.rkt")

(define NOW 100000)
(define (R p n mt #:eph [eph #f] #:write [w #t]) (hasheq 'path p 'messages n 'mtime mt 'ephemeral eph 'mem_write w))

(test-case "eligible: enough messages, idle long enough, not ephemeral, writing not off, grown since last time; most recent first"
  (define rows (list (R "/a" 8 (- NOW 9000)) (R "/b" 4 (- NOW 9000)) (R "/c" 10 (- NOW 100)) (R "/d" 12 (- NOW 8000) #:eph #t)
                     (R "/e" 12 (- NOW 8000) #:write #f) (R "/f" 20 (- NOW 7300)) (R "/g" 9 (- NOW 7500))))
  (define processed (hash "/f" 16 "/g" 2))
  (check-equal? (map (lambda (r) (hash-ref r 'path)) (eligible-sessions rows processed NOW 7200)) '("/g" "/a"))   ; f 只多了 4 条
  (check-equal? (map (lambda (r) (hash-ref r 'path)) (eligible-sessions rows (hash) NOW 0)) '("/c" "/f" "/g" "/a")))

(define CHAIN (list (hasheq 'id "global" 'kind "global" 'name "Global") (hasheq 'id "w1" 'kind "workspace" 'name "web @ local")))

(test-case "extract: prompt carries the scopes and existing memories; reply parsing tolerates fences, junk items, unknown scopes; at most 5"
  (define pr (extract-prompt "USER: always run make check" CHAIN (list (hasheq 'id "m1" 'kind "global" 'text "answer in Chinese"))))
  (check-true (string-contains? pr "- workspace: web @ local"))
  (check-true (string-contains? pr "[m1] (global) answer in Chinese"))
  (check-true (string-contains? pr "USER: always run make check"))
  (define c (parse-extract "```json\n{\"memories\": [{\"text\": \"run make check before pushing\", \"scope\": \"workspace\", \"evidence\": \"always run make check\"}, {\"text\": \"\"}, 7, {\"text\": \"x\", \"scope\": \"galaxy\"}]}\n```"))
  (check-equal? (map (lambda (x) (list (hash-ref x 'text) (hash-ref x 'scope))) c) '(("run make check before pushing" "workspace") ("x" "workspace")))
  (check-equal? (length (parse-extract (format "{\"memories\": [~a]}" (string-join (for/list ([i 9]) (format "{\"text\": \"f~a\"}" i)) ", ")))) 5)
  (check-equal? (parse-extract "nothing to remember") '()))

(test-case "merge: add / update (only ids that exist) / skip; one op per candidate; reply not JSON → add all"
  (define ops (parse-merge "{\"ops\": [{\"candidate\": 0, \"op\": \"add\"}, {\"candidate\": 1, \"op\": \"update\", \"id\": \"m3\", \"text\": \"new text\"}, {\"candidate\": 2, \"op\": \"update\", \"id\": \"m99\", \"text\": \"x\"}, {\"candidate\": 0, \"op\": \"skip\"}, {\"candidate\": 3, \"op\": \"skip\"}]}" 4 '("m3")))
  (check-equal? ops (list (hasheq 'op "add" 'candidate 0) (hasheq 'op "update" 'candidate 1 'id "m3" 'text "new text")))
  (check-equal? (parse-merge "sorry" 2 '()) (list (hasheq 'op "add" 'candidate 0) (hasheq 'op "add" 'candidate 1)))
  (check-true (string-contains? (merge-prompt (list (hasheq 'text "t" 'scope "global")) '()) "0. (global) t")))
