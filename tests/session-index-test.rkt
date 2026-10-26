#lang tstring racket
;; session-index-test.rkt — 会话记录 P1（design-content.md §4.1）：<会话>.rktd.idx 索引的建立 / 补齐 / 重建，按偏移随机读；
;; 快速重放（从最后一次压缩开始）= 全量重放；会话列表信息经索引读 = 扫描整个文件。

(require rackunit racket/file racket/pvector
         (file "../src/model.rkt") (file "../src/rktd.rkt") (file "../src/session.rkt") (file "../src/session-index.rkt"))

(define dir (make-temporary-file "pi2-sidx-~a" 'directory))
(define cfg (struct-copy config (default-config) [workdir (path->string dir)]))
(define (u n) (text-msg 'user n))
(define (a n) (text-msg 'assistant n))
(define (texts st) (for/list ([m (in-pvector (agent-state-history st))]) (message-text m)))
(define (full path) (session-replay path #:stop-after 1000000000))     ; #:stop-after → 全量路径

(test-case "the index is written alongside the log; entries locate records; types / roles / text lengths are right"
  (define p (fresh-session-path (path->string dir)))
  (define s (session-open! p cfg))
  (session-append-msg! s (u "hello there"))
  (session-append-msg! s (message 'assistant (list (thinking-block "hmm" "sig") (text-block "answer\nline 2") (tool-use-block "c1" "bash" (hasheq 'command "ls")))))
  (session-append-msg! s (message 'user (list (tool-result-block "c1" "out" #t))))
  (session-append-rec! s 'note (hasheq 'level "warn" 'code "route_switch" 'text "switched"))
  (session-append-turn! s 'begin)
  (session-close! s)
  (check-true (file-exists? (index-path p)))
  (define ix (index-open p))
  (check-equal? (index-count ix) 6)                               ; meta + 3 msg + note + turn
  (check-equal? (map (lambda (i) (type-name (ientry-type (index-ref ix i)))) (range 6)) '(meta msg msg msg note turn))
  (define e2 (index-ref ix 2))
  (check-equal? (ientry-role e2) ROLE-ASSISTANT)
  (check-equal? (bitwise-and (ientry-flags e2) (bitwise-ior FLAG-THINKING FLAG-TOOL-USE)) (bitwise-ior FLAG-THINKING FLAG-TOOL-USE))
  (check-equal? (ientry-lines e2) 2)
  (check-equal? (ientry-role (index-ref ix 3)) ROLE-RESULTS)
  (check-equal? (ientry-flags (index-ref ix 3)) FLAG-TOOL-ERROR)
  (check-equal? (ientry-role (index-ref ix 5)) 1)                 ; turn begin
  (define rs (index-read ix '(4 1)))
  (check-equal? (hash-ref (rec-data (cdr (car rs))) 'code) "route_switch")
  (check-equal? (message-text (rec-data (cdr (cadr rs)))) "hello there")
  (check-equal? (index-covered ix) (file-size p)))

(test-case "old files without an index: read-only open builds it in memory (no file); writable open writes it; appends by old code are picked up; a replaced log is re-indexed"
  (define p (path->string (build-path dir "old.rktd")))
  (define lg (datum-log-open! p))
  (datum-log-append! lg (rec 'meta "2026-01-01T00:00:00" (list 'pi2 1 cfg)))
  (for ([i 5]) (datum-log-append! lg (rec 'msg "2026-01-01T00:00:01" (u (format "m~a" i)))))
  (datum-log-close! lg)
  (define ix0 (index-open p))
  (check-equal? (index-count ix0) 6)
  (check-false (file-exists? (index-path p)))                     ; 只读打开不写索引文件
  (define s (session-open! p cfg))                                 ; 写会话：建索引文件
  (session-append-msg! s (u "new"))
  (session-close! s)
  (check-true (file-exists? (index-path p)))
  (check-equal? (index-count (index-open p)) 7)
  ;; 旧代码在后面追加（不更新索引）→ 下次打开补上
  (define lg2 (datum-log-open! p))
  (datum-log-append! lg2 (rec 'msg "2026-01-01T00:00:02" (u "by-old-code")))
  (datum-log-close! lg2)
  (define ix2 (index-open p))
  (check-equal? (index-count ix2) 8)
  (check-equal? (message-text (rec-data (cdr (car (index-read ix2 '(7)))))) "by-old-code")
  ;; 文件被替换成更短的 → 索引覆盖超过文件长度 → 重建
  (define lg3 (begin (delete-file p) (datum-log-open! p)))
  (datum-log-append! lg3 (rec 'meta "2026-01-01T00:00:00" (list 'pi2 1 cfg)))
  (datum-log-close! lg3)
  (check-equal? (index-count (index-open p)) 1))

(test-case "fast replay (from the last compaction) = full replay; a reverted compaction falls back to the full path"
  (define p (fresh-session-path (path->string dir)))
  (define s (session-open! p cfg))
  (for ([i 30]) (session-append-msg! s (if (even? i) (u (format "u~a" i)) (a (format "a~a" i)))))
  (session-append-usage! s (usage 100 50))
  (session-append-compact! s (hasheq 'history (list (u "[compacted context summary]\nS1") (a "a29")) 'usage (usage 100 50)))
  (session-append-turn! s 'begin)
  (session-append-msg! s (u "q1")) (session-append-msg! s (a "r1"))
  (session-append-turn! s 'end)
  (session-append-usage! s (usage 10 5))
  (session-append-turn! s 'begin)
  (session-append-msg! s (u "boom"))
  (session-append-turn! s 'revert)
  (session-close! s)
  (define fast (session-replay p))
  (define slow (full p))
  (check-equal? (texts fast) '("[compacted context summary]\nS1" "a29" "q1" "r1"))
  (check-equal? (texts fast) (texts slow))
  (check-equal? (agent-state-token-usage fast) (agent-state-token-usage slow))
  (check-equal? (agent-state-token-usage fast) (usage 110 55))
  ;; 压缩发生在一轮里、那轮出错撤回 → 压缩被撤回：快速路径不能从压缩点开始，结果仍要与全量一致
  (define p2 (fresh-session-path (path->string dir)))
  (define s2 (session-open! p2 cfg))
  (for ([i 6]) (session-append-msg! s2 (u (format "x~a" i))))
  (session-append-turn! s2 'begin)
  (session-append-msg! s2 (u "y"))
  (session-append-compact! s2 (hasheq 'history (list (u "SUM") (u "y")) 'usage usage-zero))
  (session-append-turn! s2 'revert)
  (session-close! s2)
  (check-equal? (texts (session-replay p2)) '("x0" "x1" "x2" "x3" "x4" "x5"))
  (check-equal? (texts (session-replay p2)) (texts (full p2))))

(test-case "session list info via the index = scanning the file; records-of-type reads only that type"
  (define p (fresh-session-path (path->string dir)))
  (define s (session-open! p cfg))
  (session-append-msg! s (u "first question about the loader"))
  (session-append-msg! s (a "answer"))
  (session-append-route! s "@pool")
  (session-append-diff! s "c9" (hasheq 'path "x" 'added 1 'removed 0 'text "+a"))
  (session-close! s)
  (define info (read-session-info p))
  (check-equal? (session-info-title info) "first question about the loader")
  (check-equal? (session-info-nmsg info) 2)
  (check-equal? (session-route p) "@pool")
  (check-equal? (hash-ref (session-diffs p) "c9") (hasheq 'path "x" 'added 1 'removed 0 'text "+a"))
  (check-equal? (length (records-of-type p 'msg)) 2))

(test-case "a record carrying an unreadable value (e.g. #<void>) is written in a readable form; later records stay readable"
  (define p (path->string (build-path dir "bad.rktd")))
  (define lg (datum-log-open! p))
  (datum-log-append! lg (rec 'note "t" (hasheq 'text "x" 'oops (void))))
  (datum-log-append! lg (rec 'msg "t" (u "after")))
  (datum-log-close! lg)
  (define ds (for/list ([d (in-datum-log p)]) d))
  (check-equal? (length ds) 2)
  (check-equal? (hash-ref (rec-data (car ds)) 'oops) "#<void>")
  (check-equal? (message-text (rec-data (cadr ds))) "after"))

(delete-directory/files dir)
