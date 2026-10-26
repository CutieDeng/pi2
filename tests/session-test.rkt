#lang tstring racket
;; session-test.rkt — 会话持久化与流式重放单测

(require
 rackunit
 racket/file
 racket/list
 racket/pvector
 racket/string
 (file "../src/model.rkt")
 (file "../src/session.rkt")
) ; end require

(define tmpdir (make-temporary-file "pi2-sesstest-~a" 'directory))

(test-case "persist and replay reconstructs state via same transitions"
  (define path (build-path tmpdir "s1.rktd"))
  (define cfg
    (struct-copy config (default-config) [model "test-model"])
  ) ; end define cfg
  (define s (session-open! path cfg))
  (session-append-msg! s (text-msg 'user "你好 agent"))
  (session-append-msg! s (message 'assistant
                                  (list (tool-use-block "c1" "bash" (hasheq 'command "ls")))))
  (session-append-usage! s (usage 10 5))
  (session-append-msg! s (message 'user
                                  (list (tool-result-block "c1" "file1\nfile2" #f))))
  (session-append-msg! s (text-msg 'assistant "found 2 files"))
  (session-append-usage! s (usage 20 8))
  (session-close! s)
  ;; 重放
  (define st (session-replay path))
  (check-equal? (config-model (agent-state-config st)) "test-model")
  (check-equal? (pvector-length (agent-state-history st)) 4)
  (check-equal? (message-text (pvector-ref (agent-state-history st) 0)) "你好 agent")
  (check-equal? (message-text (pvector-ref (agent-state-history st) 3)) "found 2 files")
  ;; usage 累计
  (check-equal? (agent-state-token-usage st) (usage 30 13))
  ;; tool-use-block 无损往返
  (define tu (car (message-blocks (pvector-ref (agent-state-history st) 1))))
  (check-true (tool-use-block? tu))
  (check-equal? (tool-use-block-input tu) (hasheq 'command "ls"))
) ; end test-case

(test-case "replay stop-after for fork/resume-to-point"
  (define path (build-path tmpdir "s2.rktd"))
  (define s (session-open! path (default-config)))
  (for ([i (in-range 5)])
    (session-append-msg! s (text-msg 'user f"msg {i}"))
  ) ; end for
  (session-close! s)
  (define st (session-replay path #:stop-after 3))
  (check-equal? (pvector-length (agent-state-history st)) 3)
  (check-equal? (message-text (pvector-ref (agent-state-history st) 2)) "msg 2")
) ; end test-case

(test-case "human-readable + read-back: file is valid datum stream"
  (define path (build-path tmpdir "s3.rktd"))
  (define s (session-open! path (default-config)))
  (session-append-msg! s (text-msg 'assistant "hi"))
  (session-close! s)
  (define content (file->string path))
  ;; pretty-write 多行；且能被 read 逐个读回
  (check-true (string-contains? content "#s(rec"))
  (define datums
    (call-with-input-file path
      (lambda (in)
        (let loop ([acc '()])
          (define d (read in))
          (if (eof-object? d) (reverse acc) (loop (cons d acc)))
        ) ; end let loop
      ) ; end lambda
    ) ; end call-with-input-file
  ) ; end define datums
  (check-equal? (length datums) 2)           ; meta + msg
  (check-equal? (rec-type (car datums)) 'meta)
) ; end test-case

(test-case "session-infos: rich metadata + auto-title from first user message"
  (define infos (session-infos tmpdir))
  (check-true (>= (length infos) 3))
  (for ([i (in-list infos)]) (check-true (session-info? i)))
  (define s1 (findf (lambda (i) (string-suffix? (session-info-path i) "s1.rktd")) infos))
  (check-true (session-info? s1))
  (check-equal? (session-info-title s1) "你好 agent")   ; 首条 user 消息
  (check-equal? (session-info-model s1) "test-model")
  (check-equal? (session-info-nmsg s1) 4)
  ;; 排序：最近修改在前
  (check-equal? (session-info-mtime (first infos))
                (apply max (map session-info-mtime infos)))
) ; end test-case

(test-case "session-info->line renders a single line with the title"
  (define s1 (findf (lambda (i) (string-suffix? (session-info-path i) "s1.rktd"))
                    (session-infos tmpdir)))
  (check-true (string-contains? (session-info->line s1) "你好 agent"))
) ; end test-case

(test-case "session-latest returns the most recently written session"
  (define p (build-path tmpdir "latest.rktd"))
  (define s (session-open! p (default-config)))
  (session-append-msg! s (text-msg 'user "newest"))
  (session-close! s)
  (check-equal? (session-latest tmpdir) (path->string p))
) ; end test-case

(test-case "empty fresh session is auto-pruned on close"
  (define p (build-path tmpdir "empty.rktd"))
  (define s (session-open! p (default-config)))       ; 只有 meta，无消息
  (session-close! s)
  (check-false (file-exists? p))                       ; 自动清理
) ; end test-case

(test-case "session-delete! removes a session file"
  (define p (build-path tmpdir "todelete.rktd"))
  (define s (session-open! p (default-config)))
  (session-append-msg! s (text-msg 'user "x"))
  (session-close! s)
  (check-true (file-exists? p))
  (session-delete! p)
  (check-false (file-exists? p))
) ; end test-case

(test-case "session-fork! branches at message N into a new file"
  (define src (build-path tmpdir "s1.rktd"))          ; 4 条消息
  (define new-path (session-fork! src tmpdir #:at 2))
  (check-true (file-exists? new-path))
  (define st (session-replay new-path))
  (check-equal? (pvector-length (agent-state-history st)) 2)
  (check-equal? (message-text (pvector-ref (agent-state-history st) 0)) "你好 agent")
  (check-equal? (session-info-title (read-session-info new-path)) "⑂ 你好 agent")   ; 分叉：标题带标记
  (check-equal? (session-info-title (read-session-info src)) "你好 agent")
) ; end test-case

(test-case "session files never contain the api key (meta config is written with api-key #f)"
  (define p (build-path tmpdir "k1.rktd"))
  (define s (session-open! p (struct-copy config (default-config) [api-key "sk-secret-123"])))
  (session-append-msg! s (text-msg 'user "hi"))
  (session-close! s)
  (check-false (regexp-match? #rx"sk-secret" (file->string p)))
  (check-false (config-api-key (agent-state-config (session-replay p))))
) ; end test-case

(test-case "session-scrub-key! removes a key stored by older versions and keeps every other record"
  (define p (build-path tmpdir "old-keyed.rktd"))
  (with-output-to-file p
    (lambda ()
      (write (rec 'meta "2026-08-01T00:00:00" (list 'pi2 1 (struct-copy config (default-config) [api-key "sk-old-999"])))) (newline)
      (write (rec 'msg "2026-08-01T00:00:01" (text-msg 'user "hello"))) (newline)))
  (check-true (session-scrub-key! p))
  (check-false (regexp-match? #rx"sk-old" (file->string p)))
  (check-equal? (message-text (pvector-ref (agent-state-history (session-replay p)) 0)) "hello")
  (check-false (session-scrub-key! p))                                          ; 再跑：无事可做
) ; end test-case

(test-case "diff records: kept out of replay, read back by id, copied by fork only up to the fork point"
  (define p (build-path tmpdir "d1.rktd"))
  (define s (session-open! p (default-config)))
  (session-append-msg! s (message 'assistant (list (tool-use-block "t1" "write_file" (hasheq 'path "a")))))
  (session-append-msg! s (message 'user (list (tool-result-block "t1" "ok" #f))))
  (session-append-diff! s "t1" (hasheq 'path "a" 'added 1 'removed 0 'text "@@ -0,0 +1,1 @@\n+x" 'truncated #f))
  (session-append-msg! s (message 'assistant (list (tool-use-block "t2" "write_file" (hasheq 'path "b")))))
  (session-append-msg! s (message 'user (list (tool-result-block "t2" "ok" #f))))
  (session-append-diff! s "t2" (hasheq 'path "b" 'added 2 'removed 0 'text "@@ -0,0 +1,2 @@\n+y\n+z" 'truncated #f))
  (session-close! s)
  (check-equal? (pvector-length (agent-state-history (session-replay p))) 4)      ; diff 不进历史
  (check-equal? (sort (hash-keys (session-diffs p)) string<?) '("t1" "t2"))
  (check-equal? (hash-ref (hash-ref (session-diffs p) "t2") 'added) 2)
  (define f (session-fork! p tmpdir #:at 2))
  (check-equal? (hash-keys (session-diffs f)) '("t1"))
) ; end test-case

(delete-directory/files tmpdir)
(displayln "session-test: all passed")
