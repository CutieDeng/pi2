#lang racket-tstring
;; session-test.rkt — 会话持久化与流式重放单测

(require
 rackunit
 racket/file
 racket/pvector
 racket/string
 (file "../model.rkt")
 (file "../session.rkt")
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

(test-case "session-list reads only meta"
  (define d (session-list tmpdir))
  (check-true (>= (length d) 3))
  (for ([sm (in-list d)])
    (check-true (session-meta? sm))
  ) ; end for
) ; end test-case

(delete-directory/files tmpdir)
(displayln "session-test: all passed")
