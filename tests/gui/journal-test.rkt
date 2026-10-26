#lang racket/base
;; tests/gui/journal-test.rkt — 操作日志组件、日志格式、回放源、调试面板（单个组件 + 假接口）

(require rackunit racket/file racket/list
         "../../src/gui/compute/hub.rkt" "../../src/gui/compute/interfaces.rkt" "../../src/gui/compute/event.rkt"
         "../../src/gui/compute/drawlist.rkt" "../../src/gui/compute/journal-format.rkt"
         "../../src/gui/components/journal.rkt" "../../src/gui/components/replay-host.rkt"
         "../../src/gui/components/debug-panel.rkt" "fakes.rkt")

(define (new-journal dir #:keep [keep 20])
  (env-ref (instantiate journal-component (hasheq) #:config (hasheq 'dir dir 'keep keep 'header (hasheq 'app "test"))) journal))

(test-case "records round-trip through the file: prefab window events, jsexpr kernel events, errors"
  (define dir (make-temporary-file "pi2-jr-~a" 'directory))
  (define jr (new-journal dir))
  (jr 'record! 'start (hasheq 'w 960.0 'h 680.0 'scale 2.0))
  (jr 'record! 'win (ev:mouse 'down 10.0 20.0 0 '(shift) 2))
  (jr 'record! 'win (ev:text "你好"))
  (jr 'record! 'core (hasheq 'type "delta" 'kind "text" 'text "a\nb"))
  (jr 'record! 'send (hasheq 'type "prompt" 'text "x"))
  (jr 'record! 'error (hasheq 'where "render" 'message "boom" 'context '("f a.rkt:1")))
  (jr 'close!)
  (define-values (head recs) (read-journal (jr 'path)))
  (check-equal? (hash-ref head 'app) "test")
  (check-equal? (map third recs) '(start win win core send error))
  (check-equal? (fourth (second recs)) (ev:mouse 'down 10.0 20.0 0 '(shift) 2))
  (check-equal? (fourth (third recs)) (ev:text "你好"))
  (check-equal? (hash-ref (fourth (fourth recs)) 'text) "a\nb")
  (check-equal? (map second recs) '(0 1 2 3 4 5))
  (check-equal? (hash-ref (car (jr 'errors)) 'message) "boom")
  (check-equal? (vector-ref (last (jr 'tail 3)) 1) 'error))

(test-case "a truncated last line (process killed mid-write) ends the stream instead of failing"
  (define dir (make-temporary-file "pi2-jr-~a" 'directory))
  (define jr (new-journal dir))
  (jr 'record! 'win (ev:text "a"))
  (jr 'close!)
  (with-output-to-file (jr 'path) #:exists 'append (lambda () (display "(12 1 win #s(ev:text \"unfin")))
  (define-values (_h recs) (read-journal (jr 'path)))
  (check-equal? (length recs) 1))

(test-case "rotation keeps the newest `keep` journals; memory-only mode has no path but still keeps a tail"
  (define dir (make-temporary-file "pi2-jr-~a" 'directory))
  (for ([i 5])
    (define jr (new-journal dir #:keep 3))
    (jr 'record! 'note (format "run ~a" i))
    (jr 'close!)
    (sleep 1.05))                                                      ; 按修改时间排序：拉开秒级时间戳
  (check-equal? (length (directory-list dir)) 3)
  (define mem (new-journal #f))
  (check-false (mem 'path))
  (mem 'record! 'note "x")
  (check-equal? (vector-ref (car (mem 'tail 5)) 2) "x"))

(test-case "replay host: kernel and window records come out in recorded order; sends are compared; ends with close"
  (define recs (list (list 0 0 'start (hasheq 'w 800.0 'h 600.0 'scale 2.0))
                     (list 1 1 'core (hasheq 'type "ready"))
                     (list 2 2 'send (hasheq 'type "list_sessions"))
                     (list 3 3 'win (ev:text "a"))
                     (list 4 4 'win (ev:text "b"))
                     (list 5 5 'core (hasheq 'type "turn_start"))
                     (list 6 6 'win (ev:text "c"))))
  (define env (instantiate replay-host-component (hasheq) #:config (hasheq 'records recs 'header (hasheq 'w 800.0 'h 600.0 'scale 2.0))))
  (define win (env-ref env window-port))
  (define ks (env-ref env kernel-set))
  (define rs (env-ref env replay-source))
  (define-values (w h sc) (win 'size))
  (check-equal? (list w h sc) '(800.0 600.0 2.0))
  (check-equal? (ks 'poll!) (list (cons "local" (hasheq 'type "ready"))))
  (check-equal? (win 'pump!) (list (ev:text "a") (ev:text "b")))           ; send 记录被跳过
  (check-equal? (win 'pump!) '())                                          ; 队首是内核事件：窗口这轮没有
  (check-equal? (ks 'poll!) (list (cons "local" (hasheq 'type "turn_start"))))
  (check-equal? (win 'pump!) (list (ev:text "c")))
  (check-equal? (win 'pump!) (list (ev:close)))
  (ks 'send! "local" (hasheq 'type "list_sessions"))
  (check-equal? (rs 'sent) (rs 'expected))
  (check-equal? (rs 'progress) '(7 . 7)))

(test-case "replay host: window events never cross a recorded frame (hit areas update on render)"
  (define recs (list (list 0 0 'win (ev:mouse 'move 1.0 1.0 0 '() 0))
                     (list 1 1 'frame 7)
                     (list 2 2 'core (hasheq 'type "x"))
                     (list 3 3 'win (ev:mouse 'down 1.0 1.0 0 '() 1))
                     (list 4 4 'win (ev:mouse 'up 1.0 1.0 0 '() 1))))
  (define env (instantiate replay-host-component (hasheq) #:config (hasheq 'records recs)))
  (define win (env-ref env window-port))
  (define ks (env-ref env kernel-set))
  (check-equal? (ks 'poll!) '())
  (check-equal? (map ev:mouse-kind (win 'pump!)) '(move))                 ; 停在重绘处
  (check-equal? (ks 'poll!) (list (cons "local" (hasheq 'type "x"))))
  (check-equal? (map ev:mouse-kind (win 'pump!)) '(down up)))

(test-case "debug panel: closed draws nothing; open shows errors and recent records"
  (define jr (new-journal #f))
  (jr 'record! 'error (hasheq 'where "render" 'message "boom" 'context '("f a.rkt:1" "g b.rkt:2")))
  (for ([i 30]) (jr 'record! 'win (ev:text (format "k~a" i))))
  (define env (instantiate debug-panel-component (hasheq 'text-layout (make-fake-text-layout) 'journal jr)))
  (define dv (env-ref env debug-view))
  (define dl (make-drawlist))
  (dv 'render! dl 0.0 0.0 600.0 400.0 2.0 '("stats"))
  (check-equal? (drawlist-count dl) 0)
  (dv 'toggle!)
  (dv 'render! dl 0.0 0.0 600.0 400.0 2.0 '("stats"))
  (check-true (dv 'open?))
  (check-true (> (drawlist-count dl) 100)))

(test-case "replay host, two hosts: '_host tags route events to their host and sends are compared with the tag"
  (define recs (list (list 0 0 'core (hasheq 'type "ready"))
                     (list 1 1 'core (hasheq 'type "ready" '_host "srv"))
                     (list 2 2 'send (hasheq 'type "prompt" 'text "x" '_host "srv"))))
  (define env (instantiate replay-host-component (hasheq) #:config (hasheq 'records recs)))
  (define ks (env-ref env kernel-set))
  (define rs (env-ref env replay-source))
  (check-equal? (ks 'poll!) (list (cons "local" (hasheq 'type "ready")) (cons "srv" (hasheq 'type "ready"))))
  (ks 'send! "srv" (hasheq 'type "prompt" 'text "x"))
  (check-equal? (rs 'sent) (rs 'expected)))

(test-case "redact!: the original text is replaced in the journal file and the in-memory entries; appending continues"
  (define dir (make-temporary-file "pi2-jr-redact-~a" 'directory))
  (define jr (new-journal dir))
  (jr 'record! 'win (ev:paste "my token xyz-SECRET-123 here"))
  (jr 'record! 'send (hasheq 'type "prompt" 'text "use xyz-SECRET-123" 'nested (list "a" "xyz-SECRET-123")))
  (jr 'redact! "xyz-SECRET-123" "⟦secret:abcd0123⟧")
  (jr 'record! 'win (ev:text "after"))
  (jr 'close!)
  (define text (file->string (jr 'path)))
  (check-false (regexp-match? #rx"xyz-SECRET-123" text))
  (check-equal? (length (regexp-match* #rx"⟦secret:abcd0123⟧" text)) 3)
  (check-regexp-match #rx"after" text)
  (define-values (_h recs) (read-journal (jr 'path)))
  (check-equal? (length recs) 3)                                          ; 记录都还在、能读
  (check-false (ormap (lambda (v) (regexp-match? #rx"xyz-SECRET" (format "~a" v))) (jr 'tail 10)))
  (delete-directory/files dir))

(test-case "attachments in the journal: no bytes, only sha and size; prompt media keep kind / type / name / sha"
  (define items (list (vector 'image "image/png" "a.png" #"PNGDATA" 7 #f)))
  (define j (attach-for-journal items))
  (check-equal? j (list (vector 'image "image/png" "a.png" #f 7 (sha256-hex #"PNGDATA"))))
  (check-equal? (redact-send (hasheq 'type "prompt" 'text "x" 'media (list (hasheq 'kind "image" 'media_type "image/png" 'name "a.png" 'sha "s" 'data "QUJD" 'meta (hasheq 'width 1)))))
                (hasheq 'type "prompt" 'text "x" 'media (list (hasheq 'kind "image" 'media_type "image/png" 'name "a.png" 'sha "s"))))
  (check-equal? (redact-send (hasheq 'type "state")) (hasheq 'type "state")))
