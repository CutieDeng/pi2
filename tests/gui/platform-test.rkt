#lang racket/base
;; platform-test.rkt — GUI 元数据平面 2：能力记录与平台请求（compute/platform.rkt）。纯数据，不开窗口。

(require rackunit racket/list "../../src/gui/compute/platform.rkt")

(test-case "capability vocabulary: every entry says what it means and what to do without it"
  (for ([c (in-list CAPABILITIES)])
    (check-equal? (length c) 4 (format "~s" (car c)))
    (check-not-false (memq (cadr c) '(boolean symbol integer)))
    (check-true (and (string? (caddr c)) (string? (cadddr c)))))
  (check-equal? (length CAPABILITY-NAMES) (length (remove-duplicates CAPABILITY-NAMES))))

(test-case "capability records: shapes checked; the macOS and replay records used by the window components are valid"
  (check-equal? (caps-problems (hasheq 'glass #t 'transparent-window #t 'ime 'marked-text 'capture 'self 'native-input-sim #t
                                       'precise-scroll #t 'pinch #f 'mouse-buttons 2))
                '())
  (check-equal? (caps-problems (hasheq 'glass #f 'transparent-window #f 'ime 'none 'capture 'none 'native-input-sim #f
                                       'precise-scroll #t 'pinch #f 'mouse-buttons 2))
                '())
  (check-equal? (length (caps-problems (hasheq 'glass "yes" 'teleport #t))) 2)
  (check-equal? (cap (hasheq 'glass #t) 'glass) #t)
  (check-equal? (cap (hasheq) 'pinch) #f "missing = not available"))

(test-case "platform state: only changed keys become requests, in a fixed order; applying them updates the state"
  (check-equal? (state-diff STATE-DEFAULTS (hasheq 'text-input #t 'cursor 'arrow)) '() "unchanged → nothing")
  (define reqs (state-diff STATE-DEFAULTS (hasheq 'cursor 'resize-h 'material 'transparent 'text-input #t)))
  (check-equal? reqs (list (req 'material 'transparent) (req 'cursor 'resize-h)))
  (define s2 (state-apply STATE-DEFAULTS reqs))
  (check-equal? (state-diff s2 (hasheq 'material 'transparent 'cursor 'resize-h)) '())
  (check-equal? (state-diff s2 (hasheq 'material 'opaque)) (list (req 'material 'opaque)))
  ;; 哨兵：初值不同于任何值 → 第一次一定发（#f 也发）
  (check-equal? (state-diff (hash-set STATE-DEFAULTS 'glass-rect 'unset) (hasheq 'glass-rect #f)) (list (req 'glass-rect #f)))
  (check-exn #rx"unknown platform state key" (lambda () (state-diff STATE-DEFAULTS (hasheq 'wallpaper 'blue)))))

(test-case "every state key has a default and a description"
  (for ([k (in-list STATE-KEYS)])
    (check-equal? (length k) 3)
    (check-true (string? (caddr k)))
    (check-true (hash-has-key? STATE-DEFAULTS (car k)))))
