#lang racket/base
;; tests/gui/inspector-test.rkt — 信息面板组件（单个组件 + 假 text-layout）：内核 session_stats → 各节的行；✕ / 点外面关闭，点卡片上不关；英文界面不漏中文

(require rackunit racket/list racket/string
         "../../src/gui/compute/drawlist.rkt" "../../src/gui/compute/interfaces.rkt" "../../src/gui/compute/i18n.rkt"
         "../../src/gui/components/inspector.rkt" "fakes.rkt")

(define STATS
  (hasheq 'type "session_stats" 'path "/d/s.rktd" 'title "修 loader" 'created "2026-10-25T03:00:00" 'last_active "2026-10-25T03:10:00"
          'workdir "/w" 'model "m" 'bytes 20480 'index_bytes 640 'records 20 'messages 6 'user_messages 2 'compactions 1 'requests 3
          'models (list (hasheq 'model "deepseek-v4-flash" 'requests 3 'input 5000 'output 700 'cost 0.0021))
          'cost 0.0021
          'tools (list (hasheq 'name "bash" 'calls 2 'ms 120 'errors 1 'source "builtin") (hasheq 'name "echo" 'calls 1 'ms 2 'errors 0 'source "plugin:echo-tool"))
          'permissions (hasheq 'yes 2 'no 1) 'notes (hasheq 'route_switch 1) 'media (hasheq 'count 1 'bytes 2048)
          'subagents (list (hasheq 'call "p1" 'records 7)) 'forked_from 'null 'effects 0))

(test-case "sections carry the numbers; open / close by ✕ and by clicking outside; a click on the card does not close it"
  (define secs (sections STATS))
  (define flat (for*/list ([sec secs] [kv (cadr sec)]) (string-append (car kv) "=" (cdr kv))))
  (check-true (for/or ([s flat]) (regexp-match? #rx"^echo \\[plugin:echo-tool\\]=" s)))
  (check-true (for/or ([s flat]) (regexp-match? #rx"^deepseek-v4-flash=3 .* in 5000 / out 700 · \\$0.0021" s)))
  (check-true (for/or ([s flat]) (regexp-match? #rx"^bash=.*120 ms" s)))
  (define p (instantiate-with inspector-component inspector-view))
  (check-false (p 'open?))
  (p 'open! STATS)
  (p 'render! (make-drawlist) 240.0 0.0 720.0 680.0 2.0)
  (check-true (p 'open?))
  (define card (p 'hit-center 'panel))
  (check-false (p 'hit (car card) (+ (cdr card) 40.0)))                           ; 卡片上：不关
  (check-true (p 'open?))
  (define x (p 'hit-center 'close))
  (check-equal? (p 'hit (car x) (cdr x)) '(close))
  (check-false (p 'open?))
  (p 'open! STATS) (p 'render! (make-drawlist) 240.0 0.0 720.0 680.0 2.0)
  (check-equal? (p 'hit 250.0 5.0) '(close))                                        ; 卡片外
  (check-false (p 'open?)))

(test-case "English: no untranslated Chinese in the section and key labels"
  (parameterize ([current-lang 'en])
    (define keys (for*/list ([sec (sections STATS)] #:when #t [kv (cons (cons (car sec) "") (cadr sec))]) (car kv)))
    (check-false (for/or ([k keys]) (regexp-match? #px"[一-鿿]" k)) (format "~s" keys))))

(test-case "W2: the 'Belongs to' section lists workspace / super-workspace / projects / git remote given by the assembly; empty ones are left out"
  (define secs (sections (hash-set STATS 'groups (hasheq 'workspace "pi2 @ local" 'super "" 'projects "A, B" 'git_remote "git@github.com:me/pi2.git"))))
  (define g (assoc (t 'in.groups) secs))
  (check-equal? (map car (cadr g)) (list (t 'in.workspace) (t 'in.projects) (t 'in.git-remote)))
  (check-equal? (map cdr (cadr g)) '("pi2 @ local" "A, B" "git@github.com:me/pi2.git"))
  (check-false (assoc (t 'in.groups) (sections STATS))))                          ; 没有归属信息：整节不显示

(test-case "M1: the instructions section lists what the last turn injected (kind, source, bytes, cut / left out); not recorded yet → one line"
  (define secs (sections (hash-set* STATS 'instructions_max_bytes 32768
                                    'instructions (list (hasheq 'kind "global" 'label "/cfg/AGENTS.md" 'bytes 12 'truncated #f 'omitted #f)
                                                        (hasheq 'kind "project" 'label "Shop" 'bytes 40 'truncated #f 'omitted #f)
                                                        (hasheq 'kind "dir" 'label "/r/AGENTS.md" 'bytes 40000 'truncated #t 'omitted #f)
                                                        (hasheq 'kind "dir" 'label "/r/sub/AGENTS.md" 'bytes 9 'truncated #f 'omitted #t)))))
  (define sec (assoc (t 'in.instructions) secs))
  (check-equal? (map car (cadr sec)) (list (t 'in.instr-k-global) (t 'in.instr-k-project) (t 'in.instr-k-dir) (t 'in.instr-k-dir)))
  (check-equal? (cdr (car (cadr sec))) (string-append "/cfg/AGENTS.md · " (t 'in.instr-bytes "12")))
  (check-equal? (cdr (caddr (cadr sec))) (string-append "/r/AGENTS.md · " (t 'in.instr-truncated "40000" 32)))
  (check-equal? (cdr (cadddr (cadr sec))) (string-append "/r/sub/AGENTS.md · " (t 'in.instr-omitted "9" 32)))
  (check-equal? (cadr (assoc (t 'in.instructions) (sections STATS))) (list (cons "—" (t 'in.instr-none))))
  (check-equal? (cadr (assoc (t 'in.instructions) (sections (hash-set STATS 'instructions '())))) (list (cons "—" (t 'in.instr-empty)))))

(test-case "M2: the memory section — injected at start (count, bytes, each id with its text from the store) and looked up; not recorded yet → one line"
  (define secs (sections (hash-set* STATS 'memory (hasheq 'ids '("m2" "m1") 'bytes 120) 'memory_used '("m3")
                                    'memory-text (hash "m1" "reply briefly" "m2" "run make test" "m3" "prices in cents"))))
  (define sec (cadr (assoc (t 'in.memory) secs)))
  (check-equal? (map cdr sec) (list (t 'in.mem-n "2" "120") "[m2] run make test" "[m1] reply briefly" (t 'in.mem-used-n "1") "[m3] prices in cents"))
  (check-equal? (cadr (assoc (t 'in.memory) (sections STATS))) (list (cons (t 'in.mem-injected) (t 'in.mem-none)))))
