#lang tstring racket
;; tool-test.rkt — 工具层与权限单测（M2 前半）

(require
 rackunit
 racket/string
 racket/file
 racket/list
 (file "../src/model.rkt")
 (file "../src/tool.rkt")
 (file "../src/permission.rkt")
 (file "../src/tools/builtin.rkt")
 (file "../src/tools/bash.rkt")
) ; end require

(define tmpdir (make-temporary-file "pi2-tooltest-~a" 'directory))
(define ctx (tool-ctx tmpdir void (default-config)))
(define reg (make-registry (builtin-tools (default-config))))

(define (run name input)
  (tool-run (registry-lookup reg name) input ctx)
) ; end define run

;; ---------------------------------------------------------------- registry

(test-case "registry lookup and specs"
  (check-true (tool? (registry-lookup reg "bash")))
  (check-true (tool? (registry-lookup reg "git")))
  (check-false (registry-lookup reg "nope"))
  (check-equal? (length (registry-specs reg)) 7)
  (for ([spec (in-list (registry-specs reg))])
    (check-equal? (hash-ref spec 'type) "function")
  ) ; end for
) ; end test-case

;; ---------------------------------------------------------------- bash

(test-case "bash: basic command, exit code, timeout"
  (define r (run "bash" (hasheq 'command "echo hello && echo world")))
  (check-false (tool-outcome-is-error? r))
  (check-equal? (tool-outcome-content r) "hello\nworld\n")
  ;; 非零退出码
  (define r2 (run "bash" (hasheq 'command "exit 3")))
  (check-true (tool-outcome-is-error? r2))
  (check-true (string-contains? (tool-outcome-content r2) "exit code 3"))
  ;; 超时
  (define fast-bash (make-bash-tool #:timeout-secs 1))
  (define r3 (tool-run fast-bash (hasheq 'command "sleep 10") ctx))
  (check-true (tool-outcome-is-error? r3))
  (check-true (string-contains? (tool-outcome-content r3) "timed out"))
) ; end test-case

(test-case "bash output truncation"
  (define s (truncate-output (make-string 100000 #\x)))
  (check-true (< (string-length s) 40000))
  (check-true (string-contains? s "[truncated"))
) ; end test-case

;; ---------------------------------------------------------------- files

(test-case "write_file / read_file / edit_file roundtrip"
  (define r1 (run "write_file" (hasheq 'path "a/b.txt" 'content "line1\nline2\nline3")))
  (check-false (tool-outcome-is-error? r1))
  (define r2 (run "read_file" (hasheq 'path "a/b.txt")))
  (check-false (tool-outcome-is-error? r2))
  (check-true (string-contains? (tool-outcome-content r2) "1\tline1"))
  (check-true (string-contains? (tool-outcome-content r2) "3 lines total"))
  ;; offset/limit
  (define r3 (run "read_file" (hasheq 'path "a/b.txt" 'offset 2 'limit 1)))
  (check-true (string-contains? (tool-outcome-content r3) "2\tline2"))
  (check-false (string-contains? (tool-outcome-content r3) "line3"))
  ;; edit: 唯一替换
  (define r4 (run "edit_file" (hasheq 'path "a/b.txt"
                                      'old_string "line2"
                                      'new_string "LINE-TWO")))
  (check-false (tool-outcome-is-error? r4))
  (check-true (string-contains? (file->string (build-path tmpdir "a/b.txt")) "LINE-TWO"))
  ;; edit: 不存在
  (define r5 (run "edit_file" (hasheq 'path "a/b.txt"
                                      'old_string "nope" 'new_string "x")))
  (check-true (tool-outcome-is-error? r5))
  ;; edit: 多次出现（未开 replace_all）→ 报错，提示 replace_all
  (run "write_file" (hasheq 'path "dup.txt" 'content "aa aa"))
  (define r6 (run "edit_file" (hasheq 'path "dup.txt"
                                      'old_string "aa" 'new_string "b")))
  (check-true (tool-outcome-is-error? r6))
  (check-true (string-contains? (tool-outcome-content r6) "replace_all"))
) ; end test-case

(test-case "edit_file: replace_all 替换全部出现"
  (run "write_file" (hasheq 'path "ra.txt" 'content "x x x y"))
  (define r (run "edit_file" (hasheq 'path "ra.txt"
                                     'old_string "x" 'new_string "Z" 'replace_all #t)))
  (check-false (tool-outcome-is-error? r))
  (check-equal? (file->string (build-path tmpdir "ra.txt")) "Z Z Z y")
  (check-true (string-contains? (tool-outcome-content r) "3 replacement"))
) ; end test-case

(test-case "edit_file: 批量 edits 按序原子应用"
  (run "write_file" (hasheq 'path "m.txt" 'content "alpha beta gamma"))
  (define r (run "edit_file"
                 (hasheq 'path "m.txt"
                         'edits (list (hasheq 'old_string "alpha" 'new_string "A")
                                      (hasheq 'old_string "gamma" 'new_string "G")))))
  (check-false (tool-outcome-is-error? r))
  (check-equal? (file->string (build-path tmpdir "m.txt")) "A beta G")
) ; end test-case

(test-case "edit_file: 批量任一失败则整体不落盘（原子性）"
  (run "write_file" (hasheq 'path "atomic.txt" 'content "keep this"))
  (define r (run "edit_file"
                 (hasheq 'path "atomic.txt"
                         'edits (list (hasheq 'old_string "keep" 'new_string "KEEP")
                                      (hasheq 'old_string "MISSING" 'new_string "x")))))
  (check-true (tool-outcome-is-error? r))
  (check-true (string-contains? (tool-outcome-content r) "edit #2"))
  (check-equal? (file->string (build-path tmpdir "atomic.txt")) "keep this")  ; 未改
) ; end test-case

(test-case "edit_file: 缺 old_string 与 edits → 参数错误"
  (run "write_file" (hasheq 'path "n.txt" 'content "hi"))
  (define r (run "edit_file" (hasheq 'path "n.txt" 'new_string "x")))
  (check-true (tool-outcome-is-error? r))
) ; end test-case

(test-case "git: init/add/commit/status/log/diff（真机 git，离线）"
  (define grepo (build-path tmpdir "grepo"))
  (make-directory* grepo)
  (define gctx (tool-ctx grepo void (default-config)))
  (define (git . args) (tool-run (registry-lookup reg "git") (hasheq 'args args) gctx))
  (void (git "init"))
  (void (git "config" "user.email" "t@example.com"))
  (void (git "config" "user.name" "pi2 test"))
  (call-with-output-file (build-path grepo "f.txt") (lambda (o) (write-string "hello" o)))
  (define r-status (git "status" "--short"))
  (check-false (tool-outcome-is-error? r-status))
  (check-true (string-contains? (tool-outcome-content r-status) "f.txt"))
  (void (git "add" "-A"))
  ;; commit message 含空格：argv 直传无需转义
  (define r-commit (git "commit" "-m" "first commit with spaces"))
  (check-false (tool-outcome-is-error? r-commit))
  (define r-log (git "log" "--oneline"))
  (check-true (string-contains? (tool-outcome-content r-log) "first commit with spaces"))
  ;; 非法子命令 → git 非零退出 → 工具错误（携带 git 报错）
  (define r-bad (git "not-a-real-subcommand"))
  (check-true (tool-outcome-is-error? r-bad))
  ;; 空 args → 参数错误
  (check-true (tool-outcome-is-error? (tool-run (registry-lookup reg "git") (hasheq 'args '()) gctx)))
) ; end test-case

(test-case "read_file: missing file is a tool error, not an exception"
  (define r (run "read_file" (hasheq 'path "no/such.txt")))
  (check-true (tool-outcome-is-error? r))
) ; end test-case

;; ---------------------------------------------------------------- search

(test-case "glob and grep"
  (run "write_file" (hasheq 'path "src/x.rkt" 'content "(define foo 1)\n(define bar 2)"))
  (run "write_file" (hasheq 'path "src/y.txt" 'content "nothing here"))
  (define g (run "glob" (hasheq 'pattern "**/*.rkt")))
  (check-false (tool-outcome-is-error? g))
  (check-true (string-contains? (tool-outcome-content g) "x.rkt"))
  (check-false (string-contains? (tool-outcome-content g) "y.txt"))
  (define gr (run "grep" (hasheq 'pattern "define (f|b)" 'glob "**/*.rkt")))
  (check-false (tool-outcome-is-error? gr))
  (check-true (string-contains? (tool-outcome-content gr) "x.rkt:1:"))
  (check-true (string-contains? (tool-outcome-content gr) "x.rkt:2:"))
  (define gr2 (run "grep" (hasheq 'pattern "zzz-nowhere")))
  (check-equal? (tool-outcome-content gr2) "(no matches)")
) ; end test-case

;; ---------------------------------------------------------------- permission

(test-case "permission matrix + always memory + persistence"
  (define store (build-path tmpdir "perms.rktd"))
  (define bash-t (registry-lookup reg "bash"))
  (define read-t (registry-lookup reg "read_file"))
  (define asked (box 0))
  (define (asker-yes _q) (set-box! asked (add1 (unbox asked))) 'yes)
  (define (asker-no _q) (set-box! asked (add1 (unbox asked))) 'no)
  (define (asker-always _q) (set-box! asked (add1 (unbox asked))) 'always)
  ;; normal: read-only 直通，dangerous 询问
  (define pol (make-policy (default-config) #:store-path store))
  (check-equal? (permission-check pol read-t (hasheq) asker-no) 'allow)
  (check-equal? (unbox asked) 0)
  (check-equal? (permission-check pol bash-t (hasheq) asker-no) 'deny)
  (check-equal? (permission-check pol bash-t (hasheq) asker-yes) 'allow)
  ;; always 记忆：本轮及之后不再询问
  (check-equal? (permission-check pol bash-t (hasheq) asker-always) 'allow)
  (define before (unbox asked))
  (check-equal? (permission-check pol bash-t (hasheq) asker-no) 'allow)
  (check-equal? (unbox asked) before)                     ; 未再询问
  ;; 持久化：新 policy 从 .rktd 恢复
  (define pol2 (make-policy (default-config) #:store-path store))
  (check-equal? (permission-check pol2 bash-t (hasheq) asker-no) 'allow)
  ;; strict: mutating 也询问
  (define strict-cfg
    (struct-copy config (default-config) [permission-mode 'strict])
  ) ; end define strict-cfg
  (define pol3 (make-policy strict-cfg))
  (define write-t (registry-lookup reg "write_file"))
  (check-equal? (permission-check pol3 write-t (hasheq) asker-no) 'deny)
  ;; yolo: 全部直通
  (define yolo-cfg
    (struct-copy config (default-config) [permission-mode 'yolo])
  ) ; end define yolo-cfg
  (define pol4 (make-policy yolo-cfg))
  (check-equal? (permission-check pol4 bash-t (hasheq) asker-no) 'allow)
) ; end test-case

(delete-directory/files tmpdir)
(displayln "tool-test: all passed")
