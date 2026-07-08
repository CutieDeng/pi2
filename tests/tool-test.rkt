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
  (check-false (registry-lookup reg "nope"))
  (check-equal? (length (registry-specs reg)) 6)
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
  ;; edit: 多次出现
  (run "write_file" (hasheq 'path "dup.txt" 'content "aa aa"))
  (define r6 (run "edit_file" (hasheq 'path "dup.txt"
                                      'old_string "aa" 'new_string "b")))
  (check-true (tool-outcome-is-error? r6))
  (check-true (string-contains? (tool-outcome-content r6) "must be unique"))
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
