#lang tstring racket
;; credentials-test.rkt — 凭据存储/解析/掩码（离线；PI_CONFIG_HOME 指向临时目录隔离）。

(require
 rackunit
 racket/file
 (file "../src/credentials.rkt")
) ; end require

(define tmp (make-temporary-file "pi2-credtest-~a" 'directory))
(putenv "PI_CONFIG_HOME" (path->string tmp))

;; 干净起点：确保测试用 env 名未被外部设置
(for ([n '("PI_TEST_KEY" "PI_TEST_ENVONLY")]) (putenv n ""))

(test-case "store-key! then resolve-key round-trips; file is 0600"
  (store-key! "PI_TEST_KEY" "sk-secret-abcd1234")
  (check-equal? (resolve-key "PI_TEST_KEY") "sk-secret-abcd1234")
  (check-equal? (key-source "PI_TEST_KEY") 'file)
  ;; 权限位应恰为 0600
  (check-equal? (bitwise-and (file-or-directory-permissions (credentials-path) 'bits) #o777) #o600)
) ; end test-case

(test-case "env var takes precedence over stored file value"
  (putenv "PI_TEST_KEY" "sk-from-env")
  (check-equal? (resolve-key "PI_TEST_KEY") "sk-from-env")
  (check-equal? (key-source "PI_TEST_KEY") 'env)
  (putenv "PI_TEST_KEY" "")                 ; 复位 → 回落文件
  (check-equal? (resolve-key "PI_TEST_KEY") "sk-secret-abcd1234")
) ; end test-case

(test-case "empty-string env is treated as unset"
  (check-equal? (key-source "PI_TEST_ENVONLY") #f)
  (check-false (resolve-key "PI_TEST_ENVONLY"))
) ; end test-case

(test-case "multiple keys coexist; stored-key-names sorted"
  (store-key! "ANTHROPIC_API_KEY" "sk-ant-xxxx")
  (check-equal? (resolve-key "ANTHROPIC_API_KEY") "sk-ant-xxxx")
  (check-equal? (resolve-key "PI_TEST_KEY") "sk-secret-abcd1234")   ; 未被覆盖
  (check-true (and (member "ANTHROPIC_API_KEY" (stored-key-names)) #t))
) ; end test-case

(test-case "delete-key! removes; resolve falls to #f"
  (check-true (delete-key! "PI_TEST_KEY"))
  (check-false (resolve-key "PI_TEST_KEY"))
  (check-false (delete-key! "PI_TEST_KEY"))     ; 二次删除 → #f
) ; end test-case

(test-case "mask-key never leaks full secret"
  (check-equal? (mask-key "sk-secret-abcd1234") "sk-…1234")
  (check-equal? (mask-key "short") "•••••")     ; ≤8 全掩
  (check-equal? (mask-key #f) "—")
) ; end test-case

(delete-directory/files tmp)
(displayln "credentials-test: all passed")
