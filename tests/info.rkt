#lang info
;; 让 `raco test tests/` 自动发现并运行离线单测；真机(live)测试需 LM Studio，
;; 从目录遍历中排除，仅在 `./run-tests.sh --live` 时按文件名显式运行。
(define test-omit-paths
  '("provider-live-test.rkt"
    "loop-live-test.rkt"
    "subagent-live-test.rkt"))
