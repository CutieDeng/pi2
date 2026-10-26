;; pi2 的工作区测试流程（design-goalmode.md §12）：模型用 tests_run 按需跑，声称完成前用 acceptance_report（跑全部步骤）。
;; 不用 run-tests.sh：它跑 `raco test tests/`，会跑到 tests/dsv4b-probes（真打 DeepSeek）。
(test-flow
  (name "pi2")
  (step offline
        "raco make -j 4 $(ls tests/*.rkt | grep -vE -- '-live-test[.]rkt$|dsv4b-bench[.]rkt$') tests/gui/*.rkt tests/gui/fixtures/*.rkt && raco test -j 4 $(ls tests/*.rkt | grep -vE -- '-live-test[.]rkt$|dsv4b-bench[.]rkt$|/info[.]rkt$') tests/gui"
        #:parse raco #:timeout 1200)
  (step compact
        "raco make tests/compact-test.rkt tests/context-test.rkt && raco test tests/compact-test.rkt tests/context-test.rkt"
        #:parse raco #:timeout 300)
  (protect "tests/**" "run-tests.sh"))
