;; E1 任务（T1，Racket 修 bug）：pi2 的 /remember 不认全角冒号（2026-10-25 真实修过的缺陷，注入回去）
(eval-task
  (id "pi2-remember-colon")
  (tier "T1")
  (source (files (from "/Users/cutiedeng/Y2026/M07/D09/pi2/src/gui/compute/memory-store.rkt" "src/gui/compute/memory-store.rkt")
                 (from "/Users/cutiedeng/Y2026/M07/D09/pi2/tests/gui/memory-store-test.rkt" "tests/gui/memory-store-test.rkt")))
  (inject (replace "src/gui/compute/memory-store.rkt"
                   "(?:[:：]\\\\s*|\\\\s+)(.*)$"
                   "[:：]?\\\\s+(.*)$")
          (replace "tests/gui/memory-store-test.rkt"
                   "  (check-equal? (p \"/remember 全局：用中文回答\") '(\"global\" \"用中文回答\"))\n"
                   ""))
  (prompt "Bug report: in this Racket project, the input-box command `/remember 全局：用中文回答` (scope word, a FULL-WIDTH colon, then the text, no space) is saved as a workspace memory with the text \"全局：用中文回答\", instead of a global memory with the text \"用中文回答\". After the scope word (global / project / workspace or 全局 / 项目 / 工作区) a colon — ASCII ':' or full-width '：' — with or without following spaces, or just whitespace, must separate the scope from the text. The parser is `parse-remember` in src/gui/compute/memory-store.rkt; tests are in tests/gui/memory-store-test.rkt (run: raco test tests/gui/memory-store-test.rkt). Fix the bug without breaking the existing behavior.")
  (reference (replace "src/gui/compute/memory-store.rkt" "[:：]?\\\\s+(.*)$" "(?:[:：]\\\\s*|\\\\s+)(.*)$"))
  (hidden (file "/Users/cutiedeng/Y2026/M07/D09/pi2/tests/gui/memory-store-test.rkt" "tests/gui/memory-store-test.rkt"))
  (protect "tests/**")
  (verify "raco test tests/gui/memory-store-test.rkt" #:parse raco #:timeout 300)
  (limits #:max-calls 40 #:minutes 15))
