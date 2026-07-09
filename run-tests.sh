#!/bin/zsh
# run-tests.sh — pi++ 测试驱动（原生 raco test）
#   ./run-tests.sh          仅离线单测（tests/info.rkt 里的 test-omit-paths 已排除 live）
#   ./run-tests.sh --live   追加对 LM Studio (gemma) 的真机验收
#
# raco test 原生集成 rackunit：自动发现 tests/ 下的 .rkt、并行执行、汇总通过数、
# 失败即非零退出——无需手写循环。
set -e
cd "$(dirname "$0")"

echo "=== offline unit tests (raco test) ==="
raco test -j 4 tests/

if [[ "$1" == "--live" ]]; then
  echo "=== live tests (LM Studio gemma-4-31b-it@6bit) ==="
  raco test \
    tests/provider-live-test.rkt \
    tests/loop-live-test.rkt \
    tests/subagent-live-test.rkt
fi

echo "all tests passed"
