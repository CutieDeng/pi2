#!/bin/zsh
# run-tests.sh — pi++ 测试驱动
#   ./run-tests.sh          仅离线单测
#   ./run-tests.sh --live   含对 LM Studio (gemma) 的真机验收
set -e
cd "$(dirname "$0")"

OFFLINE=(
  tests/base-test.rkt
  tests/stream-test.rkt
  tests/tool-test.rkt
  tests/loop-test.rkt
  tests/context-test.rkt
  tests/session-test.rkt
  tests/hook-test.rkt
  tests/tui-width-test.rkt
  tests/tui-keys-test.rkt
  tests/tui-lineedit-test.rkt
  tests/tui-e2e-test.rkt
)

LIVE=(
  tests/provider-live-test.rkt
  tests/loop-live-test.rkt
  tests/subagent-live-test.rkt
)

echo "=== offline unit tests ==="
for t in $OFFLINE; do
  printf "  %-28s " "$t"
  racket "$t" >/dev/null && echo "ok"
done

if [[ "$1" == "--live" ]]; then
  echo "=== live tests (LM Studio gemma-4-31b-it@6bit) ==="
  for t in $LIVE; do
    printf "  %-32s " "$t"
    racket "$t" >/dev/null && echo "ok"
  done
fi

echo "all tests passed"
