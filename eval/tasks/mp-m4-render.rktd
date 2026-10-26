;; E1 任务（T2，Python）：market-pit 里程碑 4——终端渲染 + CLI。隐藏验收：3a11e4e 的 test_render.py（快照那个只查子串与同种子输出一致，不泄露具体文字，保留）。
(eval-task
  (id "mp-m4-render")
  (tier "T2")
  (source (git "/Users/cutiedeng/Y2026/M07/D09/market-pit" "87316f1"))
  (prompt "Implement milestone 4 of market-pit (read AGENTS.md; Python 3.9 standard library only): ASCII rendering in pit/render.py and the CLI in pit/__main__.py. All render functions return strings and print nothing. Hidden acceptance tests use:

- render_ladder(book: OrderBook, levels: int = 5) -> str: contains the title 'Order Book'; one row per depth level (top 'levels' per side), asks above bids, each row with price, size and the label ASK or BID; when both sides are non-empty it contains 'spread=<best_ask - best_bid>' (no spaces around '='). The uppercase strings ASK and BID appear only in level rows — an empty book shows the title and neither string anywhere (so column headers must not contain them).
- render_tape(trades: List[Trade], max_lines: int = 10) -> str: title 'Trade Tape'; exactly one column-header line containing 'Price' (the word Price appears exactly once); separator lines made of '-'; only the newest max_lines trades, one row each, showing price, qty, the maker as '<maker_id><B|S>' with no space (B if maker_side is BUY, S if SELL), the taker and ts; no other non-blank lines; an empty list gives output containing '(no trades)'.
- render_leaderboard(result: SessionResult) -> str: title 'Leaderboard'; one row per entry in result.leaderboard order, each row (after leading whitespace) starting with its 1-based rank, then participant_id, total_pnl (negative numbers with '-') and the other fields; no other line may start with a digit after stripping; a footer containing 'Trades: <len(result.trade_log)>' and 'Ticks: <result.tick_count>'.
- render_session(result: SessionResult, book: OrderBook, levels: int = 5, tape_lines: int = 10) -> str: the ladder, the tape of result.trade_log (max_lines=tape_lines) and the leaderboard joined; contains 'Order Book', 'Trade Tape' and 'Leaderboard'; deterministic for a given seed.
- CLI: python3 -m pit --seed N --ticks T [--watch] runs a session with all four strategies and prints the final book, tape and leaderboard; same seed -> identical output.
Also write your own unittest tests. Do not modify the existing tests.")
  (reference (git "3a11e4e" "pit"))
  (hidden (git "3a11e4e" "tests/test_render.py"))
  (protect "tests/**" "AGENTS.md")
  (verify "python3 -m unittest discover -s tests -t ." #:parse unittest #:timeout 300)
  (limits #:max-calls 80 #:minutes 30))
