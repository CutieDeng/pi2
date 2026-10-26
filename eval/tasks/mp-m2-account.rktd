;; E1 任务（T2，Python）：market-pit 里程碑 2——账户（现金 / 持仓 / FIFO 已实现盈亏 / 守恒）。隐藏验收：9f63b9a 的 test_account.py。
(eval-task
  (id "mp-m2-account")
  (tier "T2")
  (source (git "/Users/cutiedeng/Y2026/M07/D09/market-pit" "4a3b7d9"))
  (prompt "Implement milestone 2 of market-pit (read AGENTS.md; Python 3.9 standard library only): the per-participant account in pit/account.py. Keep pit/orders.py's Trade unchanged (tests build it by keyword with exactly price, qty, maker_id, taker_id, ts). Hidden acceptance tests use this interface:

- Account(participant_id: int, initial_cash: int = 0) with attributes participant_id, cash (int, starts at initial_cash) and inventory (signed int, starts at 0, a plain writable attribute).
- apply_trade(trade: Trade, side: Side) -> None, where side is THIS account's side in the trade (passed in, not inferred). BUY: cash -= price*qty, inventory += qty. SELL: cash += price*qty, inventory -= qty. Integer arithmetic.
- realized_pnl: a read-only property (int). FIFO cost basis on long lots: each BUY adds a lot (qty, price); each SELL consumes lots oldest first and adds (sell_price - lot_price) * consumed_qty; a partially consumed lot keeps its remainder at the original price. A SELL with no long lots realizes nothing and creates no short lot; a BUY never changes realized_pnl (even when covering a short) — so a short-then-cover leaves realized_pnl == 0 and the gain shows only in cash / net liquidation value.
- unrealized_pnl(mark_price: Optional[int] = None) -> int: 0 if inventory == 0 or no mark is available; otherwise the sum over remaining lots of (mark - lot_price) * lot_qty; mark None -> this account's last trade price; no trades -> 0.
- total_pnl(mark_price=None) = realized_pnl + unrealized_pnl(mark_price).
- net_liquidation_value(mark_price: Optional[int] = None) = cash + inventory * mark (positional or keyword); mark None -> last trade price; no trades -> cash. For a long-only history NLV(m) == initial_cash + total_pnl(m).
- Conservation: when each trade is applied once as BUY to one account and once as SELL to another, sum(cash + inventory * mark) over all accounts equals sum(initial_cash).
Also write your own unittest tests. Do not modify the existing tests.")
  (reference (git "9f63b9a" "pit"))
  (hidden (git "9f63b9a" "tests/test_account.py"))
  (protect "tests/**" "AGENTS.md")
  (verify "python3 -m unittest discover -s tests -t ." #:parse unittest #:timeout 300)
  (limits #:max-calls 80 #:minutes 30))
