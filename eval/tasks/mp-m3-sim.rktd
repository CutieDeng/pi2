;; E1 任务（T3，Python）：market-pit 里程碑 3——四种策略 + 撮合回合 + 确定性。隐藏验收：87316f1 的 test_sim.py 与 test_account.py（那次也改了 Trade）。
(eval-task
  (id "mp-m3-sim")
  (tier "T3")
  (source (git "/Users/cutiedeng/Y2026/M07/D09/market-pit" "9f63b9a"))
  (prompt "Implement milestone 3 of market-pit (read AGENTS.md; Python 3.9 standard library only): the strategies and the session simulator. Where AGENTS.md disagrees with this list, this list wins. Hidden acceptance tests use:

- Trade gains a field maker_side: Side — the side of the RESTING order (SELL when the incoming order was a buy, BUY when it was a sell); OrderBook must set it on every trade. Tests build Trade only by keyword (price, qty, maker_id, taker_id, ts, maker_side); keep existing callers working (e.g. give it a default).
- pit/strategies.py: abstract Strategy with __init__(participant_id: int), attribute participant_id, and abstract actions(book, account, rng: random.Random) -> List[Order]. The method is named actions, NOT quote. Concrete classes, every parameter except participant_id with a default: MarketMaker(participant_id, fair_value=10_000, spread=20, quote_size=15, max_inventory=50, skew_factor=2.0); Momentum(participant_id, lookback=5, order_size=10, threshold=5); MeanReversion(participant_id, window=10, order_size=10, entry_z=1.5); Noise(participant_id, base_price=10_000, order_size=10, intensity=0.5) where intensity is the per-tick probability of acting.
- Every Order a strategy returns: int id; side BUY or SELL; type LIMIT, MARKET or CANCEL; qty > 0 (also for CANCEL); price 0 for MARKET and > 0 for LIMIT. Strategies must not crash on an empty book (mid() is None) and may be called repeatedly without their orders being submitted. Order ids are unique across participants and reproducible with fresh instances (e.g. participant_id * 1_000_000 + a per-instance counter; no module-level counters).
- MarketMaker reads account.inventory; with inventory 0 on an empty book it returns one LIMIT BUY and one LIMIT SELL around fair_value; when long both its quotes are <= the neutral quotes, when short both are >= (inventory skew); it cancels its previous quotes each call.
- pit/sim.py: Sim(seed: int, strategies: List[Strategy]) with run(ticks: int) -> SessionResult. Sim keeps sim._rng (the single random.Random(seed)), sim._book (OrderBook) and sim._accounts (dict participant_id -> Account, each created with initial_cash=100_000). Each tick: strategies act in list order with actions(book, own_account, sim._rng); their orders are submitted; each Trade is applied to the maker's account with trade.maker_side and to the taker's with the opposite side (map order ids to participants); all trades go to the trade log.
- NamedTuples in pit/sim.py: ParticipantResult(participant_id, total_pnl, realized_pnl, unrealized_pnl, trade_count, final_cash, final_inventory) — all int; SessionResult(leaderboard: List[ParticipantResult], trade_log: List[Trade], tick_count: int). Leaderboard: one entry per strategy, sorted by total_pnl descending (mark to the final mid(), falling back to the last trade price); trade_count counts trades where the participant was maker or taker; tick_count == ticks.
- Determinism: same seed + fresh identical strategies -> equal leaderboard, trade_log and tick_count; different seeds give different leaderboards over a 200-tick session with all four strategies. Conservation: sum(final_cash) + mark * sum(final_inventory) == 100_000 * number_of_participants.
Also write your own unittest tests. Do not modify the existing tests.")
  (reference (git "87316f1" "pit"))
  (hidden (git "87316f1" "tests/test_sim.py") (git "87316f1" "tests/test_account.py"))
  (protect "tests/**" "AGENTS.md")
  (verify "python3 -m unittest discover -s tests -t ." #:parse unittest #:timeout 300)
  (limits #:max-calls 100 #:minutes 40))
