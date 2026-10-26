;; E1 任务（T2，Python）：market-pit 里程碑 1——订单类型 + 撮合引擎。历史提交 4a3b7d9 加的测试是隐藏验收。
(eval-task
  (id "mp-m1-book")
  (tier "T2")
  (source (git "/Users/cutiedeng/Y2026/M07/D09/market-pit" "1e38782"))
  (prompt "Implement milestone 1 of market-pit (read AGENTS.md for the project; Python 3.9 standard library only, unittest for tests): the order types and the limit-order-book matching engine. Hidden acceptance tests will import and exercise exactly this interface:

- pit/orders.py: enums Side (BUY, SELL) and OrderType (LIMIT, MARKET, CANCEL — CANCEL is required even though AGENTS.md lists only LIMIT/MARKET). Order(id: int, side: Side, price: int, qty: int, type: OrderType) — an immutable NamedTuple in this field order, built positionally and by keyword; prices are integer cents. Trade(price: int, qty: int, maker_id: int, taker_id: int, ts: int) — NamedTuple in this field order.
- pit/book.py: OrderBook() with submit(order) -> List[Trade], best_bid() -> Optional[int], best_ask() -> Optional[int], mid() -> Optional[int], depth(n: int = 5) -> (asks, bids).
- best_bid / best_ask are None when that side is empty; mid() is None if either side is empty, else (best_bid + best_ask) // 2.
- depth(n) returns the tuple (asks, bids); each side is a list of (price, total_resting_qty) aggregated per level, asks ascending, bids descending, at most n levels per side; empty book -> ([], []).
- Price-time priority: an incoming BUY matches asks from the lowest price up, a SELL matches bids from the highest down; within a level, FIFO by arrival; a partially filled resting order keeps its place with reduced qty. One Trade per resting order touched, in execution order: price = the resting order's price, maker_id = resting id, taker_id = incoming id, qty = filled amount, ts an increasing int.
- LIMIT: matches while it crosses (BUY price >= best ask, SELL price <= best bid), possibly across levels; the remainder rests at its own limit price; a non-crossing limit rests and returns []. Orders on the same side never match each other.
- MARKET: price is ignored; sweeps the opposite side until filled or empty; the remainder is discarded (never rests); empty opposite side -> [] without error.
- CANCEL: the order's id names the resting order to remove (side/price/qty ignored); removes it from anywhere in its level; an emptied level disappears; unknown or already-filled id is a no-op (no exception); returns [].
- Fully filled resting orders leave the book.
Also write your own unittest tests in tests/. Work until your tests pass.")
  (reference (git "4a3b7d9" "pit"))
  (hidden (git "4a3b7d9" "tests/test_book.py") (git "4a3b7d9" "tests/__init__.py"))
  (protect "AGENTS.md")
  (verify "python3 -m unittest discover -s tests -t ." #:parse unittest #:timeout 300)
  (limits #:max-calls 80 #:minutes 30))
