# pvector 反常发现 — 交接给 Racket 原生开发

日期：2026-08-16。环境：增强版 Racket **v9.2.5 [cs]**（自编译，源码 `~/Y2026/M04/D03/racket.git`）。
缘起：pi2 原生浏览器给 JS 数组选存储结构时，实测 `racket/pvector` 出现一个反直觉行为。
复现工具：`src/web/dev/pvector-bench.rkt`（`racket src/web/dev/pvector-bench.rkt`）。

## 现象（已验证，可复现，非测量假象）

**同一个逻辑向量，构建方式决定随机访问复杂度：**

| 构建方式 | append 成本 | 随机 ref 复杂度 | 首尾 ref |
|---|---|---|---|
| `pvector-cons-right`（逐个追加） | **O(1)**（build 随 N 线性） | **O(n)**（随 N 线性翻倍） | O(1)（快路） |
| `list->pvector`（批量构建） | — | **O(log n)**（几乎不随 N 变） | O(1) |

实测（热身后，中位数，排除懒求值/首触）：cons-right 构建的 pvector，N=1000→8000（8×）时中间随机
ref 从 0.22µs 涨到 2.24µs（约 10×，**远快于 O(log n) 该有的 1.3×**，故排除「大常数 O(log n)」）；
N=64000 时中间随机 ref 达 41µs，而首尾仍 0.008µs。list->pvector 构建的则几乎不随 N 变（O(log n)）。

**反直觉点**：cons-right append 是 O(1)（已验证 build 线性），按 finger tree 理论随机 ref 本应
O(log n)，却实测 O(n)。首尾 O(1)、中间 O(n) 的指纹，指向「finger tree 结构正确但中间随机索引
未走 O(log n) 快路」。

## 已排除的假设（别再走这两条）

1. **不是退化/不平衡树**——源码 `racket/private/pvector-core.rkt` 确认是 measured finger tree
   （`ft:deep (v left inner right)` + `ft:config` 带 measure），平衡结构。
2. **不是 chunk-index 快路的问题**——`racket/private/pvector-runtime-adapter.rkt` 里
   `compiled-core-no-chunk?` 明确要求 **no-chunk 形态**（`chunked-tree?`=#f、`chunk-index-vectors`=0、
   `ref-cache?`=#f）才启用 kernel core；chunk-indexing 是**被禁用的旧机制**，不是当前快路。

## 未解（native 核心，源码读不到）

`pvector-ref` 中间下标走 `raw:pvector-ref/fast tree index`，该原语是**编译的 native**
（`compiled-core-available?` 检查的 `core-bindings` 是编译过程）。结构自省 `core-pvector-shape-stats`
**存在但未 export**，公共 API 够不到。故「为何 cons-right 构建的 finger tree 中间随机 ref 是 O(n)、
而 list->pvector 构建的是 O(log n)」——从可读源码无法确定。

## 给 Racket 原生 agent 的调查方向

1. **看两种构建的实际结构**：让 `core-pvector-shape-stats`（或等价自省）可达，dump cons-right vs
   list->pvector 构建的 finger tree 形态（深度、digit 填充、node 嵌套），对比差异。
2. **查 `raw:pvector-ref/fast` 中间索引路径**：是否对 cons-right 累积出的形态未用 size-measure 做
   O(log n) split，而退化成线性扫描？（源在 native / Chez 层，`racket.git`。）
3. **验证是否「构建即固化」**：list->pvector 是否产出扁平/literal 表示（见 `pvector-literal-pool` /
   `pvector-flat-literal-datum`），而 cons-right 产出未被 rebalance/normalize 的 finger tree？
4. **修复方向候选**：(a) cons-right 累积到阈值后自动 normalize 成 O(log n)-index 形态；
   (b) `raw:pvector-ref/fast` 对 finger tree 用 measure-split 保证 O(log n)；(c) 文档明确「增量构建的
   pvector 随机 ref 是 O(n)，需批量构建或 normalize」。

## 对 pi2 的影响（已按此决策）

JS 数组不用 pvector（靠 push 增长 = cons-right = O(n) 随机 ref，而 `a[i]` 循环高频）。现状用
`racket/intmap`（O(log n)、处理稀疏），密集数组快路留待 B3 用**原生 vector**（实测最快、对齐 V8
packed elements）。见 design-jsobj.md §2.3、browser-jsobj 记忆。
