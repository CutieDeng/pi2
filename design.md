# pi++ 设计文档

> 基于 Racket 的 LLM Agent 实践项目。目标：一个可在终端运行、支持工具调用循环 (tool-use loop)、
> 上下文管理与会话持久化的最小可扩展 agent 内核，取名 pi++（pi 的增强演进版）。

---

## 目录

1. [设计目标与原则](#1-设计目标与原则)
2. [总体架构](#2-总体架构)
3. [核心数据模型 (core/model)](#3-核心数据模型)
4. [模块划分与接口](#4-模块划分与接口)
   - 4.1 provider — LLM API 客户端
   - 4.2 stream — SSE 流式解析
   - 4.3 tool — 工具协议与注册表
   - 4.4 loop — agent 主循环
   - 4.5 context — 上下文与 token 预算管理
   - 4.6 session — 会话持久化与恢复
   - 4.7 permission — 权限门控
   - 4.8 event — 事件总线与 hooks
   - 4.9 repl — 终端交互层
   - 4.10 subagent — 子 agent
5. [各模块实现技术原理](#5-各模块实现技术原理)
6. [并发与取消模型](#6-并发与取消模型)
7. [错误处理策略](#7-错误处理策略)
8. [文件布局](#8-文件布局)
9. [里程碑](#9-里程碑)

---

## 1. 设计目标与原则

**目标**

- 用纯 Racket 实现一个完整可用的 LLM agent：用户输入 → 模型推理 → 工具调用 → 结果回填 → 循环直至产出最终回答。
- 内核小而正交：每个模块单文件、单职责，接口用 `contract` 显式声明。
- 全程流式：模型输出逐 token 渲染，工具执行可中断。
- 会话可持久化、可恢复（transcript 即真相源）。

**原则**

1. **不可变优先**：消息历史、agent 状态一律用不可变结构（`pvector` 持久向量），状态迁移 = 产生新状态；便于回溯、fork、重放。
2. **数据即协议，.rktd 为一等公民**：模块之间只传递纯数据，不传闭包（工具实现体除外）。
   内部数据的原生形态是 Racket datum：核心 struct 一律 `#:prefab`，`write`/`read` 直接
   往返，零序列化代码。transcript、配置、权限缓存全部落 `.rktd` datum 流；JSON（jsexpr）
   仅存在于 provider 的 API 边界一层。transcript 里的每一个 datum 都能完整重建状态。
3. **效应集中**：所有 IO（HTTP、文件、子进程）收敛在 provider / tool / session 三个模块，其余模块保持纯函数性。
4. **面向接口**：工具通过 `gen:tool` 泛型接口接入，provider 通过统一的 `provider<%>`-风格 struct+函数表接入，均可替换。

**依赖约定**

- 仅用 Racket 标准库：`net/http-client`、`json`、`racket/async-channel`、`racket/sandbox` 等。
- 使用本地 Racket 增强：`f""` 字符串模板、`pvector`（持久向量）、`int-map`（整数键 AVL 树）。

**代码风格约定**（全项目遵循）

C style begin/end：右括号单行分隔，注释标明配对的 datum：

```racket
(define (greet name)
  (displayln f"hello, {name}!")
) ; define greet
```

---

## 2. 总体架构

分层图（上层依赖下层，无反向依赖；跨层通信走 event 总线）：

```
┌───────────────────────────────────────────────┐
│  repl        终端交互 / 渲染 / 快捷命令        │
├───────────────────────────────────────────────┤
│  loop        agent 主循环（编排层）            │
│  subagent    子 agent 派生                     │
├──────────────┬──────────────┬─────────────────┤
│  context     │  tool        │  permission     │
│  上下文管理  │  工具注册表  │  权限门控       │
├──────────────┴──────────────┴─────────────────┤
│  provider    LLM API 客户端（含 stream 解析） │
│  session     transcript 持久化                 │
├───────────────────────────────────────────────┤
│  model       核心数据结构（消息/块/事件）     │
│  event       事件总线（横切，所有层可发布）   │
└───────────────────────────────────────────────┘
```

**一次完整交互的数据流**：

```
用户输入
  → repl 构造 user-message，append 进 history (pvector)
  → loop 调 context 做 token 预算裁剪，得到发送窗口
  → provider 发起流式请求，stream 解析 SSE 为 delta 事件
  → repl 订阅 delta 事件逐字渲染；loop 累积出 assistant-message
  → 若含 tool-use 块：permission 审批 → tool 执行 → 构造 tool-result
    → 回填 history，回到「provider 发起请求」一步
  → 若为纯文本终止：session 落盘，回到等待用户输入
```

---

## 3. 核心数据模型

`model.rkt` — 全项目共享的数据词汇，零依赖。

### 3.1 消息与内容块

对齐 Anthropic Messages API 的结构（也是业界事实标准），消息 = 角色 + 内容块列表。

**所有需要持久化的模型 struct 一律 `#:prefab`**：prefab struct 是 Racket 的自描述数据
类型，`write` 出 `#s(text-block "...")` 形态、`read` 回来即原 struct —— 不需要任何
serializer/deserializer 层，这是 .rktd 一等支持的地基（prefab 自带 transparent 语义，
可直接 `equal?` 比较与模式匹配）：

```racket
;; 内容块：代数数据类型，用带标签的 prefab struct 表达
(struct text-block (text) #:prefab)
(struct thinking-block (text signature) #:prefab)
(struct tool-use-block (id name input) #:prefab)           ; input : jsexpr
(struct tool-result-block (tool-use-id content is-error?) #:prefab)
(struct image-block (media-type data) #:prefab)

;; 消息
(struct message
  (role        ; 'user | 'assistant | 'system
   blocks      ; (listof content-block?)
  ) ; fields
  #:prefab
) ; struct message
```

注：`tool-use-block-input` 存的 jsexpr（hash/list/string/number 组合）本身就是合法
datum，prefab 外壳 + jsexpr 内容 → 整条消息可无损 `write`/`read` 往返。

### 3.2 Agent 状态

单一不可变 struct，主循环的每一步都是 `state → state` 的纯迁移：

```racket
(struct agent-state
  (history       ; pvector of message         — 全量消息历史
   pending       ; (listof tool-use-block?)   — 待执行的工具调用
   turn-count    ; exact-nonnegative-integer? — 已完成的 assistant 轮数
   token-usage   ; usage?                     — 累计 token 统计
   config        ; config?                    — 运行配置（模型、温度、上限...）
  ) ; fields
  #:transparent
) ; struct agent-state
```

选 `pvector` 而非 list 存 history 的理由：追加 O(log n)、随机访问 O(log n)（context
裁剪需要按索引二分定位截断点）、且各版本共享结构 —— fork 会话 / 回溯到第 k 条消息零拷贝。

### 3.3 事件

所有模块间的通知统一为事件（详见 4.8）：

```racket
(struct evt-base (timestamp) #:transparent)
(struct evt:delta       evt-base (kind text) #:transparent)   ; 流式增量
(struct evt:message     evt-base (msg) #:transparent)          ; 完整消息产出
(struct evt:tool-start  evt-base (block) #:transparent)
(struct evt:tool-end    evt-base (block result ms) #:transparent)
(struct evt:turn-end    evt-base (stop-reason usage) #:transparent)
(struct evt:error       evt-base (exn recoverable?) #:transparent)
```

---

## 4. 模块划分与接口

每小节给出：职责、导出接口（含 contract）、关键设计决策。实现原理集中在第 5 章。

### 4.1 provider — LLM API 客户端

**职责**：把「消息列表 + 工具 schema + 配置」变成一个流式事件序列。屏蔽具体厂商差异。

**接口**（`provider.rkt`）：

```racket
(provide
  (contract-out
    ;; 创建 provider 实例（读 env: ANTHROPIC_API_KEY 等）
    [make-anthropic-provider (-> config? provider?)]
    ;; 发起流式补全：立即返回一个 async-channel，
    ;; 通道内依次出现 evt:delta ... evt:message evt:turn-end，或 evt:error
    [provider-stream!
      (-> provider?
          (listof message?)     ; 发送窗口（已裁剪）
          (listof tool-spec?)   ; 工具 schema
          async-channel?
      ) ; ->
    ] ; provider-stream!
    ;; 同步取消：关闭底层连接
    [provider-cancel! (-> provider? void?)]
  ) ; contract-out
) ; provide
```

**设计决策**

- provider 是一个「函数表 struct」而非 class：`(struct provider (stream! cancel! count-tokens))`，
  新增 OpenAI 兼容端点只需再写一个构造器，loop 层零改动。
- 返回 async-channel 而非回调：消费方（loop/repl）用 `sync` 组合等待，天然支持超时与取消竞争。
- 消息 struct → API JSON 的序列化在此模块内完成（`message->jsexpr` / `jsexpr->blocks`），
  model 层不知道任何厂商细节。

### 4.2 stream — SSE 流式解析

**职责**：把 HTTP 响应的字节流解析成结构化事件。是 provider 的内部子模块，但独立成文件以便单测。

**接口**（`stream.rkt`）：

```racket
(provide
  (contract-out
    ;; 从 input-port 拉取并解析 SSE，将解析出的事件逐个送入 sink
    [sse-pump!
      (-> input-port?
          (-> string? jsexpr? void?)  ; sink: (event-type, data) -> void
          void?
      ) ; ->
    ] ; sse-pump!
    ;; 增量累积器：吃 Anthropic 的 content_block_delta 系事件，吐完整 message
    [make-block-accumulator (-> accumulator?)]
    [accumulator-feed! (-> accumulator? string? jsexpr? void?)]
    [accumulator-finish (-> accumulator? message?)]
  ) ; contract-out
) ; provide
```

**设计决策**

- accumulator 内部用 `int-map`（index → 累积中的块）承接 Anthropic 的
  `content_block_start / delta / stop` 三段协议：事件按 `index` 字段寻址块，
  int-map 的整数键有序遍历正好用于 finish 时按序拼装 blocks。
- `input_json_delta`（tool-use 参数的分片 JSON）先按字符串累积，`content_block_stop`
  时一次性 `string->jsexpr`，避免实现增量 JSON parser。

### 4.3 tool — 工具协议与注册表

**职责**：定义工具的统一协议，内置基础工具集，维护 name → tool 的注册表。

**接口**（`tool.rkt`）：

```racket
;; 工具协议：泛型接口
(define-generics tool
  [tool-name tool]              ; -> string?
  [tool-spec tool]              ; -> jsexpr?  给模型看的 JSON Schema
  [tool-permission-level tool]  ; -> (or/c 'read-only 'mutating 'dangerous)
  [tool-run tool input ctx]     ; input:jsexpr ctx:tool-ctx -> tool-outcome
) ; define-generics tool

(struct tool-outcome
  (content     ; string? 或 (listof content-block?) — 回填给模型的内容
   is-error?   ; boolean?
   display     ; (or/c #f string?) — 给用户看的摘要（可与 content 不同）
  ) ; fields
  #:transparent
) ; struct tool-outcome

(provide
  (contract-out
    [make-registry (-> (listof tool?) registry?)]
    [registry-lookup (-> registry? string? (or/c tool? #f))]
    [registry-specs (-> registry? (listof jsexpr?))]
    [builtin-tools (-> config? (listof tool?))]
  ) ; contract-out
) ; provide
```

**内置工具集（v1）**

| 工具 | 权限级 | 说明 |
|---|---|---|
| `bash` | dangerous | 子进程执行 shell 命令，带超时与输出截断 |
| `read_file` | read-only | 读文件，带行号与分页 |
| `write_file` | mutating | 写文件 |
| `edit_file` | mutating | old→new 精确字符串替换 |
| `glob` / `grep` | read-only | 文件名匹配 / 内容搜索 |
| `spawn_agent` | dangerous | 派生子 agent（见 4.10） |

**设计决策**

- `tool-run` 拿到的 `ctx` 携带：工作目录、event 发布函数、取消 evt、config。
  工具不直接触碰全局状态。
- `display` 与 `content` 分离：给模型的内容要完整（如全部 stdout），给用户的
  渲染可以是截断摘要 —— 两个受众，两份视图。

### 4.4 loop — agent 主循环

**职责**：编排一切。整个 agent 的语义浓缩为一个状态迁移函数。

**接口**（`loop.rkt`）：

```racket
(provide
  (contract-out
    ;; 驱动一个完整「用户轮」：从收到用户消息到模型产出终止性回答
    ;; 返回新状态；期间所有可观察行为通过 bus 发布
    [run-turn!
      (-> agent-state?
          message?        ; 新的 user message
          deps?           ; provider + registry + bus + permission 的依赖包
          agent-state?
      ) ; ->
    ] ; run-turn!
  ) ; contract-out
) ; provide
```

**核心算法**（伪代码级）：

```racket
(define (run-turn! st user-msg deps)
  (let step ([st (state-append st user-msg)])
    (define window (context-fit (agent-state-history st) (agent-state-config st)))
    (define asst-msg (stream-and-collect! deps window))    ; 阻塞直到本轮流结束
    (define st* (state-append st asst-msg))
    (define calls (message-tool-uses asst-msg))
    (cond
      [(null? calls) st*]                                  ; 终止：纯文本回答
      [(turn-budget-exceeded? st*) (state-append st* (budget-notice-msg))]
      [else
        (define results (execute-calls! calls deps))        ; 审批 + 执行
        (step (state-append st* (message 'user results)))
      ] ; else
    ) ; cond
  ) ; let step
) ; define run-turn!
```

**设计决策**

- loop 本身不做任何 IO 细节，只组合 context / provider / tool / permission 四个能力，
  是全项目最薄也最核心的一层（预计 <150 行）。
- `turn-budget`（单轮最大工具调用次数 / 最大 token）防失控循环，超限时注入一条
  提示消息让模型收尾，而非硬掐断。

### 4.5 context — 上下文与 token 预算管理

**职责**：保证发送窗口不超过模型上下文上限，且尽量保留高价值信息。

**接口**（`context.rkt`）：

```racket
(provide
  (contract-out
    ;; 估算一段消息的 token 数（本地启发式，不打 API）
    [estimate-tokens (-> (listof message?) exact-nonnegative-integer?)]
    ;; 裁剪：history (pvector) -> 发送窗口 (list)，保证 ≤ budget
    [context-fit (-> (pvector/c message?) config? (listof message?))]
    ;; 压缩：把旧历史总结成一条 summary 消息（调用模型，异步）
    [compact! (-> agent-state? deps? agent-state?)]
  ) ; contract-out
) ; provide
```

**裁剪策略（三级）**

1. **透传**：估算总量 < 80% 预算 → 原样发送。
2. **中段淘汰**：超预算 → 保留 system + 首条 user（任务锚点）+ 尾部 N 条完整轮次，
   中段的 tool-result 内容替换为 `f"[elided {n} chars]"` 占位（工具结果是历史中
   最大且最易再生的部分）。
3. **总结压缩**：仍超预算 → 触发 `compact!`：用模型把首部历史总结为一条
   summary 消息，原文段落存入 transcript 供恢复（有损但可审计）。

**设计决策**

- token 估算用本地启发式（详见 5.5）而非 count-tokens API：裁剪判断在每轮热路径上，
  不能容忍一次额外网络往返；误差用 80% 安全边际吸收。
- 裁剪永不破坏 `tool-use ↔ tool-result` 配对（API 硬约束）：淘汰以「轮次」为原子单位。

### 4.6 session — 会话持久化与恢复（.rktd 一等支持）

**职责**：transcript 即真相源。每产生一条消息/事件即追加落盘；启动时可完整重建状态。

底层抽出通用的 **datum-log** 流式读写层（`rktd.rkt`），session、permission 缓存、
配置读取共用：

```racket
;; rktd.rkt — .rktd datum 流的通用读写
(provide
  (contract-out
    [datum-log-open! (-> path-string? datum-log?)]       ; 打开或新建（append 模式）
    [datum-log-append! (-> datum-log? any/c void?)]      ; 追加一个 datum 并 flush
    ;; 流式重放：返回惰性 sequence，常数内存，可提前停止
    [in-datum-log (-> path-string? (sequence/c any/c))]
    ;; 追随模式：阻塞等待新 datum（tail -f 语义，供观测/调试工具用）
    [datum-log-follow! (-> path-string? (-> any/c any) void?)]
  ) ; contract-out
) ; provide
```

**接口**（`session.rkt`，构建在 rktd.rkt 之上）：

```racket
;; 记录 = prefab struct，直接 write 进 log
(struct rec
  (type   ; 'meta | 'msg | 'usage | 'compact
   ts     ; ISO8601 string
   data   ; 对应的 prefab 数据（如 message struct 本体）
  ) ; fields
  #:prefab
) ; struct rec

(provide
  (contract-out
    [session-open! (-> path-string? config? session?)]   ; 打开或新建（新文件写 meta）
    [session-append-msg! (-> session? message? void?)]   ; 追加一条消息记录
    [session-append-usage! (-> session? usage? void?)]   ; 追加用量增量
    [session-close! (-> session? void?)]                 ; 关闭（空的新会话自动清理）
    [session-replay (-> path-string? agent-state?)]      ; 流式重放重建状态
    ;; —— 恢复能力 ——
    [session-infos (-> path-string? (listof session-info?))]  ; 富元信息，按 mtime 降序
    [session-latest (-> path-string? (or/c #f string?))]      ; 最近一次会话路径
    [session-fork! (-> path-string? path-string? path-string?)] ; #:at N 分叉出新会话
    [session-delete! (-> path-string? void?)]            ; 删除会话文件
  ) ; contract-out
) ; provide
```

**存储格式**：`.rktd` datum 流 —— 一个文件是若干顶层 datum 的序列，每条记录形如
`#s(rec msg "2026-07-09T10:00:00" #s(message assistant (...)))`。datum 自定界
（`read` 恰好消费一个完整 datum），因此**不依赖行约定**：记录可以用 `pretty-write`
多行美化输出，transcript 同时是机器真相源和人可直接阅读/编辑/`read` 进 REPL 调试
的数据文件。

**设计决策**

- **零序列化层**：消息 struct 是 prefab，`session-append!` 内部就是一句
  `(datum-log-append! log r)`；对比 JSONL 方案省掉整个 struct↔jsexpr 双向映射及其
  漂移风险。
- **流式重放**：`session-replay` = 对 `in-datum-log` 的一次 fold，常数内存、
  可在第 k 条提前停止（`/resume` 到历史某点、fork 会话都由此免费获得）。
  回放按序调用运行时同一套 `state-append` 迁移函数 —— 「存的」与「跑的」不漂移。
- 只追加、不改写 + 每记录即时 `flush` → **崩溃安全**：`read` 遇残缺尾 datum 抛
  `exn:fail:read`，重放时捕获并丢弃即可，无需锁；`session-latest` + `-c` 即可恢复
  崩溃前的最新会话（仅丢未完成的尾记录）。
- **恢复能力**建在同一 datum 流上：`session-infos` 折叠每文件取 `meta`（时间/模型）、
  首条 user 消息（**自动标题**）、消息数，按 mtime 降序供 `--list`/选择器；
  `session-fork!` = `session-replay #:stop-after N` 后把前 N 条写入新 `.rktd`（分支）；
  空的新会话（只有 meta）在 `session-close!` 时自动删除（`fresh?`+`nmsg` 记账），
  避免每次启动堆积空文件。选择器 UI 见 §11.9。

### 4.7 permission — 权限门控

**职责**：工具执行前的审批决策点。

**接口**（`permission.rkt`）：

```racket
(provide
  (contract-out
    ;; 决策：allow / deny / ask（ask 时经 asker 回调询问用户）
    [permission-check
      (-> permission-policy?
          tool? jsexpr?                 ; 哪个工具、什么参数
          (-> string? (or/c 'yes 'no 'always))  ; asker（阻塞式询问）
          (or/c 'allow 'deny)
      ) ; ->
    ] ; permission-check
    [make-policy (-> config? permission-policy?)]
  ) ; contract-out
) ; provide
```

**策略模型**：三档模式 × 工具权限级的决策矩阵 + `always` 记忆
（用户答过 always 的 (tool, 参数模式) 记入 policy 缓存，并经 `rktd.rkt` 持久化到
`permissions.rktd`，跨会话生效）：

| | read-only | mutating | dangerous |
|---|---|---|---|
| `strict` | allow | ask | ask |
| `normal`（默认） | allow | allow | ask |
| `yolo` | allow | allow | allow |

### 4.8 event — 事件总线与 hooks

**职责**：解耦「发生了什么」与「谁关心」。repl 渲染、session 落盘、日志都是订阅者。

**接口**（`event.rkt`）：

```racket
(provide
  (contract-out
    [make-bus (-> bus?)]
    [bus-publish! (-> bus? evt-base? void?)]
    ;; 订阅：返回退订 thunk；handler 在独立 thread 上被调用
    [bus-subscribe! (-> bus? (-> evt-base? any) (-> void?))]
  ) ; contract-out
) ; provide
```

**设计决策**

- 实现为「每订阅者一个 async-channel + 消费 thread」：发布方永不被慢订阅者阻塞
  （渲染卡顿不能拖慢 agent 主循环）。
- hooks（用户自定义扩展点）即普通订阅者：v1 提供 `on-tool-start` 前置 hook 可返回
  `'block` 实现工具拦截，其余 hook 只读。

### 4.9 repl — 终端交互层

**职责**：读用户输入、渲染流式输出、处理斜杠命令与 Ctrl-C。

**接口**（`repl.rkt`，可执行入口 `main.rkt` 调用）：

```racket
(provide
  (contract-out
    [run-repl! (-> deps? agent-state? void?)]
  ) ; contract-out
) ; provide
```

**功能清单（v1）**

- 流式渲染：订阅 `evt:delta` 逐段输出；thinking 用暗色、text 正常、tool 调用一行摘要。
- 斜杠命令：`/quit` `/clear`（清历史）`/compact`（手动压缩）`/model <id>` `/resume <session>` `/usage`。
- Ctrl-C：一次 = 取消当前轮（`provider-cancel!` + 工具 custodian shutdown），两次 = 退出。
- 多行输入：以 `\` 结尾续行。

### 4.10 subagent — 子 agent

**职责**：把「派生一个独立上下文的 agent 跑子任务」暴露为一个工具。

**原理**：`spawn_agent` 工具 = 用受限 config（子任务 prompt、更少的工具集、独立空 history）
递归调用 `run-turn!`，运行于独立 thread + custodian；父 agent 拿到的 tool-result 是
子 agent 的最终文本。子 agent 的完整 transcript 另存为 `<session>-sub-<n>.jsonl` 供审计。
深度硬限制为 1（子不可再派孙），避免失控。

---

## 5. 各模块实现技术原理

### 5.1 provider：HTTP 与流式请求

- 用 `net/http-client` 的 `http-conn-sendrecv!`（keep-alive 连接复用），POST
  `https://api.anthropic.com/v1/messages`，body 为 `jsexpr->bytes`，请求头含
  `anthropic-version`、`x-api-key`（从 env 读取，绝不入 transcript/日志）。
- 响应 port 直接交给 `sse-pump!`；provider 在独立 thread 上跑 pump，把解析结果
  转换为 `evt:*` 事件塞进返回给调用方的 async-channel。
- **取消**：整个请求 thread 挂在专属 `custodian` 下，`provider-cancel!` =
  `custodian-shutdown-all`，连接与 thread 一并回收 —— 这是 Racket 做资源级取消的惯用法。
- **重试**：仅对 429/5xx/连接错误做指数退避重试（1s, 2s, 4s，上限 3 次），
  429 优先读 `retry-after` 头；已开始产出 delta 后不再重试（避免重复内容）。

### 5.2 stream：SSE 解析

- SSE 协议本质是行协议：`event: <type>\n` + `data: <json>\n` + 空行分隔。
  实现为对 input-port 的 `read-line` 循环 + 一个两字段的小状态机（当前 event 名、
  data 行缓冲），空行即 dispatch。不需要正则，不需要一次性读全响应。
- Anthropic 事件序列 → message 的还原由 accumulator 完成：

```
message_start                     → 建空消息，记 usage.input_tokens
content_block_start (index=k)     → int-map-set! k 处建对应类型的可变累积槽
content_block_delta (index=k)     → 按 delta 类型追加：text_delta / thinking_delta
                                     → string port 追加；input_json_delta → 字符串缓冲
content_block_stop  (index=k)     → 封口；tool-use 槽在此刻 string->jsexpr
message_delta                     → 记 stop_reason 与 usage.output_tokens
message_stop                      → accumulator-finish：int-map 按键序收集为 blocks
```

- 累积槽用 string port（`open-output-string`）做 O(1) 追加，避免字符串反复拼接的 O(n²)。

### 5.3 tool：子进程与文件操作

- **bash 工具**：`(process/ports #f #f 'stdout "/bin/zsh" "-c" cmd)` 合并 stderr 进 stdout；
  独立 thread 泵输出到缓冲，`sync/timeout` 实现超时（默认 120s），超时/取消走
  custodian shutdown 杀进程树。输出 > 30KB 时保留头尾各 15KB，中间替换为
  `f"[truncated {n} bytes]"`。
- **edit_file**：读全文 → `string-contains?` 确认 old-string 唯一（0 次或多次都报错，
  错误信息给模型足够上下文自纠）→ 替换写回。写回用「写临时文件 + `rename-file-or-directory`」
  保证原子性。
- **grep/glob**：优先探测并调用系统 `rg`（快、gitignore-aware），缺失时回退到
  纯 Racket 实现（`in-directory` + `regexp-match?` 逐行扫）。

### 5.4 loop：流收集与工具编排

- `stream-and-collect!`：`sync` 等待 provider 通道，逐事件转发到 bus（供 repl 渲染），
  同时喂 accumulator；收到 `evt:turn-end` 返回完整 message。单点消费、双路分发。
- `execute-calls!`：同一 assistant 消息里的多个 tool-use **串行执行**（v1 决策：
  bash/edit 类工具本质有序，并行收益小、乱序风险大；read-only 并行留作 v2，
  届时用 `thread` + `pvector` 收集即可，接口不变）。
- 每个工具调用包裹 `with-handlers`：工具抛任何异常 → 转成 `is-error? = #t` 的
  tool-result 回填给模型，loop 永不因工具崩溃而中断。

### 5.5 context：token 估算与压缩

- **估算启发式**：按字符类别加权 —— ASCII 字母数字按 ~4 chars/token、CJK 按
  ~1.5 chars/token、空白与标点单独计，对消息结构（role、块边界）加固定开销。
  对英文/中文混合文本误差实测可控制在 ±15% 内，配合 80% 预算边际足够安全。
  实现为对 history pvector 的一次 fold，并对已估算消息用 `int-map`
  （消息索引 → token 数）做 memo —— 历史消息不可变，估值终身有效，每轮只增量估新消息。
- **compact! 的自举**：压缩本身就是一次对 provider 的调用（prompt：「将以下对话
  总结为可继续工作的状态描述，保留：任务目标、已完成事项、关键文件路径、未决问题」），
  产出的 summary 作为一条特殊 user 消息插到被删段的位置。transcript 中记录
  `compact` 事件 + 被替换段的 datum 序号区间，保证有损压缩可审计、可回滚
  （回滚 = 重放时跳过 compact 记录、改用原区间的 msg 记录）。

### 5.6 session / rktd：datum 流的读写与重放

- **写**：`datum-log-append!` = `(pretty-write datum port)` + `flush-output`，
  外加 semaphore 保证多订阅者并发追加时 datum 不交错。prefab struct 与其字段
  （string / symbol / list / hash）均为可 `read` 回的 datum；contract 层用
  `datum-writable?` 守卫，杜绝闭包/port 等不可序列化值混入。
- **读（流式）**：`in-datum-log` 实现为 `(in-port read port)` 的薄包装 —— `read`
  一次恰好消费一个 datum，天然增量：重放 10MB transcript 无需整读，内存占用与
  单条最大记录同阶；`session-list` 读首 datum 即返回。
- **崩溃安全**：写入中断产生的残缺尾 datum 使 `read` 抛 `exn:fail:read`，
  包装层捕获后视为流结束并告警；上游「逐条 flush」保证最多丢最后一条。
- **安全读取**：重放前 `(parameterize ([read-accept-lang #f] [read-accept-reader #f]) ...)`
  关闭 reader 扩展 —— datum 流是纯数据，`read` 不求值，无代码注入面。
- **follow 模式**：`datum-log-follow!` 用「读到 EOF 后 `sync` 于文件变化 / 定时重试」
  实现 tail -f 语义，供 M5 的观测工具（另开终端实时看 agent 在干什么）复用。

### 5.7 repl：终端控制

- 读入：`read-line` 循环；检测 stdin 是否 tty（`terminal-port?`）决定是否输出提示符
  （支持管道输入做一次性问答）。
- Ctrl-C：安装 `exn:break` handler（`with-handlers ([exn:break? ...])` 包住等待逻辑），
  break 到达时对「当前轮的 custodian」shutdown，状态回滚到本轮开始前的 snapshot
  （不可变 state 使回滚 = 保留旧引用，零成本）。
- 渲染：v1 不做完整 markdown 渲染，只做 ANSI 着色（thinking 暗灰、代码块围栏内
  不换行重排、工具行前缀 `⏺`）。f-string 让格式化代码保持可读：
  `f"⏺ {name}({summary}) — {ms}ms"`。

---

## 6. 并发与取消模型

全项目的并发原语只用四样：`thread`、`async-channel`、`sync`、`custodian`。

```
main thread ──── repl 读输入循环
   │
   ├─ turn custodian ──────────────────────────┐
   │    ├─ provider thread（HTTP + SSE pump）  │  Ctrl-C = shutdown 整棵
   │    └─ tool thread（子进程泵）             │
   │                                           │
   ├─ bus 订阅者 threads（渲染 / 落盘 / 日志）─┘  长驻，不随轮回收
```

- **每轮一个 custodian**：轮开始时 `make-custodian`，该轮的一切资源（HTTP 连接、
  子进程、工作 thread）都在其下创建；取消 = 一次 shutdown，无泄漏。
- **背压**：provider → loop 的 async-channel 无界（delta 事件小且有限）；
  bus → 订阅者的通道同样无界但订阅者只做轻活；bash 输出泵有 30KB 截断上限兜底。
- **不变量**：跨 thread 共享的只有不可变数据与 channel；唯一的可变点
  （permission 的 always 缓存、accumulator 内部）都被单 thread 私有或加 semaphore。

---

## 7. 错误处理策略

| 错误类 | 处理 |
|---|---|
| 网络/429/5xx | provider 内重试（5.1），耗尽后 `evt:error` + repl 提示，历史不污染 |
| API 4xx（如超上下文） | 超上下文 → 自动触发 compact! 后重试一次；其余展示错误并回滚本轮 |
| 工具执行异常 | 转 error tool-result 回填模型（5.4），让模型自纠 |
| transcript 损坏 | 重放时逐 datum 容错，残缺尾 datum 告警丢弃（`exn:fail:read`） |
| 用户 Ctrl-C | 轮级 custodian shutdown + 状态回滚（5.7） |
| 不变量违反（bug） | fail fast：contract violation 直接崩溃暴露，不吞 |

原则：**模型可见的错误尽量回填给模型**（它有自纠能力），**用户该知道的走 evt:error**，
**程序员的 bug 用 contract 尽早炸**。

---

## 8. 文件布局

代码与数据分离：源码收敛在 `src/`，运行时产物落 `data/`（transcript）与 `cache/`（跨会话
缓存），二者 git 忽略。`main.rkt` 用 `define-runtime-path` 锚定项目根，故 data/cache 始终
相对项目而非 agent 的目标工作目录。

```
pi2/
├── design.md            ; 本文档
├── README.md
├── run-tests.sh
├── main.rkt             ; 可执行入口：解析命令行 → 装配 deps → run-repl!（锚定 data/、cache/）
├── src/                 ; 全部源码（#lang tstring racket）
│   ├── model.rkt        ; §3 核心数据模型（prefab structs）
│   ├── rktd.rkt         ; §4.6 datum-log 流式读写（session/permission 复用）
│   ├── event.rkt        ; §4.8 事件总线
│   ├── provider.rkt     ; §4.1 LLM 客户端
│   ├── stream.rkt       ; §4.2 SSE 解析 + accumulator
│   ├── tool.rkt         ; §4.3 工具协议与注册表
│   ├── tools/           ; 内置工具，一具一文件
│   │   ├── bash.rkt
│   │   ├── file.rkt     ; read/write/edit
│   │   ├── search.rkt   ; glob/grep
│   │   └── builtin.rkt  ; 内置工具集装配
│   ├── loop.rkt         ; §4.4 主循环
│   ├── context.rkt      ; §4.5 上下文管理
│   ├── session.rkt      ; §4.6 持久化
│   ├── permission.rkt   ; §4.7 权限
│   ├── subagent.rkt     ; §4.10 子 agent
│   └── repl.rkt         ; §4.9 终端交互
├── tests/               ; 每模块一个 <mod>-test.rkt（rackunit）+ *-live-test.rkt
├── data/                ; 运行时：会话 transcript <iso>-<rand>.rktd（git 忽略）
└── cache/               ; 运行时：permissions.rktd 等跨会话缓存（git 忽略）
```

依赖方向（→ = 依赖）：`repl → loop → {context, tool, permission} → provider → stream → model`；
`event`、`session` 被横向引用但自身只依赖 `model`；`rktd` 零依赖，位于最底层。无环。

会话文件：`data/<iso-date>-<4位随机>.rktd`（子 agent transcript 另存，见 §4.10）。

---

## 9. 里程碑

| # | 交付物 | 验收标准 |
|---|---|---|
| M1 | model + provider + stream | 脚本式单轮对话：发消息、流式打印回复（无工具） |
| M2 | tool + loop + permission | 完整 tool-use 循环：能让模型 `read_file` + `bash` 完成一个真实小任务 |
| M3 | repl + rktd + session | 交互式 REPL、Ctrl-C 取消、`.rktd` 会话落盘与流式 `/resume` |
| M4 | context + compact | 长对话自动裁剪/压缩不崩、`/usage` 统计准确 |
| M5 | subagent + hooks | `spawn_agent` 可用；`on-tool-start` 可拦截 |

每个里程碑以 `tests/` 下对应单测 + 一个端到端手工脚本收尾。M1–M2 是内核，
建议先打通再谈体验层。

---

## 10. 实现状态

M1–M5 全部落地并通过验收（Racket v9.2.2 增强版，`#lang tstring racket`）。

- **模块内 f-string**：需 `#lang tstring racket`（`tstring` 语言层包裹指定基础语言，故拿到
  完整 `racket` 而非 `racket/base`；普通 `#lang racket` 下 `f""` 不可用）。嵌套过深、带转义引号
  的 f-string 会触发 reader 报错，此类处改用 `string-append` + 简单插值。
- **API 差异**：实测后端为 OpenAI 兼容协议（LM Studio），故 `provider.rkt` 按 OpenAI
  `chat.completions` 的 `tool_calls` / `reasoning_content` 实现，而非 Anthropic 原生格式；
  §5.2 的 accumulator 三段协议对应到 OpenAI 的 `delta.tool_calls[].index` 分片累积，
  intmap 寻址逻辑不变。
- **持久化修正**：会话落盘改为「每轮结束后持久化 `history` 增量 + usage 增量」，而非订阅
  `evt:message` —— 后者会漏掉 tool-result（内部 user 轮），破坏 resume 时的 tool-use↔result
  配对。见 `repl.rkt` 的 `persist-turn!`。
- **hooks**：`on-tool-start` 拦截做成 loop 的**同步** `pre-tool-hook`（bus 订阅者是异步的，
  无法回传 `'block`），返回字符串即拦截并转 error tool-result。

测试驱动改用原生 `raco test`（自动发现、`-j` 并行、集成 rackunit 汇总与非零退出），
`tests/info.rkt` 的 `test-omit-paths` 把 live 测试排除在离线遍历外；`run-tests.sh` 退化为薄封装。
测试矩阵（`./run-tests.sh [--live]`）：离线单测（含 TUI width/keys/lineedit/e2e/console/sanitize）+ 3 套对
`gemma-4-31b-it@6bit` 的真机验收（流式/工具调用/取消、完整工具循环、子 agent 委派）。端到端实测：
agent 读文件、`edit_file` 修 bug、`bash` 跑 `python3` 验证结果，全链路自主完成。

---

## 11. TUI 抽象层

`src/tui/` 提供一套完整的终端 UI 抽象，替换裸 `read-line`，支持原始模式逐键编辑、
readline 快捷键、Unicode 正确渲染，并把输入源抽象为可脚本化后端以支撑自动化测试。

### 11.1 分层

解耦分层，自底向上、依赖单向；顶层有两个装配：`tui.rkt`（同步单行读）与
`console.rkt`（异步实时控制台），二者共用下方三层：

```
tui.rkt        同步装配：tui-read-line（raw 括入/编辑循环/渲染分发）
console.rkt    全屏异步装配：alt-screen + 自有滚动视口 + 工作动画，整屏重绘、单锁串行化
  ├─ lineedit.rkt   行编辑器：纯状态迁移 (ledit-apply) + 渲染 (ledit-render) 分离
  ├─ sanitize.rkt   不可信文本消毒（剥离 ESC/控制字符，防终端转义注入）
  ├─ terminal.rkt   终端抽象：real（stty raw）/ scripted（脚本化，测试用）
  ├─ keys.rkt       字节流 → 按键事件（CSI/SS3/Alt/UTF-8）
  └─ width.rkt      Unicode 显示宽度（wcwidth 等价）
```

### 11.2 关键设计

- **纯解析器**：`parse-key : input-port → kev` 只消费字节端口，故脚本化字节串与真实
  tty 共用同一套解析。CSI (`ESC[`)、SS3 (`ESCO`)、修饰符 (`ESC[1;5C`=Ctrl-Right)、
  Alt+char、UTF-8 多字节续读全部覆盖；孤立 ESC 用 `byte-ready?` 消歧。
- **编辑逻辑纯函数化**：`(ledit-apply st kev) → (values st* action)` 无 IO，动作码
  `'edit/'submit/'cancel/'eof/'clear-screen/'ignore` 交给上层。因此每个快捷键都能离线断言
  缓冲区与光标，无需真实终端。
- **Unicode 渲染**：光标按**字符**移动，重绘按**显示宽度**定位——`ledit-render` 用
  `string-width-upto` 算出光标目标列，`\r` 回行首后 `\e[{col}C` 右移。prompt 可能含 ANSI
  颜色码，`visible-width` 先 `strip-ansi` 再计宽，保证列数不被转义序列污染。CJK/emoji 双宽、
  组合符零宽均正确。
- **raw 模式的时机**：`tui-read-line` 用 `dynamic-wind` 仅在**编辑期**进 raw，提交/取消后
  立即恢复 cooked。故 agent 执行阶段仍是 cooked 模式，Ctrl-C 依旧走 SIGINT→`exn:break`
  中断当前轮——两种 Ctrl-C 语义（编辑期取消行 vs 执行期中断轮）各得其所。
- **CLI 式输入抽象**：`terminal` 是函数表 struct，`make-scripted-terminal` 用预置按键队列 +
  输出捕获 string port 实现同一接口。`tui-run-scripted` 一行驱动完整编辑会话并返回
  `(values 结果 输出)`——TUI 端到端测试因此完全离线、确定性，无需 pty。

### 11.3 全屏异步控制台（console.rkt）

裸的「读一行 → 跑一轮」有交互缺陷：cooked 回显会把用户按键撞进流式输出；且终端**原生滚屏**
与「底部固定框 + 相对光标重绘」相互干扰，鼠标上翻历史时会错位。`console.rkt` 因此**接管终端
窗口**：进 alternate screen、禁用原生滚屏，以「自有滚动缓存 + 视口」实现滚动，**整屏重绘**。

- **全程 raw + alt screen**：`console-start!` 写 `\e[?1049h`（alt 屏，无原生 scrollback）、
  `\e[?1000h\e[?1006h`（SGR 鼠标上报）、`\e[>4;1m`（modifyOtherKeys）；`console-stop!` 逐一复原
  并 `\e[?1049l` 回主屏（transcript 已落 `.rktd`，退出后终端还原）。
- **整屏布局**（自上而下）：`[内容区(输出视口)] [可选状态行] [分隔线] [命令预览] [输入框(光标)]`。
  内容区高 `Ch = H − 框高`；输入框恒钉底部、光标恒在框内，故 LLM 回显时光标不驻留输出体。
- **帧式写入**：`render-frame` 把整屏组装成一个字符串（`\e[H` + 逐行 `\e[K` + `\r\n` 拼接 + 末尾
  绝对定位光标 `\e[r;cH`），`frame!` 再以 `\e[?25l …帧… \e[?25h` 括起单次写入——整帧原子、
  输出体无光标游走。绝对定位免去相对光标记账，多行输入/命令预览增删皆稳。
- **按显示宽度折行**：`wrap-visual` ANSI 感知地把逻辑行折成视觉行（转义零宽不被拆断、CJK 双宽），
  故超宽输出、CJK 段落正确换行；视口只折「尾部若干逻辑行」，O(Ch) 而非全量。
- **单锁串行化一切写**：reader 线程回显、main/订阅线程流式输出、animator、权限询问都走同一把
  `semaphore`。`console-emit!` 把完整行入环形缓存并更新视觉行计数，`pending` 存未换行尾。
- **自有滚动视口**：`view` = 上滚行数（0=跟随底部）。`keys.rkt` 解析 **SGR 鼠标滚轮**
  （`\e[<64/65…M` → `scroll-up/down`）；`console-handle-key!` 拦截滚轮/`PageUp·PageDown`，
  调 `view` 并 clamp。上滚阅读时新输出**锚定视口**（`view += 新增视觉行`，不拽走用户）；
  提交新输入即 `view←0` 跳回底部。
- **工作动画**：`status` 置标签时，animator 线程每 120ms 推进 braille 转轮并重绘，状态行显示
  `⠋ thinking…`。repl 在开跑前 `statusf #t`（**首 token 前**即示忙），`console-emit!` 一有输出即
  自动清零，工具毕（`evt:tool-end`）再置以覆盖「等下一段模型输出」的空档，turn 末清零。
  同一 animator 每 ~1s 复查终端尺寸，变化即重排（处理 resize）。尺寸经 `clamp-dim` 夹紧，
  退化/零尺寸不致乱。
- **多行输入**：`Shift/Alt+Enter` 插 `\n`（普通 Enter 提交）；`ledit-render` 多行感知。**空输入回车
  不派发**、仅推一空行换行。`keys.rkt` 识别 Shift+Enter 的 CSI-u（`\e[13;2u`）与 modifyOtherKeys
  （`\e[27;2;13~`）两种上报，及 Alt+Enter（`\e\r`）可移植回退；xterm 系经 `modifyOtherKeys=1`
  上报，其余退回 Alt+Enter。
- **Ctrl-C 阶梯**（`handle-cancel!`，**不回显 `^C`**）：**有草稿**→清草稿；**空+运行中**→`break-thread`
  主线程（`parameterize-break` 内 `run-turn!` 收 `exn:break` → `provider-cancel!`，repl 先 `emit "\n"`
  收尾再单起一行给出 `⎯ interrupted ⎯`，**独立元信息块**）；**空+空闲**→无动作。
- **'/' 命令实时预览** + **询问改道**（`console-ask!`）同前。
- **可离线测试**：`render-frame` 系纯函数、脚本终端固定 80×24，可断言帧内容、滚动视口
  （PageUp/滚轮露旧、锚定、提交跳底）、工作动画、多行、Ctrl-C 四态、缓存淘汰等
  （`tests/tui-console-test.rkt`）。

### 11.4 滚动缓存（超长会话）

`console` 内嵌定长**环形缓冲**（`ring`，默认 4000 行）存已提交输出行（带样式），
`vrows` 增量维护视觉行总数供视口 clamp（O(1)，免每帧全量折行）。既是滚动视口的数据源，
也支撑**局部信息提取**：`console-tail-lines` / `/tail [n]` 取最后 n 行（去 ANSI）。

### 11.5 安全：文本格式注入防护（sanitize.rkt）

模型与工具输出是**不可信**的：其中可能夹带终端转义序列，用于改窗口标题(OSC)、清屏、
劫持光标、写剪贴板等——即「文本格式注入」。默认策略：`sanitize-untrusted` 只放行常规可打印
Unicode 与 `\n`/`\t`，移除全部 C0（含 ESC/CR/BEL）、C1、DEL。移除 ESC 即瓦解一切
CSI/SS3/OSC/DCS；移除 CR 防「回车覆盖」改写已落屏内容。消毒作用于渲染层的**不可信文本**
（模型 delta、思考、工具参数摘要、异常信息），我们自己的颜色样式单独添加、不经此函数，
故不误伤。逐 delta 消毒即便转义被拆到多个分片也安全（ESC 一律移除，残留可打印字符无害）。

### 11.6 集成

`repl.rkt` 交互式（真实 tty）走 `console.rkt` 全屏控制台；管道/非交互回退
`read-input/plain`（纯 `read-line`，支持 `\` 续行）。斜杠命令输出、流式渲染、工作动画、权限
询问统一经 `emit`/`statusf` 汇聚（交互期为 `console-emit!`/`console-set-status!`，非交互为 `display`
/`void`），避免绕过控制台锁。真实 tty 经 pty（设 winsize）实测：进/出 alt screen、鼠标上报开启、
**首 token 前工作动画**、CJK 折行、`/` 命令预览、鼠标/PageUp 滚动视口、流式中 Ctrl-C 打断成
独立块（无 `^C`）后 REPL 存活续答、Ctrl-D 干净退出均正确。

### 11.7 系统提示词行为

首条 `system` 消息由 `main.rkt` `DEFAULT-SYSTEM` 提供、装配进 `config-system-prompt`
（`provider.rkt` 读取；`--resume` 沿用存档值）。除工具用法与简洁性外，核心行为约束是
**不确定即查证**：对文件内容/符号/API/项目结构/命令输出/路径存在性有疑时，先用工具核实再
作答，不臆测、不虚构，并优先以证据佐证。pty 实测：对「离线测试用什么命令」这类问题，模型
会先 `read_file`/`bash` 核实再答，而非凭空猜测。

### 11.8 会话选择器（picker.rkt）与恢复 UI

`picker.rkt` 是通用**可选列表控件**，服务会话恢复：纯状态机 `pick-step`（↑/↓/j/k、
Home/End、PageUp/Down、Enter 选中、Esc/q/Ctrl-C 取消）与共享渲染 `pick-render-lines`
（整屏、窗口化保持选中项可见、反显高亮）分离，故完全离线可测。两处复用同一核心：

- **启动选择**（`-i`）：`run-picker` 独立进 raw+alt、循环读键渲染，返回下标或 `#f`。
- **运行中 `/resume`**：`console-pick!` 把选择器做成 console 的一种「模式」——与 `console-ask!`
  同构，reader 线程把键喂给 `pick-step`、`redraw!` 检测到 picker 态即整屏画列表（藏光标），
  选定/取消经 channel 投回主线程。选中后关闭当前会话、以当前 config 重放所选、按追加打开
  并 `set-box!` 切换 `sess-box`，随即渲染**恢复预览**。

**恢复预览**（§ 选择 "last few exchanges"）：`render-resume-preview` 取历史末 3 条经 `emit`
落屏（user→`› …`、assistant→文本、工具→`⏺ name`，每条截断 280 字），并冠以
`── resumed · N messages ──` 与 `…(K earlier)` 省略提示，恢复即见上下文。

`main.rkt` 解析 `-c/--continue`（`session-latest`）、`-i/--pick`（`run-picker`）、`--list`/`--rm`
（即时退出）、`--resume <序号|路径>`（`resolve-source`）、`--fork-at N`（`session-fork!`）
为一个待恢复路径，再走既有「replay + append」流程。pty（设 winsize）实测：`-c` 预览、
`-i`/`\/resume` 选择器、`--fork-at` 分支、`--list`/`--rm`、空会话自动清理均正确。
