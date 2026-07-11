# pi++ 第二轮检视报告 — 代码与功能层对比 pi

> 日期：2026-07-11 · 对比基准：`pi.git`（TypeScript monorepo，只读参考）
> 范围：在插件系统 M1–M5 落地后，重新逐层对比两个项目的**代码质量**与**功能覆盖**。
> 上一轮聚焦扩展/插件层;本轮补齐 agent 主循环、Provider/AI、TUI、会话、工具、权限、CLI、编排八层。

---

## 0. 现状快照

| 指标 | pi++ | 说明 |
|---|---|---|
| 源码 | **5644 行** / 27 个 `.rkt` | src + tools + tui |
| 测试 | **22 文件 / 350 test-case**（169 离线 + 6 live 断言组） | `./run-tests.sh` |
| 最大模块 | `tui/console.rkt` 795 · `repl.rkt` 656 · `plugin.rkt` 453 | |
| 内置工具 | bash / read_file / write_file / edit_file / glob / grep + spawn_agent | 6+1 |
| 插件 | 沙箱 + 能力授权 + `#lang` DSL + 多供应商 + 技能/提示词 | M1–M5 ✅ |
| pi 规模量级 | agent-loop 791 行、agent-session 2000+ 行、9+ provider、orchestrator 独立包 | 数十倍体量 |

**总判断**:pi++ 用 ~5.6k 行 Racket 覆盖了 pi 的**单机交互 agent 核心闭环**,并在**插件隔离与能力授权**这一维度**反超** pi(pi 是全信任、无代码沙箱)。pi 的领先集中在**广度与工程成熟度**——多 provider 原生、会话 DAG、差分 TUI、RPC/编排/MCP。二者定位差异清晰:pi++ 是「精悍可审计的解释型内核 + 安全插件」,pi 是「全功能生产级 IDE 后端」。

---

## 1. 逐层对比

### 1.1 Agent 主循环

| 维度 | pi (`packages/agent/agent-loop.ts`) | pi++ (`src/loop.rkt`, 250 行) |
|---|---|---|
| 结构 | 双层:外层 follow-up 队列 + 内层 steering 注入的流式状态机 | 单层 `run-turn!`:user→模型/工具交替→终止 |
| 工具执行 | 串行 / **并行**(按工具 `executionMode`,完成序发事件) | **仅串行** `execute-calls!` |
| 拦截钩子 | `beforeToolCall`(拦截+改参)/`afterToolCall`(改结果+提前终止) | 插件 `on-tool-call`/`on-tool-result` + `pre-tool-hook` **✅ 对等** |
| 预算 | 依赖 provider 输出 token 上限 | **主动**:`turn-max-calls` 超限注入「预算耗尽,收尾作答」——比 pi 更优雅 |
| 截断工具调用 | 输出 token 截断时整批失败并告知模型 | 无(本地模型少触发) |
| 压缩 | 溢出触发 LLM 总结 | `> 1.5×预算` 触发 `compact!` **✅ 对等** |

**评**:pi++ 主循环短小(250 行 vs 791)、可读性高、钩子链已对齐 pi 的 before/after 语义,且预算耗尽处理**更克制**(注入提示而非硬掐)。**缺** steering(运行中插话)、follow-up 队列、**并行工具执行**——后者是本地多工具场景的实打实吞吐差距。

### 1.2 Provider / AI 层

| 维度 | pi (`packages/ai`, 9+ 家) | pi++ (`src/provider.rkt` 272 行) |
|---|---|---|
| 供应商 | Anthropic/OpenAI/Google/Bedrock/Mistral… 各自归一化 | **仅 OpenAI 兼容 SSE**;插件可 `register-provider!` |
| 运行时切换 | 模型环 Ctrl+P | `current-config` parameter + dispatch-provider,`/model`+`/provider` **✅ 已修复即时生效** |
| 思考/推理 | 原生 adaptive thinking / `reasoning_effort` / gemini thinkingLevel | ❌ 无 |
| 提示词缓存 | Anthropic `cache_control`、OpenAI ephemeral、成本追踪 | ❌ 无 |
| Token/成本 | 各家 usage 精确回读 + 分级计价 | **本地启发式估算**(CJK 加权 + intmap memo),usage 取 API 回传 |
| 认证 | OAuth device-code、AWS profile、Google ADC | 仅 Bearer key(env) |
| 传输 | SSE / WebSocket / cached-WS | 仅 SSE |
| 重试 | 指数退避 + 上限 | 退避 `(1 2 4)`,首个 delta 后不重试(防重复)**✅ 语义正确** |

**评**:这是**最大功能落差**层。pi++ 靠「OpenAI 兼容」一把梭覆盖了 LM Studio/多数网关,工程上务实;但缺原生 Anthropic/Google 意味着拿不到 thinking、prompt-cache、精确成本。retry 的「首 delta 后不重试」和 custodian 取消是干净的实现,质量不输 pi。

### 1.3 TUI / 渲染

| 维度 | pi (`packages/tui`) | pi++ (`src/tui/*`, 6 模块 ~1.6k 行) |
|---|---|---|
| 渲染 | **差分渲染**:组件缓存行态,仅重绘变更 | **整帧重绘**:`\e[H` + 每行 `\e[K` + `\e[J`,底部固定框 |
| 布局 | 组件模型 + overlay 锚点定位 | 转录走**普通滚动流**,仅底部输入/spinner 框重绘 |
| 图像 | Kitty graphics 协议 | ❌ 无 |
| 编辑 | Emacs 式 undo/kill-ring/词导航 | `lineedit.rkt`(312 行),基础行编辑 |
| 宽度 | `visibleWidth` emoji/CJK | `tui/width.rkt`(108 行)CJK 感知 **✅ 对等** |
| 可测性 | — | **scripted-terminal 离线 80×24 快照测试**——纯函数拼帧,pi 无此设计 |

**评**:两种哲学。pi 是**全屏应用**(差分渲染,适合复杂布局/图像);pi++ 是**行内 REPL + 底部单框**(整帧原子写,牺牲复杂布局换来「纯函数渲染 + 离线可测」)。对 CLI agent 而言 pi++ 的取舍合理且**测试性更强**。差距:图像、富文本编辑、多 overlay。

### 1.4 会话 / 历史模型

| 维度 | pi (JSONL DAG) | pi++ (`src/session.rkt` 240 行, `.rktd`) |
|---|---|---|
| 格式 | JSONL,每 entry 带 `parentId` | **prefab datum-log**,追加式 |
| 结构 | **真 DAG 树**:分支/leaf/getPathToRoot | **线性**;`session-fork!` 在第 N 条**另存新文件** |
| 重放 | buildSessionContext + projector | `session-replay` 复用运行时 `state-append` **✅ 优雅:真相源即 transcript** |
| 分支导航 | leaf 切换、branch-summary、clone | 无树内导航(fork 即扁平复制) |
| 元信息 | name/cwd/model 历史 | `session-info` 列表(时间/model/标题/条数)+ pick/continue/rm |
| 向前兼容 | 版本迁移 | 未知 rec 类型跳过 **✅ 有意设计** |

**评**:pi++ 的「transcript 即真相、重放复用同一 `state-append` 迁移」是**架构亮点**——零重复逻辑。但会话是**线性**的:fork 生成独立文件而非树内分支,缺 pi 的 DAG 导航/branch-summary。对单人 CLI 够用,对「探索式多分支」弱。

### 1.5 工具

| 维度 | pi | pi++ (`src/tools/*`) |
|---|---|---|
| 内置 | read/bash/edit/write/grep/find/ls | bash/read/write/edit/glob/grep **✅ 覆盖核心** |
| 校验 | Typebox schema | `function-spec` + `input-str/int/ref` 手工取参 |
| 流式输出 | 工具执行中 `onUpdate` 增量 | ❌ 无(工具同步返回) |
| 图像内容 | TextContent+ImageContent | 仅文本 |
| 并发保护 | 文件变更队列防竞态 | 无(串行执行天然无竞态) |
| 子代理 | 见编排层 | `spawn_agent`(depth-1,独立 custodian,事件转发父 bus)**✅ 有** |
| MCP | 支持 | ❌ 无 |
| web/todo | 有 | ❌ 无 |

**评**:核心文件/shell/搜索工具齐备且质量相当。`spawn_agent` 的「受限工具集(剔除自身)+ 独立 custodian + 事件转发」实现干净。**缺**:流式工具输出、图像、web、todo、MCP 桥接。

### 1.6 权限 / 安全 —— **pi++ 反超**

| 维度 | pi | pi++ (`src/permission.rkt` + `src/plugin.rkt`) |
|---|---|---|
| 工具门控 | allowlist/denylist + cwd 限定 + `beforeToolCall` + 项目 trust | 3 模式(strict/normal/yolo)× 权限级矩阵 |
| 记忆 | — | `always` 持久化到 `.rktd` **✅** |
| 拒绝语义 | block+reason | **deny + reason 回传模型**「勿重试,调整策略」**✅ 更细** |
| 细粒度能力 | **无**(工具要么可用要么不可用) | **插件能力模型**:清单声明 `caps` → `racket/sandbox` 的 `sandbox-path-permissions` 强制 fs-write 边界;内存/时间限额;custodian 隔离 |
| 沙箱 | **无代码沙箱**(全信任 in-process) | **有**:不可信插件 `make-module-evaluator`,超时/超内存自动 kill 而宿主存活 |

**评**:**pi++ 在隔离与最小授权上明确领先**。pi 明确文档承认「无细粒度 ACL、依赖外部容器」;pi++ 用 Racket 的 sandbox/custodian/security-guard 把「不可信第三方插件」变成一等公民。这是 pi++ 相对 pi 的**核心差异化**,也是 Racket 解释型运行时的价值兑现。

### 1.7 CLI / 入口

| 模式/能力 | pi | pi++ (`main.rkt`) |
|---|---|---|
| 交互 | ✅ | ✅ |
| 一次性 print | `--print`/`--json` | `-p/--prompt` 单发 **✅ 部分** |
| **RPC/headless** | ✅ JSONL stdin/stdout,30+ 命令 | ❌ 无 |
| 会话 | `--session` + 全局搜索 | `-c/--continue`/`--resume`/`--list`/`--pick`/`--fork-at`/`--rm` **✅ 丰富** |
| 配置子命令 | get/set/list | ❌(靠 flag) |
| 导出 | `--export html` | ❌ |
| 插件/供应商 | `-ne`/`-nt` | `--plugins`/`--trust-plugins`/`--provider` **✅** |

**评**:交互 + 会话管理 CLI 已很完整(fork-at/pick/rm 甚至更顺手)。**缺** RPC/JSON 结构化输出——这是「被 IDE/编排器驱动」的前提,也是下一节路线图首选。

### 1.8 子代理 / 编排

| 维度 | pi (`packages/orchestrator`) | pi++ |
|---|---|---|
| 模型 | 多**进程** supervisor,spawn `pi --mode rpc` | 单进程内 `spawn_agent`(depth-1) |
| IPC | JSON over stdio,30+ RpcCommand | 父子 bus 事件转发(进程内) |
| 并行实例 | ✅ 跨 cwd、跨机(Radius mesh) | ❌ |
| 生命周期 | 启动/在线/停止/错误 + 崩溃回收 | custodian 关闭 |

**评**:pi 的多进程编排是**生产级分布式**能力,pi++ 无对应物,也**不是当前定位所需**(单机 agent)。`spawn_agent` 满足「委派一个聚焦子任务」的 80% 场景。

---

## 2. pi++ 领先/持平之处(勿丢失)

1. **安全插件运行时**(1.6):sandbox + 能力授权 + custodian —— pi 结构性缺失。
2. **主循环的预算耗尽处理**(1.1):注入收尾提示而非硬掐。
3. **会话「transcript 即真相 + 复用 state-append 重放」**(1.4):零重复迁移逻辑。
4. **TUI 纯函数整帧 + 离线快照测试**(1.3):pi 的差分渲染换不到这份可测性。
5. **拒绝附理由回传模型 + always 持久化**(1.6)。
6. **`current-config` parameter 化的运行时切换**(1.2):`/model`、`/provider` 即时生效,实现干净。
7. **`#lang pi/plugin` DSL 零加载器改动**(M5):Racket 语言拓展兑现。
8. **CJK token 估算 + intmap memo**(1.2/1.4):不可变历史下估值终身缓存。

---

## 3. 剩余差距(按「投入产出比」排序)

| # | 差距 | 层 | 价值 | 成本 | 建议 |
|---|---|---|---|---|---|
| 1 | ✅ **并行工具执行**(已实现) | 1.1 | 高(多工具轮吞吐) | 中 | 串行预检 + 整批 read-only 并发,结果按序归位;见 `loop.rkt` |
| 2 | ✅ **RPC/JSON 模式**(已实现) | 1.7 | 高(可被编排/IDE 驱动) | 中 | `--rpc` NDJSON,复用 bus/`run-turn!`,零内核改动;见 `rpc.rkt` |
| 3 | ✅ **多供应商 + 原生 Anthropic**(已实现) | 1.2 | 高 | 高 | lmstudio 默认 + openai/anthropic/gemini/grok 档案;见 `providers.rkt`/`provider-anthropic.rkt` |
| 3b | ✅ **prompt-cache**(已实现) / thinking(待办) | 1.2 | 高 | 中 | Anthropic `cache_control` 已打(system+tools+末消息断点)+ tools 前缀字节稳定;adaptive thinking 待接 |
| 4 | **流式工具输出** | 1.5 | 中(长命令实时反馈) | 中(`tool-ctx` 已有 `publish!`,扩 `onUpdate`) | 复用现有 bus |
| 5 | **会话树/分支导航** | 1.4 | 中(探索式工作流) | 高(`.rktd` 加 `parentId` + 导航 API) | 定位相关,谨慎 |
| 6 | steering(运行中插话) | 1.1 | 中 | 中 | 需输入线程与 loop 解耦 |
| 7 | MCP 桥接 | 1.5 | 中(生态) | 高 | 可作为一种插件供应商 |
| 8 | 富编辑(undo/kill-ring)/图像 | 1.3 | 低 | 中 | 体验打磨,非阻塞 |

---

## 4. 结论

pi++ 已是一个**功能自洽、可审计、测试覆盖扎实**(350 用例)的单机 LLM agent,核心闭环(流式、工具、权限、会话、压缩、子代理)与 pi 质量相当,并在**插件隔离/能力授权**上**超越** pi。相对 pi 的差距全部落在**广度与集成面**——多 provider 原生、RPC 编排、会话 DAG、差分 TUI——这些是「生产级 IDE 后端 vs 精悍解释型内核」的定位差,而非质量差。

若继续投入,**三步走**收益最高:
1. **并行工具执行**(#1)—— 小改动、直接吞吐,最贴合 Racket 并发原语;
2. **RPC/JSON 模式**(#2)—— 解锁「pi++ 被外部驱动」,是通往编排的门票;
3. **provider 抽象接缝 + 原生 Anthropic**(#3)—— 补上 thinking/prompt-cache 这块唯一的硬功能落差。

余者(会话树、steering、MCP、富 TUI)按实际使用场景再定,均非当前定位的阻塞项。
