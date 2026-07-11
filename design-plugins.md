# pi++ 插件运行时设计（对标 pi，利用 Racket 解释/拓展能力）

本文档规划 pi++ 的**插件（扩展）运行时**：先对标 `pi`（earendil-works/pi-mono，
TypeScript 自扩展 coding agent）梳理能力差距，再给出充分利用 **Racket runtime 的解释与
语言拓展能力**的插件运行时设计与分阶段实施方案。配套最小可运行原型见 `src/plugin.rkt`
与 `plugins/`、`tests/plugin-test.rkt`。

---

## 1. 能力差距：pi2（现状） vs pi（参照）

pi 的核心是一个**自扩展**agent：扩展是运行时用 `jiti`（TS 运行时解释器）动态载入的 TS 模块，
默认导出 `(pi: ExtensionAPI) => void`，**进程内、全信任**运行，通过 `virtualModules` 注入宿主 API。

| 维度 | pi2（现状） | pi（参照） | 差距 |
|---|---|---|---|
| **插件系统** | 无 | 运行时载入 TS 模块，`ExtensionAPI` 30+ 事件 + 注册面 | ★ 核心缺口 |
| **工具扩展** | 仅内置（bash/file/search/spawn） | `registerTool`（TypeBox schema + execute + 自渲染）+ 同名覆盖 | 无第三方工具 |
| **命令/键位/Flag** | 内置斜杠命令、键位固定 | `registerCommand/Shortcut/Flag` | 不可扩展 |
| **生命周期钩子** | 事件总线仅渲染观测（6 个 `evt:*`）+ 单个 `pre-tool-hook` | ~30 事件；含**变换型**钩子：`context`（改消息）、`tool_call`（拦截/改参）、`tool_result`（改结果）、`before_agent_start`（注入）、`input`/`user_bash`（改输入）、`before_provider_request/headers` | 无变换型钩子链 |
| **LLM 供应商** | 单一 OpenAI 兼容 | `registerProvider`（多供应商、OAuth、自定义流式） | 单供应商 |
| **UI 扩展** | 固定 TUI | `ctx.ui`：select/confirm/input/notify/setStatus/setWidget/setFooter/custom overlay/editor/theme | 不可扩展 UI |
| **消息渲染** | 固定 | `registerMessageRenderer/EntryRenderer` | 无自定义渲染 |
| **技能/提示词** | 无 | `.pi/skills`、`.pi/prompts`（YAML front-matter markdown），`resources_discover` | 无 |
| **信任/隔离** | 无（无插件即无问题） | `project_trust` 询问；但**扩展代码本身全信任、进程内、无沙箱**（README 明示无权限系统，需靠容器化） | pi 的短板→pi2 的机会 |
| **会话** | resume/fork/list/prune（已具备） | session tree/navigate/switch + 更细事件 | 部分领先 |
| **其它** | 权限选框+理由、进度动画、消毒、滚动等 TUI 强项 | — | pi2 领先项 |

**结论**：pi2 缺的核心是「插件运行时」——工具/命令/钩子/供应商的第三方扩展面。pi 在此成熟，
但其扩展**全信任、无代码沙箱**（安全边界靠外部容器）。这正是 pi2 用 Racket 可以**做得更好**的地方。

---

## 2. 为什么 Racket 特别适合做插件运行时

pi 用 `jiti` 在运行时解释 TS——这是「宿主语言即插件语言」的动态载入。Racket 天生就是围绕
**运行时解释、语言拓展、安全求值**构建的，可把 pi 的模型做得更干净、更安全：

1. **`dynamic-require`** — 运行时按路径载入一个 `.rkt` 模块并取其 provide。原生、无需转译
   （Racket 本身就是运行时），是 jiti 的对位物且更轻。
2. **`racket/sandbox`（`make-module-evaluator`/`make-evaluator`）** — 这是 pi **没有**的能力：
   在受限求值器里运行**不可信插件代码**，可限
   - 内存（`sandbox-memory-limit`）、CPU/墙钟（`sandbox-eval-limit`）
   - 文件系统（`sandbox-path-permissions`，默认仅工作区）
   - 网络（默认拒绝，`sandbox-network-guard`）、子进程
   并以 **custodian** 统一回收（卸载/重载时一键关停插件的线程/端口/文件）。
   → pi2 可提供**能力受限、可沙箱**的插件，安全性优于 pi 的「全信任 + 外部容器」。
3. **宏 / `#lang` reader 扩展** — 插件可以是**声明式 DSL**。`#lang pi/plugin` 把清单式表单
   展开为注册调用，插件即「小语言」——这是 Racket 面向语言编程的独门能力，pi（TS）无法优雅做到：
   ```racket
   #lang pi/plugin
   (deftool weather #:desc "查天气" #:params ([city string])
     (lambda (in ctx) (ok (fetch-weather (input-str in 'city)))))
   (defcommand "/weather" #:desc "..." (lambda (args ctx) ...))
   (on 'tool-start (lambda (e) ...))
   (needs 'network)                       ; 声明所需能力，供宿主授权
   ```
4. **契约（`racket/contract`）** — 插件↔宿主 API 边界用契约把守；插件行为不合约即得到
   **blame 指名插件**的清晰错误，而非宿主崩溃。
5. **custodian / thread / parameterize** — 每插件独立 custodian，卸载即回收其一切资源；
   插件回调在受控 parameterization 下运行。
6. **`gen:` 泛型 + prefab 结构** — 宿主 API 以一等值传入；工具已是 `gen:tool`；清单用 `.rktd`。
7. **命名空间隔离** — 沙箱插件的命名空间只 attach 白名单模块，杜绝 `require` 任意模块。
8. **热重载** — 沙箱模式天然可弃旧求值器建新求值器；`dynamic-require` 可配合命名空间重载。

---

## 3. 插件模型

### 3.1 两种编写形态（都被支持）

- **命令式**：`.rkt` 模块 `provide` 一个 `plugin` 值 = 注册函数 `(-> plugin-api void)`：
  ```racket
  #lang racket/base
  (require (file "…/plugin-sdk.rkt"))
  (provide plugin)
  (define (plugin api)
    (register-tool! api (make-tool …))
    (register-command! api "/hi" …)
    (on! api 'tool-start (lambda (e) …)))
  ```
- **声明式 DSL**：`#lang pi/plugin`（reader+宏）把 `deftool/defcommand/on/needs` 展开为
  上面的 `plugin` 函数。零样板，且清单（工具名/参数/能力）**可静态提取**（无需执行插件即可列出）。

### 3.2 清单（manifest）

每插件一个 `plugin.rktd`（prefab，与 pi2 数据格式一致）声明元信息与**所需能力**：
```racket
#s(plugin-manifest
   "weather"            ; name
   "0.1.0"              ; version
   "查询天气的工具"       ; description
   (network)            ; capabilities: fs-read fs-write network exec
   "trusted"|"sandbox"  ; 期望隔离级别（宿主可提升）
   "main.rkt")          ; entry
```
声明式插件可从源码提取清单；命令式插件用旁置 `plugin.rktd`。

---

## 4. 扩展点分类（映射到 pi2 现有缝）

`plugin-api` 是传给插件的一等结构，聚合下列注册面与 `ctx`。括号内为 pi2 集成缝。

### 4.1 工具（→ 可变 registry）
`register-tool! api tool` 把一个 `gen:tool` 加入**可变工具注册表**。pi2 现 `registry` 不可变
（`main.rkt` 一次装配）；引入 `mutable-registry`（`box` 工具表 + 覆盖表），`registry-lookup`/
`registry-specs` 走可变视图。支持**同名覆盖**内置工具（如给 `bash` 加审计）。沙箱插件的
`tool-run` 在其求值器内执行（带内存/时限）。

### 4.2 命令 / 键位 / Flag（→ repl COMMANDS + console）
`register-command! api name spec`、`register-shortcut!`、`register-flag!`。命令并入 repl 的
`COMMANDS` 表（现已驱动 `/help`、Tab 补全、实时预览），键位并入 console 的按键分发。

### 4.3 钩子（→ 事件总线 + loop）
两类：
- **观测型**（只读）：订阅现有 `bus` 的 `evt:*`（delta/message/tool-start/tool-end/turn-end/error）。
  已支持，插件即一个 `bus-subscribe!`。
- **变换型**（可改流程）：loop 在关键点**顺序咨询**插件钩子链，取首个非 `#f` 结果或叠加变换。
  新增钩子点（对标 pi）：
  | pi2 钩子 | 触发点 | 返回/作用 | 对标 pi |
  |---|---|---|---|
  | `on-user-input` | 提交行入 loop 前 | 改写/取消输入 | `input` |
  | `before-turn` | `run-turn!` 开头 | 注入一条上下文消息 | `before_agent_start` |
  | `on-context` | `context-fit` 后、发送前 | 过滤/改写发送窗口 | `context` |
  | `on-tool-call` | 工具执行前 | `#f` 放行 / `(deny reason)` 拦截 / 改参 | `tool_call`（泛化现 `pre-tool-hook`） |
  | `on-tool-result` | 工具执行后 | 改写结果内容/错误标志 | `tool_result` |
  | `on-turn-end` | 一轮末 | 观测/持久化 | `turn_end` |
  现有 `deps-pre-tool-hook`（单个、同步）泛化为 `on-tool-call` 钩子链。

### 4.4 供应商（→ provider 抽象，阶段 4）
`register-provider! api name config`。pi2 现为单一 OpenAI 兼容；引入 provider 按名查找 +
插件可提供 `provider-stream!` 实现（自定义流式/鉴权/OAuth）。这是较大改造，单列阶段。

### 4.5 UI / ctx（→ console）
`ctx` 暴露：`notify`、`set-status!`（对接 console 状态行）、`set-widget!`、`select`/`confirm`/
`input`（复用 `console-choose!`/`console-ask!`）、`session`（只读会话访问）、`send-message!`、
`get/set-active-tools`、`compact!`。非交互（管道）模式给出降级实现。

### 4.6 渲染（阶段 5）
`register-renderer! customType proc` 自定义消息/条目渲染，接入 console 的 emit。

---

## 5. 加载、隔离与安全（pi2 的差异化优势）

### 5.1 发现
- 项目级：`./plugins/`（每插件一个 `.rkt` 或含 `plugin.rktd`+entry 的目录）。
- 全局级：`~/.pi2/plugins/`。
- 配置级：config 中显式列出的路径。
（对标 pi 的 `.pi/extensions/` + 全局 + settings。）

### 5.2 两级隔离
- **trusted（受信）**：项目自带或用户加信的插件，`dynamic-require` 直载，全权（同 pi 的默认）。
- **sandbox（沙箱）**：第三方/不可信插件，`make-module-evaluator` 载入：
  - `sandbox-memory-limit`（如 64MB）、`sandbox-eval-limit`（如 5s/20MB 每次调用）
  - `sandbox-path-permissions`：默认仅工作区可读，写/网络/子进程按**能力授权**放开
  - 独立 **custodian**：卸载/重载/超限即 `custodian-shutdown-all` 回收
  沙箱插件采用**声明式导出**（`manifest` 数据 + `tool-run`/`hook` 过程），宿主经求值器
  跨界调用（每次调用受限），杜绝插件直接触碰宿主闭包与全局。

### 5.3 能力授权（复用 pi2 权限体系）
插件清单声明 `capabilities`（fs-read/fs-write/network/exec）。首次加载：
- **项目信任**：对项目级插件先弹**选框**询问是否信任本项目插件（对标 pi 的 `project_trust`）。
- **能力授予**：逐项用**权限选框**（`console-choose!`：允许一次/永久允许/拒绝/拒绝+理由）
  授权，决定持久化到 `cache/plugin-grants.rktd`（复用 permission 的 always 记忆机制）。
未授予能力在沙箱层被 `sandbox-path-permissions`/`sandbox-network-guard` 硬性拒绝——
**声明 + 沙箱双保险**，优于 pi 的「询问信任但代码仍全权」。

### 5.4 生命周期与热重载
- 加载：发现 → 读清单 → 授权 → 建 custodian → (trusted: `dynamic-require`; sandbox: 建求值器)
  → 调 `plugin`/读导出注册。
- 卸载/`/reload`：发 `session-shutdown` 类事件 → 注销其工具/命令/钩子 → `custodian-shutdown-all`
  → （沙箱）弃求值器。热重载即重跑加载，天然干净。
- **陈旧上下文防护**：插件回调持有的 `ctx` 在会话切换后置为 stale，调用即报错（对标 pi）。

---

## 6. 与现有架构的集成缝（改动点）

| 现有 | 改动 | 目的 |
|---|---|---|
| `tool.rkt` `registry`（不可变） | 增 `mutable-registry`（box + 覆盖表）；`registry-lookup/specs/tools` 走可变视图 | 插件注册/覆盖工具 |
| `loop.rkt` `deps-pre-tool-hook`（单个） | 泛化为**钩子链**：`on-tool-call`/`on-tool-result`/`on-context`/`before-turn` 在 `run-turn!`/`execute-one-call`/`stream-and-collect!` 处顺序咨询 | 变换型钩子 |
| `event.rkt` `bus` | 不变；观测型钩子即订阅 | 观测钩子 |
| `repl.rkt` `COMMANDS` | 由静态表变为「内置 + 插件命令」合并视图 | 命令扩展 |
| `console.rkt` | 暴露 `ctx` 所需 UI 原语（多已具备：`console-choose!`/`console-ask!`/`console-set-status!`/emit） | UI 扩展 |
| `main.rkt` | 装配阶段发现并加载插件，把 `plugin-host` 接入 registry/deps/repl | 接线 |
| `provider.rkt` | （阶段 4）按名查找 + 插件供应商 | 多供应商 |

均为**加法**，不破坏现有测试。

---

## 7. 分阶段实施（状态）

- **M1 运行时核心** ✅：`plugin.rkt`——`plugin-api`、trusted 载入（`dynamic-require`）、sandbox 载入
  （`make-module-evaluator` + 内存/时限 + custodian）、统一**可变 registry**（`tool.rkt` 改为可变哈希，
  读 API 不变）、卸载回收。
- **M2 钩子链** ✅：`hook-block`/`hook-replace` + 运行器 `run-tool-call/result/before-turn/context-hooks`；
  `loop.rkt` 在 `execute-one-call`/`run-turn!` 顺序咨询（`deps` 携 `plugin-host`）；观测钩子经
  `make-host-observer` 接 bus。泛化了原 `pre-tool-hook`（保留兼容）。
- **M3 发现 + 集成 + 命令** ✅：`load-plugins-dir!` 按文件名约定（`*-sandbox.rkt`→sandbox）发现载入；
  `main.rkt` 自动载入 `./plugins/` + `--plugins <dir>`（可重复），host 与 deps 共享 registry、订阅
  observer；插件命令并入 repl（`/help`、实时预览、Tab 补全、分派），`ctx.notify`/`ctx.session` 接通。
- **M3+ 能力授权** ✅：`grants`（`cache/plugin-grants.rktd` 持久化，语义同权限体系 yes/always/no）；
  受信插件过**信任门**（`gated-load-trusted!`），沙箱插件按旁置 `<base>.rktd` 的 `(caps …)` 声明过
  **能力门**（`gated-load-sandbox!`）；`--trust-plugins` 一键授予，非交互默认拒绝。已授予能力经
  `caps->path-permissions` 放开沙箱权限——**`fs-write` 落地并强制**（未授予则沙箱硬拒写）。
  网络/exec 能力已可声明/授权，其沙箱强制（自定义 security-guard）留待后续。
- **M4 多供应商 + 命令/键位/UI** ✅：**`register-provider!`**——host 存 name→工厂(config→provider)，
  `main.rkt` 按 `--provider <name>` 解析（插件工厂或内置 openai），故插件可接入自定义 LLM；SDK
  `make-simple-provider` 让写供应商只需一个 reply 函数（自动发 delta/message/turn-end）。**`ctx.ui`**——
  `ctx.notify/select/confirm` 经 host 注入接 console（`console-choose!`/emit），`ctx.session` 读当前状态。
  **`register-shortcut!` 已接 console**：console 新增 `#:shortcut` 回调，`console-handle-key!` 在选框/
  选择器模式之后、编辑器之前咨询——命中（kev 结构相等匹配，`kchar`/`knamed` 构造）即执行插件 thunk
  并吞键。
  **运行时切换已修**：新增 `current-config` parameter（`model.rkt`），`run-turn!`/`compact!` 每次调用前
  `parameterize` 之，内置 provider 改读 `(or (current-config) cfg)`——故 `/model`（及 endpoint/key）
  运行时切换即时生效（原先 provider 闭包创建 cfg，切换无效）。**运行时切换 provider 已做**：
  `make-dispatch-provider`——deps-provider 是分发器，每请求按 host `provider-sel` 选用名解析真实
  provider（惰性构建 + 缓存），委派 stream!/cancel!；`/provider [name]` 列出/切换（校验），
  `--provider` 设初始选用。故 `/model` 与 `/provider` 皆运行时即时生效。
- **M5 声明式 DSL + 技能/提示词** ✅：**`#lang pi/plugin`**（`src/pi-plugin-lang.rkt`）——自定义
  `#%module-begin` 把主体包成 `plugin` 注册函数，`deftool`/`defcommand`/`on`/`defprovider`/`defshortcut`
  宏在 `current-plugin-api` 参数化下展开;SDK 与 racket/base 已由语言导出,插件无样板、无 require;
  用 `#lang s-exp "…/pi-plugin-lang.rkt"` 即可(与既有加载器**零改动**——仍是 provide plugin 的受信插件)。
  **资源发现**（`src/resources.rkt`）:读带 YAML 前置元数据的 markdown;`skills/*.md` 名称/描述**渐进
  披露**进系统提示词(模型按需 read_file 全文),`prompts/*.md` 经 `/prompt <name>` 激活(正文追加进
  系统提示词);`/skills`、`/prompt` 列出。

---

## 8. 实现状态与验证

**已交付**：`src/plugin.rkt`（运行时）、`tool.rkt` 可变 registry、`loop.rkt` 钩子链、`main.rkt` 发现/接线、
`repl.rkt` 命令分派、示例插件 `examples/plugins/{echo-tool,hello-command,calc-sandbox,runaway-sandbox}.rkt`、
`plugins/`（用户项目插件目录，自动载入）、`tests/plugin-test.rkt`（11 项）。

**离线测试**（`raco test`）：受信/沙箱载入、**失控插件被资源限额关停而宿主存活**、卸载、发现；
钩子运行器；**经 `run-turn!` 端到端**：插件工具被模型调用、`on-tool-call` 拦截/改参、`on-tool-result`
改结果、`before-turn` 注入——全过。

**能力授权测试**：`read-plugin-caps` 读旁置清单；**沙箱 `fs-write` 默认拒、授予后可写**；
grants `always` 持久化并重载（一次性项不持久）；信任门拒→不加载、`always`→加载并持久；沙箱按声明
能力逐项询问。

**真机（pty）验证**：① `--plugins examples/plugins`——`/help` 列出 `/hello`、`/hello` 经 `ctx.notify`
显示、模型成功调用插件 `echo`（回显 `pluginworks`）；② `--plugins … --trust-plugins`——插件静默加载
（不卡信任提示）、沙箱 `writer`（授予 `fs-write`）经模型调用**真实写入**了文件。

**多供应商测试**：插件 `register-provider!` 注册 `echollm`；分发器按 host 选用名委派，`/provider`
运行时切换（校验未知名不改）后 `run-turn!` 用新供应商产出回复；`current-config` 切换测试证明
provider 每轮读当前 model；`ctx.select`/`confirm` 经 host 注入正确路由。pty：`--provider echollm`
用插件供应商产出（`echo-llm reply: …` 上屏）；默认 openai 经**分发器**仍能真机作答（7+8=15）；
`/provider echollm` **运行时切换**后回复即由插件供应商产出；**Ctrl-G** 触发插件 `register-shortcut!`
的处理器（notify 上屏）。

**M1–M5 全部落地并测试**。DSL 测试：`dsl-demo.rkt`（`#lang pi/plugin`）注册工具/命令/键位/钩子且
工具可执行、捕获局部 define。资源测试：前置元数据解析、`read-resource`、`skills-addendum` 渐进披露。
pty：`/skills` 列出 web-search、`/prompt reviewer` 激活、DSL 插件命令 `/dsl` 生效。
