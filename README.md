# pi++

基于 Racket 的 LLM agent。终端内运行，支持流式输出、工具调用循环、`.rktd` 原生会话持久化、
上下文压缩、权限门控与子 agent。设计文档见 [design.md](design.md)。

## 依赖

- 增强版 Racket（源码用 `#lang tstring racket`，含 `f""` 字符串模板、`pvector`、`racket/intmap`）。
- 一个 OpenAI 兼容的 LLM 端点。默认指向本地 LM Studio (`http://localhost:1234/v1`)，
  默认模型 `gemma-4-31b-it@6bit`。

## 安装（raco pkg）

本项目是**单集合包**（collection `pi2`）。在项目目录内安装后，即可从任意目录用 `racket -l pi2` 运行：

```sh
# 在项目根目录安装（--link 原地链接，改代码即时生效）
raco pkg install --link

# 运行（等价旧 `racket main.rkt`）；传参用 `--` 分隔 racket 与程序参数
racket -l pi2                                   # 交互式
racket -l pi2 -- -p "hello" --mode yolo         # 单次问答
racket -l pi2 -- --provider deepseek --list-keys

# 安装同时生成 `pi2` 可执行入口（在 racket 的 bin 目录），可直接：
pi2 --provider deepseek                         # 无需 `--` 分隔
pi2 -p "reply ok" --mode yolo

# 卸载
raco pkg remove pi2
```

- 依赖 `base` + `tstring`（`#lang tstring racket` 语言）；`net`/`racket/sandbox`/`pvector`/`intmap` 由本机增强版 racket 核心提供。
- 资源（skills/prompts/plugins）随包只读读取；会话/缓存默认落项目根 `data/`、`cache/`，
  从**只读安装位**运行时用 `PI_DATA_HOME` / `PI_CACHE_HOME`（配合 `PI_CONFIG_HOME`）导到用户可写目录。

## 运行（源码目录内，免安装）

```sh
# 交互式会话（当前目录为工作区）
racket main.rkt

# 指定模型 / 端点 / 权限模式
racket main.rkt -m gemma-4-31b-it@6bit -e http://localhost:1234/v1 --mode normal

# 单次问答（管道友好）
racket main.rkt --mode yolo -p "read config.rkt and summarize it"

# 切换 LLM 供应商（默认本地 lmstudio；云端读对应环境变量密钥）
racket main.rkt --provider anthropic     # 需 ANTHROPIC_API_KEY（原生 Messages 线路）
racket main.rkt --provider openai        # 需 OPENAI_API_KEY
racket main.rkt --provider gemini        # 需 GEMINI_API_KEY
racket main.rkt --provider grok          # 需 XAI_API_KEY
racket main.rkt --provider deepseek      # 需 DEEPSEEK_API_KEY（Anthropic 兼容端点）

# 密钥管理（存 ~/.config/pi++/credentials.rktd，权限 0600；env 优先于文件）
racket main.rkt --set-key DEEPSEEK_API_KEY   # 从 stdin 录入（不进 argv/shell 历史）
racket main.rkt --list-keys                  # 列出各档案密钥来源 + 已配置实例（掩码）
racket main.rkt --rm-key DEEPSEEK_API_KEY    # 删除一条

# 供应商实例：同一 provider 挂多套 token，label 区分（默认 default）
racket main.rkt --provider 'deepseek[work]'  # 用 work 这套 token 的 deepseek 实例
#   TUI 内：/provider add deepseek[work]（贴底提示录 token，自动存入缓存配置并切过去）

# Auto 模式（默认开；仅 DeepSeek 生效）：按任务在 flash/pro 间切，pro 用 thinking max
racket main.rkt --provider deepseek --auto on   # /auto on|off 亦可运行时切

# 推理强度（off|low|medium|high；OpenAI 兼容 → reasoning_effort，Anthropic → 扩展思考）
racket main.rkt --provider anthropic --reasoning high

# 无头 JSONL 模式（供 IDE / 编排器以进程管道驱动）
racket main.rkt --rpc

# 恢复会话
racket main.rkt -c                       # 恢复最近一次会话
racket main.rkt -i                       # 交互选择器挑一个恢复
racket main.rkt --list                   # 列出会话并退出
racket main.rkt --resume 3               # 按 --list 序号恢复
racket main.rkt --resume data/20260709-1030-8905.rktd   # 或按路径
racket main.rkt --resume 3 --fork-at 6   # 从第 6 条消息分叉出新分支
racket main.rkt --rm 3                    # 删除某会话
```

远程端点用环境变量 `PI_API_KEY` 提供密钥。

## 供应商（多后端）

主后端仍是**本地 LM Studio**（`lmstudio`，OpenAI 兼容，无需密钥）。另内置各大 code-plan 云供应商：

| 名称 | 线路 | 端点 | 密钥环境变量 |
|---|---|---|---|
| `lmstudio`（默认） | OpenAI 兼容 | `http://localhost:1234/v1` | —（本地） |
| `openai` | OpenAI 兼容 | `api.openai.com` | `OPENAI_API_KEY` |
| `anthropic` | **原生 Messages** | `api.anthropic.com` | `ANTHROPIC_API_KEY` |
| `gemini` | OpenAI 兼容 | Google `.../v1beta/openai` | `GEMINI_API_KEY` |
| `grok` | OpenAI 兼容 | `api.x.ai/v1` | `XAI_API_KEY` |
| `deepseek` | **原生 Messages**（复用 anthropic 线路） | `api.deepseek.com/anthropic` | `DEEPSEEK_API_KEY` |

- 启动用 `--provider <name>`；运行时用 `/provider <name>` 切换，`/model <id>` 再改模型。切档案会把
  endpoint / 密钥（读环境变量）/ 默认 model 写进当前 config，下一轮即生效（读 `current-config`）。
- 插件亦可 `register-provider!` 注册自定义供应商，与内置档案共存于同一分发器。
- 供应商抽象在 `src/providers.rkt`（档案表）+ `src/provider-anthropic.rkt`（原生线路），
  **不侵入**内核 `provider.rkt`（OpenAI 兼容）。
- **提示词缓存**：Anthropic 线路在 system / tools 末块 / 最后一条消息末块打 `cache_control` ephemeral
  断点;`registry-specs` 按名排序令 tools 前缀**字节稳定**——多步 / 兄弟子 agent 复用同一缓存前缀，
  显著降低重复编码与计费（OpenAI 兼容端亦被动受益于稳定前缀）。子 agent 另收紧 `max-tokens`/
  `context-budget`（对父取 min），每步更省。
- **推理强度**（`--reasoning` / `/reasoning` / RPC `set_reasoning`，全局运行时开关，不入 prefab config）：
  `off|low|medium|high|max`。OpenAI 兼容线路 → 请求加 `reasoning_effort`（o 系/gpt-5/Gemini/Grok 识别，
  LM Studio 等忽略未知字段；`max` 无对应 → 钳到 `high`）；Anthropic 线路 → 映射为扩展思考 `thinking` budget
  （`max` 给最大预算，供 DeepSeek 等「high/max 两档」；`temperature=1`、`max_tokens` 自动加高），并把**带签名的
  thinking 块**原样回传，保证多步工具轮不被 API 拒绝。

## 密钥存储与安全（`src/credentials.rkt`）

解析优先级 **环境变量 > 凭据文件 `{config-home}/credentials.rktd`**（`PI_CONFIG_HOME` >
`XDG_CONFIG_HOME/pi++` > `~/.config/pi++`）。键名即 env 变量名，与档案 `key-env` 对齐。

- `--set-key <ENV>` 从 **stdin** 读值（不进 argv / shell 历史 / `ps`）；文件以 **0600** 原子落盘，目录 0700；
  载入时权限过宽 → 警告。
- 展示一律 `mask-key`（`sk-…abcd`）；密钥**不入日志、不入 bus/RPC 事件、不入 `.rktd` 存档**（resume 时重新解析）。
- env 优先便于 CI / 容器注入；后续可插 OS keychain 后端（接口即 `resolve-key`/`store-key!`）。
  详见 [design-credentials-billing.md](design-credentials-billing.md)。

### 供应商实例（同一 provider 多套 token）

实例名 `base[label]`（默认 label `default`）——把同一 provider 的多套 token 视作不同实例：
`deepseek[work]`、`deepseek[home]`。实例 token 独立存于凭据文件（键 `provider:deepseek:work`），
`default` 实例无 stored key 时回落该 profile 的 env 变量。

- CLI：`--provider 'deepseek[work]'`；`--list-keys` 列出实例。
- TUI：`/provider add <name[label]>` 贴底录 token → 存缓存配置并切过去；`/provider key <name[label]>` 仅改 token。
- RPC：`{"type":"add_key","base":"deepseek","label":"work","token":"..."}`，`set_provider` 传实例名。
- 分发按 `base` 解析线路/工厂（`provider-base-name`），label 只决定用哪套 token（装进 config 的 `api-key`）。

### Auto 模式（DeepSeek 按任务切模型）

「选 deepseek 即自动开」——`auto on` 且当前 provider base=deepseek 时，每轮**本地启发式**（零额外调用）
按任务复杂度选模型：短/简单 → `deepseek-v4-flash`（thinking off）；含代码/长文/工程关键词/拿不准
→ `deepseek-v4-pro`（**thinking max**）。模型名可用 env `PI_AUTO_PRO`/`PI_AUTO_LIGHT` 或
`{config-home}/auto.rktd` 覆盖。

- 开关：默认 on；`/auto on|off`、`--auto on|off`、RPC `{"type":"set_auto","on":false}`。
- 生效面只限 deepseek，其它 provider 一律不受影响；`/auto off` 可退回手动 `/model`。
- 实现全在「turn 之前」层（改 config-model + 设推理 box），内核 loop/provider 不动；接入 repl/rpc/一次性 -p 各一行。
- 每轮回显选择：REPL `auto → <model> · thinking <lvl>`；RPC 发 `{"type":"auto","model":...,"reasoning":...}`。

## 增强式回退 / 动态重算（`src/retry.rkt`）

一轮请求失败（provider 抛异常）时，不直接把错误抛给用户，而是**按错误类别动态重算**（策略在
`retry.rkt`，重试循环在 `loop.rkt` 环绕 `stream-and-collect!`，非侵入）：

| 类别 | 触发（状态码/关键词） | 恢复动作 |
|------|----------------------|----------|
| `overflow` | context length / too many tokens / prompt too long | **compact! 压缩历史后重算本轮**（已压缩仍超限→放弃） |
| `rate` / `transient` | 429/5xx/overloaded/timeout/connection | **指数退避后原样重发**（上限 `retry-max-transient`，默认 3；退避 `retry-backoff-ms` 封顶 8s） |
| `auth` / `quota` | 401/403/invalid key / billing/insufficient | **按回退链降级切到备用 provider/模型后重算** |
| `other` | 其它 | 放弃（抛原异常，沿用旧报错行为） |

- **回退链**：一串 `provider[label]` 或裸模型名，鉴权/额度失败时依次降级。设置：env `PI_FALLBACK=anthropic,deepseek-v4-flash`、
  CLI `--fallback a,b`、TUI `/fallback <a> <b> …`（`/fallback clear` 清空）、RPC `{"type":"set_fallback","chain":[...]}`。
  内置 provider 实例名 → 切档案（换端点/密钥/模型）；裸模型名 → 同 provider 内改模型（如 pro→flash 降级）。
- **可编程 `on-error` 钩子**：插件 `on! 'error-recover (λ (category message attempt) …)`，返回 `'retry|'compact|'fallback|'fail`
  或 `(cons 'fallback "target")` 覆盖内置决策，`#f` 放行默认；钩子抛异常按 `#f` 处理，绝不打断主循环。
- 恢复过程以 `[recover: … → …]` 灰字提示报到 bus（TUI/RPC 可见）；开关项皆进程级 box（config 为 prefab 不加字段）。

## 自适应：失败驱动的模型升级梯（`src/escalate.rkt`）

与 Auto 模式互补,构成完整自适应闭环:**Auto 在「turn 之前」按启发式挑起点档位;升级梯在「turn 之内」
按连续失败信号沿梯 climb**。当弱模型在一轮里反复把工具用失败(编辑对不上/命令报错/测试挂),说明起点
猜轻了 → 自动换更强的模型重试后续步骤。

- **梯**（cheap→strong,`(model . reasoning)`）：默认 `flash/off → pro/high → pro/max`（先加思考再加成本,
  中间档仍是 pro 只提 thinking）。模型名复用 `auto.rkt`（`PI_AUTO_*`/`auto.rktd` 可覆盖）。
- **触发**：一轮里过半工具调用报错记一次「失败轮」,连续达阈值(默认 2)即 climb 一级并重置计数;
  到梯顶封顶。gated 到 deepseek（默认梯是 deepseek 模型,非 deepseek 不生效避免误切供应商）。
- **开关**：默认 on；`/escalate on|off`（TUI,附带显示当前梯）、RPC `{"type":"set_escalate","on":false}`；
  `state` 含 `escalate` 字段。升级时以 `[adaptive: repeated failures → escalate to <model> · thinking <lvl>]` 报到 bus。
- 非侵入：只改 config-model + 设 reasoning box,在 `loop.rkt` 的 step 循环里按工具结果计失败轮,内核 provider 不动。

## Goal 模式：自主多轮工程（`src/goal.rkt`，见 [design-goalmode.md](design-goalmode.md)）

给定「目标 + 机器可判定的验收命令」,pi2 **自主多轮**推进,直到验收全过或轮数耗尽——把「手动
喂 milestone / `--continue` / 看测试绿没绿」变成一条命令。

```sh
racket -l pi2 -- -C <proj> --mode auto --provider deepseek \
   --goal "build X per AGENTS.md" --until "python3 -m unittest" --max-turns 20
```

- **核心原则**：终止**只认验收 `--until` 的 exit code**（ground truth）,**绝不让模型自判完成**
  （虚假胜利是自主 agent 头号失败模式）。`--until` 可重复,全 exit 0 才算 done。
- **驱动循环**：每轮把「目标 + 上轮验收失败输出」喂回,跑一轮 `run-turn!`,再跑验收;过则 DONE。
- **进度 monitor**（回答「是在稳步推进还是困住了」）：每轮算失败量信号(解析 `failures=`/`errors=`,
  或数 FAIL/ERROR/Traceback 行),连 K 轮不降=**困住** → 复用升级梯 climb 模型;升到顶仍无进展 → 停下
  给人结构化总结。monitor 与 escalate 同构,只差粒度(轮间 vs 轮内)。
- **持久 plan**（P2）：agent 在 workdir 维护 `PLAN.md` markdown 复选框(`- [ ]`/`- [x]`),驱动每轮解析、
  注入进度与当前任务、展示 `plan X/Y`——当工作记忆防漂移(但**不是终止权威**,终止仍只认 `--until`)。
- **replan**（P2）：困住且升到顶仍无进展 → 逼模型换策略重写 PLAN.md 再试(默认 1 次),用尽才停。
- **regressed 反馈**（P2）：验收信号变差时,提示最近更优的 git commit,让模型**自己用 git 工具回退**
  (驱动不做破坏性 git 手术,尊重权限模型)。
- **`--budget <USD>`**（P2）：按 `pricing.rkt` 估算累计成本,超预算即停(困住多花钱的总闸)。
- **复合**：与 retry/escalate/auto/cost/`--mode auto`/AGENTS.md/`--max-calls`/session 全部复合,内核不改。
- 现为 P1+P2。P3 待做:DAG plan 并行(解 `spawn_agent` 深度 1)+ TUI `/goal` 可视化 + RPC goal 事件流。

## 记费（`src/pricing.rkt`）

内核逐轮累计 `usage(input/output tokens)`；记费按**每百万 token 单价**估算美元开销。

- 单价表 `DEFAULT-PRICES`（近似公开价，本地模型 0），匹配用「精确 > 最长前缀」；未知模型 → `n/a`。
- 可在 `{config-home}/pricing.rktd` 覆盖：`(hash "deepseek-chat" (list 0.28 0.42) …)`，改价即生效。
- 展示：每轮页脚 `↑1.2k ↓0.3k tok · ~$0.0042`、`/usage`（含成本行）、`/cost`、RPC `state`/`turn_complete` 的 `cost_usd`。
- **估算非账单**；缓存 token 暂并入 input 口径（保守低估），因 `usage` 为两字段 prefab 不便扩展。

## IDE / 无头模式（RPC）

`--rpc` 启动 **NDJSON** 无头服务：stdin 每行一个请求 JSON，stdout 每行一个事件/响应 JSON。
完全复用内核（bus 事件 + `run-turn!` + session），**不改** loop/model/provider 一行。

```jsonc
→ {"type":"prompt","text":"..."}            // 跑一个用户轮
→ {"type":"set_model","model":"..."}         // 切模型
→ {"type":"set_provider","name":"deepseek[work]"} // 切供应商（含实例名；重写 endpoint/key/model）
→ {"type":"set_reasoning","level":"max"}     // 设推理强度 off|low|medium|high|max
→ {"type":"set_auto","on":true}              // 开/关 Auto 模式（DeepSeek 按任务切 flash/pro）
→ {"type":"set_escalate","on":true}          // 开/关失败驱动模型升级（DeepSeek: flash→pro→max）
→ {"type":"add_key","base":"deepseek","label":"work","token":"sk-..."}  // 存实例 token
→ {"type":"set_fallback","chain":["anthropic","deepseek-v4-flash"]}  // 设 on-error 回退链（[] 清空）
→ {"type":"state"} / {"type":"history"}      // 查询（state 含 reasoning/auto/escalate/fallback/cost_usd）
→ {"type":"permission","decision":"yes|no|always"}  // 应答权限询问（仅轮内）
→ {"type":"shutdown"}
← ready / delta / tool_start / tool_end / message / turn_end / turn_complete
← auto / permission_request / ok / state / history / error / bye
```

权限：`yolo` 模式不询问；`normal`/`strict` 遇需确认的工具时发 `permission_request` 并阻塞读取下一行
`permission` 响应（客户端须在该轮内应答）。单线程、请求→流式→响应，v1 无中途取消。

## 会话恢复

transcript 存于 `data/*.rktd`（每记录即时 flush，崩溃只丢未完成的尾记录，重放容忍截断）。

- **选择**：`-c/--continue`（最近一次）、`-i/--pick`（交互选择器，↑/↓/Enter/Esc）、
  `--list`（编号表格）、`--resume <序号|路径>`；交互中 `/resume` 可随时切换会话。
- **恢复预览**：恢复时终端渲染**最近几条对话**（+「…(N earlier)」省略提示），一眼接续上下文。
- **标题**：列表/选择器每项自动以**首条 user 消息**派生标题，附时间、模型、消息数。
- **分叉**：`--resume <src> --fork-at N` 重放前 N 条到一个**新** `.rktd`，从该点另起分支。
- **清理**：从未产生消息的空会话在关闭时自动删除；`--rm <序号|路径>` 显式删除。

### 行编辑快捷键（交互式 TUI）

交互式会话接管终端窗口（进 alternate screen），全程原始模式、整屏重绘。屏幕自上而下 =
内容区（输出视口）→ 可选状态行 → 分隔线 → 命令预览 → 输入框。**输入框恒钉底部、光标恒在
框内**，agent 流式输出滚动于内容区且**输出体上不驻留光标**。用户可在流式输出时继续键入而
不被撞进输出流，按键仅回显至输入框。支持 readline 常用键位、多行输入与 Unicode 正确渲染
（CJK/emoji 双宽光标对齐、按显示宽度折行）。管道输入自动回退纯 `read-line`。

- **自有滚动**：接管终端窗口、禁用原生滚屏，以「内存滚动缓存 + 视口」实现滚动——
  **鼠标滚轮 / 触控板**（SGR 上报）、`PageUp/PageDown` 上下翻动历史；上滚阅读时新输出不打扰
  （视口锚定），提交新输入即跳回底部跟随。缓存亦支撑超长会话的局部提取（见 `/tail`）。
- **工作动画**：LLM 执行任务的**整轮**期间，底边隔离条播放进度动画，且**每轮从起点重新开始**（不续上一轮）。
  **默认** `bar`（256 色青色渐变**细线**左右乒乓横扫，已瘦身为细线、占位小）；`PI_PROGRESS=sweep` 可改用
  无色的 **`sweep`**（粗亮线单向掠过，纯 bold/dim，处处可用）。动画**速率按实时 token/s 估计分 4 档**
  （越快扫得越快），隔离条右端显示 `~N tok/s` 读数指征性能——该读数经**长窗采样 + 低通 EMA + 量化到 5**
  三重平滑，稳定不抖。等待首 token / 工具间隙时状态行另显转轮 + `thinking…`。
- **终端原生进度条**（OSC 9;4）：整轮向终端发 `OSC 9;4;3`（不定进度）、轮末 `OSC 9;4;0` 清除，
  让 **Ghostty / WezTerm / Windows Terminal / iTerm2 3.5+** 在其标题/边缘画原生进度条（emulator 绘制，
  非 tty 内动画）。默认对已知良好支持者自动开启，`PI_OSC_PROGRESS=on|off` 强制。
- **命令预览**：输入以 `/` 起头时，底部实时显示匹配的元命令预览面板，边打边收窄。
- **安全**：模型/工具输出默认经消毒，剥离 ESC/控制字符，杜绝终端转义注入（改标题、
  清屏、光标劫持等）。

| 键 | 作用 | 键 | 作用 |
|---|---|---|---|
| `←/→` `Ctrl-B/F` | 左右移动 | `Ctrl-A/E` `Home/End` | 行首/行尾 |
| `Alt-B/F` `Ctrl-←/→` | 按词移动 | `Backspace` `Del` | 删前/删后字符 |
| `Ctrl-W` `Alt-⌫` | 删前一词 | `Alt-D` | 删后一词 |
| `Ctrl-K` | 删到行尾 | `Ctrl-U` | 删到行首 |
| `Ctrl-Y` | 粘贴 kill-ring | `↑/↓` `Ctrl-P/N` | 历史 |
| `Ctrl-L` | 重绘 | `Ctrl-D` | 空行时退出 |
| `Enter` | 提交（空行仅换行不派发） | `Shift+Enter` `Alt+Enter` | 插入换行（多行输入） |
| `PageUp/PageDown` 鼠标滚轮 | 上下滚动历史视口 | | |

多行输入：`Shift+Enter` 在支持上报的终端（kitty 键协议 / `modifyOtherKeys`）生效，
`Alt/Option+Enter` 为可移植回退，任意终端可用。

`Ctrl-C` 分级（**不回显 `^C`**）：**输入框有草稿** → 清空草稿；**草稿为空且 agent 运行中**
→ 打断当前轮（中断提示 `⎯ interrupted ⎯` 作为独立元信息另起一块，不与输出同块）；
**草稿为空且空闲** → 无动作。

### 斜杠命令

`/help` `/quit` `/clear` `/usage` `/cost` `/compact` `/history` `/tail [n]` `/resume` `/skills` `/prompt [name]` `/provider [name|add|key]` `/reasoning [level]` `/auto [on|off]` `/escalate [on|off]` `/fallback [targets|clear]` `/model <id>`

- `/provider`（无参、交互式 TUI）弹**快速切换选择器**：列出可用 provider + 已配置实例 + auto 开关，方向键选中即切换（`● ` 标当前，Esc 取消）；管道/非交互回退文本列表。带参 `/provider <name>` 直接切。
- `/reasoning`（无参、交互式 TUI）弹贴底选框挑 `off|low|medium|high|max`；带参直接设。

输入 `/` 时 TUI 会实时预览可用命令，`Tab` 补全命令名（唯一匹配补全整名 + 空格，多匹配补到
最长公共前缀）。`/tail [n]` 从滚动缓存取最后 n 行（默认 20）；`/resume` 弹出会话选择器切换
到另一会话（Esc 取消）。

### 权限模式

| 模式 | read-only | mutating | dangerous |
|---|---|---|---|
| `strict` | 直通 | 询问 | 询问 |
| `normal`（默认） | 直通 | 直通 | 询问 |
| `yolo` | 直通 | 直通 | 直通 |
| `auto`（作用域自动批准） | 直通 | 按作用域 | 按作用域 |

**`auto` 作用域自动批准**（`--mode auto`,为**无人值守长跑**设计）:read-only 恒直通;`write_file`/`edit_file`
**在 workdir 内**自动过、越界(`..`/绝对外)则询问;`git` 本地操作(status/diff/add/commit…)直通、网络子命令
(push/pull/fetch/clone)询问;`bash` 按启发式——命中网络出口(curl/wget/pip·npm install…)或破坏性/提权
(rm -rf/sudo/dd/`>/dev/`…)则询问,否则视作项目内构建/测试直通。**「询问」在无头模式即拒绝**——所以
`auto` 下 agent 能在项目内自由读写/git/跑测试,而联网与危险操作被自动拦住。best-effort 防线(bash 不透明,
非沙箱);判定逻辑在 `src/permission.rkt` `scoped-decision`,已单测覆盖路径越界/网络/破坏性各分支。

需审批时，交互式 TUI 在**屏幕底部弹出内联小选框**（↑/↓/Enter，Esc=拒绝；上方对话仍可见、
不全屏遮挡）而非 y/n/a 回显输入，四选一：
**Yes（本次放行）· Yes（不再询问）· No（拒绝）· No（并告诉 agent 原因）**。选最后一项可填写
一句理由，随该工具的拒绝结果一并回传给模型——模型据此**不重试同一调用、调整思路**，
从而改善后续类似调用的表现。选「不再询问」的工具记入 `cache/permissions.rktd`，跨会话生效。
管道/非交互仍回退纯 `y/n/a` 文本询问。

## 插件运行时

对标 pi 的自扩展能力，用 Racket 的解释/拓展能力实现的插件运行时：`dynamic-require` 载入
**受信**插件、`racket/sandbox` 在受限求值器里安全运行**不可信**插件（限内存/时间/文件/网络
+ custodian 回收——pi 缺失的能力）。插件可扩展**工具**（模型可调用）、**斜杠命令**、**LLM 供应商**
（`--provider <name>` 选用）、**变换型钩子**（`on-tool-call` 拦截/改参、`on-tool-result` 改结果、
`before-turn` 注入、`on-context` 改窗口）与观测钩子、**斜杠命令/键位快捷键**；
`ctx.notify/select/confirm/session` 接 TUI。`/provider`、`/model` 均可**运行时切换**。

```sh
racket main.rkt --plugins examples/plugins   # 示例：echo 工具、/hello 命令、沙箱计算/写入
racket main.rkt --plugins examples/plugins --provider echollm   # 用插件注册的 LLM 供应商
```

启动自动载入 `./plugins/`（放你自己的插件），`--plugins <dir>` 可重复追加。写法：
- `foo.rkt` 受信——`(provide plugin)` 一个 `(-> plugin-api void)` 注册函数（全权，加载前过信任门）。
- `foo-sandbox.rkt` 沙箱——`#lang racket/base`，`(provide manifest tool-run)`；旁置 `foo-sandbox.rktd`
  写 `(caps fs-write …)` 声明所需**能力**，加载时逐项授权。
- **声明式 DSL**——`#lang s-exp "…/src/pi-plugin-lang.rkt"`，用 `deftool`/`defcommand`/`on`/`defprovider`/
  `defshortcut` 无样板注册（见 `examples/plugins/dsl-demo.rkt`）。

**技能/提示词**：`./skills/*.md`（YAML 前置元数据 name/description）名称+描述**渐进披露**进系统提示词
（模型按需 `read_file` 全文）；`./prompts/*.md` 经 `/prompt <name>` 激活（正文追加进系统提示词）；
`/skills`、`/prompt` 列出。

**能力授权**：受信插件加载前询问信任、沙箱插件按声明能力逐项授权（y/a/n，`always` 持久化到
`cache/plugin-grants.rktd`）；未授予的能力被沙箱硬性拒绝（如 `fs-write`）。`--trust-plugins` 一键授予、
非交互默认拒绝。

设计、能力对标 pi 与分阶段方案见 [design-plugins.md](design-plugins.md)；示例见 `examples/plugins/`。

## 系统提示词与行为

首条 `system` 消息由 `main.rkt` 的 `DEFAULT-SYSTEM` 提供（`--resume` 沿用存档 config）。
核心约束：**不确定即查证**——对文件内容、符号定义、API、项目结构、命令输出或某路径/名称是否
存在有疑时，先用工具（`read_file`/`grep`/`glob`/`bash`）核实再作答，不臆测、不虚构，并优先以
证据（文件片段、命令输出）佐证。

**项目指令自动加载**（对标 Claude Code 的 `CLAUDE.md`/`AGENTS.md`）：启动时在**工作目录**按优先级
`AGENTS.md` → `CLAUDE.md` → `.pi/AGENTS.md` → `PI.md` 取**首个存在**的文件,其正文注入系统提示词,
让 agent 每个 session 不再对项目规范「失忆」。仅全新启动注入,`--resume` 沿用存档(避免重复叠加)；
命中时 stderr 打 `[project instructions loaded: …]`。

## 内置工具

`bash` · `read_file` · `write_file` · `edit_file` · `glob` · `grep` · `git` · `spawn_agent`（子 agent）。

- **`edit_file`**（容错）：单条 `old_string`+`new_string`（须唯一,除非 `replace_all:true` 替换全部）；
  或批量 `edits:[{old_string,new_string,replace_all?}]` **按序原子应用**（任一失败则整体不落盘,报第几条错）。
  批量优于多次单调,尤其利于弱模型（deepseek/本地）少往返。
- **`git`**：args 传**字符串数组**（argv,不过 shell）,如 `["commit","-m","msg with spaces"]`——含空格/引号
  的参数零转义,弱模型也能可靠产出,且杜绝注入。权限级 `mutating`（介于只读工具与 `bash` 之间）。

**并行执行**：一条 assistant 消息里的多个工具调用先**串行预检**（权限询问不可并发），
当整批都是 `read-only`（如多个 `read_file`/`grep`/`glob`）时**并发执行**，否则按序（避免读写文件竞态）。
无论并发与否，结果都按原始调用顺序归位、事件按序发布，输出确定可测。

## 会话格式

transcript 是 `.rktd` datum 流（prefab struct 的 `write`/`read` 往返），既是机器真相源，
也可直接阅读、编辑、`read` 进 REPL 调试。重放复用运行时同一套状态迁移函数。

## 测试

原生 `raco test`：自动发现 `tests/` 下的单测、并行执行、汇总通过数、失败即非零退出。
`tests/info.rkt` 的 `test-omit-paths` 把需要 LM Studio 的真机(live)测试排除在离线遍历外。

```sh
./run-tests.sh          # 离线单测：raco test -j 4 tests/（无需 LLM）
./run-tests.sh --live   # 追加对 LM Studio gemma 的真机验收（provider/loop/subagent）
raco test tests/tui-console-test.rkt   # 也可直接对单个文件跑
```

## 目录结构

```
pi2/
├── info.rkt              raco pkg 包定义（collection "pi2" + deps + 启动器）
├── main.rkt              入口装配（module+ main；racket -l pi2 / pi2 启动）
├── run-tests.sh
├── src/                  全部源码
│   ├── model.rkt         核心数据 (prefab)      loop.rkt      agent 主循环
│   ├── rktd.rkt          datum-log 流式读写      context.rkt   token 估算/裁剪/compact
│   ├── event.rkt         事件总线               session.rkt   .rktd 持久化/流式重放
│   ├── provider.rkt      OpenAI 兼容客户端        permission.rkt 权限门控
│   ├── provider-anthropic.rkt 原生 Messages 线路  providers.rkt 内置供应商档案(含 deepseek)
│   ├── credentials.rkt   密钥/实例token存储解析       pricing.rkt  记费估算(USD)
│   ├── auto.rkt          Auto 模式(DeepSeek 按任务切模型)
│   ├── escalate.rkt      自适应升级梯(失败驱动 flash→pro→max)
│   ├── goal.rkt          Goal 模式(驱动循环+验收 oracle+进度 monitor)
│   ├── retry.rkt         增强式回退(分类/决策/回退链/动态重算)
│   ├── rpc.rkt           无头 JSONL 模式(--rpc)   subagent.rkt  spawn_agent
│   ├── stream.rkt        SSE/accumulator        repl.rkt      终端交互
│   ├── tool.rkt          工具协议/注册表         loop.rkt      主循环(含并行工具执行)
│   ├── plugin.rkt        插件运行时（dynamic-require + sandbox + 钩子 + 多供应商）
│   ├── pi-plugin-lang.rkt 声明式插件语言 #lang     resources.rkt  技能/提示词发现
│   ├── tools/            bash · file(read/write/edit容错) · search · git · builtin
│   └── tui/              终端 UI 抽象层
│       ├── width.rkt     Unicode 显示宽度 (wcwidth)
│       ├── keys.rkt      按键/转义序列解析
│       ├── terminal.rkt  终端抽象（真实 tty + 脚本后端）
│       ├── lineedit.rkt  行编辑器 + readline 快捷键
│       ├── sanitize.rkt  不可信文本消毒（剥离终端转义注入）
│       ├── picker.rkt    可选列表控件（会话选择器；纯状态机 + 渲染）
│       ├── tui.rkt       组装 tui-read-line（同步单行读）
│       └── console.rkt   全屏异步控制台（alt-screen + 自有滚动视口 + 工作动画 + 命令预览）
├── tests/               单测 + 真机验收（raco test；info.rkt 排除 live）
├── data/                运行时：会话 transcript (*.rktd)，git 忽略
└── cache/               运行时：permissions.rktd 等跨会话缓存，git 忽略
```
