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
→ {"type":"add_key","base":"deepseek","label":"work","token":"sk-..."}  // 存实例 token
→ {"type":"state"} / {"type":"history"}      // 查询（state 含 reasoning/auto/cost_usd）
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

`/help` `/quit` `/clear` `/usage` `/cost` `/compact` `/history` `/tail [n]` `/resume` `/skills` `/prompt [name]` `/provider [name|add|key]` `/reasoning [level]` `/auto [on|off]` `/model <id>`

输入 `/` 时 TUI 会实时预览可用命令，`Tab` 补全命令名（唯一匹配补全整名 + 空格，多匹配补到
最长公共前缀）。`/tail [n]` 从滚动缓存取最后 n 行（默认 20）；`/resume` 弹出会话选择器切换
到另一会话（Esc 取消）。

### 权限模式

| 模式 | read-only | mutating | dangerous |
|---|---|---|---|
| `strict` | 直通 | 询问 | 询问 |
| `normal`（默认） | 直通 | 直通 | 询问 |
| `yolo` | 直通 | 直通 | 直通 |

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

## 内置工具

`bash` · `read_file` · `write_file` · `edit_file` · `glob` · `grep` · `spawn_agent`（子 agent）。

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
│   ├── rpc.rkt           无头 JSONL 模式(--rpc)   subagent.rkt  spawn_agent
│   ├── stream.rkt        SSE/accumulator        repl.rkt      终端交互
│   ├── tool.rkt          工具协议/注册表         loop.rkt      主循环(含并行工具执行)
│   ├── plugin.rkt        插件运行时（dynamic-require + sandbox + 钩子 + 多供应商）
│   ├── pi-plugin-lang.rkt 声明式插件语言 #lang     resources.rkt  技能/提示词发现
│   ├── tools/            bash · file · search · builtin
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
