# pi++

基于 Racket 的 LLM agent。终端内运行，支持流式输出、工具调用循环、`.rktd` 原生会话持久化、
上下文压缩、权限门控与子 agent。设计文档见 [design.md](design.md)。

## 依赖

- 增强版 Racket（源码用 `#lang tstring racket`，含 `f""` 字符串模板、`pvector`、`racket/intmap`）。
- 一个 OpenAI 兼容的 LLM 端点。默认指向本地 LM Studio (`http://localhost:1234/v1`)，
  默认模型 `gemma-4-31b-it@6bit`。

## 运行

```sh
# 交互式会话（当前目录为工作区）
racket main.rkt

# 指定模型 / 端点 / 权限模式
racket main.rkt -m gemma-4-31b-it@6bit -e http://localhost:1234/v1 --mode normal

# 单次问答（管道友好）
racket main.rkt --mode yolo -p "read config.rkt and summarize it"

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

`/help` `/quit` `/clear` `/usage` `/compact` `/history` `/tail [n]` `/resume` `/provider [name]` `/model <id>`

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
├── main.rkt              入口装配（锚定 data/ 与 cache/）
├── run-tests.sh
├── src/                  全部源码
│   ├── model.rkt         核心数据 (prefab)      loop.rkt      agent 主循环
│   ├── rktd.rkt          datum-log 流式读写      context.rkt   token 估算/裁剪/compact
│   ├── event.rkt         事件总线               session.rkt   .rktd 持久化/流式重放
│   ├── provider.rkt      流式 LLM 客户端         permission.rkt 权限门控
│   ├── stream.rkt        SSE/accumulator        repl.rkt      终端交互
│   ├── tool.rkt          工具协议/注册表         subagent.rkt  spawn_agent
│   ├── plugin.rkt        插件运行时（dynamic-require + sandbox + 钩子）
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
