# design-dsv4b.md — dsv4-b 档案：Minimal 锚定 + 两阶段解锁（anchored）

日期：2026-08-15。状态：**v2 已实现**（本文档含竣工事实，见 §4/§9）。v1（纯极简裁剪）已废弃，见 §8。

## 0. 背景调研（联网，2026-08）

- 2026-08-13，DeepSeek 同日发布 **DeepSeek-V4-Pro 正式版**（1M context / 384K
  output）与 MIT 开源 agent 框架 **DeepSeek Harness v0.1**（四预设：Standard /
  Code / Minimal / Creator）。
- **Minimal（极简）预设**即官方给 V4-Pro-0813 跑 code-agent 基准的 harness
  （DeepSWE 62.7、Terminal Bench 2.1 87.9）：完整 system prompt 仅一句
  **"You are a helpful software engineer assistant."**，工具恰两个：持久化
  `bash` + `str_replace_editor`。
- **锚定效应（本设计的根据）**：社区预设
  [dsh-anchored-standard](https://github.com/xiaobright/dsh-anchored-standard)
  实证：V4-Pro **强烈 condition 在首个请求的 API 可见工具目录上**——首请求只给
  Minimal 双工具 schema，模型即锚定到 Minimal 的轨迹分布（对照：Minimal schema
  5/5 出现基准态行为模式 vs Standard schema 11/11 非基准态）；在首个 durable
  事件后**静默**把工具表换成 Standard 全量（~25 工具），system prompt 一字不动。
  Project2 V4.1b（V4-Pro，reasoningEffort=max）：**Anchored 98/99**，官方
  Minimal 99/96，Standard 91/92。作者注明「对此任务可复现，非普适性主张」。
- 结论：所谓「极简模式使能强能力」≈ 一个可利用的训练分布效应（用户语：模型
  bug）——**锚定在工具 schema，不在提示词措辞**；解锁靠换工具表，追加提示词
  可选。

来源：digitalapplied.com、venturebeat.com、github.com/deepseek-ai/deepseek-harness、
github.com/xiaobright/dsh-anchored-standard、api-docs.deepseek.com。

## 1. 目标 / 非目标

**目标**：新增内置档案 `dsv4-b`（b = bootstrap/bench-anchored），行为 =
pi2 版 anchored-standard：

1. **Phase 1（锚定）**：首请求 system prompt 用 Minimal 原句，工具表恰
   `bash` + `str_replace_editor` 两个 schema（后者需新建适配工具）。
2. **Promotion（解锁）**：首个 durable 事件后，一次性地
   (a) 把 pi2 全量工具灌回 registry，(b) 把 pi2 的常规提示词内容（DEFAULT-SYSTEM
   要点 + skills addendum + 项目指令）作为**解锁段追加**到 system prompt。
   两者同一边界发生 ⇒ prefix cache 只断一次。
3. 后端：DeepSeek Anthropic 兼容线路，默认模型 **deepseek-v4-pro**；
   base ≠ "deepseek" ⇒ auto/升级梯天然不生效（钉死 v4-pro，保持锚定纯净）。

**非目标**：

- 不做永久极简裁剪（v1 方案）；goal/压缩/子agent 等 pi2 能力 promotion 后全部
  照常可用。
- 不改 `config` prefab 结构（.rktd 回放兼容；促迁状态走 box/parameter，遵循
  auto.rkt:24 / escalate.rkt:20 惯例）。
- 不主张普适增益：锚定收益是经验性的，配套 A/B 评测（§5.3）验证，而非默认吹嘘。

## 2. 核心机制设计

### 2.1 pi2 的先天优势

- `registry` 是可变 hash（tool.rkt:78），`loop.rkt:44` **每轮重读**
  `registry-specs` ⇒ 中途 `registry-add!` 天生生效，无需改 wire 层。
- `registry-specs` 按名排序保证字节稳定前缀（tool.rkt:90）⇒ promotion 前后
  各自内部 cache 稳定，仅切换那一刻断一次——与 anchored-standard 的 cache
  语义一致。
- system prompt 是 config 字段，`state`/`struct-copy config` 即可在 promotion
  时追加解锁段；session .rktd 回放会忠实重现两阶段（见 §2.5）。

### 2.2 新模块 src/dsv4b.rkt

```racket
ANCHOR-SYSTEM      ; "You are a helpful software engineer assistant."（逐字）
BOOT-TOOL-NAMES    ; '("bash" "str_replace_editor")
(dsv4b-active? host)        ; base 名 = "dsv4-b"（镜像 escalation-active?）
(dsv4b-promoted?) / (dsv4b-promote!) ; 会话级 box；不可逆（对齐 anchored 语义）
(dsv4b-promote-on)          ; 'tool-call（默认）| 'either — 见 §2.4
(dsv4b-unlock-addendum skills proj-body proj-path)
                            ; 解锁段文本：pi2 工具须知 + skills + 项目指令
```

promotion 状态**不进 config**：进程级 box + 从 history 可重推（§2.5），
与 reasoning-effort 同样的「新旋钮不进 prefab」惯例。

### 2.3 新工具（schema 从上游源码逐字摘录，浅克隆 deepseek-harness@master 核对）

**src/tools/editor.rkt — `str_replace_editor`**（上游
`packages/fs/tool-str-replace-editor/src/index.ts`）：

- 命令恰四个：`view` / `create` / `str_replace` / `insert`——**无 `undo_edit`**
  （§7.1 已解决）。参数面：`command`(enum,req) `path`(req,**绝对路径**)
  `file_text` `insert_line` `new_str` `old_str` `view_range`，描述文本逐字。
- 行为逐字对齐：view 文件为 cat -n 风格（6 宽右对齐行号 + 两空格）、目录列
  2 层深（排除隐藏项/node_modules/__pycache__）；输出超 16000 字符截断并附
  上游 `<response clipped>` NOTE；全部错误消息逐字（相对路径提示、view_range
  三种越界、str_replace 零/多匹配含行号列表、insert 越界等）。
- 刻意偏差仅两处：create 自动补建父目录；无 sandbox 层。promotion 后保留在
  registry，与 read_file/write_file/edit_file 并存。

**src/tools/bash-persistent.rkt — 持久 `bash`**（上游 minimal 预设在
`agent.cordis.yml` 里**覆写**了 bash 描述——SWE-bench 风格多条 bullet，声明
「State is persistent across command calls」，timeoutMs 300000）：

- schema：name=`bash`，单参数 `command`（描述逐字："The bash command to run.
  Relative path is preferred in the command."）；超时 300s。
- 描述取上游覆写文本，**刻意删去两条 bullet**（"You don't have access to the
  internet…" 与 apt/pip 镜像）——那是基准沙箱事实，本地为假会误导模型拒绝
  联网命令（§7.5 记录此保真度取舍）。
- 持久化不走 PTY（上游是持久 shell 进程 + nonce 标记协议）：状态文件回放——
  每次调用 `/bin/bash --noprofile --norc` 先 source 上次的 `export -p` 快照、
  cd 回上次 cwd，执行后再落盘。覆盖描述声明的语义（cwd + 导出 env 跨调用）；
  函数/别名不保。后台任务（描述明确鼓励 `… &`）会拖住 stdout 管道：shell 退出
  后泵线程限时 1s 收尾，实测 `sleep 2 &` 亚秒返回。

### 2.4 Promotion 触发与执行

- **触发**（在 run-turn! 主循环内检测，loop.rkt）：
  - `'tool-call`（默认）：本会话第一次工具调用执行**完成**后（成败均算——
    anchored 语义「failed tool execution still promotes」）。
  - `'either`：或者首条纯文本 assistant 终止性回复后。
  - 默认取 `'tool-call`：anchored 文档指出 `either` 下纯寒暄也会促迁，浪费
    锚定；CLI/`PI_DSV4B_PROMOTE` 可选 `either`。
- **执行**（一次性，幂等锁在 box 上）：
  1. `registry-add!` 全量内置工具（glob/grep/git/read_file/write_file/edit_file）
     + `spawn_agent`；加载被推迟的插件（§2.6 装配时暂存目录列表）。
  2. `struct-copy config`：system-prompt := ANCHOR-SYSTEM + 解锁段。
  3. bus 上发一条 `[dsv4-b promoted: full toolset unlocked]` 事件（TUI 可见）。
- **解锁段内容**（≈ 现 DEFAULT-SYSTEM 的浓缩 + 两个 addendum）：pi2 全工具
  一览与使用须知、skills-addendum（resources.rkt:60）、项目指令
  addendum（resources.rkt:74）。措辞上作为**追加信息**而非「换了个人设」——
  锚定句保留在最前。
- **不可逆**：会话内不降回双工具态（对齐 anchored「promotion finality」）。

### 2.5 Session 回放 / resume

promotion 是纯函数式状态迁移的副作用，回放策略：resume 时扫描存档 history，
若存在任何 tool-call（或按 promote-on 规则已触发），启动即视为 promoted 并
重放步骤 1-2（registry 是进程内新建的，本来就要重建）。system-prompt 已在存档
config 里含解锁段 ⇒ 步骤 2 以「已含解锁段则跳过」幂等化。

### 2.6 装配期（main.rkt，`--provider dsv4-b` 启动时）

provider 名解析前移到装配步骤之前（同 v1 计划），`dsv4b-boot?` 分支：

| 步骤 | dsv4b-boot? = #t 时 |
|---|---|
| system-prompt (:283/:296-298) | 基底 = ANCHOR-SYSTEM；两个 addendum **不拼**（暂存，promotion 时进解锁段） |
| skills/项目指令 (:267-277) | 照常**发现**但不注入；暂存给 dsv4b 模块 |
| base-tools (:306) | bash + str_replace_editor 两个 |
| 插件加载 (:320-328) | **整体跳过**（非推迟——插件信任询问不宜发生在 turn 中途；stderr 提示一行） |
| spawn_agent (:349) | 推迟到 promotion |
| context-budget | 提至 ~200k（v4-pro 1M 窗口） |

运行时 `/provider dsv4-b` 中途切入：此时首请求早已发过、锚定窗口已错过，
仅换端点/模型 + 打印
`note: anchoring requires --provider dsv4-b at launch (first-request tool schema)`。

### 2.7 档案注册（providers.rkt）

```racket
;; dsv4-b：Harness Minimal 锚定档案（两阶段：双工具首请求 → 全量解锁）。
;; base ≠ "deepseek" ⇒ auto/升级梯不生效，钉死 v4-pro。机制见 src/dsv4b.rkt。
(provider-profile "dsv4-b" 'anthropic "https://api.deepseek.com/anthropic"
                  "deepseek-v4-pro" "DEEPSEEK_API_KEY")
```

凭据经兄弟档案回退与 deepseek/deepseek-lite 共享（providers.rkt:80-91）。
计价：pricing.rkt:34 deepseek 前缀回退已覆盖。

## 3. 与 v1 的行为差异一览

| 能力 | v1（纯极简） | v2（anchored，本设计） |
|---|---|---|
| 工具 | 永远 4 个 | 首请求 2 个 → 全量 + editor |
| skills/项目指令 | 永久关闭 | promotion 时注入 |
| 压缩/goal/子agent/插件 | 永久关闭 | promotion 后照常 |
| auto/升级梯 | 关（base gating） | 关（同左，保锚定纯净） |
| system prompt | 自写极简版 | Minimal 原句 + 解锁追加段 |

## 4. 实现清单（竣工）

1. `src/tools/editor.rkt`（新，~330 行）：`str_replace_editor` 保真移植。
2. `src/tools/bash-persistent.rkt`（新，~130 行）：持久 bash（状态文件回放）。
3. `src/dsv4b.rkt`（新，~140 行）：ANCHOR-SYSTEM / DSV4B-BASE / UNLOCK-MARKER、
   `dsv4b-active?`、promote-on/payload/promoted 进程级 box、
   `dsv4b-maybe-promote!`（不可逆、marker 幂等）、`history-has-tool-call?`、
   `dsv4b-unlock-addendum`、`dsv4b-reset!`（测试用）。
4. `src/providers.rkt`：BUILTIN-PROFILES 追加 dsv4-b（默认 deepseek-v4-pro）。
5. `main.rkt`：provider-name/dsv4b-boot? 前移至装配前（host 校验位置不变，
   回归面为零）；锚定装配分支（ANCHOR-SYSTEM、双工具 registry、addenda 推迟、
   插件跳过、context-budget 200k、spawn_agent 入 payload）；`--dsv4b-promote
   tool-call|either` 旗标；resume 重推（history 有 tool-call ⇒ 立即重放
   payload，marker 防重复追加）；--provider help 文案。
6. `src/loop.rkt`：run-turn! 两处促迁检测——工具批执行后（'tool-call，成败均
   促迁）与终止性纯文本回复后（'assistant-message，仅 'either 生效）；促迁
   经 bus 发 `[dsv4-b promoted: …]` 提示。
7. `src/repl.rkt`：switch-provider! 中途切入 dsv4-b 提示锚定已错过。
8. `src/tools/file.rkt`：导出 `atomic-write!` 供 editor 复用。

实际 diff ~700 行（含新模块），不含测试。README 段落待补。

## 5. 测试设计（竣工：tests/editor-test.rkt、tests/bash-persistent-test.rkt、
tests/dsv4b-test.rkt 已落地并全绿；live 为 tests/dsv4b-live-test.rkt，入
tests/info.rkt 排除表，无 DEEPSEEK_API_KEY 时跳过）

### 5.1 单元（tests/dsv4b-test.rkt，模板 providers-test.rkt / tool-test.rkt）

1. 档案注册与 apply-provider-profile：endpoint/model=deepseek-v4-pro/key-env；
   凭据兄弟回退（镜像 providers-test.rkt:96-138）。
2. gate：dsv4-b / dsv4-b[x] 下 `auto-active?`=#f、`escalation-active?`=#f、
   `dsv4b-active?`=#t；deepseek 下回归不变。
3. **首请求 schema 纯净性**（锚定的命门）：boot 装配后
   `(map spec-name (registry-specs reg))` **恰为** `("bash" "str_replace_editor")`
   ——多一个都算破坏锚定。
4. promotion 触发语义：stub 一次工具调用（含**失败的**调用）→ promoted；
   `'tool-call` 模式下纯文本回复 → 未 promoted；`'either` 下 → promoted；
   promote! 幂等、不可逆。
5. promotion 效果：registry 含全量 + editor + spawn_agent；system-prompt =
   ANCHOR-SYSTEM 前缀 + 含 skills/项目指令标记的解锁段；ANCHOR-SYSTEM 逐字
   在最前。
6. resume 重推：构造含 tool-call 的存档 → 启动即 promoted 且解锁段不重复追加
   （幂等断言）。

### 5.2 editor 工具（tests/editor-test.rkt，模板 file 相关用例）

view（文件/目录/行 range）、create（存在即错）、str_replace（唯一匹配；0 匹配
错、多匹配错）、insert；权限级别与 write_file 一致；路径越界与 file.rkt 同规。

### 5.3.1 A/B 实测结果（2026-08-15，tests/dsv4b-bench.rkt）

3 轮 × 2 臂（anchored = dsv4-b 生产路径；standard = 同端点钉 v4-pro + 全量
工具 + DEFAULT-SYSTEM，host 挂 deepseek-lite 关升级梯）× 各 4–6 run，
共 28 run，任务为确定性 bash 验收（easy 3 任务 / hard 2 任务）：

| 轮次 | 通过 (a\|s) | 工具调用 | tok-in | tok-out | 耗时 |
|---|---|---|---|---|---|
| easy · effort=high | 6/6 \| 6/6 | 4.0 \| 3.3 | 2455 \| 1228 | 1226 \| 870 | 22.5s \| 15.9s |
| hard · effort=high | 4/4 \| 4/4 | 4.0 \| 3.2 | 1375 \| 1172 | 1309 \| 1044 | 21.4s \| 18.3s |
| hard · effort=off  | 4/4 \| 4/4 | 3.5 \| 4.2 | 1485 \| 1282 | 1282 \| 1146 | 21.7s \| 21.1s |
| review · effort=high | 2/2 \| 2/2 | 14.5 \| 8.5 | 3752 \| 1992 | 9893 \| 2606 | 171.6s \| 44.1s |

review 集（只读代码审查，播种 3 bug 召回制，防改=副本+chmod+哈希审计）补充：
- **召回率 anchored 6/6 vs standard 5/6**（standard 一次漏掉一个 bug）——首个
  质量信号，方向与上游主张一致，但样本太小（n=2/臂）不足为证。
- **代价 ~4×**：anchored 呈典型基准轨迹行为——对每个疑点用 bash 跑复现片段
  实证验证（tok-out 3.8×、耗时 3.9×）。「更彻底、更贵」。
- 防改三层全绿：4 run 全部 unchanged=#t，yolo 权限下也无一越界。

结论：
1. **质量无差异（14/14 vs 14/14，天花板效应）**——此规模任务 v4-pro 两臂全对，
   判别锚定的质量收益需要 Project2 级长程任务，小基准测不出。
2. **anchored 有稳定的成本开销**：解锁段 + editor schema + 促迁处一次 cache
   断裂，easy 轮 tok-in 约 2×、耗时 +42%；任务变难/关 thinking 后差距收窄
   （hard·off 轮 anchored 调用数反而更少 3.5 vs 4.2，耗时持平）。
3. 行为差异可观察：anchored 促迁前倾向「全 bash」作业（Minimal 轨迹习惯），
   促迁生效正常（促迁后立刻改用 edit_file/write_file）。
4. **建议**：日常轻任务用 deepseek/deepseek-lite；dsv4-b 用于长程/难任务、
   审查类「宁贵勿漏」场景或基准复现。收益主张在小任务负载下不可复现也未被
   证伪；review 集给出首个方向一致的弱信号（召回 6/6 vs 5/6，代价 4×）。

### 5.3 A/B 锚定评测（live，默认排除，`--live`）

`tests/dsv4b-live-test.rkt`：同一小型多步任务（建文件→改文件→bash 验证）跑
三态：(a) dsv4-b（anchored）、(b) deepseek 档案 pin v4-pro（全量 schema 首请求）、
(c) 双工具不促迁。断言 (a) 能完成任务且发生 promotion；记录轮数/token 供人工
对比——锚定收益是经验命题，不写成硬断言。加入 tests/info.rkt test-omit-paths。

### 5.4 回归

`./run-tests.sh` 全绿；重点 providers/auto/escalate/tool/loop 测试不被
provider 解析前移与 run-turn! 促迁检测点破坏。

## 6. 风险

- **锚定效应不普适**：证据来自单一基准（Project2，Windows）。§5.3 的 A/B 是
  第一方验证；若 pi2 工作负载下无收益，dsv4-b 退化为「多一次 cache 断裂的
  deepseek-pro 档案」，损失有限。
- **schema 保真度**：pi2 的 str_replace_editor 参数面若与 Harness 版有出入，
  锚定可能减弱。上游 schema 开源可查（deepseek-harness 仓库），实现前逐字段
  对照（§7.1）。
- promotion 检测点侵入 run-turn!：保持为「查 box + 调 dsv4b-promote!」两行，
  非 dsv4-b 路径零开销。
- 中途 `/provider` 切走再切回的语义（锚定已耗尽）：文档 + 提示语兜底，不做
  状态复原。

## 7. 开放问题

1. ~~`str_replace_editor` 是否含 `undo_edit`~~ **已解决**：上游源码确认无
   undo_edit，命令恰 view/create/str_replace/insert，schema 已逐字移植（§2.3）。
2. 首请求是否还应对齐 Minimal 的 maxTokens 行为（anchored 发现「Minimal schema
   不设 cap 也锚定」）：暂不动 pi2 的 max-tokens 默认，A/B 若见差再调。
5. bash 描述删去「无互联网 / apt+pip 镜像」两条 bullet 是保真度 vs 真实性的
   取舍（保留会误导模型拒绝联网命令）。若 A/B 显示锚定明显减弱，可提供
   `PI_DSV4B_BASH_DESC=verbatim` 逃生门恢复逐字文本。
3. 解锁段放 system prompt（本设计）vs 塞首条 user 消息注入：anchored 证明
   纯 schema 切换已够；解锁段主要为 pi2 功能（skills/goal）的**可发现性**服务。
   若 A/B 显示追加 system 段削弱锚定，退到「工具表切换 + before-turn 注入
   user-role 上下文消息（plugin hook 已有此通道）」。
4. spawn_agent 的子 agent 是否也走锚定（子会话首请求同样双工具起步）：本期
   否（子 agent 用 SUB-SYSTEM + 内置工具，subagent.rkt:116 现状），P2 评估。

## 8. v1 方案（废弃记录）

v1 把 dsv4-b 做成永久极简裁剪（4 工具、关压缩/goal/skills/插件），对齐的是
「Minimal 作为约束」的表面语义。经确认，Minimal 的价值在**首请求锚定**而非
持续约束（anchored-standard：promotion 后全量工具不损分），故弃 v1 取两阶段。
v1 中仍然成立并被 v2 继承的部分：档案注册方式、base 名 gating、provider 解析
前移、凭据共享、计价回退。
