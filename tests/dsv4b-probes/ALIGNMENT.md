# dsv4-b 首请求 ↔ 上游 Minimal 预设 对齐核验

日期：2026-08-17。**证据均落地**：上游源码在 `upstream/`（抓自 deepseek-harness@master），
pi2 真实首请求在 `upstream/pi2-first-request.json`（`dump-first-request.rkt` 生成，复用 pi2 真实装配层）。
对比脚本产物 `upstream/_up_*.txt` / `upstream/_pi2_*.txt`。

## 上游权威文件（已抓）
- `upstream/apps_cli_config_agent-presets_minimal_agent.cordis.yml` — 预设组成（persona + 双工具 + 参数）
- `upstream/apps_cli_config_agent-presets_minimal_preset.yml` — 元信息
- `upstream/packages_shell_tool-bash-persistent_src_index.ts` — 持久 bash
- `upstream/packages_fs_tool-str-replace-editor_src_index.ts` — str_replace_editor

## 逐项对齐表

| 维度 | 上游 Minimal | pi2 dsv4-b（实测 dump） | 状态 |
|---|---|---|---|
| system prompt | `"You are a helpful software engineer assistant."`（`complete:true`） | ANCHOR-SYSTEM 逐字同 | ✅ |
| **运行时上下文注入** | **`includeRuntimeContext:false`（明确抑制）** | 裸用户消息，无 cwd/env/文件树注入 | ✅ |
| 首个 user turn | 裸任务（无模板、无环境块） | 裸任务（`message->anthropic`） | ✅ |
| 工具集 | persistent-bash + str_replace_editor | 同（恰两把） | ✅ |
| 工具顺序 | bash, editor | bash, editor（registry 按名排序巧合一致） | ✅ |
| **bash description** | **8 bullets** | **6 bullets——缺「无互联网」「apt/pip 镜像」两条** | ❌ **偏离** |
| editor description | `DEFAULT_DESCRIPTION` | 逐字一致 | ✅ |
| editor command enum | view/create/str_replace/insert（**无 undo_edit**） | 同 | ✅ |
| editor params | command/path/file_text/old_str/new_str/insert_line/view_range | 同 | ✅ |
| bash timeout | 300000ms | 300s | ✅ |
| editor maxOutputChars | 16000 | 16000 | ✅ |
| `cache_control` 标记 | 无（上游预设未见） | pi2 加 `ephemeral`（system/末条 msg/末个 tool） | ⚠️ 偏离（模型惰性，缓存用） |
| temperature / reasoningEffort | **不在预设**——`agent-default-model.reasoningEffort?`（可选，host/agent 级）；基准调用层设 `max` | pi2 dsv4-b 默认 max（本会话补上） | ➖ **不属预设对齐面** |

## 采样/推理参数在上游哪里设（(b) 的结论，源码证据）
- **不属 Minimal 预设**：`note-rl-composition.md` line 19「model routing … remain host-owned」；预设只 own persona+工具+策略。
- **`reasoningEffort` 是 agent 级模型选择**：`agent-default-model_index.ts` — `reasoningEffort?: string`「Adapter-owned reasoning effort, or provider/default behavior when absent」。**可选**，基准把它设成 `max` 是调用层选择。
- **`max` 的确切请求字节是「每模型声明」的**：`llm-pi-ai_catalog.ts` — `reasoningEfforts: {level → wire}`（`efforts[level]` 是派发给 provider 的 wire 值），非固定 token 预算。**要 deepseek-v4-pro 的 catalog 条目（在 pi-ai builtins 包）才知 `max` 具体派发什么。**
- **temperature** 预设/catalog 结构里未出现 → pi-ai adapter/provider 默认，非预设一部分。

### 残留未知（API 语义层，非预设层）
pi2 用 anthropic 原生 `thinking:{budget_tokens:24576}`+`temperature:1` 表示 max；上游经 pi-ai adapter 派发**模型声明的 wire 值**。
两者对 `max` 是否等价，需 deepseek-v4-pro 的 catalog 条目或 DeepSeek API 文档——**这是 API 语义层的事，不影响预设（首请求工具/prompt）层的对齐**。

## 结论
- **答用户前问**：上游 `includeRuntimeContext:false` 从源码**确证**首请求不注入环境上下文 → pi2 裸用户消息**对齐**。
- **唯一实质偏离 = bash description 缺 2 条 bullet**（design-dsv4b.md §7.5 的刻意取舍：那两条在本地为假会误导模型拒绝联网命令）。
  按锚定论「条件在首请求字节」，这**可能削弱锚定**。**且逃生门未实现**——`bash-persistent.rkt` 硬删，无 `PI_DSV4B_BASH_DESC=verbatim` 开关。
- **待核**：采样/推理参数（temperature/thinking）不在预设文件里，需查上游 CLI 调用层怎么设 `reasoningEffort`；
  以及 `cache_control` 是否影响锚定（应为模型惰性，但严格保真需确认上游是否也用 prompt caching）。

## 对实验的直接含义
要跑**保真的锚定实验**，得先给 pi2 一个 **verbatim bash 开关**（恢复 8 bullets），否则测的 minimal 首请求与 RL 分布差 2 条 bullet。
建议：`bash-persistent.rkt` 加 `PI_DSV4B_BASH_DESC=verbatim`（或探针脚本直接注入逐字描述）。
