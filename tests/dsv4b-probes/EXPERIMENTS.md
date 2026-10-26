# dsv4-b 探针实验记录（证据索引）

所有实验针对 **deepseek-v4-pro**（同一 anthropic 端点 `api.deepseek.com/anthropic`）。
「minimal mode」「standard mode」是**同一模型的不同请求形状**（工具 schema + system prompt），非不同模型/provider。
原始日志在 `logs/`。脚本：`probe.py`（单轮）/`agent.py`（多轮真执行）/`cot-compare.py`（跨模型）。
时间：2026-08-17。**样本小（n=2–3），结论按证据强度标注；含两处我自我更正的地方（见 ⚠）。**

---

## 日志清单（logs/）

| 文件 | 实验 | 配置 | 关键结论 | 状态 |
|---|---|---|---|---|
| `probe1.log` | Probe 1：内联 buggy 函数 review | 2×2(schema×prompt), 单轮, n=2 | 8/8 找到 bug(**质量天花板**);7/8 都 bash 复现→**执行冲动是基线,非 minimal 独有** | 有效 |
| `probe2.log` | Probe 2：引用文件 review | 2×2, 单轮, n=2 | **8/8 一致:min→bash, full→read_file,与 prompt 无关**→schema 决定信道 | **强信号** |
| `probe3.log` | Probe 3：多轮真执行 | min vs full, 多轮 | 两臂都执行代码;都不自终止。⚠ **workdir 被污染**(模型爬出去读我的日志)——方法失败 | ⚠作废 |
| `probe3b.log` | Probe 3b：干净 fixture 重跑 | min vs full, max-turns 12 | 仍不终止,10min 超时截断(4 run) | 有效但被下条修正 |
| `probe3c.log` | Probe 3c：强制收尾 | +撤工具强制作答 | 强制后仍 0 字(思考吃光 max_tokens)——测量 bug | 部分 |
| `probe-cli.log` | pi2-CLI 文本描述实验 | minimal schema 恒定, 文本描述 pi2, TEXT vs NOTEXT | **模型可靠使用文本描述的 `pi2 <sub>`(4/4);不告知也靠 ls 发现** | **强信号** |
| `cot.log` | 单个 TEXT/validate 的逐轮 CoT | minimal + pi2 | CoT 原文:turn1 就「using pi2 validate」;`pi2 validate` 权威裁决→收敛更快 | 有效 |
| `cotcmp.log` | ⚠跨模型 CoT 对比(**错的对比**) | v4-pro/flash/reasoner/chat, 单轮 | 四模型 CoT 语气/过度审议一致(家族共性)。⚠**但任务前提有误**(dedup 不触发 bug)+ **不该跨模型比** | ⚠作废 |
| `probe-mode.log` | **正确对比:v4-pro minimal vs standard** | 同模型, 良构任务, 单轮, n=2 | **反转:MINIMAL 直接作答(0工具,~2.5-5k思考);STANDARD 过度审议(~16-17k)+调工具不肯提交** | **主结果** |
| `inline.json` | 单次内联原始响应 | 无工具 | 证明 v4-pro 思考不可抑制(max_tokens 被思考吃光) | 佐证 |

---

## 怎么自己核验证据

- **schema 决定信道**（Probe 2）：
  `grep -A3 '=====' logs/probe2.log` —— 看每个 min-* 都 `→ bash`、每个 full-* 都 `→ read_file`。
- **pi2 CLI 文本描述被采纳**（pi2-CLI）：
  `grep -E '=====|pi2 ' logs/probe-cli.log` —— 看 TEXT/* 的 bash 命令里出现 `pi2 validate`/`pi2 skills`。
- **主结果 minimal vs standard 反转**（probe-mode）：
  `grep -E '=====|THINKING|TOOL CALLS|FINAL TEXT' logs/probe-mode.log` ——
  MINIMAL 的 `TOOL CALLS: 0` + `FINAL TEXT 597/485 chars`；STANDARD 的 `THINKING 17466/16111 chars` + `TOOL CALLS: 1` + `FINAL TEXT 0 chars`。
  CoT 原文里 STANDARD 自述 "The instruction says prefer verifying with tool when uncertain"——**证明是 rich prompt 的 verify 指令在驱动 grinding**。

## 两处我更正的地方（诚实记录）
1. **cotcmp.log 是错的对比**：我跨了模型(flash/reasoner/chat)，把模型身份和 mode 混淆；且任务前提有误
   （dedup 显式传 out，可变默认参数不经 dedup 触发，四模型都正确地卡在这个假前提上）。→ 换良构任务 + 固定 v4-pro，见 probe-mode.log。
2. **「minimal 锚定导致过度验证」被推翻**：probe-mode.log 显示相反——minimal 更干脆终止，
   是 standard 的 rich「先验证」prompt + 满桌工具在驱动 grinding。Probe 3/4 的不终止是坏任务+多轮循环叠出来的。

## 未坐实（待做）
- **2×2(工具×prompt)**：probe-mode 的 STANDARD 同时变了工具和 prompt，CoT 指向 prompt，但没单独隔离。
- **真·召回 A/B**：minimal-persistent+pi2CLI vs promote-to-full，seeded-bug + 硬早停，测召回率（良构任务下召回是天花板，需更难的任务）。
