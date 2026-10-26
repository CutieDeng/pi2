# dsv4-b-probes — Minimal 锚定分布差的受控探针

见 ../../design-dsv4b-minimal.md（本目录脚本产出其 §2 的实测）。

## 前置
把 DeepSeek key 写到 `.dskey`（本目录，git-ignore；勿提交）：
  racket -e '(require (file "../../src/providers.rkt")) (display (resolve-provider-token "deepseek" "default"))' > .dskey

## 探针
- probe.py — 单轮：给 (system, tools, task)，报思考/工具选择/最终文本。
    python3 probe.py --system-file sys-terse.txt --tools-file tools-min.json --task-file task-fileref.txt
- agent.py — 多轮：在 --workdir 隔离目录内**真执行**模型的工具调用，循环到收敛或轮数上限；
    记录轨迹、是否执行代码、最终诊断。到点强制收尾（撤工具要答）。
    python3 agent.py --system-file sys-terse.txt --tools-file tools-full.json --task-file task-lab1.txt --workdir lab1 --max-turns 6

## 关键发现（design-dsv4b-minimal.md §2/§6）
schema 决定调查信道(bash vs read_file)且与 prompt 无关；模型召回很早饱和却不肯闭合(过度自我验证)。
