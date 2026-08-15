# design-execguard.md — 受控执行平面：shparse + cmdscan + read-only 模式

日期：2026-08-15，v2 于 2026-08-16。状态：已实现。缘起：dsv4-b A/B 的 review
任务暴露「只许读」缺口（评测当时用 副本+chmod+哈希审计 外部兜底，见
design-dsv4b.md §5.3.1）。

**v2（内置解析器，all-in-one）**：v1 判定器是词法保守扫描；v2 新增
`src/shparse.rkt`——纯 Racket 递归下降的 bash 子集解析器（零外挂进程），
cmdscan 改为 AST 遍历。fail-closed 不变（解析失败→opaque），误拒大幅收窄：

- 词内展开结构化：`$(…)`/`` ` ``/`<(…)`/`${…}`/`$((…))` 的内嵌命令体收集为
  subs **递归判级**——`echo $(pwd)` 只读放行、`echo $(rm x)` 精确拒绝；
- heredoc 正文正确吞掉：引号定界=字面量安全；未引号定界提取其中的命令替换判级；
- 复合命令结构化：子 shell/分组/if/while/until/for/case/`[[ ]]`/`(( ))`
  逐体遍历，复合级重定向照查；
- 函数：定义不执行不计；调用点按定义体判级；**同名遮蔽白名单**
  （`ls(){ rm -rf /; }; ls` → mutating）；自递归经作用域剔除收敛到默认拒绝；
- 包装命令剥壳（time/timeout/nohup/nice/stdbuf/watch/command/builtin/env/!），
  `command -v` 仅查询直接放行；`find -exec/-ok` 载荷单独判级（`-exec grep`
  放行、`-exec rm` 拒绝），`-delete/-fprint*` 拒绝；
- 特判命令（git/find/sed/sort/uniq/shell -c）出现**动态参数即 opaque**——
  防经变量走私 `-i`/`-o`/`-delete` 等危险旗标。

解析器子集边界（超出→报错→opaque）：select/coproc/扩展 glob 无专门支持；
`$(( ))` 内视作无执行副作用但其中的 `$(…)` 仍被提取判级。原 v1 词法扫描器
已整体废弃。

## 0. 问题与定位

让 agent 审查/分析代码时需要「拒绝一切写、无人值守不询问」。防写能力放哪层？

- **意图平面**（tool-permission-level × permission.rkt 矩阵）：语义干净、模型
  收到可自我修正的拒绝理由；但它信任工具自我声明——bash 声明 'dangerous，
  实际写不写静态未知，单靠它给不出保证。
- **外部文件平面**（chmod/副本/worktree/sandbox-exec）：不信任任何工具；但
  粒度粗、chmod 对同 uid 可逆、错误以裸 EACCES 冒出，模型体验差。
- 本设计取用户提议的第三条路：**把 bash 从不透明逃逸口升级为受控执行平面**——
  框架在执行前拿到实际要执行的内容（含被调脚本的递归展开），静态判级，判不了
  的默认拒绝。「真实 bash」被间接禁止：模型手里的 `bash` 工具实为守门执行器。

## 1. src/cmdscan.rkt — 命令静态判级

shell 静态分析一般不可判定（rice 定理级别），故本模块是**默认拒绝的白名单**：

- 判级 `'read-only | 'mutating | 'opaque`（opaque=不可判，按最危险处理）。
- 引号感知单遍扫描：切片段（`; & | 换行`）与词；粘连重定向（`hi>out`、
  `2>&1`、`>>x`）切分/保词；词首 `#` 注释吞行；`\` 续行。
- opaque 触发：`` ` ``、`$(…)`（`$((…))` 算术放行）、`<(…)/>(…)`、heredoc
  （`<<<` herestring 放行）、eval/exec/trap/alias/command/builtin、xargs/
  nohup/timeout/watch、通用解释器（python/perl/node/…）、make 系、命令位
  变量 `$CMD`、`find -exec/-delete`、`awk` 含 `system(`/`>`。
- mutating：出向重定向落真实文件（`/dev/null`、`2>&1` 豁免；目标含 `$` →
  opaque）、`sed -i`、git 非只读子命令、以及**一切白名单外命令**（默认拒绝）。
- read-only 白名单：ls/cat/grep/find(无 exec)/sed(无 -i)/diff/… + shell 状态
  类（cd/export/set/…，只改 shell 态不改盘）+ git 只读子命令
  （`git-args-read-only?`，与 git 工具共用判据）。
- **脚本递归展开**（用户核心提议）：`bash x.sh` / `sh -c '…'` / `source` /
  `./x.sh` / 含 `/` 或 `.sh` 的调用 → 解析路径读内容递归判级；深度 ≤4、
  循环防护；非 shell shebang / 读不到 / 过大 → opaque；二进制仅当「绝对路径
  且 basename 在白名单」（/bin/ls）放行，本地同名伪装仍拒。verdict 附
  scripts 快照（(path . content)），框架据此留**取证/审计**记录。
- TOCTOU 声明：执行仍走原路径，检查与执行间文件可被并发改动；本模块做判定
  与取证，不做原子保证（P2 可选：把执行改到快照副本上）。
- 坑：tstring reader 下 `#\"` 字面量破坏字符串扫描——用 `(integer->char 34)`。

## 2. permission.rkt — 'read-only 模式

`--mode read-only`（第五种 permission-mode）。`read-only-decision`：

| 工具 | 判定 |
|---|---|
| level='read-only | 放行 |
| bash | `classify-command`：'read-only 放行；否则拒绝并附 cmdscan reason |
| git | `git-args-read-only?` |
| str_replace_editor | 仅 `view` 子命令 |
| 其余 mutating/dangerous | 拒绝 |

拒绝**不经 asker**（无人值守友好），理由随现有 deny 通道回给模型（loop.rkt
的 "User denied permission… reason: …"），模型可改写命令自我修正。
yolo/normal/strict/auto 矩阵零改动（回归用例覆盖）。

## 2.5 特判命令 = 攻击面：allowlist over denylist（v2.1，2026-08-16）

白名单命令里凡有「危险子能力」的（git/find/sed/awk/sort/uniq/shell -c），都要
给它的 argv 契约编码规则——**这本身是随命令版本增长的攻击面**。教训（审计中
真实发现并修复）：

- **黑名单必然漏**：find 原按「查 -delete/-exec，其余放行」（denylist），
  任何未列的写谓词（未来 find 新增、或我遗漏的 -fls 之类）都当只读溜过。
  已翻为 **allowlist**：只有全部谓词都在 `FIND-READ-ONLY-TOKENS`（小而稳定的
  「只读谓词」表）才放行，任何未知 dash-token → opaque。未来新谓词自动 fail-closed。
- **find `-exec` 忠实建模（v2.2，2026-08-16）**：agent 对 `find -exec` 有强
  偏好，故不再一概拒绝，而是**对齐 GNU findutils `find/parser.c: insert_exec_ok`**
  精确建模并递归判载荷命令。规则逐条源码对齐：`;` 须独立整词、`+` 仅当紧邻前一
  arg 恰为 `{}` 才是终止符（Savannah #66365）、`{}` 在命令名位=执行匹配文件本身
  → opaque、参数位 `{}` 是数据占位、无终止符 → opaque、`-delete`/`-fprint*`/
  `-fls` 精确判 mutating。载荷命令走**同一个** dispatch-simple，故 `-exec grep`
  放行、`-exec rm`/`-execdir rm`/`-ok rm` 判写、`-exec sh -c '…'` 深入 -c 体、
  多个 `-exec` 子句取最坏。递归依据仍在「我掌控的解析器 + 有测试覆盖的 find 模型」
  内，非黑箱假设。未知 find 谓词仍 opaque（fail-closed 兜底）。
- **仍不建模的嵌入语言**：sed/awk `-f`（程序体在外部文件）、sed 内联 `w` /
  awk `system()`/`>`/`|`——这些是另一门完整语言，一律 opaque 拒绝。与 find `-exec`
  的区别：find 的 `-exec` 载荷本身就是一条 shell 命令（我有解析器），而 sed/awk
  程序不是；前者可忠实建模，后者只能拒。
- 保留 denylist 的仅 sort（-o/--output）、uniq（输出位）——其写机制**单一且
  文档封闭**，denylist 完备。凡机制可扩展的命令一律 allowlist。

原则：**认识的安全子集才放行，其余 opaque**；命令内嵌另一语言就拒绝而非建模。

## 2.6 判决可解释性 + 调试（v2.3，2026-08-16）

隔离能力只是一半；另一半是**让模型读懂判决并自我修正**、让开发者能排查。

- **因果链面包屑**：`within` 组合子在每个递归边界（`$(…)`、heredoc、`sh -c`、
  `script <path>`、`find -exec`）把上下文 frame 拼进非只读判决的 reason，
  形成 `find -exec ▸ sh -c ▸ command not in read-only allowlist: rm`——模型
  一眼看到是命令树哪一层、哪条命令被否，而非光秃秃一个 `rm`。只读判决不加噪声。
- **结构化拒绝**（`read-only-deny`）：给模型的 deny 文本分四段——`tool`（谁越界）
  / `why`（因果链）/ `rule`（命中哪条策略）/ `fix`（可操作的替代写法）+ 「勿原样
  重试」。bash/git/editor/其它工具各有针对性的 rule+fix。目的：agent 据此换写法，
  而非对同一调用反复撞墙。
- **调试通道**：`verdict->debug` 打印级别 + 因果链 + 递归检查过的脚本快照清单；
  `PI_CMDSCAN_DEBUG` 置位时 read-only 判定点把它 eprintf 到 stderr，供开发者
  回答「为什么这条被判越界」。快照清单同时是审计取证面（实际读过哪些脚本）。

## 3. 分层结论（与外部平面的关系）

拒绝的解释权在意图平面（本设计），强制力对文件工具在意图平面已足（write/
edit/editor 全经 permission-check）；bash 经 cmdscan 达到「可证只读才放行」。
残余风险=cmdscan 白名单命令自身的写副作用 bug 与 TOCTOU——对抗性场景仍应叠
外部平面（worktree/副本 + 哈希审计，或 sandbox-exec），见 dsv4b-bench review
集的三层做法。

## 4. 测试

tests/cmdscan-test.rkt（10 组）：分类表（管道/重定向/粘连/豁免/边界）、git
判据、脚本递归（嵌套/循环/非 shell shebang/缺失/快照内容）、'read-only 模式
端到端（bash/git/editor/文件工具、never-ask 断言）、旧矩阵回归。
