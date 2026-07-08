# pi++

基于 Racket 的 LLM agent。终端内运行，支持流式输出、工具调用循环、`.rktd` 原生会话持久化、
上下文压缩、权限门控与子 agent。设计文档见 [design.md](design.md)。

## 依赖

- 增强版 Racket（`#lang racket-tstring`，含 `f""` 字符串模板、`pvector`、`racket/intmap`）。
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

# 恢复历史会话
racket main.rkt --resume sessions/20260709-1030-8905.rktd
```

远程端点用环境变量 `PI_API_KEY` 提供密钥。

### 斜杠命令

`/help` `/quit` `/clear` `/usage` `/compact` `/history` `/model <id>`

### 权限模式

| 模式 | read-only | mutating | dangerous |
|---|---|---|---|
| `strict` | 直通 | 询问 | 询问 |
| `normal`（默认） | 直通 | 直通 | 询问 |
| `yolo` | 直通 | 直通 | 直通 |

答 `a(lways)` 的工具记入 `~/.pi2-permissions.rktd`，跨会话生效。

## 内置工具

`bash` · `read_file` · `write_file` · `edit_file` · `glob` · `grep` · `spawn_agent`（子 agent）。

## 会话格式

transcript 是 `.rktd` datum 流（prefab struct 的 `write`/`read` 往返），既是机器真相源，
也可直接阅读、编辑、`read` 进 REPL 调试。重放复用运行时同一套状态迁移函数。

## 测试

```sh
./run-tests.sh          # 离线单测（7 套，无需 LLM）
./run-tests.sh --live   # 追加对 LM Studio gemma 的真机验收（3 套）
```

## 模块

```
main.rkt      入口装配          loop.rkt      agent 主循环
model.rkt     核心数据 (prefab) context.rkt   token 估算/裁剪/compact
rktd.rkt      datum-log 流式读写 session.rkt   .rktd 持久化/流式重放
event.rkt     事件总线          permission.rkt 权限门控
provider.rkt  流式 LLM 客户端    repl.rkt      终端交互
stream.rkt    SSE/accumulator   subagent.rkt  spawn_agent
tools/        bash/file/search/builtin
```
