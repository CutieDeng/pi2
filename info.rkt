#lang info
;; pi++ 单集合包（collection = pi2）。
;; 安装：在本目录 `raco pkg install`（或 `raco pkg install --link <dir>`）。
;; 运行：`racket -l pi2`（等价旧 `racket main.rkt`）；传参用 `racket -l pi2 -- <args>`，
;;      例如 `racket -l pi2 -- --provider deepseek --list`。
;; 单集合包中，包根目录即集合目录：`(require pi2)` → 本目录 main.rkt（其 module+ main 运行）。

(define collection "pi2")
(define version "0.1.0")
(define pkg-desc "pi++ — a Racket LLM coding agent (local LM Studio + multi-provider, TUI/RPC/plugins).")
(define pkg-authors '(cutiedeng))

;; 运行期依赖：tstring（#lang tstring racket）。net（http-client/url）、racket/sandbox、
;; racket/pvector · racket/intmap 均由本机增强版 racket 核心 collects 提供（含于 base）。
;; 移植到「完整发行版」时如缺 net/sandbox，可自行补 "net-lib" "sandbox-lib"。
(define deps '("base" "tstring"))
(define build-deps '("rackunit-lib"))

;; 安装即生成 `pi2` 可执行入口（等价 `racket -l pi2`；直接 `pi2 --provider deepseek …`）。
(define racket-launcher-names '("pi2"))
(define racket-launcher-libraries '("main.rkt"))

;; 运行期动态加载/纯资源目录，排除出 AOT 编译：
;;   examples/plugins 用运行时 dynamic-require 加载；data/cache 是会话/缓存产物。
;; 避免 raco setup 去编译示例插件（含相对路径 #lang）或触碰会话数据。
(define compile-omit-paths '("examples" "plugins" "data" "cache"))
