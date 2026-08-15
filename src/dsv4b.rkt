#lang tstring racket
;; dsv4b.rkt — dsv4-b 档案：Minimal 锚定 + 两阶段解锁（design-dsv4b.md §2）
;;
;; 机制：V4-Pro 强烈 condition 在**首个请求的工具 schema 目录**上（dsh-anchored-standard
;; 实证）。dsv4-b 启动时只暴露 bash + str_replace_editor 两个 schema 与 Minimal 原句
;; system prompt（锚定态）；首个 durable 事件后一次性「promotion」：全量工具灌回
;; registry + 解锁段追加进 system prompt——两者同边界，prefix cache 只断一次。
;;
;; 状态刻意不进 prefab config（遵循 auto/escalate 惯例，保 .rktd 回放兼容）：
;; promote-on / payload / promoted 均为进程级 box；resume 场景由 history 重推。

(require
 racket/string
 (file "model.rkt")
 (file "plugin.rkt")                    ; host-current-provider / provider-base-name
) ; end require

;; Harness Minimal 预设的完整 system prompt，逐字
;; （deepseek-harness: apps/cli/config/agent-presets/minimal/agent.cordis.yml persona.text）。
(define ANCHOR-SYSTEM "You are a helpful software engineer assistant.")

(define DSV4B-BASE "dsv4-b")

;; 解锁段的标识行：promotion 幂等的判据（resume 重推时防重复追加）。
(define UNLOCK-MARKER "## pi++ extended toolset")

;; ---------------------------------------------------------------- 进程级状态

;; 促迁触发：'tool-call（默认，寒暄不耗锚定）| 'either（首条纯文本回复亦促迁，上游默认）。
(define promote-on-box (box 'tool-call))
(define (dsv4b-promote-on) (unbox promote-on-box))
(define (set-dsv4b-promote-on! v)
  (unless (memq v '(tool-call either))
    (error 'set-dsv4b-promote-on! "expected 'tool-call or 'either, got ~a" v))
  (set-box! promote-on-box v)
) ; end define set-dsv4b-promote-on!

;; payload：无参 thunk，执行 registry 副作用（灌全量工具）并返回解锁段文本。
;; 仅 dsv4-b 启动装配（main.rkt）设置；#f = 本进程非锚定启动，促迁永不触发。
(define payload-box (box #f))
(define (set-dsv4b-payload! thunk) (set-box! payload-box thunk))

(define promoted-box (box #f))
(define (dsv4b-promoted?) (unbox promoted-box))

;; 测试用：复位全部进程级状态。
(define (dsv4b-reset!)
  (set-box! promote-on-box 'tool-call)
  (set-box! payload-box #f)
  (set-box! promoted-box #f)
) ; end define dsv4b-reset!

;; ---------------------------------------------------------------- 判定

;; 当前 provider base 是否 dsv4-b（镜像 escalation-active? 的按名 gating）。
(define (dsv4b-active? host)
  (and host
       (string=? (provider-base-name (host-current-provider host)) DSV4B-BASE))
) ; end define dsv4b-active?

;; history 中是否已有工具调用（resume 重推促迁的判据）。
(define (history-has-tool-call? msgs)
  (for/or ([m (in-list msgs)])
    (and (eq? (message-role m) 'assistant) (pair? (message-tool-uses m)))
  ) ; end for/or
) ; end define history-has-tool-call?

;; ---------------------------------------------------------------- 促迁

;; 首个 durable 事件处调用（loop.rkt）。trigger: 'tool-call | 'assistant-message。
;; 命中则执行 payload（registry 副作用）、把解锁段追加进 st 的 system-prompt，
;; 返回 (values st* 提示串)；未命中原样 (values st #f)。不可逆、幂等。
(define (dsv4b-maybe-promote! host st trigger)
  (cond
    [(or (unbox promoted-box)
         (not (unbox payload-box))
         (not (dsv4b-active? host)))
     (values st #f)]
    [(and (eq? trigger 'assistant-message) (eq? (dsv4b-promote-on) 'tool-call))
     (values st #f)]
    [else
     (set-box! promoted-box #t)
     (define addendum ((unbox payload-box)))
     (define cfg (agent-state-config st))
     (define sys (or (config-system-prompt cfg) ""))
     (define st*
       (if (string-contains? sys UNLOCK-MARKER)          ; resume 存档已含解锁段
           st
           (struct-copy agent-state st
                        [config (struct-copy config cfg
                                             [system-prompt (string-append sys addendum)])])))
     (values st* "dsv4-b promoted: full pi++ toolset unlocked")
    ] ; end else
  ) ; end cond
) ; end define dsv4b-maybe-promote!

;; 解锁段文本：pi++ 工具须知（浓缩自 DEFAULT-SYSTEM 的行为约束）+ 技能清单 + 项目指令。
;; 措辞为「追加能力」而非更换人设——锚定句保持在 system prompt 最前。
(define (dsv4b-unlock-addendum skills-add proj-add)
  (string-append
   "\n\n" UNLOCK-MARKER "\n"
   "Beyond bash and str_replace_editor, you now also have: read_file, write_file, edit_file "
   "(workdir-relative file ops; edit_file does unique-match string replacement with batch support), "
   "glob and grep (file search), git (repository inspection), and spawn_agent (delegate a "
   "self-contained subtask to a sub-agent). Use whichever tool fits; the editor remains available.\n"
   "Verify, don't guess: when uncertain about file contents, symbols, project layout, or command "
   "output, check with a tool first and answer from what you observed.\n"
   skills-add
   proj-add
  ) ; end string-append
) ; end define dsv4b-unlock-addendum

(provide
 ANCHOR-SYSTEM
 DSV4B-BASE
 UNLOCK-MARKER
 dsv4b-promote-on set-dsv4b-promote-on!
 set-dsv4b-payload!
 dsv4b-promoted?
 dsv4b-reset!
 dsv4b-active?
 history-has-tool-call?
 dsv4b-maybe-promote!
 dsv4b-unlock-addendum
) ; end provide
