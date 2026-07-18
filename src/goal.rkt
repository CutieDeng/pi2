#lang tstring racket
;; goal.rkt — Goal 模式 P1（design-goalmode.md）：外层驱动循环 + `--until` 验收 oracle + 进度 monitor。
;;
;; 把「手动驱动 dogfood 那几轮」变成一等能力：给定目标 + 机器可判定的验收命令，pi2 自主多轮推进，
;; 直到验收全过或轮数耗尽。progress monitor 盯验收信号,连 K 轮不降=困住→复用 escalate 升模型,
;; 升到顶仍不动就停下给人总结。核心原则:**终止只认验收 exit code,绝不让模型自判完成**。
;;
;; 非侵入:外层循环,内核 run-turn! 不改;与 retry/escalate/auto/cost/scoped-approve/session 复合。
;; 渲染(流式输出)由调用方(main)订阅 renderer 到 bus;本模块只负责驱动 + 状态行(经 emit)。

(require
 racket/string
 racket/port
 racket/pvector
 (file "model.rkt")
 (file "event.rkt")
 (file "loop.rkt")
 (file "session.rkt")
 (file "escalate.rkt")
 (file "plugin.rkt")
) ; end require

;; ---------------------------------------------------------------- 验收 oracle

(define VERIFY-TIMEOUT-SECS 300)

;; 跑一条 shell 验收命令(workdir 内)→ (values exit-code|'timeout combined-output)。
(define (run-verify cmd workdir)
  (parameterize ([current-directory workdir])
    (define-values (proc out in _err) (subprocess #f #f 'stdout "/bin/zsh" "-c" cmd))
    (close-output-port in)
    (define buf (open-output-string))
    (define pump (thread (lambda () (copy-port out buf))))
    (define done (sync/timeout VERIFY-TIMEOUT-SECS proc))
    (cond
      [(not done) (subprocess-kill proc #t) (thread-wait pump) (values 'timeout (get-output-string buf))]
      [else (thread-wait pump) (close-input-port out) (values (subprocess-status proc) (get-output-string buf))])
  ) ; end parameterize
) ; end define run-verify

;; 从验收输出估「失败量」(启发式,越小越好)：优先累加 failures=/errors=/failed= 的数字;
;; 找不到就数 FAIL/ERROR/Traceback/AssertionError 行。用于 monitor 判进展(见 design 开放问题#3)。
(define (failure-count output)
  (define nums (regexp-match* #px"(?i:failures|errors|failed)=(\\d+)" output #:match-select cadr))
  (cond
    [(pair? nums) (for/sum ([n (in-list nums)]) (or (string->number n) 0))]
    [else (length (regexp-match* #px"(?im:^(?:FAIL|ERROR)\\b|Traceback|AssertionError)" output))]
  ) ; end cond
) ; end define failure-count

;; 跑全部 until 命令 → (values all-pass? signal report)。
;;   signal：越小越好。= 失败命令数×1e6 + 各失败输出的 failure-count 之和(0=全过)。
;;   report：失败命令的输出摘要(喂回给模型下一轮)。
(define (run-oracle until-cmds workdir)
  (define results
    (for/list ([c (in-list until-cmds)])
      (define-values (code out) (run-verify c workdir))
      (list c code out)))
  (define failing (filter (lambda (r) (not (eqv? (cadr r) 0))) results))
  (define signal
    (+ (* 1000000 (length failing))
       (for/sum ([r (in-list failing)]) (failure-count (caddr r)))))
  (define report
    (string-join
     (for/list ([r (in-list failing)])
       f"$ {(car r)}  (exit {(cadr r)})\n{(clip-tail (caddr r) 1500)}")
     "\n\n"))
  (values (null? failing) signal report)
) ; end define run-oracle

;; 取字符串末尾 n 字符(验收输出通常尾部最相关:失败汇总)。
(define (clip-tail s n)
  (if (> (string-length s) n) (string-append "…\n" (substring s (- (string-length s) n))) s))

;; ---------------------------------------------------------------- 驱动循环

;; 每轮注入的提示词:目标 + 验收命令 + 上轮失败输出。
(define (goal-prompt goal until-cmds last-report first?)
  (string-append
   f"You are working autonomously toward a goal across multiple turns.\n\nGOAL: {goal}\n\n"
   "ACCEPTANCE — the goal is DONE only when ALL of these shell commands exit 0:\n"
   (string-join (for/list ([c (in-list until-cmds)]) f"  $ {c}") "\n")
   "\n\n"
   (if (and (not first?) last-report)
       f"The acceptance check currently FAILS. Most recent failing output:\n---\n{last-report}\n---\nFix these failures this turn. "
       "This is the first turn — start working toward the goal. ")
   "Make concrete progress now: write/edit files, run the checks yourself to confirm, and git commit when a milestone passes. Work autonomously; do NOT ask questions or wait for confirmation."
  ) ; end string-append
) ; end define goal-prompt

(define (goal-summary status st turns)
  (define u (agent-state-token-usage st))
  (string-append
   f"\n══ goal ended: {status} ══\n"
   f"turns: {turns} · messages: {(pvector-length (agent-state-history st))} · "
   f"tokens ↑{(usage-input-tokens u)} ↓{(usage-output-tokens u)}\n"))

;; 把 before→after 之间新增的历史与 usage 增量落盘(与 repl persist-turn! 同义,避免依赖 repl)。
(define (persist-goal-turn! sess before after)
  (define b (pvector-length (agent-state-history before)))
  (define a (pvector-length (agent-state-history after)))
  (define hist (agent-state-history after))
  (for ([i (in-range b a)]) (session-append-msg! sess (pvector-ref hist i)))
  (define ub (agent-state-token-usage before))
  (define ua (agent-state-token-usage after))
  (define delta (usage (- (usage-input-tokens ua) (usage-input-tokens ub))
                       (- (usage-output-tokens ua) (usage-output-tokens ub))))
  (unless (equal? delta usage-zero) (session-append-usage! sess delta))
) ; end define persist-goal-turn!

;; run-goal! : deps state session goal until-cmds max-turns host -> agent-state
;;   until-cmds：非空 shell 命令串列表(全 exit 0 = 完成)。
;;   #:stuck-k：连续多少轮验收信号不下降算「困住」(默认 2)→ 升模型;升到顶仍困 → 停。
;;   #:emit：状态行输出(默认 displayln);流式 turn 输出由调用方订阅 renderer。
;; 返回结束时的 state。终止:验收全过 | 困住升到顶仍无进展 | 轮数耗尽。
(define (run-goal! d st0 sess goal until-cmds max-turns host
                   #:stuck-k [K 2] #:emit [emit displayln])
  (define bus (deps-bus d))
  (define workdir (config-workdir (agent-state-config st0)))
  (define (dim s) f"\e[2m{s}\e[0m")
  (define result
    (let loop ([st st0] [turn 0] [prev-signal #f] [noprog 0]
               [rung (ladder-rung-of (config-model (agent-state-config st0)))]
               [last-report #f])
      (cond
        [(>= turn max-turns) (emit (goal-summary "MAX-TURNS reached (not done)" st turn)) st]
        [else
         (emit (dim f"\n── goal turn {(add1 turn)}/{max-turns} · model {(config-model (agent-state-config st))} ──"))
         ;; 1) 跑一轮(朝目标 + 上轮失败摘要)。turn 内异常不致命:记错,当作无进展。
         (define st1
           (with-handlers ([exn:fail? (lambda (e) (emit f"[turn error] {(exn-message e)}") st)])
             (run-turn! st (text-msg 'user (goal-prompt goal until-cmds last-report (zero? turn))) d)))
         (bus-drain! bus)
         (persist-goal-turn! sess st st1)
         ;; 2) 验收 oracle(ground truth)。
         (define-values (pass? signal report) (run-oracle until-cmds workdir))
         (cond
           [pass? (emit (goal-summary "DONE ✓ acceptance passed" st1 (add1 turn))) st1]
           [else
            ;; 3) progress monitor。
            (define progressed? (or (not prev-signal) (< signal prev-signal)))
            (define regressed? (and prev-signal (> signal prev-signal)))
            (define noprog* (if progressed? 0 (add1 noprog)))
            (define state-str (cond [progressed? "progressing"] [regressed? "REGRESSED"] [else "no progress"]))
            (emit (dim f"verify: signal {signal} · {state-str} · stuck {noprog*}/{K}"))
            ;; 4) 决策:困住→升模型(复用 escalate);升到顶仍困→停。
            (cond
              [(>= noprog* K)
               (define-values (st2 rung* esc)
                 (if (escalation-active? host) (escalate-step st1 host rung) (values st1 rung #f)))
               (cond
                 [esc (emit (dim f"monitor: stuck → escalate to {(car esc)} · thinking {(cdr esc)}"))
                      (loop st2 (add1 turn) signal 0 rung* report)]   ; 升级后重置 stuck 计数
                 [else (emit (goal-summary "STUCK — no progress and cannot escalate further" st1 (add1 turn))) st1])]
              [else (loop st1 (add1 turn) signal noprog* rung report)])])]
      ) ; end cond
    ) ; end let loop
  ) ; end define result
  result
) ; end define run-goal!

(provide
 run-goal!
 run-oracle failure-count run-verify
 goal-prompt
) ; end provide
