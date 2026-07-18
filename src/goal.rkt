#lang tstring racket
;; goal.rkt — Goal 模式（design-goalmode.md）：外层驱动循环 + `--until` 验收 oracle + 进度 monitor。
;; P1：驱动循环 + oracle + 线性 monitor（困住→复用 escalate 升模型）。
;; P2：持久 PLAN.md checklist（模型维护、驱动解析/注入/展示，防漂移）+ replan（困住时换策略再试）
;;     + regressed 反馈（信号变差→提示最近更优 commit，让模型自己用 git 工具回退，驱动不做破坏性 git 手术）
;;     + `--budget` 成本熔断（pricing 估算,超预算即停）。
;;
;; 核心原则不变：**终止只认验收 exit code**，绝不让模型自判完成。PLAN.md 只是工作记忆/进度展示，
;; 不是终止权威（模型可能勾错）。非侵入：外层循环，内核 run-turn! 不改。

(require
 racket/string
 racket/port
 racket/file
 racket/pvector
 (file "model.rkt")
 (file "event.rkt")
 (file "loop.rkt")
 (file "session.rkt")
 (file "escalate.rkt")
 (file "pricing.rkt")
 (file "worktree.rkt")                  ; P4.1：git worktree 隔离原语
 (file "plugin.rkt")
) ; end require

;; ---------------------------------------------------------------- 验收 oracle

(define VERIFY-TIMEOUT-SECS 300)

;; 跑一条 shell 命令(workdir 内)→ (values exit-code|'timeout combined-output)。
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

;; 从验收输出估「失败量」(越小越好)：优先累加 failures=/errors=/failed=,否则数 FAIL/ERROR/Traceback 行。
(define (failure-count output)
  (define nums (regexp-match* #px"(?i:failures|errors|failed)=(\\d+)" output #:match-select cadr))
  (cond
    [(pair? nums) (for/sum ([n (in-list nums)]) (or (string->number n) 0))]
    [else (length (regexp-match* #px"(?im:^(?:FAIL|ERROR)\\b|Traceback|AssertionError)" output))]
  ) ; end cond
) ; end define failure-count

;; 跑全部 until 命令 → (values all-pass? signal report)。signal = 失败命令数×1e6 + 各失败量之和。
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
     (for/list ([r (in-list failing)]) f"$ {(car r)}  (exit {(cadr r)})\n{(clip-tail (caddr r) 1500)}")
     "\n\n"))
  (values (null? failing) signal report)
) ; end define run-oracle

(define (clip-tail s n)
  (if (> (string-length s) n) (string-append "…\n" (substring s (- (string-length s) n))) s))

;; ---------------------------------------------------------------- 持久 plan（PLAN.md，模型维护）

;; PLAN.md 条目建模成 DAG 任务（P4.0）。行形如：
;;   - [ ] {id} 描述 (needs: id1, id2) (files: a.py, b.py)
;; {id}/needs/files 都可选：无注解 → 退化为 P2 线性 checklist（deps 空 → 就绪=未完成，文档序）。
(struct plan-task (id desc deps files done?) #:prefab)

(define (extract-list s key)
  (define m (regexp-match (pregexp (string-append "\\(" key ":\\s*([^)]*)\\)")) s))
  (if m (filter non-empty-string? (map string-trim (regexp-split #px"[,\\s]+" (cadr m)))) '()))

;; 单行 → plan-task 或 #f（非条目行）。
(define (parse-task-line line)
  (define m (regexp-match #px"^\\s*[-*]\\s*\\[([ xX])\\]\\s*(.*)$" line))
  (cond
    [(not m) #f]
    [else
     (define done? (not (string=? (cadr m) " ")))
     (define rest (caddr m))
     (define idm (regexp-match #px"^\\{([^}]+)\\}\\s*(.*)$" rest))
     (define id (and idm (string-trim (cadr idm))))
     (define rest2 (if idm (caddr idm) rest))
     (define deps (extract-list rest2 "needs"))
     (define files (extract-list rest2 "files"))
     (define desc (string-trim (regexp-replace* #px"\\((?:needs|files):[^)]*\\)" rest2 "")))
     (plan-task id desc deps files done?)]
  ) ; end cond
) ; end define parse-task-line

(define (parse-dag workdir)
  (define f (build-path workdir "PLAN.md"))
  (if (file-exists? f) (filter values (map parse-task-line (file->lines f))) '()))

(define (task-done-by-id? tasks id)
  (for/or ([t (in-list tasks)]) (and (equal? (plan-task-id t) id) (plan-task-done? t))))
(define (known-id? tasks id) (for/or ([t (in-list tasks)]) (equal? (plan-task-id t) id)))

;; 就绪 = 未完成 且 所有 deps 都是「已完成的已知任务」。
(define (task-ready? tasks t)
  (and (not (plan-task-done? t))
       (for/and ([d (in-list (plan-task-deps t))]) (task-done-by-id? tasks d))))
(define (dag-ready tasks) (filter (lambda (t) (task-ready? tasks t)) tasks))

;; 当前 active = 首个就绪任务的描述（依赖已满足者优先；无则 #f）。
(define (dag-active tasks) (let ([r (dag-ready tasks)]) (and (pair? r) (plan-task-desc (car r)))))
(define (dag-counts tasks) (values (length (filter plan-task-done? tasks)) (length tasks)))

;; 依赖图是否有环（DFS，仅对已知 id 的边）。
(define (dag-has-cycle? tasks)
  (define id->deps (make-hash))
  (for ([t (in-list tasks)] #:when (plan-task-id t))
    (hash-set! id->deps (plan-task-id t) (filter (lambda (d) (known-id? tasks d)) (plan-task-deps t))))
  (define visiting (make-hash)) (define visited (make-hash))
  (define (dfs id)
    (cond
      [(hash-ref visited id #f) #f]
      [(hash-ref visiting id #f) #t]              ; 回边 → 环
      [else
       (hash-set! visiting id #t)
       (define c (for/or ([d (in-list (hash-ref id->deps id '()))]) (dfs d)))
       (hash-set! visiting id #f) (hash-set! visited id #t) c]))
  (for/or ([id (in-hash-keys id->deps)]) (dfs id)))

;; DAG 健康问题（喂回模型修）：未知依赖 id、依赖环。空 = 无问题。
(define (dag-issues tasks)
  (append
   (for*/list ([t (in-list tasks)] #:unless (plan-task-done? t)
               [d (in-list (plan-task-deps t))] #:unless (known-id? tasks d))
     f"task \"{(plan-task-desc t)}\" depends on unknown id: {d}")
   (if (dag-has-cycle? tasks) (list "PLAN.md has a dependency cycle among tasks — break it") '())))

;; 兼容旧签名：(values done total active-desc)。基于 DAG（无 deps 时同 P2 行为）。
(define (read-plan workdir)
  (define tasks (parse-dag workdir))
  (define-values (d t) (dag-counts tasks))
  (values d t (dag-active tasks)))

;; workdir 若是 git 仓库，返回当前短 HEAD（否则 #f）。用于记录“最接近通过”的 checkpoint。
(define (git-head workdir)
  (define-values (code out) (run-verify "git rev-parse --short HEAD 2>/dev/null" workdir))
  (if (and (eqv? code 0) (non-empty-string? (string-trim out))) (string-trim out) #f))

;; 本轮成本（USD）：本轮 usage 增量 × 结束时模型价（未知模型→0）。
(define (turn-cost before after)
  (define ub (agent-state-token-usage before))
  (define ua (agent-state-token-usage after))
  (define delta (usage (- (usage-input-tokens ua) (usage-input-tokens ub))
                       (- (usage-output-tokens ua) (usage-output-tokens ub))))
  (or (estimate-cost (config-model (agent-state-config after)) delta) 0.0))

;; ---------------------------------------------------------------- 每轮提示词

(define (goal-prompt goal until-cmds last-report first? pdone ptotal pactive issues replan? regr? best-commit)
  (string-append
   f"You are working autonomously toward a goal across multiple turns.\n\nGOAL: {goal}\n\n"
   "ACCEPTANCE — the goal is DONE only when ALL of these shell commands exit 0:\n"
   (string-join (for/list ([c (in-list until-cmds)]) f"  $ {c}") "\n")
   "\n\n"
   ;; 持久 plan：有则报进度+当前任务；无则要求建 PLAN.md checklist（可选 DAG 注解）。
   (if (> ptotal 0)
       f"PLAN.md progress: {pdone}/{ptotal} done. Current task: {(or pactive "(all items checked — confirm acceptance)")}. Keep PLAN.md updated (check off `- [x]` as you finish each item).\n\n"
       (string-append
        "Maintain a PLAN.md checklist in the working directory: decompose the goal into `- [ ]` items and check them off (`- [x]`) as you complete them. Create it now if absent. "
        "You MAY annotate a task with `{id}`, dependencies `(needs: id1, id2)`, and file ownership `(files: a.py, b.py)`; the driver works dependency-ready tasks first.\n\n"))
   ;; DAG 健康问题（未知依赖/环）→ 让模型修 PLAN.md。
   (if (pair? issues)
       (string-append "PLAN.md has structural issues to fix:\n"
                      (string-join (for/list ([i (in-list issues)]) f"  - {i}") "\n") "\n\n")
       "")
   ;; replan：困住时逼它换策略。
   (if replan?
       "You appear STUCK — acceptance still fails after escalating the model. STOP repeating the same approach: step back, REWRITE PLAN.md with a different strategy, and try a genuinely different tack this turn.\n\n"
       "")
   ;; regressed：信号变差，指最近更优 commit，让它自己用 git 工具回退（驱动不做 git 手术）。
   (if (and regr? best-commit)
       f"WARNING: you REGRESSED — checks are further from passing than before. The last better checkpoint was git commit {best-commit}. Consider reverting to it with the git tool, or carefully undo your last change.\n\n"
       "")
   ;; 上轮验收失败输出（喂回 ground truth）。
   (if (and (not first?) last-report)
       f"Acceptance currently FAILS. Most recent failing output:\n---\n{last-report}\n---\n"
       "This is the first turn. ")
   "Make concrete progress now: write/edit files, run the checks yourself to confirm, git commit when a milestone passes. Work autonomously; do NOT ask questions or wait for confirmation."
  ) ; end string-append
) ; end define goal-prompt

(define (goal-summary status st turns spent)
  (define u (agent-state-token-usage st))
  (string-append
   f"\n══ goal ended: {status} ══\n"
   f"turns: {turns} · messages: {(pvector-length (agent-state-history st))} · "
   f"tokens ↑{(usage-input-tokens u)} ↓{(usage-output-tokens u)}"
   (if (> spent 0) f" · ~{(format-cost spent)}" "")
   "\n"))

;; before→after 新增历史 + usage 增量落盘（与 repl persist-turn! 同义，避免依赖 repl）。
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

;; ---------------------------------------------------------------- 驱动循环

;; run-goal! : deps state session goal until-cmds max-turns host -> agent-state
;;   #:stuck-k    连续多少轮验收信号不降算困住(默认 2)→ 升模型
;;   #:max-replans 升到顶仍困后,最多 replan 几次换策略(默认 1),用尽才停
;;   #:budget     USD 成本上限(#f=无);累计超限即停
;;   #:emit       状态行输出(流式 turn 输出由调用方订阅 renderer)
(define (run-goal! d st0 sess goal until-cmds max-turns host
                   #:stuck-k [K 2] #:max-replans [max-replans 1] #:budget [budget #f]
                   #:emit [emit displayln])
  (define bus (deps-bus d))
  (define workdir (config-workdir (agent-state-config st0)))
  (define (dim s) f"\e[2m{s}\e[0m")
  (let loop ([st st0] [turn 0] [prev-signal #f] [noprog 0]
             [rung (ladder-rung-of (config-model (agent-state-config st0)))]
             [last-report #f] [spent 0.0] [replans 0]
             [best-signal #f] [best-commit #f] [regressed? #f] [replan? #f])
    (cond
      [(>= turn max-turns) (emit (goal-summary "MAX-TURNS reached (not done)" st turn spent)) st]
      [(and budget (>= spent budget))
       (emit (goal-summary f"BUDGET reached (~{(format-cost spent)} ≥ {(format-cost budget)}), not done" st turn spent)) st]
      [else
       (define tasks (parse-dag workdir))
       (define-values (pdone ptotal) (dag-counts tasks))
       (define pactive (dag-active tasks))
       (define issues (dag-issues tasks))
       (emit (dim (string-append
                   f"\n── goal turn {(add1 turn)}/{max-turns} · model {(config-model (agent-state-config st))}"
                   (if (> ptotal 0) f" · plan {pdone}/{ptotal}" "")
                   (if (pair? issues) f" · ⚠ {(length issues)} plan issue(s)" "")
                   (if budget f" · spent ~{(format-cost spent)}/{(format-cost budget)}" "")
                   " ──")))
       ;; 1) 跑一轮。turn 内异常不致命：记错，当无进展。
       (define st1
         (with-handlers ([exn:fail? (lambda (e) (emit f"[turn error] {(exn-message e)}") st)])
           (run-turn! st (text-msg 'user
                          (goal-prompt goal until-cmds last-report (zero? turn)
                                       pdone ptotal pactive issues replan? regressed? best-commit)) d)))
       (bus-drain! bus)
       (persist-goal-turn! sess st st1)
       (define spent* (+ spent (turn-cost st st1)))
       ;; 2) 验收 oracle（ground truth）。
       (define-values (pass? signal report) (run-oracle until-cmds workdir))
       (cond
         [pass? (emit (goal-summary "DONE ✓ acceptance passed" st1 (add1 turn) spent*)) st1]
         [else
          ;; 3) progress monitor。
          (define progressed? (or (not prev-signal) (< signal prev-signal)))
          (define regr? (and prev-signal (> signal prev-signal)))
          (define noprog* (if progressed? 0 (add1 noprog)))
          ;; 记录“最接近通过”的 checkpoint（最低 signal 时的 git HEAD）。
          (define-values (best-signal* best-commit*)
            (if (or (not best-signal) (< signal best-signal)) (values signal (git-head workdir))
                (values best-signal best-commit)))
          (define state-str (cond [progressed? "progressing"] [regr? "REGRESSED"] [else "no progress"]))
          (emit (dim f"verify: signal {signal} · {state-str} · stuck {noprog*}/{K}"))
          ;; 4) 决策：困住→升模型；升到顶→replan；replan 用尽→停。
          (cond
            [(>= noprog* K)
             (define-values (st2 rung* esc)
               (if (escalation-active? host) (escalate-step st1 host rung) (values st1 rung #f)))
             (cond
               [esc (emit (dim f"monitor: stuck → escalate to {(car esc)} · thinking {(cdr esc)}"))
                    (loop st2 (add1 turn) signal 0 rung* report spent* replans best-signal* best-commit* regr? #f)]
               [(< replans max-replans)
                (emit (dim f"monitor: stuck at top model → replan #{(add1 replans)}/{max-replans}"))
                (loop st1 (add1 turn) signal 0 rung report spent* (add1 replans) best-signal* best-commit* regr? #t)]
               [else (emit (goal-summary "STUCK — no progress after escalation + replans" st1 (add1 turn) spent*)) st1])]
            [else
             (loop st1 (add1 turn) signal noprog* rung report spent* replans best-signal* best-commit* regr? #f)])])]
    ) ; end cond
  ) ; end let loop
) ; end define run-goal!

;; ------------------------------------------------ P4.1：单 worker 在 worktree 里跑（隔离链验证）

(define (reset-workdir st dir)
  (struct-copy agent-state st [config (struct-copy config (agent-state-config st) [workdir dir])]))

;; 在隔离 worktree 里让一个 worker 朝 task-until 干活 → auto-commit → merge 回 main → 全局再验收 → 清理。
;; N=1，无并发（并行是 P4.2），用来验证「隔离 + merge + 再验收」这条链正确。
;; 返回 (values status st*)：'no-repo | 'worker-failed | 'conflict | 'global-fail | 'done。
;; st* 的 workdir 复位回 repo-dir（worker 内部用的是 worktree workdir）。
(define (run-task-in-worktree! d st sess host repo-dir task-id task-desc task-until global-until
                               #:worker-turns [worker-turns 4] #:emit [emit displayln])
  (cond
    [(not (git-repo? repo-dir)) (emit "worktree: not a git repo — cannot isolate") (values 'no-repo st)]
    [else
     (define safe-id (regexp-replace* #rx"[^a-zA-Z0-9_-]" task-id "-"))
     (define branch f"pi/{safe-id}")
     (define wt-path (make-temporary-file "pi-wt-~a" 'directory))
     (delete-directory wt-path)                       ; git worktree add 要求路径尚不存在
     (define wt-dir (path->string wt-path))
     (cond
       [(not (worktree-create! repo-dir branch wt-dir "HEAD"))
        (emit f"worktree {task-id}: create failed") (values 'worker-failed st)]
       [else
        (dynamic-wind
         void
         (lambda ()
           (emit f"worktree {task-id}: worker in isolated copy")
           ;; worker：在 worktree 里跑 bounded run-goal!(workdir=worktree, 验收=task-until)。
           (define st-worker (reset-workdir st wt-dir))
           (define st-w (run-goal! d st-worker sess task-desc (list task-until) worker-turns host #:emit emit))
           (define-values (task-ok? _s _r) (run-oracle (list task-until) wt-dir))
           (cond
             [(not task-ok?)
              (emit f"worktree {task-id}: worker did not satisfy task acceptance")
              (values 'worker-failed (reset-workdir st-w repo-dir))]
             [else
              (git-commit-all! wt-dir f"goal worker: {safe-id}")   ; 确保有可 merge 的 commit
              (define mres (worktree-merge! repo-dir branch f"merge worker {safe-id}"))
              (cond
                [(not (eq? mres 'ok))
                 (emit f"worktree {task-id}: merge {mres} (would sequentialize on main in P4.2)")
                 (values 'conflict (reset-workdir st-w repo-dir))]
                [else
                 ;; merge 后跑全局验收：破坏则 revert 该 merge。
                 (define-values (gok? _gs _gr) (run-oracle global-until repo-dir))
                 (cond
                   [gok? (emit f"worktree {task-id}: merged ✓ global acceptance holds")
                         (values 'done (reset-workdir st-w repo-dir))]
                   [else (revert-last-merge! repo-dir)
                         (emit f"worktree {task-id}: merged but GLOBAL acceptance broke → reverted")
                         (values 'global-fail (reset-workdir st-w repo-dir))])])]))
         (lambda () (worktree-remove! repo-dir wt-dir branch)))])]
  ) ; end cond
) ; end define run-task-in-worktree!

(provide
 run-goal!
 run-oracle failure-count run-verify read-plan turn-cost
 goal-prompt
 ;; P4.0 DAG
 (struct-out plan-task)
 parse-task-line parse-dag dag-ready dag-active dag-counts dag-issues dag-has-cycle?
 ;; P4.1 worktree worker
 run-task-in-worktree!
) ; end provide
