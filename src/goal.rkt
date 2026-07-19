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
 racket/list
 racket/set
 racket/pvector
 (file "model.rkt")
 (file "event.rkt")
 (file "loop.rkt")
 (file "session.rkt")
 (file "permission.rkt")                ; policy-with-workdir（worker 各自作用域）
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
;;   - [ ] {id} 描述 (needs: id1, id2) (files: a.py, b.py) (verify: python3 -m unittest test_a.py)
;; {id}/needs/files/verify 都可选：无注解 → 退化为 P2 线性 checklist（deps 空 → 就绪=未完成，文档序）。
;; verify = 该任务自己的验收命令（并行 worker 用它判自己何时完成；无则该任务不参与并行）。
(struct plan-task (id desc deps files verify done?) #:prefab)

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
     (define vm (regexp-match #px"\\(verify:\\s*([^)]*)\\)" rest2))
     (define verify (and vm (non-empty-string? (string-trim (cadr vm))) (string-trim (cadr vm))))
     (define desc (string-trim (regexp-replace* #px"\\((?:needs|files|verify):[^)]*\\)" rest2 "")))
     (plan-task id desc deps files verify done?)]
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

;; ------------------------------------------------ P4.1/P4.2：worktree 隔离的 worker

(define (reset-workdir st dir)
  (struct-copy agent-state st [config (struct-copy config (agent-state-config st) [workdir dir])]))

;; 给 worker 的 deps：独立 bus（不与主输出/其它 worker 交错）、非交互 asker（并行不能抢 stdin）、
;; 作用域根 = worktree 的策略（--mode auto 下 worktree 内写才放行；worktree 在 repo 外）。
(define (worker-deps d wt-dir)
  (struct-copy deps d
               [bus (make-bus)]
               [asker (lambda (_q) 'no)]
               [policy (policy-with-workdir (deps-policy d) wt-dir)]))

;; spawn（并发安全）：建 worktree → worker 朝 task-until 干活 → 用 task-until 判达标 → auto-commit。
;; **不 merge、不清理成功者**（留给串行 merge 阶段）；失败/无仓库则就地清理。
;; 返回 (values status branch wt-dir st*)。status: 'ready | 'failed | 'no-repo。
;; 注：worker 用**自己的 throwaway session**（放 worktree 外，避免被 git add 提交 + 避免并发 worker
;; 抢同一 session 文件）。worker 的产物靠 worktree 的 git commit 回归，不靠 session。
(define (spawn-worker! d st host repo-dir task-id task-desc task-until
                       #:worker-turns [worker-turns 4] #:emit [emit displayln])
  (cond
    [(not (git-repo? repo-dir)) (values 'no-repo #f #f st)]
    [else
     (define safe-id (regexp-replace* #rx"[^a-zA-Z0-9_-]" task-id "-"))
     (define branch f"pi/{safe-id}")
     (define wt-path (make-temporary-file "pi-wt-~a" 'directory))
     (delete-directory wt-path)
     (define wt-dir (path->string wt-path))
     (cond
       [(not (worktree-create! repo-dir branch wt-dir "HEAD")) (values 'failed branch wt-dir st)]
       [else
        (define st-worker (reset-workdir st wt-dir))
        (define wsess-path (make-temporary-file "pi-wsess-~a"))
        (delete-file wsess-path)
        (define wsess (session-open! wsess-path (agent-state-config st-worker)))
        (define st-w
          (with-handlers ([exn:fail? (lambda (_e) st-worker)])
            (run-goal! (worker-deps d wt-dir) st-worker wsess
                       task-desc (list task-until) worker-turns host #:emit emit)))
        (session-close! wsess)
        (when (file-exists? wsess-path) (delete-file wsess-path))
        (define-values (ok? _s _r) (run-oracle (list task-until) wt-dir))
        (cond
          [(not ok?)
           (worktree-remove! repo-dir wt-dir branch)
           (values 'failed branch wt-dir (reset-workdir st-w repo-dir))]
          [else
           ;; 丢弃 worker 对共享 PLAN.md 的改动（driver 独占 PLAN.md via mark-task-done!，避免 merge 冲突）。
           (run-git-in wt-dir "checkout" "--" "PLAN.md")
           (git-commit-all! wt-dir f"goal worker: {safe-id}")
           (values 'ready branch wt-dir (reset-workdir st-w repo-dir))])])]
  ) ; end cond
) ; end define spawn-worker!

;; merge（串行，必须在主线程逐个跑，git index 不能并发）：merge branch → 全局再验收 →
;; 破坏则 revert → 清理 worktree/branch。返回 'merged | 'conflict | 'global-fail。
(define (merge-worker! repo-dir global-until branch wt-dir #:emit [emit displayln])
  (dynamic-wind
   void
   (lambda ()
     (define mres (worktree-merge! repo-dir branch f"merge {branch}"))
     (cond
       [(not (eq? mres 'ok)) 'conflict]
       [else
        (define-values (gok? _s _r) (run-oracle global-until repo-dir))
        (cond [gok? 'merged] [else (revert-last-merge! repo-dir) 'global-fail])]))
   (lambda () (worktree-remove! repo-dir wt-dir branch))))

;; merge-only（P4.2 并行用）：只 merge + 冲突检测 + 清理，**不跑全局验收**（并行时全局要等所有
;; worker merge 完才可能过，逐个验收是错的；全局由 run-goal-dag! 循环顶端把关）。→ 'ok|'conflict|'error。
(define (merge-branch! repo-dir branch wt-dir)
  (dynamic-wind void
    (lambda () (worktree-merge! repo-dir branch f"merge {branch}"))
    (lambda () (worktree-remove! repo-dir wt-dir branch))))

;; 在 main 的 PLAN.md 里把 id=tid 的未勾任务勾上并提交（driver 推进 DAG，不依赖模型自勾）。
(define (mark-task-done! repo-dir tid)
  (define f (build-path repo-dir "PLAN.md"))
  (when (file-exists? f)
    (define lines* (for/list ([l (in-list (file->lines f))])
                     (define t (parse-task-line l))
                     (if (and t (equal? (plan-task-id t) tid) (not (plan-task-done? t)))
                         (regexp-replace #px"\\[ \\]" l "[x]") l)))
    (call-with-output-file f #:exists 'truncate (lambda (o) (write-string (string-join lines* "\n") o)))
    (run-git-in repo-dir "add" "PLAN.md")
    (run-git-in repo-dir "commit" "-m" f"mark task {tid} done")))

;; P4.1 便捷式（N=1）：spawn + merge，自带日志。保持既有调用方/测试不变。
(define (run-task-in-worktree! d st sess host repo-dir task-id task-desc task-until global-until
                               #:worker-turns [worker-turns 4] #:emit [emit displayln])
  (emit f"worktree {task-id}: worker in isolated copy")
  (define-values (status branch wt-dir st*)
    (spawn-worker! d st host repo-dir task-id task-desc task-until
                   #:worker-turns worker-turns #:emit emit))
  (case status
    [(no-repo) (emit "worktree: not a git repo — cannot isolate") (values 'no-repo st*)]
    [(failed) (emit f"worktree {task-id}: worker did not satisfy task acceptance") (values 'worker-failed st*)]
    [else
     (define m (merge-worker! repo-dir global-until branch wt-dir #:emit emit))
     (emit (case m
             [(merged) f"worktree {task-id}: merged ✓ global acceptance holds"]
             [(conflict) f"worktree {task-id}: merge conflict → sequentialize"]
             [else f"worktree {task-id}: merged but GLOBAL acceptance broke → reverted"]))
     (values (case m [(merged) 'done] [(conflict) 'conflict] [else 'global-fail]) st*)])
) ; end define run-task-in-worktree!

;; ------------------------------------------------ P4.2：并行 DAG 调度

;; 从就绪集贪心选「文件两两不相交且都声明了 files + verify」的子集，截断到 cap。
;; 无 files 或无 verify 的任务不参与并行（不知文件范围/无停止信号 → 保守串行）。
(define (disjoint-ready tasks cap)
  (let loop ([rs (dag-ready tasks)] [chosen '()] [used (set)])
    (cond
      [(or (null? rs) (>= (length chosen) cap)) (reverse chosen)]
      [else
       (define t (car rs))
       (define fs (list->set (plan-task-files t)))
       (if (and (plan-task-verify t) (not (set-empty? fs)) (set-empty? (set-intersect fs used)))
           (loop (cdr rs) (cons t chosen) (set-union used fs))
           (loop (cdr rs) chosen used))])))

;; 并发 spawn 一批 worker（各在自己 worktree，线程并行），等全部完成。
;; 返回 list of (list task-id status branch wt-dir st*)，顺序同 tasks。
(define (parallel-spawn! d st host repo-dir tasks worker-turns emit)
  (define results (make-vector (length tasks) #f))
  (define ths
    (for/list ([t (in-list tasks)] [i (in-naturals)])
      (define tid (or (plan-task-id t) f"t{i}"))
      ;; 约束 worker 只碰自己声明的文件（防止越界写别的 worker 的文件 → merge 冲突）。
      (define wgoal
        (string-append (plan-task-desc t)
          f"\n\nSTRICT SCOPE: only create/edit these files: {(string-join (plan-task-files t) ", ")}. "
          "Do NOT edit PLAN.md or any file outside this set — the driver tracks completion and merges your work."))
      (thread (lambda ()
                (define-values (status branch wt-dir st*)
                  (spawn-worker! d st host repo-dir tid wgoal (plan-task-verify t)
                                 #:worker-turns worker-turns #:emit emit))
                (vector-set! results i (list tid status branch wt-dir st*))))))
  (for-each thread-wait ths)
  (vector->list results))

;; run-goal-dag! : deps state session host goal until-cmds max-turns -> agent-state
;; 并行能力的 Goal 驱动（--parallel 选它）。每轮：全局验收→未过则算「文件不相交的就绪集」；
;;   ≥2 → 并发 worker(worktree) + 按 task-id 确定序 merge（每次 merge 后全局再验收，冲突/破坏则丢弃该 worker）；
;;   ≤1 → 在 main 上顺序跑一轮(run-turn! + goal-prompt)。预算/轮数/连 K 轮无进展则停。
;; **限制(v1)**：worker 间共享全局 reasoning/escalate box（应改 thread-local param，见 design-goalmode-p4 §5）；
;;   worker 不 escalate，同档并发；顺序回退不含 monitor/replan(那是 run-goal! 的)。
(define (run-goal-dag! d st0 sess host goal until-cmds max-turns
                       #:budget [budget #f] #:concurrency [cap 4] #:worker-turns [worker-turns 4]
                       #:stuck-k [K 3] #:emit [emit displayln])
  (define repo-dir (config-workdir (agent-state-config st0)))
  (define (dim s) f"\e[2m{s}\e[0m")
  (let loop ([st st0] [turn 0] [prev-signal #f] [noprog 0] [spent 0.0])
    (cond
      [(>= turn max-turns) (emit (goal-summary "MAX-TURNS reached (not done)" st turn spent)) st]
      [(and budget (>= spent budget))
       (emit (goal-summary f"BUDGET reached (~{(format-cost spent)}), not done" st turn spent)) st]
      [else
       (define-values (gok? gsig _gr) (run-oracle until-cmds repo-dir))
       (cond
         [gok? (emit (goal-summary "DONE ✓ acceptance passed" st (add1 turn) spent)) st]
         [else
          (define tasks (parse-dag repo-dir))
          (define ready (disjoint-ready tasks cap))
          (cond
            ;; —— 并行轮 ——
            [(>= (length ready) 2)
             (emit (dim f"\n── goal turn {(add1 turn)}/{max-turns} · PARALLEL {(length ready)} workers: {(string-join (map (lambda (t) (or (plan-task-id t) "?")) ready) " ")} ──"))
             (define specs (parallel-spawn! d st host repo-dir ready worker-turns emit))
             ;; 串行 merge（按 task-id 确定序，git index 不能并发）；merged 者在 PLAN.md 勾掉推进 DAG。
             ;; 全局验收不在此逐个跑（并行要等全部 merge 完），交给循环顶端。
             (for ([spec (in-list (sort specs string<? #:key car))])
               (define tid (list-ref spec 0)) (define status (list-ref spec 1))
               (define branch (list-ref spec 2)) (define wt-dir (list-ref spec 3))
               (cond
                 [(eq? status 'ready)
                  (define m (merge-branch! repo-dir branch wt-dir))
                  (cond [(eq? m 'ok) (mark-task-done! repo-dir tid) (emit (dim f"  merge {tid}: merged ✓"))]
                        [else (emit (dim f"  merge {tid}: {m} (retry sequentially)"))])]
                 [else (emit (dim f"  worker {tid}: {status}"))]))
             ;; 计入 worker 成本（各 worker st* 的 usage 增量）；main st 历史不并入(worker 在隔离副本)。
             (define worker-cost (for/sum ([spec (in-list specs)]) (turn-cost st (list-ref spec 4))))
             (loop st (add1 turn) gsig 0 (+ spent worker-cost))]
            ;; —— 顺序轮（就绪 ≤1 或未声明 files/verify）——
            [else
             (define-values (pdone ptotal) (dag-counts tasks))
             (define pactive (dag-active tasks))
             (emit (dim f"\n── goal turn {(add1 turn)}/{max-turns} · sequential · plan {pdone}/{ptotal} ──"))
             (define st1
               (with-handlers ([exn:fail? (lambda (e) (emit f"[turn error] {(exn-message e)}") st)])
                 (run-turn! st (text-msg 'user
                                (goal-prompt goal until-cmds #f (zero? turn) pdone ptotal pactive
                                             (dag-issues tasks) #f #f #f)) d)))
             (bus-drain! (deps-bus d))
             (persist-goal-turn! sess st st1)
             (define spent* (+ spent (turn-cost st st1)))
             (define progressed? (or (not prev-signal) (< gsig prev-signal)))
             (define noprog* (if progressed? 0 (add1 noprog)))
             (cond
               [(>= noprog* K) (emit (goal-summary "STUCK — no global progress" st1 (add1 turn) spent*)) st1]
               [else (loop st1 (add1 turn) gsig noprog* spent*)])])])]
    ) ; end cond
  ) ; end let loop
) ; end define run-goal-dag!

(provide
 run-goal!
 run-oracle failure-count run-verify read-plan turn-cost
 goal-prompt
 ;; P4.0 DAG
 (struct-out plan-task)
 parse-task-line parse-dag dag-ready dag-active dag-counts dag-issues dag-has-cycle?
 ;; P4.1 worktree worker
 run-task-in-worktree! spawn-worker! merge-worker!
 ;; P4.2 并行 DAG
 run-goal-dag! disjoint-ready parallel-spawn! merge-branch! mark-task-done!
) ; end provide
