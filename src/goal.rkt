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
 (file "acceptance.rkt")                ; 验收加固：防篡改 / 独立审查 / 先写验收（用户 2026-10-26 a + b + c）
 (file "test-flow.rkt")                 ; 工作区测试流程（.pi/tests.rktd）：goal 的验收 = 额外命令 + 流程的全部步骤（§12）
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

(define (goal-prompt goal until-cmds last-report first? pdone ptotal pactive issues replan? regr? best-commit
                     #:protected [protected '()] #:tampered [tampered '()] #:review-findings [findings '()])
  (string-append
   f"You are working autonomously toward a goal across multiple turns.\n\nGOAL: {goal}\n\n"
   "ACCEPTANCE — the goal is DONE only when ALL of these shell commands exit 0:\n"
   (string-join (for/list ([c (in-list until-cmds)]) f"  $ {c}") "\n")
   "\n\n"
   ;; 被保护的验收文件：先告诉它（改了会被恢复）
   (if (pair? protected)
       (string-append "PROTECTED acceptance files — you cannot change these; edits are reverted before every check. Make the implementation satisfy them:\n"
                      (string-join (for/list ([p (in-list (take protected (min 30 (length protected))))]) f"  {p}") "\n")
                      (if (> (length protected) 30) f"\n  … and {(- (length protected) 30)} more" "") "\n\n")
       "")
   (if (pair? tampered)
       (string-append "WARNING: you changed protected acceptance files; they have been RESTORED to their original content:\n"
                      (string-join (for/list ([t (in-list tampered)]) f"  {(car t)} ({(cdr t)})") "\n")
                      "\nDo not edit them again — change the implementation instead.\n\n")
       "")
   ;; 独立审查复现了的问题：这些检查现在也是验收的一部分
   (if (pair? findings)
       (string-append "An independent review found problems, each demonstrated by a check that currently FAILS. These checks are now part of the acceptance:\n"
                      (string-join (for/list ([f (in-list findings)]) f"  - {(hash-ref f 'problem)}\n    $ {(hash-ref f 'check)}") "\n") "\n\n")
       "")
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
;; 轮内压缩、记录去向的处理见 session.rkt session-persist-turn!
(define (persist-goal-turn! sess before after) (session-persist-turn! sess before after))

;; ---------------------------------------------------------------- 驱动循环

(define (dim s) f"\e[2m{s}\e[0m")

;; progress monitor 的跨轮状态（run-goal! 与 run-goal-dag! 的顺序轮共用 goal-step!，故抽成 struct）。
;; tampered：上一次验收前恢复了的被保护文件；findings：审查复现了的问题（它们的检查之后每次验收都在副本里跑）；reviews：审查了几次
;; flow-results：工作区测试流程最近一次的结构化结果（完成时出验收报告用）
(struct mon (prev-signal noprog rung last-report best-signal best-commit regressed? replan? replans tampered findings reviews flow-results) #:prefab)
(define (fresh-mon st) (mon #f 0 (ladder-rung-of (config-model (agent-state-config st))) #f #f #f #f #f 0 '() '() 0 '()))

;; 跑「一轮 goal」：header + prompt + run-turn! + 验收 + monitor 决策。
;; 返回 (values decision st* m* delta-cost)。decision ∈ 'done | 'continue | 'stuck。
;; 摘要(DONE/STUCK/MAX-TURNS/BUDGET)由调用方(驱动)据 decision 打印——故 run-goal! 与 run-goal-dag! 共用。
(define (goal-step! d st sess host goal until-cmds workdir turn max-turns spent m
                    #:stuck-k [K 2] #:max-replans [max-replans 1] #:budget [budget #f] #:emit [emit displayln]
                    #:guard [g #f] #:review? [review? #f] #:review-model [review-model #f] #:max-reviews [max-reviews 2]
                    #:base-commit [base-commit #f] #:flow [fl #f])
  (define shown-cmds (append until-cmds (if fl (map step-cmd (flow-steps fl)) '())))   ; 给模型 / 审查看的验收命令
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
  (define first? (not (mon-last-report m)))
  (define st1
    (with-handlers ([exn:fail? (lambda (e) (emit f"[turn error] {(exn-message e)}") st)])
      (run-turn! st (text-msg 'user
                     (goal-prompt goal shown-cmds (mon-last-report m) first?
                                  pdone ptotal pactive issues (mon-replan? m) (mon-regressed? m) (mon-best-commit m)
                                  #:protected (if g (guard-paths g) '()) #:tampered (mon-tampered m) #:review-findings (mon-findings m))) d)))
  (bus-drain! (deps-bus d))
  (persist-goal-turn! sess st st1)
  (define dcost0 (turn-cost st st1))
  ;; a. 验收前：被保护的文件改了 / 删了 → 恢复原样（验收在恢复后的状态上跑），下一轮告诉模型
  (define tampered (if g (guard-check+restore! g) '()))
  (when (pair? tampered)
    (emit (dim f"guard: restored {(length tampered)} protected file(s): {(string-join (map car tampered) ", ")}")))
  (define-values (pass1? signal1 report1) (run-oracle until-cmds workdir))
  ;; 工作区测试流程：逐步跑、解析成结构化结果（失败量用解析出的失败数）
  (define flow-results (if fl (run-flow fl) '()))
  (define flow-failing (filter (lambda (r) (not (hash-ref r 'ok))) flow-results))
  (define pass0? (and pass1? (null? flow-failing)))
  (define signal0 (+ signal1 (for/sum ([r (in-list flow-failing)]) (+ 1000000 (or (hash-ref r 'failed #f) 0)))))
  (define report0 (string-join (filter non-empty-string?
                                       (cons report1 (for/list ([r (in-list flow-failing)])
                                                       f"$ {(hash-ref r 'cmd)}  (test flow step {(hash-ref r 'step)}, exit {(hash-ref r 'exit)})\n{(clip-tail (hash-ref r 'tail) 1500)}")))
                               "\n\n"))
  ;; b. 审查复现过的检查：每次都在副本里再跑（不在真实工作目录里跑审查给的命令）
  (define check-results (run-checks-in-copy (map (lambda (f) (hash-ref f 'check)) (mon-findings m)) workdir))
  (define failing-checks (filter (lambda (r) (not (eqv? (cadr r) 0))) check-results))
  (define pass? (and pass0? (null? failing-checks)))
  (define signal (+ signal0 (* 1000000 (length failing-checks))))
  (define report (string-join (filter non-empty-string?
                                      (cons report0 (for/list ([r (in-list failing-checks)]) f"$ {(car r)}  (review check, exit {(cadr r)})\n{(clip-tail (caddr r) 800)}")))
                              "\n\n"))
  (define m0 (struct-copy mon m [tampered tampered] [flow-results flow-results]))
  ;; b. 验收过了、宣布完成之前：独立审查（最多 max-reviews 次）
  (define-values (review-confirmed review-cost)
    (cond
      [(and pass? review? (< (mon-reviews m) max-reviews))
       (emit (dim f"review #{(add1 (mon-reviews m))}: independent read-only reviewer is looking for evidence the goal is not done…"))
       (define-values (yes no rs0 rs1)
         (with-handlers ([exn:fail? (lambda (e) (emit (dim f"review failed: {(exn-message e)} — not counted")) (values '() '() st st))])
           (run-review! d st1 goal shown-cmds workdir base-commit report0 #:model review-model)))
       (for ([f (in-list no)]) (emit (dim f"  review: not reproduced (ignored): {(hash-ref f 'problem)}")))
       (for ([f (in-list yes)]) (emit (dim f"  review: REPRODUCED: {(hash-ref f 'problem)}  $ {(hash-ref f 'check)}")))
       (values yes (turn-cost rs0 rs1))]
      [else (values '() 0.0)]))
  (define dcost (+ dcost0 review-cost))
  (define m* (if (and pass? review? (< (mon-reviews m) max-reviews)) (struct-copy mon m0 [reviews (add1 (mon-reviews m))]) m0))
  (cond
    [(and pass? (pair? review-confirmed))                                     ; 审查否决：问题进验收，接着干
     (values 'continue st1 (struct-copy mon m* [findings (append (mon-findings m*) review-confirmed)] [noprog 0]
                                       [last-report (string-join (for/list ([f review-confirmed]) f"$ {(hash-ref f 'check)}  (review check)\n{(hash-ref f 'output)}") "\n\n")])
             dcost)]
    [pass? (values 'done st1 m* dcost)]
    [else
     (define prev (mon-prev-signal m))
     (define progressed? (or (not prev) (< signal prev)))
     (define regr? (and prev (> signal prev)))
     (define noprog* (if progressed? 0 (add1 (mon-noprog m))))
     (define-values (best-signal* best-commit*)
       (if (or (not (mon-best-signal m)) (< signal (mon-best-signal m))) (values signal (git-head workdir))
           (values (mon-best-signal m) (mon-best-commit m))))
     (emit (dim f"verify: signal {signal} · {(cond [progressed? "progressing"] [regr? "REGRESSED"] [else "no progress"])} · stuck {noprog*}/{K}"))
     (define base (struct-copy mon m* [prev-signal signal] [last-report report]
                               [best-signal best-signal*] [best-commit best-commit*] [regressed? regr?]))
     (cond
       [(>= noprog* K)
        (define-values (st2 rung* esc)
          (if (escalation-active? host) (escalate-step st1 host (mon-rung m)) (values st1 (mon-rung m) #f)))
        (cond
          [esc (emit (dim f"monitor: stuck → escalate to {(car esc)} · thinking {(cdr esc)}"))
               (values 'continue st2 (struct-copy mon base [noprog 0] [rung rung*] [replan? #f]) dcost)]
          [(< (mon-replans m) max-replans)
           (emit (dim f"monitor: stuck at top model → replan #{(add1 (mon-replans m))}/{max-replans}"))
           (values 'continue st1 (struct-copy mon base [noprog 0] [replan? #t] [replans (add1 (mon-replans m))]) dcost)]
          [else (values 'stuck st1 base dcost)])]
       [else (values 'continue st1 (struct-copy mon base [noprog noprog*] [replan? #f]) dcost)])])
) ; end define goal-step!

;; run-goal! : deps state session goal until-cmds max-turns host -> agent-state
;;   #:stuck-k / #:max-replans / #:budget / #:emit（见 goal-step!）。顺序驱动（无并行）。
;;   验收加固（acceptance.rkt）：#:protect 'default | '() | (listof glob) —— 被保护的验收文件（缺省：常见测试文件 + 命令里引用的、git 跟踪的文件）；
;;   #:review? —— 验收过了以后独立审查（缺省开；#:review-model 换模型更好）；#:tests-first? —— 没有 / 不用 until：先让出题 agent 写验收测试，
;;   #:approve 确认后冻结（缺省用 deps 的 asker 问）。
(define (run-goal! d st0 sess goal until-cmds0 max-turns host
                   #:stuck-k [K 2] #:max-replans [max-replans 1] #:budget [budget #f]
                   #:emit [emit displayln]
                   #:protect [protect 'default] #:review? [review? #t] #:review-model [review-model #f] #:max-reviews [max-reviews 2]
                   #:tests-first? [tests-first? #f] #:approve [approve #f])
  (define workdir (config-workdir (agent-state-config st0)))
  (define (ask-user tool) (lambda (text detail) (parameterize ([current-permission-request (hasheq 'tool tool 'level "approve" 'input detail)]) ((deps-asker d) text))))
  ;; 工作区测试流程（.pi/tests.rktd）：有就用（随仓库来的：第一次要用户确认信任）
  (define-values (fl0 flow-err) (load-flow workdir))
  (when flow-err (emit f"test flow: {flow-err}"))
  (define fl1 (and fl0 (let ([tr (ensure-trusted fl0 #:mode (config-permission-mode (agent-state-config st0)) #:ask (or approve (ask-user "test_flow")))])
                         (if (eq? tr #t) fl0 (begin (emit f"test flow not used: {tr}") #f)))))
  ;; c. 先写验收（有测试流程且没给命令：直接用流程，不出题）
  (define-values (until-cmds frozen)
    (cond
      [(or tests-first? (and (null? until-cmds0) (not fl1)))
       (emit (dim "tests first: a separate read-only agent writes the acceptance tests…"))
       (define-values (cmd files)
         (with-handlers ([exn:fail? (lambda (e) (values #f (exn-message e)))])
           (write-acceptance! d st0 goal workdir #:emit emit
                              #:approve (or approve (lambda (text detail) (parameterize ([current-permission-request (hasheq 'tool "acceptance_tests" 'level "approve" 'input detail)])
                                                                            ((deps-asker d) text)))))))
       (if cmd (values (cons cmd until-cmds0) files) (begin (emit f"tests first: {files}") (values #f '())))]
      [else (values until-cmds0 '())]))
  ;; 确认过的验收测试并进工作区的测试流程（随项目共享）：之后的会话 / goal 都用它
  (define-values (until-cmds* fl)
    (cond
      [(and until-cmds (pair? frozen) (not (equal? until-cmds until-cmds0)))
       (define root (or (find-flow-root workdir) workdir))
       (define new-cmd (car until-cmds))
       (define id (string->symbol (format "goal-~a" (add1 (if fl1 (length (flow-steps fl1)) 0)))))
       (define fl2 (with-handlers ([exn:fail? (lambda (e) (emit f"test flow: could not add the step: {(exn-message e)}") #f)])
                     (append-step! root id new-cmd)))
       (if fl2
           (begin (emit (dim f"test flow: added step {id} to {FLOW-FILE} (shared with the project)")) (values (cdr until-cmds) fl2))
           (values until-cmds fl1))]
      [else (values until-cmds fl1)]))
  (define has-acceptance? (and until-cmds* (or (pair? until-cmds*) fl)))
  (define guard-dir (if fl (flow-root fl) workdir))
  (define g (and has-acceptance? (not (and (list? protect) (null? protect))) (not (eq? protect 'none))
                 (guard-add (make-guard guard-dir (append until-cmds* (if fl (map step-cmd (flow-steps fl)) '()))
                                        #:protect (append (if (list? protect) protect DEFAULT-PROTECT) (if fl (cons FLOW-FILE (flow-protect fl)) '())))
                            frozen)))
  (when (and g (pair? (guard-paths g))) (emit (dim f"guard: {(length (guard-paths g))} protected acceptance file(s)")))
  (define base-commit (let-values ([(code out) (run-verify "git rev-parse HEAD 2>/dev/null" workdir)]) (and (eqv? code 0) (non-empty-string? (string-trim out)) (string-trim out))))
  (if (not has-acceptance?)
      (begin (emit (goal-summary "NOT STARTED — no acceptance" st0 0 0.0)) st0)
  (let loop ([st st0] [turn 0] [m (fresh-mon st0)] [spent 0.0])
    (cond
      [(>= turn max-turns) (emit (goal-summary "MAX-TURNS reached (not done)" st turn spent)) st]
      [(and budget (>= spent budget))
       (emit (goal-summary f"BUDGET reached (~{(format-cost spent)} ≥ {(format-cost budget)}), not done" st turn spent)) st]
      [else
       (define-values (decision st* m* dcost)
         (goal-step! d st sess host goal until-cmds* workdir turn max-turns spent m
                     #:stuck-k K #:max-replans max-replans #:budget budget #:emit emit
                     #:guard g #:review? review? #:review-model review-model #:max-reviews max-reviews #:base-commit base-commit #:flow fl))
       (define spent* (+ spent dcost))
       (case decision
         [(done)
          ;; 验收报告：测试流程这一次的结果（刚跑的；必须跑）+ 改过的测试 / 被保护文件
          (when fl
            (define changed (changed-protected fl))
            (history-append! fl "acceptance" (mon-flow-results m*))
            (record! 'tests (hasheq 'kind "acceptance" 'ok #t 'summary (format "goal: ~a" goal) 'changed changed
                                    'steps (map (lambda (r) (hash-remove r 'tail)) (mon-flow-results m*))))
            (emit (format-report fl (mon-flow-results m*) #:title "Acceptance report" #:claim (format "goal: ~a" goal) #:changed changed)))
          (emit (goal-summary (if (and review? (> (mon-reviews m*) 0)) "DONE ✓ acceptance passed · independent review found nothing reproducible" "DONE ✓ acceptance passed")
                              st* (add1 turn) spent*)) st*]
         [(stuck) (emit (goal-summary "STUCK — no progress after escalation + replans" st* (add1 turn) spent*)) st*]
         [else (loop st* (add1 turn) m* spent*)])])
  ) ; end let loop
  ) ; end if
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
        ;; parameterize 隔离本 worker 的推理档：worker 内 escalate 只动自己这个 cell，不泄漏到
        ;; 主线程/其它并行 worker（current-reasoning-effort 现为 parameter，见 model.rkt）。
        (define st-w
          (parameterize ([current-reasoning-effort (current-reasoning-effort)])
            (with-handlers ([exn:fail? (lambda (_e) st-worker)])
              (run-goal! (worker-deps d wt-dir) st-worker wsess
                         task-desc (list task-until) worker-turns host #:emit emit))))
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
           (git-commit-all! wt-dir (automation-commit-message wt-dir 'FEAT f"goal worker: {safe-id}" 'goal-worker))
           (values 'ready branch wt-dir (reset-workdir st-w repo-dir))])])]
  ) ; end cond
) ; end define spawn-worker!

;; merge（串行，必须在主线程逐个跑，git index 不能并发）：merge branch → 全局再验收 →
;; 破坏则 revert → 清理 worktree/branch。返回 'merged | 'conflict | 'global-fail。
(define (merge-worker! repo-dir global-until branch wt-dir #:emit [emit displayln])
  (dynamic-wind
   void
   (lambda ()
     (define mres (worktree-merge! repo-dir branch (automation-commit-message repo-dir 'FEAT f"merge {branch}" 'goal-merge)))
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
    (lambda () (worktree-merge! repo-dir branch (automation-commit-message repo-dir 'FEAT f"merge {branch}" 'goal-merge)))
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
    (run-git-in repo-dir "commit" "-m" (automation-commit-message repo-dir 'DOCS f"mark task {tid} done" 'goal-driver))))

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
                       #:stuck-k [K 3] #:max-replans [max-replans 1] #:emit [emit displayln])
  (define repo-dir (config-workdir (agent-state-config st0)))
  (let loop ([st st0] [turn 0] [m (fresh-mon st0)] [spent 0.0])
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
            ;; —— 并行轮：就绪集≥2 文件不相交 → 并发 worker + 确定序 merge ——
            [(>= (length ready) 2)
             (emit (dim f"\n── goal turn {(add1 turn)}/{max-turns} · PARALLEL {(length ready)} workers: {(string-join (map (lambda (t) (or (plan-task-id t) "?")) ready) " ")} ──"))
             (define specs (parallel-spawn! d st host repo-dir ready worker-turns emit))
             (for ([spec (in-list (sort specs string<? #:key car))])
               (define tid (list-ref spec 0)) (define status (list-ref spec 1))
               (define branch (list-ref spec 2)) (define wt-dir (list-ref spec 3))
               (cond
                 [(eq? status 'ready)
                  (define mres (merge-branch! repo-dir branch wt-dir))
                  (cond [(eq? mres 'ok) (mark-task-done! repo-dir tid) (emit (dim f"  merge {tid}: merged ✓"))]
                        [else (emit (dim f"  merge {tid}: {mres} (retry sequentially)"))])]
                 [else (emit (dim f"  worker {tid}: {status}"))]))
             (define worker-cost (for/sum ([spec (in-list specs)]) (turn-cost st (list-ref spec 4))))
             ;; 并行有进展 → 重置 monitor 的 stuck 计数/replan，prev-signal 取最新全局信号。
             (loop st (add1 turn) (struct-copy mon m [prev-signal gsig] [noprog 0] [replan? #f]) (+ spent worker-cost))]
            ;; —— 顺序轮（就绪 ≤1 或未声明 files/verify）：复用 goal-step! 全套 monitor/escalate/replan ——
            [else
             (define-values (decision st* m* dcost)
               (goal-step! d st sess host goal until-cmds repo-dir turn max-turns spent m
                           #:stuck-k K #:max-replans max-replans #:budget budget #:emit emit))
             (define spent* (+ spent dcost))
             (case decision
               [(done) (emit (goal-summary "DONE ✓ acceptance passed" st* (add1 turn) spent*)) st*]
               [(stuck) (emit (goal-summary "STUCK — no progress after escalation + replans" st* (add1 turn) spent*)) st*]
               [else (loop st* (add1 turn) m* spent*)])])])]
    ) ; end cond
  ) ; end let loop
) ; end define run-goal-dag!

(provide
 run-goal!
 run-oracle failure-count run-verify read-plan turn-cost
 goal-prompt goal-step! (struct-out mon) fresh-mon
 ;; P4.0 DAG
 (struct-out plan-task)
 parse-task-line parse-dag dag-ready dag-active dag-counts dag-issues dag-has-cycle?
 ;; P4.1 worktree worker
 run-task-in-worktree! spawn-worker! merge-worker!
 ;; P4.2 并行 DAG
 run-goal-dag! disjoint-ready parallel-spawn! merge-branch! mark-task-done!
) ; end provide
