#lang tstring racket
;; goal-test.rkt — Goal 模式 P1（goal.rkt）离线单测：验收 oracle、失败量启发式、驱动循环 + 进度 monitor。

(require
 rackunit
 racket/async-channel
 racket/file
 racket/string
 (file "../src/model.rkt")
 (file "../src/event.rkt")
 (file "../src/provider.rkt")
 (file "../src/tool.rkt")
 (file "../src/permission.rkt")
 (file "../src/plugin.rkt")
 (file "../src/loop.rkt")
 (file "../src/session.rkt")
 (file "../src/worktree.rkt")
 (file "../src/tools/file.rkt")
 (file "../src/goal.rkt")
) ; end require

(define tmpdir (make-temporary-file "pi2-goaltest-~a" 'directory))

;; ---------------------------------------------------------------- failure-count 启发式

(test-case "failure-count：优先累加 failures=/errors=，否则数 FAIL/ERROR/Traceback 行"
  (check-equal? (failure-count "FAILED (failures=2, errors=1)") 3)
  (check-equal? (failure-count "Ran 5 tests in 0.01s\n\nOK") 0)
  (check-equal? (failure-count "Traceback (most recent call last):\n ...\nAssertionError: nope") 2)
  (check-equal? (failure-count "FAIL: test_a\nFAIL: test_b\nERROR: test_c") 3)
  (check-equal? (failure-count "everything is fine") 0)
) ; end test-case

;; ---------------------------------------------------------------- run-oracle（真 shell）

(test-case "run-oracle：全过 signal 0；有失败 signal≥1e6；多命令全过才算过"
  (define-values (p1 s1 _r1) (run-oracle '("true") tmpdir))
  (check-true p1) (check-equal? s1 0)
  (define-values (p2 s2 r2) (run-oracle '("false") tmpdir))
  (check-false p2) (check-true (>= s2 1000000))
  (define-values (p3 _s3 _r3) (run-oracle '("true" "false") tmpdir))
  (check-false p3)                                   ; 一个失败即不过
  (define-values (p4 s4 _r4) (run-oracle (list "echo 'failures=3'; exit 1") tmpdir))
  (check-false p4) (check-equal? s4 (+ 1000000 3))   ; 失败量并入 signal
) ; end test-case

;; ---------------------------------------------------------------- 驱动循环集成

;; 无操作 provider：每轮只吐一句文本(不改文件)。进展由 --until 脚本的内部计数器制造。
(define (noop-provider)
  (provider "noop"
    (lambda (_m _t)
      (define ch (make-async-channel))
      (thread (lambda ()
                (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant "working on it")))
                (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 5 3)))))
      ch)
    void))

(define (goal-deps host)
  (define cfg (struct-copy config (default-config)
                           [workdir (path->string tmpdir)] [permission-mode 'yolo]))
  (values (make-deps #:provider (noop-provider)
                     #:registry (make-registry '())
                     #:bus (make-bus)
                     #:policy (make-policy cfg)
                     #:plugin-host host)
          cfg))

(define (collect-emit)
  (define box0 (box '()))
  (values (lambda (s) (set-box! box0 (cons s (unbox box0)))) (lambda () (reverse (unbox box0)))))

(define (open-sess cfg name) (session-open! (build-path tmpdir name) cfg))

(test-case "run-goal!：验收信号逐轮下降 → monitor 判 progressing → 到 DONE"
  (define host (make-plugin-host))          ; lmstudio（非 deepseek），escalation 不触发
  (define-values (d cfg) (goal-deps host))
  ;; --until 脚本:内部计数器,第 3 次调用起 exit 0;之前 echo failures=递减 → signal 逐轮降。
  (define cmd "c=$(cat CNT 2>/dev/null||echo 0); c=$((c+1)); echo $c>CNT; r=$((3-c)); if [ $r -le 0 ]; then echo OK; exit 0; else echo failures=$r; exit 1; fi")
  (when (file-exists? (build-path tmpdir "CNT")) (delete-file (build-path tmpdir "CNT")))
  (define-values (emit dump) (collect-emit))
  (define sess (open-sess cfg "done.rktd"))
  (define st (run-goal! d (make-initial-state cfg) sess "make it pass" (list cmd) 10 host #:emit emit))
  (session-close! sess)
  (define log (string-join (dump) "\n"))
  (check-true (string-contains? log "DONE"))               ; 到达验收
  (check-false (string-contains? log "MAX-TURNS"))
  (check-equal? (string-trim (file->string (build-path tmpdir "CNT"))) "3")  ; 恰好 3 轮达标
) ; end test-case

(test-case "run-goal!：验收恒失败(信号不降) → monitor 判 stuck，无 deepseek 无法升级 → 停"
  (define host (make-plugin-host))          ; 非 deepseek → escalate 不生效
  (define-values (d cfg) (goal-deps host))
  (define-values (emit dump) (collect-emit))
  (define sess (open-sess cfg "stuck.rktd"))
  (define st (run-goal! d (make-initial-state cfg) sess "impossible" (list "echo failing; exit 1") 20 host
                        #:stuck-k 2 #:emit emit))
  (session-close! sess)
  (define log (string-join (dump) "\n"))
  (check-true (string-contains? log "STUCK"))              ; monitor 判困住并停
  (check-false (string-contains? log "DONE"))
  (check-true (string-contains? log "no progress"))
) ; end test-case

(test-case "run-goal!：轮数耗尽仍不过 → MAX-TURNS 停（signal 每轮不同，避免 stuck 先触发）"
  (define host (make-plugin-host))
  (define-values (d cfg) (goal-deps host))
  ;; 每轮 failures 递增 → 信号一直变(不判 stuck 的“无进展”≥K，因为 regressed 也重置计数? 不,regressed 不重置)
  ;; 用递减但永不到 0 的信号:failures = 100 - count，始终 exit 1 → progressing，永不 stuck，撞 max-turns。
  (define cmd "c=$(cat CT2 2>/dev/null||echo 0); c=$((c+1)); echo $c>CT2; echo failures=$((100-c)); exit 1")
  (when (file-exists? (build-path tmpdir "CT2")) (delete-file (build-path tmpdir "CT2")))
  (define-values (emit dump) (collect-emit))
  (define sess (open-sess cfg "maxt.rktd"))
  (define st (run-goal! d (make-initial-state cfg) sess "never done" (list cmd) 3 host #:emit emit))
  (session-close! sess)
  (define log (string-join (dump) "\n"))
  (check-true (string-contains? log "MAX-TURNS"))
  (check-equal? (string-trim (file->string (build-path tmpdir "CT2"))) "3")  ; 恰好跑满 3 轮
) ; end test-case

;; ---------------------------------------------------------------- P2：plan / budget / replan

(test-case "read-plan：解析 PLAN.md 复选框 → done/total/active；无文件→(0 0 #f)"
  (define pd (make-temporary-file "pi2-plan-~a" 'directory))
  (call-with-output-file (build-path pd "PLAN.md")
    (lambda (o) (write-string "# Plan\n- [x] first\n- [X] second\n- [ ] third task\n- [ ] fourth\nnot an item\n" o)))
  (define-values (done total active) (read-plan pd))
  (check-equal? done 2)
  (check-equal? total 4)
  (check-equal? active "third task")
  (define-values (a b c) (read-plan (make-temporary-file "pi2-plan2-~a" 'directory)))
  (check-equal? (list a b c) (list 0 0 #f))
  (delete-directory/files pd)
) ; end test-case

(test-case "run-goal!：累计成本超 --budget → BUDGET 停"
  (define host (make-plugin-host))
  (define cfg (struct-copy config (default-config)
                           [workdir (path->string tmpdir)] [permission-mode 'yolo]
                           [model "deepseek-v4-flash"]))     ; 有价:0.14/0.28
  (define d (make-deps #:provider (noop-provider) #:registry (make-registry '())
                       #:bus (make-bus) #:policy (make-policy cfg) #:plugin-host host))
  ;; 恒失败但信号递减(避免 stuck 先触发),让预算成为唯一终止因。
  (define cmd "c=$(cat CB 2>/dev/null||echo 0); c=$((c+1)); echo $c>CB; echo failures=$((100-c)); exit 1")
  (when (file-exists? (build-path tmpdir "CB")) (delete-file (build-path tmpdir "CB")))
  (define-values (emit dump) (collect-emit))
  (define sess (open-sess cfg "budget.rktd"))
  (run-goal! d (make-initial-state cfg) sess "spendy" (list cmd) 50 host #:budget 0.000001 #:emit emit)
  (session-close! sess)
  (check-true (string-contains? (string-join (dump) "\n") "BUDGET"))
) ; end test-case

(test-case "run-goal!：困住且无法升级 → replan → 用尽 → STUCK"
  (define host (make-plugin-host))          ; 非 deepseek → escalate 不生效 → 走 replan
  (define-values (d cfg) (goal-deps host))
  (define-values (emit dump) (collect-emit))
  (define sess (open-sess cfg "replan.rktd"))
  (run-goal! d (make-initial-state cfg) sess "impossible" (list "echo nope; exit 1") 12 host
             #:stuck-k 2 #:max-replans 1 #:emit emit)
  (session-close! sess)
  (define log (string-join (dump) "\n"))
  (check-true (string-contains? log "replan"))              ; 先尝试换策略
  (check-true (string-contains? log "STUCK"))               ; 用尽后停
) ; end test-case

;; ---------------------------------------------------------------- P4.0：DAG

(test-case "parse-task-line：解析 {id}/needs/files/done，非条目行→#f"
  (define t (parse-task-line "- [ ] {acct} Build accounting (needs: engine, orders) (files: account.py, lot.py)"))
  (check-equal? (plan-task-id t) "acct")
  (check-equal? (plan-task-desc t) "Build accounting")
  (check-equal? (plan-task-deps t) '("engine" "orders"))
  (check-equal? (plan-task-files t) '("account.py" "lot.py"))
  (check-false (plan-task-done? t))
  (define d (parse-task-line "- [x] {engine} Matching engine"))
  (check-true (plan-task-done? d))
  (check-equal? (plan-task-id d) "engine")
  (check-false (parse-task-line "not a task line"))
  ;; 无注解 → 退化：id=#f，deps/files 空
  (define p (parse-task-line "- [ ] plain task"))
  (check-false (plan-task-id p))
  (check-equal? (plan-task-desc p) "plain task")
  (check-equal? (plan-task-deps p) '())
) ; end test-case

(test-case "dag-active：跳过依赖未满足的任务，选就绪者（拓扑而非文档序）"
  (define d (make-temporary-file "pi2-dag-~a" 'directory))
  (call-with-output-file (build-path d "PLAN.md")
    (lambda (o) (write-string (string-join
      (list "- [x] {a} task A"
            "- [ ] {c} task C (needs: b)"      ; b 未完成 → C 不就绪（尽管文档序在前）
            "- [ ] {b} task B (needs: a)")     ; a 完成 → B 就绪
      "\n") o)))
  (define tasks (parse-dag d))
  (check-equal? (dag-active tasks) "task B")
  (define-values (done total) (dag-counts tasks))
  (check-equal? (list done total) (list 1 3))
  (delete-directory/files d)
) ; end test-case

(test-case "dag-issues / dag-has-cycle?：未知依赖 + 环检测"
  (define d (make-temporary-file "pi2-dag2-~a" 'directory))
  (call-with-output-file (build-path d "PLAN.md")
    (lambda (o) (write-string "- [ ] {x} X (needs: nope)\n" o)))
  (check-true (ormap (lambda (s) (string-contains? s "unknown id: nope")) (dag-issues (parse-dag d))))
  (check-false (dag-has-cycle? (parse-dag d)))
  (call-with-output-file (build-path d "PLAN.md") #:exists 'replace
    (lambda (o) (write-string "- [ ] {a} A (needs: b)\n- [ ] {b} B (needs: a)\n" o)))
  (check-true (dag-has-cycle? (parse-dag d)))
  (check-true (ormap (lambda (s) (string-contains? s "cycle")) (dag-issues (parse-dag d))))
  (delete-directory/files d)
) ; end test-case

;; ---------------------------------------------------------------- P4.1：单 worker 在 worktree

;; provider：第 1 次调用吐一个 write_file 工具调用,之后吐终止文本。
(define (make-write-provider fname content)
  (define n (box 0))
  (provider "wf"
    (lambda (_m _t)
      (define ch (make-async-channel))
      (define i (unbox n)) (set-box! n (add1 i))
      (thread (lambda ()
                (cond
                  [(= i 0)
                   (async-channel-put ch (evt:message (now-ms)
                     (message 'assistant (list (tool-use-block "c" "write_file" (hasheq 'path fname 'content content))))))
                   (async-channel-put ch (evt:turn-end (now-ms) "tool_calls" (usage 1 1)))]
                  [else
                   (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant "done")))
                   (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 1 1)))])))
      ch)
    void))

(define (git! dir . args) (define-values (c o) (apply run-git-in dir args)) (void))

(test-case "run-task-in-worktree!：worker 隔离写文件 → merge 回 main → 全局验收过 → 'done"
  (define repo (make-temporary-file "pi2-wtint-~a" 'directory))
  (git! repo "init" "-q") (git! repo "config" "user.email" "t@e.com") (git! repo "config" "user.name" "t")
  (call-with-output-file (build-path repo "seed.txt") (lambda (o) (write-string "seed" o)))
  (git! repo "add" "-A") (git! repo "commit" "-m" "init")
  (define host (make-plugin-host))
  (define cfg (struct-copy config (default-config) [workdir (path->string repo)] [permission-mode 'yolo]))
  (define d (make-deps #:provider (make-write-provider "task.txt" "worker output")
                       #:registry (make-registry (list (make-write-file-tool)))
                       #:bus (make-bus) #:policy (make-policy cfg) #:plugin-host host))
  (define sess (session-open! (build-path repo "s.rktd") cfg))
  (define-values (emit dump) (collect-emit))
  (define-values (status st*)
    (run-task-in-worktree! d (make-initial-state cfg) sess host (path->string repo)
                           "t1" "create task.txt with the write_file tool" "test -f task.txt" (list "test -f task.txt")
                           #:worker-turns 3 #:emit emit))
  (session-close! sess)
  (check-eq? status 'done)
  (check-true (file-exists? (build-path repo "task.txt")))                     ; 已 merge 回主树
  (check-equal? (config-workdir (agent-state-config st*)) (path->string repo)) ; workdir 复位
  ;; worktree 已清理:.pi/worktrees 下无残留、git worktree list 只剩主树
  (define-values (_c wl) (run-git-in repo "worktree" "list"))
  (check-equal? (length (string-split wl "\n")) 1)
  (delete-directory/files repo)
) ; end test-case

(test-case "run-task-in-worktree!：非 git 仓库 → 'no-repo"
  (define nod (make-temporary-file "pi2-norepo-~a" 'directory))
  (define host (make-plugin-host))
  (define cfg (struct-copy config (default-config) [workdir (path->string nod)] [permission-mode 'yolo]))
  (define d (make-deps #:provider (noop-provider) #:registry (make-registry '())
                       #:bus (make-bus) #:policy (make-policy cfg) #:plugin-host host))
  (define sess (session-open! (build-path nod "s.rktd") cfg))
  (define-values (emit _dump) (collect-emit))
  (define-values (status _st)
    (run-task-in-worktree! d (make-initial-state cfg) sess host (path->string nod)
                           "t" "x" "true" (list "true") #:emit emit))
  (session-close! sess)
  (check-eq? status 'no-repo)
  (delete-directory/files nod)
) ; end test-case

;; ---------------------------------------------------------------- P4.2：并行 DAG

(test-case "disjoint-ready：文件不相交且声明 verify 者并行；重叠/未声明排除；cap 截断"
  (define tasks (map parse-task-line (list
    "- [ ] {a} create a.py (files: a.py) (verify: test -f a.py)"
    "- [ ] {b} create b.py (files: b.py) (verify: test -f b.py)"
    "- [ ] {c} create c (files: a.py) (verify: test -f a.py)"   ; 与 a 文件重叠 → 排除
    "- [ ] {e} no-files task (verify: test -f e.py)"            ; 无 files → 排除
    "- [ ] {f} no-verify (files: f.py)")))                      ; 无 verify → 排除
  (check-equal? (map plan-task-id (disjoint-ready tasks 4)) '("a" "b"))
  (check-equal? (length (disjoint-ready tasks 1)) 1)             ; cap
) ; end test-case

;; module provider（无状态,线程安全）：从窗口解析目标 *.py 文件名；未写过 → 吐 write_file，写过 → 终止。
(define (make-module-provider)
  (provider "mod"
    (lambda (msgs _t)
      (define ch (make-async-channel))
      (define txt (apply string-append
                         (for/list ([m (in-list msgs)] #:when (eq? (message-role m) 'user)) (message-text m))))
      (define fm (regexp-match #px"([a-z]+\\.py)" txt))
      (define fname (if fm (cadr fm) "x.py"))
      (define wrote? (for/or ([m (in-list msgs)])
                       (for/or ([b (in-list (message-tool-uses m))]) (string=? (tool-use-block-name b) "write_file"))))
      (thread (lambda ()
                (cond
                  [wrote?
                   (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant "done")))
                   (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 1 1)))]
                  [else
                   (async-channel-put ch (evt:message (now-ms)
                     (message 'assistant (list (tool-use-block "c" "write_file" (hasheq 'path fname 'content "module"))))))
                   (async-channel-put ch (evt:turn-end (now-ms) "tool_calls" (usage 1 1)))])))
      ch)
    void))

(test-case "run-goal-dag!：两独立任务并行 worker(隔离 worktree) → 串行 merge → 全局验收过 → DONE"
  (define repo (make-temporary-file "pi2-par-~a" 'directory))
  (git! repo "init" "-q") (git! repo "config" "user.email" "t@e.com") (git! repo "config" "user.name" "t")
  (void (call-with-output-file (build-path repo "PLAN.md")
          (lambda (o) (write-string (string-join (list
            "- [ ] {a} create a.py (files: a.py) (verify: test -f a.py)"
            "- [ ] {b} create b.py (files: b.py) (verify: test -f b.py)") "\n") o))))
  (git! repo "add" "-A") (git! repo "commit" "-m" "plan")
  (define host (make-plugin-host))
  (define cfg (struct-copy config (default-config) [workdir (path->string repo)] [permission-mode 'yolo]))
  (define d (make-deps #:provider (make-module-provider) #:registry (make-registry (list (make-write-file-tool)))
                       #:bus (make-bus) #:policy (make-policy cfg) #:plugin-host host))
  (define sess (session-open! (build-path repo "s.rktd") cfg))
  (define-values (emit dump) (collect-emit))
  (define st (run-goal-dag! d (make-initial-state cfg) sess host "build a and b"
                            (list "test -f a.py && test -f b.py") 6
                            #:concurrency 4 #:worker-turns 3 #:emit emit))
  (session-close! sess)
  (define log (string-join (dump) "\n"))
  (check-true (string-contains? log "PARALLEL 2 workers"))
  (check-true (string-contains? log "DONE"))
  (check-true (file-exists? (build-path repo "a.py")))    ; 两个 worker 的产物都 merge 回主树
  (check-true (file-exists? (build-path repo "b.py")))
  (delete-directory/files repo)
) ; end test-case

(test-case "run-goal-dag!：无可并行任务(未声明 files) → 顺序路径复用 goal-step! → DONE"
  (define repo (make-temporary-file "pi2-seqdag-~a" 'directory))
  (git! repo "init" "-q") (git! repo "config" "user.email" "t@e.com") (git! repo "config" "user.name" "t")
  (void (call-with-output-file (build-path repo "PLAN.md")
          (lambda (o) (write-string "- [ ] {a} create a.py\n" o))))
  (git! repo "add" "-A") (git! repo "commit" "-m" "plan")
  (define host (make-plugin-host))
  (define cfg (struct-copy config (default-config) [workdir (path->string repo)] [permission-mode 'yolo]))
  (define d (make-deps #:provider (make-module-provider) #:registry (make-registry (list (make-write-file-tool)))
                       #:bus (make-bus) #:policy (make-policy cfg) #:plugin-host host))
  (define sess (session-open! (build-path repo "s.rktd") cfg))
  (define-values (emit dump) (collect-emit))
  (run-goal-dag! d (make-initial-state cfg) sess host "create a.py" (list "test -f a.py") 4 #:emit emit)
  (session-close! sess)
  (define log (string-join (dump) "\n"))
  (check-true (string-contains? log "DONE"))
  (check-false (string-contains? log "PARALLEL"))     ; 未声明 files → 不并行 → 走顺序 goal-step!
  (check-true (file-exists? (build-path repo "a.py")))
) ; end test-case

;; goal-step! 直接单测：一轮不达标 → 'continue + monitor 推进（复用于两个驱动）。
(test-case "goal-step!：单轮不过 → 'continue 且 monitor 状态推进"
  (define host (make-plugin-host))
  (define cfg (struct-copy config (default-config) [workdir (path->string tmpdir)] [permission-mode 'yolo]))
  (define d (make-deps #:provider (noop-provider) #:registry (make-registry '())
                       #:bus (make-bus) #:policy (make-policy cfg) #:plugin-host host))
  (define sess (session-open! (build-path tmpdir "gs.rktd") cfg))
  (define-values (emit _dump) (collect-emit))
  (define-values (decision st* m* dcost)
    (goal-step! d (make-initial-state cfg) sess host "x" (list "false") (path->string tmpdir) 0 5 0.0 (fresh-mon (make-initial-state cfg)) #:emit emit))
  (session-close! sess)
  (check-eq? decision 'continue)
  (check-true (mon? m*))
  (check-true (>= (mon-prev-signal m*) 1000000))       ; 记录了失败信号
) ; end test-case

(delete-directory/files tmpdir)
