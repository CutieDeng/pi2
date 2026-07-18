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

(delete-directory/files tmpdir)
