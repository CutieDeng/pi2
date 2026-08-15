#lang tstring racket
;; bash-persistent-test.rkt — dsv4-b 持久化 bash（design-dsv4b.md §2.3 / §5.2）。

(require
 rackunit
 racket/file
 racket/string
 (file "../src/tool.rkt")
 (file "../src/tools/bash-persistent.rkt")
) ; end require

(define t (make-persistent-bash-tool))
(define tmp (path->string (make-temporary-file "pi2-pbtest-~a" 'directory)))
(define ctx (tool-ctx tmp void #f))

(define (run cmd) (tool-run t (hasheq 'command cmd) ctx))

(test-case "schema：name=bash、command 必填、描述声明持久性（对齐 minimal 预设覆写）"
  (check-equal? (tool-name t) "bash")
  (check-equal? (tool-permission-level t) 'dangerous)
  (define fn (hash-ref (tool-spec t) 'function))
  (check-equal? (hash-ref fn 'name) "bash")
  (check-equal? (hash-ref fn 'description) PERSISTENT-BASH-DESCRIPTION)
  (check-true (string-contains? PERSISTENT-BASH-DESCRIPTION
                                "State is persistent across command calls"))
  ;; 刻意不含基准沙箱专属的两条（本地为假，见模块头注释）
  (check-false (string-contains? PERSISTENT-BASH-DESCRIPTION "internet"))
  (check-equal? (hash-ref (hash-ref fn 'parameters) 'required) '("command"))
  (check-equal? (hash-ref (hash-ref (hash-ref (hash-ref fn 'parameters) 'properties) 'command) 'description)
                "The bash command to run. Relative path is preferred in the command.")
) ; end test-case

(test-case "初始 cwd = workdir"
  (define oc (run "pwd"))
  (check-false (tool-outcome-is-error? oc))
  ;; darwin 下 /tmp 与 /private/tmp 同体，比对末段目录名即可
  (check-true (string-contains? (tool-outcome-content oc)
                                (let-values ([(_b n _d) (split-path (string->path tmp))])
                                  (path->string n))))
) ; end test-case

(test-case "cwd 与导出环境变量跨调用持久"
  (void (run "mkdir -p sub && cd sub && export PI2_PB_TEST=alive"))
  (define oc (run "echo $PWD:$PI2_PB_TEST"))
  (check-false (tool-outcome-is-error? oc))
  (define out (tool-outcome-content oc))
  (check-true (string-contains? out "/sub:alive") f"应见 …/sub:alive，实得 {out}")
) ; end test-case

(test-case "退出码：非零 → error outcome，措辞 exit code N"
  (define oc (run "exit 7"))
  (check-true (tool-outcome-is-error? oc))
  (check-true (string-prefix? (tool-outcome-content oc) "exit code 7"))
  ;; 失败调用不破坏后续状态持久
  (check-true (string-contains? (tool-outcome-content (run "echo again:$PI2_PB_TEST")) "again:alive"))
) ; end test-case

(test-case "空命令拒绝；无输出占位"
  (check-true (tool-outcome-is-error? (run "  ")))
  (check-equal? (tool-outcome-content (run "true")) "(no output)")
) ; end test-case

(test-case "后台任务不长阻塞：sleep 2 & 应远早于 2s 返回"
  (define t0 (current-inexact-milliseconds))
  (define oc (run "sleep 2 >/dev/null 2>&1 & echo started"))
  (define dt (- (current-inexact-milliseconds) t0))
  (check-false (tool-outcome-is-error? oc))
  (check-true (string-contains? (tool-outcome-content oc) "started"))
  (check-true (< dt 1900) f"后台任务拖住了泵线程：{dt}ms")
) ; end test-case

(delete-directory/files (string->path tmp))
