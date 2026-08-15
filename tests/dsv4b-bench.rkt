#lang tstring racket
;; dsv4b-bench.rkt — 真机 A/B：dsv4-b（Minimal 锚定两阶段）vs 非-b（v4-pro 全量首请求）。
;; design-dsv4b.md §5.3 的三态实验取前两态（第三态「纯双工具不促迁」意义有限，略）。
;;
;;   arm=anchored  --provider dsv4-b 等价装配：首请求恰 bash+str_replace_editor，
;;                 首个工具调用后促迁灌全量（loop.rkt 生产路径，非模拟）。
;;   arm=standard  deepseek 端点钉 deepseek-v4-pro：builtin 全量 + spawn_agent，
;;                 DEFAULT-SYSTEM；host 用 deepseek-lite 实例名保证升级梯 gate 关闭，
;;                 两臂均无模型切换干扰。
;;
;; 公平性：同任务同序、同 reasoning effort（PI_BENCH_EFFORT，默认 high）、同温度/
;; max-tokens/turn-max-calls；每 rep 交替先后。指标：验收通过率、工具调用数、
;; token in/out、耗时、估算费用。结果落 /tmp/dsv4b-bench-results.rktd。
;; 运行（需 deepseek 凭据）：racket tests/dsv4b-bench.rkt
;;   PI_BENCH_REPS=N（默认 2）  PI_BENCH_EFFORT=off|low|medium|high|max（默认 high）

(require
 racket/file
 racket/string
 racket/list
 racket/port
 (file "../src/model.rkt")
 (file "../src/event.rkt")
 (file "../src/tool.rkt")
 (file "../src/permission.rkt")
 (file "../src/loop.rkt")
 (file "../src/plugin.rkt")
 (file "../src/providers.rkt")
 (file "../src/provider-anthropic.rkt")
 (file "../src/credentials.rkt")
 (file "../src/pricing.rkt")
 (file "../src/dsv4b.rkt")
 (file "../src/subagent.rkt")
 (file "../src/tools/builtin.rkt")
 (file "../src/tools/editor.rkt")
 (file "../src/tools/bash-persistent.rkt")
) ; end require

;; ---------------------------------------------------------------- 参数

(define REPS (or (string->number (or (getenv "PI_BENCH_REPS") "")) 2))
(define EFFORT
  (let ([e (string->symbol (or (getenv "PI_BENCH_EFFORT") "high"))])
    (if (valid-reasoning-effort? e) e 'high)))
(define RUN-TIMEOUT-SECS 600)
(define RESULTS-PATH "/tmp/dsv4b-bench-results.rktd")

;; main.rkt DEFAULT-SYSTEM 逐字副本（module+ main 内不可 require，故复制）。
(define STANDARD-SYSTEM
  (string-join
   '("You are pi++, a concise coding agent running in a terminal."
     ""
     "Tools: use the provided tools (read_file, write_file, edit_file, glob, grep, bash, spawn_agent)"
     "to inspect and modify files and run commands. Prefer reading a file before editing it."
     ""
     "Verify, don't guess. When you are not certain about something — a file's contents, a symbol's"
     "definition, an API's shape, the project layout, a command's output, or whether a path/name even"
     "exists — do NOT answer from assumption. First use a tool (read_file / grep / glob / bash) to"
     "check, then answer from what you actually observed. Never fabricate file paths, identifiers, or"
     "results. If something remains uncertain after checking, say so and state the assumption you made."
     ""
     "Be concise. Explain briefly what you did and why, and prefer showing evidence (a file excerpt or"
     "command output) over asserting.")
   "\n"))

;; ---------------------------------------------------------------- 任务

;; 验收辅助：dir 下跑 bash 命令，返回 trim 后 stdout（错误 → #f）。
(define (sh dir cmd)
  (parameterize ([current-directory dir])
    (define-values (proc out in _e) (subprocess #f #f 'stdout "/bin/bash" "-c" cmd))
    (close-output-port in)
    (define s (port->string out))
    (subprocess-wait proc)
    (close-input-port out)
    (and (zero? (subprocess-status proc)) (string-trim s))))

(struct bench-task (name seed! prompt accept) #:transparent)

;; easy 集：多步但直给（首轮显示两臂 100% 通过 → 天花板效应，只能比成本）。
;; hard 集：多约束/易错任务，用于判别质量差异。PI_BENCH_TASKS=easy|hard|all。
(define EASY-TASKS
  (list
   (bench-task
    "fix-sum"
    (lambda (dir)
      (display-to-file "#!/bin/bash\necho $(($1 - $2))\n" (build-path dir "sum.sh")))
    (lambda (dir)
      f"The script {dir}/sum.sh is supposed to print the sum of its two integer arguments, but it is buggy. Fix it and verify by running it with a couple of inputs. Stop once verified.")
    (lambda (dir)
      (and (equal? (sh dir "bash sum.sh 3 5") "8")
           (equal? (sh dir "bash sum.sh 10 -2") "8"))))
   (bench-task
    "revwords"
    (lambda (_dir) (void))
    (lambda (dir)
      f"Create a script at {dir}/revwords.sh that reads one line from stdin and prints its words in reverse order, separated by single spaces. Verify it works, then stop.")
    (lambda (dir)
      (equal? (sh dir "echo 'alpha beta gamma' | bash revwords.sh") "gamma beta alpha")))
   (bench-task
    "top3-readme"
    (lambda (dir)
      (display-to-file "7\n42\n3\n19\n5\n88\n1\n" (build-path dir "data.txt")))
    (lambda (dir)
      f"In {dir} there is a data.txt with one integer per line. Create {dir}/top3.sh that prints the 3 largest numbers in data.txt in descending order, one per line. Also write {dir}/README.md briefly documenting how to use top3.sh. Verify the script output, then stop.")
    (lambda (dir)
      (and (equal? (sh dir "bash top3.sh") "88\n42\n19")
           (let ([rp (build-path dir "README.md")])
             (and (file-exists? rp)
                  (non-empty-string? (string-trim (file->string rp))))))))
  ) ; end list
) ; end define EASY-TASKS

(define HARD-TASKS
  (list
   ;; 多约束文本处理：大小写折叠 + 去标点 + 频次排序 + 平局字典序 + 恰取前2。
   (bench-task
    "wordfreq"
    (lambda (dir)
      (display-to-file "The cat and the dog. A cat!\ndog dog, bird\n"
                       (build-path dir "text.txt")))
    (lambda (dir)
      (string-append
       f"Create {dir}/wordfreq.sh that reads {dir}/text.txt and prints the 2 most frequent words, "
       "one per line as `word count`. Rules: case-insensitive (output lowercase), strip all "
       "punctuation, sort by count descending, break ties alphabetically. Verify against the "
       "actual file, then stop."))
    (lambda (dir)
      (equal? (sh dir "bash wordfreq.sh") "dog 3\ncat 2")))
   ;; 经典引号坑：含空格文件名。天真修法（只加引号不改 ls 解析）仍会挂。
   (bench-task
    "fix-quote"
    (lambda (dir)
      (display-to-file
       "#!/bin/bash\n# copy all .txt files from dir $1 into dir $2\nfor f in $(ls $1/*.txt); do\n  cp $f $2\ndone\n"
       (build-path dir "backup.sh")))
    (lambda (dir)
      (string-append
       f"{dir}/backup.sh should copy all .txt files from a source dir (arg 1) into a dest dir (arg 2), "
       "but it fails when filenames contain spaces. Fix it, verify with a filename that contains a "
       "space, then stop."))
    (lambda (dir)
      (and (sh dir "rm -rf s d && mkdir s d && printf AB > 's/a b.txt' && printf C > s/c.txt")
           (sh dir "bash backup.sh s d")
           (equal? (sh dir "cat 'd/a b.txt' 2>/dev/null") "AB")
           (equal? (sh dir "cat d/c.txt 2>/dev/null") "C"))))
  ) ; end list
) ; end define HARD-TASKS

;; review 集：代码审查（只读约束）。防改三层：任务目录本就是临时副本；
;; project/ 播种后 chmod -R a-w（写在工具层失败）；验收前全树哈希对比，
;; 有改动即判负（违规本身是指标）。审查质量 = 播种 bug 召回：3 个已知 bug，
;; findings 限 6 条（防「全列一遍」刷召回），验收 grep 函数名+症状词，≥2/3 过。

(define REVIEW-FILES
  (list
   (cons "project/parser.py"
         (string-join
          '("def parse_kv(line):"
            "    \"\"\"Parse 'k=v;k2=v2' into a dict.\"\"\""
            "    out = {}"
            "    for pair in line.split(\";\"):"
            "        if not pair:"
            "            continue"
            "        k, v = pair.split(\"=\")"          ; BUG1: 值含 '=' 时 unpack 崩
            "        out[k.strip()] = v.strip()"
            "    return out"
            "") "\n"))
   (cons "project/window.py"
         (string-join
          '("def last_n(items, n):"
            "    \"\"\"Return the last n items (empty list when n == 0).\"\"\""
            "    return items[-n:]"                     ; BUG2: n=0 返回整表
            "") "\n"))
   (cons "project/stats.py"
         (string-join
          '("def mean(xs):"
            "    \"\"\"Arithmetic mean; should return 0.0 for empty input.\"\"\""
            "    return sum(xs) / len(xs)"              ; BUG3: 空表除零
            ""
            "def total(xs):"
            "    return sum(xs)"
            "") "\n"))
   (cons "project/util.py"
         (string-join
          '("def clamp(x, lo, hi):"
            "    return max(lo, min(hi, x))"
            ""
            "def is_comment(line):"
            "    return line.lstrip().startswith(\"#\")"
            "") "\n"))
   (cons "project/main.py"
         (string-join
          '("import sys"
            "from parser import parse_kv"
            "from window import last_n"
            "from stats import mean"
            "from util import is_comment"
            ""
            "def run(path):"
            "    vals = []"
            "    with open(path) as f:"
            "        for line in f:"
            "            if is_comment(line):"
            "                continue"
            "            kv = parse_kv(line.strip())"
            "            if \"v\" in kv:"
            "                vals.append(float(kv[\"v\"]))"
            "    print(mean(last_n(vals, 10)))"
            ""
            "if __name__ == \"__main__\":"
            "    run(sys.argv[1])"
            "") "\n"))
  ) ; end list
) ; end define REVIEW-FILES

;; review 行是否命中某 bug：行内含（文件名或函数名之一）且含任一症状词（大小写不敏感）。
(define (review-hits review-text names keywords)
  (for/or ([ln (in-list (string-split (string-downcase review-text) "\n"))])
    (and (for/or ([n (in-list names)]) (string-contains? ln n))
         (for/or ([k (in-list keywords)]) (string-contains? ln k))))
) ; end define review-hits

;; 播种文件名无空格，普通 xargs 即可（BSD sort 无 -z）。
(define (tree-hash dir)
  (sh dir "find project -type f | sort | xargs shasum | shasum")
) ; end define tree-hash

(define REVIEW-TASKS
  (list
   (bench-task
    "review-bugs"
    (lambda (dir)
      (make-directory* (build-path dir "project"))
      (for ([f (in-list REVIEW-FILES)])
        (display-to-file (cdr f) (build-path dir (car f))))
      (display-to-file (tree-hash dir) (build-path dir ".baseline"))
      (sh dir "chmod -R a-w project"))
    (lambda (dir)
      (string-append
       f"Review the small Python project at {dir}/project for bugs (logic errors, crash-prone edge "
       "cases). Do NOT modify anything under project/ — it is read-only reference code. Write your "
       f"findings to {dir}/REVIEW.md, one finding per line, format `<file>: <short description>`, "
       "at most 6 findings, most severe first. Then stop."))
    (lambda (dir)
      (define unchanged? (equal? (tree-hash dir) (file->string (build-path dir ".baseline"))))
      (define rp (build-path dir "REVIEW.md"))
      (define review (if (file-exists? rp) (file->string rp) ""))
      (define lines (length (filter non-empty-string?
                                    (map string-trim (string-split review "\n")))))
      (define found
        (+ (if (review-hits review '("parse_kv" "parser.py" "parser")
                            '("=" "unpack" "split" "maxsplit" "equals")) 1 0)
           (if (review-hits review '("last_n" "window.py" "window")
                            '("n=0" "n == 0" "n is 0" "zero" "empty" "whole" "entire")) 1 0)
           (if (review-hits review '("mean" "stats.py" "stats")
                            '("empty" "zero" "division" "divide" "len(")) 1 0)))
      (sh dir "chmod -R u+w project")                  ; 恢复可写，供清理
      (printf "    [review: found=~a/3 lines=~a unchanged=~a]\n" found lines unchanged?)
      (and unchanged? (<= lines 10) (>= found 2))))
  ) ; end list
) ; end define REVIEW-TASKS

(define TASKS
  (case (or (getenv "PI_BENCH_TASKS") "easy")
    [("hard")   HARD-TASKS]
    [("review") REVIEW-TASKS]
    [("all")    (append EASY-TASKS HARD-TASKS REVIEW-TASKS)]
    [else       EASY-TASKS]
  ) ; end case
) ; end define TASKS

;; ---------------------------------------------------------------- 装配

(define (base-cfg dir)
  (struct-copy config (default-config)
               [max-tokens 8192]
               [context-budget 200000]
               [permission-mode 'yolo]
               [workdir (path->string dir)]))

;; arm → (values cfg registry host)。registry/provider/host 每 run 全新。
(define (assemble arm dir)
  (case arm
    [(anchored)
     (define cfg (struct-copy config (apply-provider-profile (base-cfg dir) "dsv4-b")
                              [system-prompt ANCHOR-SYSTEM]))
     (define host (make-plugin-host))
     (register-builtin-providers! host)
     (host-set-provider! host "dsv4-b")
     (define reg (make-registry (list (make-persistent-bash-tool) (make-editor-tool))))
     (set-dsv4b-payload!
      (lambda ()
        (for ([t (in-list (builtin-tools cfg))]
              #:unless (string=? (tool-name t) "bash"))
          (registry-add! reg t))
        (dsv4b-unlock-addendum "" "")))
     (values cfg reg host)]
    [(standard)
     (define cfg (struct-copy config (apply-provider-profile (base-cfg dir) "deepseek-lite")
                              [model "deepseek-v4-pro"]
                              [system-prompt STANDARD-SYSTEM]))
     ;; host 用 deepseek-lite：base ≠ deepseek/dsv4-b ⇒ 升级梯与促迁 gate 双关。
     (define host (make-plugin-host))
     (register-builtin-providers! host)
     (host-set-provider! host "deepseek-lite")
     (define reg (make-registry (builtin-tools cfg)))
     (registry-add! reg (make-spawn-agent-tool
                         #:provider (make-anthropic-provider cfg)
                         #:sub-tools (builtin-tools cfg)))
     (values cfg reg host)]
  ) ; end case
) ; end define assemble

;; ---------------------------------------------------------------- 执行

;; 看门狗：secs 内跑完 thunk → (list 'ok v)；超时杀 custodian → (list 'err 'timeout)。
(define (with-timeout secs thunk)
  (define cust (make-custodian))
  (define result (box (list 'err 'died)))
  (define th
    (parameterize ([current-custodian cust])
      (thread
       (lambda ()
         (set-box! result
                   (with-handlers ([exn:fail? (lambda (e) (list 'err (exn-message e)))])
                     (list 'ok (thunk))))))))
  (cond
    [(sync/timeout secs th) (unbox result)]
    [else (custodian-shutdown-all cust) (list 'err 'timeout)])
) ; end define with-timeout

(define (count-tool-calls st)
  (for/sum ([m (in-list (state-history-list st))]
            #:when (eq? (message-role m) 'assistant))
    (length (message-tool-uses m)))
) ; end define count-tool-calls

;; 一次 run：全新 tmpdir/registry/provider/host；返回结果 hash。
(define (run-one arm task rep)
  (dsv4b-reset!)
  (define dir (make-temporary-file f"pi2-bench-{arm}-{(bench-task-name task)}-~a" 'directory))
  ((bench-task-seed! task) dir)
  (define-values (cfg reg host) (assemble arm dir))
  (define bus (make-bus))
  (define unsub
    (bus-subscribe! bus (lambda (e)
                          (when (evt:tool-start? e)
                            (printf "    [~a]" (tool-use-block-name (evt:tool-start-block e)))
                            (flush-output)))))
  (define d (make-deps #:provider (make-anthropic-provider cfg)
                       #:registry reg #:bus bus
                       #:policy (make-policy cfg)
                       #:asker (lambda (_q) 'yes)
                       #:plugin-host host))
  (define st (make-initial-state cfg))
  (define t0 (current-inexact-milliseconds))
  (define r
    (parameterize ([current-reasoning-effort EFFORT])
      (with-timeout RUN-TIMEOUT-SECS
        (lambda () (run-turn! st (text-msg 'user ((bench-task-prompt task) dir)) d)))))
  (define ms (- (current-inexact-milliseconds) t0))
  (bus-drain! bus) (unsub) (newline)
  (define ok-run? (eq? (car r) 'ok))
  (define st* (and ok-run? (cadr r)))
  (define accepted? (and ok-run? (with-handlers ([exn:fail? (lambda (_e) #f)])
                                   ((bench-task-accept task) dir))))
  (define u (if st* (agent-state-token-usage st*) usage-zero))
  (define row
    (hash 'arm arm 'task (bench-task-name task) 'rep rep
          'ok (and accepted? #t)
          'error (if ok-run? #f (cadr r))
          'tool-calls (if st* (count-tool-calls st*) 0)
          'tokens-in (usage-input-tokens u)
          'tokens-out (usage-output-tokens u)
          'ms (inexact->exact (round ms))
          'promoted (dsv4b-promoted?)
          'cost (cost-line "deepseek-v4-pro" u)))
  (sh dir "chmod -R u+w . 2>/dev/null; true")          ; review 任务可能留下只读树
  (delete-directory/files dir)
  row
) ; end define run-one

;; ---------------------------------------------------------------- 主流程

(define key (resolve-provider-token "dsv4-b" "default"))
(cond
  [(not key)
   (displayln "dsv4b-bench: SKIP (no deepseek credentials)")]
  [else
   (printf "dsv4b-bench: reps=~a effort=~a timeout=~as model=deepseek-v4-pro key=~a\n\n"
           REPS EFFORT RUN-TIMEOUT-SECS (mask-key key))
   (define rows
     (for*/list ([task (in-list TASKS)] [rep (in-range REPS)]
                 [arm (in-list (if (even? rep) '(anchored standard) '(standard anchored)))])
       (printf "== ~a / ~a / rep~a\n" (bench-task-name task) arm rep)
       (define row (run-one arm task rep))
       (printf "  -> ok=~a calls=~a in=~a out=~a ~as promoted=~a~a\n\n"
               (hash-ref row 'ok) (hash-ref row 'tool-calls)
               (hash-ref row 'tokens-in) (hash-ref row 'tokens-out)
               (real->decimal-string (/ (hash-ref row 'ms) 1000.0) 1)
               (hash-ref row 'promoted)
               (let ([e (hash-ref row 'error)]) (if e (format " ERROR=~a" e) "")))
       (flush-output)
       row))
   (call-with-output-file RESULTS-PATH #:exists 'truncate
     (lambda (out) (write rows out)))

   ;; 汇总：按 arm 聚合
   (define (mean xs) (if (null? xs) 0 (/ (apply + xs) (length xs))))
   (printf "==== summary (~a runs each arm; results: ~a) ====\n" (* REPS (length TASKS)) RESULTS-PATH)
   (printf "~a\n" (string-join (list "arm" "pass" "calls" "tok-in" "tok-out" "secs") "\t"))
   (for ([arm (in-list '(anchored standard))])
     (define rs (filter (lambda (r) (eq? (hash-ref r 'arm) arm)) rows))
     (printf "~a\t~a/~a\t~a\t~a\t~a\t~a\n"
             arm
             (count (lambda (r) (hash-ref r 'ok)) rs) (length rs)
             (real->decimal-string (exact->inexact (mean (map (lambda (r) (hash-ref r 'tool-calls)) rs))) 1)
             (inexact->exact (round (mean (map (lambda (r) (hash-ref r 'tokens-in)) rs))))
             (inexact->exact (round (mean (map (lambda (r) (hash-ref r 'tokens-out)) rs))))
             (real->decimal-string (/ (mean (map (lambda (r) (hash-ref r 'ms)) rs)) 1000.0) 1)))
   ;; 按任务细分
   (printf "\nper-task pass (anchored | standard):\n")
   (for ([task (in-list TASKS)])
     (define (p arm)
       (define rs (filter (lambda (r) (and (eq? (hash-ref r 'arm) arm)
                                           (equal? (hash-ref r 'task) (bench-task-name task))))
                          rows))
       (format "~a/~a" (count (lambda (r) (hash-ref r 'ok)) rs) (length rs)))
     (printf "  ~a: ~a | ~a\n" (bench-task-name task) (p 'anchored) (p 'standard)))
   (displayln "dsv4b-bench: DONE")]
) ; end cond
