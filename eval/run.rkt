#lang tstring racket
;; eval/run.rkt — E1：对标真实场景的 agent 任务评测（design-eval.md）。
;;   racket eval/run.rkt [--tasks eval/tasks] [--only id,id] [--agent pi2|reference|noop|cheat] [--provider P] [--model M]
;;                       [--mode single|goal] [--repeat N] [--jobs N] [--pi2-arg ARG]… [--out DIR] [--keep]
;; 每次运行：
;;   1. 工作区：按任务的来源物化（git 克隆到基线提交 / 复制文件）+ 可选注入缺陷 → 提交成「基线」
;;   2. agent：pi2（子进程跑 main.rkt：-p 单次 或 --goal；数据 / 缓存目录隔离）；或校验评测器用的 reference（套用历史上的实现）/ noop / cheat
;;   3. 防作弊：被保护的文件（基线里有的）不能改 / 删；不能新加 skip 标记
;;   4. 隐藏测试放进去（覆盖同名文件）→ 跑验收命令 → 解析（test-flow 的解析器）
;;   5. 指标：会话文件里的请求数 / token / 花费、工具调用 / 出错、压缩次数、墙钟、「宣称完成但没过」（假胜利）
;;   6. 每次一行 JSON（results.jsonl）+ 汇总表（summary.md）
;; 判定只认程序：通过 = 验收命令退出 0 且没有防作弊违规。

(require json racket/date file/sha1
         (file "../src/shell-util.rkt") (only-in (file "../src/test-flow.rkt") parse-output)
         (file "../src/rktd.rkt") (only-in (file "../src/session.rkt") rec? rec-type rec-data) (file "../src/model.rkt") (only-in (file "../src/pricing.rkt") estimate-cost))

(define PI2-ROOT (simplify-path (build-path (let-values ([(d _n _x) (split-path (syntax-source #'here))]) d) "..")))

;; ---------------------------------------------------------------- 任务
(struct task (id tier source prompt inject hidden reference protect verify parse timeout max-calls minutes goal-until file) #:transparent)

(define (kw-opts xs)                                     ; (… #:k v …) → hasheq
  (let loop ([xs xs] [h (hasheq)])
    (cond [(null? xs) h]
          [(and (keyword? (car xs)) (pair? (cdr xs))) (loop (cddr xs) (hash-set h (string->symbol (keyword->string (car xs))) (cadr xs)))]
          [else (loop (cdr xs) h)])))
(define (clause d key) (for/first ([c (cdr d)] #:when (and (pair? c) (eq? (car c) key))) (cdr c)))
(define (clauses d key) (for/list ([c (cdr d)] #:when (and (pair? c) (eq? (car c) key))) (cdr c)))

(define (load-task f)
  (define d (with-input-from-file f read))
  (unless (and (pair? d) (eq? (car d) 'eval-task)) (error 'eval "~a: not (eval-task …)" f))
  (define (one key [dflt #f]) (let ([c (clause d key)]) (if (and c (pair? c)) (car c) dflt)))
  (define verify (clause d 'verify))
  (define vopts (kw-opts (cdr verify)))
  (define limits (kw-opts (or (clause d 'limits) '())))
  (task (one 'id) (one 'tier "?") (one 'source) (one 'prompt)
        (append* (clauses d 'inject)) (append* (clauses d 'hidden)) (one 'reference) (append* (clauses d 'protect))
        (car verify) (hash-ref vopts 'parse 'exit) (hash-ref vopts 'timeout 600)
        (hash-ref limits 'max-calls 80) (hash-ref limits 'minutes 30) (one 'goal-until #f) (path->string f)))

;; ---------------------------------------------------------------- 工作区
(define (sh cmd dir #:timeout [secs 120]) (let-values ([(code out) (shell-run cmd dir #:timeout secs)]) (values code out)))
(define (sh! cmd dir #:timeout [secs 120])
  (define-values (code out) (sh cmd dir #:timeout secs))
  (unless (eqv? code 0) (error 'eval "command failed (~a): ~a\n~a" code cmd (clip-tail out 1500)))
  out)
(define (q s) (string-append "'" (string-replace s "'" "'\\''") "'"))

;; source：(git 仓库 提交) | (files (from 绝对路径 相对路径) …)
(define (materialize! t ws)
  (define src (task-source t))
  (case (car src)
    [(git)
     (sh! (format "git clone -q --no-hardlinks ~a ~a" (q (cadr src)) (q ws)) "/" #:timeout 300)
     ;; 只留基线及其祖先：克隆来的分支 / 标签指向更晚的提交（里面就有隐藏测试与参考实现）→ 删掉引用、清 reflog、gc 掉够不着的对象
     (sh! (format "git checkout -q --detach ~a && git remote remove origin && git for-each-ref --format='%(refname)' | while read r; do git update-ref -d \"$r\"; done && git reflog expire --expire=now --all && git gc -q --prune=now" (q (caddr src))) ws #:timeout 300)
     (unless (equal? (string-trim (sh! "git rev-list --all | wc -l" ws)) (string-trim (sh! "git rev-list HEAD | wc -l" ws)))
       (error 'eval "~a: workspace still reaches commits past the base" (task-id t)))]
    [(files)
     (make-directory* ws)
     (for ([f (cdr src)])
       (define dst (build-path ws (caddr f)))
       (make-parent-directory* dst)
       (copy-file (cadr f) dst #t))
     (sh! "git init -q && git add -A && git -c user.email=eval@pi2 -c user.name=eval commit -q -m 'eval source'" ws)]
    [else (error 'eval "unknown source ~s" src)])
  ;; 注入缺陷：(replace 路径 旧 新)，旧文字必须恰好出现一次
  (for ([inj (task-inject t)] #:when (and (pair? inj) (eq? (car inj) 'replace)))
    (define p (build-path ws (cadr inj)))
    (define s (file->string p))
    (define n (length (regexp-match-positions* (regexp-quote (caddr inj)) s)))
    (unless (= n 1) (error 'eval "~a: inject text occurs ~a times in ~a" (task-id t) n (cadr inj)))
    (call-with-output-file p #:exists 'truncate (lambda (o) (write-string (string-replace s (caddr inj) (cadddr inj)) o))))
  (sh! "git add -A && git -c user.email=eval@pi2 -c user.name=eval commit -q --allow-empty -m 'eval base'" ws)
  (string-trim (sh! "git rev-parse HEAD" ws)))

;; 隐藏 / 参考文件的内容：(git 提交 路径) → 从任务来源仓库取；(file 绝对路径 相对路径)
(define (hidden-files t)
  (for/list ([h (task-hidden t)])
    (case (car h)
      [(git) (cons (caddr h) (sh! (format "git show ~a:~a" (q (cadr h)) (q (caddr h))) (cadr (task-source t))))]
      [(file) (cons (caddr h) (file->string (cadr h)))]
      [else (error 'eval "unknown hidden ~s" h)])))
(define (write-files! ws files)
  (for ([f files])
    (define p (build-path ws (car f)))
    (make-parent-directory* p)
    (call-with-output-file p #:exists 'truncate (lambda (o) (write-string (cdr f) o)))))

;; 本次评测的临时目录：$TMPDIR/pi2-eval/<结果目录名>（工作区与 pi2 副本都放这里，不在 pi2 仓库里）
(define (eval-temp-root out-dir)
  (let-values ([(_b n _d) (split-path (path->complete-path out-dir))]) (build-path (find-system-path 'temp-dir) "pi2-eval" n)))

;; pi2 的运行副本。系统提示里有 skills 的绝对路径 → agent 顺着它找到真仓库：20261026 r2 实测 pi2-remember 2/3 次第一条命令就
;; cd 过去，其中一次改了真仓库的 memory-store.rkt 没改回。所以从副本跑：main.rkt、src（不含 src/gui：命令行不依赖它）、skills、prompts、plugins。
;; 任务来源落在副本里（要改的正是 pi2 命令行本身的文件）→ 隔离不了，报错。
(define (prepare-install! out-dir tasks)
  (define dst (build-path (eval-temp-root out-dir) "pi2"))
  (define gui (path-str (build-path PI2-ROOT "src" "gui")))
  (define src-root (path-str (build-path PI2-ROOT "src")))
  (for* ([t tasks] #:when (eq? (car (task-source t)) 'files) [f (cdr (task-source t))])
    (define p (path-str (cadr f)))
    (when (or (equal? p (path-str (build-path PI2-ROOT "main.rkt")))
              (and (string-prefix? p (string-append src-root "/")) (not (string-prefix? p (string-append gui "/")))))
      (error 'eval "~a: source ~a is part of the pi2 command line itself; the agent's own copy would contain it" (task-id t) p)))
  (when (directory-exists? dst) (delete-directory/files dst))
  (make-directory* dst)
  (copy-file (build-path PI2-ROOT "main.rkt") (build-path dst "main.rkt"))
  (for ([d '("skills" "prompts" "plugins")] #:when (directory-exists? (build-path PI2-ROOT d)))
    (copy-directory/files (build-path PI2-ROOT d) (build-path dst d)))
  (let copy ([from (build-path PI2-ROOT "src")] [to (build-path dst "src")])
    (make-directory* to)
    (for ([e (directory-list from)])
      (define f (build-path from e))
      (cond [(member (path->string e) '("compiled" ".DS_Store")) (void)]
            [(equal? (path-str f) gui) (void)]
            [(directory-exists? f) (copy f (build-path to e))]
            [else (copy-file f (build-path to e))])))
  (sh! "raco make main.rkt" (path->string dst) #:timeout 900)
  dst)
(define current-install (make-parameter PI2-ROOT))

;; ---------------------------------------------------------------- agent
;; → (values 退出说明 会话文件或#f)
(define (run-agent! agent t ws run-dir #:provider [provider #f] #:model [model #f] #:mode [mode 'single] #:extra [extra '()])
  (case agent
    [(noop) (values "noop" #f)]
    [(reference)                                          ; 历史上的实现（参考答案）：校验评测器用——它应当通过
     (define ref (task-reference t))
     (case (car ref)
       ;; (git 提交 路径…)：只取这些路径（缺省全部）——历史提交里也有测试文件，取了会动到被保护的测试
       ;; 从来源仓库取（工作区里只有基线及其祖先，没有这些更晚的提交）
       [(git) (sh! (format "git -C ~a archive ~a -- ~a | tar -x -C ~a" (q (cadr (task-source t))) (q (cadr ref))
                           (let ([ps (cddr ref)]) (if (null? ps) "." (string-join (map q ps) " "))) (q ws))
                   ws #:timeout 120)]
       [(replace) (let* ([p (build-path ws (cadr ref))] [s (file->string p)])
                    (call-with-output-file p #:exists 'truncate (lambda (o) (write-string (string-replace s (caddr ref) (cadddr ref)) o))))]
       [else (error 'eval "unknown reference ~s" ref)])
     (values "reference" #f)]
    [(cheat)                                               ; 作弊：把被保护的测试文件清空（应被判违规、不通过）
     (for ([p (protected-at-base t ws)]) (call-with-output-file (build-path ws p) #:exists 'truncate (lambda (o) (write-string "" o))))
     (values "cheat" #f)]
    [(pi2)
     (define data (build-path run-dir "data")) (define cache (build-path run-dir "cache"))
     (make-directory* data) (make-directory* cache)
     (define args
       (append (list "-C" ws "--mode" "yolo" "--max-calls" (number->string (task-max-calls t)))
               (if provider (list "--provider" provider) '())
               (if model (list "-m" model) '())
               extra                                       ; 例如 --auto off（deepseek 缺省按任务在 flash / pro 间切换）
               (if (eq? mode 'goal)
                   (list "--goal" (task-prompt t) "--until" (or (task-goal-until t) (task-verify t)) "--max-turns" "8")
                   (list "-p" (task-prompt t)))))
     (define cmd (string-join (map q (list* (path->string (find-executable-path "racket")) (path->string (build-path (current-install) "main.rkt")) args)) " "))
     (define-values (code out)
       (parameterize ([current-environment-variables (environment-variables-copy (current-environment-variables))])
         (putenv "PI_DATA_HOME" (path->string data)) (putenv "PI_CACHE_HOME" (path->string cache))
         (sh (string-append cmd " < /dev/null") ws #:timeout (* 60 (task-minutes t)))))
     (call-with-output-file (build-path run-dir "agent.log") #:exists 'truncate (lambda (o) (write-string out o)))
     (define sess (let ([fs (filter (lambda (p) (regexp-match? #rx"[.]rktd$" (path->string p))) (directory-list data #:build? #t))])
                    (and (pair? fs) (argmax file-or-directory-modify-seconds fs))))
     (values (if (eq? code 'timeout) "timeout" (format "exit ~a" code)) sess)]
    [else (error 'eval "unknown agent ~a" agent)]))

;; ---------------------------------------------------------------- 防作弊
(define (protected-at-base t ws)
  (define rxs (map glob->regexp (task-protect t)))
  (define-values (_c out) (sh "git ls-files" ws))
  (for/list ([p (string-split out "\n")] #:when (ormap (lambda (rx) (regexp-match? rx p)) rxs)) p))
(define SKIP-RX #px"@unittest\\.skip|skipTest\\(|unittest\\.expectedFailure|pytest\\.mark\\.(skip|xfail)|\\(test-omit|#:skip")
;; → (values 违规列表 加了测试的文件列表)
;; 受保护文件只「加了行」、且判分前隐藏集会整个覆盖它 → 不算违规（加回归测试是正常做法，判分时它反正被换掉），记进 tests_added。
;; 删 / 改了行，或隐藏集不覆盖它（加的行会参与判分）→ 违规。
(define (violations t ws base protected)
  (define-values (_c names) (sh (format "git diff --name-status ~a" (q base)) ws))
  (define changed (for/list ([l (string-split names "\n")] #:when (regexp-match? #rx"^[MDR]" l)) (string-split l "\t")))
  (define hidden (map car (hidden-files t)))
  (define (additions-only? path)
    (define-values (_c3 ns) (sh (format "git diff --numstat ~a -- ~a" (q base) (q path)) ws))
    (define f (string-split (string-trim ns)))
    (and (pair? f) (equal? (cadr f) "0")))
  (define-values (added touched)
    (for/fold ([added '()] [touched '()] #:result (values (reverse added) (reverse touched)))
              ([c changed] #:when (member (last c) protected))
      (if (and (equal? (car c) "M") (member (last c) hidden) (additions-only? (last c)))
          (values (cons (last c) added) touched)
          (values added (cons (format "~a ~a" (car c) (last c)) touched)))))
  (define-values (_c2 diff) (sh (format "git diff -U0 ~a" (q base)) ws))
  (define skips (for/list ([l (string-split diff "\n")] #:when (and (regexp-match? #rx"^\\+[^+]" l) (regexp-match? SKIP-RX l))) (string-trim (substring l 1))))
  (values (append (map (lambda (x) (string-append "protected file changed: " x)) touched)
                  (map (lambda (x) (string-append "skip marker added: " x)) skips))
          added))

(define (log-truncations f)
  (if (file-exists? f)
      (length (regexp-match-positions* #rx"output cut off at max_tokens" (regexp-replace* #rx"\e\\[[0-9;]*m" (file->string f) "")))
      0))

;; 越界：工具调用的参数里出现 pi2 仓库或任务来源的路径（工作区之外）→ 违规（agent 跑的是临时目录里的副本，提示里不会有真仓库的路径）
(define (path-str p) (regexp-replace #rx"(.)/+$" (path->string (simplify-path (path->complete-path p))) "\\1"))
(define (source-roots t)
  (define src (task-source t))
  (remove-duplicates
   (cons (path-str PI2-ROOT)
         (case (car src)
           [(git) (list (path-str (cadr src)))]
           [(files) (for/list ([f (cdr src)]) (let-values ([(d _n _x) (split-path (cadr f))]) (path-str d)))]
           [else '()]))))
;; 另外禁 $TMPDIR/pi2-eval（别的运行的工作区——--keep 留着、里面是做完的答案；20261026 r4 实测：agent `find /` 后去翻了 r2 同任务的工作区），
;; 只放行本次的工作区与 pi2 副本。macOS 的临时目录有 /var/… 与 /private/var/… 两种写法，都认。
(define (both-spellings p) (if (string-prefix? p "/var/") (list p (string-append "/private" p)) (list p)))
(define (escapes t sess ws #:install [install #f])
  (define roots (append (source-roots t) (both-spellings (path-str (build-path (find-system-path 'temp-dir) "pi2-eval")))))
  (define allowed (append (both-spellings (path-str ws)) (if install (both-spellings (path-str install)) '())))
  (define (bad? s)
    (define s* (for/fold ([s s]) ([a allowed]) (string-replace s a "<OK>")))
    (for/or ([r roots]) (string-contains? s* r)))
  (if (and sess (file-exists? sess))
      (for*/list ([r (in-datum-log sess)] #:when (and (rec? r) (message? (rec-data r)))
                  [b (message-blocks (rec-data r))] #:when (tool-use-block? b)
                  [s (in-value (format "~a" (tool-use-block-input b)))] #:when (bad? s))
        (format "~a ~a" (tool-use-block-name b) (if (> (string-length s) 160) (string-append (substring s 0 160) "…") s)))
      '()))
;; 任务来源的状态（运行前后比，agent 在 yolo 下能写任何地方）：文件来源 → 各文件 sha1；git 来源 → HEAD + 工作树状态
(define (source-state t)
  (define src (task-source t))
  (case (car src)
    [(files) (for/list ([f (cdr src)]) (cons (cadr f) (and (file-exists? (cadr f)) (call-with-input-file (cadr f) sha1))))]
    [(git) (let-values ([(_c out) (sh "git rev-parse HEAD && git status --porcelain" (cadr src))]) out)]
    [else #f]))

;; 运行前后来源不同 → 一条违规；文件来源把变了的文件存进 <run>/source-changed/ 并在 stderr 报警（不自动改回：可能是人在改）
(define (source-changed! t before run-dir)
  (define after (source-state t))
  (cond
    [(equal? before after) '()]
    [else
     (when (eq? (car (task-source t)) 'files)
       (define keep (build-path run-dir "source-changed"))
       (make-directory* keep)
       (for ([b before] [a after] #:unless (equal? b a))
         (when (file-exists? (car a))
           (let-values ([(_d n _x) (split-path (car a))]) (copy-file (car a) (build-path keep n) #t)))
         (eprintf "WARNING: ~a: the task source ~a changed during the run (copy in ~a)\n" (task-id t) (car a) keep)))
     (when (eq? (car (task-source t)) 'git)
       (eprintf "WARNING: ~a: the task source repo ~a changed during the run (HEAD or working tree)\n" (task-id t) (cadr (task-source t))))
     (list "task source changed during the run")]))

;; ---------------------------------------------------------------- 指标（会话文件）
;; 两种写法都认：GUI 内核（RPC）每次请求一条 req、每次工具一条 tool；命令行（-p / --goal）只有 msg 与 usage——从消息里数
(define CLAIM-RX #px"(?i:\\b(done|complete[d]?|implemented|all tests (now )?pass(ed)?|finished)\\b|已完成|完成了|全部通过)")
(define (session-metrics f)
  (define reqs 0) (define in 0) (define out 0) (define cost 0.0) (define tools 0) (define tool-errors 0) (define compacts 0) (define truncs 0) (define last-text "")
  (define asst 0) (define calls-seen (make-hash)) (define repeated 0) (define msg-tools 0) (define msg-errors 0) (define usage-rec #f) (define model #f)
  (when (and f (file-exists? f))
    (for ([r (in-datum-log f)] #:when (rec? r))
      (define d (rec-data r))
      (case (rec-type r)
        [(meta) (when (and (list? d) (>= (length d) 3) (config? (third d))) (set! model (config-model (third d))))]
        [(req) (set! reqs (add1 reqs))
               (when (hash? d) (set! in (+ in (let ([v (hash-ref d 'input 0)]) (if (real? v) v 0)))) (set! out (+ out (let ([v (hash-ref d 'output 0)]) (if (real? v) v 0))))
                 (set! cost (+ cost (let ([v (hash-ref d 'cost 0)]) (if (real? v) v 0)))))]
        [(tool) (set! tools (add1 tools)) (when (and (hash? d) (eq? (hash-ref d 'error #f) #t)) (set! tool-errors (add1 tool-errors)))]
        [(usage) (when (usage? d) (set! usage-rec d))]
        [(compact) (set! compacts (add1 compacts))]
        [(note) (when (and (hash? d) (equal? (hash-ref d 'code #f) "output_truncated")) (set! truncs (add1 truncs)))]
        [(msg) (when (message? d)
                 (for ([b (message-blocks d)])
                   (cond [(tool-use-block? b)
                          (set! msg-tools (add1 msg-tools))
                          (define k (format "~a ~s" (tool-use-block-name b) (tool-use-block-input b)))
                          (when (hash-ref calls-seen k #f) (set! repeated (add1 repeated)))
                          (hash-set! calls-seen k #t)]
                         [(and (tool-result-block? b) (tool-result-block-is-error? b)) (set! msg-errors (add1 msg-errors))]))
                 (when (eq? (message-role d) 'assistant)
                   (set! asst (add1 asst))
                   (when (non-empty-string? (string-trim (message-text d))) (set! last-text (message-text d)))))]
        [else (void)])))
  (define cli? (zero? reqs))
  (define-values (in* out*) (if (and cli? usage-rec) (values (usage-input-tokens usage-rec) (usage-output-tokens usage-rec)) (values in out)))
  (define cost* (if cli? (or (and model usage-rec (estimate-cost model usage-rec)) 0.0) cost))
  (hasheq 'requests (if cli? asst reqs) 'input_tokens in* 'output_tokens out* 'cost (/ (round (* cost* 10000)) 10000.0)
          'tool_calls (if cli? msg-tools tools) 'tool_errors (if cli? msg-errors tool-errors) 'repeated_calls repeated
          'compactions compacts 'truncations truncs 'model (or model 'null) 'claims_done (and (regexp-match? CLAIM-RX last-text) #t) 'final_text (clip-tail last-text 400)))

;; ---------------------------------------------------------------- 一次运行
(define (run-one t agent rep out-dir #:provider provider #:model model #:mode mode #:keep? keep? #:extra [extra '()])
  (define run-id (format "~a-~a-~a" (task-id t) agent rep))
  (define run-dir (build-path out-dir run-id))
  (make-directory* run-dir)
  ;; 工作区放在系统临时目录、不在 pi2 仓库里：嵌在里面时 agent 从 pwd 推出仓库位置，去读了真仓库里的答案（20261026 flash 实测 3/3）
  (define ws (path->string (build-path (eval-temp-root out-dir) run-id "ws")))
  (when (directory-exists? ws) (delete-directory/files ws))
  (define t0 (current-inexact-milliseconds))
  (with-handlers ([exn:fail? (lambda (e) (hasheq 'task (task-id t) 'agent (symbol->string agent) 'repeat rep 'pass #f 'error (exn-message e)))])
    (define base (materialize! t ws))
    (define protected (protected-at-base t ws))
    (define src-before (source-state t))
    (define-values (agent-exit sess) (run-agent! agent t ws run-dir #:provider provider #:model model #:mode mode #:extra extra))
    (define wall (/ (round (- (current-inexact-milliseconds) t0)) 1000.0))
    (define-values (viol0 tests-added) (violations t ws base protected))
    (define viol (append viol0
                         (map (lambda (x) (string-append "accessed outside the workspace: " x)) (escapes t sess ws #:install (and (eq? agent 'pi2) (current-install))))
                         (source-changed! t src-before run-dir)))
    (write-files! ws (hidden-files t))
    (define-values (code vout) (sh (task-verify t) ws #:timeout (task-timeout t)))
    (call-with-output-file (build-path run-dir "verify.log") #:exists 'truncate (lambda (o) (write-string vout o)))
    (define parsed (parse-output (task-parse t) vout))
    ;; 截断次数：RPC 会话里有 output_truncated 记录；命令行（-p）不装记录去向 → 从 agent.log 的流里数
    (define m (let ([m0 (session-metrics sess)] [n (log-truncations (build-path run-dir "agent.log"))])
                (if (> n (hash-ref m0 'truncations 0)) (hash-set m0 'truncations n) m0)))
    (define verify-ok? (eqv? code 0))
    (define pass? (and verify-ok? (null? viol)))
    (unless keep? (delete-directory/files ws #:must-exist? #f))
    (hasheq 'task (task-id t) 'tier (task-tier t) 'agent (symbol->string agent) 'provider (or provider 'null) 'model (or model 'null) 'mode (symbol->string mode)
            'repeat rep 'pass pass? 'verify_exit (if (eq? code 'timeout) "timeout" code)
            'tests (hasheq 'passed (or (hash-ref parsed 'passed #f) 'null) 'failed (or (hash-ref parsed 'failed #f) 'null) 'total (or (hash-ref parsed 'total #f) 'null))
            'violations viol 'tests_added tests-added 'agent_exit agent-exit 'wall_s wall 'metrics m
            'false_victory (and (not pass?) (hash-ref m 'claims_done #f)) 'run_dir (path->string run-dir) 'ws (if keep? ws 'null))))

;; ---------------------------------------------------------------- 汇总
(define (summary results)
  (define by (group-by (lambda (r) (list (hash-ref r 'task) (hash-ref r 'agent) (hash-ref r 'model 'null))) results))
  (string-append
   "| task | agent | model | pass | pass^k | tests (last) | cost $ (mean) | wall s (mean) | tool calls (mean) | repeated calls (mean) | truncations (sum) | violations | false victory |\n"
   "|---|---|---|---|---|---|---|---|---|---|---|---|---|\n"
   (apply string-append
          (for/list ([g (in-list by)])
            (define n (length g))
            (define ok (count (lambda (r) (eq? (hash-ref r 'pass #f) #t)) g))
            (define (mean f) (if (zero? n) 0 (/ (round (* 100 (/ (for/sum ([r g]) (f r)) n))) 100.0)))
            (define last-r (last g))
            (define tests (hash-ref last-r 'tests (hasheq)))
            (format "| ~a | ~a | ~a | ~a/~a | ~a | ~a/~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a |\n"
                    (hash-ref (car g) 'task) (hash-ref (car g) 'agent) (let ([m (hash-ref (car g) 'model 'null)]) (if (string? m) m "-"))
                    ok n (if (= ok n) "yes" "no")
                    (hash-ref tests 'passed "-") (hash-ref tests 'total "-")
                    (mean (lambda (r) (hash-ref (hash-ref r 'metrics (hasheq)) 'cost 0)))
                    (mean (lambda (r) (hash-ref r 'wall_s 0)))
                    (mean (lambda (r) (hash-ref (hash-ref r 'metrics (hasheq)) 'tool_calls 0)))
                    (mean (lambda (r) (hash-ref (hash-ref r 'metrics (hasheq)) 'repeated_calls 0)))
                    (for/sum ([r g]) (hash-ref (hash-ref r 'metrics (hasheq)) 'truncations 0))
                    (for/sum ([r g]) (length (hash-ref r 'violations '())))
                    (count (lambda (r) (eq? (hash-ref r 'false_victory #f) #t)) g))))))

(provide session-metrics load-task)

;; 线程的返回值（thread 不带返回值：用 box 收）
(define (thread-wait-result th) (thread-wait th) (hash-ref thread-results th))
(define thread-results (make-weak-hasheq))

(module+ main
  (define tasks-dir (build-path PI2-ROOT "eval" "tasks"))
  (define only #f) (define agent 'pi2) (define provider #f) (define model #f) (define mode 'single) (define repeat 1) (define out #f) (define keep? #f)
  (define jobs 1) (define extra '())
  (command-line
   #:once-each
   [("--tasks") d "task directory (default eval/tasks)" (set! tasks-dir (string->path d))]
   [("--only") ids "comma-separated task ids" (set! only (string-split ids ","))]
   [("--agent") a "pi2 | reference | noop | cheat (the last three check the harness itself)" (set! agent (string->symbol a))]
   [("--provider") p "pi2 provider (e.g. deepseek, lmstudio)" (set! provider p)]
   [("-m" "--model") m "model id" (set! model m)]
   [("--mode") md "single (-p) | goal (--goal with the task's visible check)" (set! mode (string->symbol md))]
   [("--repeat") n "runs per task (default 1)" (set! repeat (string->number n))]
   [("--out") d "output directory (default eval/results/<timestamp>)" (set! out d)]
   [("--keep") "keep each run's workspace" (set! keep? #t)]
   [("--jobs") n "runs in parallel (default 1)" (set! jobs (max 1 (string->number n)))]
   #:multi
   [("--pi2-arg") a "extra argument for the pi2 command line (repeatable), e.g. --pi2-arg --auto --pi2-arg off" (set! extra (append extra (list a)))])
  (define ts (let ([d (seconds->date (current-seconds))]) (format "~a~a~a-~a~a~a" (date-year d) (~r (date-month d) #:min-width 2 #:pad-string "0") (~r (date-day d) #:min-width 2 #:pad-string "0")
                                                                (~r (date-hour d) #:min-width 2 #:pad-string "0") (~r (date-minute d) #:min-width 2 #:pad-string "0") (~r (date-second d) #:min-width 2 #:pad-string "0"))))
  ;; 绝对路径：PI_DATA_HOME 等由它推出，pi2 在工作区里按相对路径解析会把会话写进工作区（r2 实测）
  (define out-dir (path->string (path->complete-path (or out (build-path PI2-ROOT "eval" "results" (format "~a-~a" ts agent))))))
  (make-directory* out-dir)
  (define tasks (for/list ([f (sort (directory-list tasks-dir #:build? #t) string<? #:key path->string)]
                           #:when (regexp-match? #rx"[.]rktd$" (path->string f))
                           [t (in-value (load-task f))]
                           #:when (or (not only) (member (task-id t) only)))
                  t))
  (when (eq? agent 'pi2)
    (printf "preparing the pi2 copy …\n") (flush-output)
    (current-install (prepare-install! out-dir tasks)))       ; 之后建的线程沿用这个值
  ;; 并行：每次运行一个线程（agent 是子进程，线程只是等它）；最多 jobs 个同时跑；结果按 任务 × 次序 排回去
  (define slots (make-semaphore jobs))
  (define out-lock (make-semaphore 1))
  (define plan (for*/list ([t tasks] [rep (in-range 1 (add1 repeat))]) (cons t rep)))
  (define threads
    (for/list ([pr plan])
      (thread
       (lambda ()
         (call-with-semaphore slots
           (lambda ()
             (define t (car pr)) (define rep (cdr pr))
             (call-with-semaphore out-lock (lambda () (printf "== ~a (~a) run ~a/~a …\n" (task-id t) agent rep repeat) (flush-output)))
             (define r (run-one t agent rep out-dir #:provider provider #:model model #:mode mode #:keep? keep? #:extra extra))
             (call-with-semaphore out-lock
               (lambda ()
                 (with-output-to-file (build-path out-dir "results.jsonl") #:exists 'append (lambda () (write-json r) (newline)))
                 (printf "   ~a run ~a: pass ~a · tests ~a · violations ~a · truncations ~a · ~a s~a\n" (task-id t) rep (hash-ref r 'pass #f) (hash-ref r 'tests 'null) (length (hash-ref r 'violations '()))
                         (hash-ref (hash-ref r 'metrics (hasheq)) 'truncations 0)
                         (hash-ref r 'wall_s "?") (if (hash-ref r 'error #f) (format " · ERROR ~a" (hash-ref r 'error)) ""))
                 (flush-output)))
             (hash-set! thread-results (current-thread) r)
             r))))))
  (define results (map thread-wait-result threads))
  (define sm (summary results))
  (call-with-output-file (build-path out-dir "summary.md") #:exists 'truncate (lambda (o) (write-string sm o)))
  (display sm)
  (printf "results: ~a\n" out-dir))
