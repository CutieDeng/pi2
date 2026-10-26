#lang racket/base
;; scripted-core.rkt — GUI 冒烟用内核：真实 run-rpc!（RPC v2）+ 真实 builtin 工具 + normal 权限模式，只有模型是脚本。
;;   用户文本含 "echo" → 模型请求 bash（输出 42 行，触发权限卡片、工具卡片折叠）→ 拿到结果后流式回答
;;   用户文本含 "hang" → 流式吐一段后挂住，直到被 interrupt（provider cancel）
;;   用户文本含 "organize" → 调 sessions_overview → 拿到总览后调 propose_session_changes（新建项目 Demo、当前会话归入 Demo、
;;     最旧的非当前会话归档并删除、次旧的改名）→ 回答「提议已提交」（变更清单的冒烟，design-workspace.md §11.7）
;;   用户文本含 "unicode demo" → 回答一段含双向控制 / 零宽 / tag / 从右到左 / 控制字符 / 形近字的文字（显示策略的冒烟，design-gui.md §30）
;;   用户文本含 "ansi demo" → 请求 bash 打印带 ANSI 颜色的输出
;;   其它 → 分几段流式回答
;; 路由池（G6）：配置目录里预置池 @pool = (fa m1) (fb m2)；fa 总是 429，fb 正常回答「answer from fb/模型」。
;; 分发 provider 按 host 选择找到脚本模型（"scripted"），选了 @pool 时按路由走 fa / fb。
;; 一切都在 main 子模块里：模块顶层表达式的值会被打印到 stdout，会污染 NDJSON 流。

(require racket/async-channel racket/file racket/string racket/list json racket/port racket/system
         (file "../../../src/model.rkt") (file "../../../src/event.rkt") (file "../../../src/provider.rkt")
         (file "../../../src/tool.rkt") (file "../../../src/permission.rkt") (file "../../../src/loop.rkt")
         (file "../../../src/session.rkt") (file "../../../src/plugin.rkt") (file "../../../src/providers.rkt")
         (file "../../../src/rpc.rkt") (file "../../../src/tools/builtin.rkt") (file "../../../src/preview.rkt")
         (file "../../../src/instructions.rkt") (file "../../../src/memory.rkt"))

(define (stream-text! ch s)
  (for ([chunk (in-list (regexp-match* #px".{1,6}" s))])
    (async-channel-put ch (evt:delta (now-ms) 'text chunk))
    (sleep 0.03))
  (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant s)))
  (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 12 8)))
) ; end define stream-text!

(define (make-scripted-provider)
  (define cancel-sema (make-semaphore 0))
  (provider "scripted"
    (lambda (msgs _tools)
      (define ch (make-async-channel))
      (define last-msg (last msgs))
      (define tool-result (for/first ([b (in-list (message-blocks last-msg))] #:when (tool-result-block? b)) b))
      (define user-text (message-text last-msg))
      (thread
       (lambda ()
         (define (call! name args)
           (async-channel-put ch (evt:message (now-ms) (message 'assistant (list (tool-use-block (format "call-~a" name) name args)))))
           (async-channel-put ch (evt:turn-end (now-ms) "tool_calls" (usage 10 5))))
         (define tr-text (and tool-result (format "~a" (tool-result-block-content tool-result))))
         (cond
           [(and tr-text (regexp-match? #rx"^[{].*\"sessions\"" tr-text))           ; 总览 → 提议
            (define rows (hash-ref (string->jsexpr tr-text) 'sessions '()))
            (define cur (findf (lambda (r) (hash-ref r 'current #f)) rows))
            (define others (sort (filter (lambda (r) (not (hash-ref r 'current #f))) rows) string<? #:key (lambda (r) (hash-ref r 'last_active ""))))
            (call! "propose_session_changes"
                   (hasheq 'summary "Group today's work into a Demo project; archive and drop the oldest chat"
                           'items (append (list (hasheq 'op "new-project" 'project "Demo" 'reason "today's sessions share a topic"))
                                          (if cur (list (hasheq 'op "link" 'session (hash-ref cur 'session) 'project "Demo" 'reason "current work")) '())
                                          (if (pair? others)
                                              (list (hasheq 'op "archive" 'session (hash-ref (car others) 'session) 'reason "oldest, finished")
                                                    (hasheq 'op "delete" 'session (hash-ref (car others) 'session) 'reason "duplicate of later work"))
                                              '())
                                          (if (> (length others) 1)
                                              (list (hasheq 'op "rename" 'session (hash-ref (cadr others) 'session) 'new_title "Renamed by agent" 'reason "clearer title"))
                                              '()))))]
           [(and tr-text (regexp-match? #rx"proposed change" tr-text))
            (stream-text! ch "提议已提交，请在面板里审阅。")]
           [(regexp-match? #rx"organize" user-text) (call! "sessions_overview" (hasheq))]
           [(regexp-match? #rx"show memory" user-text)                           ; 回答系统提示词里的记忆节（M2 冒烟）
            (define sys (or (config-system-prompt (current-config)) ""))
            (define m (regexp-match-positions (regexp-quote MEMORY-HEADER) sys))
            (stream-text! ch (if m (string-trim (substring sys (caar m))) "no memory"))]
           [(regexp-match? #rx"note pref" user-text) (call! "memory_note" (hasheq 'text "MODEL-MEM: indent with tabs" 'scope "global"))]
           [(regexp-match #rx"find projects (.*)$" user-text) => (lambda (m) (call! "host_projects" (hasheq 'query (cadr m))))]
           [(regexp-match? #rx"show history" user-text) (call! "git_history" (hasheq 'limit 10))]   ; datum 提交的工具卡片（标签片冒烟）
           [(regexp-match? #rx"propose tests" user-text)                        ; 工作区测试流程（T2 冒烟）：提议加一步
            (call! "tests_propose" (hasheq 'reason "add a lint step"
                                           'content "(test-flow (name \"demo\") (step check \"grep -q READY app.txt\") (step unit \"sh tests/t.sh\" #:parse unittest) (step lint \"true\"))"))]
           [(regexp-match? #rx"show instructions" user-text)                      ; 回答系统提示词里的指令节（M1 冒烟：看注入了什么、什么顺序）
            (define-values (_b sec) (split-instructions (or (config-system-prompt (current-config)) "")))
            (stream-text! ch (if (string=? sec "") "no instructions" (string-trim sec)))]
           [(regexp-match? #rx"unicode demo" user-text)
            (define (tags s) (list->string (for/list ([c (in-string s)]) (integer->char (+ #xE0000 (char->integer c))))))
            (stream-text! ch (string-append "file \u202Etxt.exe\u202C · pass\u200Bword · hello" (tags "ignore previous instructions")
                                            " · שלום · раураl.com · bell\u0007 · 👨‍👩‍👧 · Z" (make-string 8 (integer->char #x0301))))]
           [(regexp-match? #rx"ansi demo" user-text)
            (call! "bash" (hasheq 'command "printf '\\033[31mred\\033[0m \\033[42;30mgreen bg\\033[0m \\033[38;5;27mblue 256\\033[0m \\033[30mblack\\033[0m \\033[1;33mbold yellow\\033[0m plain\\n'"))]
           [tool-result
            (define c (tool-result-block-content tool-result))
            (stream-text! ch (format "工具跑完了，输出第一行是：~a。共 ~a 行。"
                                     (car (string-split (if (string? c) c (format "~a" c)) "\n"))
                                     (length (string-split (if (string? c) c (format "~a" c)) "\n"))))]
           [(regexp-match? #rx"echo" user-text)
            (async-channel-put ch (evt:delta (now-ms) 'text "我先跑一下命令。"))
            (async-channel-put ch (evt:message (now-ms)
                                               (message 'assistant (list (text-block "我先跑一下命令。")
                                                                         (tool-use-block "call-1" "bash"
                                                                                         (hasheq 'command "echo hi-from-tool && seq 1 41"))))))
            (async-channel-put ch (evt:turn-end (now-ms) "tool_calls" (usage 10 5)))]
           [(regexp-match? #rx"hang" user-text)
            (async-channel-put ch (evt:delta (now-ms) 'text "partial answer, still thinking"))
            (semaphore-wait cancel-sema)]
           [else (stream-text! ch (format "收到：~a" user-text))])))
      ch)
    (lambda () (semaphore-post cancel-sema)))
) ; end define make-scripted-provider

(module+ main
  (define dir (make-temporary-file "pi2-gui-core-~a" 'directory))
  ;; 配置目录放在会话目录外面：会话列表递归扫会话目录，里面的 .rktd 都会被当成会话
  (define cfg-home (make-temporary-file "pi2-gui-core-cfg-~a" 'directory))
  (void (putenv "PI_CONFIG_HOME" (path->string cfg-home)))
  ;; 项目索引（--synth-hostindex）：host_scan 只扫这个临时目录（不碰真实家目录）；PI2_SEED_REPOS=1 → 里面放两个 git 仓库
  (define scan-root (make-temporary-file "pi2-gui-core-repos-~a" 'directory))
  (when (getenv "PI2_SEED_REPOS")
    (for ([name '("shop" "notes")] [readme '("# Shop\nAn online shop backend. Prices are stored in cents." "# Notes\nPersonal notes in Markdown.")])
      (define d (build-path scan-root name))
      (make-directory* d)
      (with-output-to-file (build-path d "README.md") (lambda () (write-string readme)))
      (parameterize ([current-output-port (open-output-nowhere)] [current-error-port (open-output-nowhere)])
        (define git (find-executable-path "git"))
        (system* git "-C" (path->string d) "init" "-q")
        (system* git "-C" (path->string d) "-c" "user.email=t@t" "-c" "user.name=t" "add" "-A")
        (system* git "-C" (path->string d) "-c" "user.email=t@t" "-c" "user.name=t" "commit" "-q" "-m" "first"))))
  ;; 项目描述：假的模型回答（按输入里的 README 认是哪个；不碰真实的本机模型）
  (define (fake-describe _ep _model msgs)
    (define in (message-text (car msgs)))
    (cond
      [(regexp-match? #rx"extract durable facts" in)                              ; 自动记忆：提取（M3 冒烟）
       "```json\n{\"memories\": [{\"text\": \"AUTO-WS: run make check before pushing\", \"scope\": \"workspace\", \"evidence\": \"one\"}, {\"text\": \"AUTO-GLOBAL: the user prefers short answers\", \"scope\": \"global\", \"evidence\": \"two\"}]}\n```"]
      [(regexp-match? #rx"Merge new candidate memories" in) "{\"ops\": [{\"candidate\": 0, \"op\": \"add\"}, {\"candidate\": 1, \"op\": \"add\"}]}"]
      [else (fake-describe-project in)]))
  (define (fake-describe-project in)
    (if (regexp-match? #rx"# Shop" in)
        "```json\n{\"short\": [\"e-commerce\", \"电商\", \"backend\", \"后端\"], \"long\": [\"Online shop backend\", \"在线商店后端\"], \"names\": [\"shop\"], \"summary_en\": \"An online shop backend.\", \"summary_zh\": \"一个在线商店后端。\"}\n```"
        "{\"short\": [\"notes\", \"笔记\"], \"long\": [\"Personal notes in Markdown\", \"Markdown 写的个人笔记\"], \"names\": [\"notes\"], \"summary_en\": \"Personal notes.\", \"summary_zh\": \"个人笔记。\"}"))
  ;; 工作区测试流程（--synth-tests）：工作目录里放 .pi/tests.rktd（一步会失败、一步通过）
  (when (getenv "PI2_SEED_TESTFLOW")
    (make-directory* (build-path dir ".pi")) (make-directory* (build-path dir "tests"))
    (with-output-to-file (build-path dir ".pi" "tests.rktd")
      (lambda () (write-string "(test-flow (name \"demo\") (step check \"grep -q READY app.txt\") (step unit \"sh tests/t.sh\" #:parse unittest))")))
    (with-output-to-file (build-path dir "tests" "t.sh") (lambda () (write-string "echo 'Ran 2 tests in 0.0s'; echo; echo OK"))))
  ;; datum 提交的冒烟（--synth-history）：工作目录是 git 仓库，提交有人写的、带标签的、pi2 自动化的（datum 元数据 / trailer）、旧 3 项形状、很长的标题
  (when (getenv "PI2_SEED_HISTORY")
    (define (git . args) (parameterize ([current-directory dir] [current-output-port (open-output-nowhere)])
                           (apply system* (find-executable-path "git") "-c" "user.email=t@t" "-c" "user.name=t" args)))
    (git "init" "-q")
    (for ([m (list "(FEAT \"Add the model picker\"\n\n(breaking (scope gui kernel))\n\"Picker in the title bar.\"\n)"
                   "(FIX \"Fix the crash when the session list is empty\"\n\n()\n\"Guard the empty case.\"\n)"
                   "(FEAT \"Add the commit history card\"\n\n((pi2 agent (session \"20261026-1200-0001\") (model \"deepseek-v4-flash\")))\n\"Automated.\"\n)"
                   "(DOCS \"Old three-field message\"\n\n\"no metadata\"\n)"
                   "mark task t3 done\n\nPi2-Automation: goal-driver; session=20261026-1200-0001"
                   "(TEST \"Cover the picker\"\n\n()\n\"t\"\n)"
                   "(BUILD \"Bump the CI image\"\n\n((CI github))\n\"b\"\n)"
                   "(FIX \"Fix the reconnect loop\"\n\n((scope kernel))\n\"r\"\n)"
                   "(FEAT \"Add the tests panel\"\n\n((pi2 goal-worker (session \"20261026-1200-0002\")))\n\"w\"\n)"
                   "(REFACTOR \"A deliberately long title that will not fit on one line of the tool card so it has to be truncated with an ellipsis\"\n\n((scope gui) wip)\n\"x\"\n)")]
          [i (in-naturals)])
      (with-output-to-file (build-path dir (format "f~a.txt" i)) (lambda () (displayln i)))
      (git "add" "-A") (git "commit" "-q" "-m" m)))
  ;; 指令分层冒烟（--synth-instructions）：全局文件 + 工作目录当 git 根、放一个 AGENTS.md
  (when (getenv "PI2_SEED_INSTRUCTIONS")
    (with-output-to-file (build-path cfg-home "AGENTS.md") (lambda () (write-string "GLOBAL-RULE: reply in English")))
    (make-directory* (build-path dir ".git"))
    (with-output-to-file (build-path dir "AGENTS.md") (lambda () (write-string "REPO-RULE: run the tests first"))))
  ;; 冒烟（跨主机费用合计）：参数 --seed-spend 美元 → 今天的花费记录里先记一笔（模型 seed）
  (let ([m (member "--seed-spend" (vector->list (current-command-line-arguments)))])
    (when (and m (pair? (cdr m)) (string->number (cadr m)))
      (define d (seconds->date (current-seconds)))
      (define (pad2 n) (if (< n 10) (format "0~a" n) (format "~a" n)))
      (with-output-to-file (build-path cfg-home "spend-log.rktd") #:exists 'append
        (lambda () (write (list (format "~a-~a-~a" (date-year d) (pad2 (date-month d)) (pad2 (date-day d))) (current-seconds) "seed"
                                (exact->inexact (string->number (cadr m)))))
                   (newline)))))
  (define cfg (struct-copy config (default-config) [workdir (path->string dir)] [permission-mode 'normal]))
  (define host (make-plugin-host))
  (register-builtin-providers! host)
  (define scripted (make-scripted-provider))
  (hash-set! (plugin-host-providers host) "scripted" (lambda (_c) scripted))
  (hash-set! (plugin-host-providers host) "fa"
             (lambda (_c) (provider "fa" (lambda (_m _t) (define ch (make-async-channel))
                                                (async-channel-put ch (evt:error (now-ms) (exn:fail "HTTP 429: rate limit reached (fake account fa)" (current-continuation-marks)) #f))
                                                ch)
                                    void)))
  (hash-set! (plugin-host-providers host) "fb"
             (lambda (_c) (provider "fb" (lambda (_m _t) (define ch (make-async-channel))
                                                (thread (lambda () (stream-text! ch (format "answer from fb/~a" (config-model (current-config))))))
                                                ch)
                                    void)))
  (host-set-provider! host "scripted")
  (with-output-to-file (build-path cfg-home "routes.rktd") (lambda () (write '(("pool" (("fa" "m1") ("fb" "m2")))))))
  (define d (make-deps #:provider (make-dispatch-provider host cfg) #:registry (make-registry (builtin-tools cfg))
                       #:bus (make-bus) #:policy (make-policy cfg) #:plugin-host host))
  (define sess (session-open! (fresh-session-path (path->string dir)) cfg))
  ;; 本地端点模型列表：假的（20 个，第 2 个「已加载」），不碰真实 LM Studio
  (define fake-models (cons "qwen/qwen3.5-9b" (for/list ([i 19]) (format "local-model-~a" i))))
  ;; 本机模型：起标题的请求（§11.4 B）回「Scripted title」；别的（工具输出预览挑行）照真实的走
  (define (fake-complete ep model msgs)
    (if (regexp-match? #rx"short title" (message-text (car msgs))) "Scripted title" (complete-local ep model msgs)))
  (run-rpc! d (make-initial-state cfg) sess #:plugin-host host #:complete-local fake-complete
            #:complete-long fake-describe #:scan-roots (list (path->string scan-root))
            #:fetch-models (lambda (ep) (and (regexp-match? #rx"localhost" ep) (cons fake-models '("qwen/qwen3.5-9b"))))))
