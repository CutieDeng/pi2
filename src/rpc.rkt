#lang tstring racket
;; rpc.rkt — 无头 JSONL 模式（--rpc）：供 IDE / 编排器以进程管道驱动 pi++。
;;
;; 设计原则（慎重、不干扰内核）：
;;   * 完全复用内核——bus 事件 + run-turn! + session；不改 loop/model/provider 一行。
;;   * 本模块只是又一个「事件汇 + 请求驱动器」，与交互式 repl、一次性 -p 并列。
;;   * 单线程、请求→流式→响应：一条 prompt 跑完整轮再读下一条请求。无中途取消（v1）。
;;
;; 协议（NDJSON，各方向每行一个 JSON 对象）：
;;   → 请求 (stdin)：
;;       {"type":"prompt","text":"..."}          跑一个用户轮
;;       {"type":"set_model","model":"..."}       切模型
;;       {"type":"set_provider","name":"..."}     切供应商（含实例名 base[label]；重写 endpoint/key/model）
;;       {"type":"set_reasoning","level":"off|low|medium|high|max"}  设推理强度
;;       {"type":"set_auto","on":true|false}       开/关 Auto 模式（DeepSeek 按任务切 flash/pro）
;;       {"type":"set_escalate","on":true|false}    开/关失败驱动模型升级（DeepSeek: flash→pro→max）
;;       {"type":"add_key","base":"deepseek","label":"work","token":"sk-..."}  存实例 token
;;       {"type":"set_fallback","chain":["anthropic","deepseek-v4-flash"]}     设 on-error 回退链（[] 清空）
;;       {"type":"goal","goal":"...","until":["python3 -m unittest"],"max_turns":20,"budget":0.5}  headless Goal 模式
;;         → 流式 goal_start / (turn 事件) / goal_status / goal_end
;;       {"type":"state"}                          查询模型/供应商/轮次/用量
;;       {"type":"list_providers"}                 → providers：可切换的供应商（含实例）、各自候选模型、是否有密钥、推理档（轮进行中也立即应答）
;;       {"type":"history"}                        导出当前历史（role/text）
;;       {"type":"permission","id":n?,"decision":"yes|no|always|no-reason","reason":"..."}
;;                                                  应答 permission_request（仅轮内；带 id 时须匹配）
;;       {"type":"interrupt"}                      中断正在跑的轮 / goal / compact（立即生效）
;;       {"type":"compact"} / {"type":"clear"}     压缩历史（调模型）/ 清空历史（保留会话文件）
;;       {"type":"list_sessions"}                  列出会话目录（轮进行中也立即应答）；非聊天会话的行带 git_remote（工作目录的 origin URL 或 null）
;;       {"type":"new_session"} / {"type":"resume","path":p} / {"type":"fork","at":n?}
;;       {"type":"delete_session","path":p}        会话管理（不能删当前会话）：进回收站（<会话目录>/trash/，7 天后清掉）
;;       {"type":"restore_session","path":p}       从回收站挪回原处（撤销删除）
;;       {"type":"auto_title","path":p,"endpoint":url?,"model":m?}  本机模型给没有标题的会话起个短标题 → {"type":"auto_title","path","title"|"error"|"skipped"}
;;       {"type":"set_model_input","hidden_text":"mark"|"raw"}  隐藏文字（Unicode tag）送给模型时标出（缺省）/ 原样；会话记录总是原文
;;       {"type":"set_catalog_view","projects":[名…],"sessions":[{path,projects,pinned,archived}…]}  界面的目录视图（agent 整理会话用；不回复）
;;       （事件）{"type":"changeset_proposed","summary":s,"items":[…],"session":p}  agent 调 propose_session_changes：界面打开审阅
;;       {"type":"ping"}                           → pong（轮进行中也立即应答）
;;       {"type":"shutdown"}                       优雅退出
;;   ← 事件/响应 (stdout)：
;;       ready(protocol 2, session, workdir) / turn_start / delta / tool_start / tool_end(diff?) / message / turn_end
;;       / error / permission_request(id) / turn_complete(outcome, interrupted) / ok / state / history(blocks)
;;       / sessions / session(path, history) / providers / pong / bye
;;
;; 权限：yolo 模式 run-turn! 不询问；normal/strict 遇 ask 时发 permission_request(id)，
;;   轮线程等 permission 应答；等待期间其它请求照常入队，interrupt 可打断等待。
;; EOF（客户端退出）：不打断正在跑的轮，跑完落盘后退出；之后的输出静默丢弃（R9）。

(require
 json
 racket/string
 racket/pvector
 (file "model.rkt")
 (file "event.rkt")
 (file "tool.rkt")
 (file "provider.rkt")
 (file "loop.rkt")
 (file "session.rkt")
 (file "plugin.rkt")
 (file "providers.rkt")
 (file "credentials.rkt")               ; 实例 token 写入
 (file "pricing.rkt")                   ; 记费：估算 token 开销
 (file "auto.rkt")                      ; Auto 模式
 (file "escalate.rkt")                  ; 自适应：失败驱动模型升级梯
 (file "retry.rkt")                     ; 增强式回退：回退链读写
 (file "goal.rkt")                      ; Goal 模式：headless 驱动
 (file "context.rkt")                   ; compact!（compact 请求）
 (file "permission.rkt")                ; current-permission-request（结构化权限询问）
 (file "route.rkt")                     ; 路由池（多帐号 / 跨供应商；design-gui.md §15）
 (file "wire.rkt")                      ; 调用格式（调试用：state 里报当前格式）
 (file "adapt.rkt")                     ; 增强适配规则（列出 / 开关 / 设置页建的增删）
 (file "instructions.rkt")              ; 人写的指令分层（全局 / catalog / 目录树；design-memory.md M1）
 (file "memory.rkt")                    ; 记忆：会话开始时注入的一节 + memory_search / memory_note（design-memory.md M2）
 (file "host-index.rkt")                ; 这台主机上的项目索引：找仓库、收描述输入、本地模型写描述、host_projects 工具（design-memory.md §8）
 (file "test-flow.rkt")                 ; 工作区测试流程：界面看 / 跑 / 信任 / 批准模型的修改提议（design-goalmode.md §12 T2）
 (only-in (file "dsv4b.rkt") dsv4b-anchoring-live?)
 (only-in file/sha1 sha1)
 (file "aliases.rkt")                   ; 模型别名（设置页列出 / 增删）
 (file "budget.rkt")                    ; 费用软上限（设置页 / 状态行；停止 = 按中断收尾）
 (file "items.rkt")                     ; 会话记录 → 界面条目（RPC v3：实时 item 事件与 items 翻页同一投影）
 (file "session-index.rkt")
 (file "preview.rkt")                   ; 长工具输出：本地小模型挑预览行
 (file "media.rkt")                     ; 消息里的媒体：blob 存储、垃圾回收
 (file "session-tools.rkt")             ; 会话的信息与导出（界面右键菜单）
 (file "hidden-text.rkt")               ; 送给模型的隐藏文字：标出 / 原样（set_model_input）
 (file "organize-tools.rkt")            ; agent 整理会话：sessions_overview / propose_session_changes（design-workspace.md §11.7）
 (file "chat-space.rkt")                ; 聊天会话（匿名工作区）的目录与占用管理
 (only-in racket/file delete-directory/files copy-directory/files make-directory*)
 net/base64
 (file "secrets.rkt")                   ; 聊天里的密钥：登记原值（模型只见句柄）
 (file "effects.rkt")                   ; 效果账本（幂等）：每个会话一个，记在会话文件里
 racket/async-channel
) ; end require

;; ---------------------------------------------------------------- 事件 → JSON

(define (usage->jsexpr u)
  (hasheq 'input (usage-input-tokens u) 'output (usage-output-tokens u))
) ; end define usage->jsexpr

;; 估算美元开销 → jsexpr（未知模型 → 'null，客户端据此显示 n/a）。
(define (cost->jsexpr model u)
  (define c (estimate-cost model u))
  (if c c 'null)
) ; end define cost->jsexpr

(define (tool-use->jsexpr b)
  (hasheq 'id (tool-use-block-id b) 'name (tool-use-block-name b)
          'input (tool-use-block-input b))
) ; end define tool-use->jsexpr

;; bus 事件 → jsexpr（#f = 跳过）。仅 assistant message 外发（user 工具回填由 tool_end 覆盖）。
(define (event->jsexpr e)
  (cond
    [(evt:delta? e)
     (hasheq 'type "delta" 'kind (symbol->string (evt:delta-kind e)) 'text (evt:delta-text e))]
    [(evt:tool-start? e)
     (define b (evt:tool-start-block e))
     (hasheq 'type "tool_start" 'id (tool-use-block-id b) 'name (tool-use-block-name b)
             'input (tool-use-block-input b))]
    [(evt:tool-end? e)
     (define b (evt:tool-end-block e))
     (define r (evt:tool-end-result e))
     (define base (hasheq 'type "tool_end" 'id (tool-use-block-id b) 'name (tool-use-block-name b)
                          'is_error (tool-outcome-is-error? r) 'ms (evt:tool-end-ms e)
                          'content (tool-outcome-content r)))
     (let* ([b (if (tool-outcome-diff r) (hash-set base 'diff (tool-outcome-diff r)) base)]
            [b (if (tool-outcome-ui r) (hash-set b 'ui (tool-outcome-ui r)) b)])
       b)]
    [(evt:message? e)
     (define m (evt:message-msg e))
     (if (eq? (message-role m) 'assistant)
         (hasheq 'type "message" 'role "assistant" 'text (message-text m)
                 'tool_uses (map tool-use->jsexpr (message-tool-uses m)))
         #f)]
    [(evt:turn-end? e)
     (hasheq 'type "turn_end" 'stop_reason (or (evt:turn-end-stop-reason e) "stop")
             'usage (usage->jsexpr (evt:turn-end-usage e)))]
    [(evt:error? e)
     (hasheq 'type "error" 'message (exn-message (evt:error-exn e))
             'recoverable (evt:error-recoverable? e))]
    [else #f]
  ) ; end cond
) ; end define event->jsexpr

;; ---------------------------------------------------------------- 小工具

(define (jget h k [d #f]) (if (hash? h) (hash-ref h k d) d))

(define (parse-req line)
  (with-handlers ([exn:fail? (lambda (_e) #f)]) (string->jsexpr line))
) ; end define parse-req

;; 轻量持久化（不依赖 repl/TUI）：落盘本轮新增的 history 消息与用量增量。
;; 消息已在追加进历史时经记录去向（current-record-sink，轮线程里装的）写进会话文件；这里只记用量增量
(define (persist! sess st-before st-after)
  (define ub (agent-state-token-usage st-before))
  (define ua (agent-state-token-usage st-after))
  (define du (usage (- (usage-input-tokens ua) (usage-input-tokens ub))
                    (- (usage-output-tokens ua) (usage-output-tokens ub))))
  (unless (equal? du usage-zero) (session-append-usage! sess du))
) ; end define persist!

;; ---------------------------------------------------------------- 结构化历史（R4）

(define (block->jsexpr b)
  (cond
    [(text-block? b) (hasheq 'type "text" 'text (text-block-text b))]
    [(thinking-block? b) (hasheq 'type "thinking" 'text (thinking-block-text b))]
    [(tool-use-block? b)
     (hasheq 'type "tool_use" 'id (tool-use-block-id b) 'name (tool-use-block-name b) 'input (tool-use-block-input b))]
    [(tool-result-block? b)
     (define c (tool-result-block-content b))
     (hasheq 'type "tool_result" 'tool_use_id (tool-result-block-tool-use-id b)
             'is_error (and (tool-result-block-is-error? b) #t) 'content (if (string? c) c (format "~a" c)))]
    [(image-block? b) (hasheq 'type "image" 'media_type (image-block-media-type b))]
    [(media-block? b) (media->jsexpr b)]
    [else (hasheq 'type "unknown")]
  ) ; end cond
) ; end define block->jsexpr

;; 每条消息：role、text（拼接的文本，兼容 v1）、blocks（完整结构：工具调用与结果都在）
;; diffs：工具调用id → diff（session-diffs），有则挂在对应 tool_result 块上
(define (history->jsexpr st [diffs #f])
  (for/list ([m (in-pvector (agent-state-history st))])
    (hasheq 'role (symbol->string (message-role m)) 'text (message-text m)
            'blocks (for/list ([b (in-list (message-blocks m))])
                      (define j (block->jsexpr b))
                      (define d (and diffs (tool-result-block? b) (hash-ref diffs (tool-result-block-tool-use-id b) #f)))
                      (if d (hash-set j 'diff d) j))))
) ; end define history->jsexpr

(define (path->str p) (if (path? p) (path->string p) p))

(define (session-dir sess)
  (define-values (base _n _d) (split-path (session-path sess)))
  (if (path? base) base (current-directory))
) ; end define session-dir

;; ephemeral?：当前会话是本次新建、还没有消息的（切走就删）→ 'ephemeral #t（界面画成临时的）
(define (session-info->jsexpr i current [ephemeral? #f])
  (define cur? (equal? (path->str (session-info-path i)) (path->str current)))
  (hasheq 'path (path->str (session-info-path i)) 'ts (session-info-ts i) 'model (session-info-model i)
          'title (let ([t (session-info-title i)]) (if (member t '("(empty)" "⑂ (empty)")) 'null t))   ; 无用户消息 → null
          'messages (session-info-nmsg i)
          'current cur?
          'ephemeral (and cur? ephemeral? #t)
          'chat (and (session-info-chat? i) #t)
          'workdir (or (session-info-workdir i) 'null)
          'mtime (session-info-mtime i)                                      ; 最近活动（会话文件的修改时间，秒）：侧栏按时间分段
          'titled (and (session-info-titled? i) #t))                         ; 有过标题记录：界面只给没有的自动起标题
) ; end define session-info->jsexpr

;; 一条增强适配规则 → jsexpr（设置页：名、说明、来源、开关、匹配条件）
(define (rule->jsexpr r)
  (define p (rule-props r))
  (define lv (hash-ref p 'reasoning #f))
  (hasheq 'name (rule-name r) 'description (rule-description r) 'source (symbol->string (rule-source r))
          'enabled (rule-enabled? r)
          'system_append (let ([v (hash-ref p 'system-append #f)]) (if (string? v) v 'null))
          'temperature (let ([v (hash-ref p 'temperature #f)]) (if (real? v) v 'null))
          'match (hasheq 'model (or (hash-ref p 'model #f) 'null)
                         'provider (or (hash-ref p 'provider #f) 'null)
                         'reasoning (cond [(list? lv) (map (lambda (x) (format "~a" x)) lv)]
                                          [lv (list (format "~a" lv))]
                                          [else 'null])))
) ; end define rule->jsexpr

;; ---------------------------------------------------------------- 服务器

;; 轮进行中仍立即应答的只读请求（其余请求排队，轮结束后按序处理）
(define CONCURRENT-TYPES '("test_flow" "test_flow_run" "test_flow_trust" "test_flow_decide" "set_model_input" "set_catalog_view" "host_scan" "describe_input" "describe_project" "local_complete" "memory_input" "list_sessions" "list_providers" "ping" "items" "item" "index_summary" "select_lines" "blob" "session_stats" "export_session" "chat_usage" "get_chat_config"))   ; 轮进行中也立即处理（翻页不等轮结束）

(define REASONING-LEVELS '("off" "low" "medium" "high" "max"))

;; 供应商选择器的数据：可用供应商 ∪ 已配置实例；内置档案给默认模型（DeepSeek 系再加 Auto 的轻 / 重模型）与密钥是否就绪。
;; local-models：端点 → (cons 模型ids 已加载ids)（本地端点查询结果的缓存），有则并入候选并给 loaded
(define (providers->jsexpr host cfg [local-models (hash)])
  (define names (remove-duplicates
                 (append (host-available-providers host)
                         (for/list ([bl (in-list (all-instances))]) (instance-display (car bl) (cdr bl))))))
  (hasheq 'type "providers"
          'current (host-current-provider host)
          'model (config-model cfg)
          'reasoning (symbol->string (effective-reasoning-effort (config-model cfg)))
          'reasoning_levels (filter (lambda (l) (valid-reasoning-effort? (string->symbol l))) REASONING-LEVELS)
          'auto (auto-mode-on?)
          'providers
          (for/list ([n (in-list names)])
            (define-values (base label) (parse-instance n))
            (define prof (profile-by-name base))
            (define fetched (and prof (hash-ref local-models (provider-profile-endpoint prof) #f)))
            (define models
              (if prof
                  (remove-duplicates (append (list (provider-profile-model prof))
                                             (if fetched (car fetched) '())
                                             (if (regexp-match? #rx"deepseek\\.com" (provider-profile-endpoint prof))
                                                 (list (light-model) (pro-model)) '())))
                  '()))
            (hasheq 'name n
                    'needs_key (and prof (provider-profile-key-env prof) #t)   ; 本地端点不需要 key
                    'models models
                    'loaded (if fetched (cdr fetched) '())
                    'has_key (cond [(not prof) #t]
                                   [(not (provider-profile-key-env prof)) #t]
                                   [else (and (resolve-provider-token base label) #t)]))))
) ; end define providers->jsexpr

;; run-rpc! : deps agent-state session -> void（复用内核；返回时会话已关闭）
;;
;; 线程（v2）：
;;   读线程   逐行读 stdin：permission → perm 通道；interrupt → 立即 break 轮线程；其余 → 请求队列；EOF → 'eof 入队。
;;   轮线程   prompt / goal / compact 在独立线程里跑（breaks 打开），被 interrupt 时 provider-cancel! 收尾。
;;   主循环   取请求、派发；有轮在跑时只答只读请求，其余暂存，轮结束后按序处理。
;; fetch-models：本地端点 → (cons 模型ids 已加载ids) | #f（测试注入假实现，不碰真实 LM Studio）
;; ---------------------------------------------------------------- 媒体（prompt 的 'media）

(define BLOB-CHUNK (* 512 1024))                      ; blob 请求每块的字节数上限
(define PROMPT-MEDIA-MAX (* 32 1024 1024))            ; 一条消息里媒体的总字节数上限

(define (media-kind-of v mt)
  (cond [(member v '("image" "audio" "file")) (string->symbol v)]
        [(and (string? mt) (regexp-match? #rx"^image/" mt)) 'image]
        [(and (string? mt) (regexp-match? #rx"^audio/" mt)) 'audio]
        [else 'file]))

;; jsexpr 的 meta → hasheq（只留数字 / 字符串 / 布尔的字段）
(define (clean-meta m)
  (if (hash? m)
      (for/hasheq ([(k v) (in-hash m)] #:when (or (real? v) (string? v) (boolean? v))) (values k v))
      (hasheq)))

;; → media-block 表；不合法的项跳过；总大小超过上限 → raise
(define (prompt-media items root)
  (define total 0)
  (for/list ([it (in-list (if (list? items) items '()))] #:when (hash? it)
             #:when #t
             [b (in-value
                 (let* ([mt (let ([v (hash-ref it 'media_type #f)]) (if (string? v) v "application/octet-stream"))]
                        [name (let ([v (hash-ref it 'name #f)]) (and (string? v) v))]
                        [data (hash-ref it 'data #f)]
                        [sha (cond [(string? data)
                                    (define bs (base64-decode (string->bytes/latin-1 data)))
                                    (set! total (+ total (bytes-length bs)))
                                    (when (> total PROMPT-MEDIA-MAX) (error 'prompt "media too large (over ~a MB in one message)" (quotient PROMPT-MEDIA-MAX 1048576)))
                                    (blob-put! root bs)]
                                   [(and (valid-sha? (hash-ref it 'sha #f)) (blob-size root (hash-ref it 'sha))) (hash-ref it 'sha)]
                                   [else #f])])
                   (and sha (media-block (media-kind-of (hash-ref it 'kind #f) mt) mt sha name
                                         (hash-set (clean-meta (hash-ref it 'meta #f)) 'bytes (or (blob-size root sha) 0))))))]
             #:when b)
    b)
) ; end define prompt-media

;; complete-local：(端点 模型 消息表) → 文字（select_lines 用的本地模型调用；测试注入）
(define (run-rpc! d st0 sess0 #:plugin-host host #:fetch-models [fetch-models fetch-local-models] #:complete-local [complete-local* complete-local]
                  #:complete-long [complete-long* (lambda (ep model msgs) (complete-local ep model msgs #:max-tokens 1200 #:timeout 300))]
                  #:scan-roots [scan-roots #f])                  ; 测试用：host_scan 只扫这些（不碰真实的家目录）
  (define in (current-input-port))
  (define out (current-output-port))
  ;; 写一行 JSON；带锁（bus 消费线程也会写）。管道已断（GUI 退出）后静默丢弃（R9）。
  (define out-lock (make-semaphore 1))
  (define out-open? #t)
  (define (emit! j)
    (call-with-semaphore out-lock
      (lambda ()
        (when out-open?
          (with-handlers ([exn:fail? (lambda (_e) (set! out-open? #f))])
            (write-json j out) (newline out) (flush-output out)
            (when (equal? (hash-ref j 'type #f) "bye") (set! out-open? #f)))))))   ; bye 之后什么都不再写（后台线程——聊天目录占用等——可能还在跑）
  (define (emit-error! msg) (emit! (hasheq 'type "error" 'message msg)))
  ;; ---- 读线程（R1 / R2）
  (define req-ch (make-async-channel))
  (define perm-ch (make-async-channel))
  (define running (box #f))                          ; 正在跑的轮线程或 #f
  (define reader
    (thread
     (lambda ()
       (let loop ()
         (define line (read-line in 'any))
         (cond
           [(eof-object? line)
            (async-channel-put perm-ch 'eof)
            (async-channel-put req-ch 'eof)]
           [(string=? (string-trim line) "") (loop)]
           [else
            (define req (parse-req line))
            (define type (and (hash? req) (jget req 'type)))
            (cond
              [(equal? type "permission") (async-channel-put perm-ch req)]
              [(equal? type "interrupt")
               (define t (unbox running))
               (when t (break-thread t))
               (emit! (hasheq 'type "ok" 'for "interrupt" 'running (and t #t)))]
              [else (async-channel-put req-ch (or req 'bad))])
            (loop)])))))
  ;; ---- 权限询问：发 request（带 id），在轮线程里等 perm 通道（可被 interrupt 打断）
  (define perm-seq 0)
  (define (rpc-asker prompt)
    (set! perm-seq (add1 perm-seq))
    (define id perm-seq)
    ;; 先排空 bus：之前发布的 delta / message / turn_end 由 bus 消费线程写出，与这里的直接写出没有先后保证；
    ;; 不排空时客户端会先看到询问、后看到引出它的那条 assistant 消息。
    (bus-drain! (deps-bus d))
    ;; 结构化字段（tool / level / input）供 GUI 权限卡片使用；prompt 保留（v1 客户端 / 人读）
    (define ctx (current-permission-request))
    (define input (and ctx (hash-ref ctx 'input #f)))
    (define req-j (hasheq 'type "permission_request" 'id id 'prompt prompt
                          'tool (if ctx (format "~a" (hash-ref ctx 'tool)) 'null)
                          'level (if ctx (format "~a" (hash-ref ctx 'level)) 'null)
                          'input (if (and input (jsexpr? input)) input 'null)))
    (emit! req-j)
    (record! 'perm (hash-remove req-j 'type))                         ; 会话记录：询问（轮线程里，记录去向已装）
    (let loop ()
      (define r (async-channel-get perm-ch))
      (cond
        [(eq? r 'eof) (async-channel-put perm-ch 'eof) (record! 'perm-answer (hasheq 'id id 'decision "closed")) 'no]   ; 留给后续询问
        [(let ([rid (jget r 'id)]) (and rid (not (equal? rid id)))) (loop)]   ; 过期应答
        [else
         (record! 'perm-answer (hasheq 'id id 'decision (let ([dd (jget r 'decision "no")]) (if (string? dd) dd "no"))))
         (case (jget r 'decision "no")
           [("yes") 'yes]
           [("always") 'always]
           [("no-reason") (cons 'no (jget r 'reason ""))]
           [else 'no])])))
  ;; 用 RPC asker 覆盖 deps 的交互式 asker（不改内核，仅 struct-copy）。
  (define d* (struct-copy deps d [asker rpc-asker]))      ; set_permission_mode 会换掉其中的 policy
  ;; ---- 效果账本：每个会话一个（标识 = 会话路径：恢复后幂等键不变），从会话文件恢复，新记录追加进会话文件
  (define cur-sess (box sess0))
  ;; 整理会话的两个工具（只看、只提议；改动由界面审阅后做）。目录视图由界面推（set_catalog_view）
  (define catalog-view (box #f))
  (for ([t (in-list (make-organize-tools #:session-dir (lambda () (session-dir (unbox cur-sess)))
                                         #:catalog-view (lambda () (unbox catalog-view))
                                         #:emit! emit!
                                         #:current-session (lambda () (path->str (session-path (unbox cur-sess))))))])
    (registry-add! (deps-registry d) t))
  ;; 工作区测试流程的运行历史：内核数据目录（会话目录下 test-runs/，按项目根一个文件；不进仓库）
  (current-test-history-dir (build-path (session-dir sess0) "test-runs"))
  ;; 记忆的两个工具：查（只读）、记（发事件给界面落盘）
  (for ([t (in-list (make-memory-tools #:view (lambda () (memory-view (unbox cur-sess)))
                                       #:emit! emit!
                                       #:record-used! (lambda (ids q) (when (pair? ids)
                                                                        (session-append-rec! (unbox cur-sess) 'memory_used (hasheq 'ids ids 'query q))
                                                                        (emit! (hasheq 'type "memory_used" 'ids ids 'how "search"))))   ; 界面记「最近用过」（老化用）
                                       #:current-session (lambda () (path->str (session-path (unbox cur-sess))))))])
    (registry-add! (deps-registry d) t))
  ;; 项目索引（design-memory.md §8）：界面推来这台主机的项目与描述（set_catalog_view 的 host_projects）；会话行的 host_index：#t / #f / 没有 = 缺省
  ;; 缺省：本机模型（端点在本机）能用，云端模型不能（用户 2026-10-25：缺省关，会话可单独打开）
  (define cur-endpoint (box (config-endpoint (agent-state-config st0))))
  (registry-add! (deps-registry d)
                 (make-host-index-tool
                  #:view (lambda ()
                           (define v (unbox catalog-view))
                           (define row (session-row (unbox cur-sess)))
                           (define flag (and row (hash-ref row 'host_index 'unset)))   ; 没有这一项 / null = 缺省（不是关）
                           (define local? (let ([ep (unbox cur-endpoint)]) (and (string? ep) (with-handlers ([exn:fail? (lambda (_e) #f)]) (local-endpoint? ep)))))
                           (define allowed? (cond [(eq? flag #t) #t] [(eq? flag #f) #f] [else local?]))
                           (values (and v (list? (hash-ref v 'host_projects #f)) (filter hash? (hash-ref v 'host_projects)))
                                   allowed?
                                   (if (eq? flag #f)
                                       "the project index is turned off for this session"
                                       "the project index is off for cloud models by default (it lists the user's projects); the user can turn it on for this session: right-click the session → Project index")))))
  (define ledgers (make-weak-hasheq))
  (define (ledger-of s)
    (hash-ref! ledgers s
               (lambda () (make-ledger (path->str (session-path s)) (session-effects (session-path s))
                                       #:append! (lambda (e) (session-append-effect! s e))))))
  ;; ---- 路由池：aff = 当前会话的亲和状态（#f = 不走路由，用 host 的当前供应商）
  (define aff (box #f))
  (define (current-provider-name)
    (if (unbox aff) (string-append "@" (route-name (affinity-route (unbox aff)))) (host-current-provider host)))
  (define (route-json) (and (unbox aff) (route->jsexpr (affinity-route (unbox aff)) (unbox aff))))
  ;; 当前目标（池 → 当前成员）匹配到的增强适配规则（含关掉的）
  (define (adaptations-json cfg)
    (define-values (pname model)
      (if (unbox aff)
          (let ([m (affinity-member (unbox aff))]) (values (rmember-instance m) (rmember-model m)))
          (values (host-current-provider host) (config-model cfg))))
    (rules->jsexpr pname model (effective-reasoning-effort model) (config-workdir cfg)))
  (define (emit-route-status!) (let ([j (route-json)]) (when j (emit! (hash-set j 'type "route_status")))))
  ;; 当前成员的 endpoint / key / model 写回会话 config（计价、显示、compact 用）
  (define (with-member-config st)
    (if (unbox aff)
        (struct-copy agent-state st [config (member-config (agent-state-config st) (affinity-member (unbox aff)))])
        st))
  (define (on-route kind data)
    (define a (unbox aff))
    (when a
      (case kind
        [(switch)
         (when (eq? (car data) a)
           (define reason (cadddr data))
           (define sw (hasheq 'type "route_switch" 'route (route-name (affinity-route a))
                              'from (member->jsexpr (cadr data)) 'to (member->jsexpr (caddr data))
                              'reason (cond [(symbol? reason) (symbol->string reason)] [(string? reason) reason] [else 'null])))
           (emit! sw)
           (note! "warn" "route_switch" (format "@~a: ~a/~a → ~a/~a" (route-name (affinity-route a))
                                                (rmember-instance (cadr data)) (rmember-model (cadr data))
                                                (rmember-instance (caddr data)) (rmember-model (caddr data)))
                  (hash-remove sw 'type))
           (emit-route-status!))]
        [(health)
         (when (member (car data) (route-members (affinity-route a))) (emit-route-status!))]
        [else (void)])))
  (on-route-change! on-route)
  ;; 选路由：→ 新的 st（config 换成当前成员的）
  (define (select-route! ref st sess #:record? [record? #t])
    (define r (route-by-name (route-ref->name ref)))
    (cond
      [(not r) #f]
      [else
       (set-box! aff (make-affinity r #:base-endpoint (config-endpoint (agent-state-config st))))
       (pick! (unbox aff))
       (when record? (session-append-route! sess ref))
       (with-member-config st)]))
  (define (restore-route st sess)
    (define ref (session-route (session-path sess)))
    (cond [(and (string? ref) (route-ref-name? ref)) (or (select-route! ref st sess #:record? #f) st)]
          [(equal? ref "") (set-box! aff #f) st]
          [else st]))
  ;; 本地端点（LM Studio）的模型列表：list_providers 先用缓存立即应答，后台线程查询（≤1.5s），
  ;; 结果有变化再发一次 providers。查询不在主循环里做：interrupt 等请求不能被网络等待卡住。
  (define local-models (make-hash))                  ; 端点 → (cons 模型ids 已加载ids)
  (define fetching (box #f))
  ;; providers 应答：加上路由池（成员与健康）与当前选择（池为「@名」）
  (define (providers-payload cfg)
    (hash-set* (providers->jsexpr host cfg (hash-copy local-models))
               'current (current-provider-name)
               'routes (for/list ([r (in-list (load-routes))]) (route->jsexpr r (unbox aff)))
               'adaptations (adaptations-json cfg)))
  (define (emit-providers! st)
    (define cfg (agent-state-config st))
    (emit! (providers-payload cfg))
    (unless (unbox fetching)
      (define eps (remove-duplicates
                   (for*/list ([n (in-list (host-available-providers host))]
                               [prof (in-value (profile-by-name (instance-base n)))]
                               #:when (and prof (local-endpoint? (provider-profile-endpoint prof))))
                     (provider-profile-endpoint prof))))
      (unless (null? eps)
        (set-box! fetching #t)
        (thread (lambda ()
                  (define changed?
                    (for/fold ([ch? #f]) ([ep (in-list eps)])
                      (define r (fetch-models ep))
                      (cond [(and r (not (equal? r (hash-ref local-models ep #f)))) (hash-set! local-models ep r) #t]
                            [else ch?])))
                  (set-box! fetching #f)
                  (when changed? (emit! (providers-payload cfg))))))))
  ;; 会话记录里的提示（note）：写当前会话（路由监听等不在轮线程里的地方也能写）
  (define (note! level code text [data #f])
    (with-handlers ([exn:fail? void])
      (session-append-rec! (unbox cur-sess) 'note (let ([h (hasheq 'level level 'code code 'text text)]) (if data (hash-set h 'data data) h)))))
  ;; ---- 人写的指令（design-memory.md M1）：每轮开始前按会话的工作目录 + 目录 catalog 重新收集，换掉系统提示词的指令节
  ;; （文件改了下一轮就生效；没变时提示词逐字不变，不影响前缀缓存）。注入的清单变了才在会话里记一条 instructions（信息面板读最后一条）。
  ;; dsv4-b 锚定态不动（首请求必须是固定原句；解锁段里带指令）。
  (define (catalog-instructions sess)                             ; 界面推来的：这个会话的 项目 → super-workspace → 工作区 指令
    (define v (unbox catalog-view))
    (define p (path->str (session-path sess)))
    (define row (and v (for/first ([r (in-list (hash-ref v 'sessions '()))] #:when (equal? (jget r 'path) p)) r)))
    (define by-id (if v (for/hash ([e (in-list (hash-ref v 'instructions '()))] #:when (and (hash? e) (string? (jget e 'id)))) (values (jget e 'id) e)) (hash)))
    (for/list ([id (in-list (let ([x (and row (jget row 'instr))]) (if (list? x) x '())))] #:when (hash-ref by-id id #f))
      (define e (hash-ref by-id id))
      (hasheq 'kind (let ([k (jget e 'kind)]) (if (string? k) k "project")) 'name (let ([n (jget e 'name)]) (if (string? n) n "?"))
              'text (let ([t (jget e 'text)]) (if (string? t) t "")))))
  (define instr-sig (make-hash))                                  ; 会话路径 → 最近记下的签名（第一次用到时从会话文件读）
  (define (record-instructions! sess segs add)
    (define p (path->str (session-path sess)))
    (define sig (sha1 (open-input-string add)))
    (define prev (hash-ref! instr-sig p (lambda () (let ([rs (records-of-type (session-path sess) 'instructions)])
                                                     (and (pair? rs) (let ([d (rec-data (car (reverse rs)))]) (and (hash? d) (hash-ref d 'sig #f))))))))
    (unless (equal? prev sig)
      (hash-set! instr-sig p sig)
      (session-append-rec! sess 'instructions (hasheq 'sources (instructions-summary segs) 'sig sig 'max_bytes MAX-BYTES))
      (define cut (filter (lambda (s) (or (hash-ref s 'truncated) (hash-ref s 'omitted))) segs))
      (when (pair? cut)
        (note! "warn" "instructions_truncated"
               (format "instructions exceed the ~a KiB limit; cut: ~a" (quotient MAX-BYTES 1024) (string-join (map (lambda (s) (hash-ref s 'label)) cut) ", "))))))
  ;; ---- 记忆（design-memory.md M2）：界面推来的条目与这个会话的作用域链 / 开关
  (define (session-row sess)
    (define v (unbox catalog-view))
    (define p (path->str (session-path sess)))
    (and v (for/first ([r (in-list (hash-ref v 'sessions '()))] #:when (equal? (jget r 'path) p)) r)))
  (define (memory-view sess)                                      ; → (values 条目 链 读? 写? 全局自动?)；没推过 / 不在视图里 → 条目 #f
    (define v (unbox catalog-view))
    (define row (session-row sess))
    (define chain (let ([c (and row (jget row 'mem))]) (if (list? c) (filter hash? c) #f)))
    (if (and v row chain (list? (hash-ref v 'memories #f)))
        (values (filter hash? (hash-ref v 'memories)) chain (not (eq? (hash-ref row 'mem_read #t) #f)) (not (eq? (hash-ref row 'mem_write #t) #f))   ; 没有这一项 = 开
                (eq? (hash-ref v 'memory_global_auto #f) #t))
        (values #f #f #f #f #f)))
  ;; 会话开始时注入、之后冻结（同一个会话里提示词不再变：记忆改了从下一个会话起生效；恢复时读会话里最后一条 memory 记录）
  (define frozen-memory (make-hash))                              ; 会话路径 → 冻结的那一节（字符串）
  (define (memory-section sess)
    (define p (path->str (session-path sess)))
    (or (hash-ref frozen-memory p #f)
        (let ([rs (records-of-type (session-path sess) 'memory)])
          (and (pair? rs) (let ([d (rec-data (car (reverse rs)))]) (and (hash? d) (string? (hash-ref d 'text #f))
                                                                         (begin (hash-set! frozen-memory p (hash-ref d 'text)) (hash-ref d 'text))))))
        (let-values ([(entries chain read? _w _g) (memory-view sess)])
          (cond
            [(not (and entries read?)) ""]                                ; 界面还没推这个会话 / 读关着：这一轮不注入，不冻结
            [else
             (define-values (text ids) (memory-addendum entries chain))
             (hash-set! frozen-memory p text)
             (session-append-rec! sess 'memory (hasheq 'ids ids 'bytes (bytes-length (string->bytes/utf-8 text)) 'text text))
             (when (pair? ids) (emit! (hasheq 'type "memory_used" 'ids ids 'how "injected")))
             text]))))
  (define (with-instructions st sess)
    (cond
      [(dsv4b-anchoring-live?) st]
      [else
       (define cfg (agent-state-config st))
       (define-values (base _old) (split-sections (config-system-prompt cfg) (list INSTRUCTIONS-HEADER MEMORY-HEADER)))
       (define segs (with-handlers ([exn:fail? (lambda (e) (note! "warn" "instructions_failed" (exn-message e)) '())])
                      (collect-instructions (config-workdir cfg) #:tree? (not (chat-dir-of sess)) #:catalog (catalog-instructions sess))))
       (define add (instructions-addendum segs))
       (record-instructions! sess segs add)
       (define mem (with-handlers ([exn:fail? (lambda (e) (note! "warn" "memory_failed" (exn-message e)) "")]) (memory-section sess)))
       (set-box! cur-endpoint (config-endpoint cfg))                   ; host_projects 判本机 / 云端用
       (define sys (if (and (not base) (string=? add "") (string=? mem "")) #f (string-append (or base "") add mem)))
       (if (equal? sys (config-system-prompt cfg)) st (struct-copy agent-state st [config (struct-copy config cfg [system-prompt sys])]))]))
  ;; 流到一半的文字（被中断时记成 partial：界面里看得到，模型上下文里没有）
  (define partial-text (open-output-string))
  (define partial-reasoning (open-output-string))
  (define (reset-partial!) (set! partial-text (open-output-string)) (set! partial-reasoning (open-output-string)))
  ;; 流式文字的检查点（决定 1：可配置；缺省不写，完成后才落盘）：每隔这么多秒把流到一半的文字记一条 partial（'checkpoint #t，界面不显示）
  (define checkpoint-s #f)
  (define last-checkpoint 0.0)
  (define (maybe-checkpoint!)
    (when (and checkpoint-s (> (- (now-ms) last-checkpoint) (* 1000.0 checkpoint-s)))
      (set! last-checkpoint (now-ms))
      (define pt (get-output-string partial-text))
      (define pr (get-output-string partial-reasoning))
      (unless (and (string=? pt "") (string=? pr ""))
        (with-handlers ([exn:fail? void])
          (session-append-rec! (unbox cur-sess) 'partial (hasheq 'text pt 'reasoning pr 'checkpoint #t))))))
  (define unsub
    (bus-subscribe! (deps-bus d)
      (lambda (e)
        (cond [(evt:delta? e) (case (evt:delta-kind e)
                                [(text) (write-string (evt:delta-text e) partial-text)]
                                [(thinking) (write-string (evt:delta-text e) partial-reasoning)]
                                [else (void)])
                              (maybe-checkpoint!)]
              [(or (evt:message? e) (evt:tool-start? e)) (reset-partial!)])
        (define j (event->jsexpr e))
        (when j (emit! j)))))
  (define (shutdown! sess)
    (bus-drain! (deps-bus d)) (unsub) (off-route-change! on-route) (session-close! sess) (kill-thread reader))
  (set! st0 (restore-route st0 sess0))                ; 会话里记过路由池：恢复
  ;; ---- 界面条目（RPC v3）：会话里每写一条记录就发一个 item 事件；session 事件只带尾部；更早的用 items 按序号区间取
  (define TAIL-RECORDS 120)
  (define (items-of s from to [pb DEFAULT-PREVIEW-BYTES])
    (filter values (for/list ([pr (in-list (session-read-range s from to))]) (record->item (car pr) (cdr pr) #:preview-bytes pb))))
  ;; 翻页应答带回请求的 'tag（守护进程把应答广播给所有客户端：各客户端按标签认领）
  (define (tagged req j) (let ([tg (jget req 'tag)]) (if (string? tg) (hash-set j 'tag tg) j)))
  ;; 轮数 = 会话里用户发的消息数（经索引数，不读记录）
  (define (user-turns s)
    (define ix (session-index s))
    (define msg (type-code 'msg))
    (for/sum ([i (in-range (index-count ix))]) (let ([e (index-ref ix i)]) (if (and (= (ientry-type e) msg) (= (ientry-role e) ROLE-USER)) 1 0))))
  (define (session-event s st)
    (define n (session-record-count s))
    (define from (max 0 (- n TAIL-RECORDS)))
    (hasheq 'type "session" 'path (path->str (session-path s)) 'messages (pvector-length (agent-state-history st))
            'total n 'tail_from from 'turns (user-turns s) 'items (items-of s from n)
            'chat (and (chat-dir-of s) #t) 'workdir (config-workdir (agent-state-config st))))
  (define (watch! s)
    (session-on-append! s (lambda (pos r) (let ([it (record->item pos r)]) (when it (emit! it)))))
    s)
  ;; ---- 聊天会话（匿名工作区）：工作目录 = 应用管理的根目录下、按会话一个；普通会话 = 内核启动时的工作目录
  (define base-workdir (config-workdir (agent-state-config st0)))
  (define chat-lock (make-semaphore 1))
  (define (chat-dir-of s)                                        ; 会话的聊天目录（字符串）或 #f
    (define ws (session-workspace (session-path s)))
    (and ws (equal? (hash-ref ws 'kind #f) "chat") (let ([d (hash-ref ws 'dir #f)]) (and (string? d) d))))
  ;; 进入会话的工作区：工具与权限作用域的根换成它；聊天目录被清理掉了 → 重建空目录并记一条提示
  (define (enter-workspace st s)
    (define dir (chat-dir-of s))
    (when (and dir (not (directory-exists? dir)))
      (make-directory* dir)
      (note! "warn" "chat_dir_recreated" (format "this chat's directory had been removed (size management); a new empty one was created: ~a" dir)))
    (define wd (or dir base-workdir))
    (set! d* (struct-copy deps d* [policy (policy-with-scope (deps-policy d*) wd)]))
    (struct-copy agent-state st [config (struct-copy config (agent-state-config st) [workdir wd])]))
  ;; 只删聊天根目录里的目录（防误删）
  (define (inside-chat-root? dir)
    (define root (path->string (simplify-path (path->complete-path (chat-root (load-chat-config) (session-dir sess0))))))
    (define d (path->string (simplify-path (path->complete-path dir))))
    (and (> (string-length d) (add1 (string-length root))) (string-prefix? d (string-append (regexp-replace #rx"/$" root "") "/"))))
  (define (remove-chat-dir! dir) (when (and dir (directory-exists? dir) (inside-chat-root? dir)) (delete-directory/files dir)))
  ;; 关会话：本次新建、没消息的临时聊天会话 → 会话文件被删，它的目录一起删
  (define (close-session! s)
    (define dir (and (session-ephemeral? s) (chat-dir-of s)))
    (session-close! s)
    (when dir (remove-chat-dir! dir)))
  ;; 占用管理：合计 / 磁盘空闲超限 → 删最久没用的聊天目录（不删 keep 里的），给那些会话记提示；当前聊天目录超单个上限 → 提示
  (define warned-over (make-hash))
  (define last-check (box 0.0))
  (define (chat-usage-now s)
    (define cfg (load-chat-config))
    (define dir (session-dir s))
    (chat-usage (chat-root cfg dir)
                (lambda (name) (let ([p (build-path dir (string-append name ".rktd"))]) (and (file-exists? p) (file-or-directory-modify-seconds p))))))
  (define (chat-usage->jsexpr u cfg [removed '()])
    (hasheq 'type "chat_usage" 'root (hash-ref u 'root) 'total (hash-ref u 'total) 'free (or (hash-ref u 'free) 'null)
            'count (length (hash-ref u 'dirs)) 'complete (hash-ref u 'complete)
            'config (hash-set cfg 'root (or (hash-ref cfg 'root) 'null))
            'removed (for/list ([x removed]) (hasheq 'name (car x) 'bytes (cadr x)))))
  (define (manage-chat-space! s #:force? [force? #f])
    (call-with-semaphore chat-lock
      (lambda ()
        (when (or force? (> (- (current-inexact-milliseconds) (unbox last-check)) 60000.0))
          (set-box! last-check (current-inexact-milliseconds))
          (define cfg (load-chat-config))
          (define u (chat-usage-now s))
          (define cur-dir (chat-dir-of s))
          (define cur-name (and cur-dir (let-values ([(_b n _d) (split-path cur-dir)]) (path->string n))))
          ;; 当前聊天目录超单个上限：提示一次（降回去以后再超再提示）
          (define cur-bytes (for/first ([x (hash-ref u 'dirs)] #:when (equal? (car x) cur-name)) (cadr x)))
          (cond
            [(and cur-bytes (> cur-bytes (* GB (hash-ref cfg 'max_chat_gb))))
             (unless (hash-ref warned-over cur-name #f)
               (hash-set! warned-over cur-name #t)
               (note! "warn" "chat_dir_large" (format "this chat's directory uses ~a GB, over the per-chat limit of ~a GB (~a)" (gb cur-bytes) (hash-ref cfg 'max_chat_gb) cur-dir)))]
            [cur-name (hash-remove! warned-over cur-name)])
          (define plan (cleanup-plan u cfg (if cur-name (list cur-name) '())))
          (define root (chat-root cfg (session-dir s)))
          (for ([x (in-list plan)])
            (remove-chat-dir! (path->string (build-path root (car x))))
            ;; 那个会话里记一条（会话本身不删）
            (define sp (build-path (session-dir s) (string-append (car x) ".rktd")))
            (when (and (file-exists? sp) (not (equal? (path->str sp) (path->str (session-path s)))))
              (with-handlers ([exn:fail? void])
                (define s2 (session-open! sp (agent-state-config st0)))
                (session-append-rec! s2 'note (hasheq 'level "warn" 'code "chat_cleanup"
                                                      'text (format "this chat's directory was removed by size management (~a GB freed)" (gb (cadr x)))))
                (session-close! s2))))
          (emit! (chat-usage->jsexpr (if (null? plan) u (chat-usage-now s)) cfg plan))))))
  (watch! sess0)
  ;; 启动时：进入第一个会话的工作区（续接的可能是聊天会话），后台检查一次聊天目录占用
  (set! st0 (enter-workspace st0 sess0))
  (void (thread (lambda () (with-handlers ([exn:fail? (lambda (e) (eprintf "pi2: chat space check failed: ~a\n" (exn-message e)))])
                             (manage-chat-space! sess0 #:force? #t)))))
  ;; 回收站：超过 TRASH-DAYS 天的清掉
  (with-handlers ([exn:fail? (lambda (e) (eprintf "pi2: trash purge failed: ~a\n" (exn-message e)))]) (void (trash-purge! (session-dir sess0))))
  ;; blob 垃圾回收（决定 3：定期扫描）：启动时在后台看一次，距上次不到一天就跳过
  (void (thread (lambda () (with-handlers ([exn:fail? (lambda (e) (eprintf "pi2: blob gc failed: ~a\n" (exn-message e)))])
                             (blobs-gc! (session-dir sess0) session-media-records)))))
  (emit! (hasheq 'type "ready"
                 'model (config-model (agent-state-config st0))
                 'provider (current-provider-name)
                 'session (path->str (session-path sess0))
                 'workdir (config-workdir (agent-state-config st0))
                 'protocol 3))
  (emit! (session-event sess0 st0))
  ;; ---- 轮线程。job = (vector 结果通道 kind 轮前状态 附加信息)
  (define (start-job! kind st-before info thunk)
    (define ch (make-channel))
    (define t
      (parameterize-break #f                          ; 线程以 breaks 关闭起步，break 只在 thunk 内生效
        (thread
         (lambda ()
           (define r
             (with-handlers ([exn:break? (lambda (_e) (provider-cancel! (deps-provider d)) (cons 'interrupted #f))]
                             [exn:fail:budget? (lambda (e) (emit-error! (exn-message e)) (note! "error" "budget_stop" (exn-message e)) (cons 'budget #f))]   ; 用户在费用上限询问里选了停止
                             [exn:fail? (lambda (e) (emit-error! (exn-message e)) (note! "error" "error" (exn-message e)) (cons 'error #f))])
               (cons 'ok (parameterize-break #t
                           (parameterize ([current-effect-ledger (ledger-of (unbox cur-sess))]   ; 本会话的效果账本
                                          [current-record-sink (session-record-sink (unbox cur-sess))]   ; 消息追加进历史时就落盘
                                          [current-session-id (session-id (unbox cur-sess))]   ; 自动化提交的元数据
                                          [current-blob-root (blob-root-of (session-dir (unbox cur-sess)))]   ; 发请求时读媒体
                                          [current-tool-source (lambda (name) (host-tool-source host name))])   ; 工具来源标记（插件）
                             (call-with-route (unbox aff) host thunk))))))   ; 轮线程（及工具、子 agent）走路由
           (channel-put ch r)))))
    (set-box! running t)
    (emit! (hasheq 'type "turn_start" 'kind (symbol->string kind)))
    (vector ch kind st-before info))
  (define (finish-job! job r sess)
    (set-box! running #f)
    (bus-drain! (deps-bus d))
    (define kind (vector-ref job 1))
    (define st-before (vector-ref job 2))
    ;; ok → 结果状态；prompt 被中断 → 已完成步骤 + 补齐的工具结果 + <turn_aborted> 标记；
    ;; 其余（出错、goal / compact 被中断）→ 轮前状态
    (define st*
      (cond [(eq? (car r) 'ok) (cdr r)]
            [(and (eq? (car r) 'interrupted) (eq? kind 'prompt)) (or (interrupted-turn-state (vector-ref job 3)) st-before)]
            [(and (eq? (car r) 'budget) (eq? kind 'prompt)) (or (interrupted-turn-state (vector-ref job 3) BUDGET-STOP-MARKER) st-before)]
            [else st-before]))
    (set! st* (with-member-config st*))                            ; 轮中可能切了成员
    (define outcome (symbol->string (car r)))
    (define (usage-fields st)
      (list 'usage (usage->jsexpr (agent-state-token-usage st))
            'cost_usd (cost->jsexpr (config-model (agent-state-config st)) (agent-state-token-usage st))
            'budget (budget->jsexpr)))                                    ; 今日估算花费（状态行不用再发请求）
    ;; 被中断时流到一半的文字（不进模型上下文；界面显示），放在补齐的工具结果与中断标记之前
    (when (memq (car r) '(interrupted budget))
      (define pt (get-output-string partial-text))
      (define pr (get-output-string partial-reasoning))
      (unless (and (string=? (string-trim pt) "") (string=? (string-trim pr) ""))
        (session-append-rec! sess 'partial (hasheq 'text pt 'reasoning pr))))
    (reset-partial!)
    ;; 中断 / 费用停止：保留已完成的步骤 → 补齐的工具结果与中断标记（检查点之后加的）也落盘；没有检查点 → 退回轮前（记撤回）
    (when (and (eq? kind 'prompt) (memq (car r) '(interrupted budget)))
      (define cp (vector-ref job 3))
      (define cp-st (and cp (unbox cp) (vector-ref (unbox cp) 0)))
      (cond [cp-st (for ([i (in-range (pvector-length (agent-state-history cp-st)) (pvector-length (agent-state-history st*)))])
                     (session-append-msg! sess (pvector-ref (agent-state-history st*) i)))]
            [else (session-append-turn! sess 'revert)]))
    (when (and (memq kind '(goal compact)) (memq (car r) '(interrupted budget error)))
      ;; goal / compact 没做完：内存里退回轮前状态 → 记一条 compact（轮前的完整历史），重放与内存一致（已完成的 goal 轮仍在文件里）
      (session-append-compact! sess (hasheq 'history (for/list ([m (in-pvector (agent-state-history st-before))]) m)
                                            'usage (agent-state-token-usage st-before))))
    (case kind
      [(prompt)
       (persist! sess st-before st*)
       (when (chat-dir-of sess) (void (thread (lambda () (with-handlers ([exn:fail? void]) (manage-chat-space! sess))))))   ; 聊天会话：查占用（至多每分钟一次）
       (emit! (apply hasheq 'type "turn_complete" 'turn (agent-state-turn-count st*) 'outcome outcome
                     'interrupted (and (memq (car r) '(interrupted budget)) #t)
                     'kept_messages (- (pvector-length (agent-state-history st*)) (pvector-length (agent-state-history st-before)))
                     (usage-fields st*)))]
      [(goal)
       (note! "info" "goal_end" (format "goal ended: ~a" outcome))
       (emit! (apply hasheq 'type "goal_end" 'model (config-model (agent-state-config st*))
                     'turn (agent-state-turn-count st*) 'outcome outcome (usage-fields st*)))]
      [(compact)
       (emit! (hasheq 'type "ok" 'for "compact" 'outcome outcome
                      'messages (pvector-length (agent-state-history st*))))])
    st*)
  ;; ---- 会话切换（R3）
  ;; 先读、打开目标，再关当前的（目标读不了 → 抛出、当前会话不动）
  (define (switch-session! sess path st)
    (define cfg (agent-state-config st))
    (define st2 (session-replay path #:config cfg))
    (define sess2 (watch! (session-open! path cfg)))
    (close-session! sess)
    (define st3 (enter-workspace (restore-route st2 sess2) sess2))   ; 目标会话记过路由池 → 换成它的；聊天会话 → 它的目录
    (emit! (session-event sess2 st3))
    (emit-route-status!)
    (values st3 sess2))
  (define git-remotes (make-hash))                     ; 工作目录 → git 远端或 #f（每个目录本进程只查一次）
  (define (list-sessions! sess)
    (emit! (hasheq 'type "sessions"
                   'sessions (for/list ([i (in-list (session-infos (session-dir sess)))])
                               (define row (session-info->jsexpr i (session-path sess) (session-ephemeral? sess)))
                               (define d (session-info-workdir i))
                               (if (and (string? d) (not (session-info-chat? i)))       ; 非聊天会话带工作目录的 git 远端（目录 catalog 用）
                                   (hash-set row 'git_remote (or (hash-ref! git-remotes d (lambda () (workdir-git-remote d))) 'null))
                                   row)))))
  ;; ---- 派发：→ (values st sess job quit?)
  (define (dispatch req st sess)
    (set-box! cur-sess sess)
    (define type (and (hash? req) (jget req 'type)))
    (define (same) (values st sess #f #f))
    (case type
      [("prompt")
       (define text (let ([t (jget req 'text "")]) (if (string? t) t "")))
       ;; 'media：[{kind media_type name data(base64) | sha, meta}] → 存进 blob，消息里只有引用（媒体在文字之后）
       (define media (with-handlers ([exn:fail? values]) (prompt-media (jget req 'media) (blob-root-of (session-dir sess)))))
       (define user-msg (if (or (exn? media) (null? media)) (text-msg 'user text) (message 'user (cons (text-block text) media))))
       (cond
         [(exn? media) (emit-error! (exn-message media)) (same)]          ; 媒体太大：不开始这一轮
         [else
       ;; Auto 模式（仅 DeepSeek 生效）：按任务改 model + 推理档。
       (define-values (st1 auto-dec) (maybe-apply-auto (with-instructions st sess) text host))
       (when auto-dec
         (emit! (hasheq 'type "auto" 'model (car auto-dec) 'reasoning (symbol->string (cdr auto-dec))))
         (note! "dim" "auto" (format "auto → ~a (~a)" (car auto-dec) (cdr auto-dec))
                (hasheq 'model (car auto-dec) 'reasoning (symbol->string (cdr auto-dec)))))
       ;; 检查点 box：中断时保留已完成步骤（loop.rkt interrupted-turn-state）
       (define cp (box #f))
       (values st1 sess
               (start-job! 'prompt st1 cp
                           (lambda () (parameterize ([current-turn-checkpoint cp]) (run-turn! st1 user-msg d*))))
               #f)])]
      [("goal")
       ;; headless Goal 模式:自主多轮直到 until 全过/轮数耗尽/预算耗尽。turn 流式事件仍走 bus,
       ;; 另加 goal_start / goal_status(驱动状态行) / goal_end。
       (define goal (jget req 'goal))
       (define until (jget req 'until))
       (define mt (jget req 'max_turns 20))
       (define budget (jget req 'budget))
       ;; 验收加固（acceptance.rkt）：protect = "default" | "none" | [glob]；review（缺省开）、review_model；tests_first（until 可以为空）
       (define tests-first? (eq? (jget req 'tests_first) #t))
       (define protect (let ([v (jget req 'protect "default")]) (cond [(equal? v "none") '()] [(and (list? v) (andmap string? v)) v] [else 'default])))
       (cond
         [(not (and (string? goal) (list? until) (andmap string? until)))                ; until 可以空：有工作区测试流程就用它，没有就先写验收
          (emit-error! "goal requires string 'goal and a string list 'until (empty = the workspace test flow, or write acceptance tests first)") (same)]
         [else
          (define max-turns (if (exact-positive-integer? mt) mt 20))
          (emit! (hasheq 'type "goal_start" 'goal goal 'until until 'max_turns max-turns))
          (note! "info" "goal_start" (format "goal: ~a" goal) (hasheq 'goal goal))
          (define (strip s) (regexp-replace* #rx"\e\\[[0-9;]*m" s ""))
          (define st1 (with-instructions st sess))
          (values st1 sess
                  (start-job! 'goal st1 #f
                              (lambda ()
                                (run-goal! d* st1 sess goal until max-turns host
                                           #:protect protect #:review? (not (eq? (jget req 'review) #f))
                                           #:review-model (let ([m (jget req 'review_model)]) (and (string? m) (not (string=? m "")) m))
                                           #:tests-first? tests-first?
                                           #:budget (and (real? budget) (> budget 0) budget)
                                           #:emit (lambda (s) (emit! (hasheq 'type "goal_status" 'text (strip s)))
                                                           (note! "dim" "goal_status" (strip s))))))
                  #f)])]
      [("compact")
       (values st sess (start-job! 'compact st #f (lambda () (compact! st (deps-provider d)))) #f)]
      [("clear")
       (session-append-compact! sess (hasheq 'history '() 'usage usage-zero))   ; 清空也记下（重放时历史变空；原消息仍在文件里）
       (emit! (hasheq 'type "ok" 'for "clear"))
       (values (make-initial-state (agent-state-config st)) sess #f #f)]
      [("list_sessions") (list-sessions! sess) (same)]
      [("list_providers") (emit-providers! st) (same)]
      [("ping") (emit! (hasheq 'type "pong")) (same)]
      [("new_session")
       ;; {"type":"new_session","chat":bool?}：chat → 聊天会话（匿名工作区：应用管理的目录，按会话一个）
       (close-session! sess)
       (define cfg (agent-state-config st))
       (define p (fresh-session-path (path->str (session-dir sess))))
       (define sess2 (watch! (session-open! p cfg)))
       (when (eq? (jget req 'chat) #t)
         (define dir (path->string (chat-dir-for (load-chat-config) (session-dir sess2) p)))
         (make-directory* dir)
         (session-append-workspace! sess2 (hasheq 'kind "chat" 'dir dir)))
       (define st2 (enter-workspace (make-initial-state cfg) sess2))
       (emit! (session-event sess2 st2))
       (values st2 sess2 #f #f)]
      [("resume")
       (define p (jget req 'path))
       (cond
         [(not (and (string? p) (file-exists? p))) (emit-error! f"resume: no such session file: {p}") (same)]
         [else (define-values (st2 sess2) (switch-session! sess p st)) (values st2 sess2 #f #f)])]
      [("fork")
       ;; {"type":"fork","at":n?}：从当前会话前 n 条消息（缺省全部）分叉出新会话并切过去
       (define n (let ([a (jget req 'at)]) (and (exact-nonnegative-integer? a) a)))
       (define k (let ([m (pvector-length (agent-state-history st))]) (if n (min n m) m)))
       (cond
         ;; 没有消息可分叉：分出来的会话也是空的（关闭时会被当空会话删掉）→ 不做，说明原因
         [(zero? k) (emit-error! "fork: the session has no messages yet — nothing to fork") (same)]
         [else
          (define src-chat (chat-dir-of sess))
          (define p (session-fork! (session-path sess) (path->str (session-dir sess)) #:at n))
          (define-values (st2 sess2) (switch-session! sess p st))
          (cond
            [src-chat
             ;; 聊天会话分叉：新目录；原目录不大（< 200 MB）就复制过去，否则空目录 + 提示
             (define dir (path->string (chat-dir-for (load-chat-config) (session-dir sess2) p)))
             (define-values (bytes _ok) (dir-size src-chat #:deadline (+ (current-inexact-milliseconds) 2000.0)))
             (cond [(and (directory-exists? src-chat) (< bytes (* 200 1024 1024)) (not (directory-exists? dir))) (copy-directory/files src-chat dir)]
                   [else (make-directory* dir)
                         (note! "warn" "chat_fork_dir" (format "the parent chat's files (~a GB) were not copied; they are in ~a" (gb bytes) src-chat))])
             (session-append-workspace! sess2 (hasheq 'kind "chat" 'dir dir 'forked_from src-chat))
             (define st3 (enter-workspace st2 sess2))
             (emit! (session-event sess2 st3))
             (values st3 sess2 #f #f)]
            [else (values st2 sess2 #f #f)])])]
      [("delete_session")
       (define p (jget req 'path))
       (cond
         [(not (string? p)) (emit-error! "delete_session requires string 'path") (same)]
         [(equal? (path->str p) (path->str (session-path sess))) (emit-error! "cannot delete the current session") (same)]
         [else
          ;; 进回收站（可恢复，TRASH-DAYS 天后清掉）；聊天会话的目录一起挪进去
          (define ws (session-workspace p))
          (define chat-dir (and ws (equal? (hash-ref ws 'kind #f) "chat") (let ([d (hash-ref ws 'dir #f)]) (and d (inside-chat-root? d) d))))
          (session-trash! p #:chat-dir chat-dir)
          (trash-purge! (session-dir sess))
          (emit! (hasheq 'type "ok" 'for "delete_session" 'path p 'restorable #t 'days TRASH-DAYS)) (same)])]
      [("_set_auto_title")                                         ; auto_title 的结果（内部）：还没有标题才写，写成自动的
       (define p (jget req 'path))
       (define title (jget req 'title))
       (cond
         [(not (and (string? p) (string? title) (file-exists? p))) (same)]
         [(session-info-titled? (read-session-info p)) (emit! (hasheq 'type "auto_title" 'path p 'skipped "already titled")) (same)]
         [else
          (if (equal? (path->str p) (path->str (session-path sess)))
              (session-append-title! sess title #:auto? #t)
              (let ([s2 (session-open! p (agent-state-config st))]) (session-append-title! s2 title #:auto? #t) (session-close! s2)))
          (emit! (hasheq 'type "auto_title" 'path p 'title title 'model (jget req 'model 'null)))
          (list-sessions! sess)
          (same)])]
      ;; ---- 项目索引（design-memory.md §8）：都在线程里做（走目录、读文件、调模型），结果是事件
      [("host_scan")                                                ; {roots?} → host_repos {repos [{dir name fingerprint remote mtime}] ms}
       (define roots (or scan-roots (let ([v (jget req 'roots)]) (if (and (list? v) (andmap string? v) (pair? v)) v '("~")))))
       (void (thread (lambda ()
                       (with-handlers ([exn:fail? (lambda (e) (emit! (hasheq 'type "host_repos" 'error (exn-message e))))])
                         (define t0 (current-inexact-milliseconds))
                         (define repos (map repo-info (discover-repos roots #:deadline (+ t0 60000.0))))
                         (emit! (hasheq 'type "host_repos" 'roots roots 'repos repos 'ms (round (- (current-inexact-milliseconds) t0))))))))
       (same)]
      [("describe_input" "describe_project")
       ;; describe_input {dir} → 事件 describe_input {dir fingerprint input}（远端主机的仓库：界面拿去给本机内核描述）
       ;; describe_project {dir input? fingerprint? endpoint? model? allow_remote?} → project_described {dir fingerprint desc model ms} 或 {dir error}
       (define dir (jget req 'dir))
       (define given (let ([v (jget req 'input)]) (and (string? v) v)))
       (define (git-repo? d) (and (string? d) (directory-exists? d) (or (directory-exists? (build-path d ".git")) (file-exists? (build-path d ".git")))))
       (define ep (let ([v (jget req 'endpoint)]) (if (and (string? v) (not (string=? v ""))) v DEFAULT-LOCAL-ENDPOINT)))
       (cond
         [(not (string? dir)) (emit-error! f"{type} requires string 'dir") (same)]
         [(and (not given) (not (git-repo? dir))) (emit! (hasheq 'type (if (equal? type "describe_input") "describe_input" "project_described") 'dir dir 'error "not a git repository on this machine")) (same)]
         [(equal? type "describe_input")
          (void (thread (lambda () (with-handlers ([exn:fail? (lambda (e) (emit! (hasheq 'type "describe_input" 'dir dir 'error (exn-message e))))])
                                     (emit! (hasheq 'type "describe_input" 'dir dir 'fingerprint (repo-fingerprint dir) 'input (describe-input dir)))))))
          (same)]
         [(and (not (local-endpoint? ep)) (not (eq? (jget req 'allow_remote) #t)))
          (emit! (hasheq 'type "project_described" 'dir dir 'error (format "endpoint is not on this machine: ~a" ep))) (same)]
         [else
          (void (thread
                 (lambda ()
                   (with-handlers ([exn:fail? (lambda (e) (emit! (hasheq 'type "project_described" 'dir dir 'host (jget req 'host 'null) 'error (exn-message e))))])
                     (define t0 (current-inexact-milliseconds))
                     (define input (or given (describe-input dir)))
                     (define fp (or (let ([v (jget req 'fingerprint)]) (and (string? v) v)) (repo-fingerprint dir)))
                     (define model (let ([m (jget req 'model)])
                                     (cond [(and (string? m) (not (string=? m ""))) m]
                                           [else (define r (fetch-models ep))
                                                 (or (and r (pair? (cdr r)) (cadr r)) (and r (pair? (car r)) (caar r))
                                                     (error 'describe_project "no model loaded at ~a" ep))])))
                     (define reply (complete-long* ep model (list (text-msg 'user (string-append DESCRIBE-PROMPT "\n\n" input)))))
                     (define desc (parse-description reply))
                     (if desc
                         (emit! (hasheq 'type "project_described" 'dir dir 'host (jget req 'host 'null) 'fingerprint fp 'desc desc 'model model
                                        'ms (round (- (current-inexact-milliseconds) t0))))
                         (emit! (hasheq 'type "project_described" 'dir dir 'host (jget req 'host 'null) 'error "the model's reply was not the expected JSON")))))))
          (same)])]
      ;; ---- 工作区测试流程（design-goalmode.md §12 T2）：界面的「测试」面板
      [("test_flow" "test_flow_run" "test_flow_trust" "test_flow_decide")
       (define wd (config-workdir (agent-state-config st)))
       (define-values (fl err) (load-flow wd))
       (define (proposals)                                          ; 本会话里还没处理的修改提议
         (define recs (filter hash? (map rec-data (filter rec? (records-of-type (session-path sess) 'tests)))))
         (define decided (for/list ([r recs] #:when (equal? (hash-ref r 'kind #f) "decision")) (hash-ref r 'id #f)))
         (for/list ([r recs] #:when (and (equal? (hash-ref r 'kind #f) "proposal") (not (member (hash-ref r 'id #f) decided))))
           (hasheq 'id (hash-ref r 'id) 'reason (hash-ref r 'reason "") 'diff (hash-ref r 'diff 'null) 'base_sha (hash-ref r 'base_sha ""))))
       (define (view!)
         (emit! (if fl
                    (hasheq 'type "test_flow" 'exists #t 'root (flow-root fl) 'file (flow-file fl) 'name (flow-name fl) 'sha (flow-sha fl) 'trusted (trusted? fl)
                            'steps (for/list ([s (flow-steps fl)]) (hasheq 'id (symbol->string (step-id s)) 'cmd (step-cmd s) 'parse (symbol->string (step-parse s)) 'timeout (step-timeout s)))
                            'protect (flow-protect fl) 'history (history-read (flow-root fl)) 'proposals (proposals) 'error 'null)
                    (hasheq 'type "test_flow" 'exists #f 'root (proposal-root wd) 'error (or err 'null) 'proposals (proposals) 'history '()))))
       (case type
         [("test_flow") (view!)]
         [("test_flow_trust")                                       ; 用户在面板里点「信任」：只认面板上看到的那个内容
          (cond [(and fl (equal? (jget req 'sha) (flow-sha fl))) (trust! fl) (view!)]
                [else (emit-error! "test_flow_trust: the flow changed; reopen the Tests panel")])]
         [("test_flow_run")                                         ; 用户在面板里点「运行」= 这一次同意跑（面板上列着命令）；不记成「始终信任」
          (cond
            [(not fl) (emit-error! (or err "this workspace has no test flow"))]
            [else
             (define ids (let ([v (jget req 'steps)]) (and (list? v) (pair? v) (map string->symbol (filter string? v)))))
             (define s0 sess)
             (void (thread (lambda ()
                             (with-handlers ([exn:fail? (lambda (e) (emit-error! (format "test flow: ~a" (exn-message e))))])
                               (define rs (run-flow fl #:only ids))
                               (history-append! fl "run" rs)
                               (session-append-rec! s0 'tests (hasheq 'kind "run" 'ok (andmap (lambda (r) (hash-ref r 'ok)) rs) 'by "user"
                                                                      'steps (map (lambda (r) (hash-remove r 'tail)) rs)))
                               (emit! (hasheq 'type "test_flow_result" 'ok (andmap (lambda (r) (hash-ref r 'ok)) rs)
                                              'results (for/list ([r rs]) (hash-set (step-summary r) 'tail (hash-ref r 'tail "")))
                                              'report (format-report fl rs #:title "Test run")))))))])]
         [("test_flow_decide")                                      ; {id decision "apply"|"reject"}
          (define id (jget req 'id))
          (define p (for/first ([r (filter hash? (map rec-data (filter rec? (records-of-type (session-path sess) 'tests))))]
                                #:when (and (equal? (hash-ref r 'kind #f) "proposal") (equal? (hash-ref r 'id #f) id)))
                      r))
          (cond
            [(not p) (emit-error! "test_flow_decide: no such proposal in this session")]
            [(equal? (jget req 'decision) "apply")
             (define-values (ok? v) (apply-proposal! (hash-ref p 'root) (hash-ref p 'content) (hash-ref p 'base_sha "")))
             (cond [ok? (session-append-rec! sess 'tests (hasheq 'kind "decision" 'id id 'decision "applied" 'reason (hash-ref p 'reason "")))
                        (emit! (hasheq 'type "ok" 'for "test_flow_decide" 'id id 'decision "applied"))]
                   [else (emit-error! (format "test_flow_decide: ~a" v))])
             (let-values ([(fl2 err2) (load-flow wd)]) (set! fl fl2) (set! err err2))
             (view!)]
            [else (session-append-rec! sess 'tests (hasheq 'kind "decision" 'id id 'decision "rejected" 'reason (hash-ref p 'reason "")))
                  (emit! (hasheq 'type "ok" 'for "test_flow_decide" 'id id 'decision "rejected"))
                  (view!)])])
       (same)]
      ;; ---- 自动记忆（design-memory.md M3）：界面调度；这里只给会话文字、调本机模型
      [("memory_input")                                             ; {path max_bytes?} → memory_input {path text messages}
       (define p (jget req 'path))
       (cond
         [(not (and (string? p) (file-exists? p))) (emit! (hasheq 'type "memory_input" 'path (or p 'null) 'error "no such session file"))]
         [else (void (thread (lambda ()
                               (with-handlers ([exn:fail? (lambda (e) (emit! (hasheq 'type "memory_input" 'path p 'error (exn-message e))))])
                                 (define-values (text n) (memory-input p #:max-bytes (let ([m (jget req 'max_bytes)]) (if (exact-positive-integer? m) m 16000))))
                                 (emit! (hasheq 'type "memory_input" 'path p 'text text 'messages n))))))])
       (same)]
      [("local_complete")                                           ; {id prompt endpoint? model? allow_remote?} → local_completed {id text model ms} | {id error}
       (define id (jget req 'id 'null))
       (define prompt (jget req 'prompt))
       (define ep (let ([v (jget req 'endpoint)]) (if (and (string? v) (not (string=? v ""))) v DEFAULT-LOCAL-ENDPOINT)))
       (cond
         [(not (string? prompt)) (emit! (hasheq 'type "local_completed" 'id id 'error "prompt must be a string"))]
         [(and (not (local-endpoint? ep)) (not (eq? (jget req 'allow_remote) #t))) (emit! (hasheq 'type "local_completed" 'id id 'error (format "endpoint is not on this machine: ~a" ep)))]
         [else
          (void (thread
                 (lambda ()
                   (with-handlers ([exn:fail? (lambda (e) (emit! (hasheq 'type "local_completed" 'id id 'error (exn-message e))))])
                     (define t0 (current-inexact-milliseconds))
                     (define model (let ([m (jget req 'model)])
                                     (cond [(and (string? m) (not (string=? m ""))) m]
                                           [else (define r (fetch-models ep))
                                                 (or (and r (pair? (cdr r)) (cadr r)) (and r (pair? (car r)) (caar r))
                                                     (error 'local_complete "no model loaded at ~a" ep))])))
                     (define text (complete-long* ep model (list (text-msg 'user prompt))))
                     (emit! (hasheq 'type "local_completed" 'id id 'text text 'model model 'ms (round (- (current-inexact-milliseconds) t0))))))))])
       (same)]
      [("set_model_input")                                          ; 送给模型的输入的处理（高级设置）：hidden_text = "mark" | "raw"
       (define m (jget req 'hidden_text))
       (cond [(member m '("mark" "raw")) (set-hidden-text-mode! (string->symbol m))
                                         (emit! (hasheq 'type "ok" 'for "set_model_input" 'hidden_text m))]
             [else (emit-error! "set_model_input: hidden_text must be \"mark\" or \"raw\"")])
       (same)]
      [("set_catalog_view")                                         ; 界面推来的目录视图（项目、每个会话的项目 / 置顶 / 归档），给 sessions_overview
       (set-box! catalog-view (hasheq 'instructions (let ([v (jget req 'instructions '())]) (if (list? v) (filter hash? v) '()))   ; [{id kind name text}]；会话行的 instr = id 表
                                      'memories (let ([v (jget req 'memories #f)]) (if (list? v) (filter hash? v) #f))   ; [{id scope text pinned updated}]；会话行的 mem = 作用域链、mem_read / mem_write
                                      'memory_global_auto (eq? (jget req 'memory_global_auto) #t)
                                      'host_projects (let ([v (jget req 'host_projects #f)]) (if (list? v) (filter hash? v) #f))   ; [{dir name remote mtime desc}]
                                      'projects (let ([v (jget req 'projects '())]) (if (list? v) v '()))
                                      'sessions (let ([v (jget req 'sessions '())]) (if (list? v) (filter hash? v) '()))))
       (same)]
      [("restore_session")
       (define p (jget req 'path))
       (cond
         [(not (string? p)) (emit-error! "restore_session requires string 'path") (same)]
         [else
          (define r (with-handlers ([exn:fail? (lambda (e) e)])
                      (session-restore! p #:chat-dir (lambda (rp) (let ([ws (session-workspace rp)])
                                                                    (and ws (equal? (hash-ref ws 'kind #f) "chat") (hash-ref ws 'dir #f)))))))
          (cond [(exn? r) (emit-error! (exn-message r)) (same)]
                [else (emit! (hasheq 'type "ok" 'for "restore_session" 'path p)) (same)])])]
      [("set_model")
       (define m (jget req 'model))
       (cond
         [(string? m)
          (emit! (hasheq 'type "ok" 'for "set_model" 'model m))
          (values (struct-copy agent-state st [config (struct-copy config (agent-state-config st) [model m])]) sess #f #f)]
         [else (emit-error! "set_model requires string model") (same)])]
      [("set_provider")
       (define name (jget req 'name))
       (cond
         [(route-ref-name? name)                                      ; "@池名"：走路由池
          (define st2 (select-route! name st sess))
          (cond
            [(not st2) (emit-error! f"unknown route: {name}") (same)]
            [else
             (emit! (hasheq 'type "ok" 'for "set_provider" 'provider name 'model (config-model (agent-state-config st2))))
             (emit-route-status!)
             (values st2 sess #f #f)])]
         [(not (and (string? name) (host-set-provider! host name)))
          (emit-error! f"unknown provider: {name}") (same)]
         [(builtin-provider-instance? name)
          (when (unbox aff) (set-box! aff #f) (session-append-route! sess ""))   ; 离开路由池
          (define c* (apply-provider-profile (agent-state-config st) name))
          (emit! (hasheq 'type "ok" 'for "set_provider" 'provider name 'model (config-model c*)))
          (values (struct-copy agent-state st [config c*]) sess #f #f)]
         [else (emit! (hasheq 'type "ok" 'for "set_provider" 'provider name)) (same)])]
      [("set_auto")
       (define on? (jget req 'on))
       (cond
         [(boolean? on?) (set-auto-mode! on?) (emit! (hasheq 'type "ok" 'for "set_auto" 'auto on?)) (same)]
         [else (emit-error! "set_auto requires boolean 'on") (same)])]
      [("add_key")
       ;; 存一条实例 token：{base, label?, token}。若为当前实例则刷新 config 的 api-key。
       (define base (jget req 'base))
       (define label (let ([l (jget req 'label)]) (if (string? l) l "default")))
       (define tok (jget req 'token))
       (cond
         [(not (and (string? base) (builtin-provider-name? base) (string? tok) (> (string-length tok) 0)))
          (emit-error! "add_key requires builtin base + non-empty token") (same)]
         [else
          (store-instance-key! base label tok)
          (define disp (instance-display base label))
          (emit! (hasheq 'type "ok" 'for "add_key" 'provider disp))
          (if (string=? (host-current-provider host) disp)
              (values (struct-copy agent-state st [config (apply-provider-profile (agent-state-config st) disp)]) sess #f #f)
              (same))])]
      [("set_reasoning")
       (define lvl (jget req 'level))
       (cond
         [(and (string? lvl) (valid-reasoning-effort? (string->symbol lvl)))
          (set-reasoning-effort! (string->symbol lvl))
          (emit! (hasheq 'type "ok" 'for "set_reasoning" 'level lvl)) (same)]
         [else (emit-error! "set_reasoning requires off|low|medium|high") (same)])]
      [("set_escalate")
       (define on? (jget req 'on))
       (cond
         [(boolean? on?) (set-escalate! on?) (emit! (hasheq 'type "ok" 'for "set_escalate" 'escalate on?)) (same)]
         [else (emit-error! "set_escalate requires boolean 'on") (same)])]
      [("set_fallback")
       ;; {"type":"set_fallback","chain":["anthropic","deepseek-v4-flash"]}；[] 清空。
       (define ch (jget req 'chain))
       (cond
         [(and (list? ch) (andmap string? ch))
          (set-fallback-chain! ch)
          (emit! (hasheq 'type "ok" 'for "set_fallback" 'chain (fallback-chain))) (same)]
         [else (emit-error! "set_fallback requires string list 'chain") (same)])]
      [("define_route")
       ;; {"type":"define_route","name":"pool","members":[{"instance":"deepseek[work]","model":"deepseek-v4-pro"},…]}；[] = 删除
       (define name (jget req 'name))
       (define ms (jget req 'members))
       (cond
         [(not (and (string? name) (regexp-match? #rx"^[^@ ]+$" name) (list? ms)
                    (andmap (lambda (m) (and (hash? m) (string? (hash-ref m 'instance #f)) (string? (hash-ref m 'model #f)))) ms)))
          (emit-error! "define_route requires name and members [{instance, model}]") (same)]
         [else
          (define-route! name (for/list ([m (in-list ms)]) (rmember (hash-ref m 'instance) (hash-ref m 'model))))
          (emit! (hasheq 'type "ok" 'for "define_route" 'name name)) (same)])]
      [("register_secret")
       ;; {"type":"register_secret","value":"sk-…","label":"chat"}：原值存本机凭据文件（0600），之后工具按句柄取用
       (define v (jget req 'value))
       (cond
         [(not (and (string? v) (> (string-length (string-trim v)) 0))) (emit-error! "register_secret requires value") (same)]
         [(regexp-match? #rx"^⟦secret:[0-9a-f]+⟧$" v) (emit! (hasheq 'type "ok" 'for "register_secret" 'handle 'null)) (same)]   ; 回放：只有句柄
         [else
          (define h (register-secret! (string-trim v) (let ([l (jget req 'label)]) (if (string? l) l "chat"))
                                      #:handle (jget req 'handle)))                    ; GUI 算的句柄（连远端内核时盐值不同）
          (store-key! (format "secret:~a" h) (string-trim v))
          (emit! (hasheq 'type "ok" 'for "register_secret" 'handle h)) (same)])]
      [("set_permission_mode")
       ;; {"type":"set_permission_mode","mode":"normal"}；strict | normal | auto | read-only | yolo
       (define m (let ([v (jget req 'mode)]) (and (string? v) (string->symbol v))))
       (cond
         [(not (memq m PERMISSION-MODES)) (emit-error! f"set_permission_mode: unknown mode {m}") (same)]
         [else
          (set! d* (struct-copy deps d* [policy (policy-with-mode (deps-policy d*) m)]))
          (emit! (hasheq 'type "ok" 'for "set_permission_mode" 'mode (symbol->string m)))
          (values (struct-copy agent-state st [config (struct-copy config (agent-state-config st) [permission-mode m])]) sess #f #f)])]
      [("list_adaptations")
       ;; 全部规则（不只是当前模型匹配的；设置页用）
       (emit! (hasheq 'type "adaptations" 'rules (map rule->jsexpr (all-rules (config-workdir (agent-state-config st))))))
       (same)]
      [("set_adaptation")
       ;; {"type":"set_adaptation","name":"qwen3-no-think","enabled":false}
       (define name (jget req 'name))
       (define on? (jget req 'enabled))
       (cond
         [(not (and (string? name) (boolean? on?))) (emit-error! "set_adaptation requires name and boolean enabled") (same)]
         [else (set-rule-enabled! name on?) (emit! (hasheq 'type "ok" 'for "set_adaptation" 'name name 'enabled on?)) (same)])]
      [("use_endpoint")
       ;; {"type":"use_endpoint","provider":"deepseek","endpoint":"http://127.0.0.1:N/anthropic","api_key":"pi2p-…","model":"…"}
       ;; 远端子 agent 用：端点与 key 只放进本进程内存里的配置（不写凭据文件；会话文件的 meta 本来就不记 key）。
       ;; 用的是本机签发的临时 token（经反向隧道回到本机转发）或临时分享的 key；模型固定（关掉 Auto 与自适应升级，token 只认这个模型）
       (define pname (jget req 'provider))
       (define ep (jget req 'endpoint))
       (define key (jget req 'api_key))
       (define m (jget req 'model))
       (cond
         [(not (and (string? pname) (string? ep) (string? key))) (emit-error! "use_endpoint requires provider, endpoint and api_key") (same)]
         [else
          (host-set-provider! host pname)
          (set-box! aff #f)                                            ; 离开路由池
          (set-auto-mode! #f)
          (set-escalate! #f)
          (set-borrowed-credential! #t)                                ; 花的是借出方的钱：由那边记账
          (define c (agent-state-config st))
          (define c* (struct-copy config c [endpoint ep] [api-key key] [model (if (string? m) m (config-model c))]))
          (emit! (hasheq 'type "ok" 'for "use_endpoint" 'provider pname 'model (config-model c*)))
          (values (struct-copy agent-state st [config c*]) sess #f #f)])]
      [("set_external_spend")
       ;; GUI（一窗多主机）推来：其他主机今天的花费、合计每天上限（null = 不设）、日期。只在内存里；不回应答（避免来回）
       (set-external-spend! (jget req 'others_usd 0.0) (jget req 'combined_cap_usd #f) (let ([d (jget req 'date)]) (if (string? d) d (today))))
       (same)]
      [("get_budget") (emit! (hash-set (budget->jsexpr) 'type "budget")) (same)]
      [("set_budget")
       ;; {"type":"set_budget","daily_usd":5,"turn_usd":null}：数 = 设上限，null / 0 = 取消，不给的键 = 不变
       (define (arg k) (if (hash-has-key? req k) (let ([v (hash-ref req k)]) (if (real? v) v #f)) 'keep))
       (set-budget! #:daily (arg 'daily_usd) #:turn (arg 'turn_usd))
       (emit! (hasheq 'type "ok" 'for "set_budget"))
       (same)]
      [("list_aliases")
       (emit! (hasheq 'type "aliases" 'aliases (for/list ([a (in-list (model-aliases))]) (hasheq 'name (car a) 'model (cdr a)))))
       (same)]
      [("set_alias")
       ;; {"type":"set_alias","name":"fast","model":"deepseek-v4-flash"}；model 为空 = 删除
       (define name (jget req 'name))
       (define m (jget req 'model))
       (cond
         [(not (valid-alias-name? name)) (emit-error! "set_alias: name must be letters, digits, _ . - (1-40)") (same)]
         [(not (or (string? m) (eq? m 'null) (not m))) (emit-error! "set_alias: model must be a string") (same)]
         [else (set-model-alias! name (and (string? m) m))
               (emit! (hasheq 'type "ok" 'for "set_alias" 'name name)) (same)])]
      [("save_adaptation")
       ;; {"type":"save_adaptation","name":"…","description":"…","model":"正则","provider":"正则","system_append":"…",
       ;;  "system_prepend":"…","temperature":0.6,"reasoning":["off"]}：存进 adapt-settings.rktd（同名替换）
       (define name (jget req 'name))
       (define props
         (for/fold ([h (hasheq)]) ([k (in-list '(model provider system_append system_prepend temperature reasoning))])
           (define v (jget req k))
           (define key (string->symbol (regexp-replace* #rx"_" (symbol->string k) "-")))
           (cond [(or (not v) (eq? v 'null) (equal? v "")) h]
                 [(eq? k 'reasoning) (hash-set h key (if (list? v) (map (lambda (x) (string->symbol (format "~a" x))) v) v))]
                 [else (hash-set h key v)])))
       (with-handlers ([exn:fail? (lambda (e) (emit-error! (exn-message e)))])
         (save-settings-rule! name (let ([d (jget req 'description)]) (if (string? d) d "")) props)
         (emit! (hasheq 'type "ok" 'for "save_adaptation" 'name name)))
       (same)]
      [("delete_adaptation")
       (define name (jget req 'name))
       (if (and (string? name) (delete-settings-rule! name))
           (emit! (hasheq 'type "ok" 'for "delete_adaptation" 'name name))
           (emit-error! "delete_adaptation: only rules created in settings can be deleted"))
       (same)]
      [("route_pin")
       (define i (jget req 'index))
       (cond
         [(not (and (unbox aff) (exact-nonnegative-integer? i))) (emit-error! "route_pin requires an active route and index") (same)]
         [else (pin! (unbox aff) i) (emit! (hasheq 'type "ok" 'for "route_pin" 'index i))
               (values (with-member-config st) sess #f #f)])]
      [("route_reset")
       (define inst (jget req 'instance))
       (define mdl (jget req 'model))
       (cond
         [(not (and (string? inst) (string? mdl))) (emit-error! "route_reset requires instance and model") (same)]
         [else (reset-member! (rmember inst mdl)) (emit! (hasheq 'type "ok" 'for "route_reset")) (same)])]
      [("state")
       (define c (agent-state-config st))
       (emit! (hasheq 'type "state" 'model (config-model c)
                      'provider (current-provider-name)
                      'route (or (route-json) 'null)
                      'adaptations (adaptations-json c)
                      'permission_mode (symbol->string (permission-policy-mode (deps-policy d*)))
                      ;; 调用格式（用户不感知，调试面板显示）：Anthropic 档案 → anthropic；其余 → chat / responses
                      'wire (let* ([inst (if (unbox aff) (rmember-instance (affinity-member (unbox aff))) (host-current-provider host))]
                                   [prof (profile-by-name (instance-base inst))])
                              (if (and prof (eq? (provider-profile-kind prof) 'anthropic))
                                  "anthropic"
                                  (symbol->string (wire-for (config-endpoint c) (config-model c)))))
                      'reasoning (symbol->string (effective-reasoning-effort (config-model c)))
                      'stream_checkpoint (or checkpoint-s 'null)
                      'auto (auto-mode-on?)
                      'escalate (escalate-on?)
                      'fallback (fallback-chain)
                      'turn (agent-state-turn-count st)
                      'messages (pvector-length (agent-state-history st))
                      'session (path->str (session-path sess))
                      'usage (usage->jsexpr (agent-state-token-usage st))
                      'cost_usd (cost->jsexpr (config-model c) (agent-state-token-usage st))
                      'budget (budget->jsexpr)))                                  ; 今日估算花费与上限（状态行、设置页）
       (same)]
      [("history") (emit! (hasheq 'type "history" 'messages (history->jsexpr st (session-diffs (session-path sess))))) (same)]
      ;; ---- RPC v3：翻页
      [("items")
       ;; {"type":"items","from":a,"to":b,"preview_bytes":n?} → 记录序号 [a, b) 里能显示的条目
       (define n (session-record-count sess))
       (define from (let ([v (jget req 'from)]) (if (exact-integer? v) (max 0 v) 0)))
       (define to (let ([v (jget req 'to)]) (if (exact-integer? v) (min n v) n)))
       (define pb (let ([v (jget req 'preview_bytes)]) (if (exact-positive-integer? v) v DEFAULT-PREVIEW-BYTES)))
       (emit! (tagged req (hasheq 'type "items" 'from from 'to to 'total n 'items (items-of sess from to pb))))
       (same)]
      [("item")
       ;; {"type":"item","id":k}：单条完整（长工具输出的全文）
       (define id (jget req 'id))
       (define it (and (exact-nonnegative-integer? id) (pair? (items-of sess id (add1 id) 100000000)) (car (items-of sess id (add1 id) 100000000))))
       (if it (emit! (tagged req (hash-set it 'type "item_full"))) (emit-error! f"item: no displayable record {id}"))
       (same)]
      [("index_summary")
       ;; {"type":"index_summary","from":a,"to":b} → 每条记录 6 字节（base64）：显示种类 u8、标志 u8、行数 u16、文字长度/16 u16（LE），界面估计高度用
       (define ix (session-index sess))
       (define n (index-count ix))
       (define from (let ([v (jget req 'from)]) (if (exact-integer? v) (max 0 v) 0)))
       (define to (let ([v (jget req 'to)]) (if (exact-integer? v) (min n v) n)))
       (define bs (make-bytes (* 6 (max 0 (- to from))) 0))
       (for ([i (in-range from to)] [k (in-naturals)])
         (define e (index-ref ix i))
         (define o (* 6 k))
         (bytes-set! bs o (display-kind (type-name (ientry-type e)) (ientry-role e)))
         (bytes-set! bs (+ o 1) (bitwise-and (ientry-flags e) 255))
         (integer->integer-bytes (min 65535 (ientry-lines e)) 2 #f #f bs (+ o 2))
         (integer->integer-bytes (min 65535 (quotient (ientry-text-len e) 16)) 2 #f #f bs (+ o 4)))
       (emit! (tagged req (hasheq 'type "index_summary" 'from from 'to to 'total n 'data (bytes->string/latin-1 (base64-encode bs #"")))))
       (same)]
      [("session_info") (emit! (session-event sess st)) (same)]
      ;; ---- 会话的信息 / 改名 / 导出（界面右键菜单；design-workspace.md W1）。path 必须是本会话目录里的会话文件
      [("session_stats" "set_title" "export_session" "auto_title")
       (define p (let ([v (jget req 'path)]) (if (string? v) v (path->str (session-path sess)))))
       (define dir (simplify-path (path->complete-path (session-dir sess))))
       (define ok-path? (and (file-exists? p) (regexp-match? #rx"[.]rktd$" p)
                             (let-values ([(base _n _d) (split-path (simplify-path (path->complete-path p)))]) (equal? base dir))))
       (cond
         [(not ok-path?) (emit-error! f"{type}: not a session in this kernel's session directory: {p}") (same)]
         [else
          (case type
            [("session_stats") (emit! (hash-set (session-stats p) 'type "session_stats"))]
            [("set_title")
             (define title (let ([v (jget req 'title)]) (and (string? v) (string-trim v))))
             (cond
               [(not (and title (not (string=? title "")) (<= (string-length title) 200))) (emit-error! "set_title requires a non-empty title (up to 200 characters)")]
               [(equal? (path->str p) (path->str (session-path sess))) (session-append-title! sess title)
                                                                     (emit! (hasheq 'type "ok" 'for "set_title" 'path p 'title title))]
               [else (define s2 (session-open! p (agent-state-config st)))                     ; 别的会话：打开、追加、关闭（不是本次新建的 → 不会被删）
                     (session-append-title! s2 title) (session-close! s2)
                     (emit! (hasheq 'type "ok" 'for "set_title" 'path p 'title title))])
             (list-sessions! sess)]
            [("auto_title")
             ;; 自动起标题（§11.4 B）：本机小模型读前几条消息给一个短标题；在线程里调模型，结果经请求队列回到主循环再写
             ;; （轮进行中也不会同时写会话文件）。端点缺省本机 LM Studio，不是本机的要 allow_remote。
             (define ep (let ([v (jget req 'endpoint)]) (if (and (string? v) (not (string=? v ""))) v DEFAULT-LOCAL-ENDPOINT)))
             (cond
               [(and (not (local-endpoint? ep)) (not (eq? (jget req 'allow_remote) #t))) (emit! (hasheq 'type "auto_title" 'path p 'error (format "endpoint is not on this machine: ~a" ep)))]
               [else
                (void (thread
                       (lambda ()
                         (with-handlers ([exn:fail? (lambda (e) (emit! (hasheq 'type "auto_title" 'path p 'error (exn-message e))))])
                           (define model (let ([m (jget req 'model)])
                                           (cond [(and (string? m) (not (string=? m ""))) m]
                                                 [else (define r (fetch-models ep))
                                                       (or (and r (pair? (cdr r)) (cadr r)) (and r (pair? (car r)) (caar r))
                                                           (error 'auto_title "no model loaded at ~a" ep))])))
                           (define texts (for/list ([r (in-list (records-of-type p 'msg))] #:when (and (rec? r) (message? (rec-data r)))
                                                    #:when (memq (message-role (rec-data r)) '(user assistant)))
                                           (define txt (message-text (rec-data r)))
                                           (format "~a: ~a" (message-role (rec-data r)) (if (> (string-length txt) 300) (substring txt 0 300) txt))))
                           (define reply (complete-local* ep model
                                                          (list (text-msg 'system "Give this conversation a short title: at most 6 words, in the conversation's language. Reply with the title only.")
                                                                (text-msg 'user (string-join (take texts (min 6 (length texts))) "\n")))))
                           (define title (let* ([l (car (append (string-split (string-trim reply) "\n") (list "")))]
                                                [l (string-trim l #px"[\\s\"'“”「」《》]+")])
                                           (if (> (string-length l) 60) (substring l 0 60) l)))
                           (if (string=? title "")
                               (emit! (hasheq 'type "auto_title" 'path p 'error "empty title from the model"))
                               (async-channel-put req-ch (hasheq 'type "_set_auto_title" 'path p 'title title 'model model)))))))])]
            [("export_session")
             (define fmt (if (equal? (jget req 'format) "json") "json" "markdown"))
             (emit! (hasheq 'type "export" 'path p 'format fmt 'text (if (equal? fmt "json") (export-json p) (export-markdown p))))])
          (same)])]
      ;; ---- 聊天目录（匿名工作区）：配置、占用、立即清理
      [("get_chat_config" "chat_usage")
       (void (thread (lambda () (with-handlers ([exn:fail? (lambda (e) (emit-error! (exn-message e)))])
                                  (emit! (chat-usage->jsexpr (chat-usage-now sess) (load-chat-config)))))))
       (same)]
      [("set_chat_config")
       (define changes (for/hasheq ([k '(root max_total_gb max_chat_gb min_free_gb)] #:when (hash-has-key? req k))
                         (values k (let ([v (hash-ref req k)]) (if (eq? v 'null) #f v)))))
       (with-handlers ([exn:fail? (lambda (e) (emit-error! (format "set_chat_config: ~a" (exn-message e))))])
         (save-chat-config! changes)
         (emit! (hasheq 'type "ok" 'for "set_chat_config"))
         (void (thread (lambda () (with-handlers ([exn:fail? void]) (manage-chat-space! sess #:force? #t))))))
       (same)]
      [("chat_cleanup")
       (void (thread (lambda () (with-handlers ([exn:fail? (lambda (e) (emit-error! (exn-message e)))]) (manage-chat-space! sess #:force? #t)))))
       (same)]
      [("set_stream_checkpoint")
       ;; {"type":"set_stream_checkpoint","seconds":s | null}：流式文字每 s 秒落盘一次（null = 不落盘，完成后才写）
       (define v (jget req 'seconds))
       (set! checkpoint-s (and (real? v) (positive? v) v))
       (emit! (hasheq 'type "ok" 'for "set_stream_checkpoint" 'seconds (or checkpoint-s 'null)))
       (same)]
      [("blob")
       ;; {"type":"blob","sha":s,"offset":o?,"length":n?,"tag":t} → {"type":"blob_data","sha","offset","total","data":base64,"tag"}（每块最多 BLOB-CHUNK 字节）
       (define sha (jget req 'sha))
       (define off (let ([v (jget req 'offset)]) (if (exact-nonnegative-integer? v) v 0)))
       (define len (let ([v (jget req 'length)]) (if (exact-positive-integer? v) (min v BLOB-CHUNK) BLOB-CHUNK)))
       (define-values (bs total) (if (valid-sha? sha) (blob-read-range (blob-root-of (session-dir sess)) sha off len) (values #f #f)))
       (if bs
           (emit! (tagged req (hasheq 'type "blob_data" 'sha sha 'offset off 'total total 'data (bytes->string/latin-1 (base64-encode bs #"")))))
           (emit! (tagged req (hasheq 'type "blob_data" 'sha (if (string? sha) sha 'null) 'offset off 'error "no such blob"))))
       (same)]
      [("select_lines")
       ;; {"type":"select_lines","id":记录,"call_id":c,"tool":名,"summary":输入摘要,"max_lines":k,"model":m?,"endpoint":url?,"allow_remote":bool?,"tag":t}
       ;; → {"type":"lines","id","call_id","lines":[行号（1 起）…]} 或 {"type":"lines",…,"error":原因}（都带 tag）
       ;; 本机的小模型挑工具输出里最有信息的几行（界面折叠预览）。端点缺省本机 LM Studio；不是本机的端点要显式 allow_remote
       (define id (jget req 'id))
       (define cid (jget req 'call_id))
       (define k (let ([v (jget req 'max_lines)]) (if (exact-positive-integer? v) (min v 40) 6)))
       (define ep (let ([v (jget req 'endpoint)]) (if (and (string? v) (not (string=? v ""))) v DEFAULT-LOCAL-ENDPOINT)))
       (define (reply! fields) (emit! (tagged req (apply hasheq 'type "lines" 'id id 'call_id cid fields))))
       (define it (and (exact-nonnegative-integer? id) (let ([xs (items-of sess id (add1 id) 100000000)]) (and (pair? xs) (car xs)))))
       (define out (and it (for/first ([p (in-list (hash-ref it 'parts '()))] #:when (and (equal? (hash-ref p 'type #f) "tool_result") (equal? (hash-ref p 'call_id #f) cid)))
                             (hash-ref p 'content ""))))
       (cond
         [(not out) (reply! (list 'error "no such tool result"))]
         [(and (not (local-endpoint? ep)) (not (eq? (jget req 'allow_remote) #t))) (reply! (list 'error (format "endpoint is not on this machine: ~a" ep)))]
         [else
          (thread
           (lambda ()
             (with-handlers ([exn:fail? (lambda (e) (reply! (list 'error (exn-message e))))])
               (define model (let ([m (jget req 'model)])
                               (cond [(and (string? m) (not (string=? m ""))) m]
                                     [else (define r (fetch-models ep))         ; 没指定：用端点上已加载的第一个
                                           (or (and r (pair? (cdr r)) (cadr r)) (and r (pair? (car r)) (caar r))
                                               (error 'select_lines "no model loaded at ~a" ep))])))
               (reply! (list 'lines (select-lines (let ([t (jget req 'tool)]) (if (string? t) t "?")) (let ([t (jget req 'summary)]) (if (string? t) t ""))
                                                  out k #:endpoint ep #:model model #:complete complete-local*)
                             'model model)))))])
       (same)]
      [("shutdown") (emit! (hasheq 'type "bye")) (values st sess #f #t)]
      [else
       (emit-error! (if (eq? req 'bad) "unparsable request line" f"unknown request type: {type}"))
       (same)]
    ) ; end case
  ) ; end define dispatch
  ;; ---- 主循环
  (let loop ([st st0] [sess sess0] [pending '()] [job #f])
    (cond
      [job
       (sync (handle-evt (vector-ref job 0)
                         (lambda (r) (loop (finish-job! job r sess) sess pending #f)))
             (handle-evt req-ch
                         (lambda (req)
                           (cond
                             [(and (hash? req) (member (jget req 'type) CONCURRENT-TYPES))
                              (case (jget req 'type)
                                [("ping") (emit! (hasheq 'type "pong"))]
                                [("list_providers") (emit-providers! st)]
                                [("items" "item" "index_summary" "select_lines" "blob" "session_stats" "export_session" "chat_usage" "get_chat_config") (dispatch req st sess)]   ; 只读，不改状态
                                [else (list-sessions! sess)])
                              (loop st sess pending job)]
                             [else (loop st sess (append pending (list req)) job)]))))]
      [else
       (define req (if (pair? pending) (car pending) (async-channel-get req-ch)))
       (define rest (if (pair? pending) (cdr pending) '()))
       (cond
         [(eq? req 'eof) (shutdown! sess)]                ; R9：EOF 时已无轮在跑（轮结束后才轮到它）
         [else
          ;; 一个请求出错不能让整个内核退出：报错，状态不变，接着处理下一个
          (define-values (st* sess* job* quit?)
            (with-handlers ([exn:fail? (lambda (e)
                                         (emit-error! (format "~a failed: ~a" (if (hash? req) (jget req 'type) "request") (exn-message e)))
                                         (values st sess #f #f))])
              (dispatch req st sess)))
          (if quit? (shutdown! sess*) (loop st* sess* rest job*))])]))
) ; end define run-rpc!

;; ---------------------------------------------------------------- provide

(provide
 run-rpc!
 event->jsexpr
 history->jsexpr
) ; end provide
