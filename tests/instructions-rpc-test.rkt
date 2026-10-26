#lang tstring racket
;; instructions-rpc-test.rkt — 指令分层经 run-rpc!（design-memory.md M1）：每轮换掉系统提示词的指令节（不叠加 main.rkt 启动时的那一节）；
;; 清单变了才记 instructions 记录；目录 catalog 的指令（set_catalog_view）插在全局与目录树之间；聊天会话没有目录树那层；session_stats 带清单

(require rackunit json racket/async-channel racket/file
         (file "../src/model.rkt") (file "../src/event.rkt") (file "../src/provider.rkt") (file "../src/tool.rkt")
         (file "../src/permission.rkt") (file "../src/loop.rkt") (file "../src/session.rkt") (file "../src/plugin.rkt")
         (file "../src/rpc.rkt") (file "../src/rktd.rkt") (file "../src/tools/builtin.rkt") (file "../src/resources.rkt"))

(define (put! p s) (make-directory* (let-values ([(d _n _x) (split-path p)]) d)) (call-with-output-file p (lambda (o) (write-string s o)) #:exists 'truncate))
(define dir (make-temporary-file "pi2-instr-rpc-~a" 'directory))
(define cfg-home (build-path dir "cfg"))
(void (putenv "PI_CONFIG_HOME" (path->string cfg-home)))
(define repo (build-path dir "repo"))
(define sub (build-path repo "sub"))
(make-directory* (build-path repo ".git")) (make-directory* sub)
(put! (build-path cfg-home "AGENTS.md") "GLOBAL-RULE")
(put! (build-path repo "AGENTS.md") "REPO-RULE")
(put! (build-path sub "AGENTS.md") "SUB-RULE")

(define seen (box '()))                                  ; 每次请求的系统提示词（新的在前）
(define prov (provider "s" (lambda (_msgs _t)
                             (set-box! seen (cons (config-system-prompt (current-config)) (unbox seen)))
                             (define ch (make-async-channel))
                             (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant "ok")))
                             (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 1 1))) ch) void))

(define (instr-records p) (for/list ([r (in-datum-log p)] #:when (and (rec? r) (eq? (rec-type r) 'instructions))) (rec-data r)))
(define (order-ok? s . keys) (let ([ps (map (lambda (k) (let ([m (regexp-match-positions (regexp-quote k) s)]) (and m (caar m)))) keys)]) (and (andmap values ps) (apply < ps))))

(test-case "run-rpc!: layered instructions replace the startup section; recorded when they change; catalog layer; chat sessions skip the tree"
  ;; 启动时 main.rkt 追加过一节（旧格式，工作目录的单个文件）：每轮要换掉它，不叠加
  (define base "BASE-PROMPT")
  (define cfg (struct-copy config (default-config) [workdir (path->string sub)] [permission-mode 'yolo]
                           [system-prompt (string-append base (project-instructions-addendum "STARTUP-OLD" "/old/AGENTS.md"))]))
  (define path (fresh-session-path (path->string dir)))
  (define sess (session-open! path cfg))
  (define host (make-plugin-host))
  (define d (make-deps #:provider prov #:registry (make-registry (builtin-tools cfg)) #:bus (make-bus) #:policy (make-policy cfg) #:plugin-host host))
  (define-values (in-r in-w) (make-pipe)) (define-values (out-r out-w) (make-pipe))
  (thread (lambda () (parameterize ([current-input-port in-r] [current-output-port out-w])
                       (run-rpc! d (make-initial-state cfg) sess #:plugin-host host #:fetch-models (lambda (_e) #f)))
            (close-output-port out-w)))
  (define (send! j) (write-string (jsexpr->string j) in-w) (newline in-w) (flush-output in-w))
  (define (read-type type)
    (let loop () (define l (sync/timeout 10 (read-line-evt out-r 'linefeed)))
      (unless (string? l) (error 'read-type "timeout waiting for ~a" type))
      (define j (string->jsexpr l)) (if (equal? (hash-ref j 'type #f) type) j (loop))))
  (read-type "session")
  (send! (hasheq 'type "prompt" 'text "one")) (read-type "turn_complete")
  (define s1 (car (unbox seen)))
  (check-true (string-prefix? s1 base))
  (check-false (string-contains? s1 "STARTUP-OLD"))
  (check-true (order-ok? s1 "GLOBAL-RULE" "REPO-RULE" "SUB-RULE"))
  (check-equal? (length (instr-records path)) 1)
  (check-equal? (map (lambda (s) (hash-ref s 'kind)) (hash-ref (car (instr-records path)) 'sources)) '("global" "dir" "dir"))
  ;; 没变：提示词逐字相同，不再记
  (send! (hasheq 'type "prompt" 'text "two")) (read-type "turn_complete")
  (check-equal? (car (unbox seen)) s1)
  (check-equal? (length (instr-records path)) 1)
  ;; 目录 catalog 的项目指令：插在全局与目录树之间
  (send! (hasheq 'type "set_catalog_view" 'projects '("Shop")
                 'instructions (list (hasheq 'id "p1" 'kind "project" 'name "Shop" 'text "PROJECT-RULE"))
                 'sessions (list (hasheq 'path (path->string path) 'projects '("Shop") 'instr '("p1")))))
  (send! (hasheq 'type "prompt" 'text "three")) (read-type "turn_complete")
  (define s3 (car (unbox seen)))
  (check-true (order-ok? s3 "GLOBAL-RULE" "PROJECT-RULE" "REPO-RULE" "SUB-RULE"))
  (check-equal? (length (instr-records path)) 2)
  ;; 文件改了：下一轮生效
  (put! (build-path sub "AGENTS.md") "SUB-RULE-V2")
  (send! (hasheq 'type "prompt" 'text "four")) (read-type "turn_complete")
  (check-true (string-contains? (car (unbox seen)) "SUB-RULE-V2"))
  (check-equal? (length (instr-records path)) 3)
  ;; 信息面板：session_stats 带最后一次注入的清单
  (send! (hasheq 'type "session_stats" 'path (path->string path)))
  (define st (read-type "session_stats"))
  (check-equal? (map (lambda (s) (hash-ref s 'label)) (hash-ref st 'instructions))
                (list (path->string (simplify-path (build-path cfg-home "AGENTS.md"))) "Shop"
                      (path->string (simplify-path (build-path repo "AGENTS.md"))) (path->string (simplify-path (build-path sub "AGENTS.md")))))
  ;; 聊天会话：只有全局（没有目录树；这个新会话在 catalog 视图里也没有项目）
  (send! (hasheq 'type "new_session" 'chat #t)) (read-type "session")
  (send! (hasheq 'type "prompt" 'text "chat")) (read-type "turn_complete")
  (define s5 (car (unbox seen)))
  (check-true (string-contains? s5 "GLOBAL-RULE"))
  (check-false (string-contains? s5 "REPO-RULE"))
  (check-false (string-contains? s5 "PROJECT-RULE"))
  (send! (hasheq 'type "shutdown")) (void (read-type "bye")))

(delete-directory/files dir)
