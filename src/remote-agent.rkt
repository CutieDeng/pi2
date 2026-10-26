#lang tstring racket
;; remote-agent.rkt — 跨主机子 agent：在另一台主机上起一个一次性的 pi2 内核（RPC v2，经 ssh 的 stdin / stdout），交给它一个任务，取回最终回答。
;; 与守护进程（serve.rkt）分开：不进那台上用户正在用的会话，跑完就退出（会话文件留在远端，可追溯）。
;; 凭证（主机表 'credential，见 hosts.rkt）：
;;   "proxy"（缺省）：本机签发临时 token（key-proxy.rkt：一个模型、花费上限、有效期，结束即吊销），ssh -R 把远端一个端口转回本机转发，
;;                    远端内核经 use_endpoint 用「http://127.0.0.1:远端端口 + 上游路径」与 token；真 key 不出本机
;;   "share"：把父会话的端点与 key 经 use_endpoint 交给远端内核（只在它的内存里）
;;   "remote"：远端用自己配置的 key（不发 use_endpoint）
;; 权限：远端用主机表 'mode（缺省 auto）；它要询问的经 current-asker 转给本机用户（询问里工具名带主机前缀，如 lan-11:bash）。
;; 用量：turn_complete 的 usage → (values … usage …)，调用方并进父会话；proxy 模式另有本机转发的统计。

(require json racket/random (file "model.rkt") (file "permission.rkt") (file "key-proxy.rkt") (file "providers.rkt"))

(define SUB-PREFIX
  (string-append "You are a sub-agent running on host ~a, spawned by another agent to complete one focused task. "
                 "Use your tools on this host, then give a concise final answer (it is returned to the parent agent).\n\nTask:\n"))

;; 父会话的端点 → 远端用哪个供应商档案（线路格式要一致）：同端点的内置档案里，优先 Auto / 升级梯不生效的（模型固定）
(define (profile-for-endpoint ep)
  (define ps (filter (lambda (p) (equal? (provider-profile-endpoint p) ep)) BUILTIN-PROFILES))
  (cond [(null? ps) "openai"]
        [(findf (lambda (p) (not (equal? (provider-profile-name p) "deepseek"))) ps) => provider-profile-name]
        [else (provider-profile-name (car ps))])
) ; end define profile-for-endpoint

;; 远端端口：ssh -R 用；冲突时 ssh 以 ExitOnForwardFailure 失败，调用方换一个重试
(define (pick-remote-port) (+ 20000 (random 30000)))
(define current-pick-port (make-parameter pick-remote-port))     ; 测试：指定端口序列（造端口冲突）

;; → (values 最终文字 usage 结果 详情hash)
;;   结果：'ok | 'error | 'interrupted；详情：'host 'credential 'proxy（统计）'stderr 'remote_tools（远端调过的工具名）'session（远端会话文件）
;;   'items（结束后拉回的远端会话条目：RPC v3 items，存进本机父会话的 sub 记录）
;; ssh -R 的远端端口被占（ExitOnForwardFailure → ssh 退出，stderr 有 "remote port forwarding failed"）→ 换端口重试，最多 3 次
(define (run-remote-agent entry task #:parent-config cfg #:model [model #f] #:workdir [workdir #f]
                          #:budget [budget #f] #:timeout [timeout 1800] #:progress [progress void])
  (let retry ([n 1])
    (define-values (text u outcome detail)
      (run-remote-agent/once entry task #:parent-config cfg #:model model #:workdir workdir #:budget budget #:timeout timeout #:progress progress))
    (if (and (eq? outcome 'error) (< n 3)
             (ormap (lambda (l) (regexp-match? #rx"(?i:remote port forwarding failed|port forwarding failed)" l)) (hash-ref detail 'stderr '())))
        (retry (add1 n))
        (values text u outcome detail)))
) ; end define run-remote-agent

(define (run-remote-agent/once entry task #:parent-config cfg #:model [model #f] #:workdir [workdir #f]
                               #:budget [budget #f] #:timeout [timeout 1800] #:progress [progress void])
  (define id (hash-ref entry 'id "remote"))
  (define cred (hash-ref entry 'credential "proxy"))
  (define mode (hash-ref entry 'mode "auto"))
  (define wd (or workdir (hash-ref entry 'workdir #f)))
  (define the-model (or model (config-model cfg)))
  ;; ---- 凭证
  (define proxy? (equal? cred "proxy"))
  (define token (and proxy? (issue-grant! #:endpoint (config-endpoint cfg) #:key (config-api-key cfg) #:model the-model
                                          #:budget budget #:ttl (+ timeout 120) #:label (format "sub-agent on ~a" id))))
  (define lport (and proxy? (proxy-start!)))
  (define rport (and proxy? (hash-ref entry 'ssh #f) ((current-pick-port))))
  ;; ---- 传输：'command（测试 / 本机）或 ssh
  (define kernel-args (append (list "--rpc" "--mode" mode) (if wd (list "-C" wd) '())))
  (define argv
    (cond
      [(hash-ref entry 'command #f) => (lambda (c) (append c kernel-args))]
      [(hash-ref entry 'ssh #f)
       => (lambda (target)
            (append (list "ssh" "-T" "-o" "BatchMode=yes" "-o" "ExitOnForwardFailure=yes")
                    (if rport (list "-R" (format "127.0.0.1:~a:127.0.0.1:~a" rport lport)) '())
                    (list target (string-join (cons (hash-ref entry 'remote-cmd "racket -l pi2 --") (map shell-quote kernel-args)) " "))))]
      [else (error 'spawn_agent "host ~a has neither ssh nor command" id)]))
  (define exe (or (find-executable-path (car argv)) (string->path (car argv))))
  (define-values (proc from to err) (apply subprocess #f #f #f exe (cdr argv)))
  (define stderr-lines '())
  (define err-thread (thread (lambda () (let loop () (define l (read-line err 'any))
                                          (unless (eof-object? l) (set! stderr-lines (cons l stderr-lines)) (loop))))))
  (define (send! j) (with-handlers ([exn:fail? void]) (write-json j to) (newline to) (flush-output to)))
  (define deadline (+ (current-inexact-milliseconds) (* 1000.0 timeout)))
  (define (next-event)
    (define l (sync/timeout (max 0.0 (/ (- deadline (current-inexact-milliseconds)) 1000.0)) (read-line-evt from 'linefeed)))
    (cond [(not l) 'timeout]
          [(eof-object? l) 'eof]
          [else (or (with-handlers ([exn:fail? (lambda (_e) #f)]) (let ([j (string->jsexpr l)]) (and (hash? j) j))) (next-event))]))
  (define (wait-for type)
    (let loop ()
      (define j (next-event))
      (cond [(symbol? j) j]
            [(equal? (hash-ref j 'type #f) type) j]
            [(equal? (hash-ref j 'type #f) "error") (hash-ref j 'message "error")]
            [else (loop)])))
  (define last-text #f)
  (define tools '())
  (define session #f)
  (define (finish! outcome text u extra)
    (send! (hasheq 'type "shutdown"))
    (with-handlers ([exn:fail? void]) (close-output-port to))
    (unless (sync/timeout 10 proc) (subprocess-kill proc #t))
    (thread-wait err-thread)
    (define pstats (and token (revoke-grant! token)))
    (values text u outcome
            (hash-set* extra 'host id 'credential cred 'proxy (or pstats 'null) 'remote_tools (reverse tools) 'session (or session 'null)
                       'stderr (let ([ls (reverse stderr-lines)]) (if (> (length ls) 20) (take-right ls 20) ls)))))
  (with-handlers ([exn:break? (lambda (e)                                    ; 父轮被中断：远端也停下，token 吊销
                                (send! (hasheq 'type "interrupt"))
                                (finish! 'interrupted "" (usage 0 0) (hasheq))
                                (raise e))])
    (define ready (wait-for "ready"))
    (cond
      [(not (hash? ready))
       (finish! 'error (format "remote kernel on ~a did not start (~a)" id (if (string? ready) ready ready)) (usage 0 0) (hasheq))]
      [else
       (set! session (hash-ref ready 'session #f))
       ;; 凭证
       (define cred-err
         (cond
           [proxy?
            (send! (hasheq 'type "use_endpoint" 'provider (profile-for-endpoint (config-endpoint cfg))
                           'endpoint (grant-endpoint token (or rport lport)) 'api_key token 'model the-model))
            (let ([r (wait-for "ok")]) (and (not (hash? r)) r))]
           [(equal? cred "share")
            (send! (hasheq 'type "use_endpoint" 'provider (profile-for-endpoint (config-endpoint cfg))
                           'endpoint (config-endpoint cfg) 'api_key (or (config-api-key cfg) "none") 'model the-model))
            (let ([r (wait-for "ok")]) (and (not (hash? r)) r))]
           [else (when model (send! (hasheq 'type "set_model" 'model model)) (wait-for "ok")) #f]))
       (cond
         [cred-err (finish! 'error (format "remote kernel on ~a refused the credential: ~a" id cred-err) (usage 0 0) (hasheq))]
         [else
          (send! (hasheq 'type "prompt" 'text (string-append (format SUB-PREFIX id) task)))
          (let loop ()
            (define j (next-event))
            (cond
              [(eq? j 'timeout) (send! (hasheq 'type "interrupt")) (finish! 'error (format "timed out after ~as" timeout) (usage 0 0) (hasheq))]
              [(eq? j 'eof) (finish! 'error (format "connection to ~a closed: ~a" id (string-join (take (reverse stderr-lines) (min 3 (length stderr-lines))) " | ")) (usage 0 0) (hasheq))]
              [else
               (case (hash-ref j 'type #f)
                 [("message") (when (string? (hash-ref j 'text #f)) (set! last-text (hash-ref j 'text)))]
                 [("tool_start") (set! tools (cons (hash-ref j 'name "?") tools))
                                 (progress (format "~a: ~a" id (hash-ref j 'name "?")))]
                 [("permission_request")
                  ;; 转给本机用户：工具名带主机前缀
                  (define ask (current-asker))
                  (define decision
                    (cond
                      [(not ask) "no"]
                      [else
                       (define a (parameterize ([current-permission-request
                                                 (hasheq 'tool (format "~a:~a" id (hash-ref j 'tool "?"))
                                                         'level (let ([l (hash-ref j 'level #f)]) (if (string? l) (string->symbol l) 'dangerous))
                                                         'input (hash-ref j 'input (hasheq)))])
                                   (ask (format "[~a] ~a" id (hash-ref j 'prompt "")))))
                       (case a [(yes) "yes"] [(always) "always"] [else "no"])]))
                   (send! (hasheq 'type "permission" 'id (hash-ref j 'id) 'decision decision))]
                 [else (void)])
               (cond
                 [(equal? (hash-ref j 'type #f) "turn_complete")
                  (define u (hash-ref j 'usage (hasheq)))
                  (define outcome (if (equal? (hash-ref j 'outcome "ok") "ok") 'ok 'error))
                  ;; 决定 4：结束后把远端会话（投影成的条目）拉回一份；旧版远端（协议 < 3）没有 items → 空
                  (send! (hasheq 'type "items" 'from 0 'to 1000000 'preview_bytes 65536))
                  (define copy (let ([r (wait-for "items")]) (if (hash? r) (hash-ref r 'items '()) '())))
                  (finish! outcome (or last-text "") (usage (hash-ref u 'input 0) (hash-ref u 'output 0))
                           (hasheq 'cost_usd (hash-ref j 'cost_usd 'null) 'outcome (hash-ref j 'outcome "ok") 'items copy))]
                 [else (loop)])]))])]))
) ; end define run-remote-agent/once

;; 远端 shell 里的参数：单引号包起来
(define (shell-quote s) (string-append "'" (regexp-replace* #rx"'" s "'\\\\''") "'"))

(provide run-remote-agent profile-for-endpoint shell-quote current-pick-port)
