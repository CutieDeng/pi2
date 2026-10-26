#lang tstring racket
;; host-index-test.rkt — 项目索引（design-memory.md §8）内核这边：找仓库（隐藏目录 / node_modules 跳过、嵌套的也找）、指纹、描述输入（只读白名单文件）、
;; 解析模型回答、中英词法查找；经 run-rpc!：host_scan / describe_project 事件；host_projects 工具：本机模型缺省能用、云端缺省不能、会话开关

(require rackunit json racket/async-channel racket/file
         (file "../src/model.rkt") (file "../src/event.rkt") (file "../src/provider.rkt") (file "../src/tool.rkt")
         (file "../src/permission.rkt") (file "../src/loop.rkt") (file "../src/session.rkt") (file "../src/plugin.rkt")
         (file "../src/rpc.rkt") (file "../src/rktd.rkt") (file "../src/tools/builtin.rkt") (file "../src/host-index.rkt"))

(define (put! p s) (make-directory* (let-values ([(d _n _x) (split-path p)]) d)) (call-with-output-file p (lambda (o) (write-string s o)) #:exists 'truncate))
(define tmp (make-temporary-file "pi2-hix-~a" 'directory))
(define (git! dir . args) (apply system* (find-executable-path "git") "-C" (path->string dir) args))
(define (mk-repo! rel readme)
  (define d (build-path tmp rel))
  (make-directory* d)
  (parameterize ([current-output-port (open-output-nowhere)] [current-error-port (open-output-nowhere)])
    (git! d "init" "-q") (git! d "config" "user.email" "t@t") (git! d "config" "user.name" "t")
    (put! (build-path d "README.md") readme)
    (git! d "add" "-A") (git! d "commit" "-q" "-m" "first commit"))
  (path->string (simplify-path d)))

(define shop (mk-repo! "work/shop" "# Shop\nAn online shop backend in Go. Prices in cents."))
(define nested (mk-repo! "work/shop/vendor/lib" "# lib\nA vendored library."))
(define hidden (mk-repo! ".cache/tool" "# cache repo"))
(define nm (mk-repo! "work/web/node_modules/pkg" "# pkg"))
(put! (build-path tmp "work" "shop" ".env") "SECRET_TOKEN=abc123")
(put! (build-path tmp "work" "shop" "go.mod") "module example.com/shop\n")

(test-case "discover: repos under the roots; nested ones too; hidden folders and node_modules skipped"
  (check-equal? (discover-repos (list (path->string tmp))) (sort (list shop nested) string<?)))

(test-case "fingerprint changes when the README changes; describe input reads only the allow-listed files"
  (define f0 (repo-fingerprint shop))
  (sleep 1.1)
  (put! (build-path shop "README.md") "# Shop\nAn online shop backend in Go. Prices in cents. Now with refunds.")
  (check-not-equal? (repo-fingerprint shop) f0)
  (define in (describe-input shop))
  (check-true (string-contains? in "=== README.md"))
  (check-true (string-contains? in "=== go.mod"))
  (check-true (string-contains? in "first commit"))
  (check-true (string-contains? in "=== tree"))
  (check-false (string-contains? in "SECRET_TOKEN")))

(test-case "parse the model's reply: fenced JSON, caps, junk → #f"
  (define d (parse-description "```json\n{\"short\": [\"go\", \"Go语言\", \"\"], \"long\": [\"Online shop backend\", \"在线商店后端\"], \"names\": [\"shop\"], \"summary_en\": \"A shop.\", \"summary_zh\": \"一个商店。\"}\n```"))
  (check-equal? (hash-ref d 'short) '("go" "Go语言"))
  (check-equal? (hash-ref d 'summary_zh) "一个商店。")
  ;; 成对的、对象的（本机 gemma 实测会回成对的）
  (define d2 (parse-description "{\"short\": [[\"market simulation\", \"市场模拟\"], [\"order book\", \"订单簿\"]], \"long\": [{\"en\": \"A simulator.\", \"zh\": \"一个模拟器。\"}], \"summary_en\": \"x\"}"))
  (check-equal? (hash-ref d2 'short) '("market simulation" "市场模拟" "order book" "订单簿"))
  (check-equal? (hash-ref d2 'long) '("A simulator." "一个模拟器。"))
  (check-false (parse-description "sorry, I cannot"))
  (check-false (parse-description "{\"short\": []}")))

(define PROJECTS
  (list (hasheq 'dir "/w/shop" 'name "shop" 'remote "git@x:me/shop.git" 'mtime 200
                'desc (hasheq 'short '("e-commerce" "电商" "go" "Go语言") 'long '("Online shop backend" "在线商店后端") 'names '("shop")
                              'summary_en "A shop backend." 'summary_zh "一个商店后端。"))
        (hasheq 'dir "/w/pi2" 'name "pi2" 'remote 'null 'mtime 300
                'desc (hasheq 'short '("LLM agent" "大模型智能体" "Racket") 'long '() 'names '("pi++") 'summary_en "An agent." 'summary_zh "一个智能体。"))
        (hasheq 'dir "/w/notes" 'name "notes" 'remote 'null 'mtime 100 'desc 'null)))

(test-case "search: Chinese query finds the project described in both languages; no query = most recent first; undescribed ones still listed"
  (check-equal? (map (lambda (p) (hash-ref p 'name)) (search-projects PROJECTS "电商项目在哪")) '("shop"))
  (check-equal? (map (lambda (p) (hash-ref p 'name)) (search-projects PROJECTS "pi++ agent")) '("pi2"))
  (check-equal? (map (lambda (p) (hash-ref p 'name)) (search-projects PROJECTS #f)) '("pi2" "shop" "notes"))
  (check-regexp-match #rx"not described yet" (fmt-project (caddr PROJECTS))))

;; ---------------------------------------------------------------- 经 run-rpc!
(void (putenv "PI_CONFIG_HOME" (path->string (build-path tmp "cfg"))))
(define script (box '()))
(define last-result (box #f))
(define prov (provider "s" (lambda (msgs _t)
                             (define lastm (last msgs))
                             (define tr (for/first ([b (in-list (message-blocks lastm))] #:when (tool-result-block? b)) b))
                             (when tr (set-box! last-result (format "~a" (tool-result-block-content tr))))
                             (define ch (make-async-channel))
                             (define call (and (not tr) (pair? (unbox script)) (car (unbox script))))
                             (cond
                               [call (set-box! script (cdr (unbox script)))
                                     (async-channel-put ch (evt:message (now-ms) (message 'assistant (list (tool-use-block "c1" (car call) (cdr call))))))
                                     (async-channel-put ch (evt:turn-end (now-ms) "tool_calls" (usage 1 1)))]
                               [else (async-channel-put ch (evt:message (now-ms) (text-msg 'assistant "ok")))
                                     (async-channel-put ch (evt:turn-end (now-ms) "stop" (usage 1 1)))])
                             ch) void))
(define described (box #f))                              ; describe_project 收到的输入

(define (start! endpoint)
  (define cfg (struct-copy config (default-config) [workdir (path->string tmp)] [permission-mode 'normal] [endpoint endpoint]))
  (define path (fresh-session-path (path->string (build-path tmp "sessions"))))
  (make-directory* (build-path tmp "sessions"))
  (define sess (session-open! path cfg))
  (define host (make-plugin-host))
  (define d (make-deps #:provider prov #:registry (make-registry (builtin-tools cfg)) #:bus (make-bus) #:policy (make-policy cfg) #:plugin-host host))
  (define-values (in-r in-w) (make-pipe)) (define-values (out-r out-w) (make-pipe))
  (thread (lambda () (parameterize ([current-input-port in-r] [current-output-port out-w])
                       (run-rpc! d (make-initial-state cfg) sess #:plugin-host host #:fetch-models (lambda (_e) (cons '("m-local") '("m-local")))
                                 #:complete-long (lambda (ep model msgs) (set-box! described (message-text (car msgs)))
                                                   "{\"short\": [\"go\", \"Go语言\"], \"long\": [\"Online shop\"], \"names\": [\"shop\"], \"summary_en\": \"A shop.\", \"summary_zh\": \"商店。\"}")))
            (close-output-port out-w)))
  (define (send! j) (write-string (jsexpr->string j) in-w) (newline in-w) (flush-output in-w))
  (define (read-type type)
    (let loop () (define l (sync/timeout 20 (read-line-evt out-r 'linefeed)))
      (unless (string? l) (error 'read-type "timeout waiting for ~a" type))
      (define j (string->jsexpr l)) (if (equal? (hash-ref j 'type #f) type) j (loop))))
  (read-type "session")
  (values (path->string path) send! read-type))

(test-case "run-rpc!: host_scan → repos; describe_project → description from the model (local endpoint); describe_input; non-repo refused"
  (define-values (path send! read-type) (start! "http://localhost:1234/v1"))
  (send! (hasheq 'type "host_scan" 'roots (list (path->string tmp))))
  (define hr (read-type "host_repos"))
  (check-equal? (map (lambda (r) (hash-ref r 'name)) (hash-ref hr 'repos)) '("shop" "lib"))
  (check-true (string? (hash-ref (car (hash-ref hr 'repos)) 'fingerprint)))
  (send! (hasheq 'type "describe_project" 'dir shop))
  (define pd (read-type "project_described"))
  (check-equal? (hash-ref (hash-ref pd 'desc) 'short) '("go" "Go语言"))
  (check-equal? (hash-ref pd 'model) "m-local")
  (check-true (string-contains? (unbox described) "Now with refunds"))
  (send! (hasheq 'type "describe_input" 'dir shop))
  (check-true (string-contains? (hash-ref (read-type "describe_input") 'input) "=== go.mod"))
  (send! (hasheq 'type "describe_project" 'dir "/w/remote/repo" 'input "INPUT FROM ANOTHER HOST" 'fingerprint "fp1" 'host "srv"))
  (define pd2 (read-type "project_described"))
  (check-equal? (list (hash-ref pd2 'fingerprint) (hash-ref pd2 'host)) '("fp1" "srv"))
  (check-true (string-contains? (unbox described) "INPUT FROM ANOTHER HOST"))
  (send! (hasheq 'type "describe_input" 'dir (path->string tmp)))
  (check-regexp-match #rx"not a git repository" (hash-ref (read-type "describe_input") 'error))
  (send! (hasheq 'type "describe_project" 'dir shop 'endpoint "https://api.example.com/v1"))
  (check-regexp-match #rx"not on this machine" (hash-ref (read-type "project_described") 'error))
  ;; 工具：本机模型 → 缺省能用
  (send! (hasheq 'type "set_catalog_view" 'projects '() 'host_projects PROJECTS 'sessions (list (hasheq 'path path))))
  (set-box! script (list (cons "host_projects" (hasheq 'query "电商"))))
  (send! (hasheq 'type "prompt" 'text "where")) (read-type "turn_complete")
  (check-regexp-match #rx"^1 of 3 projects\nshop  /w/shop" (unbox last-result))
  ;; 会话关掉 → 不能用
  (send! (hasheq 'type "set_catalog_view" 'projects '() 'host_projects PROJECTS 'sessions (list (hasheq 'path path 'host_index #f))))
  (set-box! script (list (cons "host_projects" (hasheq))))
  (send! (hasheq 'type "prompt" 'text "where2")) (read-type "turn_complete")
  (check-regexp-match #rx"turned off for this session" (unbox last-result))
  (send! (hasheq 'type "shutdown")) (void (read-type "bye")))

(test-case "run-rpc!: a cloud endpoint cannot use host_projects by default; the session switch turns it on"
  (define-values (path send! read-type) (start! "https://api.deepseek.com/v1"))
  (send! (hasheq 'type "set_catalog_view" 'projects '() 'host_projects PROJECTS 'sessions (list (hasheq 'path path))))
  (set-box! script (list (cons "host_projects" (hasheq))))
  (send! (hasheq 'type "prompt" 'text "where")) (read-type "turn_complete")
  (check-regexp-match #rx"off for cloud models by default" (unbox last-result))
  (send! (hasheq 'type "set_catalog_view" 'projects '() 'host_projects PROJECTS 'sessions (list (hasheq 'path path 'host_index #t))))
  (set-box! script (list (cons "host_projects" (hasheq))))
  (send! (hasheq 'type "prompt" 'text "where2")) (read-type "turn_complete")
  (check-regexp-match #rx"^3 of 3 projects\npi2" (unbox last-result))
  (send! (hasheq 'type "shutdown")) (void (read-type "bye")))

(delete-directory/files tmp)
