#lang tstring racket
;; racket-eval-test.rkt — racket_eval：沙箱里跑模型写的 Racket；密钥只以句柄进沙箱，原值只在宿主侧组 HTTP 请求时取；
;; 输出里出现的原值被换成句柄；沙箱拿不到原值（打印、拆结构、getenv、unsafe、子进程、写文件、直接联网都试过）。

(require
 rackunit
 json
 racket/tcp
 racket/file
 (file "../src/tool.rkt")
 (file "../src/credentials.rkt")
 (file "../src/secrets.rkt")
 (file "../src/tools/racket-eval.rkt")
) ; end require

(define cfg-dir (make-temporary-file "pi2-reval-~a" 'directory))
(void (putenv "PI_CONFIG_HOME" (path->string cfg-dir)))
(define work (make-temporary-file "pi2-reval-ws-~a" 'directory))
(define KEY "sk-real-value-0123456789")
(store-instance-key! "deepseek" "work" KEY)
(void (putenv "TEST_API_KEY" "sk-env-value-9876543210"))
(define H (secret-handle KEY))

;; 假服务器：把收到的 Authorization 头原样放进响应体（模拟「接口回显了 key」的最坏情况）
(define seen-auth (box #f))
(define listener (tcp-listen 0 8 #t "127.0.0.1"))
(define port (let-values ([(_a p _b _c) (tcp-addresses listener #t)]) p))
(void (thread (lambda ()
                (let loop ()
                  (define-values (in out) (tcp-accept listener))
                  (thread (lambda ()
                            (let hdrs ([auth #f])
                              (define l (read-line in 'return-linefeed))
                              (cond [(or (eof-object? l) (string=? l ""))
                                     (set-box! seen-auth auth)
                                     (define body (jsexpr->string (hasheq 'echo (or auth ""))))
                                     (fprintf out "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: ~a\r\nConnection: close\r\n\r\n~a"
                                              (bytes-length (string->bytes/utf-8 body)) body)
                                     (close-output-port out) (close-input-port in)]
                                    [(regexp-match #rx"(?i:^authorization: (.*))$" l) => (lambda (m) (hdrs (cadr m)))]
                                    [else (hdrs auth)]))))
                  (loop)))))

(define (run code) (call-with-values (lambda () (run-racket code (path->string work))) list))

(test-case "plain evaluation: defines, stdout, values"
  (check-equal? (run "(define x 20) (display \"hi\") (+ x 1)") '(#t "stdout:\nhi\n21")))

(test-case "HTTP with a secret: the server gets the real key; the model only ever sees the handle"
  (define EH (secret-handle "sk-env-value-9876543210"))                         ; 环境变量里的 key（不绑定主机，可发本地假服务器）
  (define code (format "(define r (http-request \"GET\" \"http://127.0.0.1:~a/v1/me\" #:headers (list (cons \"Authorization\" (bearer (secret \"~a\"))))))
                        (list (hash-ref r 'status) (hash-ref r 'body))" port EH))
  (match-define (list ok? text) (run code))
  (check-true ok? text)
  (check-equal? (unbox seen-auth) "Bearer sk-env-value-9876543210")             ; 宿主侧取了原值发出去
  (check-false (regexp-match? #rx"sk-env-value" text) text)                    ; 接口回显的原值被换成句柄
  (check-true (string-contains? text (string-append "⟦secret:" EH "⟧")) text)
  (check-regexp-match (pregexp (string-append "http GET http://127.0.0.1/v1/me \\(secrets: " EH "\\)")) text)   ; 审计行
  (match-define (list okj? tj) (run (format "(hash-ref (hash-ref (http-get \"http://127.0.0.1:~a/v1/me\") 'json) 'echo)" port)))
  (check-true okj? tj))                                                          ; 'json：正文已解析

(test-case "the sandbox cannot obtain the plaintext"
  (define (blocked? code) (match-let ([(list ok? text) (run code)]) (and (not (regexp-match? #rx"sk-real-value|sk-env-value" text)) text)))
  (check-equal? (cadr (run (format "(format \"~~s\" (secret \"~a\"))" H))) (format "~s" (format "#<secret ~a>" H)))
  (check-not-false (blocked? (format "(struct->vector (secret \"~a\"))" H)))
  (check-equal? (cadr (run "(getenv \"TEST_API_KEY\")")) "#f")                   ; 环境变量为空
  (check-regexp-match #rx"protected" (cadr (run (format "(require racket/unsafe/ops) (unsafe-struct-ref (secret \"~a\") 0)" H))))
  (check-regexp-match #rx"access denied" (cadr (run "(require racket/system) (system \"echo hi\")")))
  (check-regexp-match #rx"access denied" (cadr (run "(with-output-to-file \"x.txt\" (lambda () (display 1)))")))
  (check-regexp-match #rx"direct network" (cadr (run "(require racket/tcp) (tcp-connect \"127.0.0.1\" 80)")))
  (check-regexp-match #rx"access denied" (cadr (run "(require racket/file) (file->string (build-path (find-system-path 'home-dir) \".zshrc\"))"))))

(test-case "unknown handle errors and lists what exists; files in the workdir are readable and redacted on output"
  (match-define (list ok? text) (run "(secret \"deadbeef\")"))
  (check-false ok?)
  (check-regexp-match #rx"unknown secret handle deadbeef" text)
  (with-output-to-file (build-path work "notes.txt") (lambda () (display (string-append "token=" KEY))))
  (match-define (list ok2? text2) (run "(require racket/file) (file->string \"notes.txt\")"))
  (check-true ok2?)
  (check-false (regexp-match? #rx"sk-real-value" text2) text2))

(test-case "a provider account's key can only go to that provider's API host"
  (set-box! seen-auth #f)
  (match-define (list ok? text)
    (run (format "(http-request \"GET\" \"http://127.0.0.1:~a/steal\" #:headers (list (cons \"X\" (secret \"~a\"))))" port H)))
  (check-false ok?)
  (check-regexp-match #rx"may only be sent to api.deepseek.com" text)
  (check-false (unbox seen-auth))                                              ; 请求根本没发出去
  ;; 环境变量里的 key 没有绑定主机：可以发（由权限卡片把关）
  (define EH (secret-handle "sk-env-value-9876543210"))
  (match-define (list ok2? _t2)
    (run (format "(hash-ref (http-request \"GET\" \"http://127.0.0.1:~a/x\" #:headers (list (cons \"Authorization\" (bearer (secret \"~a\"))))) 'status)" port EH)))
  (check-true ok2?))

(test-case "the tool description lists handles and labels, never values"
  (define spec (tool-spec (make-racket-eval-tool)))
  (define desc (hash-ref (hash-ref spec 'function) 'description))
  (check-true (string-contains? desc (format "~a = provider deepseek[work]" H)) desc)
  (check-true (string-contains? desc "env TEST_API_KEY") desc)
  (check-false (regexp-match? #rx"sk-real-value|sk-env-value" desc)))

(tcp-close listener)
(delete-directory/files cfg-dir)
(delete-directory/files work)
