#lang tstring racket
;; hosts.rkt — 主机表（内核侧）：{config-home}/hosts.rktd，一个 hash 的表；GUI 设置页「主机」写同一个文件（compute/hosts-file.rkt）。
;; 每台：'id 名字（spawn_agent 的 host 参数、GUI 侧栏用）
;;       'ssh ssh 目标（ssh 配置里的别名或 user@host）或 'command 参数表（测试：本机直接起「远端」内核）
;;       'serve GUI 连守护进程时的 serve 名字（缺省 default）
;;       'workdir 远端子 agent 的工作目录（缺省远端家目录）
;;       'credential 远端子 agent 的模型凭证："proxy"（缺省：本机签发临时 token，经 ssh 反向隧道回本机转发，真 key 不出本机）
;;                   | "share"（把父会话的 key 临时交给远端内核，只在它的内存里）| "remote"（用远端自己配置的 key）
;;       'mode 远端子 agent 的权限模式（缺省 "auto"；需要询问的转给本机用户）
;;       'remote-cmd 远端起内核的命令（缺省 "racket -l pi2 --"，后面接 --rpc …）

(require racket/file (file "credentials.rkt"))

(define (hosts-path) (build-path (config-home) "hosts.rktd"))

(define (load-hosts)
  (with-handlers ([exn:fail? (lambda (_e) '())])
    (define v (if (file-exists? (hosts-path))
                  (call-with-input-file (hosts-path) (lambda (in) (parameterize ([read-accept-reader #f]) (read in))))
                  '()))
    (if (list? v) (filter (lambda (h) (and (hash? h) (string? (hash-ref h 'id #f)))) v) '()))
) ; end define load-hosts

(define (host-entry id) (findf (lambda (h) (equal? (hash-ref h 'id) id)) (load-hosts)))

(define (save-hosts! hs)
  (make-directory* (config-home))
  (define tmp (path-add-extension (hosts-path) #".tmp"))
  (call-with-output-file tmp #:exists 'truncate (lambda (out) (write hs out) (newline out)))
  (rename-file-or-directory tmp (hosts-path) #t)
) ; end define save-hosts!

(provide load-hosts host-entry save-hosts! hosts-path)
