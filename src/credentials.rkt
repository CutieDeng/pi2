#lang tstring racket
;; credentials.rkt — token / API 密钥的存储与安全管理（非侵入，独立于内核）。
;;
;; 设计原则（慎重、最小暴露面）：
;;   * 解析优先级：进程环境变量 > 凭据文件。故 CI / 一次性覆盖仍走 env，零改动。
;;   * 凭据文件 {config-home}/credentials.rktd，键名 = 环境变量名
;;     （与 provider-profile 的 key-env 对齐，如 "ANTHROPIC_API_KEY"）。
;;   * 文件权限强制 0600（仅属主可读写）；载入时若权限过宽（组/他人可读）→ stderr 警告一次。
;;   * 密钥绝不整串落日志：对外一律 mask-key 掩码（sk-…abcd）。
;;   * 写入经由「临时文件 + rename」原子落盘，避免半写导致的凭据损坏。
;;   * 本模块只依赖 racket 基础库——可被 providers.rkt 直接 require，无循环依赖。
;;
;; 与 OS keychain 的关系：见 design-credentials-billing.md。此文件仓储是可移植的
;; 默认后端；keychain（macOS security / libsecret）作为后续可插拔后端，接口即
;; resolve-key / store-key!，替换实现即可，调用方不变。

(require
 racket/file
 racket/string
 racket/path
) ; end require

;; ---------------------------------------------------------------- 配置目录

;; 配置根：PI_CONFIG_HOME > XDG_CONFIG_HOME/pi++ > ~/.config/pi++。
;; （测试用 PI_CONFIG_HOME 指向临时目录以隔离。）
(define (config-home)
  (cond
    [(let ([p (getenv "PI_CONFIG_HOME")]) (and p (non-empty-string? p) p))
     => (lambda (p) (string->path p))]
    [(let ([p (getenv "XDG_CONFIG_HOME")]) (and p (non-empty-string? p) p))
     => (lambda (p) (build-path p "pi++"))]
    [else (build-path (find-system-path 'home-dir) ".config" "pi++")]
  ) ; end cond
) ; end define config-home

(define (credentials-path) (build-path (config-home) "credentials.rktd"))

;; ---------------------------------------------------------------- 权限检查

(define warned-perms (make-hash))       ; 每路径只警告一次，避免刷屏

;; unix 下若组/他人有任一权限位（0o077）→ 警告。非 unix（无位概念）跳过。
(define (check-perms! path)
  (with-handlers ([exn:fail? (lambda (_e) (void))])   ; 平台不支持 bits → 忽略
    (define bits (file-or-directory-permissions path 'bits))
    (when (and (positive? (bitwise-and bits #o077))
               (not (hash-ref warned-perms (path->string path) #f)))
      (hash-set! warned-perms (path->string path) #t)
      (eprintf "warning: ~a is readable by others (mode ~a); run: chmod 600 ~a\n"
               path (number->string bits 8) path))
  ) ; end with-handlers
) ; end define check-perms!

;; ---------------------------------------------------------------- 载入 / 写入

;; 读凭据文件 → (immutable hash string->string)；不存在或损坏 → 空 hash。
(define (load-credentials)
  (define path (credentials-path))
  (cond
    [(not (file-exists? path)) (hash)]
    [else
     (check-perms! path)
     (with-handlers ([exn:fail? (lambda (_e) (hash))])
       (define v (call-with-input-file path read))
       (if (hash? v)
           ;; 规整为不可变 string->string
           (for/hash ([(k val) (in-hash v)]
                      #:when (and (string? k) (string? val)))
             (values k val))
           (hash)))]
  ) ; end cond
) ; end define load-credentials

;; 原子写：临时文件 → 权限 0600 → rename 覆盖。目录以 0700 建立。
(define (write-credentials! h)
  (define path (credentials-path))
  (define dir (path-only path))
  (make-directory* dir)
  (with-handlers ([exn:fail? (lambda (_e) (void))])
    (file-or-directory-permissions dir #o700))
  (define tmp (build-path dir (string-append (path->string (file-name-from-path path)) ".tmp")))
  (call-with-output-file tmp #:exists 'replace
    (lambda (out) (write h out) (newline out)))
  (file-or-directory-permissions tmp #o600)
  (rename-file-or-directory tmp path #t)
) ; end define write-credentials!

;; ---------------------------------------------------------------- 公共 API

;; 解析某 env 名的密钥：环境变量优先，其次凭据文件。均无 → #f。
(define (resolve-key name)
  (define ev (getenv name))
  (cond
    [(and ev (non-empty-string? ev)) ev]
    [else (hash-ref (load-credentials) name #f)]
  ) ; end cond
) ; end define resolve-key

;; 该 env 名的解析来源：'env | 'file | #f（未配置）。用于 --list-keys 提示。
(define (key-source name)
  (define ev (getenv name))
  (cond
    [(and ev (non-empty-string? ev)) 'env]
    [(hash-ref (load-credentials) name #f) 'file]
    [else #f]
  ) ; end cond
) ; end define key-source

;; 写入/更新一条密钥到凭据文件（env 覆盖仍优先，见 resolve-key）。
(define (store-key! name value)
  (write-credentials! (hash-set (load-credentials) name value))
) ; end define store-key!

;; 删除一条密钥。返回是否确有删除。
(define (delete-key! name)
  (define h (load-credentials))
  (cond
    [(hash-has-key? h name) (write-credentials! (hash-remove h name)) #t]
    [else #f]
  ) ; end cond
) ; end define delete-key!

;; 掩码：sk-…abcd。短串（≤8）全掩。用于任何面向用户/日志的展示。
(define (mask-key s)
  (cond
    [(not (string? s)) "—"]
    [(<= (string-length s) 8) (make-string (string-length s) #\•)]
    [else (string-append (substring s 0 3) "…" (substring s (- (string-length s) 4)))]
  ) ; end cond
) ; end define mask-key

;; 凭据文件中已存的键名列表（不含 env-only 的）。
(define (stored-key-names) (sort (hash-keys (load-credentials)) string<?))

;; ---------------------------------------------------------------- 供应商实例密钥
;; 同一 provider 允许多套 token，视作不同「实例」，用标签区分（默认 "default"）。
;; 实例密钥独立于「env 名」存储，键形如 "provider:deepseek:work"，不与环境变量名冲突。

(define INSTANCE-PREFIX "provider:")

(define (instance-cred-key base label)
  (string-append INSTANCE-PREFIX base ":" label)
) ; end define instance-cred-key

;; 读某实例的 token（仅查凭据文件；env 回退由上层 providers.rkt 结合 profile 的 key-env 处理）。
(define (resolve-instance-key base label)
  (hash-ref (load-credentials) (instance-cred-key base label) #f)
) ; end define resolve-instance-key

(define (store-instance-key! base label token)
  (write-credentials! (hash-set (load-credentials) (instance-cred-key base label) token))
) ; end define store-instance-key!

(define (delete-instance-key! base label)
  (define k (instance-cred-key base label))
  (define h (load-credentials))
  (cond
    [(hash-has-key? h k) (write-credentials! (hash-remove h k)) #t]
    [else #f]
  ) ; end cond
) ; end define delete-instance-key!

;; 某 base 已配置的标签列表（升序）。
(define (instance-labels-of base)
  (define pre (string-append INSTANCE-PREFIX base ":"))
  (sort
   (for/list ([k (in-list (hash-keys (load-credentials)))] #:when (string-prefix? k pre))
     (substring k (string-length pre)))
   string<?)
) ; end define instance-labels-of

;; 全部实例 (base . label)，供 --list-keys / /provider 列出。
(define (all-instances)
  (for/list ([k (in-list (sort (hash-keys (load-credentials)) string<?))]
             #:when (string-prefix? k INSTANCE-PREFIX))
    (define rest (substring k (string-length INSTANCE-PREFIX)))
    (define i (let ([m (regexp-match-positions #rx":" rest)]) (and m (caar m))))
    (if i (cons (substring rest 0 i) (substring rest (add1 i))) (cons rest "default")))
) ; end define all-instances

(provide
 config-home
 credentials-path
 resolve-key
 key-source
 store-key!
 delete-key!
 mask-key
 stored-key-names
 load-credentials
 instance-cred-key
 resolve-instance-key
 store-instance-key!
 delete-instance-key!
 instance-labels-of
 all-instances
) ; end provide
