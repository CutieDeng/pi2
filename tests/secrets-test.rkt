#lang tstring racket
;; secrets-test.rkt — 密钥句柄：HMAC-SHA256 对照 RFC 4231；内核与 GUI 两份实现用同一盐值文件得到同一句柄；盐值文件 0600

(require rackunit racket/file file/sha1
         (prefix-in k: (file "../src/secrets.rkt"))
         (prefix-in g: (file "../src/gui/compute/secret-handle.rkt")))

(define dir (make-temporary-file "pi2-secrets-~a" 'directory))
(void (putenv "PI_CONFIG_HOME" (path->string dir)))

(test-case "HMAC-SHA256 matches RFC 4231 test case 2"
  (check-equal? (bytes->hex-string (k:hmac-sha256 #"Jefe" #"what do ya want for nothing?"))
                "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843")
  (check-equal? (g:hmac-sha256 #"Jefe" #"what do ya want for nothing?") (k:hmac-sha256 #"Jefe" #"what do ya want for nothing?")))

(test-case "kernel and GUI derive the same handle from the same salt; different values differ; salt file is 0600"
  (define h1 (k:secret-handle "sk-abc-123"))
  (check-equal? (g:secret-handle "sk-abc-123") h1)
  (check-regexp-match #px"^[0-9a-f]{8}$" h1)
  (check-not-equal? (k:secret-handle "sk-abc-124") h1)
  (check-equal? (k:secret-text "sk-abc-123") (string-append "⟦secret:" h1 "⟧"))
  (check-equal? (bitwise-and (file-or-directory-permissions (build-path dir "secret-salt") 'bits) #o777) #o600))

(test-case "a client-supplied handle (remote GUI, different salt) becomes an alias: lookup and redaction use it; a taken handle is refused"
  (define v "sk-remote-gui-key-0001")
  (define gui-h "0badc0de")                                      ; GUI 那边盐值不同算出的句柄
  (check-not-equal? (k:secret-handle v) gui-h)
  (check-equal? (k:register-secret! v "chat" #:handle gui-h) gui-h)
  (check-equal? (k:secret-value gui-h) v)
  (check-equal? (k:redact-secrets (string-append "token=" v)) (string-append "token=⟦secret:" gui-h "⟧"))
  ;; 已指向别的原值的句柄不能拿来用：按本机算
  (define v2 "sk-another-key-0002")
  (check-equal? (k:register-secret! v2 "chat" #:handle gui-h) (k:secret-handle v2))
  (check-equal? (k:secret-value gui-h) v)
  ;; 格式不对：按本机算
  (check-equal? (k:register-secret! "sk-third-key-00003" "chat" #:handle "not-hex!") (k:secret-handle "sk-third-key-00003")))

(delete-directory/files dir)
