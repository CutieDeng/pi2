#lang racket/base
;; gui-settings.rkt — GUI 自己的偏好（与内核无关）：{config-home}/gui.rktd，一个 hash。
;; config-home 与内核一致：PI_CONFIG_HOME > XDG_CONFIG_HOME/pi++ > ~/.config/pi++。
;; 键：'lang（'system | 'zh | 'en）、'secret-mode（'off | 'suggest | 'auto）、'zoom（ZOOM-LEVELS 之一）、'combined-daily-usd、'combined-snooze-date。
;; 另：secret-patterns.rktd（自定义密钥格式）。

(require racket/file)

;; 第一次用到时定下来、之后不变：内核 place 与 GUI 同一进程，place 里的 putenv（脚本化内核会改 PI_CONFIG_HOME）是进程级的，
;; 不定下来的话 GUI 之后会读写别的目录（GUI 启动时先读语言设置，所以定下来的是启动时的环境）
(define home-cache #f)
(define (gui-config-home)
  (unless home-cache
    (set! home-cache
          (cond [(getenv "PI_CONFIG_HOME") => string->path]
                [(getenv "XDG_CONFIG_HOME") => (lambda (x) (build-path x "pi++"))]
                [else (build-path (find-system-path 'home-dir) ".config" "pi++")])))
  home-cache
) ; end define gui-config-home

(define (settings-path) (build-path (gui-config-home) "gui.rktd"))

(define (load-gui-settings)
  (with-handlers ([exn:fail? (lambda (_e) (hasheq))])
    (define v (if (file-exists? (settings-path))
                  (call-with-input-file (settings-path) (lambda (in) (parameterize ([read-accept-reader #f]) (read in))))
                  (hasheq)))
    (if (hash? v) v (hasheq)))
) ; end define load-gui-settings

(define (save-gui-setting! key value)
  (with-handlers ([exn:fail? (lambda (e) (eprintf "pi2 GUI: cannot save settings: ~a\n" (exn-message e)))])
    (make-directory* (gui-config-home))
    (define h (hash-set (load-gui-settings) key value))
    (define tmp (path-add-extension (settings-path) #".tmp"))
    (call-with-output-file tmp #:exists 'truncate (lambda (out) (write h out)))
    (rename-file-or-directory tmp (settings-path) #t))
) ; end define save-gui-setting!

(provide gui-config-home load-gui-settings save-gui-setting! ZOOM-LEVELS load-secret-patterns save-secret-patterns!)

;; 界面缩放的档位（设置页按钮、⌘= / ⌘- 步进）
(define ZOOM-LEVELS '(0.8 0.9 1.0 1.1 1.25 1.5 1.75))

;; 自定义密钥格式：{config-home}/secret-patterns.rktd，正则字符串的表（pregexp；与内置格式一起用于聊天里粘贴的识别）
(define (patterns-path) (build-path (gui-config-home) "secret-patterns.rktd"))
(define (load-secret-patterns)
  (with-handlers ([exn:fail? (lambda (_e) '())])
    (define v (if (file-exists? (patterns-path))
                  (call-with-input-file (patterns-path) (lambda (in) (parameterize ([read-accept-reader #f]) (read in))))
                  '()))
    (if (list? v) (filter (lambda (p) (and (string? p) (with-handlers ([exn:fail? (lambda (_e) #f)]) (pregexp p)))) v) '()))
) ; end define load-secret-patterns
(define (save-secret-patterns! ps)
  (with-handlers ([exn:fail? (lambda (e) (eprintf "pi2 GUI: cannot save secret patterns: ~a\n" (exn-message e)))])
    (make-directory* (gui-config-home))
    (define tmp (path-add-extension (patterns-path) #".tmp"))
    (call-with-output-file tmp #:exists 'truncate (lambda (out) (write ps out) (newline out)))
    (rename-file-or-directory tmp (patterns-path) #t))
) ; end define save-secret-patterns!
