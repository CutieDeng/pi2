#lang tstring racket
;; tui/terminal.rkt — 终端抽象层（design: TUI IO 层）
;; 把「读按键 / 写输出 / 尺寸 / 原始模式」抽象为函数表 struct。
;;   real-terminal   —— 真实 tty：stty raw、逐字节读 stdin、写 stdout
;;   scripted-terminal —— 脚本后端：预置输入、捕获输出，供自动化测试（CLI 式输入抽象）
;; 二者对上层（lineedit / tui）完全等价。

(require
 (file "keys.rkt")
) ; end require

(struct terminal
  (read-key   ; (-> kev)         阻塞读下一个按键事件
   write      ; (-> string void) 写输出
   size       ; (-> (values cols rows))
   raw-on!    ; (-> void)        进入原始模式
   raw-off!   ; (-> void)        恢复
   interactive? ; boolean         是否真实交互终端
  ) ; end fields
) ; end struct terminal

(define (term-read-key t) ((terminal-read-key t)))
(define (term-write t s) ((terminal-write t) s))
(define (term-write-all t . ss) ((terminal-write t) (apply string-append ss)))
(define (term-size t) ((terminal-size t)))
(define (term-raw-on! t) ((terminal-raw-on! t)))
(define (term-raw-off! t) ((terminal-raw-off! t)))
(define (term-interactive? t) (terminal-interactive? t))

;; ------------------------------------------------------------ 真实 tty

;; stty 通过子进程执行；raw 关闭行缓冲/回显/信号翻译，逐字节即时到达
(define (stty! . args)
  (define cmd (string-append "stty " (string-join args " ") " < /dev/tty > /dev/tty 2>/dev/null"))
  (system cmd)
) ; end define stty!

(define (query-size)
  (define cols (getenv "COLUMNS"))
  (define rows (getenv "LINES"))
  (define out (with-output-to-string (lambda () (system "stty size < /dev/tty 2>/dev/null"))))
  (define m (regexp-match #rx"([0-9]+) +([0-9]+)" out))
  (cond
    [m (values (string->number (caddr m)) (string->number (cadr m)))]  ; stty size = "rows cols"
    [(and cols rows) (values (string->number cols) (string->number rows))]
    [else (values 80 24)]
  ) ; end cond
) ; end define query-size

(define (make-real-terminal #:in [in (current-input-port)]
                            #:out [out (current-output-port)])
  (define saved (box #f))
  (terminal
   ;; read-key：直接从 stdin 字节端口解析
   (lambda () (parse-key in))
   ;; write
   (lambda (s) (write-string s out) (flush-output out))
   ;; size
   query-size
   ;; raw-on!：保存原设置后进 raw（保留 opost 让 \n 正常输出由上层控制）
   (lambda ()
     (set-box! saved (string-trim (with-output-to-string (lambda () (system "stty -g < /dev/tty 2>/dev/null")))))
     (stty! "raw" "-echo")
   ) ; end raw-on!
   ;; raw-off!：恢复
   (lambda ()
     (if (and (unbox saved) (non-empty-string? (unbox saved)))
         (stty! (unbox saved))
         (stty! "sane")
     ) ; end if
   ) ; end raw-off!
   #t
  ) ; end terminal
) ; end define make-real-terminal

;; ------------------------------------------------------------ 脚本后端

;; 从预置的按键事件序列 + 输出捕获构造终端。
;; keys: (listof kev) 或字节串/字符串（后者经 parse-keys 转换）。
;; 输出写入内部 string port，可用 scripted-output 读取。
(struct scripted (queue out-port cols rows) #:mutable)

(define (make-scripted-terminal keys #:cols [cols 80] #:rows [rows 24])
  (define kev-list
    (cond
      [(list? keys) keys]
      [(or (bytes? keys) (string? keys)) (parse-keys keys)]
      [else '()]
    ) ; end cond
  ) ; end define kev-list
  (define st (scripted kev-list (open-output-string) cols rows))
  (define term
    (terminal
     ;; read-key：从队列取；空则 eof
     (lambda ()
       (define q (scripted-queue st))
       (cond
         [(null? q) key-eof]
         [else
          (set-scripted-queue! st (cdr q))
          (car q)
         ] ; end else
       ) ; end cond
     ) ; end read-key
     ;; write
     (lambda (s) (write-string s (scripted-out-port st)))
     ;; size
     (lambda () (values (scripted-cols st) (scripted-rows st)))
     void void #f
    ) ; end terminal
  ) ; end define term
  (values term st)
) ; end define make-scripted-terminal

;; 读出脚本终端迄今捕获的全部输出
(define (scripted-output st)
  (get-output-string (scripted-out-port st))
) ; end define scripted-output

;; 追加更多按键到脚本队列（模拟后续输入）
(define (scripted-feed! st keys)
  (define kev-list
    (cond
      [(list? keys) keys]
      [(or (bytes? keys) (string? keys)) (parse-keys keys)]
      [else '()]
    ) ; end cond
  ) ; end define kev-list
  (set-scripted-queue! st (append (scripted-queue st) kev-list))
) ; end define scripted-feed!

;; ---------------------------------------------------------------- provide

(provide
 (struct-out terminal)
 term-read-key
 term-write
 term-write-all
 term-size
 term-raw-on!
 term-raw-off!
 term-interactive?
 make-real-terminal
 make-scripted-terminal
 scripted?
 scripted-output
 scripted-feed!
) ; end provide
