#lang tstring racket
;; tui/keys.rkt — 按键 / 转义序列解析（design: TUI 输入层）
;; 字节流 → 结构化按键事件。方向键、Home/End/Del、Ctrl/Alt 组合、UTF-8 多字节字符。
;; 纯粹对 input-port 消费，故脚本化字节串与真实 tty 共用同一解析器。

;; 按键事件
(struct kev
  (kind   ; 'char | 'named | 'eof
   char   ; character（kind='char 时有效）
   name   ; symbol（kind='named 时有效：up/down/left/right/home/end/
          ;         pgup/pgdn/insert/delete/enter/tab/backspace/escape/f1..f12）
   mods   ; (listof (or/c 'ctrl 'alt 'shift))
  ) ; end fields
  #:prefab
) ; end struct kev

(define (kchar ch [mods '()]) (kev 'char ch #f mods))
(define (knamed name [mods '()]) (kev 'named #f name mods))
(define key-eof (kev 'eof #f #f '()))

(define (kev-ctrl? k) (and (memq 'ctrl (kev-mods k)) #t))
(define (kev-alt? k) (and (memq 'alt (kev-mods k)) #t))
(define (kev-shift? k) (and (memq 'shift (kev-mods k)) #t))

;; ---------------------------------------------------------------- 工具

;; 端口是否有立即可读的字节（用于 ESC 歧义消解：孤立 ESC vs 序列前缀）
(define (avail? in)
  (and (byte-ready? in)
       (not (eof-object? (peek-byte in)))
  ) ; end and
) ; end define avail?

;; UTF-8 续读：给定 lead 字节，读齐整个码位，返回 char（非法则替换符）
(define (read-utf8-char in lead)
  (define n
    (cond
      [(< lead #x80) 1]
      [(= (bitwise-and lead #xE0) #xC0) 2]
      [(= (bitwise-and lead #xF0) #xE0) 3]
      [(= (bitwise-and lead #xF8) #xF0) 4]
      [else 1]                              ; 非法 lead
    ) ; end cond
  ) ; end define n
  (define bs
    (let loop ([acc (list lead)] [k (sub1 n)])
      (cond
        [(<= k 0) (reverse acc)]
        [(not (avail? in)) (reverse acc)]   ; 续读字节不足：尽力而为
        [else (loop (cons (read-byte in) acc) (sub1 k))]
      ) ; end cond
    ) ; end let loop
  ) ; end define bs
  (define REPLACEMENT (integer->char #xFFFD))
  (with-handlers ([exn:fail? (lambda (_e) REPLACEMENT)])
    (define s (bytes->string/utf-8 (list->bytes bs) REPLACEMENT))
    (if (> (string-length s) 0) (string-ref s 0) REPLACEMENT)
  ) ; end with-handlers
) ; end define read-utf8-char

;; CSI 参数中的修饰符编码：param(如 5)=ctrl → mods 列表
(define (decode-mods param)
  (define m (sub1 param))
  (append (if (bitwise-bit-set? m 0) '(shift) '())
          (if (bitwise-bit-set? m 1) '(alt) '())
          (if (bitwise-bit-set? m 2) '(ctrl) '())
  ) ; end append
) ; end define decode-mods

;; ---------------------------------------------------------------- CSI / SS3

;; ESC [ … final —— 收集参数与 final 字节
(define (parse-csi in)
  (define params (open-output-string))
  (define final
    (let loop ()
      (cond
        [(not (avail? in)) #f]
        [else
         (define b (read-byte in))
         (cond
           [(and (>= b #x30) (<= b #x3F))    ; 参数字节 0-9 ; : < = > ?
            (write-char (integer->char b) params)
            (loop)
           ] ; end param case
           [(and (>= b #x20) (<= b #x2F))    ; 中间字节
            (loop)
           ] ; end intermediate case
           [(and (>= b #x40) (<= b #x7E)) b] ; final
           [else b]
         ) ; end cond
        ] ; end else
      ) ; end cond
    ) ; end let loop
  ) ; end define final
  (define param-str (get-output-string params))
  (cond
    ;; SGR 鼠标事件：ESC [ < btn ; col ; row (M|m)。滚轮 → 滚动键。
    [(and final (regexp-match? #rx"^<" param-str)
          (or (= final (char->integer #\M)) (= final (char->integer #\m))))
     (sgr-mouse->kev param-str)
    ] ; end mouse
    [else
     (define nums
       (for/list ([p (in-list (string-split param-str ";"))])
         (or (string->number p) 0)
       ) ; end for/list
     ) ; end define nums
     (define mods
       (if (>= (length nums) 2) (decode-mods (cadr nums)) '())
     ) ; end define mods
     (csi->kev final nums mods)
    ] ; end else
  ) ; end cond
) ; end define parse-csi

;; SGR 鼠标：仅关心滚轮（按钮码 bit6 置位；bit0：0=上,1=下）。其余（点击/移动）忽略。
(define (sgr-mouse->kev param-str)
  (define digits (substring param-str 1))       ; 去掉前导 '<'
  (define btn (or (string->number (car (string-split digits ";"))) 0))
  (cond
    [(bitwise-bit-set? btn 6) (if (even? btn) (knamed 'scroll-up) (knamed 'scroll-down))]
    [else (knamed 'escape)]                      ; 非滚轮鼠标事件：当作无操作
  ) ; end cond
) ; end define sgr-mouse->kev

(define (csi->kev final nums mods)
  (cond
    [(not final) (knamed 'escape)]
    [else
     (define fc (integer->char final))
     (case fc
       [(#\A) (knamed 'up mods)]
       [(#\B) (knamed 'down mods)]
       [(#\C) (knamed 'right mods)]
       [(#\D) (knamed 'left mods)]
       [(#\H) (knamed 'home mods)]
       [(#\F) (knamed 'end mods)]
       [(#\Z) (knamed 'tab '(shift))]        ; CSI Z = Shift-Tab
       [(#\u) (csiu->kev nums mods)]          ; CSI u（kitty 键协议：修饰键消歧）
       [(#\~)
        (if (and (pair? nums) (= (car nums) 27))
            (moK->kev nums)                    ; \e[27;mod;code~（xterm modifyOtherKeys）
            (tilde->kev (if (pair? nums) (car nums) 0) mods))
       ] ; end tilde
       [else (knamed 'escape)]
     ) ; end case
    ] ; end else
  ) ; end cond
) ; end define csi->kev

;; CSI-u / modifyOtherKeys 的键码 → kev。核心用途：识别带修饰的 Enter（Shift/Alt+Enter）
;; 以支持多行输入；普通可打印码位也还原为字符。
(define (csiu->kev nums mods)
  (define code (if (pair? nums) (car nums) 0))
  (cond
    [(or (= code 13) (= code 10)) (knamed 'enter mods)]
    [(= code 9) (knamed 'tab mods)]
    [(= code 27) (knamed 'escape mods)]
    [(or (= code 127) (= code 8)) (knamed 'backspace mods)]
    [(and (>= code 32) (< code #x110000)) (kchar (integer->char code) mods)]
    [else (knamed 'escape mods)]
  ) ; end cond
) ; end define csiu->kev

;; \e[27;mod;code~ —— xterm modifyOtherKeys 形式，键码在第三参数。
(define (moK->kev nums)
  (define mod (if (>= (length nums) 2) (cadr nums) 1))
  (define code (if (>= (length nums) 3) (caddr nums) 0))
  (csiu->kev (list code) (decode-mods mod))
) ; end define moK->kev

(define (tilde->kev n mods)
  (case n
    [(1 7) (knamed 'home mods)]
    [(2) (knamed 'insert mods)]
    [(3) (knamed 'delete mods)]
    [(4 8) (knamed 'end mods)]
    [(5) (knamed 'pgup mods)]
    [(6) (knamed 'pgdn mods)]
    [(11) (knamed 'f1 mods)]
    [(12) (knamed 'f2 mods)]
    [(13) (knamed 'f3 mods)]
    [(14) (knamed 'f4 mods)]
    [(15) (knamed 'f5 mods)]
    [(17) (knamed 'f6 mods)]
    [(18) (knamed 'f7 mods)]
    [(19) (knamed 'f8 mods)]
    [(20) (knamed 'f9 mods)]
    [(21) (knamed 'f10 mods)]
    [(23) (knamed 'f11 mods)]
    [(24) (knamed 'f12 mods)]
    [else (knamed 'escape)]
  ) ; end case
) ; end define tilde->kev

;; ESC O final —— 应用光标键模式 / F1-F4
(define (parse-ss3 in)
  (cond
    [(not (avail? in)) (knamed 'escape)]
    [else
     (define b (read-byte in))
     (case (integer->char b)
       [(#\A) (knamed 'up)]
       [(#\B) (knamed 'down)]
       [(#\C) (knamed 'right)]
       [(#\D) (knamed 'left)]
       [(#\H) (knamed 'home)]
       [(#\F) (knamed 'end)]
       [(#\P) (knamed 'f1)]
       [(#\Q) (knamed 'f2)]
       [(#\R) (knamed 'f3)]
       [(#\S) (knamed 'f4)]
       [else (knamed 'escape)]
     ) ; end case
    ] ; end else
  ) ; end cond
) ; end define parse-ss3

;; ---------------------------------------------------------------- ESC 与 Alt

(define (parse-escape in)
  (cond
    [(not (avail? in)) (knamed 'escape)]     ; 孤立 ESC
    [else
     (define b2 (read-byte in))
     (cond
       [(= b2 (char->integer #\[)) (parse-csi in)]
       [(= b2 (char->integer #\O)) (parse-ss3 in)]
       [(= b2 #x1B) (knamed 'escape)]        ; ESC ESC
       [(or (= b2 #x0D) (= b2 #x0A)) (knamed 'enter '(alt))]  ; Alt/Option+Enter（多行）
       [(= b2 #x7F) (knamed 'backspace '(alt))]
       [(and (>= b2 #x01) (< b2 #x20))       ; Alt+Ctrl+letter
        (kchar (integer->char (+ b2 96)) '(ctrl alt))
       ] ; end alt-ctrl case
       [else                                  ; Alt + 普通字符
        (kchar (read-utf8-char in b2) '(alt))
       ] ; end alt case
     ) ; end cond
    ] ; end else
  ) ; end cond
) ; end define parse-escape

;; ---------------------------------------------------------------- 主入口

;; 从字节端口读下一个按键事件（阻塞直到有一个完整事件）
(define (parse-key in)
  (define b (read-byte in))
  (cond
    [(eof-object? b) key-eof]
    [(= b #x1B) (parse-escape in)]
    [(or (= b #x0D) (= b #x0A)) (knamed 'enter)]
    [(= b #x09) (knamed 'tab)]
    [(= b #x7F) (knamed 'backspace)]         ; DEL 键位普遍映射为退格
    [(= b #x08) (knamed 'backspace '(ctrl))] ; Ctrl-H
    [(< b #x20)                               ; 其余 C0 控制符 = Ctrl+字母
     (kchar (integer->char (+ b 96)) '(ctrl))
    ] ; end ctrl case
    [(< b #x80) (kchar (integer->char b))]    ; 可打印 ASCII
    [else (kchar (read-utf8-char in b))]      ; UTF-8 多字节
  ) ; end cond
) ; end define parse-key

;; 解析整串字节 → 事件列表（测试便捷）
(define (parse-keys bs)
  (define in (open-input-bytes (if (string? bs) (string->bytes/utf-8 bs) bs)))
  (let loop ([acc '()])
    (define k (parse-key in))
    (if (eq? (kev-kind k) 'eof)
        (reverse acc)
        (loop (cons k acc))
    ) ; end if
  ) ; end let loop
) ; end define parse-keys

;; ---------------------------------------------------------------- provide

(provide
 (struct-out kev)
 kchar
 knamed
 key-eof
 kev-ctrl?
 kev-alt?
 kev-shift?
 parse-key
 parse-keys
) ; end provide
