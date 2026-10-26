#lang racket/base
;; ansi.rkt — 工具输出里的 ANSI 转义（design-gui.md §30，用户决定：渲染颜色，只认 SGR 前景 / 背景色；其它转义去掉）。纯函数。
;;
;; (ansi-parse s) → (values 纯文字 片段)
;;   纯文字：去掉全部转义（CSI「ESC [ … 终止字节」、OSC「ESC ] … BEL / ESC \」、其它「ESC 单字符」）
;;   片段：(listof (vector 起 止 前景 背景))，纯文字的下标；前景 / 背景 = (list r g b) 或 #f（缺省）
;;   SGR：0 复位、39 / 49 缺省、30–37 / 90–97 前景、40–47 / 100–107 背景、38;5;n / 48;5;n（256 色）、38;2;r;g;b / 48;2;r;g;b（真彩色）；
;;   粗体 / 下划线等其它属性忽略
;; (readable-fg 前景 背景) → 调亮 / 调暗到与背景对比度 ≥ 4.5 的颜色（暗色主题：黑字会变成灰）

(require racket/list racket/string "contrast.rkt")

;; 16 色：按暗色卡片调过的（与终端主题无关）；黑 → 灰（暗底上看不见黑字）
(define BASE16
  (vector '(0.55 0.57 0.62) '(0.95 0.45 0.45) '(0.50 0.82 0.52) '(0.92 0.80 0.40) '(0.48 0.66 1.00) '(0.85 0.55 0.95) '(0.40 0.82 0.85) '(0.85 0.86 0.88)
          '(0.66 0.68 0.72) '(1.00 0.58 0.58) '(0.62 0.92 0.64) '(1.00 0.90 0.52) '(0.62 0.76 1.00) '(0.92 0.68 1.00) '(0.56 0.92 0.94) '(0.97 0.97 0.98)))

(define (xterm256 n)
  (cond [(< n 16) (vector-ref BASE16 n)]
        [(< n 232) (let* ([k (- n 16)] [r (quotient k 36)] [g (quotient (remainder k 36) 6)] [b (remainder k 6)]
                          [v (lambda (c) (if (zero? c) 0.0 (/ (+ 55 (* 40 c)) 255.0)))])
                     (list (v r) (v g) (v b)))]
        [else (let ([v (/ (+ 8 (* 10 (- n 232))) 255.0)]) (list v v v))]))

;; 前景向白、背景向黑调，直到对比度够
(define (readable-fg fg bg [min-ratio 4.5])
  (let loop ([c fg] [k 0])
    (if (or (>= (contrast-ratio c bg) min-ratio) (> k 20))
        c
        (loop (map (lambda (v) (+ v (* 0.12 (- 1.0 v)))) c) (add1 k)))))
(define (dark-bg bg [card '(0.16 0.16 0.18)])                ; 背景色压暗一些（字要在上面看得清）
  (map (lambda (v c) (+ (* 0.45 v) (* 0.55 c))) bg card))

(define (apply-sgr params fg bg)
  (let loop ([ps params] [fg fg] [bg bg])
    (cond
      [(null? ps) (values fg bg)]
      [else
       (define p (car ps))
       (cond
         [(= p 0) (loop (cdr ps) #f #f)]
         [(= p 39) (loop (cdr ps) #f bg)]
         [(= p 49) (loop (cdr ps) fg #f)]
         [(<= 30 p 37) (loop (cdr ps) (vector-ref BASE16 (- p 30)) bg)]
         [(<= 90 p 97) (loop (cdr ps) (vector-ref BASE16 (+ 8 (- p 90))) bg)]
         [(<= 40 p 47) (loop (cdr ps) fg (vector-ref BASE16 (- p 40)))]
         [(<= 100 p 107) (loop (cdr ps) fg (vector-ref BASE16 (+ 8 (- p 100))))]
         [(and (memv p '(38 48)) (pair? (cdr ps)) (= (cadr ps) 5) (pair? (cddr ps)))
          (define c (xterm256 (max 0 (min 255 (caddr ps)))))
          (if (= p 38) (loop (cdddr ps) c bg) (loop (cdddr ps) fg c))]
         [(and (memv p '(38 48)) (pair? (cdr ps)) (= (cadr ps) 2) (>= (length ps) 5))
          (define c (map (lambda (v) (/ (max 0 (min 255 v)) 255.0)) (take (cddr ps) 3)))
          (if (= p 38) (loop (drop ps 5) c bg) (loop (drop ps 5) fg c))]
         [else (loop (cdr ps) fg bg)])])))                  ; 粗体、下划线等：忽略

(define (ansi-parse s)
  (define n (string-length s))
  (define out (open-output-string))
  (define spans '())
  (define pos 0)                                          ; 纯文字里的位置
  (define start 0)
  (define (close-span! fg bg) (when (and (or fg bg) (> pos start)) (set! spans (cons (vector start pos fg bg) spans))))
  (let loop ([i 0] [fg #f] [bg #f])
    (cond
      [(>= i n) (close-span! fg bg)]
      [(char=? (string-ref s i) #\u1B)
       (cond
         [(and (< (add1 i) n) (char=? (string-ref s (add1 i)) #\[))      ; CSI
          (define j (let f ([j (+ i 2)]) (if (and (< j n) (not (char<=? #\@ (string-ref s j) #\~))) (f (add1 j)) j)))
          (define final (and (< j n) (string-ref s j)))
          (cond
            [(eqv? final #\m)
             (define params (let ([body (substring s (+ i 2) j)])
                              (if (string=? body "") '(0) (map (lambda (x) (or (string->number x) 0)) (string-split body ";" #:trim? #f)))))
             (define-values (fg2 bg2) (apply-sgr params fg bg))
             (close-span! fg bg) (set! start pos)
             (loop (min n (add1 j)) fg2 bg2)]
            [else (loop (min n (add1 j)) fg bg)])]                       ; 别的 CSI（光标移动、清屏…）：去掉
         [(and (< (add1 i) n) (char=? (string-ref s (add1 i)) #\]))      ; OSC：到 BEL 或 ESC \
          (define j (let f ([j (+ i 2)])
                      (cond [(>= j n) n]
                            [(char=? (string-ref s j) #\u07) (add1 j)]
                            [(and (char=? (string-ref s j) #\u1B) (< (add1 j) n) (char=? (string-ref s (add1 j)) #\\)) (+ j 2)]
                            [else (f (add1 j))])))
          (loop j fg bg)]
         [else                                                            ; ESC + 中间字节（0x20–0x2F）若干 + 终止字节（如 ESC ( B）
          (define j (let f ([j (add1 i)]) (if (and (< j n) (char<=? #\space (string-ref s j) #\/)) (f (add1 j)) j)))
          (loop (min n (add1 j)) fg bg)])]
      [else (write-char (string-ref s i) out) (set! pos (add1 pos)) (loop (add1 i) fg bg)]))
  (values (get-output-string out) (reverse spans))
) ; end define ansi-parse

(provide ansi-parse readable-fg dark-bg xterm256 BASE16)
