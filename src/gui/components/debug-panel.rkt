#lang racket/base
;; debug-panel.rkt — 调试面板组件（G6，⌘⌥D 开关）：盖在对话区右侧的半透明面板。
;; 内容：拼装层给的统计行；日志文件路径；最近的内部错误（where + message + 前几帧调用栈）；最近的操作日志记录。
;; 数据来自 journal 接口（经接口发现取得，不认识日志组件本身）。
;; needs text-layout journal → provides debug-view

(require racket/list racket/string
         "../compute/drawlist.rkt" "../compute/palette.rkt" "../compute/layout.rkt" "../compute/paint.rkt" "../compute/rich.rkt"
         "../compute/hub.rkt" "../compute/interfaces.rkt" "../compute/i18n.rkt"
         "../compute/view.rkt")                                         ; 视图平面

(define ERR '(0.95 0.50 0.45 1.0))
(define HEAD '(0.55 0.70 1.0 1.0))
(define PAD 10.0)

(define (kind-color k) (case k [(error) ERR] [(send) HEAD] [(core) '(0.60 0.80 0.62 1.0)] [else DIM]))

(define (dp-render! open? jr rc dl x y w h scale stats)
  (when (unbox open?)
    ;; 视图平面：一栏文字；最近的记录放得下多少放多少（先量出上半部分的高，剩下的高 ÷ 行高）
    (define lay (rich-ctx-layout rc))
    (define inner (- w (* 2 PAD)))
    (define (line text font color) (ui-text text #:font font #:color color))
    (define errs (jr 'errors))
    (define head
      (append
       (list (line (t 'dbg.title) 'body HEAD))
       (for/list ([st (in-list stats)]) (line st 'small DIM))
       (list (line (t 'dbg.journal (or (jr 'path) (t 'dbg.memory-only))) 'small DIM)
             (ui-box #:pad (vector 6.0 0.0 0.0 0.0) (line (t 'dbg.errors (length errs)) 'small (if (null? errs) DIM ERR))))
       (append* (for/list ([e (in-list (take errs (min 3 (length errs))))])
                  (cons (line (format "~a: ~a" (hash-ref e 'where "?") (hash-ref e 'message "?")) 'mono ERR)
                        (for/list ([c (in-list (take (hash-ref e 'context '()) (min 3 (length (hash-ref e 'context '())))))])
                          (line (format "    ~a" c) 'small DIM)))))
       (list (ui-box #:pad (vector 6.0 0.0 0.0 0.0) (line (t 'dbg.recent) 'small DIM)))))
    (define-values (_hw head-h) (measure lay (apply ui-box #:dir 'column head) inner))
    (define lh (text-block-line-height (lay "M" 'mono 100.0 #f)))
    (define n (max 0 (inexact->exact (floor (/ (- h (* 2 PAD) head-h) lh)))))
    (define tree
      (apply ui-box #:dir 'column #:pad PAD #:fill '(0.06 0.07 0.09 0.94) #:radius 8.0
             (append head
                     (for/list ([r (in-list (jr 'tail n))])
                       (line (format "~a ~a ~a" (~ms (vector-ref r 0)) (vector-ref r 1) (vector-ref r 2)) 'mono (kind-color (vector-ref r 1)))))))
    (drawlist-set-clip! dl (vector x y w h))
    (render-view! rc dl tree x y w h scale #:clip (vector x y w h))
    (drawlist-set-clip! dl #f))
) ; end define dp-render!

(define (~ms t) (let ([s (number->string t)]) (string-append (make-string (max 0 (- 8 (string-length s))) #\space) s)))

(define debug-panel-component
  (component 'debug-panel (list text-layout journal) (list debug-view)
             (lambda (use config)
               (define open? (box #f))
               (define rc ((use text-layout) 'rich))
               (define jr (use journal))
               (list (implement debug-view
                                'toggle! (lambda () (set-box! open? (not (unbox open?))))
                                'open? (lambda () (unbox open?))
                                'render! (lambda (dl x y w h scale stats) (dp-render! open? jr rc dl x y w h scale stats)))))))

(provide debug-panel-component)
