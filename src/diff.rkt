#lang tstring racket
;; diff.rkt — 文件改动的行级 diff（G5：GUI 工具卡片显示改动）。
;; 先剥掉公共前缀 / 后缀行（多数编辑只动几行），中段跑 Myers O((N+M)D)；
;; D 超过 MAX-D 时退化为「中段整删整加」（结果正确但不最短）。输出 unified 格式的 hunk（3 行上下文）。

(require racket/string racket/list)

(define MAX-D 1000)              ; Myers 编辑距离上限（trace 内存 ~ D²）
(define MAX-DIFF-LINES 2000)     ; 输出行数上限（超出截断，added/removed 仍按全量计）

;; 文本 → 行向量（末尾换行不产生空行）
(define (text-lines s)
  (define ls (string-split s "\n" #:trim? #f))
  (list->vector (if (and (pair? ls) (string=? (last ls) "")) (drop-right ls 1) ls))
) ; end define text-lines

;; Myers：a、b 的 [a0,a1) [b0,b1) 段 → 编辑表 (listof (vector op 旧下标 新下标))，op ∈ 'eq 'del 'ins；超过 MAX-D → #f
(define (myers a a0 a1 b b0 b1)
  (define n (- a1 a0))
  (define m (- b1 b0))
  (define mx (+ n m))
  (define off (add1 mx))
  (define v (make-vector (+ 3 (* 2 mx)) 0))
  (define (vref k) (vector-ref v (+ off k)))
  ;; trace：每步开始前 v 在 k ∈ [-(d+1), d+1] 的切片
  (define final-d
    (let loop ([d 0] [trace '()])
      (cond
        [(> d MAX-D) #f]
        [else
         (define slice (for/vector #:length (+ 3 (* 2 d)) ([k (in-range (- (add1 d)) (+ d 2))]) (vref k)))
         (define trace* (cons slice trace))
         (define done?
           (for/or ([k (in-range (- d) (add1 d) 2)])
             (define x0 (if (or (= k (- d)) (and (not (= k d)) (< (vref (sub1 k)) (vref (add1 k)))))
                            (vref (add1 k))
                            (add1 (vref (sub1 k)))))
             (define x (let snake ([x x0])
                         (if (and (< x n) (< (- x k) m)
                                  (string=? (vector-ref a (+ a0 x)) (vector-ref b (+ b0 (- x k)))))
                             (snake (add1 x))
                             x)))
             (vector-set! v (+ off k) x)
             (and (>= x n) (>= (- x k) m))))
         (if done? (cons d trace*) (loop (add1 d) trace*))])))
  (and final-d
       (let back ([d (car final-d)] [trace (cdr final-d)] [x n] [y m] [acc '()])
         (define slice (car trace))
         (define (sref k) (vector-ref slice (+ k d 1)))
         (define k (- x y))
         (define pk (if (or (= k (- d)) (and (not (= k d)) (< (sref (sub1 k)) (sref (add1 k))))) (add1 k) (sub1 k)))
         (define px (if (= d 0) 0 (sref pk)))
         (define py (if (= d 0) 0 (- px pk)))
         ;; 对角线（相同行）
         (define-values (x* y* acc*)
           (let diag ([x x] [y y] [acc acc])
             (if (and (> x px) (> y py))
                 (diag (sub1 x) (sub1 y) (cons (vector 'eq (+ a0 (sub1 x)) (+ b0 (sub1 y))) acc))
                 (values x y acc))))
         (cond
           [(= d 0) acc*]
           [else
            (define edit (if (= x* px) (vector 'ins (+ a0 px) (+ b0 py)) (vector 'del (+ a0 px) (+ b0 py))))
            (back (sub1 d) (cdr trace) px py (cons edit acc*))])))
) ; end define myers

;; 整串 → 编辑表（含公共前后缀的 eq）
(define (line-edits a b)
  (define n (vector-length a))
  (define m (vector-length b))
  (define pre (let loop ([i 0]) (if (and (< i n) (< i m) (string=? (vector-ref a i) (vector-ref b i))) (loop (add1 i)) i)))
  (define suf (let loop ([i 0]) (if (and (< i (- n pre)) (< i (- m pre))
                                         (string=? (vector-ref a (- n i 1)) (vector-ref b (- m i 1))))
                                    (loop (add1 i)) i)))
  (define a1 (- n suf))
  (define b1 (- m suf))
  (define mid (or (myers a pre a1 b pre b1)
                  (append (for/list ([i (in-range pre a1)]) (vector 'del i pre))
                          (for/list ([j (in-range pre b1)]) (vector 'ins a1 j)))))
  (append (for/list ([i (in-range pre)]) (vector 'eq i i))
          mid
          (for/list ([i (in-range suf)]) (vector 'eq (+ a1 i) (+ b1 i))))
) ; end define line-edits

;; 编辑表 → unified hunk 行 (listof string)
(define (unified-lines a b edits context)
  (define ev (list->vector edits))
  (define len (vector-length ev))
  (define changes (for/list ([e (in-vector ev)] [i (in-naturals)] #:unless (eq? (vector-ref e 0) 'eq)) i))
  ;; 相邻改动间相同行 ≤ 2×context 的并成一个 hunk：(cons 起 止) 是编辑表下标（含）
  (define groups
    (reverse
     (for/fold ([gs '()]) ([c (in-list changes)])
       (if (and (pair? gs) (<= (- c (cdar gs) 1) (* 2 context)))
           (cons (cons (caar gs) c) (cdr gs))
           (cons (cons c c) gs)))))
  (append*
   (for/list ([g (in-list groups)])
     (define s (max 0 (- (car g) context)))
     (define e (min (sub1 len) (+ (cdr g) context)))
     (define es (for/list ([i (in-range s (add1 e))]) (vector-ref ev i)))
     (define ol (for/sum ([x (in-list es)]) (if (eq? (vector-ref x 0) 'ins) 0 1)))
     (define nl (for/sum ([x (in-list es)]) (if (eq? (vector-ref x 0) 'del) 0 1)))
     (define o0 (vector-ref (car es) 1))
     (define n0 (vector-ref (car es) 2))
     (cons f"@@ -{(if (zero? ol) o0 (add1 o0))},{ol} +{(if (zero? nl) n0 (add1 n0))},{nl} @@"
           (for/list ([x (in-list es)])
             (case (vector-ref x 0)
               [(eq) (string-append " " (vector-ref a (vector-ref x 1)))]
               [(del) (string-append "-" (vector-ref a (vector-ref x 1)))]
               [(ins) (string-append "+" (vector-ref b (vector-ref x 2)))])))))
) ; end define unified-lines

;; 文件改动 → jsexpr：path、added、removed、text（unified hunk，行以 "\n" 连接）、truncated
(define (file-diff path before after #:context [context 3])
  (define a (text-lines before))
  (define b (text-lines after))
  (define edits (line-edits a b))
  (define added (for/sum ([e (in-list edits)]) (if (eq? (vector-ref e 0) 'ins) 1 0)))
  (define removed (for/sum ([e (in-list edits)]) (if (eq? (vector-ref e 0) 'del) 1 0)))
  (define ls (unified-lines a b edits context))
  (define truncated? (> (length ls) MAX-DIFF-LINES))
  (hasheq 'path (if (path? path) (path->string path) path)
          'added added 'removed removed
          'text (string-join (if truncated? (take ls MAX-DIFF-LINES) ls) "\n")
          'truncated truncated?)
) ; end define file-diff

;; 工具里用：diff 只是显示信息，算失败不能让工具失败
(define (try-file-diff path before after)
  (with-handlers ([exn:fail? (lambda (_) #f)]) (file-diff path before after))
) ; end define try-file-diff

(provide file-diff try-file-diff line-edits text-lines MAX-D MAX-DIFF-LINES)
