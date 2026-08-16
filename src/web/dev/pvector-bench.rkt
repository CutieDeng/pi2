#lang racket/base
;; pvector-bench.rkt — 增强版 Racket pvector 性能特征测试套（dev 工具）
;; 缘起：为 JS 数组选存储结构时，发现 pvector 的一个反直觉行为，见 pvector-finding.md。
;; 用: racket src/web/dev/pvector-bench.rkt
;; 结论（可复现）：cons-right append 是 O(1)，但 cons-right 构建出的 pvector 随机 ref 是 O(n)；
;;   list->pvector 构建出的随机 ref 是 O(log n)。同一逻辑向量，构建方式决定随机访问复杂度。

(require racket/pvector racket/list)
(define (ms) (current-inexact-milliseconds))
(define (fl) (flush-output))
(define (median xs) (list-ref (sort xs <) (quotient (length xs) 2)))
(define (bench-median thunk trials)
  (median (for/list ([_ (in-range trials)]) (collect-garbage) (define t0 (ms)) (thunk) (- (ms) t0))))

;; 顺序 cons-right 构建
(define (build-cons-right N) (for/fold ([v (pvector)]) ([i (in-range N)]) (pvector-cons-right v i)))

(printf "== 1. cons-right append 成本（build 随 N 应线性 → append 是 O(1)）==\n") (fl)
(for ([N (list 1000 4000 16000 64000)])
  (collect-garbage) (define t0 (ms)) (build-cons-right N) (define bt (- (ms) t0))
  (printf "  N=~a  build=~ams  每append=~aµs\n" N (real->decimal-string bt 2)
          (real->decimal-string (/ (* bt 1000) N) 4)) (fl))

(printf "\n== 2. 热身后随机 ref scaling：cons-right vs list->pvector ==\n")
(printf "  N\tcons-right(µs/ref)\tlist->pv(µs/ref)\t比值\n") (fl)
(for ([N (list 1000 2000 4000 8000)])
  (define cr (build-cons-right N))
  (define lp (list->pvector (range N)))
  (define R 100000)
  (define (warm+ref pv)
    (for ([i (in-range N)]) (pvector-ref pv i))              ; 热身：排除懒/首触
    (bench-median (lambda () (for ([k (in-range R)]) (pvector-ref pv (modulo (* k 97) N)))) 3))
  (define c (/ (* (warm+ref cr) 1000) R)) (define l (/ (* (warm+ref lp) 1000) R))
  (printf "  ~a\t~a\t\t\t~a\t\t~a×\n" N (real->decimal-string c 3) (real->decimal-string l 3)
          (real->decimal-string (/ c (max l 0.001)) 1)) (fl))

(printf "\n== 3. cons-right：首尾 ref(快路) vs 中间随机 ref ==\n") (fl)
(for ([N (list 16000 64000)])
  (define pv (build-cons-right N))
  (for ([i (in-range N)]) (pvector-ref pv i))                ; 热身
  (define R 200000)
  (define ends (bench-median (lambda () (for ([k (in-range R)]) (pvector-ref pv (if (even? k) 0 (sub1 N))))) 3))
  (define mid  (bench-median (lambda () (for ([k (in-range R)]) (pvector-ref pv (modulo (* k 97) N)))) 3))
  (printf "  N=~a  首尾=~aµs/ref  中间随机=~aµs/ref\n" N
          (real->decimal-string (/ (* ends 1000) R) 3) (real->decimal-string (/ (* mid 1000) R) 3)) (fl))

(printf "\n== 判定 ==\n")
(printf "  cons-right append: O(1)（§1 build 线性）\n")
(printf "  cons-right 随机 ref: O(n)（§2 随 N 线性翻倍；首尾 §3 是 O(1) 快路）\n")
(printf "  list->pvector 随机 ref: O(log n)（§2 几乎不随 N 变）\n")
(printf "  → 数组(靠 push 增长、需快随机 ref)不宜用 pvector；密集数组用原生 vector。\n")
(printf "  → 未解：为何 O(1)-append 的 finger tree 出来的随机 ref 是 O(n)（native 核心，见 pvector-finding.md）\n")
(fl)
