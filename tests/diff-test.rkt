#lang tstring racket
;; tests/diff-test.rkt — 行级 diff：重建两侧、编辑数最短（对照 LCS 动态规划）、unified hunk 格式、截断 / 退化路径

(require rackunit racket/string (file "../src/diff.rkt"))

(define (sides edits a b)
  (values (for/list ([e edits] #:unless (eq? (vector-ref e 0) 'ins)) (vector-ref a (vector-ref e 1)))
          (for/list ([e edits] #:unless (eq? (vector-ref e 0) 'del)) (vector-ref b (vector-ref e 2)))))

(define (lcs-len a b)
  (define n (vector-length a))
  (define m (vector-length b))
  (define t (make-vector (* (add1 n) (add1 m)) 0))
  (define (at i j) (vector-ref t (+ (* i (add1 m)) j)))
  (for* ([i (in-range 1 (add1 n))] [j (in-range 1 (add1 m))])
    (vector-set! t (+ (* i (add1 m)) j)
                 (if (string=? (vector-ref a (sub1 i)) (vector-ref b (sub1 j)))
                     (add1 (at (sub1 i) (sub1 j)))
                     (max (at (sub1 i) j) (at i (sub1 j))))))
  (at n m))

(test-case "random line vectors: edits rebuild both sides and are minimal"
  (random-seed 7)
  (for ([trial 300])
    (define (rnd) (for/vector ([_ (random 12)]) (string (integer->char (+ 97 (random 4))))))
    (define a (rnd))
    (define b (rnd))
    (define es (line-edits a b))
    (define-values (old new) (sides es a b))
    (check-equal? old (vector->list a))
    (check-equal? new (vector->list b))
    (define d (for/sum ([e es]) (if (eq? (vector-ref e 0) 'eq) 0 1)))
    (check-equal? d (- (+ (vector-length a) (vector-length b)) (* 2 (lcs-len a b))) (format "~s ~s" a b))))

(test-case "unified hunks: 3 lines of context, header counts, far changes split"
  (define before (string-join (for/list ([i (in-range 1 31)]) f"line {i}") "\n" #:after-last "\n"))
  (define after (string-replace (string-replace before "line 5\n" "line five\n") "line 25\n" "line 25\nadded\n"))
  (define d (file-diff "x.txt" before after))
  (check-equal? (hash-ref d 'added) 2)
  (check-equal? (hash-ref d 'removed) 1)
  (check-false (hash-ref d 'truncated))
  (check-equal? (string-split (hash-ref d 'text) "\n")
                '("@@ -2,7 +2,7 @@" " line 2" " line 3" " line 4" "-line 5" "+line five" " line 6" " line 7" " line 8"
                  "@@ -23,6 +23,7 @@" " line 23" " line 24" " line 25" "+added" " line 26" " line 27" " line 28")))

(test-case "new file and emptied file; trailing newline does not create a phantom line"
  (define d1 (file-diff "n.txt" "" "a\nb\n"))
  (check-equal? (hash-ref d1 'text) "@@ -0,0 +1,2 @@\n+a\n+b")
  (define d2 (file-diff "n.txt" "a\nb" ""))
  (check-equal? (hash-ref d2 'text) "@@ -1,2 +0,0 @@\n-a\n-b")
  (check-equal? (hash-ref (file-diff "n.txt" "same\n" "same\n") 'text) ""))

(test-case "large rewrites: Myers gives up past MAX-D and falls back to delete-all/insert-all; output is capped"
  (define before (string-join (for/list ([i 3000]) f"old {i}") "\n"))
  (define after (string-join (for/list ([i 3000]) f"new {i}") "\n"))
  (define-values (r ms _r _g) (time-apply (lambda () (file-diff "big.txt" before after)) '()))
  (define d (car r))
  (check-equal? (hash-ref d 'added) 3000)
  (check-equal? (hash-ref d 'removed) 3000)
  (check-true (hash-ref d 'truncated))
  (check-equal? (length (string-split (hash-ref d 'text) "\n" #:trim? #f)) MAX-DIFF-LINES)
  (check-true (< ms 5000) (format "~a ms" ms)))

(test-case "small edit in a big file is fast (common prefix / suffix stripped)"
  (define before (string-join (for/list ([i 50000]) f"row {i}") "\n"))
  (define after (string-replace before "row 25000\n" "row 25000 changed\n"))
  (define-values (r ms _r _g) (time-apply (lambda () (file-diff "big.txt" before after)) '()))
  (check-equal? (hash-ref (car r) 'added) 1)
  (check-equal? (hash-ref (car r) 'removed) 1)
  (check-true (< ms 1000) (format "~a ms" ms)))
