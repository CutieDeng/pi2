#lang racket/base
;; tests/gui/text-test.rkt — 断行 / 排版 / atlas（假后端，无平台依赖）

(require rackunit racket/list racket/vector
         "../../src/gui/compute/text-backend.rkt"
         "../../src/gui/compute/linebreak.rkt"
         "../../src/gui/compute/layout.rkt"
         "../../src/gui/compute/atlas.rkt")

;; 假后端：拉丁 10pt、表意 20pt、组合符 0pt；每码点一个字形，cluster = 码点下标
(define (adv c)
  (cond [(eq? (char-general-category c) 'mn) 0]
        [(ideographic? c) 20]
        [else 10]))
(define fake-raster-calls 0)
(define fake
  (text-backend
   (lambda (family size) 0)
   (lambda (s font)
     (define-values (gs x)
       (for/fold ([gs '()] [x 0.0]) ([c (in-string s)] [i (in-naturals)])
         (values (cons (sglyph 0 (char->integer c) x i) gs) (+ x (adv c)))))
     (shaped (list->vector (reverse gs)) x))
   (lambda (font) (vector 8.0 2.0 0.0))
   (lambda (font glyph scale subpx)
     (set! fake-raster-calls (add1 fake-raster-calls))
     (if (= glyph 32) #f (vector 4 6 0 6 (make-bytes (* 4 6 4) 255) #f)))))

(define (line-strings s tb)
  (for/list ([l (in-list (text-block-lines tb))]) (substring s (line-start l) (line-end l))))

;; ---------------------------------------------------------------- 断行机会

(test-case "opportunities: after spaces, between ideographs, never inside graphemes"
  (check-equal? (break-opportunities "ab cd  ef") '(3 7))
  (check-equal? (break-opportunities "你好世界") '(1 2 3))
  (check-equal? (break-opportunities "ab你好cd") '(2 3 4))                ; 拉丁 ↔ 表意 两侧可断
  (check-equal? (break-opportunities (string #\a #\space #\e (integer->char #x301) #\x)) '(2))
  (check-equal? (break-opportunities "a 👨‍👩‍👧 b") '(2 8)))                 ; ZWJ 序列是一个字素

(test-case "kinsoku: no break before closing / after opening punctuation"
  (check-equal? (break-opportunities "你好，世界。") '(1 3 4))          ; ，。之前不可断
  (check-equal? (break-opportunities "看（这里）吧") '(1 3 5))           ; （之后、）之前不可断
  (check-equal? (break-opportunities "say (hi) ok") '(4 9)))

(test-case "hyphen between letters and path slashes"
  (check-equal? (break-opportunities "well-known") '(5))
  (check-equal? (break-opportunities "a-1") '())
  (check-equal? (break-opportunities "src/gui/app.rkt") '(4 8)))

;; ---------------------------------------------------------------- 排版

(test-case "greedy wrap on spaces; trailing space not counted"
  (define s "aaa bbb ccc")
  (define tb (layout-text fake s 0 75.0))                 ; 每行最多 7 个拉丁字符
  (check-equal? (line-strings s tb) '("aaa bbb " "ccc"))
  (check-equal? (map line-width (text-block-lines tb)) '(70.0 30.0))
  (check-equal? (map cdr (line-glyphs (second (text-block-lines tb)))) '(0.0 10.0 20.0)))

(test-case "CJK wraps per character, closing punctuation stays on the line"
  (define s "你好，世界")
  (define tb (layout-text fake s 0 50.0))                 ; 2 个表意 + 一个标点放不下
  (check-equal? (line-strings s tb) '("你" "好，" "世界"))
  (check-true (andmap (lambda (l) (<= (line-width l) 50.0)) (cdr (text-block-lines tb)))))

(test-case "emergency break inside an over-long word, at grapheme boundaries"
  (define s (string-append "abcdefghij" (string #\e (integer->char #x301)) "z"))
  (define tb (layout-text fake s 0 35.0))
  (define parts (line-strings s tb))
  (check-equal? (apply string-append parts) s)
  (check-true (for/and ([p (in-list parts)]) (<= (* 10 (for/sum ([c (in-string p)]) (if (eq? (char-general-category c) 'mn) 0 1))) 35)))
  (check-false (for/or ([p (in-list (cdr parts))]) (eq? (char-general-category (string-ref p 0)) 'mn))))

(test-case "hard newlines, empty paragraphs, CR stripped"
  (define tb (layout-text fake "ab\r\n\ncd" 0 1000.0))
  (check-equal? (map (lambda (l) (- (line-end l) (line-start l))) (text-block-lines tb)) '(2 0 2))
  (check-equal? (length (text-block-lines tb)) 3)
  (check-= (text-block-line-height tb) 12.5 1e-9)          ; 1.25 × (8+2)
  (check-= (text-block-baseline tb) (+ 1.25 8.0) 1e-9))

(test-case "every glyph lands on exactly one line"
  (define s "混排 mixed 文本 with some longer words 和标点，以及 emoji 🚀 结束。")
  (define tb (layout-text fake s 0 120.0))
  (check-equal? (for/sum ([l (in-list (text-block-lines tb))]) (length (line-glyphs l))) (string-length s))
  (check-true (for/and ([l (in-list (text-block-lines tb))]) (<= (line-width l) 120.0))))

;; ---------------------------------------------------------------- atlas

(test-case "atlas caches, skips empty glyphs, packs without overlap"
  (set! fake-raster-calls 0)
  (define at (make-atlas fake #:size 64))
  (check-false (atlas-glyph! at 0 32 2.0 0.0))            ; 空格：空字形
  (check-false (atlas-glyph! at 0 32 2.0 0.0))
  (define es (for/list ([g (in-range 65 75)]) (atlas-glyph! at 0 g 2.0 0.0)))
  (check-equal? fake-raster-calls 11)                      ; 空格只光栅化一次
  (check-eq? (atlas-glyph! at 0 65 2.0 0.0) (first es))
  (check-equal? fake-raster-calls 11)
  (define rects (for/list ([e es]) (map (lambda (u) (inexact->exact (round (* 64 u))))
                                        (list (entry-u0 e) (entry-v0 e) (entry-u1 e) (entry-v1 e)))))
  (check-false (for*/or ([a rects] [b rects] #:unless (eq? a b))
                 (and (< (first a) (third b)) (< (first b) (third a)) (< (second a) (fourth b)) (< (second b) (fourth a)))))
  (define d (atlas-take-dirty! at))
  (check-true (vector? d))
  (check-false (atlas-take-dirty! at)))

(test-case "atlas full → flag, reset bumps generation and clears entries"
  (define at (make-atlas fake #:size 16))                   ; 16×16 放不下太多 4×6
  (for ([g (in-range 100 140)]) (atlas-glyph! at 0 g 1.0 0.0))
  (check-true (atlas-full? at))
  (atlas-reset! at)
  (check-false (atlas-full? at))
  (check-equal? (atlas-generation at) 1)
  (check-equal? (atlas-glyph-count at) 0)
  (check-equal? (atlas-take-dirty! at) (vector 0 0 16 16)))

;; ---------------------------------------------------------------- 行内原子框（chip）与光标

(define OBJ (string (integer->char #xFFFC)))

(test-case "inline box: breakable on both sides, takes its given width, never split"
  (check-equal? (break-opportunities (string-append "ab" OBJ "cd")) '(2 3))
  (define s (string-append "aaa " OBJ " bbb"))
  (define tb (layout-text fake s 0 1000.0 #:box-width (lambda (k) 55.0)))
  (define gs (line-glyphs (first (text-block-lines tb))))
  (define box (findf (lambda (gx) (eq? (sglyph-font (car gx)) 'box)) gs))
  (check-equal? (sglyph-glyph (car box)) 0)                 ; 框序号
  (check-equal? (cdr box) 40.0)
  (check-equal? (line-width (first (text-block-lines tb))) (+ 40.0 55.0 10.0 30.0))
  ;; 窄宽度：框单独成行也不被拆
  (define tb2 (layout-text fake s 0 60.0 #:box-width (lambda (k) 55.0)))
  (check-equal? (line-strings s tb2) (list "aaa " (string-append OBJ " ") "bbb")))

(test-case "box ordinals count across paragraphs"
  (define s (string-append OBJ "\nx" OBJ))
  (define tb (layout-text fake s 0 1000.0 #:box-width (lambda (k) (* 10.0 (add1 k)))))
  (define boxes (for*/list ([l (text-block-lines tb)] [gx (line-glyphs l)] #:when (eq? (sglyph-font (car gx)) 'box))
                  (list (sglyph-glyph (car gx)) (sglyph-cluster (car gx)))))
  (check-equal? boxes '((0 0) (1 3))))                       ; cluster 是整串下标

(test-case "caret x / line and click hit-testing round-trip"
  (define s "hello world\nsecond")
  (define tb (layout-text fake s 0 75.0))                    ; "hello " | "world" | "second"
  (check-equal? (line-strings s tb) '("hello " "world" "second"))
  (define-values (k0 x0) (text-block-caret tb 3))
  (check-equal? (list k0 x0) '(0 30.0))
  (define-values (k1 x1) (text-block-caret tb 6))            ; 软换行处 → 下一行行首
  (check-equal? (list k1 x1) '(1 0.0))
  (define-values (k2 x2) (text-block-caret tb 11))           ; "world" 行尾（硬换行前）
  (check-equal? (list k2 x2) '(1 50.0))
  (check-equal? (text-block-index-at tb 31.0 2.0) 3)
  (check-equal? (text-block-index-at tb 36.0 2.0) 4)          ; 过半 → 后一个位置
  (check-equal? (text-block-index-at tb 500.0 14.0) 11)       ; 行尾右侧
  (check-equal? (text-block-index-at tb 0.0 99.0) 12))        ; 下方 → 最后一行

;; ---------------------------------------------------------------- 多样式段落（layout-runs）

(test-case "layout-runs: runs concatenate, break across run boundaries, clusters map back to runs"
  (define runs (list (cons "plain " 0) (cons "bold words" 1) (cons " tail" 0)))
  (define-values (tb ranges) (layout-runs fake runs 80.0 #:base-font 0))
  (check-equal? (line-strings "plain bold words tail" tb) '("plain " "bold " "words " "tail"))
  (check-equal? (vector->list (vector-map vector->list ranges)) '((0 6 0) (6 16 1) (16 21 2)))
  (check-equal? (run-index-at ranges 7) 1)
  (check-equal? (run-index-at ranges 17) 2)
  ;; 字形 x 连续：第 2 个片段首字形紧接第 1 个片段
  (define gs (append-map line-glyphs (text-block-lines tb)))
  (check-equal? (sglyph-x (car (list-ref gs 6))) 60.0))

(test-case "layout-runs: empty runs are harmless; newlines inside runs become spaces"
  (define-values (tb ranges) (layout-runs fake (list (cons "" 0) (cons "a\nb" 1)) 1000.0 #:base-font 0))
  (check-equal? (length (text-block-lines tb)) 1)
  (check-equal? (line-width (car (text-block-lines tb))) 30.0))
