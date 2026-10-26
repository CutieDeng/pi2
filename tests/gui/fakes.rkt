#lang racket/base
;; tests/gui/fakes.rkt — 组件单元测试用的假接口实现（不依赖平台）。
;; 假字体：拉丁 10pt、表意 20pt、组合符 0pt；每码点一个字形；度量 asc 8 / desc 2 → 行高 12.5。

(require "../../src/gui/compute/hub.rkt" "../../src/gui/compute/interfaces.rkt"
         "../../src/gui/compute/text-backend.rkt" "../../src/gui/compute/layout.rkt" "../../src/gui/compute/linebreak.rkt"
         "../../src/gui/compute/atlas.rkt" "../../src/gui/compute/rich.rkt")

(define (adv c) (cond [(eq? (char-general-category c) 'mn) 0] [(ideographic? c) 20] [else 10]))

(define fake-backend
  (text-backend
   (lambda (family size) 0)
   (lambda (s font)
     (define-values (gs x)
       (for/fold ([gs '()] [x 0.0]) ([c (in-string s)] [i (in-naturals)])
         (values (cons (sglyph 0 (char->integer c) x i) gs) (+ x (adv c)))))
     (shaped (list->vector (reverse gs)) x))
   (lambda (font) (vector 8.0 2.0 0.0))
   (lambda (font glyph scale subpx) (if (= glyph 32) #f (vector 4 6 0 6 (make-bytes 96 255) #f)))))

;; 假 text-layout：真排版算法 + 假字体；只有 rich / layout / atlas 有意义
(define (make-fake-text-layout)
  (define at (make-atlas fake-backend #:size 1024))
  (define (lay s k w [bw #f]) (layout-text fake-backend s 0 w #:box-width bw))
  (define (lay-runs runs base w) (layout-runs fake-backend (for/list ([r runs]) (cons (car r) 0)) w #:base-font 0))
  (implement text-layout
             'layout lay
             'rich (lambda () (rich-ctx lay at lay-runs))
             'atlas (lambda () at)
             'build-frame! (lambda (dl thunk) (thunk) 0)
             'take-dirty! (lambda () #f)
             'glyph-count (lambda () (atlas-glyph-count at))))

;; 假 media-port：记下调用（fake-media-calls：最近建的那个的调用表）；缩略图在 decode! 之后「就绪」（纹理坐标固定），音频 play! 之后在播
(define last-calls (box (lambda () '())))
(define (fake-media-calls) ((unbox last-calls)))
(define (make-fake-media)
  (define calls '())
  (set-box! last-calls (lambda () (reverse calls)))
  (define decoded (make-hash))
  (define playing (make-hash))
  (define (log! . x) (set! calls (cons x calls)))
  (implement media-port
             'begin-frame! void
             'thumb (lambda (k) (and (hash-ref decoded k #f) (vector 0.0 0.0 0.25 0.25 480 340 1920 1360)))
             'decode! (lambda (k bs) (log! 'decode! k (bytes-length bs)) (hash-set! decoded k #t))
             'probe-audio! (lambda (k bs) (log! 'probe-audio! k))
             'info (lambda (k) #f)
             'status (lambda (k) (if (hash-ref decoded k #f) 'ready 'none))
             'poll! (lambda () #f)
             'take-upload! (lambda () #f)
             'atlas-size (lambda () 2048)
             'play! (lambda (k bs) (log! 'play! k (bytes-length bs)) (hash-set! playing k (vector #t 0.0 3.0)))
             'pause! (lambda (k) (log! 'pause! k) (hash-update! playing k (lambda (v) (vector #f (vector-ref v 1) (vector-ref v 2)))))
             'resume! (lambda (k) (log! 'resume! k) (hash-update! playing k (lambda (v) (vector #t (vector-ref v 1) (vector-ref v 2)))))
             'stop! void 'seek! void
             'audio (lambda (k) (hash-ref playing k #f))
             'close! void))

;; 只为 needs 提供假实现，实例化单个组件，返回它提供的某个接口
(define (instantiate-with comp provided-iface #:text-layout [tl (make-fake-text-layout)] #:media [media (make-fake-media)])
  (env-ref (instantiate comp (hasheq 'text-layout tl 'media-port media)) provided-iface))

(provide fake-backend make-fake-text-layout make-fake-media fake-media-calls instantiate-with)
