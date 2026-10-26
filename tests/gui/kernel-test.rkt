#lang racket/base
;; tests/gui/kernel-test.rkt — 内核连接组件（kernel-set）：只实例化这一个组件；place 传输用脚本化内核；两个主机同时连着

(require rackunit racket/runtime-path racket/list
         "../../src/gui/compute/hub.rkt" "../../src/gui/compute/interfaces.rkt"
         "../../src/gui/components/kernels.rkt")

(define-runtime-path SCRIPTED "fixtures/scripted-core.rkt")
(define-runtime-path CRASH "fixtures/crash-core.rkt")
(define-runtime-path EXIT3 "fixtures/exit3-core.rkt")

(define (new-ks) (env-ref (instantiate kernels-component (hasheq)) kernel-set))
;; 单主机视图：测试里按原来的 kernel-port 方式用
(define (start entry)
  (define ks (new-ks))
  (ks 'add! "k" (hasheq 'entry entry 'args '()))
  (lambda (m . args)
    (case m
      [(send!) (ks 'send! "k" (car args))]
      [(poll!) (map cdr (ks 'poll!))]
      [(close!) (ks 'remove! "k" (if (pair? args) (car args) 5.0))]
      [(alive?) (ks 'alive? "k")]
      [(stderr-tail) (ks 'stderr-tail "k" (if (pair? args) (car args) 20))])))

(define (wait-for k type [timeout 20.0])
  (define deadline (+ (current-inexact-milliseconds) (* 1000 timeout)))
  (let loop ([seen '()])
    (define evs (k 'poll!))
    (define hit (findf (lambda (e) (equal? (hash-ref e 'type #f) type)) evs))
    (cond [hit (values hit (append seen evs))]
          [(> (current-inexact-milliseconds) deadline) (values #f (append seen evs))]
          [else (sync (alarm-evt (+ (current-inexact-milliseconds) 10))) (loop (append seen evs))])))

(test-case "kernel place: ready → prompt streams → shutdown → core_exit 0; send! after exit returns #f"
  (define k (start SCRIPTED))
  (define-values (ready _s) (wait-for k "ready"))
  (check-equal? (hash-ref ready 'protocol) 3)
  (check-true (k 'send! (hasheq 'type "prompt" 'text "hello there")))
  (define-values (tc seen) (wait-for k "turn_complete"))
  (check-not-false tc)
  (check-true (> (count (lambda (e) (equal? (hash-ref e 'type #f) "delta")) seen) 1))   ; 流式多段
  (k 'send! (hasheq 'type "shutdown"))
  (define-values (ex _s2) (wait-for k "core_exit"))
  (check-equal? (hash-ref ex 'code) 0)
  (check-false (k 'alive?))
  (check-false (k 'send! (hasheq 'type "ping"))))

(test-case "close! closes the kernel's stdin; the kernel exits on its own"
  (define k (start SCRIPTED))
  (wait-for k "ready")
  (check-equal? (k 'close! 10.0) 0))

(test-case "a kernel that raises at startup ends only its place: core_exit 70 + stderr tail"
  (define k (start CRASH))
  (define-values (ex _s) (wait-for k "core_exit"))
  (check-equal? (hash-ref ex 'code) 70)
  (check-not-false (member "about to fail" (k 'stderr-tail)))
  (check-not-false (findf (lambda (l) (regexp-match? #rx"boom-at-start" l)) (k 'stderr-tail))))

(test-case "(exit 3) inside the kernel ends only its place; this process keeps running"
  (define k (start EXIT3))
  (define-values (ex _s) (wait-for k "core_exit"))
  (check-equal? (hash-ref ex 'code) 3)
  (check-true #t))                                              ; 走到这里说明测试进程没被 exit 带走

(test-case "two hosts at once: events come back tagged with their host id; removing one leaves the other"
  (define ks (new-ks))
  (ks 'add! "a" (hasheq 'entry SCRIPTED 'args '()))
  (ks 'add! "b" (hasheq 'entry SCRIPTED 'args '()))
  (check-equal? (ks 'ids) '("a" "b"))
  (define all '())                                              ; 收到的全部 (id . 事件)：等某个主机时别的主机的事件不能丢
  (define (wait-host id type)
    (define deadline (+ (current-inexact-milliseconds) 20000))
    (let loop ()
      (set! all (append all (ks 'poll!)))
      (cond [(findf (lambda (p) (and (equal? (car p) id) (equal? (hash-ref (cdr p) 'type #f) type))) all) all]
            [(> (current-inexact-milliseconds) deadline) #f]
            [else (sync (alarm-evt (+ (current-inexact-milliseconds) 10))) (loop)])))
  (check-not-false (wait-host "a" "ready"))
  (check-not-false (wait-host "b" "ready"))
  (ks 'send! "b" (hasheq 'type "prompt" 'text "to b"))
  (define seen (wait-host "b" "turn_complete"))
  (check-not-false seen)
  (check-false (findf (lambda (p) (and (equal? (car p) "a") (equal? (hash-ref (cdr p) 'type #f) "turn_start"))) seen))   ; a 没收到 b 的轮
  (check-equal? (ks 'remove! "a" 10.0) 0)
  (check-equal? (ks 'ids) '("b"))
  (check-false (ks 'send! "a" (hasheq 'type "ping")))
  (check-true (ks 'alive? "b"))
  (check-equal? (map cdr (ks 'close-all! 10.0)) '(0)))
