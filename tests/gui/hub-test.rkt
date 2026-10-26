#lang racket/base
;; tests/gui/hub-test.rkt — 组件接口发现与装配（计算层 hub.rkt）

(require rackunit "../../src/gui/compute/hub.rkt")

(define-interface greeter greet)
(define-interface counter inc! value)
(define-interface clock now)

;; counter 组件：需要 clock，提供 counter；greeter 组件：需要 counter，提供 greeter
(define counter-comp
  (component 'counter (list clock) (list counter)
             (lambda (use config)
               (define c (use clock))
               (define n (hash-ref config 'start 0))
               (list (implement counter
                                'inc! (lambda () (set! n (+ n 1)) (c 'now))
                                'value (lambda () n))))))
(define greeter-comp
  (component 'greeter (list counter) (list greeter)
             (lambda (use config)
               (define k (use counter))
               (list (implement greeter 'greet (lambda (who) (k 'inc!) (format "hi ~a #~a" who (k 'value))))))))

(define fake-clock (implement clock 'now (lambda () 42)))

(test-case "implement: methods must match the interface exactly"
  (check-exn #rx"missing \\(value\\)" (lambda () (implement counter 'inc! void)))
  (check-exn #rx"extra \\(oops\\)" (lambda () (implement clock 'now void 'oops void)))
  (check-exn #rx"not a procedure" (lambda () (implement clock 'now 1)))
  (check-exn #rx"no method" (lambda () (fake-clock 'later))))

(test-case "assemble discovers order from needs/provides regardless of list order"
  (define-values (env order)
    (assemble (list greeter-comp counter-comp) #:env (hasheq 'clock fake-clock) #:config (hasheq 'counter (hasheq 'start 10))))
  (check-equal? order '(counter greeter))
  (define g (env-ref env greeter))
  (check-equal? (g 'greet "pi") "hi pi #11")
  (check-equal? ((env-ref env counter) 'value) 11))

(test-case "unit test of one component with a fake dependency (no other component needed)"
  (define calls 0)
  (define fake-counter (implement counter 'inc! (lambda () (set! calls (add1 calls))) 'value (lambda () 7)))
  (define env (instantiate greeter-comp (hasheq 'counter fake-counter)))
  (check-equal? ((env-ref env greeter) 'greet "x") "hi x #7")
  (check-equal? calls 1))

(test-case "errors: unresolved needs, undeclared use, wrong provides, double provider"
  (check-exn #rx"unresolved needs: \\(\\(counter clock\\)\\)" (lambda () (assemble (list counter-comp))))
  (define sneaky (component 'sneaky '() (list greeter)
                            (lambda (use config) (use clock) (list (implement greeter 'greet void)))))
  (check-exn #rx"without declaring" (lambda () (instantiate sneaky (hasheq 'clock fake-clock))))
  (define liar (component 'liar '() (list greeter) (lambda (use config) (list fake-clock))))
  (check-exn #rx"declared provides" (lambda () (instantiate liar (hasheq))))
  (check-exn #rx"already provided" (lambda () (assemble (list counter-comp counter-comp) #:env (hasheq 'clock fake-clock)))))
