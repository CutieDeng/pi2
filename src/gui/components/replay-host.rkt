#lang racket/base
;; replay-host.rkt — 回放组件（G6）：把一份操作日志当成「窗口 + 内核」，拼装层的主循环原样运行（无窗口、无内核进程）。
;; 主循环每轮先 poll! 内核事件、再 pump! 窗口事件；这里按日志顺序，每次交出队首连续的同类记录，
;; 因此两类事件的相对顺序与录制时一致。日志耗尽后 pump! 交出 ev:close，主循环正常退出。
;; 回放中发给内核的请求只记下来（不执行），结束时与日志里的 'send 记录比对（翻页请求记成 'page，不比对）。
;; needs （无）→ provides window-port window-input clipboard kernel-set replay-source
;; 多主机：内核事件 / 请求按 '_host 键分主机（没有 = "local"）；add! / remove! 只是登记（没有真连接）
;; config：'records（read-journal 的记录表）'header（头信息：w h scale）

(require racket/list
         "../compute/event.rkt" "../compute/hub.rkt" "../compute/interfaces.rkt" "../compute/journal-format.rkt")

(define PAGING-TYPES '("items" "item" "index_summary" "select_lines" "blob" "set_catalog_view" "auto_title"))   ; set_catalog_view：目录视图（派生状态，§11.7）

(struct rh ([queue #:mutable] total [consumed #:mutable] [sent #:mutable] expected clip))

;; 队首连续的 kind 记录（跳过 send / error / note / start：它们是录制时的输出或元信息，不是输入）。
;; 'frame 是录制时的一次重绘：命中区在重绘时才更新，所以窗口事件不能越过它合成一批——
;; 取窗口事件时遇到 'frame 就停下并消费掉它（这一批 = 录制时两次重绘之间的事件）；取内核事件时停下不消费。
(define (take-run! r kind)
  (let loop ([acc '()])
    (define q (rh-queue r))
    (cond
      [(null? q) (reverse acc)]
      [(eq? (third (car q)) 'frame)
       (when (eq? kind 'win) (set-rh-queue! r (cdr q)) (set-rh-consumed! r (add1 (rh-consumed r))))
       (reverse acc)]
      [(memq (third (car q)) '(send script-send page error note start))
       (set-rh-queue! r (cdr q)) (set-rh-consumed! r (add1 (rh-consumed r))) (loop acc)]
      [(eq? (third (car q)) kind)
       (set-rh-queue! r (cdr q)) (set-rh-consumed! r (add1 (rh-consumed r)))
       (loop (cons (fourth (car q)) acc))]
      [else (reverse acc)]))
) ; end define take-run!

(define replay-host-component
  (component 'replay-host '() (list window-port window-input clipboard kernel-set replay-source)
             (lambda (use config)
               (define recs (hash-ref config 'records '()))
               (define header (hash-ref config 'header (hasheq)))
               (define r (rh recs (length recs) 0 '()
                             (for/list ([x (in-list recs)] #:when (eq? (third x) 'send)) (fourth x))
                             (box #f)))
               (define (noop . _) (void))
               (list
                (implement window-port
                           'size (lambda () (values (hash-ref header 'w 960.0) (hash-ref header 'h 680.0) (hash-ref header 'scale 2.0)))
                           'pump! (lambda ()
                                    (define evs (take-run! r 'win))
                                    (if (and (null? evs) (null? (rh-queue r))) (list (ev:close)) evs))
                           'set-text-input-state! noop 'discard-marked! noop 'on-live-resize! noop
                           'set-content-size! noop 'close! noop 'set-cursor! noop 'glass? (lambda () #f) 'set-glass-rect! noop 'set-transparent! noop
                           'capture (lambda (_h _b) #f) 'set-text-input-enabled! noop)
                (implement window-input
                           'post-mouse! noop 'post-mouse-ns! noop 'post-scroll! noop 'post-key! noop 'post-key-cg! noop 'post-paste! noop 'post-attach! noop 'simulate-ime! noop
                           'ime-query (lambda () (values #f #f)) 'ime-diagnostics (lambda () "replay") 'key? (lambda () #t))
                (implement clipboard
                           'get (lambda () (unbox (rh-clip r))) 'set! (lambda (s) (set-box! (rh-clip r) s))
                           'call-with-private (lambda (thunk) (thunk)))
                (let ([ids '()])
                  (implement kernel-set
                             'add! (lambda (id _cfg) (unless (member id ids) (set! ids (append ids (list id)))))
                             'remove! (lambda (id [_t #f]) (set! ids (remove id ids)) 0)
                             'ids (lambda () ids)
                             ;; 翻页 / 挑预览行的请求（items / item / index_summary / select_lines）随帧与视口而定，不比对；应答照日志给
                             'send! (lambda (id j) (unless (member (hash-ref j 'type #f) PAGING-TYPES) (set-rh-sent! r (cons (host-tag id (redact-send j)) (rh-sent r)))) #t)
                             'poll! (lambda () (map host-untag (take-run! r 'core)))
                             'alive? (lambda (_id) #t)
                             'stderr-tail (lambda (_id [_n 8]) '())
                             'close-all! (lambda ([_t #f]) (for/list ([id (in-list ids)]) (cons id 0)))))
                (implement replay-source
                           'sent (lambda () (reverse (rh-sent r)))
                           'expected (lambda () (rh-expected r))
                           'progress (lambda () (cons (rh-consumed r) (rh-total r))))))))

(provide replay-host-component)
