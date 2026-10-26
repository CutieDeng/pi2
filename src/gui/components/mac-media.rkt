#lang racket/base
;; mac-media.rkt — 媒体组件（P4）：图片缩略图（解码子进程 + 图片纹理池）与音频播放（播放子进程）。
;; 不可信的媒体字节不在 GUI 进程里解析（design-gui.md §7）：
;;   解码子进程：racket mac-media.rkt decode —— 预热（解一张小 PNG，加载 ImageIO）后 sandbox_init("pure-computation")：
;;     不能读写文件、不能联网、不能起进程，只从 stdin 收字节、往 stdout 写像素。
;;   播放子进程：racket mac-media.rkt play —— sandbox_init 自定义档案：禁网络、禁写文件、禁起进程（AVFoundation 要读自己的组件）。
;; 子进程崩了（恶意文件）：在途的请求记为失败，下次用到时重启。
;; 管道上的帧（小端）：op u8 | 键长 u32 | 键 | 参数 u32 | 载荷长 u32 | 载荷
;;   → 解码：op 1 参数 = 长边像素上限，载荷 = 图片字节；← op 1 键 载荷 = 宽 u32 高 u32 原宽 u32 原高 u32 + RGBA；出错 ← op 9 载荷 = 原因
;;   → 时长：op 2 载荷 = 音频字节；← op 2 载荷 = 秒（f64）
;;   → 播放：op 10 打开并播放（载荷 = 音频字节）/ 11 暂停 / 12 继续 / 13 停止 / 14 跳到（载荷 = 秒 f64）；← op 20 键 载荷 = 位置 f64 时长 f64 在播 u8
;; needs （无）→ provides media-port。config：'audio?（回放 / 离屏时 #f：不出声）

(require racket/list racket/runtime-path "../compute/hub.rkt" "../compute/interfaces.rkt" "../compute/image-atlas.rkt")

(define-runtime-path SELF "mac-media.rkt")
(define THUMB-PX 480)                                 ; 缩略图长边（像素；2× 屏上显示 240 point）
(define DECODED-CACHE-BYTES (* 96 1024 1024))         ; 解码好的缩略图在内存里留多少（LRU）

;; ---------------------------------------------------------------- 帧

(define (write-u32 n out) (write-bytes (integer->integer-bytes n 4 #f #f) out))
(define (read-u32 in) (let ([b (read-bytes 4 in)]) (if (and (bytes? b) (= (bytes-length b) 4)) (integer-bytes->integer b #f #f) (raise 'eof))))
(define (write-frame! out op key arg payload)
  (define kb (string->bytes/utf-8 key))
  (write-byte op out) (write-u32 (bytes-length kb) out) (write-bytes kb out)
  (write-u32 arg out) (write-u32 (bytes-length payload) out) (write-bytes payload out)
  (flush-output out))
;; → (values op 键 参数 载荷)；EOF → raise 'eof
(define (read-frame in)
  (define op (read-byte in))
  (when (eof-object? op) (raise 'eof))
  (define key (bytes->string/utf-8 (read-bytes* (read-u32 in) in) #\?))
  (define arg (read-u32 in))
  (define payload (read-bytes* (read-u32 in) in))
  (values op key arg payload))
(define (read-bytes* n in)
  (if (zero? n) #"" (let ([b (read-bytes n in)]) (if (and (bytes? b) (= (bytes-length b) n)) b (raise 'eof)))))
(define (f64 x) (real->floating-point-bytes (exact->inexact x) 8 #f))
(define (f64-ref b o) (floating-point-bytes->real b #f o (+ o 8)))

;; ---------------------------------------------------------------- 子进程

(define (racket-exe)
  (define p (find-system-path 'exec-file))
  (or (and (absolute-path? p) p) (find-executable-path p) p))

(struct worker (proc in out [alive? #:mutable]))

;; on-frame：在读线程里调 (op 键 参数 载荷)；on-exit：子进程断了
(define (spawn-worker mode on-frame on-exit)
  (define-values (p out in err) (subprocess #f #f #f (racket-exe) (path->string SELF) mode))
  (thread (lambda () (let loop () (define l (read-line err)) (unless (eof-object? l) (loop)))))   ; 丢掉 stderr
  (define w (worker p in out #t))
  (thread (lambda ()
            (with-handlers ([(lambda (e) #t) (lambda (_e) (void))])
              (let loop () (define-values (op key arg payload) (read-frame out)) (on-frame op key arg payload) (loop)))
            (set-worker-alive?! w #f)
            (on-exit)))
  w)

(define (worker-send! w op key arg payload)
  (with-handlers ([exn:fail? (lambda (_e) (set-worker-alive?! w #f) #f)])
    (write-frame! (worker-in w) op key arg payload) #t))

;; ---------------------------------------------------------------- 组件

(define media-component
  (component 'media '() (list media-port)
             (lambda (use config)
               (define audio? (hash-ref config 'audio? #t))
               (define lock (make-semaphore 1))
               (define-syntax-rule (locked e ...) (call-with-semaphore lock (lambda () e ...)))
               (define atlas (make-image-atlas 2048))
               (define decoded (make-hash))           ; 键 → (vector 宽 高 原宽 原高 RGBA 最近用到的序号)
               (define decoded-bytes 0)
               (define status (make-hash))            ; 键 → 'pending | 'failed
               (define info (make-hash))              ; 键 → hash（音频时长）
               (define audio-state (make-hash))       ; 键 → (vector 在播? 位置 时长)
               (define changed? #f)
               (define tick 0)
               (define decoder #f)
               (define player #f)
               (define in-flight '())                 ; 解码子进程在途的键
               (define (evict!)
                 (when (> decoded-bytes DECODED-CACHE-BYTES)
                   (for ([k (in-list (sort (hash-keys decoded) < #:key (lambda (k) (vector-ref (hash-ref decoded k) 5))))]
                         #:break (<= decoded-bytes (* 3/4 DECODED-CACHE-BYTES)))
                     (set! decoded-bytes (- decoded-bytes (bytes-length (vector-ref (hash-ref decoded k) 4))))
                     (hash-remove! decoded k))))
               (define (on-decoder-frame op key arg payload)
                 (locked
                  (set! in-flight (remove key in-flight))
                  (set! changed? #t)
                  (case op
                    [(1) (define (u i) (integer-bytes->integer payload #f #f (* 4 i) (* 4 (add1 i))))
                         (hash-set! decoded key (vector (u 0) (u 1) (u 2) (u 3) (subbytes payload 16) tick))
                         (set! decoded-bytes (+ decoded-bytes (- (bytes-length payload) 16)))
                         (hash-remove! status key)
                         (evict!)]
                    [(2) (hash-set! info key (hasheq 'duration (f64-ref payload 0))) (hash-remove! status key)]
                    [else (hash-set! status key 'failed)])))
               (define (on-decoder-exit)
                 (locked (for ([k (in-list in-flight)]) (hash-set! status k 'failed))
                         (set! in-flight '()) (set! decoder #f) (set! changed? #t)))
               (define (ensure-decoder!)
                 (unless (and decoder (worker-alive? decoder))
                   (set! decoder (spawn-worker "decode" on-decoder-frame on-decoder-exit)))
                 decoder)
               (define (on-player-frame op key arg payload)
                 (when (= op 20)
                   (locked (hash-set! audio-state key (vector (= 1 (bytes-ref payload 16)) (f64-ref payload 0) (f64-ref payload 8)))
                           (set! changed? #t))))
               (define (ensure-player!)
                 (unless (and player (worker-alive? player))
                   (set! player (spawn-worker "play" on-player-frame (lambda () (locked (set! player #f) (hash-clear! audio-state) (set! changed? #t))))))
                 player)
               (define (request! op key arg bs)
                 (locked
                  (unless (or (hash-ref status key #f) (and (= op 1) (hash-ref decoded key #f)) (and (= op 2) (hash-ref info key #f)))
                    (hash-set! status key 'pending)
                    (set! in-flight (cons key in-flight))
                    (unless (worker-send! (ensure-decoder!) op key arg bs) (hash-set! status key 'failed)))))
               (list
                (implement media-port
                           'begin-frame! (lambda () (set! tick (add1 tick)) (image-atlas-begin-frame! atlas))
                           ;; 缩略图：→ #f | (vector u0 v0 u1 v1 宽 高 原宽 原高)（宽高：像素）
                           'thumb (lambda (key)
                                    (locked
                                     (define d (hash-ref decoded key #f))
                                     (and d
                                          (begin
                                            (vector-set! d 5 tick)
                                            (let ([uv (image-atlas-put! atlas key (vector-ref d 0) (vector-ref d 1) (vector-ref d 4))])
                                              (and uv (vector (vector-ref uv 0) (vector-ref uv 1) (vector-ref uv 2) (vector-ref uv 3)
                                                              (vector-ref d 0) (vector-ref d 1) (vector-ref d 2) (vector-ref d 3))))))))
                           'decode! (lambda (key bs) (request! 1 key THUMB-PX bs))
                           'probe-audio! (lambda (key bs) (request! 2 key 0 bs))
                           'info (lambda (key) (locked (hash-ref info key #f)))
                           'status (lambda (key) (locked (cond [(hash-ref decoded key #f) 'ready] [(hash-ref info key #f) 'ready] [else (hash-ref status key 'none)])))
                           'poll! (lambda () (locked (begin0 changed? (set! changed? #f))))
                           ;; 图片纹理池的脏区：→ #f | (cons 位图 (vector x y w h))
                           'take-upload! (lambda () (locked (let ([d (image-atlas-take-dirty! atlas)]) (and d (cons (image-atlas-bitmap atlas) d)))))
                           'atlas-size (lambda () (image-atlas-size atlas))
                           'play! (lambda (key bs)
                                    (when audio?
                                      (locked (for ([(k v) (in-hash audio-state)]) (hash-set! audio-state k (vector #f (vector-ref v 1) (vector-ref v 2)))))   ; 换一段：别的不再「在播」
                                      (worker-send! (ensure-player!) 10 key 0 bs)))
                           'pause! (lambda (key) (when (and audio? player) (worker-send! player 11 key 0 #"")))
                           'resume! (lambda (key) (when (and audio? player) (worker-send! player 12 key 0 #"")))
                           'stop! (lambda () (when (and audio? player) (worker-send! player 13 "" 0 #"")))
                           'seek! (lambda (key secs) (when (and audio? player) (worker-send! player 14 key 0 (f64 secs))))
                           ;; → #f | (vector 在播? 位置 时长)
                           'audio (lambda (key) (locked (hash-ref audio-state key #f)))
                           'close! (lambda ()
                                     (for ([w (list decoder player)] #:when w)
                                       (with-handlers ([exn:fail? void]) (close-output-port (worker-in w)) (subprocess-kill (worker-proc w) #t)))))))))

(provide media-component)

;; ---------------------------------------------------------------- 子进程入口

(module+ main
  (require racket/port net/base64)
  (define mode (let ([a (current-command-line-arguments)]) (if (> (vector-length a) 0) (vector-ref a 0) "")))
  (define in (current-input-port))
  (define out (current-output-port))
  (file-stream-buffer-mode out 'block)
  (define (err! key msg) (write-frame! out 9 key 0 (string->bytes/utf-8 msg)))
  (case mode
    [("decode")
     (define decode-thumbnail (dynamic-require (collapse "../compute/mac-imageio.rkt") 'decode-thumbnail))
     (define sandbox-self! (dynamic-require (collapse "../compute/mac-imageio.rkt") 'sandbox-self!))
     (define audio-duration (dynamic-require (collapse "../compute/mac-audio.rkt") 'audio-duration))
     ;; 预热：ImageIO / AVFoundation 的组件在沙箱前加载（2×2 的 PNG；其它格式的解码器进沙箱后也能用，实测 JPEG GIF HEIC TIFF）
     (decode-thumbnail (base64-decode #"iVBORw0KGgoAAAANSUhEUgAAAAIAAAACCAYAAABytg0kAAAAEUlEQVR4nGP4z8DwH4QZYAwAR8oH+WdZbrcAAAAASUVORK5CYII=") 8)
     (audio-duration #"warm-up")
     (sandbox-self!)
     (let loop ()
       (define-values (op key arg payload) (with-handlers ([(lambda (e) (eq? e 'eof)) (lambda (_e) (exit 0))]) (read-frame in)))
       (case op
         [(1) (define-values (w h px ow oh) (decode-thumbnail payload (max 16 arg)))
              (if w
                  (write-frame! out 1 key 0 (bytes-append (integer->integer-bytes w 4 #f #f) (integer->integer-bytes h 4 #f #f)
                                                          (integer->integer-bytes (or ow w) 4 #f #f) (integer->integer-bytes (or oh h) 4 #f #f) px))
                  (err! key h))]
         [(2) (define d (audio-duration payload)) (if d (write-frame! out 2 key 0 (f64 d)) (err! key "not playable audio"))]
         [else (err! key "unknown op")])
       (loop))]
    [("play")
     (define-values (player-open player-duration player-position player-playing? player-play! player-pause! player-seek! player-close! sandbox-player!)
       (apply values (for/list ([n '(player-open player-duration player-position player-playing? player-play! player-pause! player-seek! player-close! sandbox-player!)])
                       (dynamic-require (collapse "../compute/mac-audio.rkt") n))))
     (sandbox-player!)
     (define cur #f) (define cur-key #f)
     (define (report!)
       (when cur (write-frame! out 20 cur-key 0 (bytes-append (f64 (player-position cur)) (f64 (player-duration cur)) (bytes (if (player-playing? cur) 1 0))))))
     (let loop ()
       (when (sync/timeout 0.1 in)
         (define-values (op key arg payload) (with-handlers ([(lambda (e) (eq? e 'eof)) (lambda (_e) (exit 0))]) (read-frame in)))
         (case op
           [(10) (when cur (player-close! cur)) (set! cur (player-open payload)) (set! cur-key key)
                 (if cur (player-play! cur) (err! key "not playable audio"))]
           [(11) (when (and cur (equal? key cur-key)) (player-pause! cur))]
           [(12) (when (and cur (equal? key cur-key)) (player-play! cur))]
           [(13) (when cur (player-pause! cur) (report!) (player-close! cur) (set! cur #f))]
           [(14) (when (and cur (equal? key cur-key)) (player-seek! cur (f64-ref payload 0)))])
         (report!))
       (when (and cur (player-playing? cur)) (report!))
       (loop))]
    [else (eprintf "mac-media: unknown mode ~a\n" mode) (exit 2)]))

(define (collapse rel) (simplify-path (build-path SELF 'up rel)))
