#lang racket/base
;; mac-audio.rkt — 音频播放（决定 2：走系统框架 AVFoundation 的 AVAudioPlayer，经自写 objc 桥）。
;; 只在播放子进程里用（components/mac-player.rkt）：不可信的音频字节不在 GUI 进程里解析。
;;   player-open：字节 → 播放器或 #f；player-duration / player-position / player-playing?；player-play! / player-pause! / player-seek! / player-close!
;;   audio-duration：字节 → 秒或 #f（附件探测时长）
;;   sandbox-player!：进程自我沙箱（禁网络、禁写文件、禁起进程；读文件放开：音频框架要加载自己的组件）

(require ffi/unsafe "mac-objc.rkt")

(define _libav (ffi-lib "/System/Library/Frameworks/AVFoundation.framework/AVFoundation"))
(define _libfoundation (ffi-lib "/System/Library/Frameworks/Foundation.framework/Foundation"))

(define-msg m-data-with-bytes "dataWithBytes:length:" _id ([p _bytes] [n _uint64]))
(define-msg m-init-with-data "initWithData:error:" _id ([d _id] [err _pointer]))
(define-msg m-prepare "prepareToPlay" _BOOL ())
(define-msg m-play "play" _BOOL ())
(define-msg m-pause "pause" _void ())
(define-msg m-stop "stop" _void ())
(define-msg m-duration "duration" _double ())
(define-msg m-current-time "currentTime" _double ())
(define-msg m-set-current-time "setCurrentTime:" _void ([t _double]))
(define-msg m-playing? "isPlaying" _BOOL ())

(struct player (obj))

(define (player-open bs)
  (call-with-autorelease-pool
   (lambda ()
     (define data (m-data-with-bytes (cls "NSData") bs (bytes-length bs)))
     (define errp (malloc _pointer 'raw))
     (ptr-set! errp _pointer #f)
     (define p (m-init-with-data (msg-alloc (cls "AVAudioPlayer")) data errp))
     (free errp)
     (and p (begin (m-prepare p) (player p))))))

(define (player-duration p) (m-duration (player-obj p)))
(define (player-position p) (m-current-time (player-obj p)))
(define (player-playing? p) (m-playing? (player-obj p)))
(define (player-play! p) (m-play (player-obj p)))
(define (player-pause! p) (m-pause (player-obj p)))
(define (player-seek! p t) (m-set-current-time (player-obj p) (exact->inexact t)))
(define (player-close! p) (m-stop (player-obj p)) (release! (player-obj p)))

(define (audio-duration bs)
  (define p (player-open bs))
  (and p (begin0 (player-duration p) (player-close! p))))

;; ---------------------------------------------------------------- 自我沙箱（自定义 SBPL 档案，flags 0）

(define libsys (ffi-lib #f))
(define sandbox_init (get-ffi-obj 'sandbox_init libsys (_fun _string _uint64 (err : (_ptr o _pointer)) -> (r : _int) -> (values r err))))
(define PLAYER-PROFILE "(version 1) (allow default) (deny network*) (deny file-write*) (deny process-exec) (deny process-fork)")

(define (sandbox-player!)
  (define-values (r err) (sandbox_init PLAYER-PROFILE 0))
  (unless (zero? r) (error 'sandbox-player! "sandbox_init failed: ~a" (if err (cast err _pointer _string) "?")))
  #t)

(provide player-open player-duration player-position player-playing? player-play! player-pause! player-seek! player-close!
         audio-duration sandbox-player!)
