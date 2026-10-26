#lang tstring racket
;; hidden-text.rkt — 送给模型的消息里的「隐藏文字」（Unicode tag 字符 U+E0000–E007F：界面上不可见、模型照读，
;; 可被用来夹带指令）。design-gui.md §30.4；用户（2026-10-25）：「两种机制都做吧，可以在高级设置中调整」。
;;   mark（缺省）：每段 tag 串换成显式的 ⟦hidden text (Unicode tag characters): 解码出的文字⟧——模型看得到「这里藏了东西」
;;   raw：原样
;; 只在发给模型时转换（provider-stream! 入口）；会话记录里始终是原文（审计、切换模式不丢东西）。

(require (file "model.rkt"))

(define hidden-text-mode (box 'mark))                    ; 'mark | 'raw；界面经 RPC set_model_input 设置
(define (set-hidden-text-mode! m) (when (memq m '(mark raw)) (set-box! hidden-text-mode m)))

(define TAG-RUN (pregexp (string #\[ (integer->char #xE0000) #\- (integer->char #xE007F) #\] #\+)))

(define (decode-tags run)
  (list->string (for/list ([c (in-string run)] #:when (<= #xE0020 (char->integer c) #xE007E))
                  (integer->char (- (char->integer c) #xE0000)))))

;; 字符串 → 给模型看的字符串。🏴 + tag 串 + 取消 tag（U+E007F）是地区旗 emoji（England / Scotland / Wales），原样
(define (model-view-string s [mode (unbox hidden-text-mode)])
  (if (or (eq? mode 'raw) (not (regexp-match? TAG-RUN s)))
      s
      (let loop ([start 0] [acc '()])
        (define m (regexp-match-positions TAG-RUN s start))
        (cond
          [(not m) (apply string-append (reverse (cons (substring s start) acc)))]
          [else
           (define a (caar m)) (define b (cdar m))
           (define run (substring s a b))
           (define flag? (and (> a 0) (= (char->integer (string-ref s (sub1 a))) #x1F3F4) (= (char->integer (string-ref s (sub1 b))) #xE007F)))
           (define d (decode-tags run))
           (define shown (cond [flag? run]
                               [(string=? d "") "⟦hidden Unicode tag characters⟧"]
                               [else (string-append "⟦hidden text (Unicode tag characters): " d "⟧")]))
           (loop b (list* shown (substring s start a) acc))]))))

(define (view-block b mode)
  (cond
    [(text-block? b) (let ([t (text-block-text b)]) (let ([v (model-view-string t mode)]) (if (eq? v t) b (text-block v))))]
    [(tool-result-block? b)
     (define c (tool-result-block-content b))
     (define c* (cond [(string? c) (model-view-string c mode)]
                      [(list? c) (map (lambda (x) (if (content-block? x) (view-block x mode) x)) c)]
                      [else c]))
     (if (equal? c* c) b (tool-result-block (tool-result-block-tool-use-id b) c* (tool-result-block-is-error? b)))]
    [else b]))

;; 消息表 → 给模型看的消息表（原样模式下不复制）
(define (model-view-messages msgs [mode (unbox hidden-text-mode)])
  (if (eq? mode 'raw)
      msgs
      (for/list ([m (in-list msgs)])
        (if (message? m)
            (let ([bs (map (lambda (b) (view-block b mode)) (message-blocks m))])
              (if (andmap eq? bs (message-blocks m)) m (message (message-role m) bs)))
            m))))

(provide hidden-text-mode set-hidden-text-mode! model-view-string model-view-messages)
