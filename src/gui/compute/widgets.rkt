#lang racket/base
;; widgets.rkt — 常用的界面构件（GUI 元数据平面 4）：由视图平面的图元（view.rkt）组成的元素树构造函数，纯数据。
;;   信息面板、测试面板、审阅面板共用同一种「模态卡片」；按钮、复选框、键值行等在多处重复——构件在这里定义一次，
;;   颜色、尺寸与迁移前各组件里写死的值相同（显示检查的数据因此不变），以后统一在这里改。

(require racket/list "view.rkt" "palette.rkt" "tokens.rkt" "drawlist.rkt")

(define CARD-FILL '(0.12 0.12 0.15 1.0))
(define CARD-BORDER '(0.30 0.31 0.36 1.0))
(define CARD-PAD 18.0)

;; 模态卡片：整块区域是「外面」（点了关闭），卡片居中、宽 ≤ max-w（两边各留 20），上下留 top / bottom；
;; 头：标题 + ✕；身：一栏可滚动（id body-id，偏移 offset）；脚（可选）：固定在底部
(define (modal-card #:title title #:body body #:offset offset #:body-id [body-id 'body] #:max-w [max-w 640.0]
                    #:top [top 30.0] #:bottom [bottom 30.0] #:footer [footer #f] #:head-extra [head-extra '()] #:body-gap [body-gap 0.0])
  (ui-box #:dir 'stack #:hit (cons 'outside #t) #:align 'center #:pad (vector top 20.0 bottom 20.0) #:id 'area
          (ui-box #:dir 'column #:max-w max-w #:fill CARD-FILL #:stroke CARD-BORDER #:radius 10.0 #:hit (cons 'panel #t) #:id 'card
                  #:pad (vector 14.0 CARD-PAD 12.0 CARD-PAD)
                  (ui-box #:dir 'row #:align 'start
                          (ui-text title #:font 'title #:color TEXT #:grow 1)
                          (ui-box #:w 30.0 #:h 30.0 #:dy -8.0 #:hit (cons 'close #t) #:dir 'stack #:align 'center #:valign 'center
                                  (ui-text "✕" #:font 'body #:color DIM #:fit? #f #:w 14.0)))
                  (apply ui-box #:dir 'column #:pad (vector 4.0 0.0 0.0 0.0) head-extra)
                  (ui-box #:dir 'column #:grow 1 #:pad (vector 8.0 0.0 0.0 0.0)
                          (apply ui-scroll body-id #:offset offset #:gap body-gap body))
                  footer)))

;; 按钮：小号字、一行（放不下截断），底色（缺省是侧栏按钮的灰），圆角 6；hit = (cons 种类 载荷)
(define (button label hit #:fill [fill (append (take BUTTON 3) '(1.0))] #:font [font 'small] #:color [color TEXT] #:h [h #f])
  (ui-box #:dir 'row #:align 'center #:pad (vector 4.0 9.0 4.0 9.0) #:fill fill #:radius (tk 'radius-m) #:hit hit #:h h
          (ui-text label #:font font #:color color)))

;; 复选框：15 × 15，边框灰，开着是蓝
(define (checkbox on? enabled?)
  (ui-box #:w 15.0 #:h 15.0 #:pad 1.5 #:fill (list 0.45 0.47 0.55 (if enabled? 1.0 0.5)) #:radius 4.0
          (ui-box #:grow 1 #:fill (if on? '(0.30 0.45 0.80 1.0) '(0.12 0.13 0.16 1.0)) #:radius 3.0)))

;; 键值行：键（小号、灰）在固定宽的左列，值（正文）可折行
(define (kv-row key value #:key-w [key-w 150.0])
  (ui-box #:dir 'row #:align 'start
          (ui-box #:w key-w #:pad (vector 2.0 10.0 0.0 0.0) (ui-text key #:font 'small #:color DIM #:fit? #f))
          (ui-box #:grow 1 (ui-text value #:font 'body #:color TEXT #:fit? #f))))

;; 一段文字（可折行）
(define (para str #:font [font 'body] #:color [color TEXT]) (ui-text str #:font font #:color color #:fit? #f))

;; 描边框（画在内容之上，不填充）：四条边，宽 lw——键盘焦点、拖放目标
(define (outline color #:w [lw 1.5])
  (ui-custom #:measure (lambda (_w) 0.0)
             #:paint (lambda (dl x y w h scale)
                       (define-values (r g b a) (apply values color))
                       (drawlist-rect! dl x y w lw r g b a) (drawlist-rect! dl x (+ y h (- lw)) w lw r g b a)
                       (drawlist-rect! dl x y lw h r g b a) (drawlist-rect! dl (+ x w (- lw)) y lw h r g b a)
                       '())))

;; 虚线框：四边各一串短横（1 point 粗、6 长、4 空），不透明度 a——临时会话
(define (dashed-outline color a)
  (ui-custom #:measure (lambda (_w) 0.0)
             #:paint (lambda (dl x y w h scale)
                       (define-values (r g b) (values (car color) (cadr color) (caddr color)))
                       (define (hline! yy) (for ([xx (in-range (+ x 6.0) (- (+ x w) 6.0) 10.0)]) (drawlist-rect! dl xx yy (min 6.0 (- (+ x w -6.0) xx)) 1.0 r g b a)))
                       (define (vline! xx) (for ([yy (in-range (+ y 6.0) (- (+ y h) 6.0) 10.0)]) (drawlist-rect! dl xx yy 1.0 (min 6.0 (- (+ y h -6.0) yy)) r g b a)))
                       (hline! y) (hline! (+ y h -1.0)) (vline! x) (vline! (+ x w -1.0))
                       '())))

(provide CARD-FILL CARD-BORDER CARD-PAD modal-card button checkbox kv-row para outline dashed-outline)
