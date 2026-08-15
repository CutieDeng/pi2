#lang racket/base
;; tests/browser/l1-charset-test.rkt — L1 charset：BOM/hint/meta 优先序与容错

(require rackunit (file "../../src/web/browser/text/charset.rkt"))

;; 用系统转换器构造非 UTF-8 测试字节（转换器缺失则跳过对应用例）
(define (encode-as name s)
  (define c (bytes-open-converter "UTF-8" name))
  (and c
       (let-values ([(out n status) (bytes-convert c (string->bytes/utf-8 s))])
         (bytes-close-converter c)
         (and (eq? status 'complete) out))
  ) ; end and
) ; end define encode-as

;; -------- 基线：utf-8 与兜底
(let-values ([(s cs) (decode-html #"hello" #f)])
  (check-equal? s "hello")
  (check-equal? cs 'utf-8))

(let-values ([(s cs) (decode-html (string->bytes/utf-8 "中文段落") #f)])
  (check-equal? s "中文段落"))

;; 坏 utf-8 字节 → 替换符，永不 raise
(let-values ([(s cs) (decode-html (bytes 104 105 #xFF #xFE 33) 'utf-8)])
  (check-true (and (regexp-match? #rx"hi" s) (regexp-match? #rx"�" s))))

;; -------- BOM：最高优先，且剥离
(let-values ([(s cs) (decode-html (bytes-append #"\357\273\277" #"abc") 'gbk)])
  (check-equal? cs 'utf-8)   ; BOM 压过 hint
  (check-equal? s "abc"))    ; BOM 不进正文

(let ([u16 (encode-as "UTF-16LE" "hi中")])
  (when u16
    (let-values ([(s cs) (decode-html (bytes-append #"\377\376" u16) #f)])
      (check-equal? cs 'utf-16le)
      (check-equal? s "hi中"))
  ) ; end when
) ; end let

;; -------- hint（传输层）
(let ([gbk (encode-as "GBK" "中文测试")])
  (when gbk
    (let-values ([(s cs) (decode-html gbk 'gbk)])
      (check-equal? cs 'gbk)
      (check-equal? s "中文测试"))
    ;; label 归一：gb2312 → gbk
    (let-values ([(s cs) (decode-html gbk 'gb2312)])
      (check-equal? cs 'gbk)
      (check-equal? s "中文测试"))
  ) ; end when
) ; end let

;; -------- meta 预扫
(let ([gbk-page (encode-as "GBK" "<html><meta charset=\"gbk\"><p>中文正文</p>")])
  (when gbk-page
    ;; 无 hint → meta 生效
    (let-values ([(s cs) (decode-html gbk-page #f)])
      (check-equal? cs 'gbk)
      (check-true (regexp-match? #rx"中文正文" s)))
    ;; hint 压过 meta（优先序）
    (let-values ([(s cs) (decode-html gbk-page 'utf-8)])
      (check-equal? cs 'utf-8))
  ) ; end when
) ; end let

;; http-equiv 形态同样命中
(let-values ([(s cs) (decode-html
                      #"<meta http-equiv=\"Content-Type\" content=\"text/html; charset=GBK\">x"
                      #f)])
  (check-equal? cs 'gbk))

;; meta 声明 utf-16 → 按 utf-8（WHATWG）
(let-values ([(s cs) (decode-html #"<meta charset=utf-16le>abc" #f)])
  (check-equal? cs 'utf-8))

;; -------- 标签归一单元
(check-equal? (normalize-charset-label "GBK") 'gbk)
(check-equal? (normalize-charset-label "Latin1") 'windows-1252)
(check-equal? (normalize-charset-label 'utf8) 'utf-8)
(check-equal? (normalize-charset-label "no-such-charset") #f)
(check-equal? (normalize-charset-label #f) #f)
