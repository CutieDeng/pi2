#lang racket/base
;; browser/text/charset.rkt — L1 · 字节 → 码点串（design-chrome.md L1.1 / §2.3）
;; 职责: charset 判定与解码（BOM → hint → meta 预扫 → UTF-8 兜底），永不 raise
;; 不做: UTF-16 语义(u16string)、rope、HTML 解析
;; 依赖: racket/base

(require racket/contract)

(provide
 (contract-out
  [decode-html (-> bytes? (or/c symbol? #f) (values string? symbol?))]
  [normalize-charset-label (-> (or/c string? symbol? #f) (or/c symbol? #f))]
 ) ; end contract-out
) ; end provide

;; ---------------------------------------------------------------- 标签归一
;; WHATWG encoding labels 的实用子集（label → 内核规范名）。

(define label-table
  (hash "utf-8" 'utf-8  "utf8" 'utf-8  "unicode-1-1-utf-8" 'utf-8
        "gbk" 'gbk  "gb2312" 'gbk  "gb_2312" 'gbk  "gb_2312-80" 'gbk
        "x-gbk" 'gbk  "chinese" 'gbk  "csgb2312" 'gbk  "csiso58gb231280" 'gbk
        "gb18030" 'gb18030
        "big5" 'big5  "big5-hkscs" 'big5  "cn-big5" 'big5  "x-x-big5" 'big5
        "shift_jis" 'shift_jis  "shift-jis" 'shift_jis  "sjis" 'shift_jis
        "ms_kanji" 'shift_jis  "windows-31j" 'shift_jis
        "euc-jp" 'euc-jp  "x-euc-jp" 'euc-jp
        "euc-kr" 'euc-kr  "korean" 'euc-kr  "windows-949" 'euc-kr
        ;; WHATWG: latin1/ascii 族全部映射到 windows-1252
        "iso-8859-1" 'windows-1252  "iso8859-1" 'windows-1252  "latin1" 'windows-1252
        "l1" 'windows-1252  "ascii" 'windows-1252  "us-ascii" 'windows-1252
        "windows-1252" 'windows-1252  "cp1252" 'windows-1252  "x-cp1252" 'windows-1252
        "utf-16" 'utf-16le  "utf-16le" 'utf-16le  "utf-16be" 'utf-16be
  ) ; end hash
) ; end define label-table

(define (normalize-charset-label label)
  (cond
    [(not label) #f]
    [else
     (define s (string-downcase (if (symbol? label) (symbol->string label) label)))
     (hash-ref label-table s #f)]
  ) ; end cond
) ; end define normalize-charset-label

;; ---------------------------------------------------------------- BOM 嗅探

(define (bom-charset bs)
  (define n (bytes-length bs))
  (cond
    [(and (>= n 3) (= (bytes-ref bs 0) #xEF) (= (bytes-ref bs 1) #xBB) (= (bytes-ref bs 2) #xBF))
     (values 'utf-8 3)]
    [(and (>= n 2) (= (bytes-ref bs 0) #xFF) (= (bytes-ref bs 1) #xFE))
     (values 'utf-16le 2)]
    [(and (>= n 2) (= (bytes-ref bs 0) #xFE) (= (bytes-ref bs 1) #xFF))
     (values 'utf-16be 2)]
    [else (values #f 0)]
  ) ; end cond
) ; end define bom-charset

;; ---------------------------------------------------------------- meta 预扫
;; WHATWG 同款：前 1024 字节内找 charset=<label>。
;; 已知简化(B1)：不限定在 <meta> 标签内——前 1024 字节里脚本撞出 charset= 字样
;; 的概率极低，语料证明误伤再收紧。

(define META-RX #rx#"(?i:charset)[ \t]*=[ \t]*[\"']?([-a-zA-Z0-9._]+)")

(define (meta-prescan bs)
  (define head (subbytes bs 0 (min 1024 (bytes-length bs))))
  (define m (regexp-match META-RX head))
  (define found (and m (normalize-charset-label (bytes->string/latin-1 (cadr m)))))
  ;; WHATWG：meta 声明 utf-16 按 utf-8 处理（meta 自身能被读到就不可能是 utf-16）
  (if (memq found '(utf-16le utf-16be)) 'utf-8 found)
) ; end define meta-prescan

;; ---------------------------------------------------------------- 解码器

(define iconv-name
  (hash 'gbk "GBK"  'gb18030 "GB18030"  'big5 "BIG5"
        'shift_jis "SHIFT_JIS"  'euc-jp "EUC-JP"  'euc-kr "EUC-KR"
        'windows-1252 "WINDOWS-1252"  'utf-16le "UTF-16LE"  'utf-16be "UTF-16BE"
  ) ; end hash
) ; end define iconv-name

(define REPLACEMENT-UTF8 #"\357\277\275")   ; U+FFFD 的 UTF-8 编码

;; 经 bytes-converter 转到 UTF-8；坏字节 → U+FFFD 跳过一字节继续。
;; 返回 string 或 #f（转换器不可得——上层兜底 utf-8）。
(define (decode-via-converter cs bs)
  (define name (hash-ref iconv-name cs #f))
  (define conv (and name (bytes-open-converter name "UTF-8")))
  (cond
    [(not conv) #f]
    [else
     (define parts
       (let loop ([i 0] [acc '()])
         (cond
           [(>= i (bytes-length bs)) (reverse acc)]
           [else
            (define-values (out consumed status) (bytes-convert conv bs i))
            (define i* (+ i consumed))
            (case status
              [(complete) (reverse (cons out acc))]
              [(continues) (loop i* (cons out acc))]
              [(error) (loop (+ i* 1) (cons REPLACEMENT-UTF8 (cons out acc)))]
              [else ; aborts: 尾部截断的多字节序列
               (reverse (cons REPLACEMENT-UTF8 (cons out acc)))]
            ) ; end case
           ] ; end else
         ) ; end cond
       ) ; end loop
     ) ; end define parts
     (bytes-close-converter conv)
     (bytes->string/utf-8 (apply bytes-append parts) #\uFFFD)]
  ) ; end cond
) ; end define decode-via-converter

(define (decode-as cs bs)
  (case cs
    [(utf-8) (bytes->string/utf-8 bs #\uFFFD)]
    [else (or (decode-via-converter cs bs)
              (bytes->string/utf-8 bs #\uFFFD))]   ; 转换器缺失 → utf-8 替换符兜底
  ) ; end case
) ; end define decode-as

;; ---------------------------------------------------------------- 入口
;; 优先序（WHATWG）：BOM → 传输层 hint → meta 预扫 → utf-8 兜底。
;; 第二返回值 = 实际生效 charset（进 page-result 可见）。

(define (decode-html bs hint)
  (define-values (bom-cs skip) (bom-charset bs))
  (define body (if (> skip 0) (subbytes bs skip) bs))
  (define cs
    (or bom-cs
        (normalize-charset-label hint)
        (meta-prescan body)
        'utf-8))
  (values (decode-as cs body) cs)
) ; end define decode-html
