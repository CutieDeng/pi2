#lang racket
;; browser/dev/v8-diff.rkt — L7 前端的 V8 差分参照工具（dev，非内核）
;; 同一段 JS 喂给「我们的引擎」与「node(V8)」，比对 ToString 结果。
;; 分歧 = 我们子集与真实 V8 的差距（parser differential / 缺失特性）。
;; 用: racket src/web/dev/v8-diff.rkt

(require (file "../browser/js/eval.rkt"))

;; 我们的引擎跑一段：→ (cons 'ok str) 或 (cons 'err msg)
(define (ours src)
  (with-handlers ([exn:fail? (lambda (e) (cons 'err (exn-message e)))])
    (cons 'ok (js->display (run-js src)))))

;; node(V8) 跑一段：eval 取完成值 → String()，→ (cons 'ok str) 或 (cons 'err msg)
(define (v8 src)
  (define tmp (make-temporary-file "v8diff-~a.js"))
  (display-to-file src tmp #:exists 'truncate)
  ;; 包装器变量用 __diff_ 前缀，避免与被测程序的标识符（如 var s）撞名
  (define wrapper
    (string-append
     "const __diff_fs=require('fs');const __diff_src=__diff_fs.readFileSync(process.argv[1],'utf8');"
     "try{let __diff_r=(0,eval)(__diff_src);console.log('OK\\t'+String(__diff_r))}catch(__diff_e){console.log('ERR\\t'+__diff_e.message)}"))
  (define out (with-output-to-string
                (lambda () (system* (find-executable-path "node") "-e" wrapper (path->string tmp)))))
  (delete-file tmp)
  (define line (string-trim out))
  (cond [(string-prefix? line "OK\t") (cons 'ok (substring line 3))]
        [(string-prefix? line "ERR\t") (cons 'err (substring line 4))]
        [else (cons 'err (string-append "node: " line))]))

(define PROGRAMS
  (list
   ;; 已知我们支持的
   "1 + 2 * 3"
   "\"foo\" + \"bar\""
   "\"n=\" + (3+4)"
   "var p={x:41}; p.x+1"
   "function fact(n){ if(n<=1) return 1; return n*fact(n-1) } fact(5)"
   "function mk(){ var c=0; return function(){ c=c+1; return c } } var f=mk(); f(); f(); f()"
   "function Point(x,y){ this.x=x; this.y=y } var p=new Point(3,4); p.x*10+p.y"
   "typeof {}"
   "typeof function(){}"
   "[1,2] instanceof Array"
   "var o={a:1,b:2,c:3}; Object.keys(o).join(\",\")"
   ;; 边角：预期会暴露分歧（Array ToString、数字格式、字符串方法、逻辑值…）
   "[1,2,3]"                          ; 数组 ToString（我们可能 [object Object]）
   "1/3"                              ; 数字格式
   "0.1 + 0.2"                        ; 浮点
   "1000000000000000000000"          ; 大数科学计数
   "\"abc\".length"                   ; 字符串属性（我们没做原始值包裹）
   "\"abc\".toUpperCase()"            ; 字符串方法
   "[3,1,2].sort()"                   ; 未实现方法
   "null + 1"                         ; null 算术
   "true + 1"                         ; 布尔算术
   "typeof undefinedVar"             ; typeof 未声明（V8: "undefined" 不报错）
   ;; 第二批：字符串方法 / sort 比较函数 / 转换 / 短路 / 嵌套
   "\"Hello\".toLowerCase()"
   "\"hello world\".split(\" \").length"
   "\"hello world\".slice(0,5)"
   "\"abcdef\".indexOf(\"cd\")"
   "\"abc\".charAt(1)"
   "[3,1,2].sort().join(\",\")"
   "[10,2,33].sort(function(a,b){ return a-b }).join(\",\")"
   "[10,2,33].sort().join(\",\")"                       ; 缺省 ToString 比较：10,2,33 → 10,2,33? 看 V8
   "var a=[1,2,3,4,5]; a.length"
   "\"\" + null + undefined"
   "1 + null"
   "\"5\" * 2"
   "\"5\" - 2"
   "!0"
   "!\"\""
   "!!\"x\""
   "1 < 2 && 2 < 3"
   "\"a\" < \"b\""
   "typeof null"
   "typeof [1,2]"
   "({a:1})[\"a\"]"
   "var o={}; o.x = o.y = 7; o.x + o.y"
   "function f(){ return arguments } typeof f"           ; arguments 未实现——预期分歧
   "[1,2,3].indexOf(9)"
   "var s=0; var i=1; while(i<=10){ s=s+i; i=i+1 } s"     ; 1..10 求和
   "(function(n){ return n*n })(7)"                       ; IIFE
   ;; 第三批：for / ++ / 迭代方法 / Math
   "var s=0; for(var i=0;i<10;i++){ s=s+i } s"
   "var a=[]; for(var i=0;i<5;i++){ a.push(i*i) } a.join(\",\")"
   "var i=5; var j=i++; j+\",\"+i"
   "var i=5; var j=++i; j+\",\"+i"
   "[1,2,3,4].map(function(x){ return x*x }).join(\",\")"
   "[1,2,3,4,5].filter(function(x){ return x%2===1 }).join(\",\")"
   "[1,2,3,4].reduce(function(a,b){ return a+b }, 0)"
   "[1,2,3].forEach(function(x){}); \"done\""
   "[5,3,8,1].slice(1,3).join(\",\")"
   "Math.max(3,7,2)"
   "Math.min(3,7,2)"
   "Math.floor(3.7) + Math.ceil(3.2)"
   "Math.pow(2,10)"
   "Math.abs(-5)"
   "Math.round(2.5)"
   "Math.round(2.4)"
   "var t=0; for(var i=1;i<=100;i++){ t+=i } t"           ; += 复合赋值 + for
   "[1,2,3,4,5,6].filter(function(x){return x>3}).map(function(x){return x*10}).reduce(function(a,b){return a+b},0)"
   "var o={n:3, sq:function(){ return this.n*this.n }}; o.sq()"
   ))

(module+ main
  (printf "~a  |  ~a\n" (~a "ours" #:min-width 22) "V8(node)")
  (printf "~a\n" (make-string 60 #\-))
  (define-values (match div)
    (for/fold ([m 0] [d 0]) ([src (in-list PROGRAMS)])
      (define o (ours src)) (define n (v8 src))
      (define same? (equal? o n))
      (printf "~a ~a\n  ours: ~a ~s\n  V8:   ~a ~s\n"
              (if same? "✓" "✗")
              (if (> (string-length src) 46) (string-append (substring src 0 46) "…") src)
              (car o) (cdr o) (car n) (cdr n))
      (values (if same? (add1 m) m) (if same? d (add1 d)))))
  (printf "~a\n匹配 ~a / 分歧 ~a / 共 ~a\n" (make-string 60 #\-) match div (length PROGRAMS)))
