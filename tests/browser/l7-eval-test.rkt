#lang racket/base
;; tests/browser/l7-eval-test.rkt — L7 端到端：真实 JS 源码编译到 Racket 后运行
;; 验证设计 A：JS → Racket 语法 → eval，跑在 L6 对象运行时上（无独立解释器）

(require rackunit (file "../../src/web/browser/js/eval.rkt"))

(define (js s) (js->display (run-js s)))

(test-case "字面量/算术/字符串"
  (check-equal? (js "1 + 2 * 3") "7")
  (check-equal? (js "(1 + 2) * 3") "9")
  (check-equal? (js "\"foo\" + \"bar\"") "foobar")
  (check-equal? (js "\"n=\" + (3 + 4)") "n=7")          ; 数字转字符串拼接
  (check-equal? (js "10 % 3") "1")
  (check-equal? (js "true && false") "false")
  (check-equal? (js "0 || \"x\"") "x"))                  ; 逻辑运算返回操作数

(test-case "对象字面量 + 属性访问 + 赋值"
  (check-equal? (js "var p = {x: 41}; p.x + 1") "42")
  (check-equal? (js "var o = {}; o.a = 5; o.b = o.a * 2; o.b") "10")
  (check-equal? (js "var o = {a: {b: {c: 7}}}; o.a.b.c") "7")
  (check-equal? (js "var o = {}; o[\"key\"] = 9; o.key") "9"))

(test-case "函数 + 递归"
  (check-equal? (js "function add(a,b){ return a+b } add(3,4)") "7")
  (check-equal? (js "function fact(n){ if (n<=1) return 1; return n*fact(n-1) } fact(5)") "120")
  (check-equal? (js "function fib(n){ if (n<2) return n; return fib(n-1)+fib(n-2) } fib(10)") "55"))

(test-case "闭包：JS 闭包 = Racket 闭包（设计 A 的白捡）"
  (check-equal? (js "function mk(){ var c=0; return function(){ c=c+1; return c } } var f=mk(); f(); f(); f()") "3")
  ;; 两个独立计数器互不干扰
  (check-equal? (js "function mk(){ var c=0; return function(){ c=c+1; return c } } var a=mk(); var b=mk(); a(); a(); b()") "1"))

(test-case "构造器 + new + this + 原型方法"
  (check-equal? (js "function Point(x,y){ this.x=x; this.y=y } var p=new Point(3,4); p.x*10+p.y") "34")
  (check-equal? (js (string-append
                     "function Point(x,y){ this.x=x; this.y=y }"
                     "Point.prototype.norm = function(){ return this.x*this.x + this.y*this.y }"
                     "var p = new Point(3,4); p.norm()")) "25"))

(test-case "数组 + 方法 + while + 索引"
  (check-equal? (js "var a=[10,20,30]; a.push(40); a.join(\"-\")") "10-20-30-40")
  (check-equal? (js "var a=[1,2,3]; a.indexOf(2)") "1")
  (check-equal? (js "var a=[1,2,3]; a.length") "3")
  (check-equal? (js (string-append
                     "var o={vals:[1,2,3,4], sum:function(){ var t=0; var i=0;"
                     "while(i<this.vals.length){ t=t+this.vals[i]; i=i+1 } return t }}; o.sum()")) "10"))

(test-case "typeof / instanceof / 三元 / 内建"
  (check-equal? (js "typeof 1") "number")
  (check-equal? (js "typeof \"s\"") "string")
  (check-equal? (js "typeof {}") "object")
  (check-equal? (js "typeof function(){}") "function")
  (check-equal? (js "[1,2] instanceof Array") "true")
  (check-equal? (js "({}) instanceof Array") "false")
  (check-equal? (js "var x=5; x > 3 ? \"big\" : \"small\"") "big")
  (check-equal? (js "var o={a:1,b:2,c:3}; Object.keys(o).join(\",\")") "a,b,c"))

(test-case "原始值包裹 + String/Array 内建（照规范对齐 V8）"
  ;; §7.1.18 字符串原始值的 length/索引/方法
  (check-equal? (js "\"abc\".length") "3")
  (check-equal? (js "\"abc\"[1]") "b")
  (check-equal? (js "\"Hello\".toUpperCase()") "HELLO")
  (check-equal? (js "\"Hello\".toLowerCase()") "hello")
  (check-equal? (js "\"hello world\".slice(0,5)") "hello")
  (check-equal? (js "\"a,b,c\".split(\",\").length") "3")
  (check-equal? (js "\"abcdef\".indexOf(\"cd\")") "2")
  ;; §13.5.3 typeof 未声明 → "undefined" 不报错
  (check-equal? (js "typeof undefinedVar") "undefined")
  ;; §23.1.3.36 数组 ToString → join；§23.1.3.30 sort（缺省 ToString / 带比较函数）
  (check-equal? (js "[1,2,3]") "1,2,3")
  (check-equal? (js "[3,1,2].sort().join(\",\")") "1,2,3")
  (check-equal? (js "[10,2,33].sort(function(a,b){ return a-b }).join(\",\")") "2,10,33"))

(test-case "for 循环 / ++ / 迭代方法 / Math（对齐 V8）"
  (check-equal? (js "var s=0; for(var i=0;i<10;i++){ s=s+i } s") "45")
  (check-equal? (js "var a=[]; for(var i=0;i<5;i++){ a.push(i*i) } a.join(\",\")") "0,1,4,9,16")
  (check-equal? (js "var i=5; var j=i++; j+\",\"+i") "5,6")     ; 后缀返回旧值
  (check-equal? (js "var i=5; var j=++i; j+\",\"+i") "6,6")     ; 前缀返回新值
  (check-equal? (js "var t=0; for(var i=1;i<=100;i++){ t+=i } t") "5050")
  (check-equal? (js "[1,2,3,4].map(function(x){ return x*x }).join(\",\")") "1,4,9,16")
  (check-equal? (js "[1,2,3,4,5].filter(function(x){ return x%2===1 }).join(\",\")") "1,3,5")
  (check-equal? (js "[1,2,3,4].reduce(function(a,b){ return a+b }, 0)") "10")
  ;; 链式：filter→map→reduce（真实框架风格）
  (check-equal? (js (string-append "[1,2,3,4,5,6].filter(function(x){return x>3})"
                                   ".map(function(x){return x*10}).reduce(function(a,b){return a+b},0)")) "150")
  (check-equal? (js "Math.max(3,7,2) + \",\" + Math.floor(3.7) + \",\" + Math.pow(2,10)") "7,3,1024"))

(test-case "realm 隔离：每次 run 独立对象世界"
  ;; 两次 run 互不影响（各自 fresh realm）
  (check-equal? (js "Object.prototype.leak = 1; \"once\"") "once")
  (check-equal? (js "var o={}; typeof o.leak") "undefined"))   ; 上次污染不残留
