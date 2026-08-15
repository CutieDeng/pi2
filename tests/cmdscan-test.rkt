#lang tstring racket
;; cmdscan-test.rkt — shell 命令静态判级 + 'read-only 权限模式（design-execguard.md）。

(require
 rackunit
 racket/file
 racket/string
 (file "../src/model.rkt")
 (file "../src/tool.rkt")
 (file "../src/cmdscan.rkt")
 (file "../src/permission.rkt")
 (file "../src/tools/bash.rkt")
 (file "../src/tools/file.rkt")
 (file "../src/tools/git.rkt")
 (file "../src/tools/editor.rkt")
) ; end require

(define tmp (path->string (make-temporary-file "pi2-cmdscan-~a" 'directory)))
(define (lvl s) (cmd-verdict-level (classify-command s #:workdir tmp)))
(define (reason s) (or (cmd-verdict-reason (classify-command s #:workdir tmp)) ""))

;; ------------------------------------------------------------ 基础分类

(test-case "只读白名单与管道/逻辑连接"
  (check-equal? (lvl "ls -la") 'read-only)
  (check-equal? (lvl "grep -rn foo src | head -5 | wc -l") 'read-only)
  (check-equal? (lvl "cat a && diff a b || echo differ") 'read-only)
  (check-equal? (lvl "cd sub && export A=1 && pwd") 'read-only)
  (check-equal? (lvl "FOO=1 env BAR=2 sort x") 'read-only)
  (check-equal? (lvl "/bin/ls -1") 'read-only)          ; 非脚本的绝对路径命令：ls 归类
) ; end test-case

(test-case "默认拒绝：白名单外命令判 mutating"
  (check-equal? (lvl "rm -rf x") 'mutating)
  (check-equal? (lvl "curl http://example.com") 'mutating)
  (check-equal? (lvl "mkdir d") 'mutating)
  (check-equal? (lvl "ls; touch x") 'mutating)          ; 任一片段写 → 整体非只读
) ; end test-case

(test-case "重定向：落文件 mutating；/dev/null 与 2>&1 放行；粘连形式也检出"
  (check-equal? (lvl "cat f > out.txt") 'mutating)
  (check-equal? (lvl "echo hi>out.txt") 'mutating)
  (check-equal? (lvl "sort x >> log") 'mutating)
  (check-equal? (lvl "ls > /dev/null 2>&1") 'read-only)
  (check-equal? (lvl "wc -l < data.txt") 'read-only)
  (check-equal? (lvl "grep x f 2>/dev/null") 'read-only)
  (check-equal? (lvl "cat f > $OUT") 'opaque)           ; 目标含展开 → 不可判
) ; end test-case

(test-case "不可判定构造 → opaque"
  (check-equal? (lvl "eval ls") 'opaque)
  (check-equal? (lvl "xargs rm") 'opaque)
  (check-equal? (lvl "$CMD --help") 'opaque)
  (check-equal? (lvl "python3 -c 'print(1)'") 'opaque)
  (check-equal? (lvl "awk '{print > \"o\"}' f") 'opaque)
  (check-equal? (lvl "bash") 'opaque)                   ; 裸交互 shell
  (check-equal? (lvl "echo 'unclosed") 'opaque)         ; 解析失败 → opaque
) ; end test-case

;; ---------------------------------------------------- v2：AST 化后的精确判级

(test-case "命令替换/进程替换/heredoc：递归判级而非一律 opaque"
  (check-equal? (lvl "echo $(pwd)") 'read-only)
  (check-equal? (lvl "echo $(rm x)") 'mutating)
  (check-equal? (lvl "echo `date`") 'read-only)
  (check-equal? (lvl "echo `rm x`") 'mutating)
  (check-equal? (lvl "x=$(git status) && echo $x") 'read-only)
  (check-equal? (lvl "x=$(git push)") 'mutating)
  (check-equal? (lvl "diff <(sort a) <(sort b)") 'read-only)
  (check-equal? (lvl "diff <(rm x) b") 'mutating)
  (check-equal? (lvl "cat <<EOF\nhello world\nEOF") 'read-only)
  (check-equal? (lvl "cat <<EOF\n$(rm x)\nEOF") 'mutating)      ; 未引号定界会展开
  (check-equal? (lvl "cat <<'EOF'\n$(rm x)\nEOF") 'read-only)   ; 引号定界纯字面量
) ; end test-case

(test-case "复合命令结构化遍历"
  (check-equal? (lvl "(ls | wc -l)") 'read-only)
  (check-equal? (lvl "(rm x)") 'mutating)
  (check-equal? (lvl "{ ls; cat f; } > /dev/null") 'read-only)
  (check-equal? (lvl "{ ls; } > log.txt") 'mutating)
  (check-equal? (lvl "if grep -q x f; then echo y; else echo n; fi") 'read-only)
  (check-equal? (lvl "if true; then rm x; fi") 'mutating)
  (check-equal? (lvl "while read l; do echo $l; done < f") 'read-only)
  (check-equal? (lvl "for f in *.txt; do wc -l $f; done") 'read-only)
  (check-equal? (lvl "for f in $(ls); do rm $f; done") 'mutating)
  (check-equal? (lvl "case $x in a|b) ls;; *) cat f;; esac") 'read-only)
  (check-equal? (lvl "case $x in a) rm f;; esac") 'mutating)
  (check-equal? (lvl "[[ -f x && $y < z ]] && cat x") 'read-only)
  (check-equal? (lvl "(( i = 1 + 2 ))") 'read-only)
) ; end test-case

(test-case "函数：定义不执行不计；调用按体判级；同名遮蔽白名单"
  (check-equal? (lvl "f() { rm x; }") 'read-only)               ; 只定义未调用
  (check-equal? (lvl "f() { ls; }; f") 'read-only)
  (check-equal? (lvl "f() { rm x; }; f") 'mutating)
  (check-equal? (lvl "ls() { rm -rf /; }; ls") 'mutating)       ; 遮蔽 allowlist
  (check-equal? (lvl "f() { f; }; f") 'mutating)                ; 自递归收敛（fail-closed）
) ; end test-case

(test-case "包装命令剥壳与 find -exec 载荷判级"
  (check-equal? (lvl "timeout 5 grep x f") 'read-only)
  (check-equal? (lvl "time rm x") 'mutating)
  (check-equal? (lvl "nohup ls") 'read-only)
  (check-equal? (lvl "command ls") 'read-only)
  (check-equal? (lvl "command -v rg") 'read-only)
  (check-equal? (lvl "! grep -q x f") 'read-only)
  ;; find：只读谓词白名单 + 写谓词精确判 + -exec 载荷忠实建模（对齐 GNU findutils）
  (check-equal? (lvl "find . -name '*.rkt' -type f") 'read-only)
  (check-equal? (lvl "find src -maxdepth 2 -type d -print") 'read-only)
  (check-equal? (lvl "find . -newer ref -size +1k") 'read-only)
  (check-equal? (lvl "find . -bogus-future-predicate") 'opaque)     ; 未知谓词自动拒
) ; end test-case

(test-case "find -exec 忠实建模：载荷命令递归判级（GNU find/parser.c 语义）"
  ;; 只读载荷 → 只读；写载荷 → mutating；{} 参数位是数据
  ;; 注：真实 shell 里 -exec 的分号须写 \; 或 ';'（裸 ; 是命令分隔符）——测试如实反映
  (check-equal? (lvl "find . -name x -exec grep y {} +") 'read-only)
  (check-equal? (lvl "find . -name x -exec grep y {} \\;") 'read-only)
  (check-equal? (lvl "find . -type f -exec wc -l {} +") 'read-only)
  (check-equal? (lvl "find . -name x -exec rm {} +") 'mutating)
  (check-equal? (lvl "find . -exec cp {} /backup \\;") 'mutating)   ; cp 非只读
  ;; 载荷是 sh -c → 深入 -c 体递归
  (check-equal? (lvl "find . -exec sh -c 'grep x \"$1\"' _ {} +") 'read-only)
  (check-equal? (lvl "find . -exec sh -c 'rm \"$1\"' _ {} +") 'mutating)
  ;; 多个 -exec 子句各自判级，取最坏
  (check-equal? (lvl "find . -exec wc -l {} + -exec cat {} +") 'read-only)
  (check-equal? (lvl "find . -exec cat {} + -exec rm {} +") 'mutating)
  ;; {} 在命令名位 = 执行匹配到的文件本身 → opaque
  (check-equal? (lvl "find . -perm -111 -exec {} \\;") 'opaque)
  (check-equal? (lvl "find . -exec {} +") 'opaque)
  ;; 无终止符（find 自身报错）→ opaque
  (check-equal? (lvl "find . -exec grep x {}") 'opaque)
  ;; + 仅在紧邻 {} 后才终止；否则不是终止符 → 无终止符 → opaque
  (check-equal? (lvl "find . -exec grep x y +") 'opaque)
) ; end test-case

(test-case "find 写谓词精确判 mutating（不再一律 opaque）"
  (check-equal? (lvl "find . -delete") 'mutating)
  (check-equal? (lvl "find . -name '*.tmp' -delete") 'mutating)
  (check-equal? (lvl "find . -fprintf out.txt %p") 'mutating)
  (check-equal? (lvl "find . -fprint dump.txt") 'mutating)
  (check-equal? (lvl "find . -fls listing.txt") 'mutating)
  ;; 写谓词的目标文件名被正确跳过，不误判为谓词
  (check-equal? (lvl "find . -fprint -weird-name.txt -type f") 'mutating)
) ; end test-case

(test-case "放行的边界：算术展开、herestring、单引号内的 $( 是字面量"
  (check-equal? (lvl "echo $((1+2))") 'read-only)
  (check-equal? (lvl "grep x <<< 'a b'") 'read-only)
  (check-equal? (lvl "echo '$(not a subst)'") 'read-only)
  (check-equal? (lvl "sed -n 1,5p f") 'read-only)
  (check-equal? (lvl "sed -i '' s/a/b/ f") 'mutating)
  (check-equal? (lvl "awk '{print $1}' f") 'read-only)
) ; end test-case

(test-case "外置程序体（-f）不建模其语言 → opaque（sed/awk 的黑名单洞修复）"
  (check-equal? (lvl "sed -f script.sed f") 'opaque)
  (check-equal? (lvl "sed --file=s.sed f") 'opaque)
  (check-equal? (lvl "awk -f prog.awk f") 'opaque)
  (check-equal? (lvl "gawk --file prog f") 'opaque)
  (check-equal? (lvl "awk 'BEGIN{print > \"o\"}'") 'opaque)
  (check-equal? (lvl "awk '{print | \"cat\"}' f") 'opaque)
  ;; 正常内联仍放行
  (check-equal? (lvl "awk '{sum+=$1} END{print sum}' f") 'read-only)
  (check-equal? (lvl "sed 's/foo/bar/g' f") 'read-only)
) ; end test-case

(test-case "白名单命令的写参数漏洞（审计补丁）：sort -o / uniq 输出位 / sed w 命令"
  (check-equal? (lvl "sort -o out.txt in.txt") 'mutating)
  (check-equal? (lvl "sort -oout.txt in.txt") 'mutating)
  (check-equal? (lvl "/usr/bin/sort -o out.txt in.txt") 'mutating)   ; 绝对路径不绕过专项规则
  (check-equal? (lvl "sort in.txt | head") 'read-only)
  (check-equal? (lvl "uniq -c f") 'read-only)
  (check-equal? (lvl "uniq f out") 'mutating)
  (check-equal? (lvl "sed -n 's/a/b/w out.txt' f") 'opaque)
  (check-equal? (lvl "sed 'w dump.txt' f") 'opaque)
  ;; 相对路径伪装：本地 ./sort 不吃 sort 规则，走递归检查（读不到 → 不可判）
  (check-equal? (lvl "./sort f") 'opaque)
) ; end test-case

(test-case "git：只读子命令白名单（bash 内联与 git 工具共用判据）"
  (check-equal? (lvl "git status && git log --oneline -5 && git diff HEAD~1") 'read-only)
  (check-equal? (lvl "git branch -a") 'read-only)
  (check-equal? (lvl "git push origin main") 'mutating)
  (check-equal? (lvl "git checkout -b x") 'mutating)
  (check-true (git-args-read-only? '("blame" "src/f.rkt")))
  (check-false (git-args-read-only? '("commit" "-m" "x")))
  (check-false (git-args-read-only? '()))
) ; end test-case

;; ------------------------------------------------------------ 调试/归因

(test-case "面包屑因果链：非只读判决在递归边界拼上下文，精确定位命令树位置"
  ;; find -exec ▸ sh -c ▸ command … : rm
  (define r1 (reason "find . -exec sh -c 'rm \"$1\"' _ {} +"))
  (check-true (string-contains? r1 "find -exec"))
  (check-true (string-contains? r1 "sh -c"))
  (check-true (string-contains? r1 "rm"))
  ;; command substitution ▸ curl
  (define r2 (reason "echo $(curl http://x)"))
  (check-true (string-contains? r2 "command substitution"))
  (check-true (string-contains? r2 "curl"))
  ;; script <path> ▸ …
  (define wpath (build-path tmp "wr.sh"))
  (display-to-file "#!/bin/bash\nrm -rf x\n" wpath #:exists 'truncate)
  (define r3 (reason "bash wr.sh"))
  (check-true (string-contains? r3 "script"))
  (check-true (string-contains? r3 "rm"))
  ;; verdict->debug 含级别、reason、检查过的脚本清单
  (define dbg (verdict->debug (classify-command "bash wr.sh" #:workdir tmp)))
  (check-true (string-contains? dbg "verdict: mutating"))
  (check-true (string-contains? dbg "inspected scripts:"))
  (check-true (string-contains? dbg "wr.sh"))
  ;; 只读判决不背归因噪声
  (check-equal? (cmd-verdict-reason (classify-command "ls -la" #:workdir tmp)) #f)
) ; end test-case

;; ------------------------------------------------------------ 脚本递归展开

(test-case "递归检查：只读脚本放行、写脚本拒绝、嵌套与循环、非 shell shebang"
  (define (w! name text) (display-to-file text (build-path tmp name) #:exists 'truncate))
  (w! "ok.sh" "#!/bin/bash\n# harmless\nls -la\ngrep foo *.txt | wc -l\n")
  (w! "evil.sh" "#!/bin/bash\nls\nrm -rf ~/important\n")
  (w! "outer.sh" "#!/bin/sh\necho start\nbash inner.sh\n")
  (w! "inner.sh" "cat data.txt\n")
  (w! "loop-a.sh" "bash loop-b.sh\n")
  (w! "loop-b.sh" "bash loop-a.sh\nls\n")
  (w! "py.sh" "#!/usr/bin/env python3\nprint(1)\n")
  (check-equal? (lvl "bash ok.sh") 'read-only)
  (check-equal? (lvl "./ok.sh") 'read-only)
  (check-equal? (lvl "source ok.sh") 'read-only)
  (check-equal? (lvl "bash evil.sh") 'mutating)
  (check-equal? (lvl "bash outer.sh") 'read-only)       ; 两层嵌套全只读
  (check-equal? (lvl "bash loop-a.sh") 'read-only)      ; 循环防护，不发散
  (check-equal? (lvl "bash py.sh") 'opaque)             ; 非 shell shebang 不可判
  (check-equal? (lvl "bash missing.sh") 'opaque)        ; 读不到即不可判
  ;; 快照：框架拿到实际执行的脚本内容（审计面）
  (define v (classify-command "bash outer.sh" #:workdir tmp))
  (define paths (map car (cmd-verdict-scripts v)))
  (check-equal? (length paths) 2)
  (check-true (for/and ([p (in-list paths)]) (string-contains? p tmp)))
  (check-true (for/or ([s (in-list (cmd-verdict-scripts v))])
                (string-contains? (cdr s) "cat data.txt")))
  ;; bash -c 内联体同样递归判级
  (check-equal? (lvl "bash -c 'ls | wc -l'") 'read-only)
  (check-equal? (lvl "bash -c 'rm x'") 'mutating)
) ; end test-case

;; ------------------------------------------------------------ 'read-only 权限模式

(define ro-cfg
  (struct-copy config (default-config) [permission-mode 'read-only] [workdir tmp]))
(define ro-policy (make-policy ro-cfg))
(define (never-ask _q) (error 'asker "read-only mode must never ask"))
(define (check-perm t input) (permission-check ro-policy t input never-ask))
(define (denied? r) (and (pair? r) (eq? (car r) 'deny)))

(test-case "'read-only 模式：读工具直通，写工具拒绝且不询问"
  (check-equal? (check-perm (make-read-file-tool) (hasheq 'path "x")) 'allow)
  (check-true (denied? (check-perm (make-write-file-tool) (hasheq 'path "x" 'content "y"))))
  (check-true (denied? (check-perm (make-edit-file-tool) (hasheq 'path "x" 'old_string "a"))))
) ; end test-case

(test-case "'read-only 模式：bash 经 cmdscan、git 经白名单、editor 仅 view"
  (define bash (make-bash-tool))
  (check-equal? (check-perm bash (hasheq 'command "grep -rn foo . | head")) 'allow)
  (check-equal? (check-perm bash (hasheq 'command "bash ok.sh")) 'allow)
  (define d1 (check-perm bash (hasheq 'command "rm -rf x")))
  (check-true (denied? d1))
  ;; 结构化拒绝：含 tool/why/rule/fix 四段，模型可据此自我修正
  (check-true (string-contains? (cdr d1) "read-only session"))
  (check-true (string-contains? (cdr d1) "why:"))
  (check-true (string-contains? (cdr d1) "rule:"))
  (check-true (string-contains? (cdr d1) "fix:"))
  (check-true (string-contains? (cdr d1) "Do not retry"))
  ;; 因果链穿透到最内层被否的命令
  (define d-nest (check-perm bash (hasheq 'command "find . -exec sh -c 'rm y' \\;")))
  (check-true (denied? d-nest))
  (check-true (string-contains? (cdr d-nest) "find -exec"))
  (check-true (string-contains? (cdr d-nest) "rm"))
  (check-true (denied? (check-perm bash (hasheq 'command "bash evil.sh"))))
  (check-equal? (check-perm bash (hasheq 'command "echo $(pwd)")) 'allow)   ; v2：替换体递归判级
  (check-true (denied? (check-perm bash (hasheq 'command "echo $(rm x)"))))
  (define git (make-git-tool))
  (check-equal? (check-perm git (hasheq 'args (list "status"))) 'allow)
  (check-true (denied? (check-perm git (hasheq 'args (list "commit" "-m" "x")))))
  (define ed (make-editor-tool))
  (check-equal? (check-perm ed (hasheq 'command "view" 'path "/x")) 'allow)
  (check-true (denied? (check-perm ed (hasheq 'command "create" 'path "/x" 'file_text "y"))))
) ; end test-case

(test-case "'read-only + --allow-write：作用域写例外（commit skill 场景：只许写 .commit）"
  (define wpol (make-policy ro-cfg #:write-allow '(".commit")))
  (define (wperm t input) (permission-check wpol t input never-ask))
  (define wf (make-write-file-tool))
  (define ef (make-edit-file-tool))
  (define ed (make-editor-tool))
  (define commit-abs (path->string (build-path tmp ".commit")))
  ;; 命中：.commit（绝对 / 相对 workdir）
  (check-equal? (wperm wf (hasheq 'path commit-abs 'content "x")) 'allow)
  (check-equal? (wperm wf (hasheq 'path ".commit" 'content "x")) 'allow)
  (check-equal? (wperm ef (hasheq 'path ".commit" 'old_string "a")) 'allow)
  (check-equal? (wperm ed (hasheq 'command "create" 'path commit-abs 'file_text "x")) 'allow)
  ;; 未命中：别的文件、workdir 外的同名文件
  (check-true (denied? (wperm wf (hasheq 'path "main.rkt" 'content "x"))))
  (check-true (denied? (wperm wf (hasheq 'path "/etc/.commit" 'content "x"))))
  (check-true (denied? (wperm ed (hasheq 'command "create" 'path "other.txt" 'file_text "x"))))
  ;; editor view 仍放行；bash 不吃写例外（重定向仍拒）
  (check-equal? (wperm ed (hasheq 'command "view" 'path ".commit")) 'allow)
  (check-true (denied? (wperm (make-bash-tool) (hasheq 'command "echo x > .commit"))))
  ;; 无 --allow-write 时（纯 read-only）一切写仍拒
  (check-true (denied? (check-perm wf (hasheq 'path ".commit" 'content "x"))))
  ;; glob 支持
  (define gpol (make-policy ro-cfg #:write-allow '("*.commit" "CHANGELOG.*")))
  (check-equal? (permission-check gpol wf (hasheq 'path "release.commit" 'content "x") never-ask) 'allow)
  (check-equal? (permission-check gpol wf (hasheq 'path "CHANGELOG.md" 'content "x") never-ask) 'allow)
  (check-true (denied? (permission-check gpol wf (hasheq 'path "x.txt" 'content "x") never-ask)))
) ; end test-case

(test-case "回归：yolo/normal/strict 矩阵不受影响"
  (define (mk mode) (make-policy (struct-copy config (default-config) [permission-mode mode])))
  (check-equal? (permission-check (mk 'yolo) (make-write-file-tool)
                                  (hasheq 'path "x" 'content "y") never-ask)
                'allow)
  (check-equal? (permission-check (mk 'normal) (make-write-file-tool)
                                  (hasheq 'path "x" 'content "y") never-ask)
                'allow)
  (check-equal? (permission-check (mk 'strict) (make-read-file-tool) (hasheq 'path "x") never-ask)
                'allow)
) ; end test-case

(delete-directory/files (string->path tmp))
