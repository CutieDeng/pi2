#lang tstring racket
;; editor-test.rkt — str_replace_editor 保真移植（design-dsv4b.md §2.3 / §5.2）。
;; 消息措辞逐字对照上游 deepseek-harness tool-str-replace-editor，故断言用全文比对。

(require
 rackunit
 racket/file
 racket/string
 (file "../src/tool.rkt")
 (file "../src/tools/editor.rkt")
) ; end require

(define t (make-editor-tool))
(define tmp (path->string (make-temporary-file "pi2-edtest-~a" 'directory)))
(define ctx (tool-ctx tmp void #f))

(define (run . kvs)
  (tool-run t (apply hasheq kvs) ctx)
) ; end define run

(define (content oc) (tool-outcome-content oc))
(define (err? oc) (tool-outcome-is-error? oc))
(define (fp name) (string-append tmp "/" name))

;; ------------------------------------------------------------ schema

(test-case "schema: name/required/enum 对齐上游"
  (check-equal? (tool-name t) "str_replace_editor")
  (check-equal? (tool-permission-level t) 'mutating)
  (define spec (tool-spec t))
  (define fn (hash-ref spec 'function))
  (check-equal? (hash-ref fn 'name) "str_replace_editor")
  (define params (hash-ref (hash-ref fn 'parameters) 'properties))
  (check-equal? (hash-ref (hash-ref (hash-ref fn 'parameters) 'properties) 'command
                          (hasheq))
                (hasheq 'type "string"
                        'enum (list "view" "create" "str_replace" "insert")
                        'description "The commands to run. Allowed options are: `view`, `create`, `str_replace`, `insert`."))
  (check-equal? (sort (hash-ref (hash-ref fn 'parameters) 'required) string<?)
                '("command" "path"))
  (for ([k (in-list '(path file_text insert_line new_str old_str view_range))])
    (check-true (hash-has-key? params k) (format "参数 ~a 应存在" k)))
) ; end test-case

;; ------------------------------------------------------------ 路径与存在性

(test-case "相对路径拒绝（上游措辞）"
  (define oc (run 'command "view" 'path "foo.txt"))
  (check-true (err? oc))
  (check-equal? (content oc)
                "The path foo.txt is not an absolute path, it should start with `/`. Maybe you meant /foo.txt?")
) ; end test-case

(test-case "不存在的路径（上游措辞）"
  (define p (fp "nope.txt"))
  (define oc (run 'command "view" 'path p))
  (check-true (err? oc))
  (check-equal? (content oc) f"The path {p} does not exist. Please provide a valid path.")
) ; end test-case

;; ------------------------------------------------------------ create

(test-case "create：成功 → 已存在拒绝"
  (define p (fp "a.txt"))
  (define oc (run 'command "create" 'path p 'file_text "a\nb\nc\n"))
  (check-false (err? oc))
  (check-equal? (content oc) f"New file created successfully at: {p}")
  (check-equal? (file->string p) "a\nb\nc\n")
  (define oc2 (run 'command "create" 'path p 'file_text "x"))
  (check-true (err? oc2))
  (check-equal? (content oc2)
                f"File already exists at: {p}. Cannot overwrite files using command `create`.")
  (define oc3 (run 'command "create" 'path (fp "b.txt")))
  (check-true (err? oc3))
  (check-equal? (content oc3) "Parameter `file_text` is required for command: create")
) ; end test-case

;; ------------------------------------------------------------ view

(test-case "view 文件：cat -n 风格 6 宽行号；尾随换行按 JS split 语义计行"
  (define p (fp "a.txt"))                                      ; "a\nb\nc\n" → 4 行
  (define oc (run 'command "view" 'path p))
  (check-false (err? oc))
  (define s (content oc))
  (check-true (string-prefix? s f"Here's the content of {p} with line numbers (which has a total of 4 lines):\n"))
  (check-true (string-contains? s "     1  a\n     2  b\n     3  c\n"))
) ; end test-case

(test-case "view_range：命中/越界/逆序/[start,-1]"
  (define p (fp "a.txt"))
  (define ok (run 'command "view" 'path p 'view_range (list 2 2)))
  (check-true (string-contains? (content ok) "with view_range=[2, 2]"))
  (check-true (string-contains? (content ok) "     2  b"))
  (check-false (string-contains? (content ok) "     1  a"))
  (define tail (run 'command "view" 'path p 'view_range (list 2 -1)))
  (check-true (string-contains? (content tail) "     3  c"))
  (define bad1 (run 'command "view" 'path p 'view_range (list 0 2)))
  (check-equal? (content bad1)
                "Invalid `view_range`: [0, 2]. Its first element `0` should be within the range of lines of the file: [1, 4]")
  (define bad2 (run 'command "view" 'path p 'view_range (list 1 9)))
  (check-equal? (content bad2)
                "Invalid `view_range`: [1, 9]. Its second element `9` should be smaller than the number of lines in the file: `4`")
  (define bad3 (run 'command "view" 'path p 'view_range (list 3 2)))
  (check-equal? (content bad3)
                "Invalid `view_range`: [3, 2]. Its second element `2` should be larger or equal than its first `3`")
  (define bad4 (run 'command "view" 'path p 'view_range (list 1)))
  (check-equal? (content bad4) "Invalid `view_range`. It should be a list of two integers.")
) ; end test-case

(test-case "view 目录：2 层、排除隐藏项，d/f 行；view_range 拒绝"
  (define sub (fp "dir"))
  (make-directory* (string-append sub "/inner"))
  (display-to-file "x" (string-append sub "/f1.txt"))
  (display-to-file "x" (string-append sub "/.hidden"))
  (display-to-file "x" (string-append sub "/inner/f2.txt"))
  (define oc (run 'command "view" 'path sub))
  (define s (content oc))
  (check-true (string-prefix? s f"Here're the files and directories up to 2 levels deep in {sub}, excluding hidden items, node_modules, and Python cache directories:\n"))
  (check-true (string-contains? s f"d\t{sub}\n"))
  (check-true (string-contains? s f"f\t{sub}/f1.txt"))
  (check-true (string-contains? s f"d\t{sub}/inner"))
  (check-true (string-contains? s f"f\t{sub}/inner/f2.txt"))
  (check-false (string-contains? s ".hidden"))
  (define bad (run 'command "view" 'path sub 'view_range (list 1 2)))
  (check-equal? (content bad)
                "The `view_range` parameter is not allowed when `path` points to a directory.")
  (define bad2 (run 'command "str_replace" 'path sub 'old_str "x"))
  (check-equal? (content bad2)
                f"The path {sub} is a directory and only the `view` command can be used on directories")
) ; end test-case

;; ------------------------------------------------------------ str_replace

(test-case "str_replace：唯一命中/缺失/多处/空 old_str"
  (define p (fp "r.txt"))
  (void (run 'command "create" 'path p 'file_text "one\ntwo\nthree\ntwo\n"))
  (define miss (run 'command "str_replace" 'path p 'old_str "zzz"))
  (check-equal? (content miss)
                f"No replacement was performed, old_str `zzz` did not appear verbatim in {p}.")
  (define multi (run 'command "str_replace" 'path p 'old_str "two"))
  (check-equal? (content multi)
                f"No replacement was performed. Multiple occurrences of old_str `two` in lines [2, 4]. Please ensure it is unique")
  (define empty (run 'command "str_replace" 'path p 'old_str ""))
  (check-equal? (content empty) "Parameter `old_str` is empty for command: str_replace")
  (define missing (run 'command "str_replace" 'path p))
  (check-equal? (content missing) "Parameter `old_str` is required for command: str_replace")
  (define ok (run 'command "str_replace" 'path p 'old_str "three" 'new_str "3"))
  (check-false (err? ok))
  (check-equal? (content ok) f"The file {p} has been edited successfully.")
  (check-equal? (file->string p) "one\ntwo\n3\ntwo\n")
  ;; new_str 缺省 = 删除
  (void (run 'command "str_replace" 'path p 'old_str "one\n"))
  (check-equal? (file->string p) "two\n3\ntwo\n")
) ; end test-case

;; ------------------------------------------------------------ insert

(test-case "insert：行后插入/0 行前插/越界/参数缺失"
  (define p (fp "i.txt"))
  (void (run 'command "create" 'path p 'file_text "a\nb\n"))
  (define ok (run 'command "insert" 'path p 'insert_line 1 'new_str "x"))
  (check-equal? (content ok) f"The file {p} has been edited successfully.")
  (check-equal? (file->string p) "a\nx\nb\n")
  (void (run 'command "insert" 'path p 'insert_line 0 'new_str "top"))
  (check-equal? (file->string p) "top\na\nx\nb\n")
  (define bad (run 'command "insert" 'path p 'insert_line 99 'new_str "x"))
  (check-equal? (content bad)
                "Invalid `insert_line` parameter: 99. It should be within the range of lines of the file: [0, 5]")
  (define m1 (run 'command "insert" 'path p 'new_str "x"))
  (check-equal? (content m1) "Parameter `insert_line` is required for command: insert")
  (define m2 (run 'command "insert" 'path p 'insert_line 1))
  (check-equal? (content m2) "Parameter `new_str` is required for command: insert")
) ; end test-case

(delete-directory/files (string->path tmp))
