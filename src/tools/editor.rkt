#lang tstring racket
;; tools/editor.rkt — str_replace_editor（design-dsv4b.md §2.3）
;; DeepSeek Harness Minimal 预设编辑器的保真移植：schema（名称/参数/描述）与
;; 全部模型可见消息**逐字**对照上游 packages/fs/tool-str-replace-editor/src/index.ts。
;; 刻意偏差仅两处：create 自动补建父目录（pi2 惯例，上游行为未明）；无 sandbox 层。

(require
 racket/string
 racket/list
 racket/file
 racket/format
 (file "../tool.rkt")
 (file "file.rkt")                      ; atomic-write!
) ; end require

(define MAX-OUTPUT-CHARS 16000)

;; 上游 TRUNCATED_MESSAGE 逐字。
(define TRUNCATED-MESSAGE
  "<response clipped><NOTE>To save on context only part of this file has been shown to you. You should retry this tool after you have searched inside the file with `grep -n` in order to find the line numbers of what you are looking for.</NOTE>")

;; 上游 DEFAULT_DESCRIPTION 逐字（trim 后）。
(define EDITOR-DESCRIPTION
  (string-join
   '("Custom editing tool for viewing, creating and editing files"
     "* State is persistent across command calls and discussions with the user"
     "* If `path` is a file, `view` displays the result of applying `cat -n`. If `path` is a directory, `view` lists non-hidden files and directories up to 2 levels deep"
     "* The `create` command cannot be used if the specified `path` already exists as a file"
     "* If a `command` generates a long output, it will be truncated and marked with `<response clipped>`"
     ""
     "Notes for using the `str_replace` command:"
     "* The `old_str` parameter should match EXACTLY one or more consecutive lines from the original file. Be mindful of whitespaces!"
     "* If the `old_str` parameter is not unique in the file, the replacement will not be performed. Make sure to include enough context in `old_str` to make it unique"
     "* The `new_str` parameter should contain the edited lines that should replace the `old_str`"
    ) ; end list
   "\n"
  ) ; end string-join
) ; end define EDITOR-DESCRIPTION

(define (maybe-truncate s)
  (if (<= (string-length s) MAX-OUTPUT-CHARS)
      s
      (string-append (substring s 0 MAX-OUTPUT-CHARS) TRUNCATED-MESSAGE)
  ) ; end if
) ; end define maybe-truncate

;; JS content.split('\n') 语义：不丢尾空元素；"" → '("")。
(define (split-lines s) (string-split s "\n" #:trim? #f))

;; sub 在 text 中的全部起始偏移（不重叠，前进 |sub|——与上游 matchOffsets 一致）。
(define (match-offsets text sub)
  (let loop ([start 0] [acc '()])
    (define m (regexp-match-positions (regexp-quote sub) text start))
    (if m
        (loop (+ (caar m) (string-length sub)) (cons (caar m) acc))
        (reverse acc)
    ) ; end if
  ) ; end let loop
) ; end define match-offsets

;; 偏移 → 1 起行号（offsets 升序）。
(define (line-numbers-at text offsets)
  (for/list ([off (in-list offsets)])
    (add1 (count-newlines text off))
  ) ; end for/list
) ; end define line-numbers-at

(define (count-newlines text upto)
  (for/sum ([c (in-string text 0 upto)] #:when (char=? c #\newline)) 1)
) ; end define count-newlines

;; path 状态：'file | 'directory | #f
(define (path-kind p)
  (cond
    [(file-exists? p) 'file]
    [(directory-exists? p) 'directory]
    [else #f]
  ) ; end cond
) ; end define path-kind

;; ---------------------------------------------------------------- view

;; cat -n 风格：6 宽右对齐行号 + 两空格。
(define (format-file-view p content view-range)
  (define all-lines (split-lines content))
  (define total (length all-lines))
  (define-values (lines initial prompt-suffix err)
    (cond
      [(not view-range) (values all-lines 1 "" #f)]
      [(or (not (list? view-range))
           (not (= 2 (length view-range)))
           (not (andmap exact-integer? view-range)))
       (values #f #f #f "Invalid `view_range`. It should be a list of two integers.")]
      [else
       (define a (first view-range))
       (define b (second view-range))
       (cond
         [(or (< a 1) (> a total))
          (values #f #f #f
                  f"Invalid `view_range`: [{a}, {b}]. Its first element `{a}` should be within the range of lines of the file: [1, {total}]")]
         [(> b total)
          (values #f #f #f
                  f"Invalid `view_range`: [{a}, {b}]. Its second element `{b}` should be smaller than the number of lines in the file: `{total}`")]
         [(and (not (= b -1)) (< b a))
          (values #f #f #f
                  f"Invalid `view_range`: [{a}, {b}]. Its second element `{b}` should be larger or equal than its first `{a}`")]
         [else
          (values (if (= b -1) (drop all-lines (sub1 a)) (take (drop all-lines (sub1 a)) (- b (sub1 a))))
                  a f" with view_range=[{a}, {b}]" #f)]
       ) ; end cond
      ] ; end else
    ) ; end cond
  ) ; end define-values
  (cond
    [err (values #f err)]
    [else
     (define numbered
       (string-join
        (for/list ([l (in-list lines)] [i (in-naturals initial)])
          (string-append (~a i #:min-width 6 #:align 'right) "  " l))
        "\n"
       ) ; end string-join
     ) ; end define numbered
     (values
      (maybe-truncate
       f"Here's the content of {p} with line numbers (which has a total of {total} lines){prompt-suffix}:\n{numbered}\n")
      #f)
    ] ; end else
  ) ; end cond
) ; end define format-file-view

;; 目录列表：2 层深，排除隐藏项/node_modules/__pycache__；d/f 行按路径排序。
(define (list-directory p)
  (define (visit dir depth)
    (append*
     (for/list ([name (in-list (with-handlers ([exn:fail? (lambda (_e) '())])
                                 (directory-list dir)))]
                #:unless (let ([n (path->string name)])
                           (or (string-prefix? n ".")
                               (string=? n "node_modules")
                               (string=? n "__pycache__"))))
       (define full (build-path dir name))
       (define kind (path-kind full))
       (define row
         (string-append (case kind [(directory) "d"] [(file) "f"] [else "?"])
                        "\t" (path->string full)))
       (if (and (eq? kind 'directory) (< depth 2))
           (cons row (visit full (add1 depth)))
           (list row)
       ) ; end if
     ) ; end for/list
    ) ; end append*
  ) ; end define visit
  (define rows
    (sort (cons (string-append "d\t" p) (visit p 1))
          string<?
          #:key (lambda (row) (substring row (add1 (or (string-index row #\tab) 0))))
    ) ; end sort
  ) ; end define rows
  (define listing (maybe-truncate (string-append (string-join rows "\n") "\n")))
  f"Here're the files and directories up to 2 levels deep in {p}, excluding hidden items, node_modules, and Python cache directories:\n{listing}\n"
) ; end define list-directory

(define (string-index s ch)
  (for/first ([c (in-string s)] [i (in-naturals)] #:when (char=? c ch)) i)
) ; end define string-index

;; ---------------------------------------------------------------- 工具本体

(struct editor-tool ()
  #:methods gen:tool
  [(define (tool-name _t) "str_replace_editor")
   (define (tool-permission-level _t) 'mutating)
   (define (tool-spec _t)
     (function-spec "str_replace_editor"
                    EDITOR-DESCRIPTION
                    (hasheq 'command
                            (hasheq 'type "string"
                                    'enum (list "view" "create" "str_replace" "insert")
                                    'description "The commands to run. Allowed options are: `view`, `create`, `str_replace`, `insert`.")
                            'path
                            (hasheq 'type "string"
                                    'description "Absolute path to file or directory, e.g. `/repo/file.py` or `/repo`.")
                            'file_text
                            (hasheq 'type "string"
                                    'description "Required parameter of `create` command, with the content of the file to be created.")
                            'insert_line
                            (hasheq 'type "integer"
                                    'description "Required parameter of `insert` command. The `new_str` will be inserted AFTER the line `insert_line` of `path`.")
                            'new_str
                            (hasheq 'type "string"
                                    'description "Optional parameter of `str_replace` command containing the new string (if not given, no string will be added). Required parameter of `insert` command containing the string to insert.")
                            'old_str
                            (hasheq 'type "string"
                                    'description "Required parameter of `str_replace` command containing the string in `path` to replace.")
                            'view_range
                            (hasheq 'type "array"
                                    'items (hasheq 'type "integer")
                                    'description "Optional parameter of `view` command when `path` points to a file. If none is given, the full file is shown. If provided, the file will be shown in the indicated line number range, e.g. [11, 12] will show lines 11 and 12. Indexing at 1 to start. Setting `[start_line, -1]` shows all lines from `start_line` to the end of the file.")
                    ) ; end hasheq
                    (list "command" "path")
     ) ; end function-spec
   ) ; end define tool-spec
   (define (tool-run _t input _ctx)
     (define cmd (input-str input 'command))
     (define p (input-str input 'path))
     (cond
       [(not cmd) (err-outcome "Parameter `command` is required.")]
       [(or (not p) (string=? (string-trim p) ""))
        (err-outcome "path must be a non-empty string")]
       [(not (absolute-path? p))
        (err-outcome
         f"The path {p} is not an absolute path, it should start with `/`. Maybe you meant /{p}?")]
       [else
        (case cmd
          [("view")        (run-view p input)]
          [("create")      (run-create p input)]
          [("str_replace") (run-str-replace p input)]
          [("insert")      (run-insert p input)]
          [else (err-outcome f"Unrecognized command {cmd}. The allowed commands are: `view`, `create`, `str_replace`, `insert`.")]
        ) ; end case
       ] ; end else
     ) ; end cond
   ) ; end define tool-run
  ] ; end methods
) ; end struct editor-tool

;; 存在性/类型检查（view 外的命令拒绝目录）。返回 kind 或 err-outcome。
(define (stat-existing p command)
  (define kind (path-kind p))
  (cond
    [(not kind)
     (err-outcome f"The path {p} does not exist. Please provide a valid path.")]
    [(and (eq? kind 'directory) (not (string=? command "view")))
     (err-outcome f"The path {p} is a directory and only the `view` command can be used on directories")]
    [else kind]
  ) ; end cond
) ; end define stat-existing

(define (run-view p input)
  (define kind (stat-existing p "view"))
  (define view-range (let ([v (input-ref input 'view_range #f)]) (if (eq? v 'null) #f v)))
  (cond
    [(tool-outcome? kind) kind]
    [(eq? kind 'directory)
     (if view-range
         (err-outcome "The `view_range` parameter is not allowed when `path` points to a directory.")
         (ok-outcome (list-directory p) #:display f"view {p}"))]
    [else
     (define-values (out err) (format-file-view p (file->string p) view-range))
     (if err (err-outcome err) (ok-outcome out #:display f"view {p}"))]
  ) ; end cond
) ; end define run-view

(define (run-create p input)
  (define text (input-str input 'file_text))
  (cond
    [(not text) (err-outcome "Parameter `file_text` is required for command: create")]
    [(path-kind p)
     (err-outcome f"File already exists at: {p}. Cannot overwrite files using command `create`.")]
    [else
     (define-values (dir _n _d?) (split-path (string->path p)))
     (when (path? dir) (make-directory* dir))
     (atomic-write! (string->path p) text)
     (ok-outcome f"New file created successfully at: {p}" #:display f"create {p}")
    ] ; end else
  ) ; end cond
) ; end define run-create

(define (run-str-replace p input)
  (define old-s (input-str input 'old_str))
  (define new-s (or (input-str input 'new_str) ""))
  (cond
    [(not old-s) (err-outcome "Parameter `old_str` is required for command: str_replace")]
    [(string=? old-s "") (err-outcome "Parameter `old_str` is empty for command: str_replace")]
    [else
     (define kind (stat-existing p "str_replace"))
     (cond
       [(tool-outcome? kind) kind]
       [else
        (define before (file->string p))
        (define offsets (match-offsets before old-s))
        (cond
          [(null? offsets)
           (err-outcome
            f"No replacement was performed, old_str `{old-s}` did not appear verbatim in {p}.")]
          [(> (length offsets) 1)
           (define lns (string-join (map number->string (line-numbers-at before offsets)) ", "))
           (err-outcome
            f"No replacement was performed. Multiple occurrences of old_str `{old-s}` in lines [{lns}]. Please ensure it is unique")]
          [else
           (define off (car offsets))
           (atomic-write! (string->path p)
                          (string-append (substring before 0 off)
                                         new-s
                                         (substring before (+ off (string-length old-s)))))
           (ok-outcome f"The file {p} has been edited successfully."
                       #:display f"str_replace {p}")
          ] ; end else
        ) ; end cond
       ] ; end else
     ) ; end cond
    ] ; end else
  ) ; end cond
) ; end define run-str-replace

(define (run-insert p input)
  (define line (input-int input 'insert_line))
  (define new-s (input-str input 'new_str))
  (cond
    [(not (exact-integer? line))
     (err-outcome "Parameter `insert_line` is required for command: insert")]
    [(not new-s) (err-outcome "Parameter `new_str` is required for command: insert")]
    [else
     (define kind (stat-existing p "insert"))
     (cond
       [(tool-outcome? kind) kind]
       [else
        (define before (file->string p))
        (define lines (split-lines before))
        (define n (length lines))
        (cond
          [(or (< line 0) (> line n))
           (err-outcome
            f"Invalid `insert_line` parameter: {line}. It should be within the range of lines of the file: [0, {n}]")]
          [else
           (define after
             (string-join (append (take lines line) (split-lines new-s) (drop lines line)) "\n"))
           (atomic-write! (string->path p) after)
           (ok-outcome f"The file {p} has been edited successfully."
                       #:display f"insert {p}")
          ] ; end else
        ) ; end cond
       ] ; end else
     ) ; end cond
    ] ; end else
  ) ; end cond
) ; end define run-insert

(define (make-editor-tool) (editor-tool))

(provide
 make-editor-tool
 EDITOR-DESCRIPTION
 match-offsets
 line-numbers-at
 format-file-view
) ; end provide
