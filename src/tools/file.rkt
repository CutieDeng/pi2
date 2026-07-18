#lang tstring racket
;; tools/file.rkt — read_file / write_file / edit_file（design.md §5.3）

(require
 racket/string
 racket/file
 racket/list
 (file "../tool.rkt")
) ; end require

(define READ-MAX-LINES 2000)
(define READ-MAX-LINE-CHARS 500)

;; 解析相对 workdir 的路径
(define (resolve ctx p)
  (path->complete-path p (tool-ctx-workdir ctx))
) ; end define resolve

;; ---------------------------------------------------------------- read_file

(struct read-file-tool ()
  #:methods gen:tool
  [(define (tool-name _t) "read_file")
   (define (tool-permission-level _t) 'read-only)
   (define (tool-spec _t)
     (function-spec "read_file"
                    "Read a text file. Returns numbered lines. Use offset/limit for large files."
                    (hasheq 'path (hasheq 'type "string" 'description "File path")
                            'offset (hasheq 'type "integer"
                                            'description "1-based start line (default 1)")
                            'limit (hasheq 'type "integer"
                                           'description f"Max lines to return (default {READ-MAX-LINES})")
                    ) ; end hasheq
                    (list "path")
     ) ; end function-spec
   ) ; end define tool-spec
   (define (tool-run _t input ctx)
     (define p (input-str input 'path))
     (define offset (max 1 (or (input-int input 'offset) 1)))
     (define limit (max 1 (or (input-int input 'limit) READ-MAX-LINES)))
     (cond
       [(not p) (err-outcome "missing required parameter: path")]
       [else
        (define fp (resolve ctx p))
        (cond
          [(not (file-exists? fp)) (err-outcome f"file not found: {p}")]
          [else
           (define lines (file->lines fp))
           (define total (length lines))
           (define slice
             (take (drop lines (min (sub1 offset) total))
                   (min limit (max 0 (- total (sub1 offset))))
             ) ; end take
           ) ; end define slice
           (define numbered
             (string-join
              (for/list ([l (in-list slice)] [i (in-naturals offset)])
                (define l* (if (> (string-length l) READ-MAX-LINE-CHARS)
                               (string-append (substring l 0 READ-MAX-LINE-CHARS) "…")
                               l
                           ) ; end if
                ) ; end define l*
                f"{i}\t{l*}"
              ) ; end for/list
              "\n"
             ) ; end string-join
           ) ; end define numbered
           (ok-outcome
            (if (string=? numbered "")
                "(empty file)"
                f"{numbered}\n[file has {total} lines total]"
            ) ; end if
            #:display f"read {p} ({total} lines)"
           ) ; end ok-outcome
          ] ; end else
        ) ; end cond
       ] ; end else
     ) ; end cond
   ) ; end define tool-run
  ] ; end methods
) ; end struct read-file-tool

;; --------------------------------------------------------------- write_file

(struct write-file-tool ()
  #:methods gen:tool
  [(define (tool-name _t) "write_file")
   (define (tool-permission-level _t) 'mutating)
   (define (tool-spec _t)
     (function-spec "write_file"
                    "Write content to a file (overwrites; creates parent dirs)."
                    (hasheq 'path (hasheq 'type "string")
                            'content (hasheq 'type "string")
                    ) ; end hasheq
                    (list "path" "content")
     ) ; end function-spec
   ) ; end define tool-spec
   (define (tool-run _t input ctx)
     (define p (input-str input 'path))
     (define content (input-str input 'content))
     (cond
       [(or (not p) (not content))
        (err-outcome "missing required parameter: path and content")
       ] ; end missing case
       [else
        (define fp (resolve ctx p))
        (define-values (dir _n _d?) (split-path fp))
        (when (path? dir)
          (make-directory* dir)
        ) ; end when
        (atomic-write! fp content)
        (ok-outcome f"wrote {(string-length content)} chars to {p}"
                    #:display f"write {p}"
        ) ; end ok-outcome
       ] ; end else
     ) ; end cond
   ) ; end define tool-run
  ] ; end methods
) ; end struct write-file-tool

;; 原子写：临时文件 + rename
(define (atomic-write! fp content)
  (define tmp (path-add-extension fp #".pi2tmp"))
  (call-with-output-file tmp #:exists 'truncate
    (lambda (out)
      (write-string content out)
    ) ; end lambda
  ) ; end call-with-output-file
  (rename-file-or-directory tmp fp #t)
) ; end define atomic-write!

;; ---------------------------------------------------------------- edit_file

(struct edit-file-tool ()
  #:methods gen:tool
  [(define (tool-name _t) "edit_file")
   (define (tool-permission-level _t) 'mutating)
   (define (tool-spec _t)
     (function-spec "edit_file"
                    (string-append
                     "Replace exact string(s) in a file. Single edit: pass old_string + new_string "
                     "(old_string must be unique unless replace_all=true, which replaces every occurrence). "
                     "Batch: pass edits=[{old_string,new_string,replace_all?}, …] applied in order and "
                     "ATOMICALLY (all-or-nothing — if any edit fails, the file is left unchanged). "
                     "Batching many edits in one call is preferred over many separate calls.")
                    (hasheq 'path (hasheq 'type "string" 'description "File path")
                            'old_string (hasheq 'type "string" 'description "Exact text to replace (single-edit mode)")
                            'new_string (hasheq 'type "string" 'description "Replacement text (single-edit mode; \"\" deletes)")
                            'replace_all (hasheq 'type "boolean"
                                                 'description "Replace all occurrences instead of requiring uniqueness (default false)")
                            'edits (hasheq 'type "array"
                                           'description "Batch mode: list of {old_string, new_string, replace_all?} applied in order, atomically"
                                           'items (hasheq 'type "object"))
                    ) ; end hasheq
                    (list "path")
     ) ; end function-spec
   ) ; end define tool-spec
   (define (tool-run _t input ctx)
     (define p (input-str input 'path))
     (cond
       [(not p) (err-outcome "missing required parameter: path")]
       [else
        (define fp (resolve ctx p))
        (cond
          [(not (file-exists? fp)) (err-outcome f"file not found: {p}")]
          [else
           (define edits (normalize-edits input))
           (cond
             [(not edits)
              (err-outcome "provide old_string (+ new_string), or an edits array")]
             [else
              (define text (file->string fp))
              (define-values (text* applied errmsg) (apply-edits text edits))
              (cond
                [errmsg (err-outcome errmsg)]              ; 任一编辑失败 → 整体不落盘
                [else
                 (atomic-write! fp text*)
                 (ok-outcome f"edited {p}: {applied} replacement(s) across {(length edits)} edit(s)"
                             #:display f"edit {p}")])])]
        ) ; end cond
       ] ; end else
     ) ; end cond
   ) ; end define tool-run
  ] ; end methods
) ; end struct edit-file-tool

;; JSON 布尔/缺省 → Racket 真值（'null 视作假）
(define (truthy v) (and v (not (eq? v 'null))))

;; 从 input 归一出编辑列表 (list (list old new replace-all?) …)，无有效输入 → #f。
;; 优先 edits 数组（批量）；否则退回单条 old_string/new_string(+replace_all)。
(define (normalize-edits input)
  (define raw (input-ref input 'edits #f))
  (cond
    [(and (list? raw) (pair? raw))
     (for/list ([e (in-list raw)])
       (list (and (hash? e) (let ([v (hash-ref e 'old_string #f)]) (and (string? v) v)))
             (and (hash? e) (let ([v (hash-ref e 'new_string "")]) (if (string? v) v "")))
             (and (hash? e) (truthy (hash-ref e 'replace_all #f)))))]
    [else
     (define old-s (input-str input 'old_string))
     (if old-s
         (list (list old-s (or (input-str input 'new_string) "") (truthy (input-ref input 'replace_all #f))))
         #f)]
  ) ; end cond
) ; end define normalize-edits

(define (clip-str s) (if (> (string-length s) 60) (string-append (substring s 0 60) "…") s))

;; 顺序把 edits 逐条应用到 text；任一步失败即返回错误（不改 text），成功返回
;; (values 新文本 替换总数 #f)。edits 元素 = (list old new replace-all?)。
(define (apply-edits text edits)
  (let loop ([text text] [es edits] [i 1] [count 0])
    (cond
      [(null? es) (values text count #f)]
      [else
       (define old (car (car es)))
       (define new (cadr (car es)))
       (define ra? (caddr (car es)))
       (cond
         [(not (string? old)) (values text count f"edit #{i}: missing old_string")]
         [else
          (define n (count-occurrences text old))
          (cond
            [(zero? n)
             (values text count
                     f"edit #{i}: old_string not found. Match the exact text including whitespace: {(clip-str old)}")]
            [(and (> n 1) (not ra?))
             (values text count
                     f"edit #{i}: old_string appears {n} times; set replace_all:true or add surrounding context to disambiguate.")]
            [else (loop (string-replace text old new) (cdr es) (add1 i) (+ count n))])])]
    ) ; end cond
  ) ; end let loop
) ; end define apply-edits

(define (count-occurrences text sub)
  (let loop ([start 0] [n 0])
    (define i (and (<= start (string-length text))
                   (let ([m (regexp-match-positions (regexp-quote sub) text start)])
                     (and m (caar m))
                   ) ; end let
               ) ; end and
    ) ; end define i
    (if i
        (loop (add1 i) (add1 n))
        n
    ) ; end if
  ) ; end let loop
) ; end define count-occurrences

;; ---------------------------------------------------------------- provide

(define (make-read-file-tool) (read-file-tool))
(define (make-write-file-tool) (write-file-tool))
(define (make-edit-file-tool) (edit-file-tool))

(provide
 make-read-file-tool
 make-write-file-tool
 make-edit-file-tool
 count-occurrences
 apply-edits normalize-edits
) ; end provide
