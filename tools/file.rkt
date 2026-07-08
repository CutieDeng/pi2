#lang racket-tstring
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
                    "Replace an exact string in a file. old_string must appear exactly once."
                    (hasheq 'path (hasheq 'type "string")
                            'old_string (hasheq 'type "string")
                            'new_string (hasheq 'type "string")
                    ) ; end hasheq
                    (list "path" "old_string" "new_string")
     ) ; end function-spec
   ) ; end define tool-spec
   (define (tool-run _t input ctx)
     (define p (input-str input 'path))
     (define old-s (input-str input 'old_string))
     (define new-s (input-str input 'new_string))
     (cond
       [(or (not p) (not old-s) (not new-s))
        (err-outcome "missing required parameter: path, old_string, new_string")
       ] ; end missing case
       [else
        (define fp (resolve ctx p))
        (cond
          [(not (file-exists? fp)) (err-outcome f"file not found: {p}")]
          [else
           (define text (file->string fp))
           (define n (count-occurrences text old-s))
           (cond
             [(zero? n)
              (err-outcome
               f"old_string not found in {p}. Read the file again and match the exact text including whitespace."
              ) ; end err-outcome
             ] ; end zero case
             [(> n 1)
              (err-outcome
               f"old_string appears {n} times in {p}; it must be unique. Add surrounding context to disambiguate."
              ) ; end err-outcome
             ] ; end multi case
             [else
              (atomic-write! fp (string-replace text old-s new-s))
              (ok-outcome f"edited {p}: 1 replacement"
                          #:display f"edit {p}"
              ) ; end ok-outcome
             ] ; end else
           ) ; end cond
          ] ; end else
        ) ; end cond
       ] ; end else
     ) ; end cond
   ) ; end define tool-run
  ] ; end methods
) ; end struct edit-file-tool

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
) ; end provide
