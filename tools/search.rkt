#lang racket-tstring
;; tools/search.rkt — glob / grep（design.md §5.3；纯 Racket 实现，无 rg 依赖）

(require
 racket/string
 racket/list
 racket/file
 file/glob
 (file "../tool.rkt")
) ; end require

(define MAX-RESULTS 200)
(define GREP-MAX-MATCHES 100)
(define SKIP-DIRS (list ".git" "compiled" "node_modules" ".DS_Store"))

(define (skip-path? p)
  (for/or ([part (in-list (explode-path p))])
    (member (path->string* part) SKIP-DIRS)
  ) ; end for/or
) ; end define skip-path?

(define (path->string* p)
  (if (path? p) (path->string p) (format "~a" p))
) ; end define path->string*

;; ---------------------------------------------------------------- glob

(struct glob-tool ()
  #:methods gen:tool
  [(define (tool-name _t) "glob")
   (define (tool-permission-level _t) 'read-only)
   (define (tool-spec _t)
     (function-spec "glob"
                    "Find files matching a glob pattern (e.g. \"**/*.rkt\") under the working directory."
                    (hasheq 'pattern (hasheq 'type "string"))
                    (list "pattern")
     ) ; end function-spec
   ) ; end define tool-spec
   (define (tool-run _t input ctx)
     (define pat (input-str input 'pattern))
     (cond
       [(not pat) (err-outcome "missing required parameter: pattern")]
       [else
        (parameterize ([current-directory (tool-ctx-workdir ctx)])
          (define hits
            (with-handlers ([exn:fail? (lambda (e) e)])
              (for/list ([p (in-glob pat)]
                         #:unless (skip-path? p)
                         #:final #f)
                (path->string* p)
              ) ; end for/list
            ) ; end with-handlers
          ) ; end define hits
          (cond
            [(exn? hits) (err-outcome f"bad glob pattern: {(exn-message hits)}")]
            [(null? hits) (ok-outcome "(no matches)")]
            [else
             (define shown (take hits (min MAX-RESULTS (length hits))))
             (ok-outcome
              (string-append
               (string-join shown "\n")
               (if (> (length hits) MAX-RESULTS)
                   f"\n[{(- (length hits) MAX-RESULTS)} more not shown]"
                   ""
               ) ; end if
              ) ; end string-append
              #:display f"glob {pat}: {(length hits)} hits"
             ) ; end ok-outcome
            ] ; end else
          ) ; end cond
        ) ; end parameterize
       ] ; end else
     ) ; end cond
   ) ; end define tool-run
  ] ; end methods
) ; end struct glob-tool

;; ---------------------------------------------------------------- grep

(struct grep-tool ()
  #:methods gen:tool
  [(define (tool-name _t) "grep")
   (define (tool-permission-level _t) 'read-only)
   (define (tool-spec _t)
     (function-spec "grep"
                    "Search file contents with a regex under the working directory. Returns file:line:text matches."
                    (hasheq 'pattern (hasheq 'type "string"
                                             'description "Regular expression")
                            'glob (hasheq 'type "string"
                                          'description "Optional glob to filter files (e.g. \"**/*.rkt\")")
                    ) ; end hasheq
                    (list "pattern")
     ) ; end function-spec
   ) ; end define tool-spec
   (define (tool-run _t input ctx)
     (define pat-s (input-str input 'pattern))
     (define file-glob (input-str input 'glob))
     (cond
       [(not pat-s) (err-outcome "missing required parameter: pattern")]
       [else
        (define pat
          (with-handlers ([exn:fail? (lambda (e) e)])
            (regexp pat-s)
          ) ; end with-handlers
        ) ; end define pat
        (cond
          [(exn? pat) (err-outcome f"bad regex: {(exn-message pat)}")]
          [else
           (parameterize ([current-directory (tool-ctx-workdir ctx)])
             (define files
               (if file-glob
                   (for/list ([p (in-glob file-glob)]
                              #:unless (skip-path? p)
                              #:when (file-exists? p))
                     p
                   ) ; end for/list
                   (for/list ([p (in-directory)]
                              #:unless (skip-path? p)
                              #:when (file-exists? p))
                     p
                   ) ; end for/list
               ) ; end if
             ) ; end define files
             (define matches
               (let/ec done
                 (for/fold ([acc '()]) ([f (in-list files)])
                   (define acc*
                     (with-handlers ([exn:fail? (lambda (_e) acc)])   ; 二进制/不可读跳过
                       (for/fold ([a acc])
                                 ([line (in-list (file->lines f))]
                                  [ln (in-naturals 1)])
                         (if (regexp-match? pat line)
                             (cons f"{(path->string* f)}:{ln}:{line}" a)
                             a
                         ) ; end if
                       ) ; end for/fold
                     ) ; end with-handlers
                   ) ; end define acc*
                   (if (>= (length acc*) GREP-MAX-MATCHES)
                       (done acc*)
                       acc*
                   ) ; end if
                 ) ; end for/fold
               ) ; end let/ec
             ) ; end define matches
             (if (null? matches)
                 (ok-outcome "(no matches)")
                 (ok-outcome (string-join (reverse matches) "\n")
                             #:display f"grep {pat-s}: {(length matches)} matches"
                 ) ; end ok-outcome
             ) ; end if
           ) ; end parameterize
          ] ; end else
        ) ; end cond
       ] ; end else
     ) ; end cond
   ) ; end define tool-run
  ] ; end methods
) ; end struct grep-tool

(define (make-glob-tool) (glob-tool))
(define (make-grep-tool) (grep-tool))

(provide
 make-glob-tool
 make-grep-tool
) ; end provide
