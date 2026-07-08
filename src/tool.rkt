#lang tstring racket
;; tool.rkt — 工具协议与注册表（design.md §4.3）

(require
 racket/generic
 racket/string
) ; end require

;; ---------------------------------------------------------------- 协议

(define-generics tool
  [tool-name tool]              ; -> string
  [tool-spec tool]              ; -> jsexpr (OpenAI function 格式)
  [tool-permission-level tool]  ; -> 'read-only | 'mutating | 'dangerous
  [tool-run tool input ctx]     ; input:jsexpr ctx:tool-ctx -> tool-outcome
) ; end define-generics tool

(struct tool-outcome
  (content     ; string — 回填给模型的内容
   is-error?   ; boolean
   display     ; (or/c #f string) — 给用户看的摘要（#f = 用 content）
  ) ; end fields
  #:transparent
) ; end struct tool-outcome

(define (ok-outcome content #:display [display #f])
  (tool-outcome content #f display)
) ; end define ok-outcome

(define (err-outcome content #:display [display #f])
  (tool-outcome content #t display)
) ; end define err-outcome

(struct tool-ctx
  (workdir     ; path-string — 工具的工作目录
   publish!    ; (-> evt any) — 事件发布（工具可报进度）
   config      ; config
  ) ; end fields
) ; end struct tool-ctx

;; 便捷：构造 OpenAI function spec
(define (function-spec name description params required)
  (hasheq 'type "function"
          'function
          (hasheq 'name name
                  'description description
                  'parameters
                  (hasheq 'type "object"
                          'properties params
                          'required required
                  ) ; end hasheq parameters
          ) ; end hasheq function
  ) ; end hasheq
) ; end define function-spec

;; 参数读取：jsexpr input 中取 string / int 字段
(define (input-ref input key [dflt #f])
  (define v (if (hash? input) (hash-ref input key dflt) dflt))
  (if (eq? v 'null) dflt v)
) ; end define input-ref

(define (input-str input key [dflt #f])
  (define v (input-ref input key dflt))
  (if (string? v) v dflt)
) ; end define input-str

(define (input-int input key [dflt #f])
  (define v (input-ref input key dflt))
  (if (exact-integer? v) v dflt)
) ; end define input-int

;; ---------------------------------------------------------------- 注册表

(struct registry
  (table   ; immutable hash: name -> tool
  ) ; end fields
) ; end struct registry

(define (make-registry tools)
  (registry
   (for/hash ([t (in-list tools)])
     (values (tool-name t) t)
   ) ; end for/hash
  ) ; end registry
) ; end define make-registry

(define (registry-lookup reg name)
  (hash-ref (registry-table reg) name #f)
) ; end define registry-lookup

(define (registry-specs reg)
  (for/list ([t (in-hash-values (registry-table reg))])
    (tool-spec t)
  ) ; end for/list
) ; end define registry-specs

(define (registry-tools reg)
  (hash-values (registry-table reg))
) ; end define registry-tools

;; ---------------------------------------------------------------- provide

(provide
 gen:tool
 tool?
 tool-name
 tool-spec
 tool-permission-level
 tool-run
 (struct-out tool-outcome)
 ok-outcome
 err-outcome
 (struct-out tool-ctx)
 function-spec
 input-ref
 input-str
 input-int
 registry?
 make-registry
 registry-lookup
 registry-specs
 registry-tools
) ; end provide
