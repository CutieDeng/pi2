#lang racket/base
;; editor.rkt — 输入框编辑器模型（G2，纯 Racket，与渲染 / 平台无关）。
;;
;; 文本是一个字符串；每个折叠 chip 在其中占一个 U+FFFC，chips 按出现顺序记录 fold id。
;; 光标、选区、撤销都在这个字符串上做，chip 天然是原子单元。
;; IME 组字（marked）不写进 text，显示时插在光标处；提交（insertText）时才写入。
;; 编辑命令名沿用 AppKit 的 selector（moveLeft: / deleteBackward: …），平台层原样转交。

(require racket/list racket/string "fold.rkt" "linebreak.rkt")

;; ---------------------------------------------------------------- 状态

;; 不可变快照：撤销栈存它
(struct snap (text chips cursor anchor) #:transparent)

(struct editor ([text #:mutable] [chips #:mutable] [cursor #:mutable] [anchor #:mutable]
                [marked #:mutable]                 ; #f | (cons str sel)，sel 为组字串内的码点下标
                [undo #:mutable] [redo #:mutable] [last-op #:mutable]
                folds                              ; id → fold（会话内不删除，撤销可恢复）
                [next-id #:mutable]
                [history #:mutable] [hist-idx #:mutable] [stash #:mutable]
                [expanded #:mutable]))             ; #f | 正在预览的 fold id

(define (make-editor)
  (editor "" '() 0 0 #f '() '() #f (make-hasheqv) 1 '() #f #f #f)
) ; end define make-editor

(define UNDO-MAX 200)

(define (snapshot e) (snap (editor-text e) (editor-chips e) (editor-cursor e) (editor-anchor e)))

(define (restore! e s)
  (set-editor-text! e (snap-text s))
  (set-editor-chips! e (snap-chips s))
  (set-editor-cursor! e (snap-cursor s))
  (set-editor-anchor! e (snap-anchor s))
) ; end define restore!

;; 记一个撤销点。连续同类操作（打字、删除）合并为一步。
(define (checkpoint! e op)
  (unless (and op (eq? op (editor-last-op e)))
    (set-editor-undo! e (take (cons (snapshot e) (editor-undo e)) (min UNDO-MAX (add1 (length (editor-undo e)))))))
  (set-editor-redo! e '())
  (set-editor-last-op! e op)
) ; end define checkpoint!

(define (break-coalescing! e) (set-editor-last-op! e #f))

;; ---------------------------------------------------------------- 查询

(define (editor-selection e)                       ; → (values start end)
  (values (min (editor-cursor e) (editor-anchor e)) (max (editor-cursor e) (editor-anchor e)))
) ; end define editor-selection

(define (has-selection? e) (not (= (editor-cursor e) (editor-anchor e))))

(define (editor-empty? e) (and (string=? (editor-text e) "") (not (editor-marked e))))

(define (chip-ordinal text i)                      ; text[i] 是第几个 chip（0 起）
  (for/sum ([c (in-string text 0 i)]) (if (char=? c OBJ) 1 0))
) ; end define chip-ordinal

(define (editor-fold e id) (hash-ref (editor-folds e) id))

;; 第 k 个 chip（0 起）在 text 中的下标
(define (chip-position text k)
  (let loop ([i 0] [seen 0])
    (cond [(>= i (string-length text)) #f]
          [(char=? (string-ref text i) OBJ) (if (= seen k) i (loop (add1 i) (add1 seen)))]
          [else (loop (add1 i) seen)]))
) ; end define chip-position

;; 区间内的 chip 展开成原文
(define (flatten-range e a b)
  (define text (editor-text e))
  (define-values (out _k)
    (for/fold ([out '()] [k (chip-ordinal text a)]) ([c (in-string text a b)])
      (if (char=? c OBJ)
          (values (cons (fold-send-text (editor-fold e (list-ref (editor-chips e) k))) out) (add1 k))   ; 密钥 → 占位符
          (values (cons (string c) out) k))))
  (apply string-append (reverse out))
) ; end define flatten-range

(define (editor-flatten e) (flatten-range e 0 (string-length (editor-text e))))

;; 显示用：组字串插在光标处。→ (values 显示串 光标显示下标 组字区间或#f)
(define (editor-display e)
  (define text (editor-text e))
  (define c (editor-cursor e))
  (define m (editor-marked e))
  (if m
      (values (string-append (substring text 0 c) (car m) (substring text c))
              (+ c (cdr m))
              (cons c (+ c (string-length (car m)))))
      (values text c #f))
) ; end define editor-display

;; ---------------------------------------------------------------- 基本编辑

;; 删除 [a,b)，同步 chips 列表
(define (delete-range! e a b)
  (when (< a b)
    (define text (editor-text e))
    (define k0 (chip-ordinal text a))
    (define n (chip-ordinal (substring text a b) (- b a)))
    (define chips (editor-chips e))
    (set-editor-chips! e (append (take chips k0) (drop chips (+ k0 n))))
    (set-editor-text! e (string-append (substring text 0 a) (substring text b)))
    (set-editor-cursor! e a)
    (set-editor-anchor! e a))
) ; end define delete-range!

;; 在光标处插入（先删选区）。ids：插入串中各 U+FFFC 对应的 fold id。
(define (insert-at-cursor! e s [ids '()])
  (define-values (a b) (editor-selection e))
  (delete-range! e a b)
  (define text (editor-text e))
  (define k (chip-ordinal text a))
  (define chips (editor-chips e))
  (set-editor-chips! e (append (take chips k) ids (drop chips k)))
  (set-editor-text! e (string-append (substring text 0 a) s (substring text a)))
  (set-editor-cursor! e (+ a (string-length s)))
  (set-editor-anchor! e (editor-cursor e))
) ; end define insert-at-cursor!

;; 键入 / IME 提交的文本
(define (editor-insert! e s0)
  (define s (normalize-paste s0))
  (set-editor-marked! e #f)
  (when (> (string-length s) 0)
    (checkpoint! e (if (has-selection? e) #f 'type))
    (insert-at-cursor! e s))
) ; end define editor-insert!

;; 粘贴：达到阈值 → 建 fold、插一个 chip；否则原样插入。返回新建的 fold 或 #f。
(define (editor-paste! e s0)
  (define s (normalize-paste s0))
  (set-editor-marked! e #f)
  (cond
    [(string=? s "") #f]
    [(fold-worthy? s)
     (define f (make-fold (editor-next-id e) s))
     (hash-set! (editor-folds e) (fold-id f) f)
     (set-editor-next-id! e (add1 (editor-next-id e)))
     (checkpoint! e #f)
     (insert-at-cursor! e OBJ-STR (list (fold-id f)))
     (break-coalescing! e)
     f]
    [else (checkpoint! e #f) (insert-at-cursor! e s) (break-coalescing! e) #f])
) ; end define editor-paste!

;; 附件 chip（图片 / 音频 / 文件）→ fold；附件本身由调用方按 fold id 记着
(define (editor-attach! e kind name size)
  (set-editor-marked! e #f)
  (define f (make-media-fold (editor-next-id e) kind name size))
  (hash-set! (editor-folds e) (fold-id f) f)
  (set-editor-next-id! e (add1 (editor-next-id e)))
  (checkpoint! e #f)
  (insert-at-cursor! e OBJ-STR (list (fold-id f)))
  (break-coalescing! e)
  f
) ; end define editor-attach!

;; 以密钥 chip 粘贴（pending?：像密钥、等用户选）→ fold
(define (editor-paste-secret! e value pending?)
  (set-editor-marked! e #f)
  (define f (make-secret-fold (editor-next-id e) value pending?))
  (hash-set! (editor-folds e) (fold-id f) f)
  (set-editor-next-id! e (add1 (editor-next-id e)))
  (checkpoint! e #f)
  (insert-at-cursor! e OBJ-STR (list (fold-id f)))
  (break-coalescing! e)
  f
) ; end define editor-paste-secret!

;; 选中的文字 → 密钥 chip（已确认）：选区为空 / 只有空白 / 含 chip → #f；否则 → (cons fold 原文)。可撤销（撤销后原文回到输入框）
(define (editor-secret-selection! e)
  (define-values (a b) (editor-selection e))
  (define v (and (< a b) (substring (editor-text e) a b)))
  (cond
    [(or (not v) (string=? (string-trim v) "") (for/or ([c (in-string v)]) (char=? c (string-ref OBJ-STR 0)))) #f]
    [else (cons (editor-paste-secret! e (string-trim v) #f) v)])
) ; end define editor-secret-selection!

;; 「作为密钥」：待选 → 确认
(define (editor-confirm-secret! e id)
  (define f (editor-fold e id))
  (when (and f (eq? (fold-kind f) 'secret-pending))
    (hash-set! (editor-folds e) id (struct-copy fold f [kind 'secret]))
    (when (eqv? (editor-expanded e) id) (set-editor-expanded! e #f)))
) ; end define editor-confirm-secret!

;; chip → 原文（可编辑），可撤销
(define (editor-unfold! e id)
  (define k (index-of (editor-chips e) id))
  (when k
    (define pos (chip-position (editor-text e) k))
    (checkpoint! e #f)
    (set-editor-cursor! e pos)
    (set-editor-anchor! e (add1 pos))
    (insert-at-cursor! e (fold-content (editor-fold e id)))
    (when (eqv? (editor-expanded e) id) (set-editor-expanded! e #f))
    (break-coalescing! e))
) ; end define editor-unfold!

(define (editor-remove-chip! e id)
  (define k (index-of (editor-chips e) id))
  (when k
    (define pos (chip-position (editor-text e) k))
    (checkpoint! e #f)
    (delete-range! e pos (add1 pos))
    (when (eqv? (editor-expanded e) id) (set-editor-expanded! e #f))
    (break-coalescing! e))
) ; end define editor-remove-chip!

(define (editor-toggle-expand! e id)
  (set-editor-expanded! e (if (eqv? (editor-expanded e) id) #f id))
) ; end define editor-toggle-expand!

;; ---------------------------------------------------------------- IME

(define (editor-set-marked! e str sel)
  (cond
    [(string=? str "") (set-editor-marked! e #f)]
    [else
     (when (and (not (editor-marked e)) (has-selection? e))   ; 开始组字时替换选区
       (checkpoint! e #f)
       (define-values (a b) (editor-selection e))
       (delete-range! e a b))
     (set-editor-marked! e (cons (normalize-paste str) (max 0 (min sel (string-length str)))))])
) ; end define editor-set-marked!

(define (editor-unmark! e) (set-editor-marked! e #f))

;; ---------------------------------------------------------------- 移动

(define (grapheme-before text i)
  (define starts (grapheme-starts text))
  (let loop ([j (sub1 i)]) (if (or (<= j 0) (vector-ref starts j)) (max j 0) (loop (sub1 j))))
) ; end define grapheme-before

(define (grapheme-after text i)
  (if (>= i (string-length text)) i (+ i (max 1 (string-grapheme-span text i))))
) ; end define grapheme-after

;; 词类：空白 / 标点 / 拉丁词 / 表意（每字一词）/ chip（一个词）
(define (char-class c)
  (cond [(char=? c OBJ) 'chip]
        [(char-whitespace? c) 'space]
        [(ideographic? c) 'ideo]
        [(or (char-alphabetic? c) (char-numeric? c) (char=? c #\_)) 'word]
        [else 'punct])
) ; end define char-class

(define (word-left text i)
  (let* ([i (let skip ([i i]) (if (and (> i 0) (memq (char-class (string-ref text (sub1 i))) '(space punct))) (skip (sub1 i)) i))])
    (cond
      [(= i 0) 0]
      [(memq (char-class (string-ref text (sub1 i))) '(ideo chip)) (sub1 i)]
      [else (let run ([i i]) (if (and (> i 0) (eq? (char-class (string-ref text (sub1 i))) 'word)) (run (sub1 i)) i))]))
) ; end define word-left

(define (word-right text i)
  (define n (string-length text))
  (let* ([i (let skip ([i i]) (if (and (< i n) (memq (char-class (string-ref text i)) '(space punct))) (skip (add1 i)) i))])
    (cond
      [(= i n) n]
      [(memq (char-class (string-ref text i)) '(ideo chip)) (add1 i)]
      [else (let run ([i i]) (if (and (< i n) (eq? (char-class (string-ref text i)) 'word)) (run (add1 i)) i))]))
) ; end define word-right

(define (text-line-start text i)
  (let loop ([j i]) (if (and (> j 0) (not (char=? (string-ref text (sub1 j)) #\newline))) (loop (sub1 j)) j))
) ; end define text-line-start

(define (text-line-end text i)
  (define n (string-length text))
  (let loop ([j i]) (if (and (< j n) (not (char=? (string-ref text j) #\newline))) (loop (add1 j)) j))
) ; end define text-line-end

;; 把光标移到 i；extend? → 保留 anchor（选区）
(define (editor-move-to! e i #:extend? [extend? #f])
  (define n (string-length (editor-text e)))
  (set-editor-cursor! e (max 0 (min n i)))
  (unless extend? (set-editor-anchor! e (editor-cursor e)))
  (break-coalescing! e)
) ; end define editor-move-to!

;; ---------------------------------------------------------------- 历史

(define (editor-load! e s)
  (set-editor-text! e (snap-text s))
  (set-editor-chips! e (snap-chips s))
  (set-editor-cursor! e (string-length (snap-text s)))
  (set-editor-anchor! e (editor-cursor e))
  (set-editor-marked! e #f)
  (break-coalescing! e)
) ; end define editor-load!

;; 草稿（一窗多主机：每台主机一份输入框内容）：文字、chip、光标 / 选区、撤销栈、展开中的 chip。组字中的内容不保留。
(define (editor-draft e)
  (vector (snapshot e) (editor-undo e) (editor-redo e) (editor-expanded e))
) ; end define editor-draft

(define (editor-load-draft! e d)
  (restore! e (vector-ref d 0))
  (set-editor-undo! e (vector-ref d 1))
  (set-editor-redo! e (vector-ref d 2))
  (set-editor-expanded! e (vector-ref d 3))
  (set-editor-marked! e #f)
  (set-editor-hist-idx! e #f)
  (break-coalescing! e)
) ; end define editor-load-draft!

(define EMPTY-DRAFT (vector (snap "" '() 0 0) '() '() #f))

;; 上一条 / 下一条历史（chip 原样恢复：fold 注册表在会话内不删除）
(define (editor-history-prev! e)
  (define h (editor-history e))
  (define idx (editor-hist-idx e))
  (define next (if idx (add1 idx) 0))
  (when (< next (length h))
    (unless idx (set-editor-stash! e (snapshot e)))
    (set-editor-hist-idx! e next)
    (editor-load! e (list-ref h next)))
) ; end define editor-history-prev!

(define (editor-history-next! e)
  (define idx (editor-hist-idx e))
  (when idx
    (cond
      [(= idx 0) (set-editor-hist-idx! e #f) (editor-load! e (editor-stash e))]
      [else (set-editor-hist-idx! e (sub1 idx)) (editor-load! e (list-ref (editor-history e) (sub1 idx)))]))
) ; end define editor-history-next!

;; 提交：返回 (cons 展开后的全文 快照) 或 #f（空）。清空编辑器，快照进历史。
(define (editor-submit! e)
  (cond
    [(or (editor-marked e) (string=? (string-trim (editor-text e)) "")) #f]
    [else
     (define out (editor-flatten e))
     (define s (snap (editor-text e) (editor-chips e) 0 0))
     (set-editor-history! e (cons s (editor-history e)))
     (set-editor-hist-idx! e #f)
     (set-editor-text! e "")
     (set-editor-chips! e '())
     (set-editor-cursor! e 0)
     (set-editor-anchor! e 0)
     (set-editor-undo! e '())
     (set-editor-redo! e '())
     (set-editor-expanded! e #f)
     (break-coalescing! e)
     (cons out s)])
) ; end define editor-submit!

;; ---------------------------------------------------------------- 命令分派

;; cmd：AppKit selector 名（符号）。返回：'submit（insertNewline:，交给调用方决定）、
;; (cons 'copy 文本)（copy: / cut:，交给调用方写剪贴板）、'handled、'ignored。
;; 上下移动（moveUp: / moveDown:）需要排版信息，由视图层处理。
(define (editor-command! e cmd)
  (define text (editor-text e))
  (define c (editor-cursor e))
  (define-values (a b) (editor-selection e))
  (define (move! i [extend? #f]) (editor-move-to! e i #:extend? extend?) 'handled)
  (define (del! from to op)
    (when (< from to) (checkpoint! e op) (delete-range! e from to))
    'handled)
  (define (del-or-sel! from to op) (if (has-selection? e) (del! a b #f) (del! from to op)))
  (case cmd
    [(insertNewline:) 'submit]
    [(insertNewlineIgnoringFieldEditor: insertLineBreak:) (editor-insert! e "\n") (break-coalescing! e) 'handled]
    [(insertTab:) (editor-insert! e "\t") 'handled]
    [(deleteBackward: deleteBackwardByDecomposingPreviousCharacter:) (del-or-sel! (grapheme-before text c) c 'delete)]
    [(deleteForward:) (del-or-sel! c (grapheme-after text c) 'delete)]
    [(deleteWordBackward:) (del-or-sel! (word-left text c) c #f)]
    [(deleteWordForward:) (del-or-sel! c (word-right text c) #f)]
    [(deleteToBeginningOfLine:) (del-or-sel! (text-line-start text c) c #f)]
    [(deleteToEndOfLine: deleteToEndOfParagraph:)
     (del-or-sel! c (let ([le (text-line-end text c)]) (if (= le c) (min (string-length text) (add1 c)) le)) #f)]
    [(moveLeft:) (move! (if (has-selection? e) a (grapheme-before text c)))]
    [(moveRight:) (move! (if (has-selection? e) b (grapheme-after text c)))]
    [(moveLeftAndModifySelection:) (move! (grapheme-before text c) #t)]
    [(moveRightAndModifySelection:) (move! (grapheme-after text c) #t)]
    [(moveWordLeft:) (move! (word-left text c))]
    [(moveWordRight:) (move! (word-right text c))]
    [(moveWordLeftAndModifySelection:) (move! (word-left text c) #t)]
    [(moveWordRightAndModifySelection:) (move! (word-right text c) #t)]
    [(moveToBeginningOfLine: moveToLeftEndOfLine: moveToBeginningOfParagraph:) (move! (text-line-start text c))]
    [(moveToEndOfLine: moveToRightEndOfLine: moveToEndOfParagraph:) (move! (text-line-end text c))]
    [(moveToBeginningOfLineAndModifySelection: moveToLeftEndOfLineAndModifySelection:) (move! (text-line-start text c) #t)]
    [(moveToEndOfLineAndModifySelection: moveToRightEndOfLineAndModifySelection:) (move! (text-line-end text c) #t)]
    [(moveToBeginningOfDocument:) (move! 0)]
    [(moveToEndOfDocument:) (move! (string-length text))]
    [(moveToBeginningOfDocumentAndModifySelection:) (move! 0 #t)]
    [(moveToEndOfDocumentAndModifySelection:) (move! (string-length text) #t)]
    [(selectAll:) (set-editor-anchor! e 0) (set-editor-cursor! e (string-length text)) (break-coalescing! e) 'handled]
    [(copy:) (if (has-selection? e) (cons 'copy (flatten-range e a b)) 'handled)]
    [(cut:)
     (cond [(has-selection? e) (define out (flatten-range e a b)) (del! a b #f) (cons 'copy out)]
           [else 'handled])]
    [(undo:)
     (cond [(pair? (editor-undo e))
            (set-editor-redo! e (cons (snapshot e) (editor-redo e)))
            (restore! e (car (editor-undo e)))
            (set-editor-undo! e (cdr (editor-undo e)))
            (break-coalescing! e)
            'handled]
           [else 'ignored])]
    [(redo:)
     (cond [(pair? (editor-redo e))
            (set-editor-undo! e (cons (snapshot e) (editor-undo e)))
            (restore! e (car (editor-redo e)))
            (set-editor-redo! e (cdr (editor-redo e)))
            (break-coalescing! e)
            'handled]
           [else 'ignored])]
    [(cancelOperation:)
     (cond [(editor-expanded e) (set-editor-expanded! e #f) 'handled]
           [(has-selection? e) (set-editor-anchor! e c) 'handled]
           [else 'ignored])]
    [else 'ignored])
) ; end define editor-command!

(provide (struct-out snap)
         make-editor editor? editor-text editor-chips editor-cursor editor-anchor editor-marked
         editor-history editor-expanded editor-fold editor-folds chip-position chip-ordinal
         editor-selection has-selection? editor-empty? editor-flatten editor-display
         editor-insert! editor-paste! editor-unfold! editor-remove-chip! editor-toggle-expand!
         editor-paste-secret! editor-confirm-secret! editor-secret-selection! editor-attach!
         editor-set-marked! editor-unmark! editor-move-to! editor-command!
         editor-history-prev! editor-history-next! editor-submit!
         editor-draft editor-load-draft! EMPTY-DRAFT
         text-line-start text-line-end grapheme-before grapheme-after word-left word-right)
