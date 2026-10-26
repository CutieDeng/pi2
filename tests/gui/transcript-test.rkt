#lang racket/base
;; tests/gui/transcript-test.rkt — 对话区组件（只实例化这一个组件，text-layout 用假实现）：
;; 条目（RPC v3 item）→ 显示条目、权限应答、工具折叠与全文、中断、滚动贴底、实时 = 重建、翻页与逐出、多主机文档

(require rackunit racket/list racket/string net/base64
         "../../src/gui/compute/drawlist.rkt" "../../src/gui/compute/interfaces.rkt"
         "../../src/gui/components/transcript.rkt" "../../src/gui/compute/i18n.rkt" "../../src/gui/compute/paint.rkt" "fakes.rkt")

;; 单个组件 + 假 text-layout（组件只认识接口）
(define (new-tr) (instantiate-with transcript-component transcript-port))

(define (render tr [h 400.0])
  (define dl (make-drawlist))
  (tr 'render! dl 0.0 0.0 600.0 h 2.0)
  dl)

;; 条目构造（与 src/items.rkt 的投影同形）
(define (it id kind . kvs) (apply hasheq 'type "item" 'id id 'kind kind kvs))
(define (txt s) (hasheq 'type "text" 'text s))
(define (think s) (hasheq 'type "thinking" 'text s))
(define (call id name input) (hasheq 'type "tool_call" 'id id 'name name 'input input))
(define (res id content #:error? [err? #f] #:total [total #f] #:truncated? [trunc? #f])
  (hasheq 'type "tool_result" 'call_id id 'is_error err? 'content content 'total_len (or total (string-length content)) 'truncated trunc?))
(define (user id s) (it id "user" 'parts (list (txt s)) 'synthetic #f))
(define (asst id . parts) (it id "assistant" 'parts parts))
(define (results id . parts) (it id "results" 'parts parts))

(define TOOL-TURN
  (list (hasheq 'type "turn_start" 'kind "prompt")
        (user 1 "hi")
        (hasheq 'type "delta" 'kind "text" 'text "我先")
        (hasheq 'type "delta" 'kind "text" 'text "跑一下")
        (asst 2 (txt "我先跑一下") (call "t1" "bash" (hasheq 'command "ls")))
        (it 3 "usage" 'model "m" 'input 5 'output 3 'cost 0.0)
        (it 4 "permission" 'perm_id 3 'tool "bash" 'level "dangerous" 'input (hasheq 'command "ls") 'prompt "allow?")
        (it 5 "answer" 'perm_id 3 'decision "yes")
        (it 6 "tool" 'call_id "t1" 'name "bash" 'ms 12.7 'error #f 'source "builtin" 'display 'null 'diff 'null)
        (results 7 (res "t1" "a\nb\nc\nd\ne\nf\ng\nh\ni\nj\nk\nl"))
        (hasheq 'type "delta" 'kind "text" 'text "看完了")
        (asst 8 (txt "看完了"))
        (it 9 "usage" 'model "m" 'input 5 'output 3 'cost 0.0)
        (hasheq 'type "turn_complete" 'turn 1 'interrupted #f 'outcome "ok")))

(define (session-ev items #:total [total #f] #:from [from 0] #:turns [turns 0])
  (hasheq 'type "session" 'path "/d/s.rktd" 'messages 0 'total (or total (add1 (apply max 0 (map (lambda (i) (hash-ref i 'id)) items))))
          'tail_from from 'turns turns 'items items))

(test-case "a tool turn: the sent message is taken over by its record (one user entry); tool card in the assistant step; usage lines not in the summary"
  (define tr (new-tr))
  (tr 'add-user! "hi" '())
  (for ([ev TOOL-TURN]) (tr 'apply! ev))
  (check-equal? (tr 'summary) '(user assistant tool:bash/ok perm/yes assistant))
  (check-false (tr 'busy?))
  (check-equal? (tr 'turns) 1)
  (check-equal? (tr 'last-assistant-text) "看完了"))

(test-case "live = rebuilt: the same records fed live and as a session tail give the same entries"
  (define live (new-tr))
  (live 'add-user! "hi" '())
  (for ([ev TOOL-TURN]) (live 'apply! ev))
  (define rebuilt (new-tr))
  (rebuilt 'apply! (session-ev (filter (lambda (e) (equal? (hash-ref e 'type) "item")) TOOL-TURN) #:turns 1))
  (check-equal? (drop-right (rebuilt 'summary) 1) (live 'summary))           ; 重建的末尾多一条「会话 …」提示
  (check-equal? (rebuilt 'turns) 1)
  ;; 选出来的文字也一样（用户消息、回答、卡片标题）
  (render live 2000.0) (render rebuilt 2000.0)
  (for ([t (list live rebuilt)]) (t 'select-start! 0.0 0.0) (t 'select-extend! 599.0 1990.0) (render t 2000.0))
  (define (strip s) (string-trim (regexp-replace #rx"\n\n[^\n]*s\\.rktd[^\n]*$" s "")))
  (check-equal? (strip (rebuilt 'selection-text)) (strip (live 'selection-text))))

(test-case "pure tool-call step (no text) leaves no empty assistant entry"
  (define tr (new-tr))
  (for ([ev (list (hasheq 'type "turn_start") (asst 1 (call "t" "read_file" (hasheq 'path "x"))))]) (tr 'apply! ev))
  (check-equal? (tr 'summary) '(tool:read_file/running)))

(test-case "permission buttons hit-test and produce the protocol request once"
  (define tr (new-tr))
  (for ([ev (take TOOL-TURN 7)]) (tr 'apply! ev))         ; 停在权限询问
  (render tr)
  (check-equal? (tr 'pending-permissions) '(3))
  (define pt (tr 'hit-center 'perm (cons 3 "yes")))
  (check-true (pair? pt))
  (check-equal? (tr 'hit (car pt) (cdr pt)) (list 'perm 3 "yes"))
  (check-equal? (tr 'answer! 3 "yes") (hasheq 'type "permission" 'id 3 'decision "yes"))
  (check-false (tr 'answer! 3 "no"))                      ; 已答过
  (render tr)
  (check-false (tr 'hit-center 'perm (cons 3 "yes"))))    ; 按钮消失

(define SEQ30 (apply string-append (for/list ([i 30]) (format "line~a\n" i))))

(test-case "tool card: collapsed preview (5 lines by default) / expanded; preview modes head / tail / head_tail per tool"
  (define tr (new-tr))
  (for ([ev (list (asst 1 (call "t1" "bash" (hasheq 'command "seq 30"))) (results 2 (res "t1" SEQ30)))]) (tr 'apply! ev))
  (define (shown)
    (render tr 2000.0)
    (tr 'select-start! 0.0 0.0) (tr 'select-extend! 599.0 1990.0) (render tr 2000.0)
    (begin0 (tr 'selection-text) (tr 'clear-selection!)))
  (define s0 (shown))
  (check-true (string-contains? s0 "line4"))
  (check-false (string-contains? s0 "line5"))
  (check-true (string-contains? s0 "25"))                         ; 「还有 25 行」
  (tr 'toggle-tool! "t1")
  (check-true (string-contains? (shown) "line29"))
  (tr 'toggle-tool! "t1")
  (tr 'set-preview! (hasheq 'lines 4 'mode "head" 'tools (hasheq 'bash (hasheq 'mode "tail" 'lines 3))))
  (define s1 (shown))
  (check-true (string-contains? s1 "line29"))
  (check-false (string-contains? s1 "line26"))
  (tr 'set-preview! (hasheq 'lines 4 'mode "head_tail"))
  (define s2 (shown))
  (check-true (and (string-contains? s2 "line0") (string-contains? s2 "line1") (string-contains? s2 "line28") (string-contains? s2 "line29")))
  (check-false (string-contains? s2 "line2\n")))

(test-case "truncated tool output: expanding asks for the full record once; item_full fills the card"
  (define tr (new-tr))
  (tr 'apply! (session-ev (list (asst 1 (call "t1" "bash" (hasheq 'command "big"))) (results 2 (res "t1" "head-part" #:total 50000 #:truncated? #t)))))
  (void (tr 'wanted))
  (tr 'toggle-tool! "t1")
  (define rq (tr 'wanted))
  (check-equal? (map (lambda (r) (list (hash-ref r 'type) (hash-ref r 'id))) rq) '(("item" 2)))
  (tr 'toggle-tool! "t1") (tr 'toggle-tool! "t1")
  (check-equal? (tr 'wanted) '())                                 ; 取全文中：不重复要
  (tr 'apply! (hasheq 'type "item_full" 'id 2 'kind "results" 'tag (hash-ref (car rq) 'tag) 'parts (list (res "t1" "head-part...the whole thing"))))
  (render tr 2000.0)
  (tr 'select-start! 0.0 0.0) (tr 'select-extend! 599.0 1990.0) (render tr 2000.0)
  (check-true (string-contains? (tr 'selection-text) "the whole thing")))

(test-case "interrupt: half-streamed text comes back as a dim partial entry; running tool aborted; marker note; pending permission void"
  (define tr (new-tr))
  (tr 'add-user! "go" '())
  (for ([ev (list (hasheq 'type "turn_start") (user 1 "go")
                  (asst 2 (call "a" "bash" (hasheq 'command "echo 1")))
                  (results 3 (res "a" "1"))
                  (asst 4 (txt "step one done, now") (call "b" "bash" (hasheq 'command "sleep 5")))
                  (it 5 "permission" 'perm_id 9 'tool "bash" 'level "dangerous" 'input (hasheq 'command "sleep 5") 'prompt "?")
                  (hasheq 'type "delta" 'kind "text" 'text "half an ans")
                  (it 6 "partial" 'text "half an ans" 'reasoning "")
                  (it 7 "user" 'parts (list (res "b" "aborted by user after 1.2s" #:error? #t)) 'synthetic #f)
                  (it 8 "user" 'parts (list (txt "<turn_aborted>\nx\n</turn_aborted>")) 'synthetic "aborted")
                  (hasheq 'type "turn_complete" 'interrupted #t 'outcome "interrupted"))])
    (tr 'apply! ev))
  (check-equal? (tr 'summary) '(user tool:bash/ok assistant tool:bash/aborted perm/cancelled assistant/interrupted note))
  (check-equal? (tr 'pending-permissions) '())
  (check-false (tr 'answer! 9 "yes")))

(test-case "stick to bottom while new content arrives; scrolling up releases it and new deltas don't pull it back"
  (define tr (new-tr))
  (for ([i 40]) (tr 'apply! (user (add1 i) (format "message ~a" i))))
  (render tr 200.0)
  (define st0 (tr 'stats))
  (check-true (> (hash-ref st0 'scroll) 0.0))
  (check-true (>= (hash-ref st0 'scroll) (- (hash-ref st0 'content_h) 200.0 1.0)))
  (tr 'scroll! 300.0)                                   ; 手指下滑：往上看
  (render tr 200.0)
  (define sc1 (hash-ref (tr 'stats) 'scroll))
  (tr 'apply! (hasheq 'type "delta" 'kind "text" 'text "streaming"))
  (render tr 200.0)
  (check-= (hash-ref (tr 'stats) 'scroll) sc1 0.5)
  (tr 'add-user! "more" '())                            ; 用户发了消息：重新贴底
  (render tr 200.0)
  (define st2 (tr 'stats))
  (check-= (hash-ref st2 'scroll) (- (hash-ref st2 'content_h) 200.0) 0.5))

(test-case "session event: turns from the kernel; synthetic messages are notes; another client's prompt shows as a user entry"
  (define tr (new-tr))
  (tr 'apply! (session-ev (list (user 1 "now hang")
                                (it 2 "user" 'parts (list (txt "<turn_aborted>\nx\n</turn_aborted>")) 'synthetic "aborted")
                                (it 3 "compact" 'count 2 'summary "S")
                                (it 4 "note" 'level "warn" 'code "route_switch" 'text "@team: a/m1 → b/m2"
                                    'data (hasheq 'route "team" 'from (hasheq 'instance "a" 'model "m1") 'to (hasheq 'instance "b" 'model "m2") 'reason "rate limit")))
                          #:turns 1))
  (check-equal? (tr 'summary) '(user note note note note))
  (check-equal? (tr 'turns) 1)
  (tr 'apply! (user 5 "from another client"))           ; 没有本地待接管的：按记录显示
  (check-equal? (take-right (tr 'summary) 2) '(note user)))

(test-case "delta kinds: thinking → reasoning, tool-json ignored (no stray assistant entry)"
  (define tr (new-tr))
  (check-false (tr 'apply! (hasheq 'type "delta" 'kind "tool-json" 'text "{\"command\":")))
  (check-equal? (tr 'summary) '())
  (tr 'apply! (hasheq 'type "delta" 'kind "thinking" 'text "hmm"))
  (tr 'apply! (hasheq 'type "delta" 'kind "text" 'text "answer"))
  (check-equal? (tr 'summary) '(assistant))
  (check-equal? (tr 'last-assistant-text) "answer")
  (tr 'apply! (asst 1 (think "hmm") (txt "answer.")))  ; 定稿：流式条目作废，只剩记录的
  (check-equal? (tr 'summary) '(assistant))
  (check-equal? (tr 'last-assistant-text) "answer."))

;; ---------------------------------------------------------------- Markdown 回答（G4）

(define LONG-ANSWER
  (string-append "Here is the fix, it is long enough to deserve a copy link:\n\n```sh\n"
                 (apply string-append (for/list ([i 40]) (format "echo ~a\n" i))) "```"))

(test-case "assistant answers render as Markdown: copy (code / whole message) and long-code toggle are hit targets"
  (define tr (new-tr))
  (for ([ev (list (hasheq 'type "turn_start") (asst 1 (txt LONG-ANSWER))
                  (hasheq 'type "turn_complete" 'turn 1 'interrupted #f 'outcome "ok"))])
    (tr 'apply! ev))
  (render tr 3000.0)
  (define code-copy (tr 'hit-center 'copy (apply string-append (for/list ([i 40]) (if (= i 39) (format "echo ~a" i) (format "echo ~a\n" i))))))
  (check-true (pair? code-copy))
  (check-equal? (car (tr 'hit (car code-copy) (cdr code-copy))) 'copy)
  (define msg-copy (tr 'hit-center 'copy LONG-ANSWER))
  (check-true (pair? msg-copy))
  ;; 长代码块折叠键：点一下展开，内容变高
  (define n0 (drawlist-count (render tr 3000.0)))
  (define toggle (for/or ([y (in-range 0 3000 2)])
                   (define hh (tr 'hit 300.0 (exact->inexact y)))
                   (and hh (eq? (car hh) 'code-toggle) hh)))
  (check-not-false toggle)
  (tr 'toggle-code! (cadr toggle))
  (check-true (> (drawlist-count (render tr 3000.0)) n0)))

(test-case "scrolled up: a 'back to bottom' pill appears and re-sticks"
  (define tr (new-tr))
  (for ([i 60]) (tr 'add-user! (format "message ~a" i) '()))
  (render tr 200.0)
  (check-false (tr 'hit-center 'to-bottom #t))                   ; 在底部：没有
  (tr 'scroll! 500.0)
  (render tr 200.0)
  (define pt (tr 'hit-center 'to-bottom #t))
  (check-true (pair? pt))
  (check-equal? (tr 'hit (car pt) (cdr pt)) '(to-bottom))
  (tr 'scroll-to-bottom!)
  (render tr 200.0)
  (check-false (tr 'hit-center 'to-bottom #t)))

(test-case "height cache: a delta grows only the streaming entry"
  (define tr (new-tr))
  (for ([i 5]) (tr 'add-user! (format "u~a" i) '()))
  (tr 'apply! (hasheq 'type "delta" 'kind "text" 'text "stream"))
  (define n1 (drawlist-count (render tr 2000.0)))
  (tr 'apply! (hasheq 'type "delta" 'kind "text" 'text "ing more text"))
  (define n2 (drawlist-count (render tr 2000.0)))
  (check-true (> n2 n1)))

;; ---------------------------------------------------------------- 选择 / 复制（G4）

;; 假字体：拉丁 10pt 一字符，行高 12.5；对话区 x=0、条目内边距 14
(define (select-text tr x0 y0 x1 y1)
  (tr 'select-start! x0 y0)
  (tr 'select-extend! x1 y1)
  (render tr 2000.0)
  (tr 'selection-text))

(test-case "select within one answer: copies the rendered text (no Markdown markers)"
  (define tr (new-tr))
  (for ([ev (list (hasheq 'type "turn_start") (asst 1 (txt "run `make test` now"))
                  (hasheq 'type "turn_complete" 'turn 1 'interrupted #f 'outcome "ok"))])
    (tr 'apply! ev))
  (render tr 2000.0)
  ;; 第一行文字顶在 GAP(10) 处；"run make test now" 每字符 10pt，从 x=14 开始
  (check-equal? (select-text tr 14.0 15.0 (+ 14.0 (* 10 13)) 15.0) "run make test"))

(test-case "selection across entries joins with a blank line; soft-wrapped lines join without newline"
  (define tr (new-tr))
  (tr 'add-user! "first message" '())
  (tr 'add-user! "second" '())
  (render tr 2000.0)
  (define all (select-text tr 0.0 0.0 590.0 1990.0))
  (check-equal? all "first message\n\nsecond")
  ;; 窄一点让一段软换行：文字连起来，不插换行
  (define tr2 (new-tr))
  (tr2 'add-user! "aaaa bbbb cccc dddd" '())
  (define dl (make-drawlist))
  (tr2 'render! dl 0.0 0.0 100.0 2000.0 2.0)                  ; 内宽 72pt → 每行最多 7 字符
  (tr2 'select-start! 0.0 0.0)
  (tr2 'select-extend! 100.0 1990.0)
  (tr2 'render! (make-drawlist) 0.0 0.0 100.0 2000.0 2.0)
  (check-equal? (tr2 'selection-text) "aaaa bbbb cccc dddd"))

(test-case "click without drag: no selection; clear-selection! clears"
  (define tr (new-tr))
  (tr 'add-user! "hello" '())
  (render tr 2000.0)
  (tr 'select-start! 20.0 20.0)
  (check-false (tr 'selection-text))
  (tr 'select-extend! 60.0 20.0)
  (check-true (string? (tr 'selection-text)))
  (tr 'clear-selection!)
  (check-false (tr 'selection-text)))

(test-case "file-changing tools show a colored diff (from the tool record, or an old-style diff record); errors keep the output"
  (define tr (new-tr))
  (define text (string-join (cons "@@ -1,3 +1,22 @@" (append (list " a" "-b") (for/list ([i 20]) (format "+n~a" i)) (list " c"))) "\n"))
  (tr 'apply! (asst 1 (call "w" "write_file" (hasheq 'path "x.txt" 'content "..."))))
  (tr 'apply! (it 2 "tool" 'call_id "w" 'name "write_file" 'ms 3 'error #f 'source "builtin" 'diff (hasheq 'path "x.txt" 'added 20 'removed 1 'text text 'truncated #f)))
  (tr 'apply! (results 3 (res "w" "wrote 9 chars to x.txt")))
  (check-equal? (tr 'summary) '(tool:write_file/ok+20-1))
  ;; 数底色块：加行绿底、删行红底（float32 存储，按容差比）
  (define (bg-count dl rgb)
    (for/sum ([i (in-range (drawlist-count dl))])
      (define v (drawlist-ref dl i))
      (if (and (= (list-ref v 13) KIND-RECT) (for/and ([c (in-list rgb)] [k (in-list '(4 5 6))]) (< (abs (- c (list-ref v k))) 0.002))) 1 0)))
  (define ADD '(0.12 0.24 0.16))
  (define DEL '(0.29 0.13 0.14))
  (define dl1 (render tr 2000.0))                                                 ; 折叠：前 16 行 = @@、a、-b、13 个加行
  (check-equal? (list (bg-count dl1 ADD) (bg-count dl1 DEL)) '(13 1))
  (tr 'toggle-tool! "w")
  (define dl2 (render tr 2000.0))                                                 ; 展开：20 个加行全画
  (check-equal? (list (bg-count dl2 ADD) (bg-count dl2 DEL)) '(20 1))
  ;; 出错的工具：不显示 diff
  (tr 'apply! (asst 4 (call "e" "edit_file" (hasheq 'path "x.txt"))))
  (tr 'apply! (results 5 (res "e" "old_string not found" #:error? #t)))
  (check-equal? (cadr (tr 'summary)) 'tool:edit_file/error)
  ;; 旧会话：diff 是单独的记录，且（分叉出的会话里）在工具结果之后很远
  (define tr2 (new-tr))
  (tr2 'apply! (session-ev (list (asst 1 (call "w" "write_file" (hasheq 'path "a.txt"))) (results 2 (res "w" "wrote"))
                                 (it 9 "diff" 'call_id "w" 'diff (hasheq 'path "a.txt" 'added 2 'removed 0 'text "@@ -0,0 +1,2 @@\n+a\n+b" 'truncated #f)))))
  (check-equal? (tr2 'summary) '(tool:write_file/ok+2-0 note)))

(test-case "headless: answer record marks the card (can't be answered again); daemon note shows; MCP source shows in the card title"
  (define tr (new-tr))
  (for ([ev (take TOOL-TURN 7)]) (tr 'apply! ev))
  (tr 'apply! (it 5 "answer" 'perm_id 3 'decision "yes"))
  (check-false (tr 'answer! 3 "no"))
  (tr 'apply! (hasheq 'type "note" 'level "warn" 'text "(earlier output not kept)"))
  (check-equal? (tr 'summary) '(user assistant tool:bash/running perm/yes note))
  (tr 'apply! (asst 10 (call "m1" "search" (hasheq 'pattern "x"))))
  (tr 'apply! (it 11 "tool" 'call_id "m1" 'name "search" 'ms 5 'error #f 'source "mcp:docs"))
  (render tr 2000.0)
  (tr 'select-start! 0.0 0.0) (tr 'select-extend! 599.0 1990.0) (render tr 2000.0)
  (check-true (string-contains? (tr 'selection-text) "[mcp:docs] search")))

(test-case "budget question uses the permission card with its own title, text and buttons"
  (parameterize ([current-lang 'en])
    (define tr (new-tr))
    (tr 'apply! (hasheq 'type "turn_start"))
    (tr 'apply! (it 1 "permission" 'perm_id 9 'prompt "Spending cap reached" 'tool "budget" 'level "spend"
                    'input (hasheq 'scope "day" 'spent 5.02 'cap 5.0)))
    (render tr)                                            ; 看到的文字：全选后取选区文字（对话区自己记录画出的行）
    (tr 'select-start! 1.0 1.0)
    (tr 'select-extend! 599.0 399.0)
    (define text (tr 'selection-text))
    (check-true (string-contains? text "Spending cap reached"))
    (check-true (string-contains? text "$5.02"))
    (check-true (string-contains? text "Continue this turn"))               ; 第一个按钮的文字（选区只取到同一行的第一个）
    (check-false (string-contains? text "Allow"))
    (check-not-false (tr 'hit-center 'perm (cons 9 "always")))
    (check-equal? (tr 'answer! 9 "no") (hasheq 'type "permission" 'id 9 'decision "no"))))

(test-case "per-host documents: background events go to their own doc; switching shows that doc; doc-info reports busy / pending"
  (define tr (new-tr))
  (tr 'add-user! "on local" '())
  (tr 'apply-to! "srv" (hasheq 'type "turn_start" 'kind "prompt"))
  (tr 'apply-to! "srv" (it 1 "permission" 'perm_id 1 'prompt "ok?" 'tool "bash" 'level "dangerous" 'input (hasheq 'command "ls")))
  (check-equal? (tr 'summary) '(user))                          ; 当前文档（local）不受影响
  (check-equal? (tr 'doc-info "srv") (hasheq 'busy #t 'pending 1 'turns 0))
  (tr 'use-doc! "srv")
  (check-equal? (tr 'current-doc) "srv")
  (check-equal? (tr 'summary) '(perm/pending))
  (check-equal? (tr 'pending-permissions) '(1))
  (tr 'use-doc! "local")
  (check-equal? (tr 'summary) '(user))
  (tr 'reset-doc! "srv")
  (check-equal? (tr 'doc-info "srv") (hasheq 'busy #f 'pending 0 'turns 0))
  (tr 'drop-doc! "srv")
  (check-false (tr 'doc-info "srv")))

;; ---------------------------------------------------------------- 局部加载（P3）

;; 合成会话：记录 i 偶数 = 用户消息，奇数 = 回答
(define (rec-item i) (if (even? i) (user i (format "question ~a" i)) (asst i (txt (format "answer ~a\nsecond line" i)))))
(define (summary-ev from to tag)
  (define bs (make-bytes (* 6 (- to from)) 0))
  (for ([i (in-range from to)] [k (in-naturals)])
    (bytes-set! bs (* 6 k) (if (even? i) 1 2))
    (integer->integer-bytes (if (even? i) 1 2) 2 #f #f bs (+ (* 6 k) 2))
    (integer->integer-bytes 1 2 #f #f bs (+ (* 6 k) 4)))
  (hasheq 'type "index_summary" 'from from 'to to 'total to 'tag tag 'data (bytes->string/latin-1 (base64-encode bs #""))))
(define (answer! tr rq)                                   ; 假内核：应答翻页请求
  (case (hash-ref rq 'type)
    [("index_summary") (tr 'apply! (summary-ev (hash-ref rq 'from) (hash-ref rq 'to) (hash-ref rq 'tag)))]
    [("items") (tr 'apply! (hasheq 'type "items" 'from (hash-ref rq 'from) 'to (hash-ref rq 'to) 'total N 'tag (hash-ref rq 'tag)
                                   'items (for/list ([i (in-range (hash-ref rq 'from) (hash-ref rq 'to))]) (rec-item i))))]))
(define N 20000)

(test-case "long session: only the tail is loaded; summary estimates heights; scrolling to the top pages in; far pages are evicted; anchor holds"
  (define tr (new-tr))
  (tr 'apply! (session-ev (for/list ([i (in-range (- N 120) N)]) (rec-item i)) #:total N #:from (- N 120)))
  (render tr 600.0)
  (define rqs (tr 'wanted))
  (check-equal? (map (lambda (r) (hash-ref r 'type)) rqs) '("index_summary"))   ; 尾部已装载：只要摘要
  (check-equal? (hash-ref (tr 'stats) 'loaded) 120)
  (for-each (lambda (rq) (answer! tr rq)) rqs)
  (render tr 600.0)
  (check-equal? (tr 'wanted) '())
  ;; 一个没有标签（别的客户端要的）的应答：不装
  (tr 'apply! (hasheq 'type "items" 'from 0 'to 100 'total N 'items (for/list ([i 100]) (rec-item i))))
  (check-equal? (hash-ref (tr 'stats) 'loaded) 120)
  ;; 滚到最顶上：要第 0 页；应答后装上
  (tr 'scroll! 1e9)
  (render tr 600.0)
  (define top-rq (tr 'wanted))
  (check-equal? (map (lambda (r) (list (hash-ref r 'type) (hash-ref r 'from))) top-rq) '(("items" 0)))
  (for-each (lambda (rq) (answer! tr rq)) top-rq)
  (render tr 600.0)
  (check-equal? (take (tr 'summary) 4) '(user assistant user assistant))
  ;; 往下一页页滚：装载的页数不超过上限（尾部两页常驻）
  (define seen (make-hash))
  (for ([k 400])
    (tr 'scroll! -600.0)
    (render tr 600.0)
    (for ([rq (in-list (tr 'wanted))]) (hash-set! seen (hash-ref rq 'from #f) #t) (answer! tr rq)))
  (check-true (> (hash-count seen) 40))                           ; 走过的页比上限多 → 逐出确实发生了
  (check-true (<= (hash-ref (tr 'stats) 'pages) 30))
  (check-equal? (length (filter (lambda (s) (memq s '(user assistant))) (tr 'summary))) (hash-ref (tr 'stats) 'loaded))
  ;; 锚点：视口上方的高度变了（逐出后重新估计 / 摘要到达）视口顶部那条不动
  (define a0 (hash-ref (tr 'stats) 'anchor))
  (tr 'apply! (summary-ev 0 100 (hash-ref (car top-rq) 'tag)))
  (render tr 600.0)
  (check-equal? (hash-ref (tr 'stats) 'anchor) a0))

(test-case "a stale page answer (from before a session switch) is dropped"
  (define tr (new-tr))
  (tr 'apply! (session-ev (for/list ([i (in-range (- N 120) N)]) (rec-item i)) #:total N #:from (- N 120)))
  (define old (car (tr 'wanted)))
  (tr 'apply! (session-ev (list (user 1 "new session")) #:total 2))
  (answer! tr (hash-set old 'type "items"))                 ; 旧会话的标签
  (check-equal? (hash-ref (tr 'stats) 'total) 2)
  (check-equal? (tr 'summary) '(user note)))

(test-case "minimap: in the right margin when content is longer than the viewport; clicking jumps there (pages then load)"
  (define tr (new-tr))
  (tr 'apply! (session-ev (for/list ([i (in-range (- N 120) N)]) (rec-item i)) #:total N #:from (- N 120)))
  (render tr 600.0)
  (for-each (lambda (rq) (answer! tr rq)) (tr 'wanted))
  (render tr 600.0)
  (define c (tr 'hit-center 'minimap (vector 0.0 600.0)))
  (check-true (pair? c))
  (check-true (> (car c) 600.0))                                  ; 在排版宽度之外
  (check-equal? (tr 'hit (car c) (cdr c)) '(minimap))
  (tr 'minimap-jump! 300.0)                                       ; 中间
  (render tr 600.0)
  (define a (hash-ref (tr 'stats) 'anchor))
  (check-true (< (abs (- a (quotient N 2))) (quotient N 20)) (format "anchor ~a" a))
  (define rq (tr 'wanted))
  (check-true (and (pair? rq) (andmap (lambda (r) (equal? (hash-ref r 'type) "items")) rq)))
  (check-true (for/or ([r rq]) (<= (hash-ref r 'from) a (hash-ref r 'to))))       ; 要的正是跳到的那一页
  (tr 'minimap-jump! 0.0)
  (render tr 600.0)
  (check-true (< (hash-ref (tr 'stats) 'anchor) 5))
  (tr 'minimap-jump! 600.0)                                       ; 底：重新贴底
  (render tr 600.0)
  (check-false (tr 'hit-center 'to-bottom #t)))

(test-case "tool preview rules: drop hides noise lines; match shows lines matching keep first; smart asks the local model once and shows its picks"
  (define out (string-join (append (list "compiling a" "" "compiling b" "warning: unused x") (for/list ([i 30]) (format "ok ~a" i)) (list "error: boom" "done")) "\n"))
  (define tr (new-tr))
  (tr 'apply! (session-ev (list (asst 1 (call "t1" "bash" (hasheq 'command "make"))) (results 2 (res "t1" out)))))
  (void (tr 'wanted))
  (define (shown)
    (render tr 2000.0)
    (tr 'select-start! 0.0 0.0) (tr 'select-extend! 599.0 1990.0) (render tr 2000.0)
    (begin0 (tr 'selection-text) (tr 'clear-selection!)))
  (tr 'set-preview! (hasheq 'lines 3 'mode "head" 'drop "^(compiling|\\s*$)"))
  (define s1 (shown))
  (check-true (string-contains? s1 "warning: unused x"))
  (check-false (string-contains? s1 "compiling a"))
  (tr 'set-preview! (hasheq 'lines 3 'mode "match" 'keep "^(error|warning)"))
  (define s2 (shown))
  (check-true (and (string-contains? s2 "warning: unused x") (string-contains? s2 "error: boom")))
  (check-true (string-contains? s2 "⋯ 30 ⋯"))                       ; 中间跳过的行
  ;; smart：看得到时要一次 select_lines；应答回来按挑的行显示；出错退回按模式
  (tr 'set-preview! (hasheq 'lines 2 'mode "smart" 'min_lines 10 'model "tiny"))
  (render tr 2000.0)
  (define rq (tr 'wanted))
  (check-equal? (map (lambda (r) (list (hash-ref r 'type) (hash-ref r 'id) (hash-ref r 'call_id) (hash-ref r 'max_lines) (hash-ref r 'model))) rq)
                '(("select_lines" 2 "t1" 2 "tiny")))
  (render tr 2000.0)
  (check-equal? (tr 'wanted) '())                                  ; 不重复要
  (tr 'apply! (hasheq 'type "lines" 'id 2 'call_id "t1" 'lines '(4 35) 'tag (hash-ref (car rq) 'tag)))
  (define s3 (shown))
  (check-true (and (string-contains? s3 "warning: unused x") (string-contains? s3 "error: boom")))
  (check-false (string-contains? s3 "compiling a")))

;; ---------------------------------------------------------------- 媒体（P4）

(test-case "media: a sent image is decoded at once (thumbnail); the record takes over the local entry and keeps its media; an image from history is fetched in chunks then decoded; audio play fetches the bytes first, then plays / pauses / resumes"
  (define tr (new-tr))
  (define img (make-bytes 1000 1))
  (define isha (make-string 64 #\c))
  (tr 'add-user! "look" '() (list (hasheq 'kind "image" 'media_type "image/png" 'sha isha 'name "a.png" 'meta (hasheq 'width 800 'height 600) 'bytes img)))
  (check-equal? (fake-media-calls) (list (list 'decode! isha 1000)))
  (tr 'apply! (it 1 "user" 'parts (list (txt "look") (hasheq 'type "media" 'kind "image" 'media_type "image/png" 'sha isha 'name "a.png" 'meta (hasheq 'width 800 'height 600)))
                  'synthetic #f))
  (check-equal? (tr 'summary) '(user))
  (render tr 2000.0)
  (check-equal? (tr 'wanted) '())                                     ; 已解码：不用取
  ;; 历史里的图片 + 音频：图片看得到就取（分块），音频点了才取
  (define hsha (make-string 64 #\d)) (define asha (make-string 64 #\e))
  (tr 'apply! (it 2 "user" 'parts (list (hasheq 'type "media" 'kind "image" 'media_type "image/jpeg" 'sha hsha 'name "old.jpg" 'meta (hasheq 'bytes 700000))
                                        (hasheq 'type "media" 'kind "audio" 'media_type "audio/mp4" 'sha asha 'name "memo.m4a" 'meta (hasheq 'duration 3.0)))
                  'synthetic #f))
  (render tr 2000.0)
  (define rq (tr 'wanted))
  (check-equal? (map (lambda (r) (list (hash-ref r 'type) (hash-ref r 'sha) (hash-ref r 'offset))) rq) (list (list "blob" hsha 0)))
  (define tag (hash-ref (car rq) 'tag))
  (define (chunk sha off bs total) (hasheq 'type "blob_data" 'sha sha 'offset off 'total total 'tag tag 'data (bytes->string/latin-1 (base64-encode bs #""))))
  (tr 'apply! (chunk hsha 0 (make-bytes 524288 7) 700000))
  (check-equal? (map (lambda (r) (list (hash-ref r 'sha) (hash-ref r 'offset))) (tr 'wanted)) (list (list hsha 524288)))
  (tr 'apply! (chunk hsha 524288 (make-bytes (- 700000 524288) 7) 700000))
  (check-equal? (last (fake-media-calls)) (list 'decode! hsha 700000))
  ;; 音频：点 ▶ → 取 → 播；再点 → 暂停；再点 → 继续
  (define c (tr 'hit-center 'media-play asha))
  (check-equal? (tr 'hit (car c) (cdr c)) (list 'media-play asha))
  (tr 'toggle-play! asha)
  (define arq (tr 'wanted))
  (check-equal? (map (lambda (r) (hash-ref r 'sha)) arq) (list asha))
  (tr 'apply! (chunk asha 0 (make-bytes 5000 9) 5000))
  (check-equal? (last (fake-media-calls)) (list 'play! asha 5000))
  (tr 'toggle-play! asha)
  (check-equal? (last (fake-media-calls)) (list 'pause! asha))
  (tr 'toggle-play! asha)
  (check-equal? (last (fake-media-calls)) (list 'resume! asha))
  ;; 取不到的：显示「无法显示」，不再要
  (define gsha (make-string 64 #\f))
  (tr 'apply! (it 3 "user" 'parts (list (hasheq 'type "media" 'kind "image" 'media_type "image/png" 'sha gsha 'name "gone.png" 'meta (hasheq))) 'synthetic #f))
  (render tr 2000.0)
  (define grq (tr 'wanted))
  (tr 'apply! (hasheq 'type "blob_data" 'sha gsha 'offset 0 'error "no such blob" 'tag (hash-ref (car grq) 'tag)))
  (render tr 2000.0)
  (check-equal? (tr 'wanted) '())
  (tr 'select-start! 0.0 0.0) (tr 'select-extend! 599.0 1990.0) (render tr 2000.0)
  (check-true (string-contains? (tr 'selection-text) "gone.png · 无法显示")))

(test-case "sub-agent (P5): its records hang under the spawn_agent card — a count when collapsed, the nested entries (with their own tool cards) when expanded"
  (define tr (new-tr))
  (define (sub id inner) (it id "sub" 'parent "p1" 'host 'null 'inner inner))
  (for ([ev (list (asst 1 (call "p1" "spawn_agent" (hasheq 'task "count")))
                  (sub 2 (hasheq 'kind "user" 'parts (list (txt "count the files")) 'synthetic #f))
                  (sub 3 (hasheq 'kind "assistant" 'parts (list (txt "checking") (call "s1" "echo" (hasheq 'text "hi")))))
                  (sub 4 (hasheq 'kind "tool" 'call_id "s1" 'name "echo" 'ms 2 'error #f 'source "plugin:echo-tool"))
                  (sub 5 (hasheq 'kind "results" 'parts (list (res "s1" "echo: hi"))))
                  (results 6 (res "p1" "sub says: 3 files")))])
    (tr 'apply! ev))
  (check-equal? (tr 'summary) '(tool:spawn_agent/ok))
  (define (shown)
    (render tr 2000.0)
    (tr 'select-start! 0.0 0.0) (tr 'select-extend! 599.0 1990.0) (render tr 2000.0)
    (begin0 (tr 'selection-text) (tr 'clear-selection!)))
  (define s1 (shown))
  (check-true (string-contains? s1 "子 agent 的过程：3 条"))
  (check-false (string-contains? s1 "count the files"))
  (tr 'toggle-tool! "p1")
  (define s2 (shown))
  (check-true (string-contains? s2 "count the files"))
  (check-true (string-contains? s2 "[plugin:echo-tool] echo"))
  (check-true (string-contains? s2 "echo: hi")))
