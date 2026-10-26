(skill
 (name "commit")
 (group "git-workflow")
 (brief "Generate & validate a .commit message for staged changes")
 (when-to-use "user wants to prepare/refresh the .commit datum for the current staged (or modified) changes")
 (desc "Validate a .commit Racket-datum file: (TYPE title metadata detail)")
 (prompt "Update the .commit file for the currently staged (or if none, modified) changes. Use ONLY read-only git queries (git status --short, git diff --cached, git diff --cached --stat, git log --oneline -3). Then write .commit as exactly one Racket datum (TYPE title metadata detail), formatted on MULTIPLE lines: the FIRST line contains ONLY the type and the quoted title, e.g. the whole first line is `(FEAT \"short title\"` and nothing more. Put the metadata `()` on the second line, and the detail string (with a Modified: list of the exact files) starting on the third line. Do NOT put the metadata or the detail on the first line. Title must be <=50 display columns (CJK/fullwidth count as 2). TYPE is one of FEAT FIX REFACTOR TEST DOCS BUILD. Example:\n(FEAT \"short title\"\n ()\n \"what changed and why.\nModified:\n- path/to/file\")\nDo not run any state-changing git command. Do not write any file other than .commit. After writing .commit, call the commit_validate tool with message-file \".commit\" to check it; if it reports invalid, read the detail, fix .commit, and re-validate until valid.")
 (writes ".commit")
 (entry validate (".commit"))
 (fn validate
     (params message-file)
     (reads message-file)
     (limits (memory 64) (time 5))
     (body
      (define TYPES '(FEAT FIX REFACTOR TEST DOCS BUILD))
      ;; git subject 的 50 是**显示列**约定，非码点数：CJK/全角占 2 列。用码点数会放行
      ;; 50 个汉字（≈100 列）的超宽标题，量纲错误。故按显示宽度度量。
      (define (wide? c)
        (define n (char->integer c))
        (or (<= #x1100 n #x115F) (<= #x2E80 n #xA4CF) (<= #xAC00 n #xD7A3)
            (<= #xF900 n #xFAFF) (<= #xFE30 n #xFE4F) (<= #xFF00 n #xFF60)
            (<= #xFFE0 n #xFFE6) (<= #x1F300 n #x1FAFF) (<= #x20000 n #x3FFFD)))
      (define (display-width s) (for/sum ([c (in-string s)]) (if (wide? c) 2 1)))
      (define (validate message-file)
        (with-handlers ([exn:fail? (lambda (e) (result #f "parse error" (exn-message e) '()))])
          (define d (call-with-input-file message-file read))
          ;; 物理首行（供「标题独占首行」结构检查）：只有 (TYPE "title"，不许带 () 或 detail。
          ;; 注：沙箱是 racket/base，无 string-trim，故首尾空白交给正则容忍。
          (define first-line
            (with-handlers ([exn:fail? (lambda (_e) "")])
              (let ([l (call-with-input-file message-file read-line)]) (if (string? l) l ""))))
          (cond
            [(not (and (list? d) (= 4 (length d))))
             (result #f "bad shape" "must be (TYPE title metadata detail)" (list 'got d))]
            [else
             (define type (list-ref d 0)) (define title (list-ref d 1))
             (define meta (list-ref d 2)) (define detail (list-ref d 3))
             (define tw (and (string? title) (display-width title)))
             (define checks
               (list (list 'type-valid (and (memq type TYPES) #t))
                     (list 'title-string (string? title))
                     (list 'title-width<=50 (and tw (<= tw 50)))
                     ;; 首行须恰为 (TYPE "title" —— metadata/detail 换行，不得挤在标题行
                     (list 'title-on-first-line
                           (and (memq type TYPES) (string? title)
                                (regexp-match? (regexp (format "^[ \t]*[(]~a[ ]+\"[^\"]*\"[ \t]*$" type)) first-line)
                                #t))
                     (list 'metadata-list (list? meta))
                     (list 'detail-string (string? detail))
                     (list 'has-modified (and (string? detail) (regexp-match? #rx"Modified:" detail) #t))))
             (define ok (andmap cadr checks))
             (result ok
                     (if ok "valid" "invalid")
                     (format "~a commit, title ~a cols (~a cp), ~a checks ~a"
                             type (or tw '?) (if (string? title) (string-length title) '?)
                             (length (filter cadr checks)) (if ok "pass" "-> see detail"))
                     (list (list 'type type) (list 'title-cols tw) (list 'checks checks)))]))))))
