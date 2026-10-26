#lang racket/base
;; tests/gui/window-smoke-test.rkt — 真窗口冒烟（会弹窗；默认不跑，./run-tests.sh --gui）
;;  1. 合成输入：改尺寸、点击、⌘V（私有剪贴板，不碰系统剪贴板）→ chip、打字、模拟输入法组字、⇧Enter、Enter 发送
;;  2. 经内核（脚本模型）：权限卡片、工具、Esc 中断；会话侧栏新建 / 切换 / 分叉 / 删除；模型菜单
;;  3. 真实输入法（若已启用五笔 SCIM.WBX）：临时切换输入源，经输入法进程组字上屏，结束后切回

(require rackunit racket/runtime-path racket/system racket/string racket/file
         "../../src/gui/compute/mac-inputsource.rkt")

(define-runtime-path APP "../../src/gui/assembly/app.rkt")

(define (run-app . args)
  (define out (open-output-string))
  (define ok?
    (parameterize ([current-output-port out] [current-error-port out]
                   [current-environment-variables (environment-variables-copy (current-environment-variables))])
      (putenv "MTL_DEBUG_LAYER" "1")
      ;; 测试不往 ~/Library/Logs/pi2 写日志：没指定日志目录就只留在内存
      (apply system* (find-system-path 'exec-file) (path->string APP)
             (if (or (member "--journal-dir" args) (member "--replay" args)) args (append args '("--no-journal"))))))
  (values ok? (get-output-string out)))

(test-case "input box (no core): resize + ⌘V fold + typing + IME protocol + ⇧Enter + Enter + Cmd-Q"
  (define-values (ok? text) (run-app "--exit-after" "8000" "--resize-to" "700x400" "--synth"))
  (check-true ok? text)
  (check-regexp-match #rx"quit=close" text)                         ; Cmd-Q 经菜单 → piQuit: 退出（非超时）
  (check-regexp-match #rx"resizes=\\(\\(960.0 680.0\\) \\(700.0 400.0\\)\\)" text)
  (check-regexp-match #rx"synth-check=ok" text)                     ; 发送内容 = "a" + 报错栈原文 + " fix你\nx"
  (check-regexp-match #rx"\\(struct:ev:paste \\. 1\\)" text)        ; 粘贴事件到达（窗口为 key 时走 ⌘V → Edit 菜单 → paste:）
  (check-regexp-match #rx"paste-path=(menu|direct)" text)
  (check-regexp-match #rx"ime-query=\\(#\\([0-9.]+ [0-9.]+ 2.0 [0-9.]+\\) #t\\)" text)   ; 组字中：候选框矩形 + hasMarkedText
  ;; S10：每帧 objc 消息数与实例数无关；裁剪段数固定（3 段），上限 30
  (define m (regexp-match #rx"objc-msgs/frame=\\(([0-9 ]+)\\)" text))
  (check-true (and m (andmap (lambda (n) (<= n 30)) (map string->number (string-split (cadr m))))) text))

(define-runtime-path CORE "fixtures/scripted-core.rkt")

(test-case "with core (scripted model, real RPC v2 + tools): permission click → tool → answer; Esc interrupts"
  (define-values (ok? text) (run-app "--core" (path->string CORE) "--synth-core" "--exit-after" "30000"))
  (check-true ok? text)
  (check-regexp-match #rx"quit=close" text)
  (check-regexp-match #rx"turns=2" text)
  (check-regexp-match #rx"perm/yes tool:bash/ok assistant" text)   ; 点「允许」后工具执行、模型回答
  (check-true (string-contains? text "user note) core-exit") text)             ; Esc → 中断：半截回答不留，只剩用户消息 + 中断提示
  (check-true (string-contains? text "(\"user\" \"now hang\") (\"user\" \"<turn_aborted>") text)   ; 内核历史：用户消息 + 标记
  (check-regexp-match #rx"core-exit=0" text))                       ; 关 stdin 后内核自行退出

(test-case "session sidebar through the core: new → resume → fork → two-click delete"
  (define-values (ok? text) (run-app "--core" (path->string CORE) "--synth-sessions" "--exit-after" "30000"))
  (check-true ok? text)
  (check-regexp-match #rx"quit=close" text)
  (check-true (string-contains? text "(new-empty note)") text)                          ; 新会话：对话区只剩切换提示
  (check-true (string-contains? text "(b-answer . \"收到：beta question\")") text)
  (check-true (string-contains? text "(resumed-answer . \"收到：alpha question\")") text)  ; 点回 A：对话区换成 A 的历史
  (check-true (string-contains? text "(fork-msgs . 2)") text)                            ; 分叉带上 A 的 2 条消息
  (check-true (string-contains? text "(final #t #t 2)") text)                            ; 删掉 B：剩 A 与当前的 C
  (check-regexp-match #rx"core-exit=0" text))

(test-case "model menu through the core: reasoning level, provider switch and back, Esc closes the menu without interrupting"
  (define-values (ok? text) (run-app "--core" (path->string CORE) "--synth-picker" "--exit-after" "30000"))
  (check-true ok? text)
  (check-regexp-match #rx"quit=close" text)
  (check-true (string-contains? text "(reasoning \"high\" #t)") text)                  ; 选了档位：菜单不关，内核 state 回报 high
  (check-true (string-contains? text "(switched \"grok\" \"grok-4\" #t)") text)       ; 切供应商：模型换成该档案默认；菜单仍开
  (check-true (string-contains? text "(back \"lmstudio\"") text)
  (check-true (string-contains? text "(local-model . \"qwen/qwen3.5-9b\")") text)       ; 内核后台查到的本地模型可选
  (check-true (string-contains? text "(wheel-over-sidebar-menu-unmoved . #t)") text)  ; 滚轮跟指针走，不跟打开的菜单
  (check-true (string-contains? text "(wheel-over-models-moved . #t)") text)
  (check-true (string-contains? text "(chip-closed . #t)") text)
  (check-true (string-contains? text "(esc-closed #t #f)") text)                        ; Esc 只关菜单，不发 interrupt
  (check-regexp-match #rx"core-exit=0" text))

(test-case "routing pool through the core: select @pool, 429 on fa fails over to fb with a visible note, member pin"
  (define-values (ok? text) (run-app "--core" (path->string CORE) "--synth-route" "--exit-after" "30000"))
  (check-true ok? text)
  (check-true (string-contains? text "(selected \"@pool\" \"m1\")") text)
  (check-true (string-contains? text "(answer . \"answer from fb/m2\")") text)        ; fa 限流 → 同一请求切到 fb
  (check-true (string-contains? text "(transcript note user note assistant note)") text) ; 用户消息后有切换提示
  (check-true (string-contains? text "(pinned . \"m1\")") text)
  (check-regexp-match #rx"core-exit=0" text))

(test-case "settings page through the core: add an account with a key, permission mode; the key never reaches the journal"
  (define dir (make-temporary-file "pi2-gui-jr-~a" 'directory))
  (define-values (ok? text) (run-app "--core" (path->string CORE) "--synth-settings" "--exit-after" "30000" "--journal-dir" (path->string dir)))
  (check-true ok? text)
  (check-true (string-contains? text "(account-added . \"grok[team]\")") text)
  (check-true (string-contains? text "(permission . \"read-only\")") text)
  (check-true (string-contains? text "(closed . #t)") text)
  (define journal (file->string (car (directory-list dir #:build? #t))))
  ;; 按键、上屏文字、请求里都不能拼出 key：逐字符的按键记录连起来也不行
  (define keys (apply string-append (map cadr (regexp-match* #px"#s\\(ev:key #t [0-9]+ \"([^\"]*)\"" journal #:match-select values))))
  (check-false (regexp-match? #rx"sk-test" keys) keys)
  (check-false (regexp-match? #rx"sk-test" journal))
  (check-regexp-match #rx"#s\\(ev:secret \"⟦secret:[0-9a-f]+⟧\"" journal)          ; 只留句柄
  (check-false (regexp-match? (pregexp (string-append (string (integer->char #x2022)) "{5}")) journal)))   ; 不再记等长圆点

(test-case "a key pasted into the chat becomes a secret chip; the core, the transcript and the journal only see the handle"
  (define dir (make-temporary-file "pi2-gui-jr-~a" 'directory))
  (define-values (ok? text) (run-app "--core" (path->string CORE) "--synth-secret" "--exit-after" "30000" "--journal-dir" (path->string dir)))
  (check-true ok? text)
  (check-true (string-contains? text "(pending-chip . #t)") text)
  (check-true (string-contains? text "(answer-has-placeholder . #t)") text)               ; 内核回显：占位符
  (check-true (string-contains? text "(answer-has-key . #f)") text)
  (define journal (file->string (car (directory-list dir #:build? #t))))
  (check-false (regexp-match? #rx"AbCdEf" journal))
  (check-regexp-match #rx"#s\\(ev:secret \"⟦secret:[0-9a-f]+⟧\" \"chat-pending\"\\)" journal))

(test-case "operation journal: recorded window runs replay headless with identical requests and no errors"
  (for ([flow '("--synth-sessions" "--synth-picker" "--synth-core" "--synth-route" "--synth-settings" "--synth-secret")])
    (define dir (make-temporary-file "pi2-gui-jr-~a" 'directory))
    (define-values (ok? text) (run-app "--core" (path->string CORE) flow "--exit-after" "30000" "--journal-dir" (path->string dir)))
    (check-true ok? text)
    (define j (car (directory-list dir #:build? #t)))
    (define-values (ok2? text2) (run-app "--replay" (path->string j)))
    (check-true ok2? text2)
    (check-regexp-match #rx"G6 replay: progress=\\(([0-9]+) \\. \\1\\) errors=\\(\\) sends=([0-9]+)/\\2 match=#t" text2)))

(define WUBI "com.apple.inputmethod.SCIM.WBX")

(when (member WUBI (enabled-input-sources))
  (test-case "real IME (Wubi): wq␣ → 你, vb␣ → 好; input source restored"
    (define before (current-input-source))
    (define-values (ok? text) (run-app "--exit-after" "6000" "--ime-real" WUBI))
    (check-true ok? text)
    (check-regexp-match #rx"\\(marked wq\\)" text)                  ; 输入法进程回调 setMarkedText:
    (check-regexp-match #rx"\\(text 你\\)" text)
    (check-regexp-match #rx"\\(text 好\\)" text)
    (check-equal? (current-input-source) before)))                  ; 另起查询：输入源已切回
