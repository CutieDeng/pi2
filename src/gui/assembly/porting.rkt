#lang racket/base
;; porting.rkt — 平台移植报告（GUI 元数据平面 5）：从元数据平面生成「一个平台的后端要做什么、已知什么」（markdown）。
;;   报告由数据生成，不手写：表改了，报告跟着变，不会过期。在拼装层：它把各平台的表放在一起读（计算层的中立模块不许引用平台文件）。
;;   racket src/gui/assembly/porting.rkt windows   → 打印给 Windows 组的报告（macos 也行：现状核对）

(require racket/list racket/string
         "../compute/hub.rkt" "../compute/interfaces.rkt" "../compute/keys.rkt" "../compute/keymap.rkt" "../compute/win-keys.rkt" "../compute/mac-keys.rkt" "../compute/platform.rkt" "../compute/interaction.rkt"
         "../compute/tokens.rkt" "../compute/conformance.rkt" "../compute/drawlist.rkt" "../compute/platform-knowledge.rkt")

;; 平台后端要提供的接口（组件层里平台相关的那几个；其余组件与平台无关、直接复用）
(define PLATFORM-INTERFACES
  (list (cons window-port "window: size, logical events, capabilities, platform requests, capture")
        (cons window-input "smoke-test input injection (logical keys; native paths where the platform has them)")
        (cons clipboard "clipboard (with a private variant for tests)")
        (cons gpu-port "renderer of the drawlist contract (5 instance kinds, clip segments, glyph atlas, image pool)")
        (cons font-backend "text backend: font by role, shape, metrics, rasterize")
        (cons media-port "image thumbnails / audio playback (sandboxed decoding); may be omitted at first")))

(define (porting-report platform)
  (define o (open-output-string))
  (define (p fmt . args) (write-string (apply format fmt args) o) (newline o))
  (parameterize ([current-key-platform platform])
    (p "# pi2 GUI — porting report: ~a" platform)
    (p "")
    (p "Generated from the GUI metadata plane (src/gui/compute/*.rkt). Platform-neutral code (compute/, all components except the platform ones, assembly logic) is reused as is.")
    (p "")
    (p "## 1. Interfaces the platform backend implements")
    (for ([pi (in-list PLATFORM-INTERFACES)])
      (p "- **~a** — ~a" (iface-name (car pi)) (cdr pi))
      (p "  - methods: ~a" (string-join (map symbol->string (iface-methods (car pi))) ", ")))
    (p "")
    (p "## 2. Capabilities to report (window-port 'caps) — what to do without each")
    (for ([c (in-list CAPABILITIES)]) (p "- `~a` (~a): ~a. Without it: ~a" (first c) (second c) (third c) (fourth c)))
    (p "")
    (p "## 3. Platform requests to apply (window-port 'apply!)")
    (for ([k (in-list STATE-KEYS)]) (p "- `~a` (default ~s): ~a" (first k) (second k) (third k)))
    (p "")
    (p "## 4. Input: logical events")
    (p "- Logical keys: ~a; modifiers: ~a; editing commands: ~a." (length LOGICAL-KEYS) LOGICAL-MODS (length COMMAND-NAMES))
    (case platform
      [(windows)
       (p "- Key table: win-keys.rkt, ~a VK codes, status ~a." (length WIN-VK) WIN-STATUS)
       (p "- Editing commands come from WIN-TEXT-KEYMAP (~a bindings); declared without a Windows convention: ~a."
          (length WIN-TEXT-KEYMAP) (string-join (map (lambda (u) (format "~a (~a)" (car u) (cadr u))) WIN-TEXT-UNBOUND) "; "))]
      [(macos)
       (p "- Key table: mac-keys.rkt, ~a key codes; editing commands from AppKit selectors (~a mapped)." (length MAC-KEYCODES) (length MAC-SELECTORS))])
    (p "")
    (p "## 5. Interaction profile (~a, status ~a)" platform (profile-status platform))
    (for ([i (in-list INTENT-NAMES)])
      (define paths (profile-paths platform i))
      (p "- `~a`: ~a" i (if (null? paths) "— (not available)" (string-join (map (lambda (x) (format "~s" x)) paths) " | "))))
    (p "")
    (p "Elements and the intents they accept on this platform (required ones must be reachable; optional = platform enhancement):")
    (for ([el (in-list ELEMENTS)])
      (p "- **~a**: ~a" (car el)
         (string-join (for/list ([d (in-list (cdr el))] #:when (or (not (decl-optional? d)) (reachable? platform (decl-intent d))))
                        (format "~a→~a~a" (decl-intent d) (decl-action d) (if (decl-optional? d) " (enhancement)" "")))
                      ", ")))
    (p "")
    (p "Interaction lint: ~a" (let ([ps (interaction-problems)]) (if (null? ps) "no problems" (string-join ps "; "))))
    (p "")
    (p "## 6. Shortcuts (labels in this platform's style)")
    (for* ([ctx (in-list APP-KEYMAP)] [b (in-list (cdr ctx))]) (p "- ~a · `~a` → ~a" (car ctx) (chord-label (car b)) (cadr b)))
    (p "")
    (p "## 7. Rendering contract")
    (p "- Drawlist: ~a bytes per instance; kinds: rect ~a, mask glyph ~a, color glyph ~a, image ~a, hole ~a; one draw per clip segment; premultiplied over blending; BGRA8 target."
       INSTANCE-BYTES KIND-RECT KIND-MASK-GLYPH KIND-COLOR-GLYPH KIND-IMAGE KIND-HOLE)
    (p "- Reference: compute/softraster.rkt. Conformance: max channel diff ≤ ~a, pixels over ~a/255 ≤ ~a%."
       CONFORMANCE-MAX-DIFF CONFORMANCE-CHANNEL-TOLERANCE (* 100 CONFORMANCE-MAX-FRACTION))
    (p "")
    (p "## 8. Fonts by role")
    (define ff (cdr (assq platform FONT-FAMILIES)))
    (p "- status: ~a" (cadr (assq 'status ff)))
    (for ([r (in-list ff)] #:unless (eq? (car r) 'status)) (p "- `~a`: ~a" (car r) (string-join (cdr r) " → ")))
    (p "- Type scale: ~a" (string-join (for/list ([(k v) (in-hash TYPE-SCALE)]) (format "~a ~a/~a" k (car v) (cdr v))) ", "))
    (p "")
    (p "## 9. Known platform facts (macOS findings → what they mean here)")
    (for ([area (in-list AREAS)])
      (define es (filter (lambda (e) (eq? (hash-ref e 'area) area)) PLATFORM-KNOWLEDGE))
      (unless (null? es)
        (p "### ~a" area)
        (for ([e (in-list es)])
          (p "- **~a** — macOS (~a): ~a" (hash-ref e 'id) (hash-ref e 'macos-status) (hash-ref e 'macos))
          (p "  - Windows (~a): ~a" (hash-ref e 'windows-status) (hash-ref e 'windows))
          (p "  - check: ~a  (source: ~a)" (hash-ref e 'check) (hash-ref e 'source))))))
  (get-output-string o))

(provide PLATFORM-INTERFACES porting-report)

(module+ main
  (define args (current-command-line-arguments))
  (define platform (if (> (vector-length args) 0) (string->symbol (vector-ref args 0)) 'windows))
  (display (porting-report platform)))
