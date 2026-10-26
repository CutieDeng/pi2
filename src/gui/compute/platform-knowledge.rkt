#lang racket/base
;; platform-knowledge.rkt — 平台知识登记（GUI 元数据平面 5）：macOS 上实测过的坑与方案，以及它们对 Windows 后端意味着什么。纯数据。
;;   每条：id、area、macos（发现 + 做法）、macos-status、windows（对应的做法 / API）、windows-status、check（在 Windows 上怎么验证）、source。
;;   status：verified（实测）| inferred（有间接证据）| documented（取自公开文档，未实测）| unknown。
;;   来源：design-gui.md §3 问题登记表（P*、S*）与各节实测记录。Windows 一栏是给 Windows 组的起点，不是结论——他们按实测改 status。

(define AREAS '(threading rendering text input ime window clipboard media testing process))
(define STATUSES '(verified inferred documented unknown))

(define (fact id area #:macos m #:macos-status ms #:windows w #:windows-status ws #:check c #:source src)
  (hasheq 'id id 'area area 'macos m 'macos-status ms 'windows w 'windows-status ws 'check c 'source src))

(define PLATFORM-KNOWLEDGE
  (list
   (fact 'main-thread 'threading
         #:macos "AppKit must run on the OS main thread; `racket` starts on it (NSThread isMainThread = #t). Creating NSWindow off the main thread aborts."
         #:macos-status 'verified
         #:windows "A Win32 window belongs to the thread that created it; its message loop must run on that thread. Whether Racket's main thread is the process's first OS thread on Windows is not checked."
         #:windows-status 'unknown
         #:check "Create the window and pump GetMessage/DispatchMessage from the Racket main thread; confirm with GetCurrentThreadId before/after."
         #:source "design-gui.md §3 P1")
   (fact 'place-reinstantiates 'threading
         #:macos "A `place` re-instantiates its module on a new OS thread: GUI code there runs off the main thread. Place entries must be separate modules (dynamic-place) that (require racket/place), or the place dies silently."
         #:macos-status 'verified
         #:windows "Racket semantics, not macOS: the same rule applies to any GUI module (keep window code out of place entry modules)."
         #:windows-status 'inferred
         #:check "Start the in-process kernel place on Windows; confirm the window thread id is unchanged."
         #:source "design-gui.md §3 P2")
   (fact 'render-on-change 'rendering
         #:macos "Rendering every vsync blocks the main thread in nextDrawable and starves green threads (timer p99 64 ms). Render only on change + (sleep 0.004) → p99 1.06 ms; kernel in its own place → 2.6 ms."
         #:macos-status 'verified
         #:windows "Present with vsync (DXGI) blocks the same way: keep render-on-change, and measure timer lateness the same way before adding animation."
         #:windows-status 'documented
         #:check "Run the --synth flows with the debug panel open; compare timer lateness p99 with the macOS numbers."
         #:source "design-gui.md §3 P3")
   (fact 'single-instanced-draw 'rendering
         #:macos "All rects and glyphs of a frame go into one instance buffer (drawlist, 64 bytes per instance, 5 kinds); one instanced draw per clip segment; 19 objc messages per frame regardless of content."
         #:macos-status 'verified
         #:windows "Implement the same drawlist contract (compute/drawlist.rkt) with one instanced draw per clip segment (D3D11: DrawInstanced + RSSetScissorRects), premultiplied blending (ONE, INV_SRC_ALPHA). Check against compute/softraster.rkt with compute/conformance.rkt thresholds."
         #:windows-status 'documented
         #:check "Render the --conformance drawlists with the Windows renderer and compare with the reference rasterizer (max diff ≤ 64, ≤ 0.5% pixels over 8/255)."
         #:source "design-gui.md §3 S10; compute/softraster.rkt")
   (fact 'native-faults-abort 'process
         #:macos "Native faults are not catchable from Racket: an un-ended Metal encoder aborts in dealloc; NSException unwinding through Racket frames calls std::terminate. Guards: dynamic-wind ends the encoder on every exit path, frames run with breaks disabled; MTL_DEBUG_LAYER=1 in tests."
         #:macos-status 'verified
         #:windows "Whether SEH / C++ exceptions from D3D or COM can cross Racket frames is unknown — assume not; guard the same way. Use the D3D11 debug layer (D3D11_CREATE_DEVICE_DEBUG) in tests as the counterpart of MTL_DEBUG_LAYER."
         #:windows-status 'unknown
         #:check "Port tests/gui/fault-test.rkt: inject a failure mid-frame and confirm the process survives and the next frame renders."
         #:source "design-gui.md §3 P8, §7")
   (fact 'shader-compile-time 'rendering
         #:macos "MSL compiled at runtime from source: about 100 ms at startup."
         #:macos-status 'verified
         #:windows "HLSL: compile at runtime with D3DCompile (d3dcompiler_47.dll) or ship precompiled bytecode; measure startup to first frame."
         #:windows-status 'documented
         #:check "Time window creation to first presented frame."
         #:source "design-gui.md §3 P11")
   (fact 'text-backend 'text
         #:macos "Core Text C interface (no objc): UI fonts by role, CJK falls back to PingFang SC automatically; glyph indices are UTF-16 and combining marks have their own index (≠ graphemes)."
         #:macos-status 'verified
         #:windows "DirectWrite: system font collection + IDWriteFontFallback (Windows 8.1+) for CJK; cluster maps are UTF-16 as well. Implement the font-backend interface (font shape metrics rasterize); fonts per role in compute/tokens.rkt FONT-FAMILIES."
         #:windows-status 'documented
         #:check "Run tests/gui/text-test.rkt and the --selftest text checks (Latin, CJK fallback, color emoji) through the Windows backend."
         #:source "design-gui.md §3 S1, §8")
   (fact 'ime-marked-text 'ime
         #:macos "IME via NSTextInputClient on our own view (marked text drawn in the input box, candidate window positioned by firstRectForCharacterRange). Verified with the real Wubi input method."
         #:macos-status 'verified
         #:windows "IMM32 (WM_IME_STARTCOMPOSITION / WM_IME_COMPOSITION / WM_IME_ENDCOMPOSITION, ImmGetCompositionString, ImmSetCandidateWindow for the caret rect) or TSF; map to ev:marked / ev:text. The platform request 'caret (platform.rkt) carries the caret rect."
         #:windows-status 'documented
         #:check "Type 你好 with Microsoft Pinyin: composition shows in the input box, candidates appear at the caret, ev:text \"你好\" arrives."
         #:source "design-gui.md §3 P4, §9.2")
   (fact 'ime-synthetic-keys 'testing
         #:macos "Key events made with keyEventWithType: never reach the input method process; only CGEventCreateKeyboardEvent + eventWithCGEvent: do (no accessibility permission needed)."
         #:macos-status 'verified
         #:windows "SendInput with virtual-key / scan codes goes through the IME; KEYEVENTF_UNICODE input bypasses it. The window-input post-key-cg! method is the place for the IME-reaching path."
         #:windows-status 'documented
         #:check "Drive Microsoft Pinyin with SendInput scan codes from a smoke flow and check that composition happens."
         #:source "memory: IME synthetic key events (G2)")
   (fact 'live-resize-nested-loop 'window
         #:macos "Dragging the window edge enters a nested run loop: our loop stops; windowDidResize: renders a frame synchronously through the on-live-resize hook. A real drag inside the nested loop is not verified."
         #:macos-status 'inferred
         #:windows "The modal sizing loop (WM_ENTERSIZEMOVE … WM_EXITSIZEMOVE) blocks the message loop the same way: render from WM_SIZE / WM_PAINT through the same on-live-resize hook."
         #:windows-status 'documented
         #:check "Drag the window edge; content keeps redrawing."
         #:source "design-gui.md §3 P5")
   (fact 'backing-scale 'window
         #:macos "Backing scale 2.0 on Retina; scale changes (moving to a 1x screen) need contentsScale / drawable size reset and an atlas rebuild — not verified."
         #:macos-status 'inferred
         #:windows "Per-monitor DPI awareness v2 (SetProcessDpiAwarenessContext) and WM_DPICHANGED (use its suggested rect); ev:resize carries the new scale."
         #:windows-status 'documented
         #:check "Move the window between monitors with different scaling; text stays sharp, layout in points unchanged."
         #:source "design-gui.md §3 P9")
   (fact 'own-window-capture 'testing
         #:macos "CGWindowListCreateImage of our own window needs no screen-recording permission (includes the compositor's glass); `screencapture -l` does need it."
         #:macos-status 'verified
         #:windows "PrintWindow(PW_RENDERFULLCONTENT) or DXGI Desktop Duplication; there is no capture permission model. Implement window-port 'capture for the visual checks (capability 'capture)."
         #:windows-status 'documented
         #:check "Run a visual-check flow on Windows; text contrast numbers match the drawlist-based expectation."
         #:source "design-gui.md §28; memory gui-visual-check")
   (fact 'offscreen-test-window 'testing
         #:macos "Test windows live off screen (accessory policy, a window subclass that overrides constrainFrameRect, moved to (-30000,-30000)); WindowServer does not composite glass off screen, so glass checks run on screen."
         #:macos-status 'verified
         #:windows "Off-screen or WS_EX_TOOLWINDOW | WS_EX_NOACTIVATE windows; whether DWM backdrops (Mica / Acrylic) are composited off screen is unknown."
         #:windows-status 'unknown
         #:check "Run the flows with an off-screen window; compare captured pixels with an on-screen run."
         #:source "design-gui.md §31")
   (fact 'focus-loss-stalls 'testing
         #:macos "When the app loses focus nothing triggers redraws, so flows that wait for N frames stall; smoke mode draws at least one frame every 50 ms."
         #:macos-status 'verified
         #:windows "Unknown whether a background window gets the same effect; keep the 50 ms minimum frame rule in smoke mode."
         #:windows-status 'unknown
         #:check "Run a flow while another window is focused."
         #:source "memory gui-visual-check")
   (fact 'synthetic-mouse-not-frontmost 'testing
         #:macos "postEvent: mouse events get shifted coordinates when the app is not frontmost (about (-46, -17) pt): smoke flows inject logical events directly, and use sendEvent: (post-mouse-ns!) for the real AppKit path."
         #:macos-status 'verified
         #:windows "SendInput needs the foreground window; PostMessage(WM_LBUTTONDOWN …) to our own HWND works in the background. Keep both paths: logical injection and native."
         #:windows-status 'documented
         #:check "Run the right-click and drag flows with the window in the background."
         #:source "components/mac-window.rkt window-post-mouse!")
   (fact 'private-clipboard 'clipboard
         #:macos "Tests use a private pasteboard (pasteboardWithUniqueName) so they never touch the user's clipboard."
         #:macos-status 'verified
         #:windows "The Windows clipboard is global: implement clipboard 'call-with-private with an in-process fake (or save and restore) so tests do not clobber the user's clipboard."
         #:windows-status 'documented
         #:check "Run the paste tests while the clipboard holds user text; it is unchanged afterwards."
         #:source "memory: private pasteboard")
   (fact 'media-sandbox 'media
         #:macos "Images and audio are decoded / played in two sandboxed subprocesses (sandbox_init pure-computation after a warm-up decode; audio under a custom SBPL that denies network, file writes and exec)."
         #:macos-status 'verified
         #:windows "Decode with WIC and play with Media Foundation in a subprocess with a restricted token / AppContainer / Job object; until then the capability can be off (no previews)."
         #:windows-status 'documented
         #:check "From the decoding subprocess, try a file write and a TCP connect; both must fail."
         #:source "design-content.md §9 P4")
   (fact 'glass-material 'window
         #:macos "Liquid Glass sidebar: NSGlassEffectView behind a transparent Metal overlay; the main layer leaves a same-shaped hole (drawlist kind 4)."
         #:macos-status 'verified
         #:windows "Windows 11 22H2+: DwmSetWindowAttribute(DWMWA_SYSTEMBACKDROP_TYPE, Mica / Acrylic) with a transparent client area where the panel shows; otherwise report capability 'glass #f (opaque sidebar)."
         #:windows-status 'documented
         #:check "Turn the glass material on; the sidebar shows the backdrop and the visual check passes."
         #:source "design-gui.md §26–§28")
   (fact 'keyboard-mapping 'input
         #:macos "Logical keys: named keys and punctuation by key code; letters and digits by charactersIgnoringModifiers (follows the layout); ⌘ → primary; AppKit key bindings produce editing commands."
         #:macos-status 'verified
         #:windows "WM_KEYDOWN VK codes (letters follow the layout) + WM_CHAR for text; Ctrl → primary; editing commands from compute/win-keys.rkt WIN-TEXT-KEYMAP (no system key-binding layer)."
         #:windows-status 'documented
         #:check "tests/gui/keys-test.rkt passes; type Ctrl+Shift+T, Ctrl+Backspace, Shift+Home in the input box."
         #:source "compute/mac-keys.rkt, compute/win-keys.rkt")
   (fact 'racket-on-windows-arm 'process
         #:macos "—"
         #:macos-status 'unknown
         #:windows "Whether Racket CS runs on Windows ARM64 (relevant for a VM on Apple Silicon) is not checked; x64 Windows is the safe target."
         #:windows-status 'unknown
         #:check "Install Racket CS on the target machine; run raco test tests/gui."
         #:source "—")))

(define (knowledge-problems [entries PLATFORM-KNOWLEDGE])
  (append
   (let ([ids (map (lambda (e) (hash-ref e 'id)) entries)])
     (for/list ([i (in-list ids)] #:when (> (length (filter (lambda (x) (eq? x i)) ids)) 1)) (format "duplicate id ~s" i)))
   (for*/list ([e (in-list entries)]
               [problem (in-list (list (and (not (memq (hash-ref e 'area) AREAS)) "unknown area")
                                       (and (not (memq (hash-ref e 'macos-status) STATUSES)) "bad macos-status")
                                       (and (not (memq (hash-ref e 'windows-status) STATUSES)) "bad windows-status")
                                       (and (not (for/and ([k '(macos windows check source)]) (and (string? (hash-ref e k)) (positive? (string-length (hash-ref e k))))))
                                            "empty text field")))]
               #:when problem)
     (format "~a: ~a" (hash-ref e 'id) problem))))

(provide AREAS STATUSES PLATFORM-KNOWLEDGE knowledge-problems)
