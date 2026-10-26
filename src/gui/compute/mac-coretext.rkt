#lang racket/base
;; coretext.rkt — 文字后端的 macOS 实现（S1 阶段 1）。只用 Core Text / Core Graphics / CoreFoundation 的
;; C 接口，零 objc。提供 core/text-backend.rkt 定义的四个过程：font / shape / metrics / rasterize。
;;
;; 下标换算：Core Text 的字符串下标是 UTF-16 码元；这里统一换成 Racket 码点下标再交出去。

(require ffi/unsafe "mac-cf.rkt" "mac-geom.rkt" "text-backend.rkt")

(define libcf (ffi-lib "/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation"))
(define libct (ffi-lib "/System/Library/Frameworks/CoreText.framework/CoreText"))
(define libcg (ffi-lib "/System/Library/Frameworks/CoreGraphics.framework/CoreGraphics"))

(define-cstruct _CFRange ([location _long] [length _long]))
(define RANGE-ALL (make-CFRange 0 0))

;; ---------------------------------------------------------------- CoreFoundation

(define kCFTypeDictionaryKeyCallBacks (ffi-obj-ref 'kCFTypeDictionaryKeyCallBacks libcf))
(define kCFTypeDictionaryValueCallBacks (ffi-obj-ref 'kCFTypeDictionaryValueCallBacks libcf))
(define CFDictionaryCreate
  (get-ffi-obj 'CFDictionaryCreate libcf (_fun _pointer _pointer _pointer _long _pointer _pointer -> _pointer)))
(define CFDictionaryGetValue (get-ffi-obj 'CFDictionaryGetValue libcf (_fun _pointer _pointer -> _pointer)))
(define CFAttributedStringCreate
  (get-ffi-obj 'CFAttributedStringCreate libcf (_fun _pointer _pointer _pointer -> _pointer)))
(define CFArrayGetCount (get-ffi-obj 'CFArrayGetCount libcf (_fun _pointer -> _long)))
(define CFArrayGetValueAtIndex (get-ffi-obj 'CFArrayGetValueAtIndex libcf (_fun _pointer _long -> _pointer)))

;; ---------------------------------------------------------------- Core Text

(define kCTFontAttributeName (get-ffi-obj 'kCTFontAttributeName libct _pointer))
(define CTFontCreateWithName (get-ffi-obj 'CTFontCreateWithName libct (_fun _pointer _double _pointer -> _pointer)))
(define CTFontCreateCopyWithAttributes
  (get-ffi-obj 'CTFontCreateCopyWithAttributes libct (_fun _pointer _double _pointer _pointer -> _pointer)))
(define CTFontCopyPostScriptName (get-ffi-obj 'CTFontCopyPostScriptName libct (_fun _pointer -> _pointer)))
(define CTFontGetSize (get-ffi-obj 'CTFontGetSize libct (_fun _pointer -> _double)))
(define CTFontGetAscent (get-ffi-obj 'CTFontGetAscent libct (_fun _pointer -> _double)))
(define CTFontGetDescent (get-ffi-obj 'CTFontGetDescent libct (_fun _pointer -> _double)))
(define CTFontGetLeading (get-ffi-obj 'CTFontGetLeading libct (_fun _pointer -> _double)))
(define CTFontGetSymbolicTraits (get-ffi-obj 'CTFontGetSymbolicTraits libct (_fun _pointer -> _uint32)))
(define CTFontGetBoundingRectsForGlyphs
  (get-ffi-obj 'CTFontGetBoundingRectsForGlyphs libct (_fun _pointer _uint32 _pointer _pointer _long -> _CGRect)))
(define CTFontDrawGlyphs (get-ffi-obj 'CTFontDrawGlyphs libct (_fun _pointer _pointer _pointer _long _pointer -> _void)))
(define CTLineCreateWithAttributedString
  (get-ffi-obj 'CTLineCreateWithAttributedString libct (_fun _pointer -> _pointer)))
(define CTLineGetGlyphRuns (get-ffi-obj 'CTLineGetGlyphRuns libct (_fun _pointer -> _pointer)))
(define CTLineGetTypographicBounds
  (get-ffi-obj 'CTLineGetTypographicBounds libct (_fun _pointer _pointer _pointer _pointer -> _double)))
(define CTRunGetGlyphCount (get-ffi-obj 'CTRunGetGlyphCount libct (_fun _pointer -> _long)))
(define CTRunGetGlyphs (get-ffi-obj 'CTRunGetGlyphs libct (_fun _pointer _CFRange _pointer -> _void)))
(define CTRunGetPositions (get-ffi-obj 'CTRunGetPositions libct (_fun _pointer _CFRange _pointer -> _void)))
(define CTRunGetStringIndices (get-ffi-obj 'CTRunGetStringIndices libct (_fun _pointer _CFRange _pointer -> _void)))
(define CTRunGetAttributes (get-ffi-obj 'CTRunGetAttributes libct (_fun _pointer -> _pointer)))

(define TRAIT-COLOR-GLYPHS (arithmetic-shift 1 13))   ; kCTFontTraitColorGlyphs

;; ---------------------------------------------------------------- Core Graphics

(define CGColorSpaceCreateDeviceRGB (get-ffi-obj 'CGColorSpaceCreateDeviceRGB libcg (_fun -> _pointer)))
(define CGBitmapContextCreate
  (get-ffi-obj 'CGBitmapContextCreate libcg (_fun _pointer _size _size _size _size _pointer _uint32 -> _pointer)))
(define CGContextRelease (get-ffi-obj 'CGContextRelease libcg (_fun _pointer -> _void)))
(define CGContextSetRGBFillColor
  (get-ffi-obj 'CGContextSetRGBFillColor libcg (_fun _pointer _double _double _double _double -> _void)))
(define (cg-bool-setter name) (get-ffi-obj name libcg (_fun _pointer _stdbool -> _void)))
(define CGContextSetShouldAntialias (cg-bool-setter 'CGContextSetShouldAntialias))
(define CGContextSetAllowsFontSubpixelPositioning (cg-bool-setter 'CGContextSetAllowsFontSubpixelPositioning))
(define CGContextSetShouldSubpixelPositionFonts (cg-bool-setter 'CGContextSetShouldSubpixelPositionFonts))
(define CGContextSetAllowsFontSubpixelQuantization (cg-bool-setter 'CGContextSetAllowsFontSubpixelQuantization))
(define CGContextSetShouldSubpixelQuantizeFonts (cg-bool-setter 'CGContextSetShouldSubpixelQuantizeFonts))

(define BITMAP-PREMULTIPLIED-LAST 1)                  ; RGBA，预乘
(define device-rgb (CGColorSpaceCreateDeviceRGB))     ; 进程内共享，不释放

;; ---------------------------------------------------------------- 字体表

;; font-id → (vector ctfont postscript-name size color?)；按 (postscript-name . size) 去重，含回退字体。
(define fonts (make-hasheqv))
(define font-ids (make-hash))
(define scaled-cache (make-hash))                      ; (font-id . scale) → 放大到像素尺寸的 CTFont

(define (register-font! ctfont)                        ; ctfont 不转移所有权；登记时自行 retain
  (define ps (let ([p (CTFontCopyPostScriptName ctfont)]) (begin0 (cfstring->string p) (cf-release! p))))
  (define size (CTFontGetSize ctfont))
  (define key (cons ps size))
  (or (hash-ref font-ids key #f)
      (let ([id (hash-count fonts)])
        (cf-retain! ctfont)
        (hash-set! fonts id (vector ctfont ps size
                                    (not (zero? (bitwise-and (CTFontGetSymbolicTraits ctfont) TRAIT-COLOR-GLYPHS)))))
        (hash-set! font-ids key id)
        id))
) ; end define register-font!

(define (font-ref id) (vector-ref (hash-ref fonts id) 0))

(define CTFontCreateUIFontForLanguage
  (get-ffi-obj 'CTFontCreateUIFontForLanguage libct (_fun _uint32 _double _pointer -> _pointer)))
(define CTFontCreateCopyWithSymbolicTraits
  (get-ffi-obj 'CTFontCreateCopyWithSymbolicTraits libct (_fun _pointer _double _pointer _uint32 _uint32 -> _pointer)))
(define TRAIT-ITALIC 1)
(define TRAIT-BOLD 2)

;; 在基础字体上加符号特征（斜体 / 粗体）；字体没有该变体时返回基础字体本身（+1）
(define (with-traits base size traits)
  (define f (CTFontCreateCopyWithSymbolicTraits base size #f traits traits))
  (cond [f (cf-release! base) f] [else base])
) ; end define with-traits

;; family：字体族名，或 "system" / "system-bold" / "monospace"（系统 UI 字体，kCTFontUIFontSystem=2 /
;; EmphasizedSystem=3 / UserFixedPitch=1），以及加符号特征的 "system-italic" / "system-bold-italic" / "monospace-bold"
(define (ct-font family size)
  (define sz (exact->inexact size))
  (define f
    (case family
      [("system") (CTFontCreateUIFontForLanguage 2 sz #f)]
      [("system-bold") (CTFontCreateUIFontForLanguage 3 sz #f)]
      [("monospace") (CTFontCreateUIFontForLanguage 1 sz #f)]
      [("system-italic") (with-traits (CTFontCreateUIFontForLanguage 2 sz #f) sz TRAIT-ITALIC)]
      [("system-bold-italic") (with-traits (CTFontCreateUIFontForLanguage 3 sz #f) sz TRAIT-ITALIC)]
      [("monospace-bold") (with-traits (CTFontCreateUIFontForLanguage 1 sz #f) sz TRAIT-BOLD)]
      [else (call-with-cfstring family (lambda (n) (CTFontCreateWithName n sz #f)))]))
  (unless f (error 'ct-font "cannot create font ~a ~a" family size))
  (begin0 (register-font! f) (cf-release! f))
) ; end define ct-font

(define (font-name id) (vector-ref (hash-ref fonts id) 1))
(define (font-color? id) (vector-ref (hash-ref fonts id) 3))

(define (ct-metrics id)
  (define f (font-ref id))
  (vector (CTFontGetAscent f) (CTFontGetDescent f) (CTFontGetLeading f))
) ; end define ct-metrics

;; ---------------------------------------------------------------- shaping

;; UTF-16 码元下标 → 码点下标
(define (utf16->cp-map s)
  (define v (make-vector (for/sum ([c (in-string s)]) (if (> (char->integer c) #xFFFF) 2 1)) 0))
  (for/fold ([u 0]) ([c (in-string s)] [i (in-naturals)])
    (vector-set! v u i)
    (cond [(> (char->integer c) #xFFFF) (vector-set! v (add1 u) i) (+ u 2)]
          [else (add1 u)]))
  v
) ; end define utf16->cp-map

(define (ct-shape str font-id)
  (cond
    [(zero? (string-length str)) (shaped (vector) 0.0)]
    [else
     (define u2c (utf16->cp-map str))
     (define keys (malloc _pointer 1 'raw))
     (define vals (malloc _pointer 1 'raw))
     (ptr-set! keys _pointer kCTFontAttributeName)
     (ptr-set! vals _pointer (font-ref font-id))
     (define attrs (CFDictionaryCreate #f keys vals 1 kCFTypeDictionaryKeyCallBacks kCFTypeDictionaryValueCallBacks))
     (free keys)
     (free vals)
     (define astr (call-with-cfstring str (lambda (s) (CFAttributedStringCreate #f s attrs))))
     (define line (CTLineCreateWithAttributedString astr))
     (define width (CTLineGetTypographicBounds line #f #f #f))
     (define runs (CTLineGetGlyphRuns line))             ; get 规则：不拥有
     (define out
       (for*/fold ([acc '()]) ([ri (in-range (CFArrayGetCount runs))])
         (define run (CFArrayGetValueAtIndex runs ri))
         (define n (CTRunGetGlyphCount run))
         (define fid (register-font! (CFDictionaryGetValue (CTRunGetAttributes run) kCTFontAttributeName)))
         (define gbuf (malloc (* 2 (max n 1)) 'raw))
         (define pbuf (malloc (* 16 (max n 1)) 'raw))
         (define ibuf (malloc (* 8 (max n 1)) 'raw))
         (CTRunGetGlyphs run RANGE-ALL gbuf)
         (CTRunGetPositions run RANGE-ALL pbuf)
         (CTRunGetStringIndices run RANGE-ALL ibuf)
         (define acc*
           (for/fold ([acc acc]) ([k (in-range n)])
             (cons (sglyph fid (ptr-ref gbuf _uint16 k) (ptr-ref pbuf _double (* 2 k))
                           (vector-ref u2c (ptr-ref ibuf _long k)))
                   acc)))
         (free gbuf) (free pbuf) (free ibuf)
         acc*))
     (cf-release! line)
     (cf-release! astr)
     (cf-release! attrs)
     (shaped (list->vector (reverse out)) width)])
) ; end define ct-shape

;; ---------------------------------------------------------------- 光栅化

(define (scaled-font id scale)
  (define key (cons id scale))
  (or (hash-ref scaled-cache key #f)
      (let* ([f (font-ref id)]
             [sf (CTFontCreateCopyWithAttributes f (* scale (CTFontGetSize f)) #f #f)])
        (hash-set! scaled-cache key sf)
        sf))
) ; end define scaled-font

;; subpx ∈ [0,1)：笔位置的像素小数部分（调用方已量化）。返回 #f（空字形）或
;; (vector w h left top rgba color?)，约定见 core/text-backend.rkt。
(define (ct-rasterize id glyph scale subpx)
  (define f (scaled-font id scale))
  (define gp (malloc 2 'raw))
  (ptr-set! gp _uint16 glyph)
  (define bbox (CTFontGetBoundingRectsForGlyphs f 0 gp #f 1))
  (define bx (CGPoint-x (CGRect-origin bbox)))
  (define by (CGPoint-y (CGRect-origin bbox)))
  (define bw (rect-w bbox))
  (define bh (rect-h bbox))
  (cond
    [(or (<= bw 0.0) (<= bh 0.0)) (free gp) #f]
    [else
     ;; 1px 余量防止抗锯齿边缘被裁
     (define x0 (- (floor (+ bx subpx)) 1))
     (define x1 (+ (ceiling (+ bx bw subpx)) 1))
     (define y0 (- (floor by) 1))
     (define y1 (+ (ceiling (+ by bh)) 1))
     (define w (inexact->exact (- x1 x0)))
     (define h (inexact->exact (- y1 y0)))
     (define data (malloc (* w h 4) 'raw))
     (memset data 0 (* w h 4))
     (define ctx (CGBitmapContextCreate data w h 8 (* w 4) device-rgb BITMAP-PREMULTIPLIED-LAST))
     (CGContextSetShouldAntialias ctx #t)
     (CGContextSetAllowsFontSubpixelPositioning ctx #t)
     (CGContextSetShouldSubpixelPositionFonts ctx #t)
     (CGContextSetAllowsFontSubpixelQuantization ctx #f)
     (CGContextSetShouldSubpixelQuantizeFonts ctx #f)
     (CGContextSetRGBFillColor ctx 1.0 1.0 1.0 1.0)      ; 单色字形画白色，着色在 GPU 上做
     (define pp (malloc 16 'raw))
     (ptr-set! pp _double 0 (- subpx x0))                ; 位图坐标：原点左下
     (ptr-set! pp _double 1 (- y0))
     (CTFontDrawGlyphs f gp pp 1 ctx)
     (define out (make-bytes (* w h 4)))
     (memcpy out data (* w h 4))
     (CGContextRelease ctx)
     (free data) (free pp) (free gp)
     (vector w h (inexact->exact x0) (inexact->exact y1) out (font-color? id))])
) ; end define ct-rasterize

(define (make-coretext-backend)
  (text-backend ct-font ct-shape ct-metrics ct-rasterize)
) ; end define make-coretext-backend

(provide make-coretext-backend font-name font-color?)
