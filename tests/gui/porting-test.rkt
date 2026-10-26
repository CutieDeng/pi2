#lang racket/base
;; porting-test.rkt — GUI 元数据平面 5：平台知识登记的形状、移植报告覆盖面（由数据生成，不手写）。

(require rackunit racket/string
         "../../src/gui/compute/platform-knowledge.rkt" "../../src/gui/assembly/porting.rkt" "../../src/gui/compute/hub.rkt"
         "../../src/gui/compute/platform.rkt" "../../src/gui/compute/interaction.rkt")

(test-case "knowledge registry: unique ids, known areas and statuses, no empty fields"
  (check-equal? (knowledge-problems) '())
  (check-true (>= (length PLATFORM-KNOWLEDGE) 15))
  (check-equal? (length (knowledge-problems (list (hasheq 'id 'x 'area 'gpu 'macos "a" 'macos-status 'sure 'windows "" 'windows-status 'unknown 'check "c" 'source "s"))))
                3 "unknown area, bad status, empty field"))

(test-case "the porting report covers every interface method, capability, element and known fact"
  (for ([platform (in-list '(windows macos))])
    (define r (porting-report platform))
    (for* ([pi (in-list PLATFORM-INTERFACES)] [m (in-list (iface-methods (car pi)))])
      (check-true (string-contains? r (symbol->string m)) (format "~a: method ~a" platform m)))
    (for ([c (in-list CAPABILITY-NAMES)]) (check-true (string-contains? r (format "`~a`" c)) (format "~a: capability ~a" platform c)))
    (for ([el (in-list ELEMENTS)]) (check-true (string-contains? r (format "**~a**" (car el)))))
    (for ([e (in-list PLATFORM-KNOWLEDGE)]) (check-true (string-contains? r (format "**~a**" (hash-ref e 'id)))))
    (check-true (string-contains? r "Interaction lint: no problems"))))

(test-case "shortcut labels follow the platform: Windows report shows Ctrl+…, macOS ⌘"
  (check-true (string-contains? (porting-report 'windows) "`Ctrl+Shift+T` → tests-panel"))
  (check-true (string-contains? (porting-report 'macos) "`⇧⌘T` → tests-panel"))
  (check-true (string-contains? (porting-report 'windows) "Editing commands come from WIN-TEXT-KEYMAP")))
