;; (include "../src/ansuz/re/fsm.scm")
;; (include "../src/ansuz/re/sets.scm")
;; (include "../src/ansuz/re/parser.scm")
;; (##namespace (""))

;; (include "~~/lib/gambit#.scm")

(include "~~/site-scheme/futhark/ansuz/on-strings#.scm")
(include "~~/site-scheme/futhark/ansuz/re#.scm")

(define (repeat-times n fn)
  (if (>= n 0)
      (begin
        (fn)
        (repeat-times (- n 1) fn))))

(time (repeat-times 100000 (lambda () (run (regexp "a*b") "aaaaaaaaaabcdef"))))
