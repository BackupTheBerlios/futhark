;; gsi ansuz-extra
;; result:
;; (run a*b on "aaaaaaabcdef" : "aaaaaaab")

(include "~~/site-scheme/futhark/ansuz/on-strings#.scm")

(define-parser (a*b)
  (>> (<- s0 (kleene (char #\a)))
      (<- s (char #\b))
      (return (list->string (append s0 (list s))))))

(define str "aaaaaaabcdef")


(pp `(run a*b on ,str : ,(run (a*b) str)))
