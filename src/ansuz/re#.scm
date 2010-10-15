
(namespace ("ansuz-re-parser#" regexp define-regexp))


(define-macro (regexp e . x)
  
  (include "~~/site-scheme/futhark/ansuz/sources/string#.scm")
  (include "~~/site-scheme/futhark/ansuz/char-stream-parser#.scm")
  (include "~~/site-scheme/futhark/ansuz/re/parser#.scm")
  (include "~~/site-scheme/futhark/ansuz/re/cgen#.scm")
  
  (define (macro-time-load f)
    (with-exception-catcher
     (lambda (ex) 'ignore)
     (lambda () (load f))))

  (macro-time-load "~~/site-scheme/futhark/ansuz/re/sets")
  (macro-time-load "~~/site-scheme/futhark/ansuz/re/fsm")
  (macro-time-load "~~/site-scheme/futhark/ansuz/re/parser")
  (macro-time-load "~~/site-scheme/futhark/ansuz/re/cgen")

  `(with-state ,x ,(fsm->code (run-monad (re)
                                         (stream e)
                                         (lambda v (car v))
                                         (lambda r (error (car r)))))))

(define-macro (define-regexp name val)
  `(define-parser (,name) (regexp ,val)))