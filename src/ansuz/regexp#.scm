(namespace ("ansuz-regexp#"
            regexp
            make-regexp
            match
            find
            replace))

(define-macro (regexp e)
  
  (define (macro-time-load f)
    (with-exception-catcher
     (lambda (ex) 'ignore)
     (lambda () (load f))))

  (include "~~/site-scheme/futhark/ansuz/sources/string#.scm")
  (include "~~/site-scheme/futhark/ansuz/char-stream-parser#.scm")
  (include "~~/site-scheme/futhark/ansuz/re/parser#.scm")
  (include "~~/site-scheme/futhark/ansuz/re/fgen#.scm")
  (macro-time-load "~~/site-scheme/futhark/ansuz/re/sets")
  (macro-time-load "~~/site-scheme/futhark/ansuz/re/fsm")
  (macro-time-load "~~/site-scheme/futhark/ansuz/re/parser")
  (macro-time-load "~~/site-scheme/futhark/ansuz/re/fgen")
  (fsm->code (run-monad (re)
                        (stream e)
                        (lambda v (car v))
                        (lambda r (error (car r))))))

