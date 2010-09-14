(##namespace ("prolog-prolog#"
              clause
              define-clause
              ?))

(include "kernel#.scm")
(include "unify#.scm")

(define-macro (clause f . b)
  `(lambda+ ,f (logic (>> ,@b))))

(define-macro (define-clause h . b)
  `(define ,(car h) (clause ,(cdr h) ,@b)))

(define-macro (? e)
  (let(
       (v (gensym 'v))
       (st (gensym 'st))
       (bt (gensym 'bt))
       (vars (get-variables e)))
  `(run-monad (logic (let ,(map (lambda (v) `(,v (make-variable))) vars) ,e))
              #t
              (lambda (v st bt) (cons (list ,@vars) bt)))))

