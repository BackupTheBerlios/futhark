(##namespace ("ansuz-monad#"
              lambda+
              return
              bind))

(include "reflect#.scm")
(declare (inlining-limit 0))

(define-macro (lambda+ f b)
  (let(
       (ts (gensym 'ts))
       (sc (gensym 'sc))
       (fl (gensym 'fl)))
    `(lambda ,(append f (list ts sc fl))
       (with-state (,ts ,sc ,fl) ,b))))

(define-macro (return v . x)
  (let(
       (ts (gensym 'ts))
       (sc (gensym 'sc))
       (fl (gensym 'fl)))
    `(with-state ,x
                (reflect (,ts ,sc ,fl)
                         (,sc ,v ,ts ,fl)))))

(define-macro (bind p n ts sc fl)
  (let(
       (v (car p))
       (m (cadr p))
       (ts1 (gensym 'ts1))
       (fl1 (gensym 'fl1)))
    `(with-state
      (,ts
       (lambda (,v ,ts1 ,fl1) (with-state (,ts1 ,sc ,fl1) ,n))
       ,fl)
      ,m)))

