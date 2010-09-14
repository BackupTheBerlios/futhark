(##namespace ("laguz#"))

(##include "~~/lib/gambit#.scm")
(include "reflect#.scm")
(include "monad-plus#.scm")
(include "variables#.scm")

(define *-var-* (list 'var))

(define (occur? v b)
  (let occur ((b b))
    (cond
     ((eq? b v) #t)
     ((variable? b) (occur (subst b)))
     ((pair? b)
      (or (occur (car b))
          (occur (cdr b))))
     (else #f))))

(define (subst v)
  (cond
   ((variable? v)
    (let(
         (val (variable-get v)))
      (if (eq? val #!void) v (subst val))))
   ((pair? v)
    (cons (subst (car v))
          (subst (cdr v))))
   (else v)))

(define (reset-variables! ms m0)
  (let reset ((xs ms) (ys '()))
    (if (eq? xs m0) ys
        (begin
          (variable-reset! (car xs))
          (reset (cdr xs) (cons (car xs) ys))))))

(define unify
  (lambda+ (a b)
           (reflect (mv oc zz ct bt)
                    (begin
                      (let unify ((a a) (b b) (mv mv) (cn (lambda (r mv1) (if r (ct #t mv1 oc zz bt) (bt mv1)))))
                        (cond
                         ((eq? a b) (cn #t mv))
                         
                         ((variable? a) ;; occur checko
                          (let(
                               (val (variable-get a)))
                            (if (eq? val #!void)
                                (if (and oc (occur? a b))
                                    (cn #f mv)
                                    (begin
                                      (variable-set! a b)
                                      (cn #t (cons a mv))))
                                (unify val b mv cn))))
                         
                         ((variable? b) ;; occur check
                          (let(
                               (val (variable-get b)))
                            (if (eq? val #!void)
                              (if (and oc (occur? b a))
                                  (cn #f mv)
                                  (begin
                                    (variable-set! b a)
                                    (cn #t (cons b mv))))
                              (unify a val mv cn))))
                         
                         ((and (pair? a) (pair? b))
                          (unify (car a) (car b) mv
                                 (lambda (r mv1)
                                   (if r (unify (cdr a) (cdr b) mv1 cn) (cn r mv1)))))
                         (else
                          (cn #f mv))))))))
