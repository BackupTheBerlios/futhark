(##namespace ("prolog-clause#"))

(##include "~~/lib/gambit#.scm")

(include "reflect#.scm")
(include "monad-plus#.scm")
(include "unify1.scm")

(define-macro (sequence m n . x)
  `(with-state ,x (bind (,(gensym 'ignore) ,m) ,n)))

(define-macro (success . x)
  `(with-state ,x (return #t)))

(define-macro (negate m . x)
  (let(
       (st (gensym 'st))
       (ct (gensym 'ct))
       (bt (gensym 'bt))
       (mm (gensym 'm))
       (st1 (gensym 'st))
       (bt1 (gensym 'bt)))
    `(with-state ,x
                 (reify (,mm ,m)
                        (reflect (,st ,ct ,bt)
                                 (,mm ,st (lambda (,v ,st1 ,bt1) (,bt)) 
                                      (lambda () (,ct #t ,st ,bt))))))))

(define state (lambda+ () (reflect (st ct bt) (ct st st bt))))

(define-macro (if+ t? m n . x)
  (let(
       (mm (gensym 'm))
       (nn (gensym 'n))
       (st (gensym 'st))
       (ct (gensym 'ct))
       (bt (gensym 'bt)))
    `(with-state ,x
                 (reify (,mm ,m)
                        (reify (,nn ,n)
                               (reflect (,st ,ct ,bt)
                                        (if ,t?
                                            (,mm ,st ,ct ,bt)
                                            (,nn ,st ,ct ,bt))))))))
                  
(define-macro (logic e . x)
  `(with-state ,x
               ,(cond
                 ((equal? e '(>>))
                  `(success))
                 ((equal? e '(<>))
                  `(fail))
                 ((and (pair? e) (eq? (car e) '>>) (null? (cddr e)))
                  `(logic ,(cadr e)))
                 
                 ((and (pair? e) (eq? (car e) '>>) (pair? (cadr e)) (eq? (car (cadr e)) '<-))
                  `(bind (,(cadr (cadr e)) (logic ,(caddr (cadr e)))) (logic (>> ,@(cddr e)))))
                 
                 ((and (pair? e) (eq? (car e) '>>))
                  `(sequence (logic ,(cadr e)) (logic (>> ,@(cddr e)))))
                 
                 ((and (pair? e) (eq? (car e) '<>) (null? (cddr e)))
                  `(logic ,(cadr e)))
                 
                 ((and (pair? e) (eq? (car e) '<>))
                  `(orelse (logic ,(cadr e)) (logic (<> ,@(cddr e)))))

                 ((and (pair? e) (eq? (car e) 'not))
                  `(negate (logic ,(cadr e))))

                 ((and (pair? e) (eq? (car e) 'case-unify))
                  `(logic (<> ,@(map (lambda (c) `(if-unify ,(cadr e) ,(car c) (logic (>> ,@(cdr c))))) (cddr e)))))

                 (else e))))

(define-macro (if-unify a b c . x)
  (let*(
        (variables
         (let variables ((data (list b c)) (vs '()))
           (cond
            ((and (pair? data) (eq? (car data) 'unquote))
             (if (memq (cadr data) vs) vs (cons (cadr data) vs)))
            ((pair? data)
             (variables (cdr data) (variables (car data) vs)))
            (else vs)))))
    `(with-state ,x
                 (logic (>> ,@(append (map (lambda (v) `(<- ,v (newvar (quote ,v)))) variables)
                                      (map (lambda (a0 b0) `(unify ,a0 ,(list 'quasiquote b0))) a b)
                                      (let replace ((c (list c)))
                                        (cond
                                         ((and (pair? c) (eq? (car c) 'unquote)) (cadr c))
                                         ((pair? c) (cons (replace (car c))
                                                          (replace (cdr c))))
                                         (else c)))))))))
                                          
              
(define-macro (clause f . b)
  `(lambda+ ,f (logic (>> ,@b))))

(define-macro (define-clause h . b)
  `(define ,(car h) (clause ,(cdr h) ,@b)))

(define-macro (?- e #!key (occur-check #f))
  (let(
       (variables
        (let variables ((data e) (vs '()))
          (cond
           ((and (pair? data) (eq? (car data) 'unquote))
            (if (memq (cadr data) vs) vs (cons (cadr data) vs)))
           ((pair? data)
            (variables (cdr data) (variables (car data) vs)))
           (else vs)))))
    `(run-monad
      (logic (>> ,@(append (map (lambda (v) `(<- ,v (newvar ',v))) variables)
                           (list e))))
      occur-check
      (lambda (v st bt) (values (list ,@(map (lambda (v) `(cons (variable-name ,v) (variable-value ,v))) variables)) bt))
      (lambda () (error "not found")))))

;; (define-clause (parent a b)
;;   (case-unify (a b)
;;               ((luigi marco))
;;               ((marco giuseppe))
;;               ((giuseppe carpoforo))))

;; (define-clause (ancestor a b)
;;   (case-unify (a b)
;;               ((,x ,y) (parent ,x ,y))
;;               ((,x ,y) (parent ,x ,z) (ancestor ,z ,y))))

;; (define-clause (append/3 a b c)
;;   (case-unify (a b c)
;;               (((,x . ,y) ,z (,x . ,w)) (append/3 ,y ,z ,w))
;;               ((() ,r ,r))))

(define-clause (append/3 a b c)
  (<> (let
          (x (newvar 'x)))
          (<- y (newvar 'y))
          (<- z (newvar 'z))
          (<- w (newvar 'w))
          (unify a (cons x y))
          (unify b z)
          (unify c (cons x w))
          (append/3 y z w))
      (>> (<- r (newvar 'r))
          (unify a '())
          (unify b r)
          (unify c r)
          (success))))
  

;; (define x (make-var 'x))
;; (define y (make-var 'y))

(define (find-value v teta)
  (let(
       (x (cdr (assq v teta))))
    (if (variable? x) (find-value x teta) x)))

;; (define (u) (ancestor x y '()
;;                     (lambda (v st cb) (cons (list (find-value x st) (find-value y st)) (cb)))
;;                     (lambda () '())))

(define s (make-var 's))

(define (v) (append/3 '(1 2 3) '(4 5 6) s
                    '()
                    (lambda (v st cb) (subst st s))
                    (lambda () 'not-found)))


(pp (v))