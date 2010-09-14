(##namespace
 ("prolog-reflect#"
  with-state
  reflect
  reify
  run-monad))

(define-macro (with-state x c)
  (append c x))

(define-macro (reflect v b . x)
  `(let ,(map list v x) ,b))

(define-macro (reify vm val . x)
  `(with-state
    ,x
    ,(let subst ((v (car vm))
                 (m (cadr vm))
                 (val val))
       (cond
        ((not (pair? val))
         val)
        
        ((eq? (car val) v)
         (append m (subst v m (cdr val))))
        
        (else
         (cons (subst v m (car val))
               (subst v m (cdr val))))))))

;; (define-macro (reify vm val . x)
;;   (let(
;;        (x1 (gensym 'x)))
;;   `(let ()
;;      (define-macro (,(car vm) . ,x1)
;;        `(with-state ,,x1 ,,(cdr vm)))
;;      (with-state ,x ,val))))

(define-macro (run-monad p . x)
  `(with-state ,x ,p))
