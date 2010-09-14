(define-macro+ (logic e)
  (cond
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
   
   ((and (pair? e) (eq? (car e) 'if))
    `(if+ ,(cadr e) (logic ,(caddr e)) (logic ,(cadddr e))))
   
   ((and (pair? e) (eq? (car e) 'when))
    `(when+ ,(cadr e) (logic (>> ,@(cddr e)))))
   
   ((and (pair? e) (eq? (car e) 'let))
    `(let+ ,(cadr e) (logic (>> ,@(cddr e)))))
   
   ((and (pair? e) (eq? (car e) 'let*))
    `(let*+ ,(cadr e) (logic (>> ,@(cddr e)))))
      
   ((and (pair? e) (eq? (car e) 'letrec))
    `(letrec+ ,(cadr e) (logic (>> ,@(cddr e)))))
   
   (else e)))

(define-macro (relation f . b)
  `(lambda+ ,f (logic (>> ,@b))))

(define-macro (define-relation h . b)
  `(define ,(car h) (rel ,(cdr h) ,@b)))