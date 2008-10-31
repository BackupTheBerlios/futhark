(namespace ("match#" match-once match))
           
(define-macro (match-once e t a)
  (cond
   ((pair? e)
    (let(
         (s (gensym 'e)))
      `(let(
            (,s ,e))
         (match-once ,s ,t ,a))))
        
   ((and (pair? t) (eq? (car t) '?))
    `(let(
          (,(cadr t) ,e))
       ,a))
   
   ((and (pair? t) (eq? (car t) 'as))
    (let(
         (n1 (cadr t))
         (t1 (caddr t)))
      `(let ((,n1 ,e))
         (match-once ,e ,t1 ,a))))

   ((eq? t '_) a)
    
   ((symbol? t)
    `(and (symbol? ,e) (eq? ,e ',t) ,a))

   ((string? t)
    `(and (string? ,e) (string=? ,e ,t) ,a))

   ((number? t)
    `(and (number? ,e) (= ,e ,t) ,a))

   ((pair? t)
    `(and (pair? ,e)
          (match-once (car ,e) ,(car t)
                 (match-once (cdr ,e) ,(cdr t) ,a))))

   ((vector? t)
    `(and (vector? ,e)
          (= (vector-length ,e) ,(vector-length t))
          ,(let loop ((i 0))
             (if (= i (vector-length t)) a
                 `(match-once (vector-ref ,e ,i) ,(vector-ref t i) 
                         ,(loop (+ i 1)))))))
   ((null? t)
    `(and (null? ,e) ,a))
   
   (else
    `(error "wrong pattern"))))

(define-macro (match e #!rest as)
  (cond
   ((pair? e)
    (let(
         (s (gensym 'e)))
      `(let ((,s ,e)) (match ,s ,@as))))
    
   ((null? as) `(error "match failed"))
   (else
    `(or (match-once ,e ,(caar as) (begin ,@(cdar as)))
         (match ,e ,@(cdr as))))))

          