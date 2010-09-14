
(define-macro (bound? v)
  (with-exception-catcher
   (lambda (ex)
     (if (and (unbound-global-exception? ex) (eq? (unbound-gloable-exception-variable ex) ,(list 'quote v)))
         #f
         (raise ex)))
   (lambda () ,v)))
        
(define-macro (:- h . b)
  `(if (bound? (car h))
       (add-horn h ,@b)
       (define-horn ,h ,@b)))

;; (define *-arity-table-* (make-table))
 
(define-macro (define-horn h . b)
  (let(
       (fp (map (lambda (_) (gensym 'f)) (cdr h))))
  `(define ,(car h) (lambda+ ,fp (logic (>> ,@b))))))

(define-macro (add-horn h . b)
  (let(
       (prev (gensym 'previous))
       (fp (map (lambda (_) (gensym 'f)) (cdr h))))
    (set! ,(car h)
          (let(
               (,prev ,(car h)))
            (lambda+ ,fp (logic (<> (,prev ,@fp) (>> ,@b))))))))
  
                            
      

    
              
  