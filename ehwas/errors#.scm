(##namespace
 ("ehwas-errors#"
  *errors*
  http-error
  define-error))

; (define-syntax define-error
;   (lambda (s) 
;     (syntax-case s ()
;       ((_ ?n ?l)
;        (syntax 
;         (table-set! *errors* ?n ?l))))))

; (define-syntax http-error
;   (lambda (s)
;     (syntax-case s ()
;       ((_ ?n ?v) (syntax ((table-ref *errors* ?n) ?v))))))

(define-macro (define-error n l)
  `(table-set! *errors* ,n ,l))

(define-macro (http-error n . r)
  `((table-ref *errors* ,n) ,@r))
