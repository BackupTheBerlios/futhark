(##namespace ("ansuz-re#" regexp define-regexp))
;; todo make regexp compatible with parser language
;; i.e
;; (define-parser (assign x)
;;   (>> (<- v (regexp "[a-zA-z][a-zA-Z0-9]*"))
;;       (regexp "[ \t\n]*=[ \t\n]*")
;;       (<- e (value))
;;       (return `(set ,v ,e))))

(define-macro (regexp e . x)
  (with-exception-catcher
   (lambda (ex)
     (if (error-exception? ex)
         `(eval `(with-state ,x ,(fsm->code (run (re) ,e))))
         (raise ex)))
   (lambda ()
     `(with-state ,x ,(fsm->code (run (re) e))))))
  
(define-macro (define-regexp name val)
  `(define-parser (,name) (regexp ,val)))
