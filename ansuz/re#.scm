(##namespace ("ansuz-re#" regexp define-regexp))

(define-macro (regexp e)
  (with-exception-catcher
   (lambda (ex)
     (if (error-exception? ex)
         (begin
           `(eval (fsm->code (run (re) ,e))))
         (raise ex)))
   (lambda ()
     (fsm->code (run (re) e)))))
  
(define-macro (define-regexp name val)
  `(define ,name (regexp ,val)))
