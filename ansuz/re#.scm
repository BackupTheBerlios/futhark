(##namespace ("ansuz-re#" regexp define-regexp))

(define-macro (regexp e . x)
  `(with-state ,x ,(fsm->code (run (re) e))))

  
(define-macro (define-regexp name val)
  `(define-parser (,name) (regexp ,val)))
