(##namespace ("laguz-variables#"

              make-variable
              variable?
              variable-get
              variable-set!
              variable-reset!
              ))


(##namespace ("laguz#" *-var-*))

(define-macro (make-variable)
  `(cons *-var-* #!void))

(define-macro (variable-get v)
  `(cdr ,v))

(define-macro (variable-set! v vv)
  `(set-cdr! ,v ,vv))

(define-macro (variable-reset! v)
  `(variable-set! ,v #!void))

(define-macro (variable? v)
  `(and (pair? ,v) (eq? (car ,v) *-var-*)))
