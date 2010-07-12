(##namespace ("ansuz-sources#"
              source-car source-kar
              source-cdr source-kdr
              source? kource?
              source-row source-kow
              source-col source-kol
              source-pos source-kos

              source-append
              function->source
              port->source
              string->source
              vector->source
              list->source
              ->source
              ))

(define-macro (source-car e)
  `(source-kar (force ,e)))

(define-macro (source-cdr e)
  `(source-kdr (force ,e)))

(define-macro (source-row e)
  `(source-kow (force ,e)))

(define-macro (source-col e)
  `(source-kol (force ,e)))

(define-macro (source-pos e)
  `(source-kos (force ,e)))

(define-macro (source? e)
  `(kource? (force ,e)))

               
              