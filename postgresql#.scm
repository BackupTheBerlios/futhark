;; @author Francesco Bracchi
;; @email frbracch@gmail.com

(##namespace
 ("postgresql#"
  current-connection
  connect
  connect!
  disconnect
  reconnect

  backend-exception?
  backend-exception-fields
  
  set-parser!
  execute
  c
  
  result-status
  result-tuples
  result-notices
  result-names
  result-alists

  with-transaction
  transaction))


(define-macro (transaction . bd)
  `(with-transaction (lambda () ,@bd)))
     