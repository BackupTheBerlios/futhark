(##namespace
 ("ehwas-response#"
  make-response
  response?
  response-version response-version-set!
  response-code response-code-set!
  response-status response-status-set!
  response-header response-header-set!
  response-writer response-writer-set!
  response
  header

  write-http-response
  ))

(define-macro (header . ps)
  `(list ,@(let loop ((ps ps) (rs '()))
	     (cond
	      ((null? ps) (reverse rs))
	      ((symbol? (car ps)) (loop (cddr ps) (cons `(cons ,(list 'quote (car ps)) ,(cadr ps)) rs)))
	      ((string? (car ps)) (loop (cons (string->symbol (car ps)) (cdr ps)) rs))
	      ((keyword? (car ps)) (loop (cons (keyword->string (car ps)) (cdr ps)) rs))
	      (else (error "syntax error in header macro"))))))

(define-macro (response c s h . w)
  `(make-response ,c ,s ,h (lambda () ,@w)))