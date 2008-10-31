(##namespace
 ("ehwas-response#"
  make-response
  make-empty-response
  response?
  response-version response-version-set!
  response-code response-code-set!
  response-status response-status-set!
  response-headers response-headers-set!
  response-printer response-printer-set!
  
  response-header-ref
  response-header-set!
  response-append
  writer displayer
  response-write
  header
  body))

(define-macro (header . ps)
  (let(
       (m (gensym 'm)))
    `(let(
          (,m (make-table init: #f)))
       ,@(map (lambda (p)
               `(table-set! ,m ,(car p) ,(cadr p)))
              ps)
       ,m)))

(define-macro (body . a)
  (let(
       (port (gensym 'port)))
  `(lambda (,port)
     (display (list ,@a) ,port))))

 