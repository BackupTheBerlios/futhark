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
  response
  header
  text))

(define-macro (header . ps)
  (let(
       (m (gensym 'm)))
    `(let(
          (,m (make-table init: #f)))
       ,@(map (lambda (p)
               `(table-set! ,m ,(if (symbol? (car p)) (symbol->string (car p)) (car p)) ,(cadr p)))
              ps)
       ,m)))

(define-macro (text . a)
  (let(
       (port (gensym 'port)))
  `(lambda (,port)
     (print port: ,port (list ,@a)))))

(define-macro (response x y z h b)
  (let(
       (bd (gensym 'body))
       (hd (gensym 'head))
       (ln (gensym 'leng))
       (pt (gensym 'port)))
    `(let*(
           (,hd ,h)
           (,bd (call-with-output-u8vector (list init: (u8vector) char-encoding: 'UTF-8) ,b))
           (,ln (u8vector-length ,bd)))
       (table-set! ,hd "Content-length" ,ln)
       (make-response ,x ,y ,z ,hd
                      (lambda (,pt)
                        (write-subu8vector ,bd 0 ,ln ,pt)
                        (force-output ,pt))))))