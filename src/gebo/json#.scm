(##namespace
 ("gebo-json#"
  json
  obj
  :
  json-read
  json-write))

;; this is a handy macro for generate tables
;; example:
;; (define u
;;   (obj
;;    a: 10
;;    b: 30))

(define-macro (obj . fs)
  `(list->table
    (,(quote quasiquote)
     ,(let loop ((fs fs) (rs '()))
        (if (null? fs) (reverse rs)
            (loop (cddr fs)
                  (cons
                   (cons (keyword->string (car fs)) (list 'unquote (cadr fs)))
                   rs)))))))

;; the same of table-ref but with json semantics 
(define-macro (: t v)
  `(table-ref ,t ,(if (symbol? v) (symbol->string v) v) '()))

         
    
          


   