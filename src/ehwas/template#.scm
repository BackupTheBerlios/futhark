(##namespace ("ehwas-template#" 
	      sgml-template
	      html-template
	      xml-template
	      svg-template
	      xhtml-template
	      html
	      xml
	      svg
	      xhtml
	      ))

(define-macro (sgml-template #!key (empty '()) . e)
  (list
   'quasiquote
   (letrec(
           (ssgml
            (lambda (e)
              (cond
               ((symbol? e) (symbol->string e))
               ((number? e) (number->string e))
               ((string? e) e)
               ((not (pair? e))
                (raise `("syntax error" ,e)))
               ((eq? (car e) 'unquote) e)
               (else
                (let*(
                      (n (car e))
                      (xn (ssgml n))
                      (atr? (and (>= (length e) 2) (pair? (cadr e)) (keyword? (caadr e))))
                      (as (if atr? (cadr e) '()))
                      (cs (if atr? (cddr e) (cdr e))))
                  (list
                   "<" xn
                   (let attributes ((as as) (rs '()))
                     (if (null? as) (reverse rs)
                         (let(
                              (n (car as))
                              (a (cadr as)))
                           (attributes (cddr as)
                                 (cons (list " " (keyword->string (car as)) "=\"" (ssgml (cadr as)) "\"") rs)))))
                   (cond
                    ((member n empty) ">")
                    ((null? cs) "/>")
                    (else
                     (list ">" (map ssgml cs) "</" xn ">")))))))))
           (flatten
            (lambda (ss)
              (cond
               ((null? ss) '())
               ((and (pair? (car ss)) (eq? (caar ss) 'unquote))
                (cons (car ss) (flatten (cdr ss))))
               ((pair? (car ss))
                (append
                 (flatten (car ss))
                 (flatten (cdr ss))))
               ((null? (car ss))
                (flatten (cdr ss)))
               (else
                (cons (car ss) (flatten (cdr ss)))))))

           (simplify
            (lambda (ss)
              (if (null? ss) '()
                  (let (
                        (a (car ss))
                        (as (simplify (cdr ss))))
                    (cond
                     ((null? as) ss)
                     ((and (string? a) (string? (car as)))
                      (cons (string-append a (car as)) (cdr as)))
                     (else
                      (cons a as))))))))
     
     (map (lambda (s) (if (string? s) `(,'unquote (##still-copy ,s)) s))
          (simplify (flatten (map ssgml e)))))))


(define-macro (html-template . e)
  `(sgml-template empty: (area base basefont br col frame hr img input isindex link meta param) ,@e))

(define-macro (xml-template . e)
  `(sgml-template empty: () ,@e))

(define-macro (svg-template . e)
  `(xml-template ,@e))

(define-macro (xhtml-template e)
  `(xml-template ,@e))

(define-macro (html . e)
  `(print (html-template (html ,@e))))

(define-macro (xml . e)
  `(print (xml-template ,@e)))

(define-macro (svg . e)
  `(print (svg-template ,@e)))

(define-macro (xhtml . e)
  `(print (xhtml-template ,@e)))