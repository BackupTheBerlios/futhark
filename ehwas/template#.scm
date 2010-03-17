(##namespace ("sxml#" sgml html xml block image))

;; (define-macro (sxml e)
;;   (cond
;;    ((symbol? e) `(sxml ,(symbol->string e)))
;;    ((number? e) (number->string e))
;;    ((string? e) e)
;;    ((not (pair? e))
;;     (raise `("syntax error" ,e)))
;;    ((eq? (car e) 'unquote) (cadr e))
;;    (else
;;     (let*(
;;           (n (car e))
;;           (atr? (and (pair? (cdr e)) (pair? (cadr e)) (eq? (caadr e) '@)))
;;           (a (if atr? (cdadr e) '()))
;;           (cs (if atr? (cddr e) (cdr e))))
;;       (list
;;        'quasiquote
;;        (append
;;         (list "<" (list 'unquote (list 'sxml n)))
;;         (let loop ((a a))
;;           (if (null? a) '()
;;               (append 
;;                (list " " (list 'unquote (list 'sxml (caar a)))
;;                      "=\""
;;                      (list 'unquote (list 'sxml (cadar a))) "\"")
;;                (loop (cdr a)))))
;;         (if (and (null? cs) (not (eq? n 'script)))
;;             (list "/>")
;;             (append
;;              (list ">")
;;              (map (lambda (c)
;;                     (list 'unquote (list 'sxml c)))
;;                   cs)
;;              (list "</" (list 'unquote (list 'sxml n)) ">")))))))))


;; (define-macro (image fn)
;;   (let(
;;        (prt (gensym 'prt)))
;;     `(lambda (,prt)
;;        ,(call-with-input-file fn
;;           (lambda (p)
;;             (let*(
;;                   (siz (file-size fn))
;;                   (buf (make-u8vector siz)))
;;               (read-subu8vector buf 0 siz p)
;;               `(write-subu8vector (quote ,buf) 0 ,siz ,prt)))))))

(define-macro (image file)
  (let(
       (port (gensym 'port)))
    `(lambda (,port)
       (for-each
        (lambda (b) (write-subu8vector b 0 (u8vector-length b) ,port))
        (list
         ,@(call-with-input-file file
             (lambda (p)
               (let loop ()
                 (let*(
                       (buf (make-u8vector 65536))
                       (len (read-subu8vector buf 0 65536 p)))
                  (if (< len 65536)
                      (list `(##still-copy (quote ,(subu8vector buf 0 len))))
                      (begin
                        (cons
                         `(##still-copy (quote ,buf))
                         (loop)))))))))))))

(define-macro (block file)
  `(list
    ,@(call-with-input-file file
        (lambda (p)
          (let loop ()
            (let*(
                  (buf (make-string 65536))
                  (len (read-substring buf 0 65536 p)))
              (if (< len (string-length buf))
                  (list `(##still-copy ,(substring buf 0 len)))
                  (begin
                    (cons
                     `(##still-copy ,buf)
                     (loop))))))))))

(define-macro (sgml #!key (empty '()) . e)
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
                      (atr? (and (pair? (cdr e)) (pair? (cadr e)) (eq? (caadr e) '@)))
                      (as (if atr? (cdadr e) '()))
                      (cs (if atr? (cddr e) (cdr e))))
                  (list
                   "<" xn
                   (map (lambda (a)
                          (list " " (ssgml (car a)) "=\"" (map ssgml (cdr a)) "\""))
                        as)
                   (if (member n empty)
                       "/>"
                       (list ">" (map ssgml cs) "</" xn ">"))))))))
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


(define-macro (html . e)
  `(sgml empty: (area base basefont br col frame hr img input isindex link meta param) ,@e))

(define-macro (xml . e)
  `(sgml empty: () ,@e))
