(namespace
 ("gebo-match#"
  match
  match-once
  obj))

(define-macro (obj . ps)
  `(list->table
    (list ,@(map (lambda (a b) `(cons ,a ,b))
                 (map car ps)
                 (map cadr ps)))))

(define-macro (match-once e v s f)
  
  (define obj?
    (lambda (s)
      (and (pair? s)
           (eq? (car s) 'obj))))
  
  (define qsymbol?
    (lambda (s)
      (and (pair? s)
           (eq? (car s) 'quote))))
  
  (define match
    (lambda (e v s f)
      (cond
       ((eq? e '_) s)
       
       ((symbol? e)
        `(let ((,e ,v)) ,s))

       ((qsymbol? e)
        (match-symbol e v s f))
       
       ((obj? e)
        `(if (table? ,v)
             ,(match-table (cdr e) v s f)
             ,f))
       
       ((pair? e)
        `(if (pair? ,v)
             ,(match-pair e v s f)
             ,f))
       
       ((string? e)
        `(if (and (string? ,v) (string=? ,v ,e)) ,s ,f))

       ((number? e)
        `(if (and (number? ,v) (= ,v ,e)) ,s ,f))
       
       ((null? e)
        `(if (null? ,v) ,s ,f))

       (else (error "unknown match" e)))))

  (define match-symbol
    (lambda (e v s f)
      `(if (eq? ,e ,v) ,s ,f)))
  
  (define match-table
    (lambda (as v s f)
      (if (null? as) s
          (let(
               (ex (gensym 'ex))
               (nx (gensym 'nx)))
            `(with-exception-catcher
              (lambda (,ex)
                (if (unbound-table-key-exception? ,ex)
                    ,f
                    (raise ,ex)))
              (lambda ()
                (let(
                     (,nx (table-ref ,v ,(caar as))))
                  ,(match (cadar as) nx
                          (match-table (cdr as) v s f)
                          f))))))))

  (define (match-pair e v s f)
    (let(
         (a (gensym 'kar))
         (d (gensym 'kdr)))
      `(let(
            (,a (car ,v)))
         ,(match (car e) a
                 (list
                  `(let(
                        (,d (cdr ,v)))
                     ,(match (cdr e) d s f)))
                 f))))
  (let(
       (return (gensym 'return))
       (fail (gensym 'fail)))
    
    `(call-with-current-continuation
      (lambda (,return)
        (call-with-current-continuation 
         (lambda (,fail)
           (,return ,(match e v s `(,fail '())))))
        ,f))))

(define-macro (match k . cs)
  (let(
       (kk (gensym 'kk)))
    `(let ((,kk ,k))
       ,(let loop ((cs cs))
          (if (null? cs) `(error "match failed")
              `(match-once
                ,(caar cs)
                ,kk
                (begin ,@(cdar cs))
                ,(loop (cdr cs))))))))


