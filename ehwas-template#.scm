(##namespace ("sxml#" xml block image))

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


;; insert a file as a string
(define-macro (block fn)
  (call-with-input-file fn
    (lambda (p)
      (read-line p #\nul))))

(define-macro (image fn)
  (let(
       (prt (gensym 'prt)))
    `(lambda (,prt)
       ,(call-with-input-file fn
          (lambda (p)
            (let*(
                  (siz (file-size fn))
                  (buf (make-u8vector siz)))
              (read-subu8vector buf 0 siz p)
              `(write-subu8vector (quote ,buf) 0 ,siz ,prt)))))))

(define-macro (xml e)
  (list
   'quasiquote
   (letrec(
           (sxml
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
                      (xn (sxml n))
                      (atr? (and (pair? (cdr e)) (pair? (cadr e)) (eq? (caadr e) '@)))
                      (as (if atr? (cdadr e) '()))
                      (cs (if atr? (cddr e) (cdr e))))
                  (list
                   "<" xn
                   (map (lambda (a)
                          (list " " (sxml (car a)) "=\"" (sxml (cadr a)) "\""))
                        as)
                   (if #f ;; (and (null? cs) (not (eq? n 'script)))
                       "/>"
                       (list ">" (map sxml cs) "</" xn ">"))))))))
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
                    
               
     (simplify (flatten (sxml e))))))