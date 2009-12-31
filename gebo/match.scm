(define (unify-const a b bs)
  (and (eq? a b) bs))

(define (unify-var a b bs)
  (let(
       (n (substring a 1 (string-length a))))
    (cond
     ((table-ref bs n #f) =>
      (lambda (x) (unify b x bs)))
     (else
      (table-set! bs n b)
      bs))))

(define (unify-list a b bs)
  (and
   (pair? b)
   (let(
        (bs1 (unify (car a) (car b))))
     (if bs1
         (unify (cdr a) (cdr b) bs1)))))

(define und (vector 'undefined))

(define (unify-table a b bs)
  (and
   (table? b)
   (let(
        (r bs))
     (table-for-each (lambda (k v)
                       (set! r (unify (table-ref a 
      
      