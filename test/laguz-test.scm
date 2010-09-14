(include "~~/site-scheme/futhark/laguz/laguz.scm")


(include "~~/site-scheme/futhark/laguz/laguz#.scm")


(laguz 
 (:- (list-append () U U))
 (:- (list-append (A . X) Y (A . Z)) (list-append X Y Z))

 (:- (father-child 'a 'b))
 (:- (father-child 'b 'c))
 (:- (father-child 'c 'd))
 
 (:- (ancestor A B) (father-child A X) (ancestor X B))
 (:- (ancestor A B) (father-child A B)))

(define (to-list r)
  (if (null? r) '()
      (cons (car r) (to-list ((cdr r))))))

(define (times n fn)
  (if (>= n 0)
      (begin
        (fn)
        (times (- n 1) fn))))

;; display results
(pp '(laguz 
      (:- (list-append () U U))
      (:- (list-append (A . X) Y (A . Z)) (list-append X Y Z))
      
      (:- (father-child 'a 'b))
      (:- (father-child 'b 'c))
      (:- (father-child 'c 'd))
      
      (:- (ancestor A B) (father-child A B))
      (:- (ancestor A B) (ancestor X B) (father-child A X)))
    (current-output-port))

(newline)
(pp `(?- (ancestor X 'd)) (current-output-port))
(for-each
 (lambda (as)
   (for-each (lambda (a) (for-each display (list (car a) "=" (cdr a) "\n"))) as)
   (newline))
 (to-list (?- (ancestor X 'd))))
(newline)

(pp `(?- (list-append X Y '(1 2 3 4 5 6))) (current-output-port))
(for-each
 (lambda (as)
   (for-each (lambda (a) (for-each display (list (car a) "=" (cdr a) "\n"))) as)
   (newline))
 (to-list (?- (list-append X Y '(1 2 3 4 5 6)))))
