(namespace ("poll#"))

(##include "~~/lib/gambit#.scm")
(include "../../gebo/resolver#.scm")

(define (remove us u)
  (cond
   ((null? us) '())
   ((eq? (car us) u) (cdr us))
   (else (cons (car us) (remove (cdr us) u)))))

(define *title* "manamana!")

(define *question* "Do you like manamana! song?")

(define *choices* '("Yes" "No" "I've never heard it"))

(define *users* '())

(define *result*
  (list->table
   (map (lambda (c) (cons c 0)) *choices*)))

(define-macro (adduser u)
  `(set! *users* (cons ,u *users*)))

(define (deluser u)
  `(set! *users* (remove *users* , u)))
  
(define (poll)
  (with-exception-catcher
   (lambda (ex) (poll))
   (lambda ()
     (let(
          (c (thread-receive)))
       (cond
        ((not (table? c)) (poll))
        ((table-ref c "register" #f) =>
         poll-register)
        
        ((table-ref c "unregister" #f) =>
         poll-unregister)
        
        ((table-ref c "choice" #f) =>
         poll-choice)
        
        (else (poll)))))))


(define (poll-register u)
  ;; (pp `(poll-register ,u))
  (gebo-send u (list->table
                `(
                  ("title" . ,*title*)
                  ("question" . ,*question*)
                  ("result" . ,*result*))))
  (adduser u)
  (poll))

(define (poll-unregister u)
  ;; (pp `(poll-unregister ,u))
  (deluser u)
  (poll))

(define (poll-choice x)
  ;; (pp `(poll-choice ,x))
  (let(
       (n1 (+ 1 (table-ref *result* x))))
    (table-set! *result* x n1)
    (broadcast (list->table
                `(("change" . ,x)
                  ("votes" . ,n1))))
    (poll)))

(define (broadcast m)
  (for-each
   (lambda (u)
     (with-exception-catcher
      (lambda (ex) (deluser u))
      (lambda () (gebo-send u m))))
   *users*))

(define *poll*
  (thread-start!
   (make-thread
    poll)))