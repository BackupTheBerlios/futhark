(load "~~/site-scheme/futhark/ansuz/expressions")

(load "~~/site-scheme/futhark/ehwas/server")
(load "~~/site-scheme/futhark/ehwas/request")
(load "~~/site-scheme/futhark/ehwas/rfc822")
(load "~~/site-scheme/futhark/ehwas/rfc3986")
(load "~~/site-scheme/futhark/ehwas/mime-types")
(load "~~/site-scheme/futhark/ehwas/response")
(load "~~/site-scheme/futhark/ehwas/query")
(load "~~/site-scheme/futhark/ehwas/cookies")
(load "~~/site-scheme/futhark/ehwas/websocket")
(load "~~/site-scheme/futhark/ehwas/combinators")

(load "~~/site-scheme/futhark/gebo/gebo")

(load "~~/site-scheme/futhark/yera/mangle")
(load "~~/site-scheme/futhark/yera/compile")
(load "~~/site-scheme/futhark/yera/parser")
(load "~~/site-scheme/futhark/yera/resolver")

(load "~~/site-scheme/futhark/encode/uids")

(include "~~/site-scheme/futhark/ehwas/server#.scm")
(include "~~/site-scheme/futhark/ehwas/request#.scm")
(include "~~/site-scheme/futhark/ehwas/response#.scm")
(include "~~/site-scheme/futhark/ehwas/query#.scm")
(include "~~/site-scheme/futhark/ehwas/template#.scm")
(include "~~/site-scheme/futhark/ehwas/websocket#.scm")
(include "~~/site-scheme/futhark/ehwas/combinators#.scm")
(include "~~/site-scheme/futhark/ehwas/cookies#.scm")

(include "~~/site-scheme/futhark/gebo/gebo#.scm")
(include "~~/site-scheme/futhark/yera/resolver#.scm")

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


(http-service-register!
 (orelse
  (paths
   ((registry poll) (lambda (req) (json-response req *poll*)))
   (() (lambda (req) (response 200 "OK" (header Content-type: "text/html")
                               (call-with-input-file "poll.html"
                                 (lambda (p) (display (read-line p #f))))))))
  (filesystem "~~/site-scheme/futhark/gebo/js")
  (filesystem "~~/site-scheme/futhark/yera/js")
  gebo-resolver
  (yera ".")
  (filesystem "."))
 
 port-number: 9080)

(thread-sleep! +inf.0)