(load "fgi")
(include "fgi#.scm")

(file-session-dir "sessions")
;; (pg-session-dbname "sessions")
;; (pg-session-user "francesco")

(current-session-driver file-session-driver)
;; (ehwas-pages-compiler "gsc-gambit")
(ehwas-pages-compile? #t)

(define *-registry-* (make-table))

(define registry-resolver 
  (with-prefix-resolver '("registry") (make-table-resolver *-registry-*)))

(define clean-session-timeout (make-parameter 60))

(thread-start!
 (make-thread
  (lambda ()
    (let loop ()
      (##gc) ;; makes sure unreferenced sessions are saved
      (clean-sessions)
      (thread-sleep! (clean-session-timeout))
      (loop)))))

(define (last n)
  (if (null? (cdr n)) (car n)
      (last (cdr n))))

(define (deny-extensions xs res)
  (deny
   (lambda (req)
     (let*(
           (path (request-path req))
           (x (if (null? path) "" (path-extension (last path)))))
       (member x xs)))
   res))

(define (allow-extensions xs res)
  (allow
   (lambda (req)
     (let*(
           (path (request-path req))
           (x (if (null? path) "" (path-extension (last path)))))
       (member x xs)))
   res))

(define (serve-dir d)
  (let(
       (fsr
        (deny-extensions
         '(".scm" ".o1" ".ehwas" ".yera")
         (make-filesystem-resolver d)))
       (spr
        (allow-extensions
         '(".ehwas")
         (make-serverpage-resolver d)))
       (yrr
        (allow-extensions
         '(".js" ".yera")
         (make-yera-resolver d))))
    (orelse-resolver
     yrr
     (with-index-resolver "index.ehwas" spr)
     (with-index-resolver "index.html" fsr)
     (with-index-resolver "index.htm" fsr)
     spr
     fsr)))
     

(define server
  (make-server
   #f 1080
   (make-guarded-resolver
    (orelse-resolver
     (serve-dir "www")
     gebo-resolver
     registry-resolver
     not-found-resolver))))

(define (echo)
  (let(
       (c (thread-receive)))
    (cond
     ((eq? c 'die) 'ok)
     ((pair? c)
      (gebo-send (car c) (string-append "this is the echo scheme actor: " (cadr c)))
      (echo))
     (else
      (echo)))))

(define *echo*
  (thread-start!
   (make-thread echo)))

(table-set! *-registry-* '("echo")
            (lambda (r) (json-response r *echo*)))


(load "www/chatroom/chatroom.scm")

(table-set! *-registry-* '("chatroom")
            (lambda (r) (json-response r chatroom#*chat*)))

(load "www/poll/poll.scm")

(table-set! *-registry-* '("poll")
            (lambda (r) (json-response r poll#*poll*)))



(pp 'starting)
(start! server)
