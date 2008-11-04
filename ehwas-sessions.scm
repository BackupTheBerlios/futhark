(##namespace ("ehwas-sessions#"))

(##include "~~/lib/gambit#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         (not safe)
         )

(define raise-error
  (lambda x (error "sessions not supported")))

(define-structure session-driver init identifier table clean)

(define null-session-driver
  (make-session-driver
   raise-error
   raise-error
   raise-error
   raise-error))
    
(define current-session-driver
  (make-parameter null-session-driver))

(define (session-init #!optional (uid #f))
  ((session-driver-init (current-session-driver)) uid))

(define (session-identifier session)
  ((session-driver-identifier (current-session-driver)) session))

(define (session-table session)
  ((session-driver-table (current-session-driver)) session))

(define (clean-sessions)
  ((session-driver-clean (current-session-driver))))

         
   
  