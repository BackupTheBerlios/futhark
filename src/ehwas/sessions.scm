(##namespace ("ehwas-sessions#"))

(##include "~~/lib/gambit#.scm")
(include "cookies#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         ;;(not safe)
         )
(define raise-error
  (lambda x (error "sessions not supported")))

(define-structure session-driver init identifier set ref clean)

(define null-session-driver
  (make-session-driver
   raise-error
   raise-error
   raise-error
   raise-error
   (lambda x '())))
    
(define current-session-driver
  (make-parameter null-session-driver))

(define (session-init #!optional (uid #f))
  ((session-driver-init (current-session-driver)) uid))

(define (session-identifier session)
  ((session-driver-identifier (current-session-driver)) session))

(define *-del-* (list 'del))

(define (session-set! session key #!optional (val *-del-*))
  (if (eq? val *-del-*)
      ((session-driver-set (current-session-driver)) session key)
      ((session-driver-set (current-session-driver)) session key val)))

(define (session-ref session key #!optional (val *-del-*))
  (if (eq? val *-del-*)
      ((session-driver-ref (current-session-driver)) session key)
      ((session-driver-ref (current-session-driver)) session key val)))
      
;; (define (session-table session)
;;   ((session-driver-table (current-session-driver)) session))

(define (clean-sessions)
  ((session-driver-clean (current-session-driver))))

(define clean-session-timeout (make-parameter (* 30 60))) ;; 30 minutes

(define clean-sessions-thread
  (thread-start!
   (make-thread
    (lambda ()
      (let loop ()
        (thread-sleep! (clean-session-timeout))
        ((session-driver-clean (current-session-driver)))
        (loop))))))

(define (make-request-session request)
  (let*(
        (cookies (request-cookies request))
        (session-id (and cookies (table-ref cookies 'Session-id #f)))) ;; todo modify cookies so that keys are symbols
    (session-init session-id)))

(define *-memo-* (make-table weak-keys: #t))

(define (request-session request)
  (or (table-ref *-memo-* request #f)
      (let(
           (session (make-request-session request)))
        (table-set! *-memo-* request session)
        session)))

;; (define session (make-parameter #f))

;; (define (with-session handler)
;;   (lambda (req)
;;     (let*(
;; 	  (cookies (request-cookies req))
;; 	  (session-id0 (and cookies (table-ref cookies 'Session-id #f))))
;;       (if session-id0
;; 	  (let(
;; 	       (sess (request-session req)))
;; 	    (parameterize 
;; 	     ((session sess))
;; 	     (let(
;; 		  (res (handler req)))
;; 	       (if (not (equal? (session-identifier sess) session-id0))
;; 		   (response-cookie-set res 'Session-id (session-id sess))
;; 		   res))))))))

(define (sess thunk)
  (lambda (req)
    (let*(
	  (cookies (request-cookies req))
	  (session-id0 (and cookies (table-ref cookies 'Session-id #f))))
      (if session-id0
	  (let*(
		(sess (request-session req))
		(res (thunk req sess)))
	    (if (not (equal? (session-identifier sess) session-id0))
		(response-cookie-set res 'Session-id (session-id sess))
		res))))))

(define session (make-parameter #f))

(define (with-session handler)
  (sess (lambda (req sess)
	  (parameterize 
	   ((session sess))
	   (handler req)))))
