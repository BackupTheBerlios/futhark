(##namespace ("ehwas-sessions-pg-sessions#"))

(##include "~~/lib/gambit#.scm")
(include "../../database/postgresql#.scm")
(include "../sessions#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         ;,(not safe)
         )

(define-structure pg-session connection identifier table)

(define pg-session-dbname (make-parameter ""))
(define pg-session-user (make-parameter ""))
(define pg-session-password (make-parameter ""))

(define (pg-new-uid con)
  (caar
   (result-tuples
    (execute
     "SELECT new_session ()" con))))

(define (pg-get-table uid con)
  (let*(
        (res (execute `("SELECT get_session (" ,(c uid) ")") con))
        (ts (caar (result-tuples res))))
    (if (null? ts) (raise *-expired-*)
        (u8vector->object ts))))

(define (pg-save uid tbl con)
  (execute
   `("SELECT save_session (" ,(c uid) "," ,(c tbl) ")")
   con))

(define (session-will s)
  (make-will s
             (lambda (_)
               (pg-save (pg-session-identifier s)
                        (pg-session-table s)
                        (pg-session-connection s)))))

(define *-expired-* (list 'expired))

(define (hard-get-session uid)
  (with-exception-catcher
   (lambda (ex)
     (if (eq? ex *-expired-*) (hard-new-session)
         (raise ex)))
   (lambda ()
     (let*(
           (con (connect (pg-session-dbname)
                         (pg-session-user)
                         (pg-session-password)))
           (tbl (pg-get-table uid con)))
       (make-pg-session con uid tbl)))))

(define (hard-new-session)
  (let*(
        (con (connect (pg-session-dbname)
                      (pg-session-user)
                      (pg-session-password)))
        (uid (pg-new-uid con)))
    (make-pg-session con uid (make-table))))

(define *-session-cache-*
  (make-table test: string=? init: #f weak-keys: #t  weak-values: #t))

(define (put-cache s)
  (table-set!
   *-session-cache-*
   (pg-session-identifier s)
   (session-will s))
  s)

(define (lookup-cache uid)
  (let(
       (c (table-ref *-session-cache-* uid)))
    (and c (will-testator c))))

(define (get-session uid)
  (or (lookup-cache uid)
      (put-cache (hard-get-session uid))))

(define (new-session)
  (put-cache (hard-new-session)))

(define (pg-session-init #!optional (uid #f))
   (if uid
       (get-session uid)
       (new-session)))

(define *-clean-con-* '())

(define (pg-clean-sessions)
  (if (null? *-clean-con-*)
      (set! *-clean-con-* (connect (pg-session-dbname)
                                   (pg-session-user)
                                   (pg-session-password))))
  (execute "DELETE FROM expired_sessions" *-clean-con-*))

(define *-del-* (list 'del))

(define (pg-session-set! sess key #!optional (val *-del-*))
  (if (eq? val *-del-*)
       (table-set! (pg-session-table sess) key)
       (table-set! (pg-session-table sess) key val)))

(define (pg-session-ref sess key #!optional (val *-del-*))
  (if (eq? val *-del-*)
      (table-ref (pg-session-table sess) key)
      (table-ref (pg-session-table sess) key val)))

(define pg-session-driver
  (make-session-driver
   pg-session-init
   pg-session-identifier
   pg-session-set!
   pg-session-ref
   pg-clean-sessions))

