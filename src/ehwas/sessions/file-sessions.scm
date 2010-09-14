(##namespace ("ehwas-sessions-file-sessions#"))

(##include "~~/lib/gambit#.scm")

(include "../../encode/uids#.scm")
(include "../sessions#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         ;;(not safe)
         )

(define file-session-dir (make-parameter #f))
(define file-session-max-age (make-parameter (* 60 60)))

(define-structure file-session identifier table)

(define (expired? f)
  (let(
       (t0 (time->seconds (file-last-modification-time f)))
       (t1 (time->seconds (current-time))))
    (> (- t1 t0) (file-session-max-age))))
  
(define (file-save uid tbl)
  (let(
       (f (string-append (file-session-dir) "/" uid ".sss"))
       (img (object->u8vector tbl)))
    (call-with-output-file f
      (lambda (p)
        (write-subu8vector img 0 (u8vector-length img) p)))))

(define *-expired-* (list 'expired))

(define (file-get-table uid)
  (let(
       (f (string-append (file-session-dir) "/" uid ".sss")))
    (if (or (not (file-exists? f))
            (expired? f))
        (raise *-expired-*)
        (let(
             (img (make-u8vector (file-size f))))
          (call-with-input-file f
            (lambda (p)
              (read-subu8vector img 0 (u8vector-length img) p)
              (u8vector->object img)))))))

(define *-session-cache-*
  (make-table test: string=? init: #f weak-keys: #t  weak-values: #t))

(define (put-cache s)
  (table-set! *-session-cache-* (file-session-identifier s) (session-will s))
  s)

(define (hard-get-session uid)
  (with-exception-catcher
   (lambda (ex)
     (if (eq? ex *-expired-*) (hard-new-session)
         (raise ex)))
   (lambda ()
     (make-file-session uid (file-get-table uid)))))

(define (hard-new-session)
  (make-file-session (make-uid) (make-table)))

(define (session-will s)
  (make-will
  s
  (lambda (_)
    (file-save (file-session-identifier s) (file-session-table s)))))
   
(define (lookup-cache uid)
  (let(
       (c (table-ref *-session-cache-* uid)))
    (and c (will-testator c))))

(define (get-session uid)
  (or (lookup-cache uid)
      (put-cache (hard-get-session uid))))

(define (new-session)
  (put-cache (hard-new-session)))

(define (file-session-init #!optional (uid #f))
  (if uid
      (get-session uid)
      (new-session)))

(define (file-clean-sessions)
  (let(
       (p (open-directory (file-session-dir))))
    (let loop ()
      (let(
           (file (read p)))
        (if (string? file)
            (begin
              (and (expired? file)
                   (delete-file (string-append (file-session-dir) "/" file)))
              (loop)))))))


(define *-del-* (list 'del))

(define (file-session-set! sess key #!optional (val *-del-*))
  (if (eq? val *-del-*)
       (table-set! (file-session-table sess) key)
       (table-set! (file-session-table sess) key val)))

(define (file-session-ref sess key #!optional (val *-del-*))
  (if (eq? val *-del-*)
      (table-ref (file-session-table sess) key)
      (table-ref (file-session-table sess) key val)))

(define file-session-driver
  (make-session-driver
   file-session-init
   file-session-identifier
   file-session-set!
   file-session-ref
   file-clean-sessions))