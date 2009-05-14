(##namespace ("uids#"))
(##include "~~/lib/gambit#.scm")

(declare
  (standard-bindings)
  (extended-bindings)
  (block)
  (not safe)
  (fixnum)
  )

(define *-uids-* (make-table init: #f weak-keys: #t))
(define *-uid-max-* (expt 2 32))
(define *-mutex-* (make-mutex))

(define (new-uid)
  (let(
        (tm (inexact->exact
             (round
              (time->seconds
               (current-time)))))
       (rd0 (random-integer *-uid-max-*))
       (rd1 (random-integer *-uid-max-*))
       (rd2 (random-integer *-uid-max-*))
       (rd3 (random-integer *-uid-max-*)))
    (string-append
     (number->string (bitwise-xor tm rd0) 16) ; ":"
     (number->string (bitwise-xor tm rd1) 16) ; ":"
     (number->string (bitwise-xor tm rd2) 16) ; ":"
     (number->string (bitwise-xor tm rd3) 16))))

(define (make-uid)
  (let(
       (u (new-uid)))
    (mutex-lock! *-mutex-*)
    (if (table-ref *-uids-* u)
        (make-uid)
        (begin
          (table-set! *-uids-* u #t)
          (mutex-unlock! *-mutex-*)
          u))))
