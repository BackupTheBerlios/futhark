(##namespace ("gebo-termite#"))

(##include "~~/lib/gambit#.scm")

(include "resolver#.scm")

(##include "~~/lib/termite/termite#.scm")
(##namespace ("gebo-termite#" ! !? on))

(declare (standard-bindings)
         (extended-bindings)
         (fixnum)
         ;;(not safe)
         (block))

(define ! gebo-send)

(define (!? pid msg . opt)
  (let ((tag (make-tag)))
    (! pid (list (self) tag msg))
    (match opt
           (()
            (recv
             ((,tag reply) reply)))
           
           ((timeout)
            (recv
             ((,tag reply) reply)
             (after timeout (raise 'timeout))))
           
           ((timeout default)
            (recv
             ((,tag reply) reply)
             (after timeout default))))))


(define (on node thunk)
  (let ((tag (make-tag))
        (from (self)))
    (remote-spawn node
                  (lambda ()
                    (! from (list tag (thunk)))))
    (recv
     ((,tag reply) reply))))
