(namespace ("chatroom#"))

(##include "~~/lib/gambit#.scm")
(include "../../gebo-resolver#.scm")
(include "../../gebo-match#.scm")

(define *users* (make-table))

(define *queue* '())

(define-macro (enq m)
  `(set!
    *queue*
    (if (>= (length *queue*) 5)
        (append
         (cdr *queue*)
         (list m))
        (append *queue* (list ,m)))))
  
(define (chat)
  (with-exception-catcher
   (lambda (ex) (chat))
   (lambda ()
     (let(
          (c (thread-receive)))
       (cond
        ((not (table? c))
         (chat))
        
        ((table-ref c "enter" #f)
         => enter-chat)
        
        ((table-ref c "exit" #f)
         => exit-chat)
        
        ((table-ref c "publish" #f)
         => publish)
        
        (else (chat)))))))

(define (broadcast msg)
    (table-for-each
     (lambda (u a)
       (with-exception-catcher
        (lambda (ex)
          (table-set! *users* u))
        (lambda ()
          (pp `(gebo-send ,a ,msg))
          (gebo-send a msg))))
     *users*))

(define (send-queue addr)
  (for-each
   (lambda (m)
     (gebo-send addr (obj ("message" m))))
   *queue*))

(define (success-enter name addr)
  ;; send new user to each connected user
  (broadcast
   (obj ("newUser" (obj ("name" name) ("address" addr)))))
  ;; send welcome message
  (gebo-send addr (obj ("welcome"
                        (string-append "welcome to chatroom," name))))
  ;; send user list
  (gebo-send addr (obj ("users" *users*)))
  
  ;; send queue messages
  (send-queue addr)
  
  (table-set! *users* name addr)
  (chat))

(define (fail-enter name addr)
  (gebo-send addr "fail")
  (chat))

;; {enter: {name: name, address: addr}}
(define (enter-chat c)
  (let(
       (name (table-ref c "name" #f))
       (addr (table-ref c "address" #f)))
       (if (and name addr (not (table-ref *users* name #f)))
           (success-enter name addr)
           (fail-enter name addr))))

(define (exit-success name addr)
  (broadcast (obj ("exitUser" name)))
  (chat))

(define (exit-fail name addr)
  ;; incident will be reported
  (chat))

;;{exit: {name: name, address: addr}}
(define (exit-chat m)
  (let(
       (name (table-ref m "name" #f))
       (addr (table-ref m "address" #f)))
    (if (and name addr
             (equal? (table-ref *users* name) addr))
        (exit-success name addr)
        (exit-fail name addr))))


;; {publish: msg}
(define (publish msg)
  (pp `(publish ,msg))
  (enq msg)
  (broadcast (obj ("message" msg)))
  (chat))


(define *chat*
  (thread-start!
   (make-thread
    chat)))






        