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

(include "~~/site-scheme/futhark/gebo/resolver#.scm")

(define *users* (make-table))

(define *emp* (list 'emp))

(define *size* 10)

(define *queue* (make-vector *size* *emp*))

(define *pt* -1)

(define-macro (nxt)
  '(modulo (+ 1 *pt*) *size*))

(define (enq v)
  (set! *pt* (nxt))
  (vector-set! *queue* *pt* v))

(define (queue-for-each fn)
  (let loop ((j 0))
    (cond
     ((>= j *size*) 'ok)
     ((vector-ref *queue* (modulo (+ 1 j *pt*) *size*)) =>
      (lambda (k)
        (if (not (eq? k *emp*)) (fn k))
        (loop (+ j 1)))))))
              
 (define (chat)
  (with-exception-catcher
   (lambda (ex)
     ;; (pp ex)
     (chat))
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
          (gebo-send a msg))))
     *users*))

(define (send-queue addr)
  (queue-for-each
   (lambda (m)
     (gebo-send addr m))))

(define-macro (obj . ps)
  `(list->table
    (list ,@(map (lambda (p) `(cons ,(car p) ,(cadr p))) ps))))

(define (success-enter name addr)
  ;; send new user to each connected user
  (broadcast
   (obj ("newUser" (obj ("name" name) ("address" addr)))))
  ;; send welcome message
  (gebo-send addr (obj ("welcome"
                        (string-append "welcome to chatroom, " name))))
  ;; send user list
  (gebo-send addr (obj ("users" (table-copy *users*))))
  
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
  (let(
       (a0 (table-ref *users* name #f)))
    (if (not (equal? a0 addr))
        (pp '(address not match))
        (begin
          (table-set! *users* name)
          (broadcast (obj ("exitUser" name)))))
    (chat)))
  
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


;; {publish: {"from": name, "body": body}}
(define (publish msg)
  (let(
       (m (obj ("message" msg))))
    (enq m)
    (broadcast m)
    (chat)))


(define *chat*
  (thread-start!
   (make-thread
    chat)))


        
(http-service-register!
 (orelse
  (paths
   ((registry chatroom) (lambda (req) (json-response req *chat*)))
   (() (lambda (req) (response (request-version req) 200 "OK" (header Content-type: "text/html")
                               (call-with-input-file "chatroom.html"
                                 (lambda (p) (display (read-line p #f))))))))
  (filesystem "~~/site-scheme/futhark/gebo/js")
  gebo-resolver
  (filesystem "."))
 
 port-number: 9080)

(thread-sleep! +inf.0)