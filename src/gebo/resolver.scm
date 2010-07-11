(##namespace ("gebo-resolver#"))

(##include "~~/lib/gambit#.scm")

(include "json#.scm")

(include "../encode/base64#.scm")
(include "../utils/uids#.scm")
(include "../ehwas/request#.scm")
(include "../ehwas/response#.scm")
(include "../ehwas/resolver#.scm")

(include "../ansuz/language#.scm")
;; (include "rfc3986#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (fixnum)
         ;; (not safe)
         (block))

;; library parameters
;; this is the max length in seconds of a connection
;; this means that if nothing happens within this
;; time the server sends a n empty array signal to
;; the client.
(define gebo-request-length (make-parameter 250))

;; this is the time that the server waits between
;; two calls from the client.
;; in this protocol the connection is of that type:
;; 1) the client calls /gebo/uid;
;; this returns an unique identifier that is used
;; to pass identity through a stateless protocol as HTTP is.
;; Meanwhile the server waits for this parameter seconds
;; if no connections to /gebo/listen from the identified
;; client, all resources are released.
;; the same between a two listen call.

(define gebo-wait-timeout (make-parameter 40))

;; this parameter defines the maximum number of objects that an unique
;; listen request can carry.
(define gebo-max-objects (make-parameter 50))


(define-macro (obj . ps)
  `(list->table
    (list ,@(map (lambda (a b) `(cons ,a ,b))
                 (map car ps)
                 (map cadr ps)))))

(define-macro (sync l . b)
  (let(
       (lok (gensym 'lock))
       (ex (gensym 'ex))
       (r (gensym 'result)))
    `(let(
          (,lok ,l))
       (mutex-lock! ,lok)
       (with-exception-catcher
        (lambda (,ex)
          (mutex-unlock! ,lok)
          (raise ,ex))
        (lambda ()
          (let(
               (,r (begin ,@b)))
            (mutex-unlock! ,lok)
            ,r))))))

(define *-uid->mailbox-* (make-table))
(define *-mailbox->uid-* (make-table))

(define *-mb-lock-* (make-mutex))

(define (uid->mailbox u)
  (sync *-mb-lock-* (table-ref *-uid->mailbox-* u)))

(define (mailbox->uid m)
  (sync *-mb-lock-* (table-ref *-mailbox->uid-* m)))

(define (set-mailbox-uid! u m)
  (sync *-mb-lock-*
        (table-set! *-uid->mailbox-* u m)
        (table-set! *-mailbox->uid-* m u)))

;; (define-structure jid conn-id proc-id)

(define *-pid->uid-* (make-table weak-keys: #t))
(define *-uid->pid-* (make-table weak-values: #t))

(define *-pid-lock-* (make-mutex))

(define (pid->uid t)
  (sync *-pid-lock-*
        (or (table-ref *-pid->uid-* t #f)
            (let(
                 (uid (make-uid)))
              (table-set! *-pid->uid-* t uid)
              (table-set! *-uid->pid-* uid t)
;;               (thread-start!
;;                (make-thread
;;                 (lambda () 
;;                   (thread-join! t)
;;                   (pp `(thread ended ,t ,uid))
;;                   (table-set! *-pid->uid-* t uid)
;;                   (table-set! *-uid->pid-* uid t))))
              uid))))

(define (uid->pid u)
  (sync *-pid-lock-*
        (table-ref *-uid->pid-* u)))


;; behavioral transformations of mailbox as an actor

;; EF -> (put v) -> FF
;; FF -> (put v) -> FF
;; EF -> (get ra) -> WE
;; FF -> (get ra) / emit content -> EF
;; WE -> (put v) -> EF
;; EW -> after timeout / emit '() -> EF

;; EF -> pawn -> EP
;; FF -> pawn -> FP

;; EP -> free -> EF
;; FP -> free -> FF

;; EP -> after timeout -> SD
;; FP -> after timeout -> SD

;; EF : Empty and free
;; EP : Empty and pawned
;; FF : Full (i.e) contains one or more elements) and free
;; FP : Full and pawned
;; SD : Silently Die

;; (get a) where a is the return address,
;; to this address is sent the entire queue
;; (put v) put v in the mailbox queue

;; draw them in a FSM style diagram and could see how it works.

;; (define (silently-die)
;;   (let*(
;;         (mb (current-thread))
;;         (uid (mailbox->uid mb)))
;;     (sync *-mb-lock-*
;;           (table-set! *-uid->mailbox-* uid)
;;           (table-set! *-mailbox->uid-* mb))))

(define (silently-die)
  (sync *-mb-lock-*
        (let(
             (mb (current-thread)))
          (table-set! *-uid->mailbox-* (table-ref *-mailbox->uid-* mb))
          (table-set! *-mailbox->uid-* mb))))

 
(define (pawned-empty-mailbox)
  (let(
       (c (thread-mailbox-next (gebo-wait-timeout) 'die)))
    (cond
     ((eq? c 'free)
      (thread-mailbox-extract-and-rewind)
      (empty-mailbox))
     ((eq? c 'die)
      (silently-die))

     (else 
      (pawned-empty-mailbox)))))

;; (define (pawned-full-mailbox f r)
;;   (let(
;;        (c (thread-receive (gebo-wait-timeout) #f)))
;;     (if (eq? c 'free)
;;         (full-mailbox f r)
;;         (silently-die))))
(define (pawned-full-mailbox f r)
  (let(
       (c (thread-mailbox-next (gebo-wait-timeout) 'die)))
    (cond
     ((eq? c 'free)
      (thread-mailbox-extract-and-rewind)
      (full-mailbox f r))

     ((eq? c 'die)
      (silently-die))

     (else
      (pawned-full-mailbox f r)))))
      

(define (empty-mailbox)
  (let(
       (c (thread-receive)))
    (cond
     ((and (pair? c) (eq? (car c) 'put))
      (let(
           (r (cdr c)))
        (full-mailbox r r)))

     ((and (pair? c) (eq? (car c) 'get))
      (waiting-mailbox (cadr c)))
     
     ((eq? c 'pawn)
      (pawned-empty-mailbox))
     (else
      (empty-mailbox)))))

(define (waiting-mailbox pid)
  (let(
       (c (thread-receive (gebo-request-length) #f)))
    (cond
     ((and (pair? c) (eq? (car c) 'put))
      (thread-send pid (cdr c))
      (empty-mailbox))

     (else
      (thread-send pid '())
      (empty-mailbox)))))

(define (take n l0)
  (let take ((i 0) (l l0) (r '()))
    (cond
     ((null? l) (values l0 '()))
     ((>= i n) (values (reverse r) l))
     (else
      (take (+ i 1) (cdr l) (cons (car l) r))))))

(define (full-mailbox f r)
  (let(
       (c (thread-receive)))
    (cond
     ((and (pair? c) (eq? (car c) 'put))
      (let(
           (r1 (cdr c)))
        (set-cdr! r r1)
        (full-mailbox f r1)))

     ((and (pair? c) (eq? (car c) 'get))
      (receive (h t) (take (gebo-max-objects) f)
        (thread-send (cadr c) h)
        (if (null? t)
            (empty-mailbox)
            (full-mailbox t r))))
            
      ;; (thread-send (cadr c) f)
      ;; (empty-mailbox))

     ((eq? c 'pawn)
      (pawned-full-mailbox f r)))))

(define (make-mailbox)
  (thread-start!
   (make-thread
    empty-mailbox)))

(define (pid->json o)
  (if (scheme-pid? o)
      (obj ("T" "s")
           ("P" (pid->uid o)))
      ;; (obj ("id-type" "scheme")
      ;;      ("process-id" (pid->uid o)))
      o))
;;   (or (and (scheme-pid? o)
;;            (obj ("id-type" "scheme")
;;                 ("process-id" (pid->uid o))))
;;       o))

(define (json->pid o)
  (if (and (table? o)
           (string=? "s" (table-ref o "T" "")))
           ;; (string=? "scheme" (table-ref o "id-type" "")))
      (uid->pid (table-ref o "P" ""))
      ;;(uid->pid (table-ref o "process-id" ""))
      o))
;;   (or (and (table? o)
;;            (string=? (table-ref o "id-type" "") "scheme")
;;            (uid->pid (table-ref o "process-id" "")))
;;       o))

(define (json-response req val)
  (let(
       (str (call-with-output-u8vector
             (list char-encoding: 'UTF-8)
             (lambda (p)
               (json-write val p pid->json)))))
    (make-response
     (request-version req) 200 "OK"
     (header
      ("Pragma" "no-cache")
      ("Cache-Control" "no-cache, must revalidate")
      ("Expires:" "-1")
      ("Access-Control-Allow-Origin" (table-ref (request-header req) "Origin" "*"))
      ("Vary" "Accept-Encoding")
      ("Content-Type" "application/json; charset=UTF-8")
      ("Char-Encoding" "utf-8")
      ("Content-Length" (u8vector-length str)))
     (lambda (p)
       (write-subu8vector str 0 (u8vector-length str) p)))))


;; (define (get-query req)
;;   (url-decode (uri-query (request-uri req))))

(define (gebo-uid req)
  (let(
       (uid (make-uid))
       (mb (make-mailbox)))
    (set-mailbox-uid! uid mb)
    (thread-send mb 'pawn)
    (json-response req uid)))

(define (gebo-listen req)
  (call-with-request
   req
   (lambda ()
     (let(
          (mb (uid->mailbox (table-ref (query) "uid" ""))))
       (thread-send mb 'free)
       (thread-send mb `(get ,(current-thread)))
       (let*(
             (q (thread-receive))
             (res (json-response req q)))
         (thread-send mb 'pawn)
         res)))))
   
;; (define (gebo-notify req)
;;   (let*(
;;         (qry (request-parse-query req))
;;         (ms (json-read
;;              (request-port req)
;;              json->pid)))
;;     (for-each (lambda (m)
;;                 (gebo-send (table-ref m "pid") (table-ref m "message")))
;;               ms)
;;     (json-response (table-ref qry "callback" #f) req #t)))

;; (define (string->u8vector s)
;;   (call-with-output-u8vector
;;    (u8vector)
;;    (lambda (p)
;;      (display s p))))

;; (define (gebo-notify req)
;;   (let*(
;;         (ms (json-read (request-port req) json->pid)))
;;     (for-each
;;      (lambda (m)
;;        (gebo-send (table-ref m "P") (table-ref m "M")))
;;      ms)
;;     (json-response req #t)))

(define (gebo-notify req)
  (for-each
   (lambda (m)
     (gebo-send (table-ref m "P") (table-ref m "M")))
   (json-read (request-port req) json->pid))
  (json-response req #t))

(include "rts.scm")

(define (gebo-rts req)
  (make-response
   (request-version req) 200 "OK"
   (header    
    ("Content-Type" "text/javascript")
    ("Content-Length" *-rts-size-*))
   (lambda (p)
     (write-subu8vector *-rts-data-* 0 *-rts-size-* p))))

(define (gebo-options req)
  (make-response
   (request-version req) 200 "OK"
   (header
    ("Access-Control-Allow-Origin" (table-ref (request-header req) "Origin" ""))
    ("Access-Control-Allow-Methods" "POST, GET, OPTIONS")
    ("Access-Control-Allow-Headers" (table-ref (request-header req) "Access-Control-Request-Headers" ""))
    ("Access-Control-Max-Age" "1728000")
    ("Vary" "Accept-Encoding")
    ("Content-Length" 0))
   (lambda (p) #f)))

(define (gebo-resolver req)
  (let(
       (path (request-path req)))
    (cond
     ((string=? (request-method req) "OPTIONS")
      (gebo-options req))
     
     ((equal? path '("gebo" "uid"))
      (gebo-uid req))
     
     ((equal? path '("gebo" "listen"))
      (gebo-listen req))
     
     ((equal? path '("gebo" "notify"))
      (gebo-notify req))
     
     ((equal? path '("gebo" "rts.js"))
      (gebo-rts req))
     
     (else #f))))

(define-macro (cond-termite a b)
  (define-macro (defined? e)
    `(with-exception-catcher
      (lambda (_) #f)
      (lambda () ,e #t)))
  
  (if (defined? termite#!) a b))
      
(define scheme-pid?
  (cond-termite termite#pid? thread?))

(define (javascript-pid? p)
  (and (table? p)
       (string=? (table-ref p "T" "") "j")))

;; (define (mixed-pid? p)
;;   (and (table? p)
;;        (string=? (table-ref p "id-type" "") "scheme")))

;; (define (mixed-send to msg)
;;   (scheme-send (uid->pid (table-ref to "process-id" "")) msg))

(define scheme-send
  (cond-termite termite#! thread-send))

(define (javascript-send to msg)
  (let(
       (th (uid->mailbox (table-ref to "C" #f))))
    (scheme-send
     th
     `(put ,(obj ("P" to) ("M" msg))))))

(define (gebo-send to msg)
  (cond
   ((scheme-pid? to) (scheme-send to msg))
   ((javascript-pid? to) (javascript-send to msg))
;;    ((mixed-pid? to) (mixed-send to msg))
   (else
    (raise `("wrong address type" ,to)))))

