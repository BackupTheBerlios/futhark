(##namespace ("gebo-resolver#"))

(##include "~~/lib/gambit#.scm")

(include "gebo-json#.scm")
(include "base64#.scm")
(include "uids#.scm")
(include "ehwas-request#.scm")
(include "ehwas-response#.scm")

(include "ansuz-language#.scm")
(include "ansuz-streams#.scm")
(include "ehwas-resolver#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (fixnum)
         (block))

(include "gebo-rts.scm")

;; library parameters

(define gebo-request-length (make-parameter 250))

(define gebo-wait-timeout (make-parameter 40))

(define-macro (obj . ps)
  `(list->table
    (list ,@(map (lambda (a b) `(cons ,a ,b))
                 (map car ps)
                 (map cadr ps)))))

(define *-uid->mailbox-* (make-table))
(define *-mailbox->uid-* (make-table))

(define-macro (uid->mailbox u)
  `(table-ref *-uid->mailbox-* ,u))

(define-macro (mailbox->uid m)
  `(table-ref *-mailbox->uid-* ,m))

(define-macro (set-mailbox-uid! u m)
  `(begin
     (table-set! *-uid->mailbox-* ,u ,m)
     (table-set! *-mailbox->uid-* ,m ,u)))

(define-structure jid conn-id proc-id)

(define *-uid->thread-* (make-table weak-values: #t))
(define *-thread->uid-* (make-table weak-keys: #t))

(define (thread->uid t)
  (or (table-ref *-thread->uid-* t #f)
      (let(
           (uid (make-uid)))
        (table-set! *-thread->uid-* t uid)
        (table-set! *-uid->thread-* uid t)
        uid)))

(define (uid->thread u)
  (table-ref *-uid->thread-* u #f))

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

(define (silently-die)
  (let*(
        (me (current-thread))
        (uid (mailbox->uid me)))
    (table-set! *-uid->mailbox-* uid)
    (table-set! *-mailbox->uid-* me)))

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

(define (pawned-full-mailbox f r)
  (let(
       (c (thread-receive (gebo-wait-timeout) #f)))
    (if (eq? c 'free)
        (full-mailbox f r)
        (silently-die))))

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
      (thread-send (cadr c) f)
      (empty-mailbox))

     ((eq? c 'pawn)
      (pawned-full-mailbox f r)))))


(define (make-mailbox)
  (thread-start!
   (make-thread
    empty-mailbox)))

(define (scheme-id->javascript-id o)
  (cond
   ((thread? o)
    (obj ("id-type" "scheme")
         ("process-id" (thread->uid o))))
   ((jid? o)
    (obj ("id-type" "javascript")
         ("connection-id" (jid-conn-id o))
         ("process-id" (jid-proc-id o))))
   (else o)))

(define (javascript-id->scheme-id o)
  (if (not (table? o)) o
      (let(
           (t (table-ref o "id-type" #f)))
        (if (not t) o
            (cond
             ((string=? t "scheme")
              (uid->thread (table-ref o "process-id")))
             ((string=? t "javascript")
              (make-jid (table-ref o "connection-id")
                        (table-ref o "process-id")))
             (else o))))))

(define (gebo-send to msg)
  (cond
   ((thread? to) (thread-send to msg))
   ((jid? to) (js-send to msg))
   (else (error "cannot handle address in gebo-send" to))))

(define (js-send to msg)
  (let(
       (th (uid->mailbox (jid-conn-id to))))
    (thread-send
     th
     `(put ,(list->table `(("pid" . ,to) ("message" . ,msg)))))))

;;     (thread-send th
;;      `(put ,(obj ("pid" to) ("message" msg))))))

(define (json-response r o)
  (let(
       (str (call-with-output-u8vector
             (u8vector)
             (lambda (p)
               (json-write o p scheme-id->javascript-id)))))
    (make-response
     (request-version r) 200 "OK"
     (header
      ("Pragma" "no-cache")
      ("Cache-Control" "no-cache, must revalidate")
      ("Expires:" "-1")
      ("Content-type" "application/json")
      ("Content-length" (u8vector-length str)))
     (lambda (p)
       (write-subu8vector str 0 (u8vector-length str) p)))))

(define (gebo-uid req)
  (let(
       (uid (make-uid))
       (mb (make-mailbox)))
    (set-mailbox-uid! uid mb)
    (thread-send mb 'pawn)
    (json-response req uid)))

(define (gebo-listen req)
  (let(
       (mb (uid->mailbox
            (request-query req))))
    (thread-send mb 'free)
    (thread-send mb `(get ,(current-thread)))
    (let*(
          (q (thread-receive))
          (res (json-response req q)))
      (thread-send mb 'pawn)
      res)))

(define (gebo-notify req)
  (let(
       (ms (json-read
            (request-port req)
            javascript-id->scheme-id)))
    (for-each (lambda (m)
                (gebo-send (table-ref m "pid")
                           (table-ref m "message")))
              ms)
    (json-response req #t)))

(define (gebo-rts req)
  (make-response
   (request-version req) 200 "OK"
   (header    
    ("Content-type" "text/plain")
    ("Content-length" *-rts-size-*))
   (lambda (p)
     (write-subu8vector *-rts-data-* 0 *-rts-size-* p))))

(define (gebo-resolver req)
  (let(
       (path (request-path req)))
    (cond
     ((equal? path '("gebo" "rts.js"))
      (gebo-rts req))
     
     ((equal? path '("gebo" "uid"))
      (gebo-uid req))
     
     ((equal? path '("gebo" "listen"))
      (gebo-listen req))
     
     ((equal? path '("gebo" "notify"))
      (gebo-notify req))
     
     (else #f))))
