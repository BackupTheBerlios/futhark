(##namespace ("encode-openssl-ports#"))
(##include "~~/lib/gambit#.scm")
(include "openssl#.scm")

(define-macro (protect . cs)
  `(let () 
     (declare (not interrupts-enabled))
     ,@cs))

(define current-ssl-ctx (make-parameter #f))

(define (port->ssl-port p #!optional (opts '()) (clnt? #f) (ctx (current-ssl-ctx)))
  (receive (clnt srvr) (open-u8vector-pipe opts)
    (letrec(
            (len 512)
            (ssl (protect (ssl-new ctx)))
            (rbio (protect (bio-new (bio-s-mem))))
            (wbio (protect (bio-new (bio-s-mem))))
            (init? #f)
            
            (handshake
             (lambda ()
               (let(
                    (ac (protect ((if clnt? ssl-connect ssl-accept) ssl))))
                 (copy-from-bio-to-port)
                 (cond
                  ((= ac 0) `(shutdown))
                  ((< ac 0) `(need more))
                  (else
                   (set! init? #t)
                   (thread-start!
                    (make-thread
                     copy-from-server-to-ssl)))))))
            
            (port-shutdown
             (lambda ()
               (close-port clnt)
               ))
            (server-shutdown
             (lambda ()
               ;; (protect (ssl-shutdown ssl))
               (close-port p)
               ))
            
            (copy-from-bio-to-port
             (lambda ()
               (let(
                    (buf (make-u8vector len)))
                 (let copy ()
                   (let(
                        (c (bio-read wbio buf len)))
                     (if (> c 0)
                         (begin
                           (write-subu8vector buf 0 c p)
                           (force-output p)
                           (copy))))))))
            
            (data-copy-from-port-to-bio
             (lambda ()
               (let(
                    (buf (make-u8vector len)))
                 (let copy ()
                   (let(
                        (c (read-subu8vector buf 0 len p 0)))
                     (if (> c 0)
                         (begin
                           (protect (bio-write rbio buf c))
                           (copy))))))))
            
            (copy-from-port-to-bio
             (lambda ()
               (let(
                    (c (read-u8 p)))
                 (if (not (eof-object? c))
                     (begin
                       (protect (bio-write rbio (u8vector c) 1))
                       (data-copy-from-port-to-bio)
                       (if (not init?) (handshake))
                       (copy-from-ssl-to-server)
                       (copy-from-port-to-bio))
                     (port-shutdown)))))
             
            (copy-from-ssl-to-server
             (lambda ()
               (let(
                    (buf (make-u8vector len)))
                 (let copy ()
                   (let(
                        (c (protect (ssl-read ssl buf len))))
                     (if (> c 0)
                         (begin
                           (write-subu8vector buf 0 c srvr)
                           (force-output srvr)
                           (copy))))))))
            
            (data-copy-from-server-to-ssl
             (lambda ()
               (let(
                    (buf (make-u8vector len)))
                 (let copy ()
                   (let(
                        (c (read-subu8vector buf 0 len srvr 0)))
                     (if (> c 0)
                         (begin
                           (protect (ssl-write ssl buf c))
                           (copy))))))))
              
            (copy-from-server-to-ssl
             (lambda ()
               (let(
                    (c (read-u8 srvr)))
                 (if (not (eof-object? c))
                     (begin
                       (protect (ssl-write ssl (u8vector c) 1))
                       (data-copy-from-server-to-ssl)
                       (copy-from-bio-to-port)
                       (copy-from-server-to-ssl))
                     (server-shutdown))))))
      (protect (ssl-set-bio ssl rbio wbio))
      (thread-start! (make-thread copy-from-port-to-bio))
      (make-will ssl (lambda (_) (protect (ssl-free ssl))))
      clnt)))

(define (port->ssl-server-port p #!optional (opts '()) (ctx (current-ssl-ctx)))
  (port->ssl-port p opts #f ctx))

(define (port->ssl-client-port p #!optional (opts '()) (ctx (current-ssl-ctx)))
  (port->ssl-port p opts #t ctx))
