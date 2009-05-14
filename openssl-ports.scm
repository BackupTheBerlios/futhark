(##namespace ("openssl-ports#"))
(##include "~~/lib/gambit#.scm")
(include "openssl#.scm")

(define-macro (protect . cs)
  `(let () 
     (declare (not interrupts-enabled))
     ,@cs))

(define current-ssl-ctx (make-parameter #f))
           
(define (port->ssl-port p #!optional (clnt? #f) (ctx (current-ssl-ctx)))
  (receive (clnt srvr) (open-u8vector-pipe)
    (letrec(
            (len 512)
            (ssl (protect (ssl-new ctx)))
            (___ (make-will ssl (lambda (_) (protect (ssl-free ssl)))))
            (rbio (bio-new (bio-s-mem)))
            (wbio (bio-new (bio-s-mem)))
            (init? #f)
            (handshake
             (lambda ()
               (let(
                    (ac ((if clnt? ssl-connect ssl-accept) ssl)))
                 (copy-from-bio-to-port)
                 (cond
                  ((= ac 0) `(shutdown))
                  ((< ac 0) `(need read/write))
                  (else
                   (set! init? #t)
                   (thread-start!
                    (make-thread
                     copy-from-server-to-ssl)))))))

            (port-shutdown
             (lambda ()
               (close-port srvr)
               (server-shutdown)
               ))
            
            (server-shutdown
             (lambda ()
               (protect (ssl-shutdown ssl))
               (copy-from-bio-to-port)
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
            
            (copy-from-port-to-bio
             (lambda ()
               (let(
                    (buf (make-u8vector len)))
                 (let copy ()
                   (let(
                        (c (read-subu8vector buf 0 len p 1)))
                     (if (> c 0)
                         (begin
                           (protect (bio-write rbio buf c))
                           (if (not init?) (handshake))
                           (copy-from-ssl-to-server)
                           (copy))
                         (port-shutdown)))))))
             
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
;;                  (let pending ((c (ssl-read ssl buf len)))
;;                    (if (> c 0)
;;                        (begin
;;                          (write-subu8vector buf 0 c srvr)
;;                          (force-output srvr)
;;                          (pending (ssl-read ssl buf len))))))))
            (copy-from-server-to-ssl
             (lambda ()
               (let(
                    (buf (make-u8vector len)))
                 (let copy ()
                   (let(
                        (c (read-subu8vector buf 0 len srvr 1)))
                     (if (> c 0)
                         (begin
                           (ssl-write ssl buf c)
                           (copy-from-bio-to-port)
                           (copy))
                         (server-shutdown))))))))
      
      (protect (ssl-set-bio ssl rbio wbio))
      ;; (protect ((if clnt? ssl-set-connect-state ssl-set-accept-state) ssl))
      (thread-start! (make-thread copy-from-port-to-bio))
      clnt)))


;; (define (open-ssl-server ps #!optional (ctx (current-ssl-ctx)))
;;   (let(
;;        (p (open-tcp-server ps)))
;;     (receive (c s) (open-vector-pipe)
;;       (thread-start!
;;        (make-thread
;;         (lambda ()
;;           (let loop ()
;;             (let(
;;                  (n (read p)))
;;               (write (if (eof-object? n) n (port->ssl-port n #f ctx)) s)
;;               (loop))))))
;;       c)))

;; (define (open-ssl-client ps #!optional (ctx (current-ssl-ctx)))
;;   (port->ssl-port (open-tcp-client ps) #t ctx))


(define (port->ssl-server-port p #!optional (ctx (current-ssl-ctx)))
  (port->ssl-port p #f ctx))

(define (port->ssl-client-port p #!optional (ctx (current-ssl-ctx)))
  (port->ssl-port p #t ctx))
