(##namespace ("ehwas-server#"))

(##include "~~/lib/gambit#.scm")

(include "request#.scm")
(include "response#.scm")
(include "errors#.scm")
(include "resolver#.scm")

(include "../ansuz/language#.scm")
(include "../ansuz/sources#.scm")
(include "../encode/openssl-ports#.scm")

(declare (standard-bindings)
         (extended-bindings)
         ;;(not safe)
         (block))

(define-structure server
  host
  port
  sock
  run
  resolver
  ssl?)

(set! make-server
      (let(
           (really-make-server make-server))
        (lambda (h p r #!optional (ssl? #f))
          (really-make-server h p #f #f r ssl?))))

(define (serve-new-connection port resolver)
  (with-exception-catcher
   (lambda (c)
     ;; INSERT HERE LOG ERROR CODE
     ;; ignore all errors and close connection
     ;; (log-error c)
     (close-port port))
   (lambda ()
     (let(
          (req (run (http-request port) port)))
       (let(
            (res (resolver req)))
         (close-input-port port)
         (response-write res port)
         (close-port port))))))
;;         (let(
;;              (r (resolver (run (http-request port) (port->stream port)))))
;;           (close-input-port port)
  ;;           (response-write r port))))))

(define (open-server-socket host port)
  ;; (declare (fixnum port))
  (open-tcp-server
   (list
    char-encoding: 'UTF-8
    eol-encoding: 'cr-lf
    server-address: host
    port-number: port
    backlog: 1000000
    reuse-address: #t)))

;; (define (open-ssl-server-socket host port)
;;   ;; (declare (fixnum port))
;;   (open-ssl-server
;;    (list
;;     char-encoding: 'UTF-8
;;     eol-encoding: 'cr-lf
;;     server-address: host
;;     port-number: port
;;     backlog: 1000000
;;     reuse-address: #t)))

(define (open-client-socket host port)
  ;; (declare (fixnum port))
  (open-tcp-client
   (list
    server-address: (or host "localhost")
    port-number: port)))

;; (define (open-ssl-client-socket host port)
;;   ;; (declare (fixnum port))
;;   (open-tcp-client
;;    (list
;;     server-address: (or host "localhost")
;;     port-number: port)))

(define real-http-port (make-parameter #f))

(define (main-loop s)
  (let main-loop ()
    (let(
         (p (read (server-sock s))))
      (thread-start!
       (make-thread
        (lambda ()
          (let(
               (pt (if (server-ssl? s) (port->ssl-server-port p (list char-encoding: 'UTF-8)) p)))
            (parameterize ((real-http-port p))
              (serve-new-connection pt (server-resolver s)))))))
      ;; TODO portare server-run prima di servire la richiesta.
      (if (server-run s)
          (main-loop)
          (close-port (server-sock s))))))

(define (touch-server s)
  (let (
        (p
         (open-client-socket
          (server-host s)
          (server-port s))))
    (print port: p "GET /\n")
    (force-output p)
    (close-port p)))

(define (start! s)
  (server-sock-set! s
                    (open-server-socket (server-host s) (server-port s)))
  (server-run-set! s #t)
  (main-loop s))

(define (stop! s)
  (server-run-set! s #f)
  (touch-server s))

