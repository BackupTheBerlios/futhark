(##namespace ("ehwas-server#"))

(##include "~~/lib/gambit#.scm")

(include "request#.scm")
(include "response#.scm")
(include "../encode/openssl-ports#.scm")

(declare (standard-bindings)
         (extended-bindings)
         ;;(not safe)
         (block))

(define current-http-port (make-parameter #f))

(define (http-server handler #!key (secure #f))
  (let(
       (handler (lambda ()
                  (with-exception-catcher
                   (lambda (ex)
                     ;; (pp ex)
                     '(error 500))
                   (lambda () 
                     (write-http-response (handler (read-http-request))))))))
    (if secure
        (secure-http-server handler)
        (clear-http-server handler))))

(define (secure-http-server handler)
  (lambda ()
    (let(
         (port (port->ssl-server-port (current-input-port) (list (char-encoding:'UTF-8)))))
      (parameterize ((current-http-port (current-input-port))
                     (current-input-port port)
                     (current-output-port port))
        (handler)))))

(define (clear-http-server handler)
  (lambda ()
    (parameterize ((current-http-port (current-input-port)))
      (handler))))

(define (http-service-register! handler #!key (server-address "*") (secure #f) (port-number (if secure 443 80)) (backlog 128) (reuse-address #t))
  (tcp-service-register!
   (list server-address: server-address
         port-number: port-number
         backlog: backlog
         reuse-address: reuse-address
         char-encoding: 'UTF-8
         eol-encoding: 'cr-lf
         buffering: #f)
   (http-server handler)))

(define (http-service-unregister! #!key (server-address "*") (port-number 80))
  (tcp-service-unregister!
   (list server-address: server-address
         port-number: port-number)))