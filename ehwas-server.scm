(##namespace ("ehwas-server#"))

(##include "~~/lib/gambit#.scm")

(include "ehwas-request#.scm")
(include "ehwas-response#.scm")
(include "ehwas-errors#.scm")
(include "ehwas-resolver#.scm")

(include "ansuz-language#.scm")
(include "ansuz-streams#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (not safe)
         (block))

;; (define-structure server
;;   host
;;   port
;;   resolver
;;   connection-port
;;   process)

;; (set! make-server
;;       (let(
;;            (make make-server))
;;         (lambda (h p)
;;           (make h p not-found-resolver #f #f))))

;; (define (response-exception e)
;;   (call-with-response
;;    (make-response '(1 . 1) 200 "OK")
;;    (lambda (res)
;;      (response-set! res "Content-type" "application/xhtml+xml")
;;      (response-display!
;;       res
;;       (list "<?xml version = \"1.0\" ?>\n"
;;             "<html xmlns = \"http://www.w3.org/1999/xhtml\">\n"
;;             "<head><title>Ehwas Exception</title></head>\n"
;;             "<body><center>"
;;             "<h1>Ehwas Exception</h1>"
;;             "<p><![CDATA["
;;             (call-with-output-string "" (lambda (p) (##display-exception e p)))
;;             "]]></p>"
;;             "</center></body></html>")))))

;; (define (serve-new-connection port resolver)
;;   (with-exception-catcher
;;    (lambda (ex)
;;      (display ex)
;;      (write-response!
;;       (http-error 400 (make-request "GET" "" '(1 . 1) '() '()))
;;       port)
;;      (close-port port))
;;    (lambda ()
;;      ;; (input-port-timeout-set! port 300)
;;      (let*(
;;            (req (run (request) (port->stream port)))
;;            (response
;;             (with-exception-catcher
;;              response-exception
;;              (lambda () (resolver req)))))
;;        (write-response! response port)
;;        ;; keep alive test
;;        (close-port port)))))

;; (define (server-start! s)
;;   (let*(
;;         (conn-port (open-tcp-server
;;                     (list
;;                      eol-encoding: 'cr-lf
;;                      server-address: (server-host s)
;;                      port-number: (server-port s)
;;                      reuse-address: #t)))
;;         (thread (make-thread
;;                  (lambda ()
;;                    (let forever ((port (read conn-port)))
;;                      (thread-start! (make-thread (lambda () (serve-new-connection port (server-resolver s)))))
;;                      (forever (read conn-port)))))))
;;     (server-process-set! s thread)
;;     (server-connection-port-set! s conn-port)
;;     (thread-start! thread)
;;     thread))

;; (define (server-stop! s)
;;   (close-port (server-connection-port s))
;;   (thread-terminate! (server-process s))
;;   (server-process-set! s #f)
;;   (server-connection-port-set! s #f))

(define-structure server
  host
  port
  sock
  run
  resolver)

(set! make-server
      (let(
           (really-make-server make-server))
        (lambda (h p r)
          (really-make-server h p #f #f r))))

(define (serve-new-connection port resolver)
  (call-with-current-continuation
   (lambda (exit)
     (with-exception-catcher
      (lambda (c)
        ;; INSERT HERE LOG ERROR CODE
        ;; ignore all errors and close connection
        (pp (type-exception-procedure c))
        (exit 'ok))
      (lambda ()
        (let(
             (r (resolver (run (http-request port) (port->stream port)))))
          (response-write r port))))))
  (close-port port))

(define (open-server-socket host port)
  ;; (declare (fixnum port))
  (open-tcp-server
   (list
    char-encoding: 'ASCII
    eol-encoding: 'cr-lf
    server-address: host
    port-number: port
    reuse-address: #t)))

(define (open-client-socket host port)
  ;; (declare (fixnum port))
  (open-tcp-client
   (list
    server-address: (or host "localhost")
    port-number: port)))

(define (main-loop s)
  (let main-loop ()
    (let(
         (p (read (server-sock s))))
      (thread-start! (make-thread (lambda () (serve-new-connection p (server-resolver s)))))
      ;; TODO portare server-run prima di servire la richiesta.
      (if (server-run s)
          (main-loop)
          (close-port (server-sock s))))))

(define (touch-server s)
  (close-port 
   (open-client-socket
    (server-host s)
    (server-port s))))

(define (start! s)
  (server-sock-set! s (open-server-socket (server-host s) (server-port s)))
  (server-run-set! s #t)
  (main-loop s))

(define (stop! s)
  (server-run-set! s #f)
  (touch-server s))

