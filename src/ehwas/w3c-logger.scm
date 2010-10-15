(##namespace ("ehwas-w3c-logger#"))

(##include "~~/lib/gambit#.scm")
;; (include "~~/lib/termite/termite#.scm")

(include "server#.scm")
(include "request#.scm")
(include "response#.scm")

(declare
 (standard-bindings)
 (extended-bindings)
 ;; (not safe)
 ;; (block)
 )
 
(c-declare "#include <time.h>")

(define ctime (c-lambda () int "___result=time(NULL);"))

(define format-time
  (c-lambda (char-string long) char-string
#<<end-format-time

char res [80];
struct tm *t;
int status;

t=localtime (&___arg2);
status=strftime(res, 80, ___arg1, t);
if (! status) return ___FIX (___UNKNOWN_ERR);
___result= res;

end-format-time
))
                

(define (w3c-date ct)
  (declare (not interrupts-enabled))
  (format-time "%Y-%m-%d" ct))

(define (w3c-time ct)
  (declare (not interrupts-enabled))
  (format-time "%H:%M:%S" ct))


(define (datetime)
  (let(
       (now (ctime)))
    `(,(w3c-date now) " " ,(w3c-time now))))


(define (address->string a)
  (string-append
   (number->string (u8vector-ref a 0))
   "."
   (number->string (u8vector-ref a 1))
   "."
   (number->string (u8vector-ref a 2))
   "."
   (number->string (u8vector-ref a 3))))
                   
;; (define (make-w3clogger resolver log)
;;   (let*(
;;         (logger
;;          (spawn
;;           (lambda ()
;;             (print
;;              port: log
;;              `("#Software futhark+notapipe\n"
;;                "#Version 1.0\n"
;;                "#Start-Date " ,(datetime) "\n"
;;                "#Fields: "
;;                "date time "
;;                "c-ip "
;;                "cs-method cs-host cs-uri "
;;                "sc-code sc-status sc-bytes\n"))
;;             (force-output log)
;;             (let forever ()
;;               (recv
;;                ('exit 'exit)
;;                (val
;;                 (print port: log val)
;;                 (newline log)
;;                 (force-output log)
;;                 (forever)))))))
;;         (wrapper
;;          (lambda (req)
;;            (let(
;; 		(res (handler req)))
;;              (and res
;;                   (begin
;;                     (! logger
;;                        `(,(datetime) " "
;;                          ,(address->string (socket-info-address (tcp-client-peer-socket-info (current-http-port)))) " "
;;                          ,(request-method req) " "
;;                          ,(table-ref (request-header req) "Host" "-") " "
;;                          ,(request-uri-string req) " "
;;                          ,(response-code res) " "
;;                          "\"" ,(response-status res) "\" "
;;                          ,(assq (response-header res) 'Content-Length "-") " "))
;;                     res))))))
;;     (make-will
;;      wrapper
;;      (lambda (wrp)
;;        (print
;;         port: log
;;         `("#End-Date " ,(datetime) "\n"))
;;        (force-output log)))
;;     wrapper))

(define (log! handler fn)
  (let*(
	(lock (make-mutex))
	(wrp
	 (lambda (req)
	   (let(
		(res (handler req)))
	     (thread-start!
	      (make-thread
	       (lambda ()
		 (mutex-lock! lock)
		 (with-exception-catcher
		  (lambda (ex) 
		    (mutex-unlock! lock)
		    (raise ex))
		  (lambda ()
		    (append-to-file fn (print-log! req res))
		    (mutex-unlock! lock))))))
	     res))))
    (make-will wrp (lambda (_) (append-to-file fn print-footer!)))
    (append-to-file fn print-header!)
    wrp))

(define (print-header! p)
  (print
   port: p 
   `("#Software futhark+notapipe\n"
     "#Version 1.0\n"
     "#Start-Date " ,(datetime) "\n"
     "#Fields: "
     "date time "
     "c-ip "
     "cs-method cs-host cs-uri "
     "sc-code sc-status sc-bytes\n")))

(define (print-log! req res)
  (lambda (p)
    (print
     port: p
     `(,(datetime) " "
       ,(address->string (socket-info-address (tcp-client-peer-socket-info (current-http-port)))) " "
       ,(request-method req) " "
       ,(let(
	     (host (assoc 'Host (request-header req))))
	  (if host (cdr host) "-")) 
       " "
       ,(request-uri-string req) " "
       ,(response-code res) " "
       "\"" ,(response-status res) "\" "
       ,(let(
	     (len (assoc 'Content-Length (response-header res))))
	  (if len (cdr len) "-"))
       " \n"))))

(define (print-footer! p)
  (print
   port: p
   `("#End-Date " ,(datetime) "\n")))

(define (append-to-file fn proc)
  (call-with-output-file (list path: fn append: #t create: 'maybe) proc))

		  
;; (define (log-port! handler p)
;;   (define lock (make-mutex))
;;   (define exit '())
;;   (define (state req)
;;     (mutex-lock! lock) 
;;     (print-header! p)
;;     (mutex-unlock! lock)
;;     (let forever ((req req))
;;       (let(
;; 	   (res (handler req)))
;; 	(mutex-lock! lock)
;; 	(with-exception-catcher
;; 	 (lambda (ex) 
;; 	   (mutex-unlock! lock) 
;; 	   (raise ex))
;; 	 (lambda ()
;; 	   (print-log! p req res)
;; 	   (mutex-unlock! lock)))
;; 	(forever
;; 	 (call/cc
;; 	  (lambda (here)
;; 	    (set! state here)
;; 	    (exit res)))))))
;;   (define (wrp req)
;;     (call/cc
;;      (lambda (here)
;;        (set! exit here)
;;        (state req))))

;;   (make-will wrp (lambda (_) (print-footer! p)))
;;   wrp)

;; (define (print-header! p)
;;   (print
;;    port: p 
;;    `("#Software futhark+notapipe\n"
;;      "#Version 1.0\n"
;;      "#Start-Date " ,(datetime) "\n"
;;      "#Fields: "
;;      "date time "
;;      "c-ip "
;;      "cs-method cs-host cs-uri "
;;      "sc-code sc-status sc-bytes\n"))
;;   (force-output p))

;; (define (print-log! p req res)
;;   (print
;;    port: p
;;    `(,(datetime) " "
;;      ,(address->string (socket-info-address (tcp-client-peer-socket-info (current-http-port)))) " "
;;      ,(request-method req) " "
;;      ,(table-ref (request-header req) "Host" "-") " "
;;      ,(request-uri-string req) " "
;;      ,(response-code res) " "
;;      "\"" ,(response-status res) "\" "
;;      ,(let(
;; 	   (len (assq 'Content-Length (response-header res))))
;; 	(if len len "-"))
;;      " \n"))
;;   (force-output p))

;; (define (print-footer! p)
;;   (print
;;    port: p
;;    `("#End-Date " ,(datetime) "\n"))
;;   (force-output p)
;;   (close-port p))

;; (define (log! handler fn)
;;   (let(
;;        (p (open-output-file (list path: fn create: 'maybe appent: #t))))
;;     (log-port! handler p)))
