(##namespace ("ehwas-w3c-logger#"))

(##include "~~/lib/gambit#.scm")
(include "~~/site-scheme/termite/termite#.scm")

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
                   
(define (make-w3clogger resolver log)
  (let*(
        (logger
         (spawn
          (lambda ()
            (print
             port: log
             `("#Software futhark+notapipe\n"
               "#Version 1.0\n"
               "#Start-Date " ,(datetime) "\n"
               "#Fields: "
               "date time "
               "c-ip "
               "cs-method cs-host cs-uri "
               "sc-code sc-status sc-bytes\n"))
            (force-output log)
            (let forever ()
              (recv
               ('exit 'exit)
               (val
                (print port: log val)
                (newline log)
                (force-output log)
                (forever)))))))
        (wrapper
         (lambda (req)
           (let*(
                 (res (resolver req)))
             (and res
                  (begin
                    (! logger
                       `(,(datetime) " "
                         ,(address->string (socket-info-address (tcp-client-peer-socket-info (current-http-port)))) " "
                         ,(request-method req) " "
                         ,(table-ref (request-header req) "Host" "-") " "
                         ,(request-uri-string req) " "
                         ,(response-code res) " "
                         "\"" ,(response-status res) "\" "
                         ,(table-ref (response-header res) "Content-Length" "-") " "))
                    res))))))
    (make-will
     wrapper
     (lambda (wrp)
       (print
        port: log
        `("#End-Date " ,(datetime) "\n"))
       (force-output log)))
    wrapper))
