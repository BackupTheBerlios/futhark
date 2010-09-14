(##namespace ("ehwas-errors#"))

(##include "~~/lib/gambit#.scm")

(include "response#.scm")
(include "request#.scm")
(include "template#.scm")
(include "errors#.scm")

(##include "~~/lib/gambit#.scm")

(declare (standard-bindings)
         (extended-bindings)
         ;;(not safe)
         (block))

(define *default-errors* (make-table test: =))

(define-macro (define-default-for-error code status)
  (let(
       (body (gensym 'body))
       (req (gensym 'req)))
    `(define-default-error-handler ,code
       (lambda (,req #!key exception continuation)
         (response
          (request-version ,req) ,code ,status
          (header
           Pragma: "no-cache"
           Cache-Control: "no-cache, must revalidate"
           Expires: "-1"
           Content-type: "text/html")
          (print
           (html-template
           "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
           (html
            (head (title "Webserver error " ,code))
            (body
             (center
              (h1 "Web server error " ,code)
              (h4 ,status)))))))))))

(define-default-for-error 400 "Bad Request")
(define-default-for-error 401 "Unauthorized Access")
(define-default-for-error 403 "Forbidden Access")
(define-default-for-error 404 "File Not Found")
             
(define (string->html s)
  (let(
       (len (string-length s)))
    (call-with-output-string
     (string)
     (lambda (port)
       (let loop ((j 0))
         (if (>= j len) 'ok
             (let(
                  (ch (string-ref s j)))
               (cond
                ((char=? ch #\>) (display "&gt;" port))
                ((char=? ch #\<) (display "&lt;" port))
                ((char=? ch #\&) (display "&amp;" port))
                ((char=? ch #\newline) (display "<br>" port))
                (else
                 (display ch port)))
               (loop (+ j 1)))))))))

(define (exception->string e k)
  (call-with-output-string
   ""
   (lambda (p) (display-exception-in-context e k p))))

(define (backtrace->string k)
  (call-with-output-string
   ""
   (lambda (p)
     (display-continuation-backtrace k p))))

(define-default-error-handler 500
  (lambda (req #!key exception continuation)
    (response
     (request-version req) 500 "Internal Server Error"
     (header
      Pragma: "no-cache"
      Cache-Control: "no-cache, must revalidate"
      Expires: "-1"
      Content-type: "text/html")
     (print
      (html-template
       "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
       (html
        (head
         (title "Webserver Error " 500))
        (body
         (center
          (h1 "Webserver error " 500)
          (h2 (@ (style "align:left")) 
              ,(call-with-output-string
                ""
                (lambda (p)
                  (print
                   port: p
                   (html-template
                    (h4 ,(string->html (exception->string exception continuation)))
                    (h5 "In:")
                    (textarea (@ (cols "80") (rows "10") (readonly "true")) ,(backtrace->string continuation)))))))))))))))


(define (default-handler req code #!key (exception #f) (continuation #f))
  ((table-ref *default-errors* code) req exception: exception continuation: continuation))

;; (define (with-error-handler handler res)
;;   (lambda (req)
;;     (or (with-exception-catcher
;;          (lambda (e)
;;            (continuation-capture
;;             (lambda (k)
;;               (handler req 500 exception: e continuation: k))))
;;          (lambda () (res req)))
;;         (handler req 404))))

(define (with-error-handler handler res)
  (lambda (req)
    (let(
         (kont #f))
      (or (with-exception-catcher
           (lambda (e)
             (handler req 500 exception: e continuation: kont))
           (lambda ()
             (continuation-capture
              (lambda (k)
                (set! kont k)
                (res req)))))
          (handler req 404)))))

(define (with-default-error-handler res)
  (with-error-handler default-handler res))
