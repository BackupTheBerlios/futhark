;; a response is a structure that encapsulates the return value of
;; a request
;; resolver :: request -> response
;; it has 5 fields:
;; version: the http protocol version
;; code: the response code (normally 200, not found 404, et cetera)
;; status: the response status (normally OK, Not found ...)
;; header: an hash table containing response headers,
;;         keys and values should be strings
;; Author Francesco Bracchi

(##namespace ("ehwas-response#"))

(##include "~~/lib/gambit#.scm")

(include "ehwas-request#.scm")
 
(define-structure response version code status headers printer)

(define (make-empty-response v c s)
  (make-response v c s (make-table init: #f) (lambda (p) 'ok)))

(define (response-header-set! r k v)
  (table-set! (response-headers r) k v))

(define (response-header-ref r k)
  (table-ref (response-headers r) k))

(define (response-append r p1)
  (let(
       (p0 (response-printer r)))
    (response-printer-set!
     r
     (lambda (p) (p0 p) (p1 p)))))

(define (writer w)
  (lambda (p) (write w p)))

(define (displayer w)
  (lambda (p) (print port: p w)))

(define (response-write r #!optional (p (current-output-port)))
  (let(
       (version (response-version r))
       (code (response-code r))
       (status (response-status r))
       (headers (response-headers r))
       (printer (response-printer r)))
    
    (print port: p
     (list "HTTP/" (car version) "." (cdr version) " " code " " status "\n"))
    (table-for-each
     (lambda (k v)
       (print port: p (list k ": " v #\newline)))
     headers)
    (newline p)
    (printer p)))
