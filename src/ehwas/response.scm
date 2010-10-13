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

;; (include "request#.scm")

(declare (standard-bindings)
         (extended-bindings)
         ;; (not safe)
         (block))

(define response-version (make-parameter "HTTP/1.1"))

(define-structure response code status header writer)

(define (write-http-response response #!optional (port (current-output-port)))
  (let(
       (display* (lambda (x) (display x port))))
    (for-each display* (list (response-version) " " (response-code response) " " (response-status response) "\n"))
    (for-each (lambda (pair) (for-each display* (list (car pair) ": " (cdr pair) "\n"))) (response-header response))
    (newline port)
    (parameterize ((current-output-port port)) ((response-writer response))))))

