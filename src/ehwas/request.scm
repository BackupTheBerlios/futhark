(##namespace ("ehwas-request#"))

(##include "~~/lib/gambit#.scm")

(include "../ansuz/sources/port#.scm")
(include "../ansuz/char-stream-parser#.scm")
(include "../ansuz/re#.scm")

(include "rfc822#.scm")
(include "rfc3986#.scm")

(declare (standard-bindings)
         (extended-bindings)
         ;;(not safe)
         (block))

(define-structure request method uri-string uri version header)

;; (define-parser (token-list)
;;   (<> (>> (<> (char #\space) (char #\newline)) (return '()))
;;       (>> (<- c (any))
;;           (<- cs (token-list))
;;           (return (cons c cs)))))

;; (define-parser (token)
;;   (>> (<- x (token-list))
;;       (return (list->string x))))

;; (define-regexp token "[~ \n]*")

;; (define-parser (method)
;;   (<> (word "GET")
;;       (word "POST")
;;       (word "OPTIONS")
;;       (word "HEAD")
;;       (word "PUT")
;;       (word "DELETE")
;;       (word "TRACE")
;;       (word "CONNECT")))

(define-regexp method "GET|POST|OPTIONS|HEAD|PUT|DELETE|TRACE|CONNECT")

(define-regexp space " *")

(define digit->number
  (let(
       (d0 (char->integer #\0)))
    (lambda (v)
      (- (char->integer v) d0))))

(define-parser (http-version)
  (regexp "HTTP/")
  (<- mj (digit))
  (char #\.)
  (<- mn (digit))
  (return (cons (digit->number mj)
                (digit->number mn))))


(define-parser (http-request)
  (<- m (method))
  (space)
  (<- su (regexp "[~ \n]*"))
  (space)
  (<- v (<> (http-version) (return '(1 . 0))))
  (space)
  (char #\newline)
  (<- hd (rfc822))
  (return (make-request
           (string->symbol m) su (string->uri su) v hd)))

(define (read-http-request #!optional (port (current-input-port)))
  (run (http-request) port))

(define (request-scheme request)
  (if (request? request)
      (uri-scheme (request-uri request))))

;; (define (request-authority request)
;;   (uri-authority (request-uri request)))

(define (request-path request)
  (uri-path (request-uri request)))

;; (define (request-query request)
;;   (uri-query (request-uri request)))

;; (define (request-fragment request)
;;   (uri-fragment (request-uri request)))

