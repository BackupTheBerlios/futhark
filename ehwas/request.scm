(##namespace ("ehwas-request#"))

(##include "~~/lib/gambit#.scm")

(include "../ansuz/language#.scm")
(include "../ansuz/kernel#.scm")
(include "../ansuz/extras#.scm")
(include "rfc822#.scm")
(include "rfc3986#.scm")

(include "request#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         ;;(not safe)
         )

(define-structure request
  method uri-string uri version
  header port)

(define (request-path r)
  (uri-path (request-uri r)))

(define (request-uri-query r)
  (uri-query (request-uri r)))

(define (request-autority r)
  (uri-authority (request-uri r)))

(define (request-header-ref r v)
  (table-ref (request-header r) v))

(define-parser (token-list)
  (<> (>> (<> (char #\space) (char #\newline)) (return '()))
      (>> (<- c (any))
          (<- cs (token-list))
          (return (cons c cs)))))

(define-parser (token)
  (>> (<- x (token-list))
      (return (list->string x))))

(define-parser (method)
  (<> (word "GET")
      (word "POST")
      (word "OPTIONS")
      (word "HEAD")
      (word "PUT")
      (word "DELETE")
      (word "TRACE")
      (word "CONNECT")))

(define-parser (space)
  (<> (>> (char #\space) (space))
      (return 'ok)))

(define digit->number
  (let(
       (d0 (char->integer #\0)))
    (lambda (v)
      (- (char->integer v) d0))))

(define-parser (http-version)
  (>> (word "HTTP/")
      (<- mj (digit))
      (char #\.)
      (<- mn (digit))
      (return (cons (digit->number mj)
                       (digit->number mn)))))

(define-parser (http-request p)
  (>> (<- m (method))
      (space)
      (<- su (token))
      (space)
      (<- v (<> (http-version) (return '(1 . 0))))
      (char #\newline)
      (<- hd (rfc822))
      (let(
           (u (run (rfc3986) su)))
        (return (make-request
                 m su u v
                 (list->table hd)
                 p)))))