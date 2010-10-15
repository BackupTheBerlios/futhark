(load "~~/site-scheme/futhark/ehwas/server")
(load "~~/site-scheme/futhark/ehwas/request")
(load "~~/site-scheme/futhark/ehwas/rfc822")
(load "~~/site-scheme/futhark/ehwas/rfc3986")
(load "~~/site-scheme/futhark/ehwas/response")
(load "~~/site-scheme/futhark/ehwas/query")
(load "~~/site-scheme/futhark/ehwas/cookies")
(load "~~/site-scheme/futhark/ehwas/combinators")
(load "~~/site-scheme/futhark/gebo/gebo")


(include "~~/site-scheme/futhark/ehwas/server#.scm")
(include "~~/site-scheme/futhark/ehwas/request#.scm")
(include "~~/site-scheme/futhark/ehwas/response#.scm")
(include "~~/site-scheme/futhark/ehwas/query#.scm")
(include "~~/site-scheme/futhark/ehwas/template#.scm")
(include "~~/site-scheme/futhark/ehwas/combinators#.scm")
(include "~~/site-scheme/futhark/ehwas/cookies#.scm")

(include "~~/site-scheme/futhark/gebo/gebo#.scm")

(define (app req)
  (display "HTTP/1.1 200 OK\n")
  (display "Content-Type: text/plain\n")
  ;; (display "Content-length: 11\n")
  (newline)
  (display "Hello Mom\n")
  (display "request-query:\n")
  (table-for-each
   (lambda (k v) (for-each display `(,k = ,v))(newline))
   (request-query req))
  (display "\n"))

(http-service-register! app port-number: 9080)

(thread-sleep! +inf.0)

;; (http-service-register! app port-number: 9080)

