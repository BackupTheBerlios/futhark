(load "~~/site-scheme/futhark/ehwas/server")
(load "~~/site-scheme/futhark/ehwas/request")
(load "~~/site-scheme/futhark/ehwas/rfc822")
(load "~~/site-scheme/futhark/ehwas/rfc3986")
(load "~~/site-scheme/futhark/ehwas/response")
(load "~~/site-scheme/futhark/ehwas/query")
(load "~~/site-scheme/futhark/ehwas/cookies")
(load "~~/site-scheme/futhark/ehwas/websocket")
(load "~~/site-scheme/futhark/ehwas/combinators")

(load "~~/lib/digest")

(include "~~/site-scheme/futhark/ehwas/server#.scm")
(include "~~/site-scheme/futhark/ehwas/request#.scm")
(include "~~/site-scheme/futhark/ehwas/response#.scm")
(include "~~/site-scheme/futhark/ehwas/query#.scm")
(include "~~/site-scheme/futhark/ehwas/template#.scm")
(include "~~/site-scheme/futhark/ehwas/websocket#.scm")
(include "~~/site-scheme/futhark/ehwas/combinators#.scm")
(include "~~/site-scheme/futhark/ehwas/cookies#.scm")


(define (result req)
  (response
   (request-version req) 200 "OK"
   (header
    Content-type: "text/html")
   (print
    (html-template
     (html
      (head (title ,(request-method req)))
      (body
       (h1 "method:" ,(request-method req))
       ,(map (lambda (kv) (html-template (b ,(car kv)) " : " (pre ,(cdr kv))))
             (table->list (request-query req)))))))))

(define (index req)
  (response
   (request-version req) 200 "OK"
   (header
    Content-type: "text/html")
   (print
    (html-template
     (html
      (head (title "test"))
      (body
       (h1 "GET")
       (form (method: "GET" action: "result")
             (input (type: "text" name:"x" value:"azz"))
               (input (type: "submit")))
       (br)
       (h1 "POST form data")
       (form (method: "POST" action: "result" enctype: "multipart/form-data")
             (input (type: "text" name:"x" value:"azz"))
             (textarea (name: "ararar") "this is a textarea")
             (input (type: "submit")))
       (br)
       (h1 "POST urlencoded")
       (form (method: "POST" action: "result" enctype: "application/x-www-urlencoded")
             (input (type: "text" name:"x" value:"azz"))
             (textarea (name: "araara") "this is a textarea")
             (input (type: "submit")))))))))

(define app
  (paths
   ((result) result)
   (() index)))

(http-service-register! app port-number: 9080)
(thread-sleep! +inf.0)