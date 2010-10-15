(load "../src/ehwas/response")
(include "../src/ehwas/response#.scm")

(define resp
  (response
   200 "OK"
   (header
    Content-type "text/html"
    Content-length: 2020
    "XSpam" 22)
   (print "ciao\n")))

(write-http-response resp)
