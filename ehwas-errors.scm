(##namespace ("ehwas-errors#"))

(##include "~~/lib/gambit#.scm")

(include "ehwas-response#.scm")
(include "ehwas-request#.scm")
(include "ehwas-errors#.scm")
 
(declare (standard-bindings)
         (extended-bindings)
         (not safe)
         (block))

(define *errors* (make-table test: =))

(define-error 400
  (lambda (req)
    (make-response
     (request-version req) 400 "Bad Request"
     (header ("Content-type" "application/xhtml+xml")
             ("Pragma" "no-cache"))
     (body
      "<?xml version = \"1.0\" ?>\n"
      "<html xmlns = \"http://www.w3.org/1999/xhtml\">\n"
      "<head><title>Webserver Error 400</title></head>\n"
      "<body><center><h1>Webserver Error 400</h1></center></body></html>\n"))))

(define-error 401
  (lambda (req)
    (make-response
     (request-version req) 401 "Unauthorized Access"
     (header ("Content-type" "application/xhtml+xml")
             ("Pragma" "no-cache"))
     (body
      "<?xml version = \"1.0\" ?>\n"
      "<html xmlns = \"http://www.w3.org/1999/xhtml\">\n"
      "<head><title>Webserver Error 401</title></head>\n"
      "<body><center><h1>Webserver Error 401</h1></center></body></html>\n"))))
    
(define-error 403
  (lambda (req)
    (make-response
     (request-version req) 403 "Forbidden Access"
     (header ("Content-type" "application/xhtml+xml")
             ("Pragma" "no-cache"))
     (body
      "<?xml version = \"1.0\" ?>\n"
      "<html xmlns = \"http://www.w3.org/1999/xhtml\">\n"
      "<head><title>Webserver Error 403</title></head>\n"
      "<body><center><h1>Webserver Error 403</h1></center></body></html>\n"))))

(define-error 404
  (lambda (req)
    (make-response
     (request-version req) 404 "File Not Found"
     (header ("Content-type" "application/xhtml+xml")
             ("Pragma" "no-cache"))
     (body
      "<?xml version = \"1.0\" ?>\n"
      "<html xmlns = \"http://www.w3.org/1999/xhtml\">\n"
      "<head><title>Webserver Error 404</title></head>\n"
      "<body><center><h1>File not found</h1>"
      "<h2>" (request-uri-string req) "</h2>"
      "</center></body></html>\n"))))

(define-error 500
  (lambda (req c)
    (make-response
     (request-version req) 500 "Internal Server Error"
     (header
      ("Content-type" "application/xhtml+xml")
      ("Pragma" "no-cache"))
     (body
      "<?xml version = \"1.0\" ?>\n"
      "<html xmlns = \"http://www.w3.org/1999/xhtml\">\n"
      "<head><title>Webserver Error 500</title></head>\n"
      "<body><center><h1>Webserver Error 500 </h1>"
      "<h2><![CDATA[" c "]]></h2>"
      "</center></body></html>\n"))))
