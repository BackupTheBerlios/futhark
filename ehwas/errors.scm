(##namespace ("ehwas-errors#"))

(##include "~~/lib/gambit#.scm")

(include "response#.scm")
(include "request#.scm")
(include "template#.scm")
(include "errors#.scm")
 
(declare (standard-bindings)
         (extended-bindings)
         ;;(not safe)
         (block))

(define *errors* (make-table test: =))

(define-macro (define-default-error code status)
  (let(
       (req (gensym 'req)))
    `(define-error ,code
       (lambda (,req)
         (response
          (request-version ,req) ,code ,status
          (header
           ("Content-type" "text/html"))
          (text
           "<?xml version = \"1.0\" ?>\n"
           (xml
            (html
             (@ (xmlns "http://www.w3.org/1999/xhtml"))
             (head (title "Webserver error " ,code))
             (body
              (center
               (h1 "Web server error " ,code)
               (h4 ,status)))))))))))

(define-default-error 400 "Bad Request")
(define-default-error 401 "Unauthorized Access")
(define-default-error 403 "Forbidden Access")
(define-default-error 404 "File Not Found")
  
;; (define-error 400
;;   (lambda (req)
;;     (response
;;      (request-version req) 400 "Bad Request"
;;      (header ("Content-type" "application/xhtml+xml")
;;              ("Pragma" "no-cache"))
;;      (text
;;       "<?xml version = \"1.0\" ?>\n"
;;       (xml
;;        (html
;;         (@ (xmlns "http://www.w3.org/1999/xhtml"))
;;         (head (title "Web server error 400"))
;;         (body
;;          (center
;;           (h1 "Web server error 400")))))))))

;; (define-error 401
;;   (lambda (req)
;;     (response
;;      (request-version req) 401 "Unauthorized Access"
;;      (header ("Content-type" "application/xhtml+xml")
;;              ("Pragma" "no-cache"))
;;      (text
;;       "<?xml version = \"1.0\" ?>\n"
;;       "<html xmlns = \"http://www.w3.org/1999/xhtml\">\n"
;;       "<head><title>Webserver Error 401</title></head>\n"
;;       "<body><center><h1>Webserver Error 401</h1></center></body></html>\n"))))
    
;; (define-error 403
;;   (lambda (req)
;;     (response
;;      (request-version req) 403 "Forbidden Access"
;;      (header ("Content-type" "application/xhtml+xml")
;;              ("Pragma" "no-cache"))
;;      (text
;;       "<?xml version = \"1.0\" ?>\n"
;;       "<html xmlns = \"http://www.w3.org/1999/xhtml\">\n"
;;       "<head><title>Webserver Error 403</title></head>\n"
;;       "<body><center><h1>Webserver Error 403</h1></center></body></html>\n"))))

;; (define-error 404
;;   (lambda (req)
;;     (response
;;      (request-version req) 404 "File Not Found"
;;      (header ("Content-type" "application/xhtml+xml")
;;              ("Pragma" "no-cache"))
;;      (text
;;       "<?xml version = \"1.0\" ?>\n"
;;       "<html xmlns = \"http://www.w3.org/1999/xhtml\">\n"
;;       "<head><title>Webserver Error 404</title></head>\n"
;;       "<body><center><h1>File not found</h1>"
;;       "<h2>" (request-uri-string req) "</h2>"
;;       "</center></body></html>\n"))))

(define-error 500
  (lambda (req c)
    (response
     (request-version req) 500 "Internal Server Error"
     (header
      ("Content-type" "application/xhtml+xml"))
     (text
      "<?xml version = \"1.0\" ?>\n"
      (xml
       (html
        (@ (xmlns "http://www.w3.org/1999/xhtml"))
        (head
         (title "Webserver Error " 500))
        (body
         (center
          (h1 "Webserver error " 500)
          (h2 "<![CDATA[",c "]]>")))))))))