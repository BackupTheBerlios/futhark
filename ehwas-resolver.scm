

(##namespace ("ehwas-resolver#"))

(##include "~~/lib/gambit#.scm")

(include "ehwas-response#.scm")
(include "ehwas-request#.scm")
(include "rfc3986#.scm")
(include "ehwas-errors#.scm")
(include "ehwas-resolver#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (fixnum)
         (block))

(define not-found-resolver
  (lambda (request)
    (http-error 404 request)))

(define (identity x) x)

;; (define (orelse-resolver . rs)
;;   (lambda (request)
;;     (let test ((rs rs))
;;       (cond
;;        ((null? rs) #f)
;;        (((car rs) request) => identity)
;;        (else (test (cdr rs)))))))

(define (orelse-resolver . rs)
  (lambda (request)
    (let test ((rs rs))
      (and (not (null? rs))
           (or ((car rs) request)
               (test (cdr rs)))))))

(define (map-request fn resolve)
  (lambda (request)
    (resolve (fn request))))

(define (exception->string e)
  (call-with-output-string
   ""
   (lambda (p)
     (##continuation-capture
      (lambda (k)
        (##display-exception-in-context e k p))))))

(define (make-guarded-resolver res)
  (lambda (r)
    (with-exception-catcher
     (lambda (c) (http-error 500 r (exception->string c)))
     (lambda () (res r)))))

(define (path->localstring p)
  (if (null? p) ""
      (string-append
       (car p)
       (let conv ((p (cdr p)))
         (if (null? p) ""
             (string-append
              "/" (car p)
              (conv (cdr p))))))))

(define types (make-table test: string-ci=? init: "text/plain"))

(table-set! types  ".js"    "text/ecmascript")
(table-set! types  ".css"   "text/css")
(table-set! types  ".html"  "text/html")
(table-set! types  ".htm"   "text/html")
(table-set! types  ".shtml" "text/html")
(table-set! types  ".html"  "text/html")
(table-set! types  ".txt"   "text/plain")
(table-set! types  ".xhtml" "application/xhtml+xml")
(table-set! types  ".xht"   "application/xhtml+xml")
(table-set! types  ".svg"   "image/svg+xml")
(table-set! types  ".svgz"  "image/svg+xml")
(table-set! types  ".png"   "image/png")
(table-set! types  ".gif"   "image/gif")
(table-set! types  ".jpg"   "image/jpeg")
(table-set! types  ".jpeg"  "image/jpeg")
(table-set! types  ".jpe"   "image/jpeg")
(table-set! types  ".xml"   "application/xml")
(table-set! types  ".zip"   "application/zip")
(table-set! types ".ico"    "image/x-icon")

(define (content-type ext)
  (table-ref types ext))

(define (directory? f)
  (eq? (file-type f) 'directory))

(define (regular? f)
  (eq? (file-type f) 'regular))

(define (with-buffered-printer response)
  (and response
       (let*(
             (printer (response-printer response))
             (buffer (call-with-output-u8vector (u8vector) printer))
             (size (u8vector-length buffer)))
         (make-response
          (response-version response)
          (response-code response)
          (response-status response)
          (response-headers response)
          (lambda (p) (write-subu8vector buffer 0 size p))))))

(define (make-cached-resolver resolve)
  (let(
       (m0 (make-mutex))
       (cache (make-table weak-keys: #t
                          weak-values: #t
                          init: #f)))
    (lambda (request)
      (let(
           (key (request-uri-string request)))
       (define response)
       (mutex-lock! m0)
       (set! response (table-ref cache key))
       (cond
        ((mutex? response)
         (let ((m1 response))
           (mutex-unlock! m0)
           (mutex-lock! m1)
           (set! response (table-ref cache key))
           (mutex-unlock! m1)))

        ((not response)
         (let(
              (m1 (make-mutex)))
           (mutex-lock! m1)
           (table-set! cache key m1)
           (mutex-unlock! m0)
           (set! response (with-buffered-printer (resolve request)))
           (if response
               (table-set! cache key response)
               (table-set! cache key))
           (mutex-unlock! m1)))

        (else
         (mutex-unlock! m0)))
        response))))

(define (make-filesystem-resolver root)
  (lambda (request)
    (if (null? (uri-path (request-uri request)))
        (make-directory-response (request-version request) 200 "OK" "" (string-append root "/"))
        (let*(
              (local (path->localstring (uri-path (request-uri request))))
              (full (string-append root "/" local)))
          (cond
           ((not (file-exists? full)) #f)
           ((regular? full) (make-file-response (request-version request) 200 "OK" full))
           ((directory? full) (make-directory-response (request-version request) 200 "OK" local full))
           (else #f))))))

(define buffer-size 4096)
(define (make-file-response v c s full)
  (let(
       (buffer (make-u8vector buffer-size))
       (size (file-size full)))
    (make-response
     v c s
     (header ("Content-type" (content-type (path-extension full)))
             ("Content-length" size)
             ("Pragma" "no-cache"))
     (lambda (out)
       (call-with-input-file full
         (lambda (in)
           (let loop ((j 0))
             (if (< j size)
                 (let(
                      (delta (read-subu8vector buffer 0 buffer-size in)))
                   (write-subu8vector buffer 0 delta out)
                   (loop (+ j delta)))))))))))
       
(define (make-directory-response v c s local full)
  (let(
       (str (call-with-output-string
             ""
             (body
              "<xml version = \"1.0\">\n"
              "<xhtml xmlns=\"http://www.w3.org/1999/xhtml\">\n"
              "<body>"
              (map
               (lambda (name) (list "<a href = \"" local "/" name "\">" name "</a><br/>\n"))
               (directory-files full))
              "</body></xhtml>\n"))))
    (make-response
     v c s
     (header
      ("Content-type" "text/html")
      ("Content-length" (string-length str))
      ("Pragma" "no-cache"))
     (body str))))

(define (with-index-resolver file resolver)
  (lambda (request)
    (resolver
     (make-request
      (request-method request)
      (request-uri-string request)
      (let(
           (uri (request-uri request)))
        (make-uri
         (uri-scheme uri)
         (uri-authority uri)
         (append (uri-path uri) (list file))
         (uri-query uri)
         (uri-fragment uri)))
      (request-version request)
      (request-header request)
      (request-port request)))))

;; (define (make-table-resolver table)
;;   (lambda (request)
;;     (let walk ((path (cdr (uri-path (request-uri request))))
;;                (table table))
;;       (cond
;;        ((null? path) (and (procedure? table) (table request)))
;;        ((table-ref table (car path)) =>
;;         (lambda (table) (and (table? table) (walk (cdr path) table))))
;;        (else #f)))))

(define (make-table-resolver table)
  (lambda (req)
    ((table-ref table (request-path req)) req)))

(define (remove-base base val)
  (cond
   ((null? base) val)
   ((null? val) #f)
   ((not (equal? (car base) (car val))) #f)
   (else
    (remove-base (cdr base) (cdr val)))))


(define (with-prefix-resolver prefix resolver)
  (lambda (request)
    (let(
         (p1 (remove-base prefix (request-path request))))
      (and p1
           (resolver (make-request
                      (request-method request)
                      (request-uri-string request)
                      (let(
                          (u0 (request-uri request)))
                        (make-uri (uri-scheme u0)
                                 (uri-authority u0)
                                 p1
                                 (uri-query u0)
                                 (uri-fragment u0)))
                      (request-version request)
                      (request-header request)
                      (request-port request)))))))


(define (allow fn resolver)
  (lambda (request)
    (and (fn request) (resolver request))))


(define (deny fn resolver)
  (allow
   (lambda (req) (not (fn req)))
   resolver))
       