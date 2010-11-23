(##namespace ("ehwas-combinators#"))

(##include "~~/lib/gambit#.scm")

(include "response#.scm")
(include "request#.scm")
(include "rfc3986#.scm")
(include "errors#.scm")

(include "query#.scm")
(include "cookies#.scm")
(include "sessions#.scm")
(include "mime-types#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (fixnum)
         ;; (not safe)
         (block))

(define (orelse . rs)
  (lambda (request)
    (let test ((rs rs))
      (and (not (null? rs))
           (or ((car rs) request)
               (test (cdr rs)))))))


(define (cache handler)
  (let(
       (*cache* (make-table weak-values: #t test: equal?)))
    (lambda (req)
      (with-exception-catcher pp (lambda () 
      (let(
           (key (request-uri-string req)))
        (or (table-ref *cache* key #f)
            (let(
                 (res (handler req)))
              (if (and (response? res) (equal? (response-code res) 200))
                  (let*(
                        (data (with-output-to-u8vector
			       (u8vector)
			       (response-writer res)))
			(res (make-response
			      200 "OK" 
			      `(("Content-length" . ,(u8vector-length data)) ,@(response-header res))
			      (lambda () (write-subu8vector data 0 (u8vector-length data))))))
                    (table-set! *cache* key res)
                    res)
                  res)))))))
))
(define (regular-file? f)
  (and (file-exists? f)
       (eq? (file-type f) 'regular)))

(define (filesystem root #!key (buffer-size 16384))
  (lambda (request)
    (let*(
	  (local-path (map (lambda (t) (string-append "/" (symbol->string t))) (request-path request)))
	  (path (apply string-append (cons root local-path))))
      (and (file-exists? path)
	   (eq? (file-type path) 'regular)
	   (let(
		(buffer (make-u8vector buffer-size))
		(size (file-size path))
		(etag (number->string
		       (time->seconds
			(file-last-modification-time path))))
		(if-none-match (assq 'If-None-Match (request-header request))))
	     (if (and if-none-match (equal? etag (cdr if-none-match)))
		 (response 304 "Not Modified" (header) 'nothing)
		 (response
		  200 "OK"
		  (header
		   Content-type: (mime-type path)
		   Content-length: size
		   Etag: etag)
		  (call-with-input-file path
		    (lambda (port)
		      (let buffer-write ((j 0))
			(if (< j size)
			    (let(
				 (delta (read-subu8vector buffer 0 buffer-size port)))
				(write-subu8vector buffer 0 delta)
				(buffer-write (+ j delta))))))))))))))

(define (with-table table)
  (lambda (request)
    ((table-ref table (request-path request) (lambda (request) #f)) request)))

(define (request-with-new-path request path)
  (make-request
   (request-method request)
   (request-uri-string request)
   (let(
        (uri (request-uri request)))
     (make-uri
      (uri-scheme uri)
      (uri-authority uri)
      path
      (uri-query uri)
      (uri-fragment uri)))
   (request-version request)
   (request-header request)))
  
(define (with-index file handler)
  (lambda (request)
    (let(
         (reverse-path (reverse (request-path request))))
      (and (pair? reverse-path)
           (string=? (car reverse-path) "")
           (handler
            (request-with-new-path request (reverse (cons file (cdr reverse-path)))))))))


(define (strip-path-prefix path prefix)
  (cond
   ((null? prefix) path)
   ((null? path) #f)
   ((eq? (car prefix) (car path)) (strip-path-prefix (cdr path) (cdr prefix)))
   (else #f)))

(define (with-prefix prefix handler)
  (lambda (request)
    (and prefix
         (handler (request-with-new-path request (strip-path-prefix (request-path request) prefix))))))

(define (allow test? handler)
  (lambda (request)
    (and (test? request) (handler request))))

(define (deny test? handler)
  (lambda (request)
    (if (test? request) #f (handler request))))

(define (get? request)
  (eq? (request-method request) 'GET))

(define (post? request)
  (eq? (request-method request) 'POST))

(define (redir to)
  (response
   302 "Found"
   (header 
    Location: to
    Content-type: "text/plain")
   'nothing))

(define (redirect to)
  (lambda (req) (redir to)))
   
(define (catch-exception exception-handler handler)
  (lambda (request)
    (with-exception-catcher
     (lambda (ex) (exception-handler request ex))
     (lambda () (handler request)))))

;; move somewhere else
;; (include "../encode/base64#.scm")
;; (include "mime-types#.scm")
