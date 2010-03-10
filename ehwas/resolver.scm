(##namespace ("ehwas-resolver#"))

(##include "~~/lib/gambit#.scm")

(include "response#.scm")
(include "request#.scm")
(include "rfc3986#.scm")
(include "errors#.scm")
(include "template#.scm")
(include "resolver#.scm")

(include "query#.scm")
(include "cookies#.scm")
(include "sessions#.scm")
(include "mime-types#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (fixnum)
         ;; (not safe)
         (block))

;; this parameter defines the size of buffer copying file parts
(define buffer-size (make-parameter 4096))

(define not-found-resolver
  (lambda (request) #f))

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
     (continuation-capture
      (lambda (k)
        (display-exception-in-context e k p))))))

;; (define (make-guarded-resolver res)
;;   (lambda (r)
;;     (with-exception-catcher
;;      (lambda (c) (http-error 500 r (exception->string c)))
;;      (lambda () (res r)))))

(define (path->localstring p)
  (if (null? p) ""
      (string-append
       (car p)
       (let conv ((p (cdr p)))
         (if (null? p) ""
             (string-append
              "/" (car p)
              (conv (cdr p))))))))

(define (directory? f)
  (eq? (file-type f) 'directory))

(define (regular? f)
  (eq? (file-type f) 'regular))

(define (with-buffered-printer response)
  (and response
       (let*(
             (printer (response-printer response))
             (buffer (##still-copy (call-with-output-u8vector (u8vector) printer)))
             (size (u8vector-length buffer)))
         (make-response
          (response-version response)
          (response-code response)
          (response-status response)
          (response-headers response)
          (lambda (p) (write-subu8vector buffer 0 size p))))))

;; (define cache-timeout (make-parameter (* 30 60)))

;; (define (make-cached-resolver resolve #!optional)
;;   (let(
;;        (m0 (make-mutex))
;;        (cache (make-table weak-values: #t
;;                           init: #f)))
;;     (lambda (request)
;;       (let*(
;;             (key (request-uri-string request))
;;             (val (table-ref cache key)))
;;         (or (and val (thread-send (cdr val) 'touch) (car val))
;;             (let*(
;;                   (orig (with-buffered-printer (resolve request)))
;;                   (val (cons orig #f)))
;;               (set-cdr! val
;;                         (thread-start!
;;                          (make-thread
;;                           (lambda ()
;;                             (let cont ()
;;                               (let(
;;                                    (c (thread-receive (cache-timeout) #f)))
;;                                 val
;;                                 (if c (cont))))))))
;;               (table-set! cache key val)
;;               orig))))))
            
;; (define (make-cached-resolver resolve)
;;   (let(
;;        (m0 (make-mutex))
;;        (cache (make-table weak-keys: #t
;;                           weak-values: #t
;;                           init: #f)))
;;     (lambda (request)
;;       (let(
;;            (key (request-uri-string request)))
;;        (define response)
;;        (mutex-lock! m0)
;;        (set! response (table-ref cache key))
;;        (cond
;;         ((mutex? response)
;;          (let ((m1 response))
;;            (mutex-unlock! m0)
;;            (mutex-lock! m1)
;;            (set! response (table-ref cache key))
;;            (mutex-unlock! m1)))

;;         ((not response)
;;          (let(
;;               (m1 (make-mutex)))
;;            (mutex-lock! m1)
;;            (table-set! cache key m1)
;;            (mutex-unlock! m0)
;;            (set! response (with-buffered-printer (resolve request)))
;;            (if response
;;                (table-set! cache key response)
;;                (table-set! cache key))
;;            (mutex-unlock! m1)))

;;         (else
;;          (mutex-unlock! m0)))
;;         response))))

            
(define (make-cached-resolver resolve)
  (let(
       (cache (make-table
               weak-values: #t
               init: #f)))
    (lambda (request)
      (let(
           (key (request-uri-string request)))
        (or (table-ref cache key)
            (let(
                 (resp (with-buffered-printer (resolve request))))
              (table-set! cache key resp)
              resp))))))

(define (make-filesystem-resolver root #!optional (dir? #f))
  (lambda (request)
    (if (null? (uri-path (request-uri request)))
        (and dir? (make-directory-response (request-version request) 200 "OK" "" (string-append root "/")))
        (let*(
              (local (path->localstring (uri-path (request-uri request))))
              (full (string-append root "/" local)))
          (cond
           ((not (file-exists? full)) #f)
           ((regular? full) (make-file-response request full))
           ((and (directory? full) dir?) (make-directory-response (request-version request) 200 "OK" local full))
           (else #f))))))


(define (make-file-response request full)
  (let*(
        (buf-size (buffer-size))
        (buffer (make-u8vector buf-size))
        (size (file-size full))
        (etag (number->string (time->seconds (file-last-modification-time full)) 16))
        (if-none-match (table-ref (request-header request) "If-None-Match" "")))
    (if (string=? etag if-none-match)
        (make-response
         (request-version request) 304 "Not Modified"
         (header)
         (text ""))
        (make-response
         (request-version request) 200 "OK"
         (header ("Content-type" (mime-type full))
                 ("Content-length" size)
                 ("Etag" etag)
                 ;; ("Pragma" "no-cache")
                 )
         (lambda (out)
           (call-with-input-file full
             (lambda (in)
               (let loop ((j 0))
                 (if (< j size)
                     (let(
                          (delta (read-subu8vector buffer 0 buf-size in)))
                       (write-subu8vector buffer 0 delta out)
                       (loop (+ j delta))))))))))))
  
(define (make-directory-response v c s local full)
  (let(
       (str (call-with-output-string
             ""
             (text
              "<xml version = \"1.0\">\n"
              (xml
               (xhtml (@ (xmln "http://www.w3.org/1999/xhtml"))
                      (body
                       ,(map (lambda (name)
                               (xml (p (a (@ (href ,(string-append local "/" name)))))))
                             (directory-files full)))))))))
    (make-response
     v c s
     (header
      ("Content-type" "text/html")
      ("Content-length" (string-length str))
      ("Pragma" "no-cache"))
     (text str))))

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
    ((table-ref table (request-path req) (lambda (_) #f)) req)))

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

(define (allow-resolver fn resolver)
  (lambda (req)
    (and (fn req) (resolver req))))

(define (deny-resolver fn resolver)
  (allow-resolver
   (lambda (req) (not (fn req)))
   resolver))


;; utility function for open a request and handle automatically
;; session/cookies/query stuff
(define with-request
  (let(
       (memo (make-table weak-keys: #t)))
    (lambda (req fn)
      (let*(
            (args
             (or (table-ref memo req #f)
                 (let*(
                       (path
                        (request-path req))
                       (query
                        (request-query req))
                       (cookies
                        (request-cookies req))
                       (sess-id
                        (and cookies (table-ref cookies "Session-id" #f)))
                       (session
                        (session-init sess-id))
                       (args (list path query cookies session)))
                   (table-set! memo req args)
                   args)))
            (response (apply fn args)))
        (if (response? response) (set-cookie! response "Session-id" (session-identifier (list-ref args 3)) `("path" . "/")))
        response))))
      
(define (with-request-keys req fn)
  (with-request
   req
   (lambda (path query cookies session)
     (fn path: path
         query: query
         cookies: cookies
         session: session))))

(define path (make-parameter #f))
(define query (make-parameter #f))
(define cookies (make-parameter #f))
(define session (make-parameter #f))

(define (call-with-request req fn)
  (with-request
   req
   (lambda (_path _query _cookies _session)
     (parameterize
         ((path _path)
          (query _query)
          (cookies _cookies)
          (session _session))
       (fn)))))

(define (redirect nt)
  (lambda (req)
    (response
     (request-version req) 302 "Found"
     (header
      ("Location" nt)
      ("Content-type" "text/html"))
     (text ""))))