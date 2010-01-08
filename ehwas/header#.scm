(define-macro (wait e)
  (let(
       (k (gensym 'k)))
    `(let(
          (,k #f))
       (lambda ()
         (if ,k ,k
             (begin
               (set! ,k ,e)
               ,k))))))

(define response
  (make-empty-response (request-version request) 200 "OK"))

(define cookies
  (wait (or (request-cookies request)
            (make-table))))

;; (define query
;;   (wait
;;    (let(
;;         (mtd (request-method request))
;;         (ats (rfc822-attributes
;;               (table-ref
;;                (request-header request)
;;                "Content-Type"
;;                ""))))
;;      (cond
;;       ((string=? mtd "GET")
;;        (url-decode (uri-query (request-uri request))))
      
;;       ((and (string=? mtd "POST")
;;             (string=? (car ats) "multipart/form-data"))
;;        (data-decode
;;         (cdr (assoc "boundary" (cdr ats)))
;;         (request-port request)))
      
;;       ((and (string=? mtd "POST")
;;             (string=? (car ats) "application/x-www-form-urlencoded"))
;;        (let*(
;;              (len (string->number
;;                    (table-ref
;;                     (request-header request)
;;                     "Content-Length")))
;;              (buf (make-string len)))
;;          (read-substring buf 0 len (request-port request))
;;          (url-decode buf)))

;;       (else
;;        (error "unknown form data encoding"))))))

(define query (wait (request-query request)))

(define session
  (wait
   (let*(
         (uid (table-ref (cookies) "Session-uid" #f))
         (s (session-init uid)))
     (set-cookie! response "Session-uid" (session-identifier s))
     s)))

(define-macro (echo w)
  `(response-append response (displayer ,w)))

(define-macro (header-set! k v)
  `(response-header-set! response ,k ,v))

(define-macro (cookie-set! k . v)
  `(begin
     (table-set! (cookies) ,k ,@v)
     (set-cookie! response ,k ,@v)))

(define-macro (cookie-ref k . v)
  `(table-ref (cookies) ,k ,@v))

(define-macro (query-ref k . v)
  `(table-ref (query) ,k ,@v))

(define-macro (query-set! k . v)
  `(table-set! (query) ,k ,@v))
