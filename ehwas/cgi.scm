;; move in another file
(##namespace ("ehwas-cgi#"))

(##include "~~/lib/gambit#.scm")

(include "response#.scm")
(include "request#.scm")
(include "rfc3986#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (fixnum)
         (block))

(define (cgi-resolver root)
  (lambda (r)
    (let(
         (mtd (request-method r)))
      (cond
       ((string=? mtd "GET") (cgi-get root r))
       ((string=? mtd "POST") (cgi-post root r))
       ((string=? mtd "PUT") (cgi-post root r))
       (else #f)))))


(define (cgi-get root r)
  (let(
       (qry (url-decode (uri-query (request-uri r))))
       (cmd (string-append root "/" (path->localstring (uri-path (request-uri request))))))
    (and (file-exists? cmd)
         (shell-command
          (call-with-output-string
           ""
           (lambda (p)
             (table-for-each
              (lambda (k v)
                (print port: p `(,(shell-encode k) "=" (shell-encode v) "\n")))
              qry)
             (print port: p cmd)))))))

(define (cgi-post root r)
  (let(
       (ats (rfc822-attributes
             (table-ref
              (request-header request)
              "Content-Type"
              ""))))
    (cond
     ((string=? (car ats) "multipart/form-data")
      (cgi-post0 ats r))
     ((string=? (car ats) "application/x-www-form-urlencoded")
      (cgi-post1 ats r))
     (else #f))))

(define (cgi-post0 root ats r)
  (let(
       (qry (data-decode
             (cdr (assoc "boundary" (cdr ats)))
             (request-port r)))
              (cmd (string-append root "/" (path->localstring (uri-path (request-uri request))))))
    (and (file-exists? cmd)
         (shell-command
          (call-with-output-string
           ""
           (lambda (p)
             (table-for-each
              (lambda (k v)
                (print port: p `("export ",(shell-encode k) "=" (shell-encode v) "\n")))
              qry)
             (print port: p cmd)))))))


(define (with-io prg)
  (let(
       (tmp "/tmp/")
       (i (string-append tmp (make-uid)))
       (o (string-append tmp (make-uid))))
    (make-fifo i)
    (make-fifo o)
    (shell-command (string-append "cat " i "|" cmd ">" o))
    (let(
         (res (call-with-input-file o (lambda (p) (read-line p #\nul)))))
      (delete-file i)
      (delete-file o)
      res)))

(define (shell-encode s)
  (list->string 
   (let enc ((s (string->list s)) (rs '()))
     (cond
      ((null? s) (reverse rs))
      ((char=? (car s) #\space)
       (enc (cdr s) (append '(#\space #\\) rs)))
      ((char=? (car s) #\=)
       (enc (cdr s) (append '(#\= #\\) rs)))
      (else
       (enc (cdr s) (cons (car s) rs)))))))