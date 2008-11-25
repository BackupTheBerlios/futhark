(##namespace ("ehwas-pages#"))

(##include "~~/lib/gambit#.scm")

(include "ansuz-language#.scm")
(include "ansuz-kernel#.scm")
(include "ansuz-extras#.scm")
(include "ansuz-streams#.scm")

(include "ehwas-request#.scm")
(include "ehwas-errors#.scm")
(include "ehwas-response#.scm")
(include "rfc3986#.scm")
(include "ehwas-resolver#.scm")
 
(declare (standard-bindings)
         (extended-bindings)
         (block)
         (fixnum)
         ;; (not safe)
         )

(define ehwas-pages-compiler (make-parameter "gsc"))
(define ehwas-pages-compile? (make-parameter #t))
(define ehwas-pages-includes
  (make-parameter
  (map (lambda (c)
         (string-append (current-directory) c))
       `("ehwas-request#.scm"
         "ehwas-response#.scm"
         "rfc3986#.scm"
         "ehwas-query#.scm"
         "ehwas-cookies#.scm"
         "ehwas-sessions#.scm"))))

(define path-separator
  (make-parameter
   (let*(
         (cd (current-directory)))
     (string (string-ref cd (- (string-length cd) 1))))))
         

(define ehwas-pages-header
  (make-parameter
   (string-append (current-directory) "ehwas-header#.scm")))

(define-parser (more-text) 
  (<> (>> (<> (eos) (word "<?scheme")) (return '()))
;;       (>> (<- c (<> (char #\") (char #\\)))
;;           (<- cs (more-text))
;;           (return (cons #\\ (cons c cs))))
      (>> (<- c (any))
          (<- cs (more-text))
          (return (cons c cs)))))

(define-parser (text)
  (>> (<- l (more-text))
      (return (lambda (p)
                (if (not (null? l))
                    (begin
                      (display "(echo " p)
                      (write (list->string l) p)
                      (display ")" p)))))))
  
(define-parser (more-string)
  (<> (>> (char #\")
          (return (list #\")))
      (>> (char #\\)
          (<- c (any))
          (<- cs (more-string))
          (return (cons #\\ (cons c cs))))
      (>> (<- c (any))
          (<- cs (more-string))
          (return (cons c cs)))))

(define-parser (_string)
  (>> (char #\")
      (<- cs (more-string))
      (return (cons #\" cs))))

(define-parser (more-scheme)
  (<> (>> (word "?>")
          (return '()))
      
      (>> (<- s (_string))
          (<- cs (more-scheme))
          (return (append s cs)))
      
      (>> (<- c (any))
          (<- cs (more-scheme))
          (return (cons c cs)))))

(define-parser (scheme)
  (>> (<- l (more-scheme))
      (return (lambda (p) (display l p)))))

(define-parser (server-page)
  (>>
   (<- t0 (text))
   (<- ss (more-server-page))
   (return
    (lambda (p)
      (t0 p)
      (ss p)))))

(define-parser (more-server-page)
  (<> (>> (<- t0 (scheme))
          (<- s0 (text))
          (<- ss (more-server-page))
          (return
           (lambda (p)
             (t0 p)
             (s0 p)
             (ss p))))
      (>> (eos)
          (return (lambda (p) 'nothing)))))

(define (serverpage->scheme if of)
  (call-with-input-file if
    (lambda (ip)
      (call-with-output-file of
        (run (server-page) (port->stream ip))))))

;; (define (scheme->application if of)
;;   (call-with-output-file of
;;     (lambda (op)
;;       (display
;;        (list
;;         (map (lambda (s) `("(##include \"" ,s "\")\n")) (ehwas-pages-includes))
;;         "(ehwas-pages#registry-set! \"" (path-strip-extension if) "\" (lambda (request)\n"
;;         "(include \"" (ehwas-pages-header) "\")\n"
;;         "(include \"" (path-strip-directory if) "\")\n"
;;         "response))")
;;         op))))
               
(define (scheme->application if of)
  (call-with-output-file of
    (lambda (op)
      (for-each
       (lambda (s)
         (write `(##include ,s) op)
         (newline op))
       (ehwas-pages-includes))
      (display "(ehwas-pages#registry-set! " op)
      (write (path-strip-extension if) op)
      (display "(lambda (request)" op)
      (newline op)
      (write `(include ,(ehwas-pages-header)) op)
      (newline op)
      (write `(include ,if) op)
      (newline op)
      (display "response))" op))))
  
; (define (application->object if of)
;   (let(
;        (r (shell-command (string-append "gsc -o " of " " if))))
;     (if (eq? r 0)
;         'ok
;         (raise '(gsc goes wrong)))))

(define (application->object if of)
  (shell-command (string-append (ehwas-pages-compiler) " -o " of " " if)))

(define *-registry-*
  (make-table init: #f))

;; mutex object that make modifications synchronous
(define *-registry-mutex-* (make-mutex))

;; function used by compiled serverpage code to
;; register itself to the global *-registry-* table
(define (registry-set! file handler)
  (mutex-lock! *-registry-mutex-*)
  (table-set! *-registry-* file (cons (time->seconds (file-last-modification-time file)) handler))
  (mutex-unlock! *-registry-mutex-*))

;; tests if the version of compiled code is older than serverpage (file)
(define (dirty? file)
  (let(
       (scm (string-append file ".scm")))
    (or (not (file-exists? scm))
        (let(
             (e-time (time->seconds (file-last-modification-time file)))
             (c-time (time->seconds (file-last-modification-time scm))))
          (> e-time c-time)))))

;; used by serverpage-resolver to resolve the path.
;; if not already present into the global *-registry-* tries to load it
(define (registry-get file)
  (define entry)
  (mutex-lock! *-registry-mutex-*)
  (set! entry (table-ref *-registry-* file))
  (mutex-unlock! *-registry-mutex-*)
  
  (cond
   ((and (not entry) (file-exists? file))
    (serverpage-compile file))

   ((and entry
         (file-exists? file)
         (> (time->seconds (file-last-modification-time file)) (car entry)))
    (serverpage-compile file))

   ((file-exists? file) (cdr entry))
   (else (lambda (req) #f))))

;; (define (serverpage-error x)
;;   (lambda (request)
;;     (call-with-response
;;      (make-response (request-version request) 300 "SERVERPAGE_ERROR")
;;      (lambda (response)
;;        (response-set! response "Content-type" "application/xhtml+xml")
;;        (response-display!
;;         response
;;         (list "<?xml version = \"1.0\" ?>\n"
;;               "<html xmlns = \"http://www.w3.org/1999/xhtml\">\n"
;;               "<head><title>Ingwaz Exception</title></head>\n"
;;               "<body><center>"
;;               "<h1>IngWaz Exception</h1>"
;;               "<p><![CDATA["
;;               (call-with-output-string "" (lambda (p) (##display-exception x p)))
;;               "]]></p>"
;;               "</center>"
;;               "</body>"
;;               "</html>"))))))

(define (serverpage-compile file)
  (let(
       (scm (string-append file ".scm"))
       (app (string-append file "-app.scm"))
       (obj (string-append file "-"
                           (number->string
                            (inexact->exact
                             (round
                              (time->seconds (file-last-modification-time file)))))
                           ".o1")))
    (serverpage->scheme file scm)
    (scheme->application scm app)
    (if (ehwas-pages-compile?)
        (begin
          (application->object app obj)
          (load obj)
          (for-each delete-file (list app obj scm))
          )
        (begin
          (load app)
          (for-each delete-file (list app scm))
          ))
    (cdr (table-ref *-registry-* file))))

(define (path->localstring p)
  (if (null? p) ""
      (string-append
       (car p)
       (let conv ((p (cdr p)))
         (if (null? p) ""
             (string-append
              (path-separator) (car p)
              (conv (cdr p))))))))


;; creates a web server resolver
;; if the file has extension ".sp" then it is interpreted
;; as a server page.
;; scm and o1 are blocked signalling error 403 (Forbidden access)
;; (todo move this feature in a new resolver and couple with this through an orelse-resolver)
;; other extensions are ignored

(define (make-serverpage-resolver root)
  (let(
       (root (path-expand root)))
    (lambda (request)
      (and (not (null? (uri-path (request-uri request))))
           (let*(
                 (local (path->localstring (uri-path (request-uri request))))
               (full (string-append root (path-separator) local)))
             ((registry-get full) request))))))
