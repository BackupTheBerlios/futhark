;; file: query.scm
;; description:
;; utility that parses a query and returns a table
;; exports:
;; query is a the parser
;; string->query transforms a string in a table
;; author: francesco bracchi (frbracch@gmail.com)
;; transforming query

(namespace ("ehwas-query#"))

(##include "~~/lib/gambit#.scm")

(include "ansuz-language#.scm")
(include "ansuz-kernel#.scm")
(include "ansuz-streams#.scm")
(include "ansuz-extras#.scm")
(include "rfc822#.scm")

(include "rfc3986#.scm")
(include "ehwas-request#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         (not safe))

(define ehwas-query-max-file-length (make-parameter (* 50 1000 1000)))

;; parser that reads a single hex digit 0..9
;; author: francesco bracchi (frbracch@gmail.com)
(define-parser (dec-digit)
  (>> (<- d (digit))
      (return (- (char->integer d) 48))))

;; parser that reads a single hex digit a..f
;; author: francesco bracchi (frbracch@gmail.com)
(define-parser (up-digit)
  (>> (<- d (interval #\a #\f))
      (return (- (char->integer d) 87))))

;; parser that reads a single hex digit A..F
;; author: francesco bracchi (frbracch@gmail.com)
(define-parser (lo-digit)
  (>> (<- d (interval #\A #\F))
      (return (- (char->integer d) 55))))

;; parser that reads a single hex digit
;; author: francesco bracchi (frbracch@gmail.com)             
(define-parser (hex-digit)
  (<> (dec-digit)
      (up-digit)
      (lo-digit)))

;; parser that reads a percent quoted char
;; author: francesco bracchi (frbracch@gmail.com)
(define-parser (urlencoded-quoted-char)
  (>> (char #\%)
      (<- c0 (hex-digit))
      (<- c1 (hex-digit))
      (return (integer->char (+ (* 16 c0) c1)))))

;; parser that reads urlencoded char
;; author: francesco bracchi (frbracch@gmail.com)
(define-parser (urlencoded-char)
  (<> (urlencoded-quoted-char)
      (>> (char #\+) (return #\space))
      (test-token
       (lambda (c)
         (not
          (or
           (char=? c #\nul)
           (char=? c #\&)
           (char=? c #\?)
           (char=? c #\=)
           (char=? c #\%)
           (char=? c #\:)
           ;; (char=? c #\@)
           (char=? c #\/)
           (char=? c #\;)))))))

;; parser that reads utf8 chars
;; author: francesco bracchi (frbracch@gmail.com)

(define-parser (utf8-extended)
  (>> (<- c (urlencoded-char))
      (if (char>? c #\x80)
          (return c)
          (fail 'stop-here))))
      
(define-parser (utf8-seq)
  (>> (<- c (urlencoded-char))
      (if (char<? c #\x80)
          (return (list c))
          (>> (<- cs (upto 3 utf8-extended))
              (return (cons c cs))))))
                  
(define-parser (utf8-char)
  (>> (<- cs (utf8-seq))
      (return 
       (let(
            (l (length cs)))
         (cond
          ((= l 1) (car cs))
          ((= l 2)
           (integer->char
            (bitwise-ior
             (arithmetic-shift (bitwise-and (char->integer (car cs))   #b00011111) 6)
             (bitwise-and (char->integer (cadr cs)) #b00111111))))
          ((= l 3)
           (integer->char
            (bitwise-ior
             (arithmetic-shift (bitwise-and (char->integer (car cs))   #b00001111) 12)
             (arithmetic-shift (bitwise-and (char->integer (cadr cs))  #b00111111) 6)
             (bitwise-and (char->integer (caddr cs)) #b00111111))))
          (else
           (integer->char
            (bitwise-ior
             (arithmetic-shift (bitwise-and (char->integer (car cs))   #b00000111) 18)
             (arithmetic-shift (bitwise-and (char->integer (cadr cs))  #b00111111) 12)
             (arithmetic-shift (bitwise-and (char->integer (caddr cs)) #b00111111) 6)
             (bitwise-and (char->integer (cadddr cs)) #b00111111)))))))))
             
                     

;; parser that reads an urlencoded pair
;; author: francesco bracchi (frbracch@gmail.com)
(define-parser (urlencoded-pair)
  (>> (<- k (urlencoded-key))
      (char #\=)
      (<- v (urlencoded-value))
      (return (cons k v))))

;; parser that reads an urlencoded key
;; author: francesco bracchi (frbracch@gmail.com)
(define-parser (urlencoded-key)
  (>> (<- c (utf8-char))
      (<- cs (kleene utf8-char))
      (return (list->string (cons c cs)))))
;; (define-parser (urlencoded-key)
;;   (>> (<- c (urlencoded-char))
;;       (<- cs (kleene urlencoded-char))
;;       (return (list->string (cons c cs)))))

;; parser that reads an urlencoded value
;; author: francesco bracchi (frbracch@gmail.com)
;; (define-parser (urlencoded-value)
;;   (>> (<- c (urlencoded-char))
;;       (<- cs (kleene urlencoded-char))
;;       (return (list->string (cons c cs)))))
(define-parser (urlencoded-value)
  (>> (<- c (utf8-char))
      (<- cs (kleene utf8-char))
      (return (list->string (cons c cs)))))

;; read query and asserts it consumes the whole input stream
;; author: francesco bracchi (frbracch@gmail.com)
(define-parser (urlencoded-query)
  (<> (>> (<- p (urlencoded-pair))
          (<- ps (kleene (parser () (>> (char #\&) (urlencoded-pair)))))
          (eos)
          (return (list->table (cons p ps))))
      (return (make-table))))

(define-parser (sp)
  (kleene whitespace))

(define-parser (quoted-char)
  (<> (>> (char #\\) (any))
      (test-token
       (lambda (c)
         (not
          (or
           (char=? c #\")
           (char=? c #\nul)))))))

(define-parser (free-char)
  (test-token
   (lambda (c)
     (not
      (or
       (char=? c #\space)
       (char=? c #\;)
       (char=? c #\newline)
       (char=? c #\return)
       (char=? c #\nul))))))

(define-parser (attribute-quoted)
  (>> (char #\")
      (<- ps (kleene quoted-char))
      (char #\")
      (return (list->string ps))))

(define-parser (attribute-free)
  (>> (<- l (kleene free-char))
      (return (list->string l))))

(define-parser (attribute-value)
  (<> (attribute-quoted)
      (attribute-free)))

(define-parser (attribute-key)
  (>> (<- c (alpha))
      (<- cs (kleene (parser () (<> (alpha) (digit)))))
      (return (list->string (cons c cs)))))

(define-parser (attribute)
  (>> (<- k (attribute-key))
      (sp) (char #\=) (sp)
      (<- v (attribute-value))
      (return (cons k v))))

(define-parser (attributes)
  (kleene
   (parser ()
           (>> (sp)
               (char #\;)
               (sp)
               (attribute)))))

(define-parser (parameter)
  (>> (<- cs (kleene
             (parser ()
              (test-token
               (lambda (c)
                 (not
                  (or
                   (char=? c #\;)
                   (char=? c #\newline)
                   (char=? c #\return)
                   (char=? c #\space)
                   (char=? c #\nul))))))))
      (return (list->string cs))))

(define-parser (with-attributes)
  (>> (<- p (parameter))
      (<- as (attributes))
      (eos)
      (return (cons p as))))

(define-parser (rfc822+)
  (>> (<- h (rfc822))
      (return
       (map
        (lambda (p)
          (cons (car p)
                (run (with-attributes)
                     (string->stream (cdr p)))))
        h))))

(define-parser (dataencoded-text-value r b)
  (if (> r 0)
      (<> (>> (word "\n--") (word b) (return '()))
          (>> (<- c (any))
              (<- cs (dataencoded-text-value (- r 1) b))
              (return (cons c cs))))
      (fail "dataencoded text too long")))
  
(define-parser (dataencoded-pair b)
  (>> (<- h (rfc822+))
      (let*(
            (cd (assoc "Content-Disposition" h))
            (as (cddr cd))
            (name (cdr (assoc "name" as))))
        (>> (<- c (dataencoded-text-value (ehwas-query-max-file-length) b))
            (return (cons name (list->string c)))))))

(define-parser (dataencoded-query b)
  (>> (word "--")
      (word b)
      (char #\newline)
      (<- c (dataencoded-pair b))
      (<- cs (kleene (parser () (>> (char #\newline) (dataencoded-pair b)))))
      (word "--")
      (return (cons c cs))))

(define (url-decode str)
  (run (urlencoded-query) (string->stream str)))

(define (data-decode boundary port)
  (list->table
   (run (dataencoded-query boundary)
        (port->stream port))))

(define (get-boundary v)
  (cdr
   (assoc "boundary"
          (cdr
           (run (with-attributes) (string->stream v))))))

(define (rfc822-attributes v)
  (run (with-attributes) (string->stream v)))

(define (request-parse-query request)
  (let(
       (mtd (request-method request))
       (ats (rfc822-attributes
             (table-ref
              (request-header request)
              "Content-Type"
              ""))))
    (cond
     ((string=? mtd "GET")
      (url-decode (uri-query (request-uri request))))
     
     ((and (string=? mtd "POST")
           (string=? (car ats) "multipart/form-data"))
      (with-exception-catcher
       (lambda (ex) (pp ex) (raise ex))
       (lambda () 
         (data-decode
          (cdr (assoc "boundary" (cdr ats)))
          (request-port request)))))
     
     ((and (string=? mtd "POST")
           (string=? (car ats) "application/x-www-form-urlencoded"))
      (let*(
            (len (string->number
                  (table-ref
                   (request-header request)
                   "Content-Length")))
            (buf (make-string len)))
        (read-substring buf 0 len (request-port request))
        (url-decode buf)))

     (else
      (make-table)))))
