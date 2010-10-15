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

(include "../ansuz/char-stream-parser#.scm")

(include "rfc822#.scm")
(include "rfc3986#.scm")
(include "request#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         ;;(not safe)
         )

(define ehwas-query-max-file-length (make-parameter (* 100 1024))) ;; default 100K

(include "../ansuz/sources/string#.scm")

;; parser that reads a single hex digit 0..9
;; author: francesco bracchi (frbracch@gmail.com)
(define-parser (dec-digit)
  (<- d (digit))
  (return (- (char->integer d) 48)))

;; parser that reads a single hex digit a..f
;; author: francesco bracchi (frbracch@gmail.com)

(define-parser (up-digit)
  (<- d (interval #\a #\f))
  (return (- (char->integer d) 87)))


;; parser that reads a single hex digit A..F
;; author: francesco bracchi (frbracch@gmail.com)
(define-parser (lo-digit)
  (<- d (interval #\A #\F))
  (return (- (char->integer d) 55)))

;; parser that reads a single hex digit
;; author: francesco bracchi (frbracch@gmail.com)             
(define-parser (hex-digit)
  (<> (dec-digit)
      (up-digit)
      (lo-digit)))

;; parser that reads a percent quoted char
;; author: francesco bracchi (frbracch@gmail.com)
(define-parser (urlencoded-quoted-char)
  (char #\%)
  (<- c0 (hex-digit))
  (<- c1 (hex-digit))
  (return (integer->char (+ (* 16 c0) c1))))

;; parser that reads urlencoded char
;; author: francesco bracchi (frbracch@gmail.com)
(define-parser (urlencoded-char)
  (<> (urlencoded-quoted-char)
      (>> (char #\+) (return #\space))
      (get-if
       (lambda (c)
         (not
          (or
           (eof-object? c)
           (char=? c #\nul)
           (char=? c #\&)
           (char=? c #\?)
           (char=? c #\=)
           (char=? c #\%)
           (char=? c #\:)
           ;; (char=? c #\@)
           ;; (char=? c #\/)
           (char=? c #\;)))))))

;; parser that reads utf8 chars
;; author: francesco bracchi (frbracch@gmail.com)

(define-parser (utf8-extended)
  (<- c (urlencoded-char))
  (if (char>? c #\x80)
      (return c)
      (fail 'stop-here)))
      
(define-parser (utf8-seq)
  (<- c (urlencoded-char))
  (if (char<? c #\x80)
      (return (list c))
      (>> (<- cs (upto 3 (utf8-extended)))
          (return (cons c cs)))))

(define-parser (utf8-char)
  (<- cs (utf8-seq))
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
             (bitwise-and (char->integer (cadddr cs)) #b00111111))))))))

;; parser that reads an urlencoded pair
;; author: francesco bracchi (frbracch@gmail.com)
(define-parser (urlencoded-pair)
  (<- k (urlencoded-key))
  (char #\=)
  (<- v (urlencoded-value))
  (return (cons (string->symbol k) v)))

;; parser that reads an urlencoded key
;; author: francesco bracchi (frbracch@gmail.com)
(define-parser (urlencoded-key)
  (<- c (utf8-char))
  (<- cs (kleene (utf8-char)))
  (return (list->string (cons c cs))))

(define-parser (urlencoded-value)
  (<- cs (kleene (utf8-char)))
  (return (list->string cs)))

;; read query and asserts it consumes the whole input stream
;; author: francesco bracchi (frbracch@gmail.com)
(define-parser (urlencoded-query)
  (<> (>> (<- p (urlencoded-pair))
          (<- ps (kleene (>> (char #\&) (urlencoded-pair))))
          ;; (return (for-each pp ps))
          (eos)
          (return (list->table (cons p ps))))
      (return (make-table))))


(define (url-decode #!optional (src (read-line (current-input-port) #f)))
  (run (urlencoded-query) src))

;;; application/x-www-form-urlencod
(define-parser (sp)
  (kleene (whitespace)))

(define-parser (quoted-char)
  (<> (>> (char #\\) (any))
      (get-if
       (lambda (c)
         (not
          (or
           (eof-object? c)
           (char=? c #\")
           (char=? c #\nul)))))))

(define-parser (free-char)
  (get-if
   (lambda (c)
     (not
      (or
       (eof-object? c)
       (char=? c #\space)
       (char=? c #\;)
       (char=? c #\newline)
       (char=? c #\return)
       (char=? c #\nul))))))

(define-parser (attribute-quoted)
  (char #\")
  (<- ps (kleene (quoted-char)))
  (char #\")
  (return (list->string ps)))

(define-parser (attribute-free)
  (<- l (kleene (free-char)))
  (return (list->string l)))

(define-parser (attribute-value)
  (<> (attribute-quoted)
      (attribute-free)))

(define-parser (attribute-key)
  (<- c (alpha))
  (<- cs (kleene (<> (alpha) (digit))))
  (return (list->string (cons c cs))))

(define-parser (attribute)
  (<- k (attribute-key))
  (sp) (char #\=) (sp)
  (<- v (attribute-value))
  (return (cons k v)))

(define-parser (attributes)
  (kleene
   (>> (sp)
       (char #\;)
       (sp)
       (attribute))))

(define-parser (parameter)
  (<- cs (kleene
          (get-if
           (lambda (c)
             (not
              (or
               (eof-object? c)
               (char=? c #\;)
               (char=? c #\newline)
               (char=? c #\return)
               (char=? c #\space)
                   (char=? c #\nul)))))))
  (return (list->string cs)))

(define-parser (with-attributes)
  (<- p (parameter))
  (<- as (attributes))
  (eos)
  (return (cons p as)))

(define (split-attributes s)
  (run (with-attributes) s))

(include "../ansuz/sources/port#.scm")

(define-parser (rfc822+)
  (<- h (rfc822))
  (return
   (let(
        (t1 (make-table)))
     (table-for-each (lambda (k v) (table-set! t1 k (split-attributes v))) h)
     t1)))

;; (define-parser (dataencoded-text-value boundary)
;;   (reflect (head tail row col pos datum sc fl)
;;            (begin
;;            (if (= (string-length boundary) 0)
;;                (fl "zero-length boundary")
;;                (let loop ((datum datum) (buf (make-string 64)) (pos 0))
;;                  (let(
;;                       (buf (if (>= pos (string-length buf)) (string-append buf (make-string (string-length buf))) buf))
;;                       (c (head datum)))
;;                    (cond
;;                     ((eof-object? c) (fl "end of file reached"))
;;                     ((eq? c (string-ref boundary 0))
;;                      (let iloop ((d (tail datum)) (p 1))
;;                        (let(
;;                             (x (head d)))
;;                          (cond
;;                           ((>= p (string-length boundary))
;;                            (sc (substring buf 0 pos) d fl))
;;                           ((eof-object? x)
;;                            (fl "end of file reached"))
;;                           ((eq? x (string-ref boundary p))
;;                            (iloop (tail d) (+ p 1)))
;;                           (else
;;                            (string-set! buf pos c)
;;                            (loop (tail datum) buf (+ pos 1)))))))
;;                     (else
;;                      (string-set! buf pos c)
;;                      (loop (tail datum) buf (+ pos 1))))))))))

(define-parser (dataencoded-value boundary)
  (reflect (st sc fl)
           (begin
             (if (= (string-length boundary) 0)
                 (fl "zero-length boundary" st sc)
                 (let loop ((st st) (buf (make-u8vector 64)) (pos 0))
                   (let(
                        (buf (if (>= pos (u8vector-length buf)) (u8vector-append buf (make-u8vector (u8vector-length buf))) buf))
                        (c (stream-car st)))
                   (cond
                    ((>= pos (ehwas-query-max-file-length)) (fl "length exceded max file length" st sc))
                    ((eof-object? c) (fl "end of file reached" st sc))
                    ((eq? c (string-ref boundary 0))
                     (let iloop ((d (stream-cdr st)) (p 1))
                       (let(
                            (x (stream-car d)))
                         (cond
                          ((>= p (string-length boundary))
                           (sc (subu8vector buf 0 pos) d fl))
                          ((eof-object? x)
                           (fl "end of file reached" d sc))
                          ((eq? x (string-ref boundary p))
                           (iloop (stream-cdr d) (+ p 1)))
                          (else
                           (u8vector-set! buf pos (char->integer c))
                           (loop (stream-cdr st) buf (+ pos 1)))))))
                    (else
                     (u8vector-set! buf pos (char->integer c))
                     (loop (stream-cdr st) buf (+ pos 1))))))))))

(define-parser (dataencoded-pair b)
  (<- h (rfc822+))
  (let*(
        (cd (table-ref h 'Content-Disposition #f)) ;; (assoc "Content-Disposition" h))
        (as (cdr cd))
        (name (string->symbol (cdr (assoc "name" as)))))
    (>> (<- c (dataencoded-value b))
        (return (cons name
                      (if (assoc "filename" as)
                          c
                          (call-with-input-u8vector c (lambda (p) (read-line p #f)))))))))

(define-parser (dataencoded-query b)
  (word b)
  (char #\newline)
  (<- c (dataencoded-pair b))
  (<- cs (kleene (>> (char #\newline) (dataencoded-pair b))))
  (word "--")
  (return (cons c cs)))

(define (data-decode boundary #!optional (src (current-input-port)))
  (list->table
   (run (dataencoded-query (string-append "\n--" boundary))
        src)))

;; (define (get-boundary v)
;;   (cdr
;;    (assoc "boundary"
;;           (cdr
;;            (run (with-attributes) (->source v))))))


;; (define (get-boundary v)
;;   (cdr (assoc "boundary" (cdr (split-attributes v)))))

;; (define (rfc822-attributes v)
;;   (run (with-attributes) v))

;; this is very dirty about using buffered ports
;; (define (string->u8vector s)
;;   (call-with-output-u8vector
;;    (list char-encoding: 'UTF-8)
;;    (lambda (p)
;;      (print port: p s))))

;; (define (read-buffer port)
;;   (let*(
;;         (ln (input-port-characters-buffered port))
;;         (s (make-string ln)))
;;     (read-substring s 0 ln port)
;;     (string->u8vector s)))

;;(define (request-parse-query request)

(define (make-request-query request)
  (let(
       (mtd (request-method request))
       (ats (let(
		 (ct (assoc 'Content-type (request-header request))))
	      (if ct (split-attributes (cdr ct)) '()))))
    (cond
     ((eq? mtd 'GET)
      (url-decode (uri-query (request-uri request))))
     
     ((and (eq? mtd 'POST)
           (string=? (car ats) "multipart/form-data"))
      (port-settings-set! (current-input-port) (list char-encoding: 'ASCII))
      (data-decode (cdr (assoc "boundary" (cdr ats)))))
     
     ((and (eq? mtd 'POST)
           (string=? (car ats) "application/x-www-form-urlencoded"))
      (let(
           (s (make-string (min (let(
				     (len (assoc 'Content-length (request-header request))))
				  (if len (string->number len) 0))
                                (ehwas-query-max-file-length)))))
        (read-substring s 0 (string-length s))
        (url-decode s)))
     (else
      (make-table)))))

(define *-memo-* (make-table weak-keys: #t))

(define (request-query request)
  (or (table-ref *-memo-* request #f)
      (let (
            (query (make-request-query request)))
        (table-set! *-memo-* request query)
        query)))
