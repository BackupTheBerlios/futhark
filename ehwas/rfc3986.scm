;; METTERE IPV6 !!! e ipvfuture
;; quest'ultima è cazzata, ma per ipv6
;; tenere conto che dopo l'ellissi
;; ogni numero potrerbbe essere trattato come
;; ipv4 o come un altro blocco ipv6

(##namespace ("rfc3986#"))

(##include "~~/lib/gambit#.scm")
(include "../ansuz/language#.scm")
(include "../ansuz/kernel#.scm")
(include "../ansuz/extras#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         ;;(not safe)
         (fixnum))

(define-structure uri scheme authority path query fragment)

(define (fold-left f i l)
  (let fold ((i i) (l l))
    (if (null? l) i
        (fold (f i (car l)) (cdr l)))))

(define digit->number
  (let(
       (d0 (char->integer #\0)))
    (lambda (d)
      (- (char->integer d) d0))))

(define locase->number
  (let(
       (d0 (- (char->integer #\a) 10)))
    (lambda (c)
      (- (char->integer c) d0))))

(define upcase->number
  (let(
       (d0 (- (char->integer #\A) 10)))
    (lambda (c)
      (- (char->integer c) d0))))

(define-parser (digit-number)
               (>> (<- d (digit))
                   (return (digit->number d))))

(define-parser (locase-number)
                 (>> (<- c (interval #\a #\f))
                     (return (locase->number c))))

(define-parser (upcase-number)
                 (>> (<- c (interval #\A #\F))
                     (return (upcase->number c))))

(define-parser (hex-digit-number)
               (<> (digit-number)
                   (upcase-number)
                   (locase-number)))

(define-parser (gen-delims)
               (<> (char #\:)
                   (char #\/)
                   (char #\?)
                   (char #\#)
                   (char #\@)
                   (char #\[)
                   (char #\])))

(define-parser (sub-delims)
               (<> (char #\!)
                   (char #\$)
                   (char #\&)
                   (char #\')
                   (char #\()
                   (char #\))
                   (char #\*)
                   (char #\+)
                   (char #\,)
                   (char #\;)
                   (char #\=)))

(define-parser (reserved)
                 (<> (gen-delims)
                     (sub-delims)))
  
(define-parser (unreserved)
               (<> (alpha)
                   (digit)
                   (char #\-)
                   (char #\.)
                   (char #\_)
                   (char #\~)))

(define-parser (pct-encoded)
               (>> (char #\%)
                   (<- n0 (hex-digit-number))
                   (<- n1 (hex-digit-number))
                   (return (integer->char (+ (* 16 n0) n1)))))

(define-parser (port)
                 (>> (<- c (digit-number))
                     (<- cs (upto 5 digit-number))
                     (let(
                          (p (fold-left (lambda (i c) (+ c (* i 10))) c cs)))
                       (if (<= p 65535) (return p) (fail "port number too high")))))


(define-parser (scheme)
  (>> (<- c (alpha))
      (<- cs (kleene (parser () (<> (alpha)
                                    (digit)
                                    (char #\+)
                                    (char #\-)
                                    (char #\.)))))
      (return (list->string (cons c cs)))))

(define-parser (userinfo)
  (>> (<- cs (kleene (parser () (<> (unreserved)
                                    (pct-encoded)
                                    (sub-delims)
                                    (char #\:)))))
      (return (list->string cs))))

(define-parser (h8)
  (>> (<- cs (repeat-max 1 3 digit-number))
      (let(
           (v (fold-left (lambda (i t) (+ t (* 10 i))) 0 cs)))
        (if (and (>= v 0) (<= v 255))
            (return v)
            (fail "h8")))))

(define-parser (ipv4-list)
  (>> (<- b (h8))
      (<- bs (times 3 (parser () (>> (char #\.) (h8)))))
      (return (cons b bs))))

(define-parser (ipv4)
  (>> (<- bs (ipv4-list))
      (return `(ip 4 ,@bs))))

(define-parser (hostname)
  (>> (<- cs (kleene (parser ()
                             (<> (unreserved)
                                 (pct-encoded)
                                 (sub-delims)))))
      (return (list->string cs))))

(define-parser (host)
  (<> (ipv4)
      (hostname)))

(define-parser (authority)
  (>> (<- ui (maybe (parser ()
                            (>> (<- u (userinfo))
                                (char #\@)
                                (return u)))))
      (<- h (host))
      (<- n (maybe (parser ()
                           (>> (char #\:)
                               (port)))))
      (return (append
               (if (null? ui) '() `((userinfo ,@ui)))
               `((host ,@h))
               (if (null? n) '() `((port ,@n)))))))

(define-parser (pchar)
  (<> (unreserved)
      (pct-encoded)
      (sub-delims)
      (char #\@)
      (char #\:)))

(define-parser (segment)
  (>> (<- cs (kleene pchar))
      (return (list->string cs))))

(define-parser (segment-nz)
  (>> (<- c (pchar))
      (<- cs (kleene pchar))
      (return (list->string (cons c cs)))))
  
(define-parser (segment-nz-nc)
  (>> (<- c (<> (pchar) (char #\:)))
      (<- cs (kleene (parser ()
                             (<> (pchar)
                                 (char #\:)))))
      (return (list->string (cons c cs)))))

(define-parser (path-abempty)
  (<> (>> (char #\/)
          (<- s (segment))
          (<- ss (path-abempty))
          (return (cons s ss)))
      (return '())))

(define-parser (path-absolute)
  (<> (>> (char #\/)
          (<- s (segment-nz))
          (<- ss (path-abempty))
          (return `(,s ,@ss)))
      (>> (char #\/) (return '()))))

(define-parser (path-noscheme)
  (>> (<- s (segment-nz-nc))
      (<- ss (path-abempty))
      (return (cons s ss))))

(define-parser (path-rootless)
  (>> (<- s (segment-nz))
      (<- ss (path-abempty))
      (return (cons s ss))))

(define-parser (path-empty)
  (return '()))

(define-parser (path)
  (<> (path-abempty)
      (path-absolute)
      (path-noscheme)
      (path-rootless)
      (return '())))
                                        ;                 (path-empty)))))

(define-parser (query)
  (<>  (>> (char #\?)
           (<- cs  (kleene (parser ()
                           (<> (pchar)
                               (char #\/)
                               (char #\?)))))
           (return (list->string cs)))
       (return "")))

(define-parser (fragment)
  (maybe
   (parser ()
    (>> (char #\#)
        (<- cs (kleene (parser ()
                        (<> (pchar)
                            (char #\/)
                            (char #\?)))))
        (return (list->string cs))))))

(define-parser (absolute)
  (>> (<- s (scheme))
      (char #\:)
      (<> (>> (char #\/)
              (char #\/)
              (<- a  (authority))
              (<- pa (path-abempty))
              (<- q  (query))
              (<- f  (fragment))
              (return (make-uri s a pa q f)))
          (>> (<- pa (<> (path-absolute)
                         (path-rootless)
                         (path-empty)))
              (<- q (query))
              (<- f (fragment))
              (return (make-uri s '() pa q f))))))

(define-parser (relative)
  (<> (>> (char #\/)
          (char #\/)
          (<- a  (authority))
          (<- pa (path-abempty))
          (<- q  (query))
          (<- f  (fragment))
          (return (make-uri '() a pa q f)))
      (>> (<- pa (<> (path-absolute)
                     (path-noscheme)
                     (path-empty)))
          (<- q (query))
          (<- f (fragment))
          (return (make-uri '() '() pa q f)))))

(define-parser (rfc3986)
  (<> (absolute)
      (relative)))

(define (string->uri s)
  (run (rfc3986) s))

(define (uri->string u)
  (string-append
   (scheme->string (uri-scheme u))
   (authority->string (uri-authority u))
   (path->string (uri-path u))
   (query->string (uri-query u))
   (fragment->string (uri-fragment u))))

(define (scheme->string s)
  (if (null? s) ""
      (string-append s ":")))

(define (authority->string a)
  (if (null? a) ""
      (string-append
       (userinfo->string (assoc 'userinfo a))
       (host->string (assoc 'host a))
       (port->string (assoc 'port a)))))

(define (userinfo->string p)
  (if (not p) "" (string-append (cdr p) "@")))

(define (host->string p)
  (if (not p) "" (cdr p)))

(define (port->string p)
  (if (not p) "" (string-append ":" (cdr p))))

(define (path->string p)
  (cond
   ((null? p) "")
   ((null? (cdr p)) (car p))
   (else (string-append (car p)
                        "/"
                        (path->string p)))))

(define (query->string q)
  (if (null? q) ""
      (string-append "?" q)))

(define (fragment->string f)
  (if (null? f) ""
      (string-append "#" f)))

;; escape a string to uri format
(define (escape s)
  (let(
       (len (string-length s)))
    (call-with-output-string
     (string)
     (lambda (port)
       (let loop ((j 0))
         (if (>= j len) 'ok
             (let*(
                   (ch (string-ref s j))
                   (int (char->integer ch)))
               (cond
                ((or (char<? ch #\space)
                     (char=? ch #\&)
                     (char=? ch #\#)
                     (char=? ch #\?)
                     (char=? ch #\/)
                     (char<? ch #\x20))
                 (print port: port `(#\% ,(number->string int 16))))
                
                ((char<? ch #\z)
                 (print port: port ch))
                
                (else
                 (for-each
                  (lambda (int)
                    (print port: port `(#\% ,(number->string int 16))))
                  (u8vector->list
                   (call-with-output-u8vector
                    (u8vector)
                     (lambda (q)
                      (display ch q)))))))
               
               (loop (+ j 1)))))))))

(define (unescape s)
  (run (segment) s))