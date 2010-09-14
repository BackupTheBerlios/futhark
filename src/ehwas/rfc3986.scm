;; METTERE IPV6 !!! e ipvfuture
;; quest'ultima è cazzata, ma per ipv6
;; tenere conto che dopo l'ellissi
;; ogni numero potrerbbe essere trattato come
;; ipv4 o come un altro blocco ipv6

(##namespace ("rfc3986#"))

(##include "~~/lib/gambit#.scm")
(include "../ansuz/sources/string#.scm")
(include "../ansuz/char-stream-parser#.scm")

(include "../ansuz/re#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         (safe)
         (fixnum))

(define-structure uri scheme authority path query fragment)

;; (declare (not safe))

(define-regexp re-port "[0-9]{1,5}")

(define-parser (port)
  (<- c (re-port))
  (let(
       (p (string->number c)))
    (if (< p 65536)
        (return p)
        (fail "port number too high"))))


;; (define-parser (scheme)
;;   (>> (<- c (alpha))
;;       (<- cs (kleene (parser () (<> (alpha)
;;                                     (digit)
;;                                     (char #\+)
;;                                     (char #\-)
;;                                     (char #\.)))))
;;       (return (list->string (cons c cs)))))

(define-regexp scheme "[a-zA-Z][a-zA-Z0-9\\+\\-\\.]*")

;; (define-parser (userinfo)
;;   (>> (<- cs (kleene (parser () (<> (unreserved)
;;                                     (pct-encoded)
;;                                     (sub-delims)
;;                                     (char #\:)))))
;;       (return (list->string cs))))

(define (pct->u8vector s)
  (call-with-output-u8vector
   (u8vector)
   (lambda (out)
     (let for ((j 0))
       (if (>= j (string-length s)) 'ok
           (let(
                (c (string-ref s j)))
             (cond
              ((char=? c #\%)
               (write-char (integer->char (string->number (substring s (+ j 1) (+ j 3)) 16)) out)
               (for (+ j 3)))
              ((char=? c #\+)
               (write-char #\space out)
               (for (+ j 1)))
              (else
               (write-char c out)
               (for (+ j 1))))))))))

(define (pct-decode s)
  (if (= 0 (string-length s)) ""
      (call-with-input-u8vector
       (list init: (pct->u8vector s)
             char-encoding: 'UTF-8)
       (lambda (p) (read-line p #f)))))

;; (define-regexp re-userinfo "([a-zA-Z0-9]|\\-|\\.|\\_|\\~|%[0-9a-fA-F]{2}|!|$|&|\\'|\\(|\\)|\\*|\\+|\\,|\\;|=|:)*")

(define-regexp re-userinfo  "([a-zA-Z0-9!$&',;=:\\-\\.\\_\\~\\(\\)\\*\\+]|%[0-9a-fA-F]{2})*")

(define-parser (userinfo)
  (>> (<- c (re-userinfo))
      (return (pct-decode c))))

;; (define-parser (h8)
;;   (>> (<- cs (repeat-max 1 3 digit-number))
;;       (let(
;;            (v (fold-left (lambda (i t) (+ t (* 10 i))) 0 cs)))
;;         (if (and (>= v 0) (<= v 255))
;;             (return v)
;;             (fail "h8")))))

;; (define-parser (ipv4-list)
;;   (>> (<- b (h8))
;;       (<- bs (times 3 (parser () (>> (char #\.) (h8)))))
;;       (return (cons b bs))))

;; (define-parser (ipv4)
;;   (>> (<- bs (ipv4-list))
;;       (return `(ip 4 ,@bs))))

(define (string-split str ch)
  (let for ((l0 0) (l1 0) (acc '()))
    (cond
     ((>= l1 (string-length str))
      (reverse (cons (substring str l0 l1) acc)))
     ((char=? (string-ref str l1) ch)
      (for (+ 1 l1) (+ 1 l1) (cons (substring str l0 l1) acc)))
     (else
      (for l0 (+ 1 l1) acc)))))

(define-regexp octet "((2[0-5][0-9]|1[0-9][0-9]|[0-9][0-9]|[0-9])\\.){3}(2[0-5][0-9]|1[0-9][0-9]|[0-9][0-9]|[0-9])")

(define-parser (ipv4)
  (>> (<- v (octet))
      (return `(ip4 ,@(map string->number (string-split v #\.))))))

;; (define-parser (hostname)
;;   (>> (<- cs (kleene (parser ()
;;                              (<> (unreserved)
;;                                  (pct-encoded)
;;                                  (sub-delims)))))
;;       (return (list->string cs))))

;; (define-regexp re-hostname "([a-zA-Z]|[0-9]|\\-|\\.|\\_|\\~|%[0-9a-fA-F]{2}|[!$&\\'\\(\\)\\*\\+\\,\\;=])*")

(define-regexp re-hostname  "([a-zA-Z0-9!$&',;=\\-\\.\\_\\~\\(\\)\\*\\+]|%[0-9a-fA-F]{2})*")

(define-parser (hostname)
  (>> (<- name (re-hostname))
      (return (pct-decode name))))

(define-parser (host)
  (<> (ipv4)
      (hostname)))

(define-parser (authority)
  (>> (<- ui (maybe (>> (<- u (userinfo))
                        (char #\@)
                        (return u))))
      (<- h (host))
      (<- n (maybe (>> (char #\:) (port))))
      (return (append
               (if (null? ui) '() `((userinfo ,@ui)))
               `((host ,@h))
               (if (null? n) '() `((port ,@n)))))))

;; (define-regexp re-segment "([a-zA-Z0-9]|\\-|\\.|_|\\~|%[0-9a-fA-F]{2}|!|$|&|'|\\(|\\)|\\*|\\+|,|;|=)*")

(define-regexp re-segment "([a-zA-Z0-9\\-\\.\\_\\~!$&'\\(\\)\\*\\+,;=]|%[0-9a-fA-F]{2})*")

(define-parser (segment)
  (>> (<- r (re-segment))
      (return (string->symbol (pct-decode r)))))

;; (define-regexp re-segment-nz "([a-zA-Z0-9]|\\-|\\.|_|\\~|%[0-9a-fA-F]{2}|!|$|&|'|\\(|\\)|\\*|\\+|,|;|=)+")

(define-regexp re-segment-nz "([a-zA-Z0-9\\-\\.\\_\\~!$&'\\(\\)\\*\\+,;=]|%[0-9a-fA-F]{2})+")

(define-parser (segment-nz)
  (>> (<- r (re-segment-nz))
      (return (string->symbol (pct-decode r)))))
  
(define-regexp re-segment-nz-nc "([a-zA-Z0-9\\-\\.\\_\\~!$&'\\(\\)\\*\\+,;=:]|%[0-9a-fA-F]{2})+")

(define-parser (segment-nz-nc)
  (>> (<- r (re-segment-nz-nc)) 
      (return (string->symbol (pct-decode r)))))

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

(define-regexp re-query "[a-zA-Z0-9\\-\\.\\_\\~!$&\\'\\(\\)\\*\\+\\,\\;=%@\\/\\?]*")

(define-parser (query)
  (<> (>> (char #\?) (re-query))
      (return "")))

(define-regexp re-fragment "([a-zA-Z0-9\\-\\.\\_\\~!$&\\'\\(\\)\\*\\+,;=\\/\\?]|%[0-9a-fA-F]{2})*")

(define-parser (fragment)
  (maybe
   (>> (char #\#)
       (<- cs (re-fragment))
       (return (pct-decode cs)))))

(define-parser (absolute)
  (>> (<- s (scheme))
      (char #\:)
      (<> (>> (regexp "//")
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
  (<> (>> (regexp "//")
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

(declare (safe))

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
  (if (= 0 (string-length q)) "" 
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
                     (char=? ch #\:)
                     (char=? ch #\.)
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