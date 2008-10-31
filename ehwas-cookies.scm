(##namespace  ("ehwas-cookies#"))

(##include "~~/lib/gambit#.scm")

(include "ehwas-request#.scm")
(include "ehwas-response#.scm")
(include "ehwas-resolver#.scm")

(include "ansuz-language#.scm")
(include "ansuz-kernel#.scm")
(include "ansuz-extras#.scm")
(include "ansuz-streams#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         (not safe))

(define-parser (valid-key-char)
  (test-token
   (lambda (c)
     (not (or (char=? c #\=)
              (char=? c #\;)
              (char=? c #\nul))))))

(define-parser (more-key)
  (<> (>> (<- c (valid-key-char))
          (<- cs (more-key))
          (return (cons c cs)))
      (return '())))

(define-parser (key)
  (>> (<- cs (more-key))
      (return (list->string cs))))

(define-parser (valid-value-char)
  (test-token
   (lambda (c)
     (not (or (char=? c #\;)
              (char=? c #\nul))))))

(define-parser (more-value)
  (<> (>> (<- c (valid-value-char))
          (<- cs (more-value))
          (return (cons c cs)))
      (return '())))

(define-parser (value)
  (>> (<- cs (more-value))
      (return (list->string cs))))

(define-parser (spaces)
  (<> (>> (whitespace) (spaces))
      (return '())))

(define-parser (cookie)
  (>> (<- c (cookie-value))
      (<- cs (kleene
              (parser () (>> (spaces) (char #\;) (spaces)
                             (cookie-value)))))
      (return (list->table (cons c cs)))))

(define-parser (cookie-value)
  (>> (<- nam (key))
      (spaces) (char #\=) (spaces)
      (<- val (value))
      (return (cons nam val))))

(define (string->cookie s)
  (run (cookie) (string->stream s)))

(define (request-cookies request)
  (let(
       (string (table-ref (request-header request) "Cookie" #f)))
    (and string (string->cookie string))))

(define (fold-left f i l)
  (if (null? l) i
      (fold-left f (f i (car l)) (cdr l))))

(define (set-cookie! response k v . avs)
  (response-header-set!
   response
   "Set-Cookie"
   (fold-left (lambda (p av)
                (cond
                 ((pair? av)
                  (string-append p ";" (car av) "=" (cdr av)))
                 (else
                  (string-append p av ";"))))
              (string-append k "=" v)
              avs)))

      