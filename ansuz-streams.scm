(##namespace ("ansuz-streams#"))

(##include "~~/lib/gambit#.scm")
(include "ansuz-streams#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         (not safe))

(define (stream-car s) (car (force s)))
(define (stream-cdr s) (cdr (force s)))

(define (port->stream p)
  (stream-cons
   (read-char p)
   (port->stream p)))

(define (string->stream s)
  (list->stream (string->list s)))

(define nul-stream (stream-cons #!eof nul-stream))

(define (list->stream l)
  (if (null? l) nul-stream
      (stream-cons (car l) (list->stream (cdr l)))))

(define (stream . l) (list->stream l))

(define (stream->list s)
  (if (eof-object? (stream-car s)) '()
      (cons (stream-car s) (stream->list (stream-cdr s)))))

(define (stream->string s)
  (list->string (stream->list s)))

(define (stream->port s p)
  (if (eof-object? (stream-car s)) '()
      (begin
        (display (stream-car s) p)
        (stream->port (stream-cdr s) p))))

(define (stream-append s . ss)
  (let stream-append ((s s)
                      (ss ss))
    (cond
     ((null? ss) s)
     ((eof-object? (stream-car s)) (stream-append (car ss) (cdr ss)))
     (else
      (stream-cons (stream-car s)
                   (stream-append (stream-cdr s) ss))))))
