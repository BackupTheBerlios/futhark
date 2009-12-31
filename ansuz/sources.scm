(##namespace ("ansuz-sources#"))
(##include "~~/lib/gambit#.scm")
(include "sources#.scm")

(define-type source
  predicate: kource?
  (kar unprintable: read-only:)
  (kdr unprintable: read-only:)
  (kow unprintable: read-only:)
  (kol unprintable: read-only:)
  (kos unprintable: read-only:))

(define (function->source fn)
  (let port->source ((row 0) (col 0) (pos 0))
    (delay
      (let(
           (c (fn)))
        (make-source
         c
         (cond
          ((not (char? c))
           (port->source row col pos))
          
          ((char=? c #\newline) 
           (port->source (+ 1 row) 0 (+ 1 pos)))
          
          (else
           (port->source row (+ col 1) (+ 1 pos))))
         row col pos)))))

(define (port->source p)
  (function->source (lambda () (read-char p))))

(define (string->source s #!optional (c 0))
  (function->source
   (lambda ()
     (if (< c (string-length s))
         (let(
              (r (string-ref s c)))
           (set! c (+ c 1))
           r)
         #!eof))))

(define (vector->source s)
  (let ((c 0))
    (function->source
     (lambda ()
       (if (< c (vector-length s))
           (let(
                (r (vector-ref s c)))
             (set! c (+ c 1))
             r)
           #!eof)))))

(define (list->source s)
  (let ((s s))
    (function->source
     (lambda ()
       (if (null? s)
           #!eof
           (let(
                (r (car s)))
             (set! s (cdr s))
             r))))))

(define (source-append s0 s1)
  (function->source
   (lambda ()
     (if (eof-object? (source-car s0))
         (let(
              (r (source-car s1)))
           (set! s1 (source-cdr s1))
           r)
         (let(
              (r (source-car s0)))
           (set! s0 (source-cdr s0))
           r)))))

(define (->source s)
  (cond
   ((string? s) (string->source s))
   ((input-port? s) (port->source s))
   ((vector? s) (vector->source s))
   ((list? s) (list->source s))
   ((procedure? s) (function->source s))
   ((source? s) s)
   (else (error "unknown type ->source" s))))

  
   