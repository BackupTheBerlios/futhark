(##namespace ("gebo-json#"))

(##include "~~/lib/gambit#.scm")
           
(include "ansuz-streams#.scm")
(include "ansuz-language#.scm")
(include "ansuz-kernel#.scm")
(include "ansuz-extras#.scm")

(declare
 (standard-bindings)
 (extended-bindings)
 (not safe)
 (block)
 )

;; json to scheme transform

(define-parser (sp)
  (<> (>> (whitespace) (sp))
      (return #\space)))

(define-parser (json)
  (>> (sp) (json-value)))

(define-parser (json-value tr)
  (<> (json-object tr)
      (json-array tr)
      (json-true tr)
      (json-false tr)
      (json-null tr)
      (json-string tr)
      (json-number tr)))

(define-parser (json-object tr)
  (>> (char #\{)
      (sp)
      (<- as (separated-values
              (parser () (>> (char #\,) (sp)))
              (parser () (json-object-pair tr))))
      (sp)
      (char #\})
      (return (tr (list->table as)))))

(define-parser (json-object-pair tr)
  (>> (<- k (json-string tr))
      (sp)
      (char #\:)
      (sp)
      (<- v (json-value tr))
      (sp)
      (return (cons k v))))

(define-parser (json-array tr)
  (>> (char #\[) (sp)
      (<- ls (separated-values
              (parser () (>> (char #\,) (sp)))
              (parser () (json-value tr))))
      (sp) (char #\])
      (return (tr ls))))

(define-parser (json-true tr)
  (>> (word "true")
      (return (tr #t))))

(define-parser (json-false tr)
  (>> (word "false")
      (return (tr #f))))

(define-parser (json-null tr)
  (>> (word "null")
      (return (tr 'null))))

(define-parser (json-string tr)
  (>> (char #\")
      (<- cs (kleene json-char))
      (char #\")
      (return (tr (list->string cs)))))
                           
(define-parser (json-char)
  (<> (json-special)
      (json-unicode)))

(define-parser (json-unicode)
  (test-token
   (lambda (c)
     (not
      (or
       (char=? c #\")
       ;; (char=? c #\\)
       (char=? c #\nul))))))

(define-parser (json-special)
  (>> (char #\\) (json-escaped-char)))

(define-parser (json-escaped-char)
  (<> (char #\")
      (char #\\)
      (char #\/)
      (char-backspace)
      (char-linefeed)
      (char-newline)
      (char-carriage-ret)
      (char-tab)
      (char-unicode)))

(define-parser (char-backspace)
  (>> (char #\b) (return #\backspace)))

(define-parser (char-linefeed)
  (>> (char #\f) (return #\page)))

(define-parser (char-newline)
  (>> (char #\n) (return #\newline)))

(define-parser (char-carriage-ret)
  (>> (char #\r) (return #\return)))

(define-parser (char-tab)
  (>> (char #\t) (return #\tab)))

(define-parser (char-unicode)
  (>> (char #\u) (hex-unicode)))

(define-parser (hex-unicode)
  (>> (<- c0 (hex-digit))
      (<- c1 (hex-digit))
      (<- c2 (hex-digit))
      (<- c3 (hex-digit))
      ; (declare (fixnum c0 c1 c2 c3))
      (return (integer->char
               (fx+ (fx* c0 4096)
                    (fx* c1 256)
                    (fx* c2 16)
                    c3)))))

(define-parser (hex-digit)
  (>> (decimal-digit)
      (locase-alpha-digit)
      (upcase-alpha-digit)))

(define char-0 (char->integer #\0))
(define-parser (decimal-digit)
  (>> (<- c (digit))
      (return (fx- (char->integer c) char-0))))

(define char-lo-a (fx- (char->integer #\a) 10))
(define-parser (locase-alpha-digit)
  (>> (<- c (test-token
             (lambda (c)
               (and (char>=? c #\a)
                    (char<=? c #\f)))))
      (return (fx- (char->integer c) char-lo-a))))

(define char-up-a (fx- (char->integer #\A) 10))
(define-parser (upcase-alpha-digit)
  (>> (<- c (test-token
             (lambda (c)
               (and (char>=? c #\A)
                    (char<=? c #\F)))))
      (return (fx- (char->integer c) char-up-a))))

(define-parser (json-number tr)
  (>> (<- s (sign))
      (sp)
      (<- v (positive-number))
      (return (tr (s v)))))

(define-parser (sign)
  (<> (>> (char #\-) (return -))
      (>> (char #\+) (return +))
      (return +)))

(define-parser (positive-number)
  (<> (infinity)
      (>> (<- b (float))
          (<> (>> (<> (char #\e) (char #\E))
                  (<- s (sign))
                  (<- e (integer))
                  (return (expt b (s e))))
              (return b)))))

(define-parser (infinity)
  (>> (word "Infinity")
      (return +inf.0)))

(define-parser (float)
  (>> (<- i (integer))
      (<> (>> (<- d (fractional))
              (return (+ i d)))
          (return i))))

(define-parser (digit-number)
  (>> (<- d (digit))
      (return (fx- (char->integer d) char-0))))
  
(define-parser (zero)
  (>> (char #\0) (return 0)))

(define-parser (integer)
  (<> (zero)
      (>> (<- i (repeat 1 digit-number))
          (return
           (fold-left
            (lambda (i d) (+ d (* 10 i)))
            0
            i)))))

(define-parser (fractional)
  (>> (char #\.)
      (<- ds (kleene digit-number))
      (return
       (fold-right
        (lambda (d i) (/ (+ i d) 10))
        ds
        0.))))

(define (fold-left f i l)
  (let fold ((i i) (l l))
    (if (null? l) i
        (fold (f i (car l)) (cdr l)))))

(define (fold-right f l i)
  (let fold ((l l) (i i))
    (if (null? l) i
        (f (car l) (fold (cdr l) i)))))

;; this is very dirty about using buffered ports

(define (string->u8vector s)
  (let(
       (r (make-u8vector (string-length s))))
    (let loop ((c 0))
      (if (>= c (string-length s)) r
          (begin
            (u8vector-set! r c (char->integer (string-ref s c)))
            (loop (+ c 1)))))))

(define (empty-buffer port)
  (let*(
        (ln (input-port-characters-buffered port))
        (s (make-string ln)))
    (pp `(empty-buffer ,ln))
    (read-substring s  0 ln port)
    (string->u8vector s)))

(define (json-read #!optional (port (current-input-port)) (tr (lambda (x) x)))
  (with-exception-catcher
   (lambda (ex) (pp ex))
   (lambda () 
     (let*(
           (s1 (empty-buffer port)))
       (call-with-input-u8vector
        (list init: s1
              char-encoding: 'UTF)
        (lambda (p0)
          (port-settings-set! port (list char-encoding: 'UTF))
          (run (json-value tr)
               (stream-append
                (port->stream p0)
                (port->stream port)))))))))
   
;; scheme to json transform stuff;
 
(define (json-write o #!optional (p (current-output-port)) (tr (lambda (x) x)))
  (let(
       (o (tr o)))
    (cond
     ((null? o) (display "[]" p))
     ((eq? o -inf.0) (display "-Infinity" p))
     ((eq? o +inf.0) (display "Infinity" p))
     ((eq? o 'null) (display "null" p))
     ((eq? o #t) (display "true" p))
     ((eq? o #f) (display "false" p))
     
     ((table? o) (table-write o p tr))
     ((pair? o) (list-write o p tr))
     ((number? o) (display o p))
     ((string? o) (write o p))
     (else (raise "can't convert this object to json")))))
  
(define (table-write o p tr)
  (let(
       (fst #t))
    (display "{" p)
    (table-for-each
     (lambda (k v)
       (if (not fst)
           (display "," p)
           (set! fst #f))
       (write (tr k) p)
       (display ":" p)
       (json-write v p tr))
     o)
    (display "}" p)))

(define (list-write o p tr)
  (let(
       (fst #t))
    (display "[" p)
    (for-each (lambda (v)
                (if (not fst)
                    (display "," p)
                    (set! fst #f))
                (json-write v p tr))
              o)
    (display "]" p)))

;; (define (test)
;;   (call-with-input-string
;;    "{\"id-type\" : \"temite\", \"pid\" : 12}"
;;    (lambda (p)
;;      (json-read p (lambda (c)
;;                     (if (and (table? c)
;;                              (string=? (table-ref c "id-type")  "termite"))
;;                         "ciao ciao a termite"
;;                         c))))))

;; (pp (test))

