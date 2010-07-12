(##namespace ("gebo-json#"))

(##include "~~/lib/gambit#.scm")
           
(include "../ansuz/language#.scm")
(include "../ansuz/kernel#.scm")
(include "../ansuz/extras#.scm")
(include "../ansuz/sources#.scm")

(declare
 (standard-bindings)
 (extended-bindings)
 ;;(not safe)
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
  (>>  (char #\\) (json-escaped-char)))

(define-parser (json-escaped-char)
  (<> (char #\")
      (char #\\)
      (char #\/)
      (char-unicode)
      (char-backspace)
      (char-linefeed)
      (char-newline)
      (char-carriage-ret)
      (char-tab)))

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
  (<> (decimal-digit)
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

(define (json-read  #!optional (port (current-input-port)) (tr (lambda (x) x)))
  (run (json-value tr) port))

;; (define (json-read #!optional (port (current-input-port)) (tr (lambda (x) x)))
;;   (let(
;;        (s1 (empty-buffer port)))
;;     (call-with-input-u8vector
;;      (list init: s1
;;            char-encoding: 'UTF-8)
;;      (lambda (p0)
;;        (run (json-value tr)
;;             (stream-append
;;              (port->stream p0)
;;              (port->stream port)))))))

;; scheme to json transform stuff;
 
(define (json-write o #!optional (p (current-output-port)) (tr (lambda (x) x)))
  (let(
       (o (tr o)))
    (cond
     ((null? o) (print port: p "[]"))
     ((eq? o -inf.0) (print port: p "-Infinity"))
     ((eq? o +inf.0) (print port: p "Infinity"))
     ((eq? o 'null) (print port: p "null"))
     ((eq? o #t) (print port: p "true"))
     ((eq? o #f) (print port: p "false"))
     
     ((table? o) (table-write o p tr))
     ((pair? o) (list-write o p tr))
     ((number? o) (print port: p o))
     ((string? o) (string-write o p))
     (else (raise "can't convert this object to json")))))

(define (string-write o p)
  (print port: p #\")
  (let for ((j 0))
    (if (< j (string-length o))
        (let(
             (c (string-ref o j)))
          
          (cond
           ((or (char=? c #\") (char=? c #\\))
            (print port: p #\\)
            (print port: p c))
           
           ((char=? c #\newline)
            (print port: p "\\n"))

           (else
            (print port: p c)))
           (for (+ j 1)))))
  (print port: p #\"))

(define (table-write o p tr)
  (let(
       (fst #t))
    (print port: p "{")
    (table-for-each
     (lambda (k v)
       (if (not fst)
           (print port: p ",")
           (set! fst #f))
       (write (tr k) p)
       (print port: p ":")
       (json-write v p tr))
     o)
    (print port: p "}")))

(define (list-write o p tr)
  (let(
       (fst #t))
    (print port: p "[")
    (for-each (lambda (v)
                (if (not fst)
                    (print port: p ",")
                    (set! fst #f))
                (json-write v p tr))
              o)
    (print port: p "]")))

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

