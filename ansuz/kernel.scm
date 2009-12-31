(##namespace ("ansuz-kernel#"))

(##include "~~/lib/gambit#.scm")

(include "reflect#.scm")
(include "sources#.scm")
(include "language#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         ;;(not safe)
         )

(define-structure parser-exception reason)

(define-parser (char c0)
  (reflect (ts sc fl)
           (let(
                (c (source-car ts)))
             
           (if (and (char? c) (char=? (source-car ts) c0))
               (sc c0 (source-cdr ts) fl)
               (fl (make-parser-exception
                    (string-append
                     "not "
                     (list->string (list c0)))))))))


(define-parser (digit)
  (reflect (ts sc fl)
           (let(
                (c (source-car ts)))
             (if (and (char? c) (char-numeric? c))
                 (sc c (source-cdr ts) fl)
                 (fl (make-parser-exception "not a digit"))))))

(define-parser (upcase)
  (reflect (ts sc fl)
           (let(
                (c (source-car ts)))
             (if (and (char? c) (char>=? c #\A) (char<=? c #\Z))
                 (sc c (source-cdr ts) fl)
                 (fl (make-parser-exception "not an upcase"))))))

(define-parser (locase)
  (reflect (ts sc fl)
           (let(
                (c (source-car ts)))
             (if (and (char? c) (char>=? c #\a) (char<=? c #\z))
                 (sc c (source-cdr ts) fl)
                 (fl (make-parser-exception "not a locase"))))))

(define-parser (interval l u)
  (reflect (ts sc fl)
           (let(
                (c (source-car ts)))
             (if (and (char? c) (char>=? c l) (char<=? c u))
                 (sc c (source-cdr ts) fl)
                 (fl (make-parser-exception "not in interval"))))))

(define-parser (alpha)
  (reflect (ts sc fl)
           (let(
                (c (source-car ts)))
             (if (and (char? c) (char-alphabetic? c))
                 (sc c (source-cdr ts) fl)
                 (fl (make-parser-exception "not an alpha"))))))

(define-parser (whitespace)
  (reflect (ts sc fl)
           (let(
                (c (source-car ts)))
             (if (and (char? c) (char-whitespace? c))
                 (sc c (source-cdr ts) fl)
                 (fl (make-parser-exception "not a whitespace"))))))

(define-parser (eos)
  (reflect (ts sc fl)
           (if (eof-object? (source-car ts))
               (sc #!eof (source-cdr ts) fl)
               (fl (make-parser-exception "not end")))))

(define-parser (any)
  (reflect (ts sc fl)
           (let(
                (c (source-car ts)))
             (if (char? c)
                 (sc c (source-cdr ts) fl)
                 (fl (make-parser-exception "not any"))))))

(define-parser (test-token t?)
  (reflect (ts sc fl)
           (let(
                (c (source-car ts)))
             (if (and (char? c) (t? c))
                 (sc c (source-cdr ts) fl)
                 (fl (make-parser-exception "test failed"))))))

(define-parser (fail r)
  (reflect (ts sc fl)
           (fl (make-parser-exception r))))

(define-parser (token-source)
  (reflect (ts sc fl) (sc ts ts fl)))

(define-parser (continuation)
  (reflect (ts sc fl) (sc (lambda (v) (sc v ts fl)) ts fl)))

(define-parser (failure)
  (reflect (ts sc fl) (sc fl ts fl)))

