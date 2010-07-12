(##namespace ("ansuz-kernel#"))

(##include "~~/lib/gambit#.scm")

(include "reflect#.scm")
(include "sources.new#.scm")
(include "language#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         ;;(not safe)
         )

(define-structure parser-exception reason source)

(define-parser (char c0)
  (reflect (head tail row col pos datum sc fl) 
           (let(
                (c (head datum)))
             (if (and (char? c) (char=? c c0))
                 (sc c0 (tail datum) fl)
                 (fl (make-parser-exception
                      (string-append
                       "not "
                       (list->string (list c0)))
                      datum))))))


(define-parser (digit)
  (reflect (head tail row col pos datum sc fl)
           (let(
                (c (head datum)))
             (if (and (char? c) (char-numeric? c))
                 (sc c (tail datum) fl)
                 (fl (make-parser-exception "not a digit" datum))))))

(define-parser (upcase)
  (reflect (head tail row col pos datum sc fl) 
           (let(
                (c (head datum)))
             (if (and (char? c) (char>=? c #\A) (char<=? c #\Z))
                 (sc c (tail datum) fl)
                 (fl (make-parser-exception "not an upcase" datum))))))

(define-parser (locase)
  (reflect (head tail row col pos datum sc fl)
           (let(
                (c (head datum)))
             (if (and (char? c) (char>=? c #\a) (char<=? c #\z))
                 (sc c (tail datum) fl)
                 (fl (make-parser-exception "not a locase" datum))))))

(define-parser (interval l u)
  (reflect (head tail row col pos datum sc fl)
           (let(
                (c (head datum)))
             (if (and (char? c) (char>=? c l) (char<=? c u))
                 (sc c (tail datum) fl)
                 (fl (make-parser-exception "not in interval" datum))))))

(define-parser (alpha)
  (reflect (head tail row col pos datum sc fl)
           (let(
                (c (head datum)))
             (if (and (char? c) (char-alphabetic? c))
                 (sc c (tail datum) fl)
                 (fl (make-parser-exception "not an alpha" datum))))))

(define-parser (whitespace)
  (reflect (head tail row col pos datum sc fl)
           (let(
                (c (head datum)))
             (if (and (char? c) (char-whitespace? c))
                 (sc c (tail datum) fl)
                 (fl (make-parser-exception "not a whitespace" datum))))))

(define-parser (eos)
  (reflect (head tail row col pos datum sc fl)
           (if (eof-object? (head datum))
               (sc #!eof (tail datum) fl)
               (fl (make-parser-exception "not end" datum)))))

(define-parser (any)
  (reflect (head tail row col pos datum sc fl)
           (let(
                (c (head datum)))
             (if (char? c)
                 (sc c (tail datum) fl)
                 (fl (make-parser-exception "not any" datum))))))

(define-parser (test-token t?)
  (reflect (head tail row col pos datum sc fl)
           (let(
                (c (head datum)))
             (if (and (char? c) (t? c))
                 (sc c (tail datum) fl)
                 (fl (make-parser-exception "test failed" datum))))))

(define-parser (fail r)
  (reflect (head tail row col pos datum sc fl)
           (fl (make-parser-exception r datum))))

(define-parser (token-source)
  (reflect (head tail row col pos datum sc fl) (sc datum datum fl)))

(define-parser (continuation)
  (reflect (head tail row col pos datum sc fl) (sc (lambda (v) (sc v datum fl)) datum fl)))

(define-parser (failure)
  (reflect (head tail row col pos datum sc fl) (sc fl datum fl)))

