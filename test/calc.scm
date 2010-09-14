;; gsi calc

(declare (standard-bindings)
         (extended-bindings)
         (not safe)
         (block))

(include "~~/site-scheme/futhark/ansuz/expressions.scm")

(##namespace ("calc#"))

(##include "~~/lib/gambit#.scm")

;; (include "~~/site-scheme/futhark/ansuz/sources/string#.scm")
(include "~~/site-scheme/futhark/ansuz/on-strings#.scm")
(include "~~/site-scheme/futhark/ansuz/expressions#.scm")

(define idigit
  (let(
       (i0 (char->integer #\0)))
    (parser ()
            (>> (<- c (digit))
                (return (- (char->integer c) i0))))))

(define-parser (int+ c)
  (<> (>> (<- c1 (idigit))
          (int+ (+ c1 (* 10 c))))
      (return c)))

(define-parser (int) 
  (>> (<- d (idigit))
      (int+ d)))

(define-parser (frac)
  (>> (char #\.)
      (<- d (idigit))
      (frac+ (/ 1 100) (/ d 10))))

(define-parser (frac+ m c0)
  (<> (>> (<- c (idigit))
          (frac+ (/ m 10) (+ (* m c) c0)))
      (return c0)))

(define-parser (sign)
  (<> (>> (char #\+) (return (lambda (n) n)))
      (>> (char #\-) (return (lambda (n) (- n))))
      (return (lambda (n) n))))
                      
(define-parser (pow10)
  (>> (<> (char #\e) (char #\E))
      (<- s (sign))
      (<- i (int))
      (return (s i))))

;; NUMBER : 1 1.1 1.1e-10 1.1e+10 +1 -1 1.1e10 ...
(define-parser (num)
  (>> (<- s (sign))
      (<- ip (int))
      (<- fp (<> (frac) (return 0)))
      (<- ex (<> (pow10) (return 0)))
      (return (s (* (+ ip fp) (expt 10 ex))))))

(define-parser (number)
  (>> (<- n (num))
      (eos)
      (return n)))


(define-parser (sum)
  (>> (char #\+) (return +)))

(define-parser (dif)
  (>> (char #\-) (return -)))

(define-parser (mul)
  (>> (char #\*) (return *)))

(define-parser (div)
  (>> (char #\/) (return /)))

(define-parser (sqr)
  (>> (char #\^) (return (lambda (c) (* c c)))))

(define table
  (let(
       (prefix
        `((,dif 1)))
       
       (infix 
        `((,sum 1 left)
          (,dif 1 left)
          (,mul 2 left)
          (,div 2 left)))
       
       (postfix
        `((,sqr 3))))
  (make-operator-table
   prefix
   infix
   postfix)))

(define-parser (par)
  (>> (char #\()
      (<- e (expr table _term))
      (char #\))
      (return e)))

(define-parser (_term)
  (<> (num) (par)))

(define-parser (math-exp)
  (>> (<- r (expr table _term))
      (<> (eos) (fail "end expected"))
      (return r)))
                
(define (calc s)
  (with-exception-catcher
   (lambda (ex) 'fail)
   (lambda () 
     (run (math-exp) s))))

(define (calc-repl)
  (display "press ctrl-D to exit\n")
  (display "available commands:\n")
  (display "+ - * / (^ is the unary operator for square)\n")
  (let forever ()
    (display "?")
    (let (
          (c (read-line)))
      (if (eof-object? c) (newline)
          (let(
               (val (calc c)))
            (if (eq? val 'fail)
                (display "not well formed expression\n")
                (begin
                  (display "=")
                  (display (exact->inexact val))
                  (newline)))
            (forever))))))

(calc-repl)
