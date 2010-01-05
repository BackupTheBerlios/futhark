(##namespace ("ansuz-re-parser#"))

(##include "~~/lib/gambit#.scm")

(include "../kernel#.scm")
(include "../expressions#.scm")
(include "../extras#.scm")
(include "../sources#.scm")
(include "../language#.scm")

(include "sets#.scm")
(include "fsm#.scm")

(define *-not-valid-*
  (cons #!eof (string->list "|?+*~.-\\(){}[]")))

(define-parser (VBAR)   (char #\|))
(define-parser (QUEST)  (char #\?))
(define-parser (PLUS)   (char #\+))
(define-parser (MULT)   (char #\*))
(define-parser (TILDE)  (char #\~))
(define-parser (DOT)    (char #\.))
(define-parser (MINUS)  (char #\-))
(define-parser (COMMA)  (char #\,))
(define-parser (ESCAPE) (char #\\))

(define-parser (ROUND_OPEN)   (char #\())
(define-parser (ROUND_CLOSE)  (char #\)))
(define-parser (CURLED_OPEN)  (char #\{))
(define-parser (CURLED_CLOSE) (char #\}))

(define-macro (i0)
  (char->integer #\0))

(define-parser (decimal)
  (reflect (ts sc fl)
           (let(
                (c (source-car ts)))
             (if (char-numeric? c)
                 (let loop ((ts (source-cdr ts))
                            (k (- (char->integer c) (i0))))
                   (let(
                        (c (source-car ts)))
                     (if (char-numeric? c)
                         (loop (source-cdr ts)
                               (+ (* k 10) (- (char->integer c) (i0))))
                         (sc k ts fl))))
                 (fl "not a decimal")))))
 
(define-parser (valid-char)
  (reflect (ts sc fl)
           (let(
                (c (source-car ts)))
             (cond
              ((eof-object? c) (fl "end of object reached"))
              ((memq c *-not-valid-*)
               (fl "not a valid char"))
              (else
               (sc c (source-cdr ts) fl))))))

(define (fsm-interval p lo up)
  (cond
   ((> lo 0)
    (nfa:++ p (fsm-interval p (- lo 1) (if (eq? up 'inf) up (- up 1)))))
   ((eq? up 'inf)
    (nfa:kleene p))
   ((> up 0)
    (nfa:// (nfa:++ p (fsm-interval p 0 (- up 1)))
            (nfa:empty)))
   (else
    (nfa:empty))))

(define-parser (re-interval p)
  (<>
   (>> (<- s0 (decimal))
       (COMMA)
       (<- s1 (decimal))
       (return (fsm-interval p s0 s1)))
   
   (>> (<- s0 (decimal))
       (COMMA)
       (return (fsm-interval p s0 'inf)))
   
   (>> (<- s0 (decimal))
       (return (fsm-interval p s0 s0)))
   
   (>> (COMMA)
       (<- s1 (decimal))
       (return (fsm-interval p 0 s1)))))     

(define-parser (re-times p)
  (>> (CURLED_OPEN)
      (<- s0 (re-interval p))
      (CURLED_CLOSE)
      (return s0)))

(define-parser (re-parenthesis)
  (>> (ROUND_OPEN)
      (<- s0 (re-expr))
      (ROUND_CLOSE)
      (return s0)))

(define-parser (re-range)
  (>> (<- s0 (valid-char))
       (MINUS)
       (<- s1 (valid-char))
       (return (list (cons (char->integer s0) (+ 1 (char->integer s1)))))))

(define-parser (re-range-singleton)
  (>> (<- s0 (valid-char))
      (return (list (cons (char->integer s0) (+ 1 (char->integer s0)))))))

(define-parser (seti)
  (<> (re-range)
      (re-range-singleton)))

(define-parser (re-set)
  (>> (char #\[)
      (<- neg? (<> (char #\~) (return #f)))
      (<- xs (repeat 1 seti))
      (char #\])
      (return (nfa:set (if neg? (set-complement (set-union+ xs))
                           (set-union+ xs))))))

(define (set-union+ xs)
  (if (null? xs) '()
      (let set-union+ ((x (car xs))
                       (xs (cdr xs)))
        (if (null? xs) x
            (set-union+ (set-union x (car xs)) (cdr xs))))))
    
(define-parser (re-any)
  (>> (DOT)
      (return (nfa:set `((0 . ,*max*))))))

(define-parser (re-char)
  (>> (<- s0 (valid-char))
      (return (nfa:set
               (list (cons
                      (char->integer s0)
                      (+ 1 (char->integer s0))))))))

(define-parser (re-atom)
  (<> (re-parenthesis)
      (re-set)
      (re-any)
      (re-char)))

(define-parser (re-factor)
  (>> (<- s0 (re-atom))
      (<> (>> (QUEST) (return (fsm-interval s0 0 1)))
          (>> (PLUS) (return (fsm-interval s0  1 'inf)))
          (>> (MULT) (return (fsm-interval s0 0 'inf)))
          (re-times s0)
          (return s0))))

(define-parser (re-term)
  (>> (<- s0 (re-factor))
      (<> (>> (<- s1 (re-term))
              (return (nfa:++ s0 s1)))
          (return s0))))

(define-parser (re-expr)
  (>> (<- s0 (re-term))
      (<> (>> (VBAR)
              (<- s1 (re-expr))
              (return (nfa:// s0 s1)))
          (return s0))))

(define-parser (re)
  (>> (<- s0 (re-expr))
      (eos)
      (return (nfa->dfa s0))))
