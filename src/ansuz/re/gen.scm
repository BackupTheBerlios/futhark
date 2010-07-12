(##namespace ("ansuz-re-gen#"))

(##include "~~/lib/gambit#.scm")
(include "parser#.scm")
(include "fsm#.scm")
(include "sets#.scm")

(define (filter t? l)
  (cond
   ((null? l) '())
   ((t? (car l)) (cons (car l) (filter t? (cdr l))))
   (else (filter t? (cdr l)))))

;; transform a set in the for of list of intervals
;; in a predicate suitable to be part of a cond clause
(define (set->predicate s)
  `(or ,@(map interval->predicate s)))

;; transform an interval in the form (<lower> . <upper>)
;; where lower and upper are numbers, not chars
;; in a predicate suitable to be part of a cond clause
(define (interval->predicate i)
  (if (= (+ 1 (car i)) (cdr i))
      `(char=? c ,(integer->char (car i)))
      `(and
        ,@(append
           (if (> (car i) 0)
               `((char>=? c ,(integer->char (car i))))
               '())
           (if (< (car i) *max*)
               `((char<? c ,(integer->char (cdr i)))))))))
       
(define (fsm->code fsm)
  (let(
       (states (fsm-states fsm))
       ;;(transition-table (fsm-transition-table fsm))
       (final? (lambda (x) (memq x (fsm-final-states fsm))))
       (transitions-from (lambda (x) (filter (lambda (t) (eq? (car t) x)) (fsm-transition-table fsm))))
       )
    `(reflect (head tail row col pos datum sc fl)
              (let ((pos0 (pos datum)))
                (declare (not inline))
                (let ,(map (lambda (s) `(,s #f)) states)
                  ,@(map (lambda (s)
                           `(set! ,s (lambda (datum fdatum)
                                       (let(
                                            (c (head datum)))
                                         (cond
                                          ((eof-object? c)
                                           ,(if (final? s)
                                                `(sc (- (pos fdatum) pos0) fdatum fl)
                                                `(if fdatum
                                                     (sc (- (pos fdatum) pos0) fdatum fl)
                                                     (fl "failed re"))))
                                          ,@(map (lambda (t)
                                                   `(,(set->predicate (cadr t))
                                                     (,(caddr t) (tail datum) ,(if (final? s) 'datum 'fdatum))))
                                                 (transitions-from s))
                                          ,@(if (final? s)
                                                `((else (sc (- (pos datum) pos0) datum fl)))
                                                `((fdatum (sc (- (pos fdatum) pos0) fdatum fl))
                                                  (else (fl "failed re")))))))))
                         states)
                  (,(fsm-initial-state fsm) datum #f))))))
