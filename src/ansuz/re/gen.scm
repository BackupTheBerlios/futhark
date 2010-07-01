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
    `(reflect (ts sc fl)
              (let ((pos0 (source-pos ts)))
                (declare (not inline))
                (let ,(map (lambda (s) `(,s #f)) states)
                  ,@(map (lambda (s)
                           `(set! ,s (lambda (ts fts)
                                       (let(
                                            (c (source-car ts)))
                                         (cond
                                          ((eof-object? c)
                                           ,(if (final? s)
                                                `(sc (- (source-pos fts) pos0) fts fl)
                                                `(if fts
                                                     (sc (- (source-pos fts) pos0) fts fl)
                                                     (fl "failed re"))))
                                          ,@(map (lambda (t)
                                                   `(,(set->predicate (cadr t))
                                                     (,(caddr t) (source-cdr ts) ,(if (final? s) 'ts 'fts))))
                                                 (transitions-from s))
                                          ,@(if (final? s)
                                                `((else (sc (- (source-pos ts) pos0) ts fl)))
                                                `((fts (sc (- (source-pos fts) pos0) fts fl))
                                                  (else (fl "failed re")))))))))
                         states)
                  (,(fsm-initial-state fsm) ts #f))))))
