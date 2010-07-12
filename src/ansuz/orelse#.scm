;; Orelse
;; there are 2 orelses because there is one that is fully correct that maintains a
;; reference to the branching point even when the left branch matches, and another
;; that if the orelse left branch matches the whole parser returns that.
;; it's clearer by an example.
;; suppose
;; (>> (<> ab a) b)
;; where ab matches string 'ab' , a the string 'a' and b string 'b'
;; the above expression can match
;; 'abb' and 'ab' strings.
;; but if the <> operator is the one that do not mantain the backtrace reference
;; happens that it fails when input is 'ab':
;; the parser first recognize ab, then it expects another b so if simply fails.
;; in the case of orelse* when fail looking for b call backtracking and finds
;; a then b.
;; Why two orelses? The answer is that the not perfectly correct is more efficient
;; because do not create a reference to backtracking closure for each orelse
;; when left branch succeed.
;; In any case incorrect orelse works fine for many cases, and many times when
;; do not work fine we can simply transform it. We can rewrite our example as
;; (<> (>> ab b) (>> a b))
;; that works correctly.
;; For deterministic parsers the incorrect orelse is sufficient to describe any
;; parser.
;; orelse* is the theorical
;; orelse is the pratical

(##namespace ("ansuz-orelse#" orelse orelse*))
(##include "reflect#.scm")

(define-macro (orelse* m n . x)
  (let(
       (mm (gensym 'm))
       (nn (gensym 'n))
       
       (head (gensym 'head))
       (tail (gensym 'tail))
       (row (gensym 'row))
       (column (gensym 'column))
       (position (gensym 'position))
       (datum (gensym 'datum))
       
       (sc (gensym 'sc))
       (fl (gensym 'fl))
       (ignore (gensym '*ignore*)))
  `(with-state ,x
               (reify (mm ,n)
                      (reify (nn ,n)
                             (reflect (,head ,tail ,row ,column ,position ,datum ,sc ,fl)
                                      (,mm ,head ,tail ,row ,column ,position ,datum
                                           (lambda (,ignore) (,n ,head ,tail ,row ,column ,position ,datum ,sc ,fl)))))))))

;; (define-syntax orelse*
;;   (syntax-rules ()
;;     ((_ ?m ?n ?x ...)
;;      (with-state (?x ...)
;;                  (reify (m ?m)
;;                         (reify (n ?n)
;;                                (reflect (ts sc fl)
;;                                         (m ts
;;                                            sc
;;                                            (lambda (r) (n ts sc fl))))))))))

(define-macro (orelse m n . x)
  (let(
       (mm (gensym 'm))
       (nn (gensym 'n))
       
       (head (gensym 'head))
       (tail (gensym 'tail))
       (row (gensym 'row))
       (column (gensym 'column))
       (position (gensym 'position))
       (datum (gensym 'datum))
       
       (sc (gensym 'sc))
       (fl (gensym 'fl))
       (v (gensym 'v))
       
       (datum1 (gensym 'datum1))
       
       (fl1 (gensym 'fl1))
       (ignore (gensym 'ignore)))
    `(with-state ,x
                 (reify (,mm ,m)
                        (reify (,nn ,n)
                               (reflect (,head ,tail ,row ,column ,position ,datum ,sc ,fl)
                                        (,mm ,head ,tail ,row ,column ,position ,datum
                                             (lambda (,v ,datum1 ,fl1) (,sc ,v ,datum1 ,fl))
                                             (lambda (,ignore) (,nn ,head ,tail ,row ,column, position ,datum ,sc ,fl)))))))))

;; (define-syntax orelse
;;   (syntax-rules ()
;;     ((_ ?m ?n ?x ...)
;;      (with-state (?x ...)
;;                  (reify (m ?m)
;;                         (reify (n ?n)
;;                                (reflect (ts sc fl)
;;                                         (m ts
;;                                            (lambda (v ts fl1) (sc v ts fl))
;;                                            (lambda (*ignore*) (n ts sc fl))))))))))
