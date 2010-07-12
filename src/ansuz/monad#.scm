(##namespace ("ansuz-monad#"
              lambda+
              return
              bind))

(include "reflect#.scm")
(declare (inlining-limit 0))

(define-macro (lambda+ f b)
  (let(
       (head (gensym 'head))
       (tail (gensym 'tail))
       (row (gensym 'row))
       (column (gensym 'column))
       (position (gensym 'position))
       (datum (gensym 'datum))
       (sc (gensym 'sc))
       (fl (gensym 'fl)))
    `(lambda ,(append f (list head tail row column position datum sc fl))
       (with-state (,head ,tail ,row ,column ,position ,datum ,sc ,fl) ,b))))

(define-macro (return v . x)
  (let(
       (head (gensym 'head))
       (tail (gensym 'tail))
       (row (gensym 'row))
       (column (gensym 'column))
       (position (gensym 'position))
       (datum (gensym 'datum))
       (sc (gensym 'sc))
       (fl (gensym 'fl)))
    `(with-state ,x
                 (reflect (,head ,tail ,row ,column, position ,datum ,sc ,fl)
                          (,sc ,v ,datum ,fl)))))

(define-macro (bind p n head tail row column position datum sc fl)
  (let(
       (v (car p))
       (m (cadr p))
       (datum1 (gensym 'datum1))
       (fl1 (gensym 'fl1)))
    `(with-state
      (,head ,tail ,row ,column ,position ,datum
       (lambda (,v ,datum1 ,fl1) (with-state (,head ,tail ,row ,column ,position ,datum1 ,sc ,fl1) ,n))
       ,fl)
      ,m)))


;; (define-macro (lambda+ f b)
;;   (let(
;;        (ts (gensym 'ts))
;;        (sc (gensym 'sc))
;;        (fl (gensym 'fle)))
;;     `(lambda ,(append f (list ts sc fl))
;;        (with-state (,ts ,sc ,fl) ,b))))

;; (define-macro (return v . x)
;;   (let(
;;        (ts (gensym 'ts))
;;        (sc (gensym 'sc))
;;        (fl (gensym 'fl)))
;;     `(with-state ,x
;;                 (reflect (,ts ,sc ,fl)
;;                          (,sc ,v ,ts ,fl)))))

;; (define-macro (bind p n ts sc fl)
;;   (let(
;;        (v (car p))
;;        (m (cadr p))
;;        (ts1 (gensym 'ts1))
;;        (fl1 (gensym 'fl1)))
;;     `(with-state
;;       (,ts
;;        (lambda (,v ,ts1 ,fl1) (with-state (,ts1 ,sc ,fl1) ,n))
;;        ,fl)
;;       ,m)))

