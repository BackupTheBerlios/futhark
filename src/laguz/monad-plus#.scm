(##namespace ("prolog-monad#"
              lambda+
              define+
              return
              bind
              orelse))

(include "reflect#.scm")

(##namespace ("laguz#" reset-variables))

(define-macro (lambda+ f b)
  (let(
       (mv (gensym 'mv))
       (oc (gensym 'oc))
       (zz (gensym 'zz))
       (ct (gensym 'ct))
       (bt (gensym 'bt)))
    `(lambda ,(append f (list mv oc zz ct bt))
       (with-state (,mv ,oc ,zz ,ct ,bt) ,b))))

(define-macro (define+ a b)
  `(define ,(car a) (lambda+ ,(cdr a) ,@b)))

(define-macro (return v . x)
  (let(
       (mv (gensym 'mv))
       (oc (gensym 'oc))
       (zz (gensym 'zz))
       (ct (gensym 'ct))
       (bt (gensym 'bt)))
    `(with-state ,x (reflect (,mv ,oc ,zz ,ct ,bt) (,ct ,v ,mv ,oc ,zz ,bt)))))

(define-macro (bind p n . x)
  (let(
       (v (car p))
       (m (cadr p))
       (mm (gensym 'mm))
       (nn (gensym 'nn))
       (mv (gensym 'mv))
       (oc (gensym 'oc))
       (zz (gensym 'zz))
       (ct (gensym 'ct))
       (bt (gensym 'bt))
       (mv1 (gensym 'mv))
       (oc1 (gensym 'oc))
       (zz1 (gensym 'zz))
       (bt1 (gensym 'bt)))
    `(with-state ,x
                 (reify (,mm ,m)
                        (reify (,nn ,n)
                               (reflect (,mv ,oc ,zz ,ct ,bt)
                                        (,mm ,mv ,oc ,zz (lambda (,v ,mv1 ,oc1 ,zz1 ,bt1) (,nn ,mv1 ,oc1 ,zz1 ,ct ,bt1))
                                             ,bt)))))))

(define-macro (orelse m n . x)
  (let(
       (mm (gensym 'm))
       (nn (gensym 'n))
       (mv (gensym 'mv))
       (oc (gensym 'oc))
       (zz (gensym 'zz))
       (ct (gensym 'ct))
       (bt (gensym 'bt))
       (v (gensym 'v))
       (mv1 (gensym 'mv))
       (oc1 (gensym 'oc))
       (zz1 (gensym 'zz))
       (bt1 (gensym 'bt1)))
    `(with-state ,x
                 (reify (,mm ,m)
                        (reify (,nn ,n)
                               (reflect (,mv ,oc ,zz ,ct ,bt)
                                        (,mm ,mv ,oc ,zz
                                             (lambda (,v ,mv1 ,oc1 ,zz1 ,bt1) (,ct ,v ,mv1 ,oc1 ,zz1 (if ,zz ,bt ,bt1)))
                                             (lambda (,mv1)
                                               (reset-variables! ,mv1 ,mv)
                                               (,nn ,mv ,oc ,zz ,ct ,bt)))))))))



;; (define-macro (orelse-cut m n . x)
;;   (let(
;;        (mm (gensym 'm))
;;        (nn (gensym 'n))
;;        (mv (gensym 'mv))
;;        (mv1 (gensym 'mv))
;;        (ct (gensym 'ct))
;;        (bt (gensym 'bt)))
;;     `(with-state ,x
;;                  (reify (,mm ,m)
;;                         (reify (,nn ,n)
;;                                (reflect (,mv ,ct ,bt)
;;                                         (,mm ,mv ,ct
;;                                              (lambda (,mv1)
;;                                                (reset-variables! ,mv1 ,mv)
;;                                                (,nn ,mv ,ct ,bt)))))))))

;; (define (reset-variables! ms m0)
;;   (let reset ((xs ms) (ys '()))
;;     (if (eq? xs m0) ys
;;         (begin
;;           (variable-reset! (car xs))
;;           (reset (cdr xs) (cons (car xs) ys))))))


(define-macro (define-macro+ h b)
  (let(
       (mv (gensym 'mv))
       (oc (gensym 'oc))
       (zz (gensym 'zz))
       (ct (gensym 'ct))
       (bt (gensym 'bt)))
    
    `(define-macro ,(append h (list mv oc zz ct bt))
       (list 'with-state (list ,mv ,oc ,zz ,ct ,bt) ,b))))