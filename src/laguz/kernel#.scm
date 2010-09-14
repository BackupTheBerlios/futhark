
(define-macro+ (success) `(return #t))

(define-macro+ (fail)
  (let(
       (mv (gensym 'mv))
       (oc (gensym 'oc))
       (zz (gensym 'zz))
       (ct (gensym 'ct))
       (bt (gensym 'bt)))
  `(reflect (,mv ,oc ,zz ,ct ,bt) (,bt ,mv))))

(define-macro+ (sequence m n)
  `(bind (,(gensym 'ignore) ,m) ,n))

(define-macro+ (negate m)
  (let(
       (mv (gensym 'mv))
       (oc (gensym 'oc))
       (zz (gensym 'zz))
       (ct (gensym 'ct))
       (bt (gensym 'bt))
       (mm (gensym 'm))
       (mv1 (gensym 'mv))
       (oc1 (gensym 'oc))
       (zz1 (gensym 'zz))
       (bt1 (gensym 'bt)))
    `(reify (,mm ,m)
            (reflect (,mv ,oc ,zz ,ct ,bt)
                     (,mm ,mv ,oc ,zz (lambda (,v ,mv1 ,oc1 ,zz1 ,bt1) (,bt ,mv1))
                          (lambda (,mv1) (,ct #t ,mv1 ,oc ,zz ,bt)))))))

(define-macro+ (if+ t? m n)
  (let(
       (mv (gensym 'mv))
       (oc (gensym 'oc))
       (ct (gensym 'ct))
       (zz (gensym 'zz))
       (bt (gensym 'bt))
       (mm (gensym 'm))
       (nn (gensym 'n)))
    `(reify (,mm ,m)
            (reify (,nn ,n)
                   (reflect (,mv ,oc ,zz ,ct ,bt)
                            (if ,t?
                                (,mm ,mv ,oc ,zz ,ct ,bt)
                                (,nn ,mv ,oc ,zz ,ct ,bt)))))))

(define-macro+ (when+ t? m)
  (let(
       (mv (gensym 'mv))
       (oc (gensym 'oc))
       (zz (gensym 'zz))
       (ct (gensym 'ct))
       (bt (gensym 'bt))
       (mm (gensym 'm))
       (nn (gensym 'n)))
    `(reify (,mm ,m)
           (reflect (,mv ,oc ,zz ,ct ,bt)
                    (if ,t? (,mm ,mv ,oc ,zz ,ct ,bt) (,bt ,mv))))))

(define-macro+ (let+ v b)
  (let(
       (bb (gensym 'b))
       (mv (gensym 'mv))
       (oc (gensym 'oc))
       (zz (gensym 'zz))
       (ct (gensym 'ct))
       (bt (gensym 'bt)))

    `(reify (,bb ,b)
            (reflect (,mv ,oc ,zz ,ct ,bt)
                     (let ,v (,bb ,mv ,oc ,zz ,ct ,bt))))))


(define-macro+ (letenv+ vs b)
  `(let+ ,(map (lambda (v) `(,v (make-variable))) vs)
     ,b))

(define-macro+ (newvar)
  (let(
       (mv (gensym 'mv))
       (oc (gensym 'oc))
       (zz (gensym 'zz))
       (ct (gensym 'ct))
       (bt (gensym 'bt)))
    `(reflect (,mv ,oc ,zz ,ct ,bt)
              (,ct (make-variable) ,mv ,oc ,zz ,bt))))

(define-macro+ (occur-check-set! v)
  (let(
       (mv (gensym 'mv))
       (oc (gensym 'oc))
       (zz (gensym 'zz))
       (ct (gensym 'ct))
       (bt (gensym 'bt)))
    `(reflect (,mv ,oc ,zz ,ct ,bt)
              (,ct ,v ,mv ,v ,zz ,bt))))

(define-macro+ (cut-set! v)
  (let(
       (mv (gensym 'mv))
       (oc (gensym 'oc))
       (zz (gensym 'zz))
       (ct (gensym 'ct))
       (bt (gensym 'bt)))
    `(reflect (,mv ,oc ,zz ,ct ,bt)
              (,ct ,v ,mv ,oc ,v ,bt))))

(define-macro+ (cut)
  `(cut-set! #t))
