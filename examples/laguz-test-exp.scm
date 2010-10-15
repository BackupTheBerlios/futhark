Expansion:

(define laguz#*-var-* ('#<procedure #2 ##list> 'var))

(define laguz#occur?
  (lambda (v b)
    (letrec ((occur (lambda (v b)
                      (if ('#<procedure #3 ##eq?> b v)
                          #t
                          (if (and ('#<procedure #4 ##pair?> b)
                                   ('#<procedure #3 ##eq?>
                                    ('#<procedure #5 ##car> b)
                                    laguz#*-var-*))
                              (occur v (laguz#subst b))
                              (if ('#<procedure #4 ##pair?> b)
                                  (or (occur v ('#<procedure #5 ##car> b))
                                      (occur v ('#<procedure #6 ##cdr> b)))
                                  #f))))))
      (occur v b))))

(define laguz#subst
  (lambda (v)
    (if (and ('#<procedure #4 ##pair?> v)
             ('#<procedure #3 ##eq?> ('#<procedure #5 ##car> v) laguz#*-var-*))
        (let ((val ('#<procedure #6 ##cdr> v)))
          (if ('#<procedure #3 ##eq?> val #!void) v (laguz#subst val)))
        (if ('#<procedure #4 ##pair?> v)
            ('#<procedure #7 ##cons>
             (laguz#subst ('#<procedure #5 ##car> v))
             (laguz#subst ('#<procedure #6 ##cdr> v)))
            v))))

(define laguz#reset-variables!
  (lambda (ms m0)
    (letrec ((reset (lambda (m0 xs ys)
                      (if ('#<procedure #3 ##eq?> xs m0)
                          ys
                          (let ((begin-temp.0
                                 ('#<procedure #8 ##set-cdr!>
                                  ('#<procedure #5 ##car> xs)
                                  #!void)))
                            (let ((xs ('#<procedure #6 ##cdr> xs))
                                  (ys ('#<procedure #7 ##cons>
                                       ('#<procedure #5 ##car> xs)
                                       ys)))
                              (if ('#<procedure #3 ##eq?> xs m0)
                                  ys
                                  (let ((begin-temp.0
                                         ('#<procedure #8 ##set-cdr!>
                                          ('#<procedure #5 ##car> xs)
                                          #!void)))
                                    (reset m0
                                           ('#<procedure #6 ##cdr> xs)
                                           ('#<procedure #7 ##cons>
                                            ('#<procedure #5 ##car> xs)
                                            ys))))))))))
      (reset m0 ms '()))))

(define laguz#unify
  (lambda (a b #:mv0 #:oc1 #:zz2 #:ct3 #:bt4)
    (letrec ((unify (lambda (#:oc1 a b mv cn)
                      (if ('#<procedure #3 ##eq?> a b)
                          (cn #t mv)
                          (if (and ('#<procedure #4 ##pair?> a)
                                   ('#<procedure #3 ##eq?>
                                    ('#<procedure #5 ##car> a)
                                    laguz#*-var-*))
                              (let ((val ('#<procedure #6 ##cdr> a)))
                                (if ('#<procedure #3 ##eq?> val #!void)
                                    (if (and #:oc1 (laguz#occur? a b))
                                        (cn #f mv)
                                        (let ((begin-temp.1
                                               ('#<procedure #8 ##set-cdr!>
                                                a
                                                b)))
                                          (cn #t
                                              ('#<procedure #7 ##cons> a mv))))
                                    (unify #:oc1 val b mv cn)))
                              (if (and ('#<procedure #4 ##pair?> b)
                                       ('#<procedure #3 ##eq?>
                                        ('#<procedure #5 ##car> b)
                                        laguz#*-var-*))
                                  (let ((val ('#<procedure #6 ##cdr> b)))
                                    (if ('#<procedure #3 ##eq?> val #!void)
                                        (if (and #:oc1 (laguz#occur? b a))
                                            (cn #f mv)
                                            (let ((begin-temp.2
                                                   ('#<procedure #8 ##set-cdr!>
                                                    b
                                                    a)))
                                              (cn #t
                                                  ('#<procedure #7 ##cons>
                                                   b
                                                   mv))))
                                        (unify #:oc1 a val mv cn)))
                                  (if (and ('#<procedure #4 ##pair?> a)
                                           ('#<procedure #4 ##pair?> b))
                                      (unify #:oc1
                                             ('#<procedure #5 ##car> a)
                                             ('#<procedure #5 ##car> b)
                                             mv
                                             (lambda (r mv1)
                                               (if r
                                                   (unify #:oc1
                                                          ('#<procedure #6 ##cdr>
                                                           a)
                                                          ('#<procedure #6 ##cdr>
                                                           b)
                                                          mv1
                                                          cn)
                                                   (cn r mv1))))
                                      (cn #f mv))))))))
      (unify #:oc1
             a
             b
             #:mv0
             (lambda (r mv1)
               (if r (#:ct3 #t mv1 #:oc1 #:zz2 #:bt4) (#:bt4 mv1)))))))

(define laguz#list-append
  (lambda (#:f75 #:f76 #:f77 #:mv78 #:oc79 #:zz80 #:ct81 #:bt82)
    (let ((#:bt102 (lambda (#:mv91)
                     (let ((begin-temp.3
                            (laguz#reset-variables! #:mv91 #:mv78)))
                       (let ((X ('#<procedure #7 ##cons> laguz#*-var-* #!void))
                             (#:mv114 #:mv78)
                             (#:oc115 #:oc79)
                             (#:zz116 #:zz80)
                             (#:bt117 #:bt82))
                         (let ((A ('#<procedure #7 ##cons>
                                   laguz#*-var-*
                                   #!void)))
                           (let ((b ('#<procedure #7 ##cons> A X)))
                             (letrec ((unify (lambda (#:oc115 a b mv cn)
                                               (if ('#<procedure #3 ##eq?> a b)
                                                   (cn #t mv)
                                                   (if (and ('#<procedure #4 ##pair?>
                                                             a)
                                                            ('#<procedure #3 ##eq?>
                                                             ('#<procedure #5 ##car>
                                                              a)
                                                             laguz#*-var-*))
                                                       (let ((val ('#<procedure #6 ##cdr>
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                           a)))
                 (if ('#<procedure #3 ##eq?> val #!void)
                     (if (and #:oc115
                              (letrec ((occur (lambda (a b)
                                                (if ('#<procedure #3 ##eq?>
                                                     b
                                                     a)
                                                    #t
                                                    (if (and ('#<procedure #4 ##pair?>
                                                              b)
                                                             ('#<procedure #3 ##eq?>
                                                              ('#<procedure #5 ##car>
                                                               b)
                                                              laguz#*-var-*))
                                                        (let ((b (laguz#subst
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                          b)))
                  (if ('#<procedure #3 ##eq?> b a)
                      #t
                      (if (and ('#<procedure #4 ##pair?> b)
                               ('#<procedure #3 ##eq?>
                                ('#<procedure #5 ##car> b)
                                laguz#*-var-*))
                          (let ((b (laguz#subst b)))
                            (if ('#<procedure #3 ##eq?> b a)
                                #t
                                (if (and ('#<procedure #4 ##pair?> b)
                                         ('#<procedure #3 ##eq?>
                                          ('#<procedure #5 ##car> b)
                                          laguz#*-var-*))
                                    (let ((b (laguz#subst b)))
                                      (if ('#<procedure #3 ##eq?> b a)
                                          #t
                                          (if (and ('#<procedure #4 ##pair?> b)
                                                   ('#<procedure #3 ##eq?>
                                                    ('#<procedure #5 ##car> b)
                                                    laguz#*-var-*))
                                              (let ((b (laguz#subst b)))
                                                (if ('#<procedure #3 ##eq?>
                                                     b
                                                     a)
                                                    #t
                                                    (if (and ('#<procedure #4 ##pair?>
                                                              b)
                                                             ('#<procedure #3 ##eq?>
                                                              ('#<procedure #5 ##car>
                                                               b)
                                                              laguz#*-var-*))
                                                        (occur a
                                                               (laguz#subst b))
                                                        (if ('#<procedure #4 ##pair?>
                                                             b)
                                                            (or (occur a
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                               ('#<procedure #5 ##car> b))
                        (occur a ('#<procedure #6 ##cdr> b)))
                    #f))))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                              (if ('#<procedure #4 ##pair?> b)
                                                  (or (occur a
                                                             ('#<procedure #5 ##car>
                                                              b))
                                                      (occur a
                                                             ('#<procedure #6 ##cdr>
                                                              b)))
                                                  #f))))
                                    (if ('#<procedure #4 ##pair?> b)
                                        (or (occur a
                                                   ('#<procedure #5 ##car> b))
                                            (occur a
                                                   ('#<procedure #6 ##cdr> b)))
                                        #f))))
                          (if ('#<procedure #4 ##pair?> b)
                              (or (occur a ('#<procedure #5 ##car> b))
                                  (occur a ('#<procedure #6 ##cdr> b)))
                              #f))))
                (if ('#<procedure #4 ##pair?> b)
                    (or (occur a ('#<procedure #5 ##car> b))
                        (occur a ('#<procedure #6 ##cdr> b)))
                    #f))))))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                (occur a b)))
                         (cn #f mv)
                         (let ((begin-temp.1
                                ('#<procedure #8 ##set-cdr!> a b)))
                           (cn #t ('#<procedure #7 ##cons> a mv))))
                     (unify #:oc115 val b mv cn)))
               (if (and ('#<procedure #4 ##pair?> b)
                        ('#<procedure #3 ##eq?>
                         ('#<procedure #5 ##car> b)
                         laguz#*-var-*))
                   (let ((val ('#<procedure #6 ##cdr> b)))
                     (if ('#<procedure #3 ##eq?> val #!void)
                         (if (and #:oc115 (laguz#occur? b a))
                             (cn #f mv)
                             (let ((begin-temp.2
                                    ('#<procedure #8 ##set-cdr!> b a)))
                               (cn #t ('#<procedure #7 ##cons> b mv))))
                         (unify #:oc115 a val mv cn)))
                   (if (and ('#<procedure #4 ##pair?> a)
                            ('#<procedure #4 ##pair?> b))
                       (unify #:oc115
                              ('#<procedure #5 ##car> a)
                              ('#<procedure #5 ##car> b)
                              mv
                              (lambda (r mv1)
                                (if r
                                    (unify #:oc115
                                           ('#<procedure #6 ##cdr> a)
                                           ('#<procedure #6 ##cdr> b)
                                           mv1
                                           cn)
                                    (cn r mv1))))
                       (cn #f mv))))))))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                               (unify #:oc115
                                      #:f75
                                      b
                                      #:mv114
                                      (lambda (r mv1)
                                        (if r
                                            (let ((Y ('#<procedure #7 ##cons>
                                                      laguz#*-var-*
                                                      #!void)))
                                              (laguz#unify
                                               #:f76
                                               Y
                                               mv1
                                               #:oc115
                                               #:zz116
                                               (lambda (#:ignore167
                                                        #:mv175
                                                        #:oc176
                                                        #:zz177
                                                        #:bt178)
                                                 (let ((Z ('#<procedure #7 ##cons>
                                                           laguz#*-var-*
                                                           #!void)))
                                                   (laguz#unify
                                                    #:f77
                                                    ('#<procedure #7 ##cons>
                                                     A
                                                     Z)
                                                    #:mv175
                                                    #:oc176
                                                    #:zz177
                                                    (lambda (#:ignore195
                                                             #:mv203
                                                             #:oc204
                                                             #:zz205
                                                             #:bt206)
                                                      (laguz#list-append
                                                       X
                                                       Y
                                                       Z
                                                       #:mv203
                                                       #:oc204
                                                       #:zz205
                                                       #:ct81
                                                       #:bt206))
                                                    #:bt178)))
                                               #:bt117))
                                            (#:bt117 mv1)))))))))))
          (#:ct101 (lambda (#:zz80
                            #:ct81
                            #:bt82
                            #:v90
                            #:mv91
                            #:oc92
                            #:zz93
                            #:bt194)
                     (#:ct81 #:v90
                             #:mv91
                             #:oc92
                             #:zz93
                             (if #:zz80 #:bt82 #:bt194)))))
      (let ((a #:f75)
            (b '())
            (#:mv0 #:mv78)
            (#:oc1 #:oc79)
            (#:zz2 #:zz80)
            (#:ct3 (lambda (#:f76
                            #:f77
                            #:zz80
                            #:ct81
                            #:bt82
                            #:ignore95
                            #:mv103
                            #:oc104
                            #:zz105
                            #:bt106)
                     (let ((U ('#<procedure #7 ##cons> laguz#*-var-* #!void))
                           (#:mv214 #:mv103)
                           (#:oc215 #:oc104)
                           (#:zz216 #:zz105)
                           (#:bt217 #:bt106))
                       (laguz#unify
                        #:f76
                        U
                        #:mv214
                        #:oc215
                        #:zz216
                        (lambda (#:ignore223 #:mv231 #:oc232 #:zz233 #:bt234)
                          (laguz#unify
                           #:f77
                           U
                           #:mv231
                           #:oc232
                           #:zz233
                           (lambda (#:ignore235
                                    #:mv243
                                    #:oc244
                                    #:zz245
                                    #:bt246)
                             (#:ct101 #:zz80
                                      #:ct81
                                      #:bt82
                                      'ok
                                      #:mv243
                                      #:oc244
                                      #:zz245
                                      #:bt246))
                           #:bt234))
                        #:bt217))))
            (#:bt4 #:bt102))
        (letrec ((unify (lambda (#:oc1 a b mv cn)
                          (if ('#<procedure #3 ##eq?> a b)
                              (cn #t mv)
                              (if (and ('#<procedure #4 ##pair?> a)
                                       ('#<procedure #3 ##eq?>
                                        ('#<procedure #5 ##car> a)
                                        laguz#*-var-*))
                                  (let ((val ('#<procedure #6 ##cdr> a)))
                                    (if ('#<procedure #3 ##eq?> val #!void)
                                        (if (and #:oc1
                                                 (let ((v a) (b b))
                                                   (letrec ((occur (lambda (v
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                                    b)
                             (if ('#<procedure #3 ##eq?> b v)
                                 #t
                                 (if (and ('#<procedure #4 ##pair?> b)
                                          ('#<procedure #3 ##eq?>
                                           ('#<procedure #5 ##car> b)
                                           laguz#*-var-*))
                                     (let ((b (laguz#subst b)))
                                       (if ('#<procedure #3 ##eq?> b v)
                                           #t
                                           (if (and ('#<procedure #4 ##pair?>
                                                     b)
                                                    ('#<procedure #3 ##eq?>
                                                     ('#<procedure #5 ##car> b)
                                                     laguz#*-var-*))
                                               (occur v (laguz#subst b))
                                               (if ('#<procedure #4 ##pair?> b)
                                                   (or (occur v
                                                              ('#<procedure #5 ##car>
                                                               b))
                                                       (occur v
                                                              ('#<procedure #6 ##cdr>
                                                               b)))
                                                   #f))))
                                     (if ('#<procedure #4 ##pair?> b)
                                         (or (occur v
                                                    ('#<procedure #5 ##car> b))
                                             (occur v
                                                    ('#<procedure #6 ##cdr>
                                                     b)))
                                         #f))))))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                     (occur v b))))
                                            (cn #f mv)
                                            (let ((begin-temp.1
                                                   ('#<procedure #8 ##set-cdr!>
                                                    a
                                                    b)))
                                              (cn #t
                                                  ('#<procedure #7 ##cons>
                                                   a
                                                   mv))))
                                        (unify #:oc1 val b mv cn)))
                                  (if (and ('#<procedure #4 ##pair?> b)
                                           ('#<procedure #3 ##eq?>
                                            ('#<procedure #5 ##car> b)
                                            laguz#*-var-*))
                                      (let ((val ('#<procedure #6 ##cdr> b)))
                                        (if ('#<procedure #3 ##eq?> val #!void)
                                            (if (and #:oc1 (laguz#occur? b a))
                                                (cn #f mv)
                                                (let ((begin-temp.2
                                                       ('#<procedure #8 ##set-cdr!>
                                                        b
                                                        a)))
                                                  (cn #t
                                                      ('#<procedure #7 ##cons>
                                                       b
                                                       mv))))
                                            (unify #:oc1 a val mv cn)))
                                      (if (and ('#<procedure #4 ##pair?> a)
                                               ('#<procedure #4 ##pair?> b))
                                          (unify #:oc1
                                                 ('#<procedure #5 ##car> a)
                                                 ('#<procedure #5 ##car> b)
                                                 mv
                                                 (lambda (r mv1)
                                                   (if r
                                                       (unify #:oc1
                                                              ('#<procedure #6 ##cdr>
                                                               a)
                                                              ('#<procedure #6 ##cdr>
                                                               b)
                                                              mv1
                                                              cn)
                                                       (cn r mv1))))
                                          (cn #f mv))))))))
          (unify #:oc1
                 a
                 b
                 #:mv0
                 (lambda (r mv1)
                   (if r
                       (#:ct3 #:f76
                              #:f77
                              #:zz80
                              #:ct81
                              #:bt82
                              #t
                              mv1
                              #:oc1
                              #:zz2
                              #:bt4)
                       (#:bt4 mv1)))))))))

(define laguz#father-child
  (lambda (#:f252 #:f253 #:mv254 #:oc255 #:zz256 #:ct257 #:bt258)
    (let ((#:bt278 (lambda (#:mv267)
                     (let ((begin-temp.5
                            (laguz#reset-variables! #:mv267 #:mv254)))
                       (let ((#:bt302 (lambda (#:mv291)
                                        (let ((begin-temp.4
                                               (laguz#reset-variables!
                                                #:mv291
                                                #:mv254)))
                                          (laguz#unify
                                           #:f252
                                           'c
                                           #:mv254
                                           #:oc255
                                           #:zz256
                                           (lambda (#:ignore307
                                                    #:mv315
                                                    #:oc316
                                                    #:zz317
                                                    #:bt318)
                                             (laguz#unify
                                              #:f253
                                              'd
                                              #:mv315
                                              #:oc316
                                              #:zz317
                                              (lambda (#:ignore319
                                                       #:mv327
                                                       #:oc328
                                                       #:zz329
                                                       #:bt330)
                                                (#:ct257 'ok
                                                         #:mv327
                                                         #:oc328
                                                         #:zz329
                                                         #:bt330))
                                              #:bt318))
                                           #:bt258))))
                             (#:ct301 (lambda (#:zz256
                                               #:ct257
                                               #:bt258
                                               #:v290
                                               #:mv291
                                               #:oc292
                                               #:zz293
                                               #:bt1294)
                                        (#:ct257 #:v290
                                                 #:mv291
                                                 #:oc292
                                                 #:zz293
                                                 (if #:zz256
                                                     #:bt258
                                                     #:bt1294)))))
                         (laguz#unify
                          #:f252
                          'b
                          #:mv254
                          #:oc255
                          #:zz256
                          (lambda (#:ignore295 #:mv303 #:oc304 #:zz305 #:bt306)
                            (laguz#unify
                             #:f253
                             'c
                             #:mv303
                             #:oc304
                             #:zz305
                             (lambda (#:ignore336
                                      #:mv344
                                      #:oc345
                                      #:zz346
                                      #:bt347)
                               (#:ct301 #:zz256
                                        #:ct257
                                        #:bt258
                                        'ok
                                        #:mv344
                                        #:oc345
                                        #:zz346
                                        #:bt347))
                             #:bt306))
                          #:bt302)))))
          (#:ct277 (lambda (#:zz256
                            #:ct257
                            #:bt258
                            #:v266
                            #:mv267
                            #:oc268
                            #:zz269
                            #:bt1270)
                     (#:ct257 #:v266
                              #:mv267
                              #:oc268
                              #:zz269
                              (if #:zz256 #:bt258 #:bt1270)))))
      (laguz#unify
       #:f252
       'a
       #:mv254
       #:oc255
       #:zz256
       (lambda (#:ignore271 #:mv279 #:oc280 #:zz281 #:bt282)
         (laguz#unify
          #:f253
          'b
          #:mv279
          #:oc280
          #:zz281
          (lambda (#:ignore353 #:mv361 #:oc362 #:zz363 #:bt364)
            (#:ct277 #:zz256
                     #:ct257
                     #:bt258
                     'ok
                     #:mv361
                     #:oc362
                     #:zz363
                     #:bt364))
          #:bt282))
       #:bt278))))

(define laguz#ancestor
  (lambda (#:f370 #:f371 #:mv372 #:oc373 #:zz374 #:ct375 #:bt376)
    (let ((#:bt395 (lambda (#:mv385)
                     (let ((begin-temp.6
                            (laguz#reset-variables! #:mv385 #:mv372)))
                       (let ((A ('#<procedure #7 ##cons> laguz#*-var-* #!void))
                             (#:mv407 #:mv372)
                             (#:oc408 #:oc373)
                             (#:zz409 #:zz374)
                             (#:bt410 #:bt376))
                         (letrec ((unify (lambda (#:oc408 a b mv cn)
                                           (if ('#<procedure #3 ##eq?> a b)
                                               (cn #t mv)
                                               (if (and ('#<procedure #4 ##pair?>
                                                         a)
                                                        ('#<procedure #3 ##eq?>
                                                         ('#<procedure #5 ##car>
                                                          a)
                                                         laguz#*-var-*))
                                                   (let ((val ('#<procedure #6 ##cdr>
                                                               a)))
                                                     (if ('#<procedure #3 ##eq?>
                                                          val
                                                          #!void)
                                                         (if (and #:oc408
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                          (laguz#occur? a b))
                     (cn #f mv)
                     (let ((begin-temp.1 ('#<procedure #8 ##set-cdr!> a b)))
                       (cn #t ('#<procedure #7 ##cons> a mv))))
                 (unify #:oc408 val b mv cn)))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                   (if (and ('#<procedure #4 ##pair?>
                                                             b)
                                                            ('#<procedure #3 ##eq?>
                                                             ('#<procedure #5 ##car>
                                                              b)
                                                             laguz#*-var-*))
                                                       (let ((val ('#<procedure #6 ##cdr>
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                           b)))
                 (if ('#<procedure #3 ##eq?> val #!void)
                     (if (and #:oc408 (laguz#occur? b a))
                         (cn #f mv)
                         (let ((begin-temp.2
                                ('#<procedure #8 ##set-cdr!> b a)))
                           (cn #t ('#<procedure #7 ##cons> b mv))))
                     (unify #:oc408 a val mv cn)))
               (if (and ('#<procedure #4 ##pair?> a)
                        ('#<procedure #4 ##pair?> b))
                   (unify #:oc408
                          ('#<procedure #5 ##car> a)
                          ('#<procedure #5 ##car> b)
                          mv
                          (lambda (r mv1)
                            (if r
                                (unify #:oc408
                                       ('#<procedure #6 ##cdr> a)
                                       ('#<procedure #6 ##cdr> b)
                                       mv1
                                       cn)
                                (cn r mv1))))
                   (cn #f mv))))))))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                           (unify #:oc408
                                  #:f370
                                  A
                                  #:mv407
                                  (lambda (r mv1)
                                    (if r
                                        (let ((#:ignore416 #t)
                                              (#:mv424 mv1)
                                              (#:oc425 #:oc408)
                                              (#:zz426 #:zz409)
                                              (#:bt427 #:bt410))
                                          (let ((B ('#<procedure #7 ##cons>
                                                    laguz#*-var-*
                                                    #!void)))
                                            (laguz#unify
                                             #:f371
                                             B
                                             #:mv424
                                             #:oc425
                                             #:zz426
                                             (lambda (#:ignore444
                                                      #:mv452
                                                      #:oc453
                                                      #:zz454
                                                      #:bt455)
                                               (laguz#father-child
                                                A
                                                B
                                                #:mv452
                                                #:oc453
                                                #:zz454
                                                #:ct375
                                                #:bt455))
                                             #:bt427)))
                                        (#:bt410 mv1)))))))))
          (#:ct394 (lambda (#:v384 #:mv385 #:oc386 #:zz387 #:bt1388)
                     (#:ct375 #:v384
                              #:mv385
                              #:oc386
                              #:zz387
                              (if #:zz374 #:bt376 #:bt1388)))))
      (let ((A ('#<procedure #7 ##cons> laguz#*-var-* #!void))
            (#:mv396 #:mv372)
            (#:oc397 #:oc373)
            (#:zz398 #:zz374)
            (#:bt399 #:bt395))
        (letrec ((unify (lambda (#:oc397 a b mv cn)
                          (if ('#<procedure #3 ##eq?> a b)
                              (cn #t mv)
                              (if (and ('#<procedure #4 ##pair?> a)
                                       ('#<procedure #3 ##eq?>
                                        ('#<procedure #5 ##car> a)
                                        laguz#*-var-*))
                                  (let ((val ('#<procedure #6 ##cdr> a)))
                                    (if ('#<procedure #3 ##eq?> val #!void)
                                        (if (and #:oc397
                                                 (letrec ((occur (lambda (a b)
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                           (if ('#<procedure #3 ##eq?> b a)
                               #t
                               (if (and ('#<procedure #4 ##pair?> b)
                                        ('#<procedure #3 ##eq?>
                                         ('#<procedure #5 ##car> b)
                                         laguz#*-var-*))
                                   (let ((b (laguz#subst b)))
                                     (if ('#<procedure #3 ##eq?> b a)
                                         #t
                                         (if (and ('#<procedure #4 ##pair?> b)
                                                  ('#<procedure #3 ##eq?>
                                                   ('#<procedure #5 ##car> b)
                                                   laguz#*-var-*))
                                             (let ((b (laguz#subst b)))
                                               (if ('#<procedure #3 ##eq?> b a)
                                                   #t
                                                   (if (and ('#<procedure #4 ##pair?>
                                                             b)
                                                            ('#<procedure #3 ##eq?>
                                                             ('#<procedure #5 ##car>
                                                              b)
                                                             laguz#*-var-*))
                                                       (let ((b (laguz#subst
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                         b)))
                 (if ('#<procedure #3 ##eq?> b a)
                     #t
                     (if (and ('#<procedure #4 ##pair?> b)
                              ('#<procedure #3 ##eq?>
                               ('#<procedure #5 ##car> b)
                               laguz#*-var-*))
                         (occur a (laguz#subst b))
                         (if ('#<procedure #4 ##pair?> b)
                             (or (occur a ('#<procedure #5 ##car> b))
                                 (occur a ('#<procedure #6 ##cdr> b)))
                             #f))))
               (if ('#<procedure #4 ##pair?> b)
                   (or (occur a ('#<procedure #5 ##car> b))
                       (occur a ('#<procedure #6 ##cdr> b)))
                   #f))))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                             (if ('#<procedure #4 ##pair?> b)
                                                 (or (occur a
                                                            ('#<procedure #5 ##car>
                                                             b))
                                                     (occur a
                                                            ('#<procedure #6 ##cdr>
                                                             b)))
                                                 #f))))
                                   (if ('#<procedure #4 ##pair?> b)
                                       (or (occur a ('#<procedure #5 ##car> b))
                                           (occur a
                                                  ('#<procedure #6 ##cdr> b)))
                                       #f))))))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                   (occur a b)))
                                            (cn #f mv)
                                            (let ((begin-temp.1
                                                   ('#<procedure #8 ##set-cdr!>
                                                    a
                                                    b)))
                                              (cn #t
                                                  ('#<procedure #7 ##cons>
                                                   a
                                                   mv))))
                                        (unify #:oc397 val b mv cn)))
                                  (if (and ('#<procedure #4 ##pair?> b)
                                           ('#<procedure #3 ##eq?>
                                            ('#<procedure #5 ##car> b)
                                            laguz#*-var-*))
                                      (let ((val ('#<procedure #6 ##cdr> b)))
                                        (if ('#<procedure #3 ##eq?> val #!void)
                                            (if (and #:oc397
                                                     (laguz#occur? b a))
                                                (cn #f mv)
                                                (let ((begin-temp.2
                                                       ('#<procedure #8 ##set-cdr!>
                                                        b
                                                        a)))
                                                  (cn #t
                                                      ('#<procedure #7 ##cons>
                                                       b
                                                       mv))))
                                            (unify #:oc397 a val mv cn)))
                                      (if (and ('#<procedure #4 ##pair?> a)
                                               ('#<procedure #4 ##pair?> b))
                                          (unify #:oc397
                                                 ('#<procedure #5 ##car> a)
                                                 ('#<procedure #5 ##car> b)
                                                 mv
                                                 (lambda (r mv1)
                                                   (if r
                                                       (unify #:oc397
                                                              ('#<procedure #6 ##cdr>
                                                               a)
                                                              ('#<procedure #6 ##cdr>
                                                               b)
                                                              mv1
                                                              cn)
                                                       (cn r mv1))))
                                          (cn #f mv))))))))
          (unify #:oc397
                 #:f370
                 A
                 #:mv396
                 (lambda (r mv1)
                   (if r
                       (let ((#:ignore461 #t)
                             (#:mv469 mv1)
                             (#:oc470 #:oc397)
                             (#:zz471 #:zz398)
                             (#:bt472 #:bt399))
                         (let ((B ('#<procedure #7 ##cons>
                                   laguz#*-var-*
                                   #!void)))
                           (laguz#unify
                            #:f371
                            B
                            #:mv469
                            #:oc470
                            #:zz471
                            (lambda (#:ignore489
                                     #:mv497
                                     #:oc498
                                     #:zz499
                                     #:bt500)
                              (let ((X ('#<procedure #7 ##cons>
                                        laguz#*-var-*
                                        #!void)))
                                (laguz#father-child
                                 A
                                 X
                                 #:mv497
                                 #:oc498
                                 #:zz499
                                 (lambda (#:ignore517
                                          #:mv525
                                          #:oc526
                                          #:zz527
                                          #:bt528)
                                   (laguz#ancestor
                                    X
                                    B
                                    #:mv525
                                    #:oc526
                                    #:zz527
                                    #:ct394
                                    #:bt528))
                                 #:bt500)))
                            #:bt472)))
                       (#:bt399 mv1)))))))))

(define laguz#to-list
  (lambda (r)
    (if ('#<procedure #9 ##null?> r)
        '()
        ('#<procedure #7 ##cons>
         ('#<procedure #5 ##car> r)
         (let ((r (('#<procedure #6 ##cdr> r))))
           (if ('#<procedure #9 ##null?> r)
               '()
               ('#<procedure #7 ##cons>
                ('#<procedure #5 ##car> r)
                (laguz#to-list (('#<procedure #6 ##cdr> r))))))))))

(define laguz#times
  (lambda (n fn)
    (if ('#<procedure #10 ##fx>=> n 0)
        (let ((begin-temp.7 (fn)))
          (let ((n ('#<procedure #11 ##fx-> n 1)) (fn fn))
            (if ('#<procedure #10 ##fx>=> n 0)
                (let ((begin-temp.7 (fn)))
                  (laguz#times ('#<procedure #11 ##fx-> n 1) fn))
                #!void)))
        #!void)))

(pp '(laguz (:- (list-append () U U))
            (:- (list-append (A . X) Y (A . Z)) (list-append X Y Z))
            (:- (father-child 'a 'b))
            (:- (father-child 'b 'c))
            (:- (father-child 'c 'd))
            (:- (ancestor A B) (father-child A B))
            (:- (ancestor A B) (ancestor X B) (father-child A X)))
    (current-output-port))

('#<procedure #12 newline>)

(pp '(?- (ancestor X 'd)) (current-output-port))

(let ((temp.21 (let ((r (let ((#:bt541 (lambda (mv) '()))
                              (#:ct540 (lambda (#:vv529
                                                #:mv530
                                                #:ot531
                                                #:zz532
                                                #:bt534)
                                         ('#<procedure #7 ##cons>
                                          #:vv529
                                          (lambda () (#:bt534 #:mv530))))))
                          (let ((X ('#<procedure #7 ##cons>
                                    laguz#*-var-*
                                    #!void))
                                (#:mv542 '())
                                (#:oc543 #f)
                                (#:zz544 #f)
                                (#:bt545 #:bt541))
                            (laguz#ancestor
                             X
                             'd
                             #:mv542
                             #:oc543
                             #:zz544
                             (lambda (#:ignore551
                                      #:mv559
                                      #:oc560
                                      #:zz561
                                      #:bt562)
                               (#:ct540 ('#<procedure #2 ##list>
                                         ('#<procedure #7 ##cons>
                                          'X
                                          (laguz#subst X)))
                                        #:mv559
                                        #:oc560
                                        #:zz561
                                        #:bt562))
                             #:bt545)))))
                 (if ('#<procedure #9 ##null?> r)
                     '()
                     ('#<procedure #7 ##cons>
                      ('#<procedure #5 ##car> r)
                      (let ((r (('#<procedure #6 ##cdr> r))))
                        (if ('#<procedure #9 ##null?> r)
                            '()
                            ('#<procedure #7 ##cons>
                             ('#<procedure #5 ##car> r)
                             (let ((r (('#<procedure #6 ##cdr> r))))
                               (if ('#<procedure #9 ##null?> r)
                                   '()
                                   ('#<procedure #7 ##cons>
                                    ('#<procedure #5 ##car> r)
                                    (let ((r (('#<procedure #6 ##cdr> r))))
                                      (if ('#<procedure #9 ##null?> r)
                                          '()
                                          ('#<procedure #7 ##cons>
                                           ('#<procedure #5 ##car> r)
                                           (let ((r (('#<procedure #6 ##cdr>
                                                      r))))
                                             (if ('#<procedure #9 ##null?> r)
                                                 '()
                                                 ('#<procedure #7 ##cons>
                                                  ('#<procedure #5 ##car> r)
                                                  (let ((r (('#<procedure #6 ##cdr>
                                                             r))))
                                                    (if ('#<procedure #9 ##null?>
                                                         r)
                                                        '()
                                                        ('#<procedure #7 ##cons>
                                                         ('#<procedure #5 ##car>
                                                          r)
                                                         (let ((r (('#<procedure #6 ##cdr>
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                            r))))
                   (if ('#<procedure #9 ##null?> r)
                       '()
                       ('#<procedure #7 ##cons>
                        ('#<procedure #5 ##car> r)
                        (let ((r (('#<procedure #6 ##cdr> r))))
                          (if ('#<procedure #9 ##null?> r)
                              '()
                              ('#<procedure #7 ##cons>
                               ('#<procedure #5 ##car> r)
                               (let ((r (('#<procedure #6 ##cdr> r))))
                                 (if ('#<procedure #9 ##null?> r)
                                     '()
                                     ('#<procedure #7 ##cons>
                                      ('#<procedure #5 ##car> r)
                                      (let ((r (('#<procedure #6 ##cdr> r))))
                                        (if ('#<procedure #9 ##null?> r)
                                            '()
                                            ('#<procedure #7 ##cons>
                                             ('#<procedure #5 ##car> r)
                                             (let ((r (('#<procedure #6 ##cdr>
                                                        r))))
                                               (if ('#<procedure #9 ##null?> r)
                                                   '()
                                                   ('#<procedure #7 ##cons>
                                                    ('#<procedure #5 ##car> r)
                                                    (let ((r (('#<procedure #6 ##cdr>
                                                               r))))
                                                      (if ('#<procedure #9 ##null?>
                                                           r)
                                                          '()
                                                          ('#<procedure #7 ##cons>
                                                           ('#<procedure #5 ##car>
                                                            r)
                                                           (let ((r (('#<procedure #6 ##cdr>
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                              r))))
                     (if ('#<procedure #9 ##null?> r)
                         '()
                         ('#<procedure #7 ##cons>
                          ('#<procedure #5 ##car> r)
                          (let ((r (('#<procedure #6 ##cdr> r))))
                            (if ('#<procedure #9 ##null?> r)
                                '()
                                ('#<procedure #7 ##cons>
                                 ('#<procedure #5 ##car> r)
                                 (laguz#to-list
                                  (('#<procedure #6 ##cdr>
                                    r)))))))))))))))))))))))))))))))))))))))))))))))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  (letrec ((loop2.22
            (lambda (temp.23)
              (if ('#<procedure #4 ##pair?> temp.23)
                  (let ((x.24 (let ((as ('#<procedure #5 ##car> temp.23)))
                                (let ((begin-temp.8
                                       (letrec ((loop2.17
                                                 (lambda (temp.18)
                                                   (if ('#<procedure #4 ##pair?>
                                                        temp.18)
                                                       (let ((x.19 (let ((a ('#<procedure #5 ##car>
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                                     temp.18)))
                             (let ((temp.11 ('#<procedure #2 ##list>
                                             ('#<procedure #5 ##car> a)
                                             "="
                                             ('#<procedure #6 ##cdr> a)
                                             "\n")))
                               (letrec ((loop2.12
                                         (lambda (temp.13)
                                           (if ('#<procedure #4 ##pair?>
                                                temp.13)
                                               (let ((x.14 ('#<procedure #13 display>
                                                            ('#<procedure #5 ##car>
                                                             temp.13))))
                                                 (let ((temp.13 ('#<procedure #6 ##cdr>
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                         temp.13)))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                   (if ('#<procedure #4 ##pair?>
                                                        temp.13)
                                                       (let ((x.14 ('#<procedure #13 display>
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                            ('#<procedure #5 ##car> temp.13))))
                 (loop2.12 ('#<procedure #6 ##cdr> temp.13)))
               #!void)))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                               #!void))))
                                 (loop2.12 temp.11))))))
                 (let ((temp.18 ('#<procedure #6 ##cdr> temp.18)))
                   (if ('#<procedure #4 ##pair?> temp.18)
                       (let ((x.19 (let ((a ('#<procedure #5 ##car> temp.18)))
                                     (let ((temp.11 ('#<procedure #2 ##list>
                                                     ('#<procedure #5 ##car> a)
                                                     "="
                                                     ('#<procedure #6 ##cdr> a)
                                                     "\n")))
                                       (letrec ((loop2.12
                                                 (lambda (temp.13)
                                                   (if ('#<procedure #4 ##pair?>
                                                        temp.13)
                                                       (let ((x.14 ('#<procedure #13 display>
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                            ('#<procedure #5 ##car> temp.13))))
                 (let ((temp.13 ('#<procedure #6 ##cdr> temp.13)))
                   (if ('#<procedure #4 ##pair?> temp.13)
                       (let ((x.14 ('#<procedure #13 display>
                                    ('#<procedure #5 ##car> temp.13))))
                         (loop2.12 ('#<procedure #6 ##cdr> temp.13)))
                       #!void)))
               #!void))))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                         (loop2.12 temp.11))))))
                         (loop2.17 ('#<procedure #6 ##cdr> temp.18)))
                       #!void)))
               #!void))))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                         (loop2.17 as))))
                                  ('#<procedure #12 newline>)))))
                    (let ((temp.23 ('#<procedure #6 ##cdr> temp.23)))
                      (if ('#<procedure #4 ##pair?> temp.23)
                          (let ((x.24 (let ((as ('#<procedure #5 ##car>
                                                 temp.23)))
                                        (let ((begin-temp.8
                                               (letrec ((loop2.17
                                                         (lambda (temp.18)
                                                           (if ('#<procedure #4 ##pair?>
                                                                temp.18)
                                                               (let ((x.19 (let ((a ('#<procedure #5 ##car>
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                                             temp.18)))
                                     (let ((temp.11 ('#<procedure #2 ##list>
                                                     ('#<procedure #5 ##car> a)
                                                     "="
                                                     ('#<procedure #6 ##cdr> a)
                                                     "\n")))
                                       (letrec ((loop2.12
                                                 (lambda (temp.13)
                                                   (if ('#<procedure #4 ##pair?>
                                                        temp.13)
                                                       (let ((x.14 ('#<procedure #13 display>
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                            ('#<procedure #5 ##car> temp.13))))
                 (let ((temp.13 ('#<procedure #6 ##cdr> temp.13)))
                   (if ('#<procedure #4 ##pair?> temp.13)
                       (let ((x.14 ('#<procedure #13 display>
                                    ('#<procedure #5 ##car> temp.13))))
                         (loop2.12 ('#<procedure #6 ##cdr> temp.13)))
                       #!void)))
               #!void))))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                         (loop2.12 temp.11))))))
                         (let ((temp.18 ('#<procedure #6 ##cdr> temp.18)))
                           (if ('#<procedure #4 ##pair?> temp.18)
                               (let ((x.19 (let ((a ('#<procedure #5 ##car>
                                                     temp.18)))
                                             (let ((temp.11 ('#<procedure #2 ##list>
                                                             ('#<procedure #5 ##car>
                                                              a)
                                                             "="
                                                             ('#<procedure #6 ##cdr>
                                                              a)
                                                             "\n")))
                                               (letrec ((loop2.12
                                                         (lambda (temp.13)
                                                           (if ('#<procedure #4 ##pair?>
                                                                temp.13)
                                                               (let ((x.14 ('#<procedure #13 display>
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                                    ('#<procedure #5 ##car> temp.13))))
                         (let ((temp.13 ('#<procedure #6 ##cdr> temp.13)))
                           (if ('#<procedure #4 ##pair?> temp.13)
                               (let ((x.14 ('#<procedure #13 display>
                                            ('#<procedure #5 ##car> temp.13))))
                                 (loop2.12 ('#<procedure #6 ##cdr> temp.13)))
                               #!void)))
                       #!void))))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                 (loop2.12 temp.11))))))
                                 (loop2.17 ('#<procedure #6 ##cdr> temp.18)))
                               #!void)))
                       #!void))))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                 (loop2.17 as))))
                                          ('#<procedure #12 newline>)))))
                            (loop2.22 ('#<procedure #6 ##cdr> temp.23)))
                          #!void)))
                  #!void))))
    (loop2.22 temp.21)))

('#<procedure #12 newline>)

(pp '(?- (list-append X Y '(1 2 3 4 5 6))) (current-output-port))

(let ((temp.36 (let ((r (let ((#:bt580 (lambda (mv) '()))
                              (#:ct579 (lambda (#:vv568
                                                #:mv569
                                                #:ot570
                                                #:zz571
                                                #:bt573)
                                         ('#<procedure #7 ##cons>
                                          #:vv568
                                          (lambda () (#:bt573 #:mv569))))))
                          (let ((X ('#<procedure #7 ##cons>
                                    laguz#*-var-*
                                    #!void))
                                (#:mv581 '())
                                (#:oc582 #f)
                                (#:zz583 #f)
                                (#:bt584 #:bt580))
                            (let ((Y ('#<procedure #7 ##cons>
                                      laguz#*-var-*
                                      #!void)))
                              (laguz#list-append
                               X
                               Y
                               '(1 2 3 4 5 6)
                               #:mv581
                               #:oc582
                               #:zz583
                               (lambda (#:ignore606
                                        #:mv614
                                        #:oc615
                                        #:zz616
                                        #:bt617)
                                 (#:ct579 ('#<procedure #2 ##list>
                                           ('#<procedure #7 ##cons>
                                            'X
                                            (laguz#subst X))
                                           ('#<procedure #7 ##cons>
                                            'Y
                                            (laguz#subst Y)))
                                          #:mv614
                                          #:oc615
                                          #:zz616
                                          #:bt617))
                               #:bt584))))))
                 (if ('#<procedure #9 ##null?> r)
                     '()
                     ('#<procedure #7 ##cons>
                      ('#<procedure #5 ##car> r)
                      (let ((r (('#<procedure #6 ##cdr> r))))
                        (if ('#<procedure #9 ##null?> r)
                            '()
                            ('#<procedure #7 ##cons>
                             ('#<procedure #5 ##car> r)
                             (let ((r (('#<procedure #6 ##cdr> r))))
                               (if ('#<procedure #9 ##null?> r)
                                   '()
                                   ('#<procedure #7 ##cons>
                                    ('#<procedure #5 ##car> r)
                                    (let ((r (('#<procedure #6 ##cdr> r))))
                                      (if ('#<procedure #9 ##null?> r)
                                          '()
                                          ('#<procedure #7 ##cons>
                                           ('#<procedure #5 ##car> r)
                                           (let ((r (('#<procedure #6 ##cdr>
                                                      r))))
                                             (if ('#<procedure #9 ##null?> r)
                                                 '()
                                                 ('#<procedure #7 ##cons>
                                                  ('#<procedure #5 ##car> r)
                                                  (let ((r (('#<procedure #6 ##cdr>
                                                             r))))
                                                    (if ('#<procedure #9 ##null?>
                                                         r)
                                                        '()
                                                        ('#<procedure #7 ##cons>
                                                         ('#<procedure #5 ##car>
                                                          r)
                                                         (let ((r (('#<procedure #6 ##cdr>
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                            r))))
                   (if ('#<procedure #9 ##null?> r)
                       '()
                       ('#<procedure #7 ##cons>
                        ('#<procedure #5 ##car> r)
                        (let ((r (('#<procedure #6 ##cdr> r))))
                          (if ('#<procedure #9 ##null?> r)
                              '()
                              ('#<procedure #7 ##cons>
                               ('#<procedure #5 ##car> r)
                               (let ((r (('#<procedure #6 ##cdr> r))))
                                 (if ('#<procedure #9 ##null?> r)
                                     '()
                                     ('#<procedure #7 ##cons>
                                      ('#<procedure #5 ##car> r)
                                      (let ((r (('#<procedure #6 ##cdr> r))))
                                        (if ('#<procedure #9 ##null?> r)
                                            '()
                                            ('#<procedure #7 ##cons>
                                             ('#<procedure #5 ##car> r)
                                             (let ((r (('#<procedure #6 ##cdr>
                                                        r))))
                                               (if ('#<procedure #9 ##null?> r)
                                                   '()
                                                   ('#<procedure #7 ##cons>
                                                    ('#<procedure #5 ##car> r)
                                                    (let ((r (('#<procedure #6 ##cdr>
                                                               r))))
                                                      (if ('#<procedure #9 ##null?>
                                                           r)
                                                          '()
                                                          ('#<procedure #7 ##cons>
                                                           ('#<procedure #5 ##car>
                                                            r)
                                                           (let ((r (('#<procedure #6 ##cdr>
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                              r))))
                     (if ('#<procedure #9 ##null?> r)
                         '()
                         ('#<procedure #7 ##cons>
                          ('#<procedure #5 ##car> r)
                          (let ((r (('#<procedure #6 ##cdr> r))))
                            (if ('#<procedure #9 ##null?> r)
                                '()
                                ('#<procedure #7 ##cons>
                                 ('#<procedure #5 ##car> r)
                                 (let ((r (('#<procedure #6 ##cdr> r))))
                                   (if ('#<procedure #9 ##null?> r)
                                       '()
                                       ('#<procedure #7 ##cons>
                                        ('#<procedure #5 ##car> r)
                                        (let ((r (('#<procedure #6 ##cdr> r))))
                                          (if ('#<procedure #9 ##null?> r)
                                              '()
                                              ('#<procedure #7 ##cons>
                                               ('#<procedure #5 ##car> r)
                                               (let ((r (('#<procedure #6 ##cdr>
                                                          r))))
                                                 (if ('#<procedure #9 ##null?>
                                                      r)
                                                     '()
                                                     ('#<procedure #7 ##cons>
                                                      ('#<procedure #5 ##car>
                                                       r)
                                                      (let ((r (('#<procedure #6 ##cdr>
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                         r))))
                (if ('#<procedure #9 ##null?> r)
                    '()
                    ('#<procedure #7 ##cons>
                     ('#<procedure #5 ##car> r)
                     (let ((r (('#<procedure #6 ##cdr> r))))
                       (if ('#<procedure #9 ##null?> r)
                           '()
                           ('#<procedure #7 ##cons>
                            ('#<procedure #5 ##car> r)
                            (let ((r (('#<procedure #6 ##cdr> r))))
                              (if ('#<procedure #9 ##null?> r)
                                  '()
                                  ('#<procedure #7 ##cons>
                                   ('#<procedure #5 ##car> r)
                                   (let ((r (('#<procedure #6 ##cdr> r))))
                                     (if ('#<procedure #9 ##null?> r)
                                         '()
                                         ('#<procedure #7 ##cons>
                                          ('#<procedure #5 ##car> r)
                                          (let ((r (('#<procedure #6 ##cdr>
                                                     r))))
                                            (if ('#<procedure #9 ##null?> r)
                                                '()
                                                ('#<procedure #7 ##cons>
                                                 ('#<procedure #5 ##car> r)
                                                 (laguz#to-list
                                                  (('#<procedure #6 ##cdr>
                                                    r)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  (letrec ((loop2.37
            (lambda (temp.38)
              (if ('#<procedure #4 ##pair?> temp.38)
                  (let ((x.39 (let ((as ('#<procedure #5 ##car> temp.38)))
                                (let ((begin-temp.9
                                       (letrec ((loop2.32
                                                 (lambda (temp.33)
                                                   (if ('#<procedure #4 ##pair?>
                                                        temp.33)
                                                       (let ((x.34 (let ((a ('#<procedure #5 ##car>
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                                     temp.33)))
                             (let ((temp.26 ('#<procedure #2 ##list>
                                             ('#<procedure #5 ##car> a)
                                             "="
                                             ('#<procedure #6 ##cdr> a)
                                             "\n")))
                               (letrec ((loop2.27
                                         (lambda (temp.28)
                                           (if ('#<procedure #4 ##pair?>
                                                temp.28)
                                               (let ((x.29 ('#<procedure #13 display>
                                                            ('#<procedure #5 ##car>
                                                             temp.28))))
                                                 (let ((temp.28 ('#<procedure #6 ##cdr>
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                         temp.28)))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                   (if ('#<procedure #4 ##pair?>
                                                        temp.28)
                                                       (let ((x.29 ('#<procedure #13 display>
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                            ('#<procedure #5 ##car> temp.28))))
                 (loop2.27 ('#<procedure #6 ##cdr> temp.28)))
               #!void)))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                               #!void))))
                                 (loop2.27 temp.26))))))
                 (let ((temp.33 ('#<procedure #6 ##cdr> temp.33)))
                   (if ('#<procedure #4 ##pair?> temp.33)
                       (let ((x.34 (let ((a ('#<procedure #5 ##car> temp.33)))
                                     (let ((temp.26 ('#<procedure #2 ##list>
                                                     ('#<procedure #5 ##car> a)
                                                     "="
                                                     ('#<procedure #6 ##cdr> a)
                                                     "\n")))
                                       (letrec ((loop2.27
                                                 (lambda (temp.28)
                                                   (if ('#<procedure #4 ##pair?>
                                                        temp.28)
                                                       (let ((x.29 ('#<procedure #13 display>
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                            ('#<procedure #5 ##car> temp.28))))
                 (let ((temp.28 ('#<procedure #6 ##cdr> temp.28)))
                   (if ('#<procedure #4 ##pair?> temp.28)
                       (let ((x.29 ('#<procedure #13 display>
                                    ('#<procedure #5 ##car> temp.28))))
                         (loop2.27 ('#<procedure #6 ##cdr> temp.28)))
                       #!void)))
               #!void))))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                         (loop2.27 temp.26))))))
                         (loop2.32 ('#<procedure #6 ##cdr> temp.33)))
                       #!void)))
               #!void))))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                         (loop2.32 as))))
                                  ('#<procedure #12 newline>)))))
                    (let ((temp.38 ('#<procedure #6 ##cdr> temp.38)))
                      (if ('#<procedure #4 ##pair?> temp.38)
                          (let ((x.39 (let ((as ('#<procedure #5 ##car>
                                                 temp.38)))
                                        (let ((begin-temp.9
                                               (letrec ((loop2.32
                                                         (lambda (temp.33)
                                                           (if ('#<procedure #4 ##pair?>
                                                                temp.33)
                                                               (let ((x.34 (let ((a ('#<procedure #5 ##car>
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                                             temp.33)))
                                     (let ((temp.26 ('#<procedure #2 ##list>
                                                     ('#<procedure #5 ##car> a)
                                                     "="
                                                     ('#<procedure #6 ##cdr> a)
                                                     "\n")))
                                       (letrec ((loop2.27
                                                 (lambda (temp.28)
                                                   (if ('#<procedure #4 ##pair?>
                                                        temp.28)
                                                       (let ((x.29 ('#<procedure #13 display>
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                            ('#<procedure #5 ##car> temp.28))))
                 (let ((temp.28 ('#<procedure #6 ##cdr> temp.28)))
                   (if ('#<procedure #4 ##pair?> temp.28)
                       (let ((x.29 ('#<procedure #13 display>
                                    ('#<procedure #5 ##car> temp.28))))
                         (loop2.27 ('#<procedure #6 ##cdr> temp.28)))
                       #!void)))
               #!void))))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                         (loop2.27 temp.26))))))
                         (let ((temp.33 ('#<procedure #6 ##cdr> temp.33)))
                           (if ('#<procedure #4 ##pair?> temp.33)
                               (let ((x.34 (let ((a ('#<procedure #5 ##car>
                                                     temp.33)))
                                             (let ((temp.26 ('#<procedure #2 ##list>
                                                             ('#<procedure #5 ##car>
                                                              a)
                                                             "="
                                                             ('#<procedure #6 ##cdr>
                                                              a)
                                                             "\n")))
                                               (letrec ((loop2.27
                                                         (lambda (temp.28)
                                                           (if ('#<procedure #4 ##pair?>
                                                                temp.28)
                                                               (let ((x.29 ('#<procedure #13 display>
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                                    ('#<procedure #5 ##car> temp.28))))
                         (let ((temp.28 ('#<procedure #6 ##cdr> temp.28)))
                           (if ('#<procedure #4 ##pair?> temp.28)
                               (let ((x.29 ('#<procedure #13 display>
                                            ('#<procedure #5 ##car> temp.28))))
                                 (loop2.27 ('#<procedure #6 ##cdr> temp.28)))
                               #!void)))
                       #!void))))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                 (loop2.27 temp.26))))))
                                 (loop2.32 ('#<procedure #6 ##cdr> temp.33)))
                               #!void)))
                       #!void))))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                 (loop2.32 as))))
                                          ('#<procedure #12 newline>)))))
                            (loop2.37 ('#<procedure #6 ##cdr> temp.38)))
                          #!void)))
                  #!void))))
    (loop2.37 temp.36)))

(define laguz#a
  '(0
    1
    2
    3
    4
    5
    6
    7
    8
    9
    0
    a
    b
    c
    d
    e
    f
    g
    h
    i
    l
    m
    n
    o
    p
    q
    r
    s
    t
    u
    v
    z
    0
    1
    2
    3
    4
    5
    6
    7
    8
    9
    0
    a
    b
    c
    d
    e
    f
    g
    h
    i
    l
    m
    n
    o
    p
    q
    r
    s
    t
    u
    v
    z
    0
    1
    2
    3
    4
    5
    6
    7
    8
    9
    0
    a
    b
    c
    d
    e
    f
    g
    h
    i
    l
    m
    n
    o
    p
    q
    r
    s
    t
    u
    v
    z
    0
    1
    2
    3
    4
    5
    6
    7
    8
    9
    0
    a
    b
    c
    d
    e
    f
    g
    h
    i
    l
    m
    n
    o
    p
    q
    r
    s
    t
    u
    v
    z
    0
    1
    2
    3
    4
    5
    6
    7
    8
    9
    0
    a
    b
    c
    d
    e
    f
    g
    h
    i
    l
    m
    n
    o
    p
    q
    r
    s
    t
    u
    v
    z
    0
    1
    2
    3
    4
    5
    6
    7
    8
    9
    0
    a
    b
    c
    d
    e
    f
    g
    h
    i
    l
    m
    n
    o
    p
    q
    r
    s
    t
    u
    v
    z
    0
    1
    2
    3
    4
    5
    6
    7
    8
    9
    0
    a
    b
    c
    d
    e
    f
    g
    h
    i
    l
    m
    n
    o
    p
    q
    r
    s
    t
    u
    v
    z
    0
    1
    2
    3
    4
    5
    6
    7
    8
    9
    0
    a
    b
    c
    d
    e
    f
    g
    h
    i
    l
    m
    n
    o
    p
    q
    r
    s
    t
    u
    v
    z
    0
    1
    2
    3
    4
    5
    6
    7
    8
    9
    0
    a
    b
    c
    d
    e
    f
    g
    h
    i
    l
    m
    n
    o
    p
    q
    r
    s
    t
    u
    v
    z
    0
    1
    2
    3
    4
    5
    6
    7
    8
    9
    0
    a
    b
    c
    d
    e
    f
    g
    h
    i
    l
    m
    n
    o
    p
    q
    r
    s
    t
    u
    v
    z))

(define laguz#b '(this is b))

(##time (lambda ()
          (let ((n 100000)
                (fn (lambda ()
                      (let ((#:bt635 (lambda (mv) '()))
                            (#:ct634 (lambda (#:vv623
                                              #:mv624
                                              #:ot625
                                              #:zz626
                                              #:bt628)
                                       ('#<procedure #7 ##cons>
                                        #:vv623
                                        (lambda () (#:bt628 #:mv624))))))
                        (let ((X ('#<procedure #7 ##cons>
                                  laguz#*-var-*
                                  #!void))
                              (#:mv636 '())
                              (#:oc637 #f)
                              (#:zz638 #f)
                              (#:bt639 #:bt635))
                          (laguz#list-append
                           '(0
                             1
                             2
                             3
                             4
                             5
                             6
                             7
                             8
                             9
                             0
                             a
                             b
                             c
                             d
                             e
                             f
                             g
                             h
                             i
                             l
                             m
                             n
                             o
                             p
                             q
                             r
                             s
                             t
                             u
                             v
                             z
                             0
                             1
                             2
                             3
                             4
                             5
                             6
                             7
                             8
                             9
                             0
                             a
                             b
                             c
                             d
                             e
                             f
                             g
                             h
                             i
                             l
                             m
                             n
                             o
                             p
                             q
                             r
                             s
                             t
                             u
                             v
                             z
                             0
                             1
                             2
                             3
                             4
                             5
                             6
                             7
                             8
                             9
                             0
                             a
                             b
                             c
                             d
                             e
                             f
                             g
                             h
                             i
                             l
                             m
                             n
                             o
                             p
                             q
                             r
                             s
                             t
                             u
                             v
                             z
                             0
                             1
                             2
                             3
                             4
                             5
                             6
                             7
                             8
                             9
                             0
                             a
                             b
                             c
                             d
                             e
                             f
                             g
                             h
                             i
                             l
                             m
                             n
                             o
                             p
                             q
                             r
                             s
                             t
                             u
                             v
                             z
                             0
                             1
                             2
                             3
                             4
                             5
                             6
                             7
                             8
                             9
                             0
                             a
                             b
                             c
                             d
                             e
                             f
                             g
                             h
                             i
                             l
                             m
                             n
                             o
                             p
                             q
                             r
                             s
                             t
                             u
                             v
                             z
                             0
                             1
                             2
                             3
                             4
                             5
                             6
                             7
                             8
                             9
                             0
                             a
                             b
                             c
                             d
                             e
                             f
                             g
                             h
                             i
                             l
                             m
                             n
                             o
                             p
                             q
                             r
                             s
                             t
                             u
                             v
                             z
                             0
                             1
                             2
                             3
                             4
                             5
                             6
                             7
                             8
                             9
                             0
                             a
                             b
                             c
                             d
                             e
                             f
                             g
                             h
                             i
                             l
                             m
                             n
                             o
                             p
                             q
                             r
                             s
                             t
                             u
                             v
                             z
                             0
                             1
                             2
                             3
                             4
                             5
                             6
                             7
                             8
                             9
                             0
                             a
                             b
                             c
                             d
                             e
                             f
                             g
                             h
                             i
                             l
                             m
                             n
                             o
                             p
                             q
                             r
                             s
                             t
                             u
                             v
                             z
                             0
                             1
                             2
                             3
                             4
                             5
                             6
                             7
                             8
                             9
                             0
                             a
                             b
                             c
                             d
                             e
                             f
                             g
                             h
                             i
                             l
                             m
                             n
                             o
                             p
                             q
                             r
                             s
                             t
                             u
                             v
                             z
                             0
                             1
                             2
                             3
                             4
                             5
                             6
                             7
                             8
                             9
                             0
                             a
                             b
                             c
                             d
                             e
                             f
                             g
                             h
                             i
                             l
                             m
                             n
                             o
                             p
                             q
                             r
                             s
                             t
                             u
                             v
                             z)
                           '(this is b)
                           X
                           #:mv636
                           #:oc637
                           #:zz638
                           (lambda (#:ignore645
                                    #:mv653
                                    #:oc654
                                    #:zz655
                                    #:bt656)
                             (#:ct634 ('#<procedure #2 ##list>
                                       ('#<procedure #7 ##cons>
                                        'X
                                        (laguz#subst X)))
                                      #:mv653
                                      #:oc654
                                      #:zz655
                                      #:bt656))
                           #:bt639))))))
            (if ('#<procedure #10 ##fx>=> n 0)
                (let ((begin-temp.7 (fn)))
                  (let ((n ('#<procedure #11 ##fx-> n 1)))
                    (if ('#<procedure #10 ##fx>=> n 0)
                        (let ((begin-temp.7 (fn)))
                          (let ((n ('#<procedure #11 ##fx-> n 1)) (fn fn))
                            (if ('#<procedure #10 ##fx>=> n 0)
                                (let ((begin-temp.7 (fn)))
                                  (let ((n ('#<procedure #11 ##fx-> n 1)))
                                    (if ('#<procedure #10 ##fx>=> n 0)
                                        (let ((begin-temp.7 (fn)))
                                          (let ((n ('#<procedure #11 ##fx->
                                                    n
                                                    1))
                                                (fn fn))
                                            (if ('#<procedure #10 ##fx>=> n 0)
                                                (let ((begin-temp.7 (fn)))
                                                  (let ((n ('#<procedure #11 ##fx->
                                                            n
                                                            1)))
                                                    (if ('#<procedure #10 ##fx>=>
                                                         n
                                                         0)
                                                        (let ((begin-temp.7
                                                               (fn)))
                                                          (let ((n ('#<procedure #11 ##fx->
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                            n
                            1))
                        (fn fn))
                    (if ('#<procedure #10 ##fx>=> n 0)
                        (let ((begin-temp.7 (fn)))
                          (let ((n ('#<procedure #11 ##fx-> n 1)))
                            (if ('#<procedure #10 ##fx>=> n 0)
                                (let ((begin-temp.7 (fn)))
                                  (let ((n ('#<procedure #11 ##fx-> n 1))
                                        (fn fn))
                                    (if ('#<procedure #10 ##fx>=> n 0)
                                        (let ((begin-temp.7 (fn)))
                                          (let ((n ('#<procedure #11 ##fx->
                                                    n
                                                    1)))
                                            (if ('#<procedure #10 ##fx>=> n 0)
                                                (let ((begin-temp.7 (fn)))
                                                  (let ((n ('#<procedure #11 ##fx->
                                                            n
                                                            1))
                                                        (fn fn))
                                                    (if ('#<procedure #10 ##fx>=>
                                                         n
                                                         0)
                                                        (let ((begin-temp.7
                                                               (fn)))
                                                          (let ((n ('#<procedure #11 ##fx->
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                            n
                            1)))
                    (if ('#<procedure #10 ##fx>=> n 0)
                        (let ((begin-temp.7 (fn)))
                          (let ((n ('#<procedure #11 ##fx-> n 1)) (fn fn))
                            (if ('#<procedure #10 ##fx>=> n 0)
                                (let ((begin-temp.7 (fn)))
                                  (let ((n ('#<procedure #11 ##fx-> n 1)))
                                    (if ('#<procedure #10 ##fx>=> n 0)
                                        (let ((begin-temp.7 (fn)))
                                          (laguz#times
                                           ('#<procedure #11 ##fx-> n 1)
                                           fn))
                                        #!void)))
                                #!void)))
                        #!void)))
                #!void)))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                #!void)))
                                        #!void)))
                                #!void)))
                        #!void)))
                #!void)))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                #!void)))
                                        #!void)))
                                #!void)))
                        #!void)))
                #!void)))
        '(times 100000 (lambda () (?- (list-append a b X)))))

(define laguz#x #!void)

(##time (lambda ()
          (laguz#times
           100000
           (lambda ()
             (set! laguz#x
                   ('#<procedure #14 append>
                    '(0
                      1
                      2
                      3
                      4
                      5
                      6
                      7
                      8
                      9
                      0
                      a
                      b
                      c
                      d
                      e
                      f
                      g
                      h
                      i
                      l
                      m
                      n
                      o
                      p
                      q
                      r
                      s
                      t
                      u
                      v
                      z
                      0
                      1
                      2
                      3
                      4
                      5
                      6
                      7
                      8
                      9
                      0
                      a
                      b
                      c
                      d
                      e
                      f
                      g
                      h
                      i
                      l
                      m
                      n
                      o
                      p
                      q
                      r
                      s
                      t
                      u
                      v
                      z
                      0
                      1
                      2
                      3
                      4
                      5
                      6
                      7
                      8
                      9
                      0
                      a
                      b
                      c
                      d
                      e
                      f
                      g
                      h
                      i
                      l
                      m
                      n
                      o
                      p
                      q
                      r
                      s
                      t
                      u
                      v
                      z
                      0
                      1
                      2
                      3
                      4
                      5
                      6
                      7
                      8
                      9
                      0
                      a
                      b
                      c
                      d
                      e
                      f
                      g
                      h
                      i
                      l
                      m
                      n
                      o
                      p
                      q
                      r
                      s
                      t
                      u
                      v
                      z
                      0
                      1
                      2
                      3
                      4
                      5
                      6
                      7
                      8
                      9
                      0
                      a
                      b
                      c
                      d
                      e
                      f
                      g
                      h
                      i
                      l
                      m
                      n
                      o
                      p
                      q
                      r
                      s
                      t
                      u
                      v
                      z
                      0
                      1
                      2
                      3
                      4
                      5
                      6
                      7
                      8
                      9
                      0
                      a
                      b
                      c
                      d
                      e
                      f
                      g
                      h
                      i
                      l
                      m
                      n
                      o
                      p
                      q
                      r
                      s
                      t
                      u
                      v
                      z
                      0
                      1
                      2
                      3
                      4
                      5
                      6
                      7
                      8
                      9
                      0
                      a
                      b
                      c
                      d
                      e
                      f
                      g
                      h
                      i
                      l
                      m
                      n
                      o
                      p
                      q
                      r
                      s
                      t
                      u
                      v
                      z
                      0
                      1
                      2
                      3
                      4
                      5
                      6
                      7
                      8
                      9
                      0
                      a
                      b
                      c
                      d
                      e
                      f
                      g
                      h
                      i
                      l
                      m
                      n
                      o
                      p
                      q
                      r
                      s
                      t
                      u
                      v
                      z
                      0
                      1
                      2
                      3
                      4
                      5
                      6
                      7
                      8
                      9
                      0
                      a
                      b
                      c
                      d
                      e
                      f
                      g
                      h
                      i
                      l
                      m
                      n
                      o
                      p
                      q
                      r
                      s
                      t
                      u
                      v
                      z
                      0
                      1
                      2
                      3
                      4
                      5
                      6
                      7
                      8
                      9
                      0
                      a
                      b
                      c
                      d
                      e
                      f
                      g
                      h
                      i
                      l
                      m
                      n
                      o
                      p
                      q
                      r
                      s
                      t
                      u
                      v
                      z)
                    '(this is b))))))
        '(times 100000 (lambda () (set! x (append a b)))))

