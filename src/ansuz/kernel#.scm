(##namespace
 ("ansuz-kernel#"
  char
  digit
  upcase
  locase
  interval
  alpha
  whitespace
  eos
  any
  test-token
  fail
  token-source
  continuation
  failure
  make-parser-exception
  parser-exception?
  parser-exception-reason
  ))

;; (define-macro (char _c . x)
;;   (let(
;;        (ts (gensym 'ts))
;;        (sc (gensym 'sc))
;;        (fl (gensym 'fl))
;;        (c (gensym 'c))
;;        (c0 (gensym 'c0)))
;;     `(let(
;;           (,c0 ,_c))
;;       (with-args ,x
;;                  (reflect (,ts ,sc ,fl)
;;                           (let(
;;                                (,c (source-car ,ts)))
;;                             (if (and (char? ,c) (char=? (source-car ,ts) ,c0))
;;                                 (,sc ,c0 (source-cdr ,ts) ,fl)
;;                                 (,fl (make-parser-exception
;;                                       (string-append
;;                                        "not "
;;                                        (string ,c0)))))))))))
    
