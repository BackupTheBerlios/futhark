(##namespace ("ansuz-sources-port-with-position#"
              stream-car
              stream-cdr
              stream-pos
              stream-row
              stream-col
              stream))


(define-macro (stream-car p)
  `(force (vector-ref ,p 0)))

(define-macro (stream-cdr p)
  `(force (vector-ref ,p 1)))

(define-macro (stream-pos p)
  (vector-ref ,p 2))

(define-macro (stream-row p)
  `(vector-ref ,p 3))

(define-macro (stream-col p)
  `(vector-ref ,p 4))
  
(define-macro (stream p)
  (let(
       (str (gensym 'str))
       (pos (gensym 'pos))
       (row (gensym 'row))
       (col (gensym 'col))
       (ch (gensym 'ch))
       (c (gensym 'ch)))
    `(let ,str ((,pos 0) (,row 1) (,col 0))
       (let(
            (,ch (delay (read-char ,p))))
       (vector
        ,ch
        (delay
          (let(
               (,c (force ,ch)))
            ;;(display ,c (current-error-port))
            (if (or (eof-object? ,c) (eq? ,c #\newline))
                (,str (+ ,pos 1) (+ ,row 1) 0)
                (,str (+ ,pos 1) ,row (+ ,col 1)))))
        ,pos
        ,row
        ,col)))))
