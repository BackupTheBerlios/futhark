(##namespace ("ansuz-sources-port#"
              stream-car
              stream-cdr
              stream-pos
              stream-row
              stream-col
              stream))


(define-macro (stream-car p)
  `(force (car ,p)))

(define-macro (stream-cdr p)
  `(force (cdr ,p)))

(define-macro (stream-pos st)
  `(error "position not supported by port parser"))

(define-macro (stream-row st)
  `(error "row not supported by port parser"))

(define-macro (stream-col st)
  `(error "column not supported by port parser"))

(define-macro (stream p)
  (let(
       (kdr (gensym 'cdr))
       (kar (gensym 'car)))
    `(let ,kdr ()
          (let(
               ;;(,kar (delay (let ((c (read-char ,p))) (display c (current-error-port)) c))))
               (,kar (delay (read-char ,p))))
            (cons
             ,kar
             (delay (begin (force ,kar) (,kdr))))))))

  