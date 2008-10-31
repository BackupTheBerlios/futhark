(define files 
  (list
   "actors.js"
   "actors_test.js"
   "actors_match.js"
   "actors_remote.js"))

(define sizes (map file-size files))

(define total (apply + sizes))

(define out "../gebo-rts.scm")
 
(define j 0)
 
(define out-vector (make-u8vector total))

(for-each (lambda (f s)
            (call-with-input-file f
              (lambda (p)
                (let(
                     (j1 (+ j s)))
                  (read-subu8vector out-vector j j1 p)
                (set! j j1)))))
          files sizes)

(call-with-output-file out
  (lambda (p)
    (pp `(define *-rts-data-* ',out-vector) p)
    (pp `(define *-rts-size-* ,total) p)))