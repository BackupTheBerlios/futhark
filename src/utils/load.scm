(##namespace ("utils-compile#"))
(##include "~~/lib/gambit#.scm")

(declare
  (standard-bindings)
  (extended-bindings)
  (block)
  ;;(not safe)
  (fixnum)
  )

(define *-loaded-* (make-table)) 

(define (load-recursively d r)
  (load-files-in-dir d)
  (for-each load-recursively
            (filter directory?
                    (map (lambda (f) (string-append d "/" f))
                         (directory-files d)))))

(define (load-files-in-dir d r)
  (for-each
   (lambda (f) (load-file f r))
   (filter
    (lambda (fn) (string=? (path-extension fn) (object-extension)))
    (directory-files d))))

(define (loaded? f)
  (table-ref *-loaded-* f #f))

(define (loaded! f)
  (table-set! *-loaded-* f #t))

(define (load-file f r)
  (if (or (not (loaded? f)) (need-update f))
      (begin
        (update-file f)
        (load (object-file f))
        (loaded! f))
      #t))

(define (load-directory d #!key (recursive #t) (recompile-if-needed #t))
  (if recursive
      (load-recursively d recompile-if-needed)
      (load-files-in-dir d recompile-if-needed)))