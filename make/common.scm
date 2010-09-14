(include "commands.scm")

(define (sh . as)
  (shell-command (apply string-append (cons (car as)
                                            (map (lambda (a) (string-append " " a)) (cdr as))))))

(define (split s d)
  (reverse
   (let ((len (string-length s)))
     (let split ((i 0)
                 (j 0)
                 (rs '()))
       (cond
        ((= j len)
         (cons (substring s i j) rs))
        
        ((char=? (string-ref s j) d)
         (split (+ j 1) (+ j 1) (cons (substring s i j) rs)))
        
        (else
         (split i (+ j 1) rs)))))))

(define (assert-directory fn)
  (let while ((path "") (dir (split (path-expand fn) #\/)))
    (if (null? dir) #t
        (let(
             (path (string-append path "/" (car dir))))
          (if (not (file-exists? path)) (create-directory path))
          (while path (cdr dir))))))


