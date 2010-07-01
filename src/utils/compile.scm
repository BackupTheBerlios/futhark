(##namespace ("utils-compile#"))
(##include "~~/lib/gambit#.scm")

(declare
  (standard-bindings)
  (extended-bindings)
  (block)
  ;;(not safe)
  (fixnum)
  )
          
(define compiler-istructions-prefix (make-parameter ";;CI"))
(define object-extension (make-parameter ".o"))
(define gsc-command (make-parameter (path-expand "~~/bin/gsc ")))

(define (object-file f)
  (string-append
   (path-strip-extension f)
   (object-extension)))

(define (gsc-compile file options)
  (shell-command
   (with-output-to-string
     ""
     (lambda ()
       (display (gsc-command))
       (display " ")
       (display (path-expand file))
       (display " ")
       (display options)
       (display " -o ")
       (display (path-expand (object-file file)))))))

(define (header? f)
  (and (eq? (file-type f) 'regular)
       (>= (string-length f) 5)
       (string=? (substring f (- (string-length f) 5) (string-length f))
                 "#.scm")))

(define (scheme? f)
  (and (eq? (file-type f) 'regular)
       (>= (string-length f) 4)
       (string=? (substring f (- (string-length f) 4) (string-length f))
                 ".scm")))

(define (directory? f)
  (eq? (file-type f) 'directory))

(define (filter t? l)
  (let filter ((l l) (r '()))
    (cond
     ((null? l) (reverse r))
     ((t? (car l)) (filter (cdr l) (cons (car l) r)))
     (else (filter (cdr l) r)))))

(define (directories-in-directory d)
  (filter directory? (directory-files d)))

(define (scheme-files-in-directory d)
  (filter
   (lambda (f)
     (and (scheme? f) (not (header? f))))))

(define (header-files-in-directory d)
  (filter header? (directory-files f)))
  
(define (first-line f)
  (call-with-input-file f read-line))
  
(define (compile+ f)
  (let*(
        (of (object-file f))
        (cip (compiler-istruction-prefix))
        (cl (string-length cip))
        (fl (first-line f)))
    
    (if (file-exists?  of) (delete-file of))
    (if (string=? (substring fl 0 cl) cip)
        (let(
             (options (substring fl cl (string-length fl))))
          (gsc-compile f options)))))

(define (need-update? f)
  (let(
       (obj  (object-file f)))
  (or (not (file-exists? obj))
      (>= (time->seconds (file-last-change-time f))
          (time->seconds (file-creation-time obj))))))

(define (delete-obj-then-compile f)
  (let(
       (obj (object-file f)))
    (if (file-exists? obj) (delete-file obj))
    (compile+ f)))

(define (update-file f)
  (if (need-update? f) (delete-obj-then-compile f)))
  
(define (compile-directory d)
  (let compile ((ds (list d)))
    (if (null? ds) #t
        (let(
             (d (car ds)))
          (for-each update-file (scheme-files-in-dir d))
          (compile (append (directories-in-directory d) (cdr ds)))))))
