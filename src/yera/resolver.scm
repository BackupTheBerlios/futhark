(##namespace ("yera-resolver#"))

(##include "~~/lib/gambit#.scm")
(include "../ehwas/request#.scm")
(include "../ehwas/response#.scm")
(include "../ehwas/query#.scm")
(include "../ehwas/rfc3986#.scm")

(include "parser#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (fixnum)
         (block))

;; (define-macro (runtime #!optional min)
;;  (let*(
;;        (r (gensym 'r))
;;        (a (call-with-input-file "~~/site-scheme/futhark/yera/js/compat_dom.min.js" (lambda (p) (read-line p #f))))
;;        (b (call-with-input-file "~~/site-scheme/futhark/yera/js/patch.min.js" (lambda (p) (read-line p #f))))
;;        (c (call-with-input-file "~~/site-scheme/futhark/yera/js/yera.min.js" (lambda (p) (read-line p #f))))
;;        (d (call-with-input-file "~~/site-scheme/futhark/yera/js/yera_core.min.js" (lambda (p) (read-line p #f))))
;;        (e (call-with-input-file "~~/site-scheme/futhark/yera/js/yera_dom.min.js" (lambda (p) (read-line p #f))))
;;        (f (call-with-input-file "~~/site-scheme/futhark/yera/js/yera_math.min.js" (lambda (p) (read-line p #f))))
;;        (g (call-with-input-file "~~/site-scheme/futhark/yera/js/yera_userevent.min.js" (lambda (p) (read-line p #f))))
;;        (rts (string-append a b c d e f g)))
   
;;    `(lambda (,r)
;;       (response (request-version ,r) 200 "OK"
;;                 (header
;;                  Content-Type: "application/javascript"
;;                  Content-Length: ,(string-length rts))
;;                 (display (##still-copy ,rts))))))

(define (fold-left f i l)
  (let fold ((i i) (l l))
    (if (null? l) i
        (fold (f i (car l)) (cdr l)))))

(define (yerac cwd in out)
  (yera->js cwd in out))

(define (yerabc cwd in out)
  (write (yera->bytecode cwd in) out))

(define (last l)
  (cond
   ((null? l) '())
   ((null? (cdr l)) (car l))
   (else (last (cdr l)))))

(define (yera d)
  (lambda (r)
    (let(
         (path (uri-path (request-uri r))))
      (cond
       ((null? path) #f)
       ((string-ci=? (path-extension (symbol->string (last path))) ".yera")
        (let(
             (full (fold-left
                    (lambda (d v) (string-append d "/" (symbol->string v)))
                    d path))
             (query (url-decode (uri-query (request-uri r)))))
          (let(
               (a (table-ref query "action" #f)))
            (cond
             ((not a)
              (make-javascript-response (request-version r) 200 "OK" full))
             ((string=? a "code")
             (let (
                   (t (table-ref query "type")))
               (cond
                ((not t) (raise "type not found"))
                ((string=? t "yera")
                 (make-source-response (request-version r) 200 "OK" full))
                ((string=? t "bytecode")
                 (make-bytecode-response (request-version r) 200 "OK" full))
                ((string=? t "ecmascript")
                 (make-javascript-response (request-version r) 200 "OK" full))
                (else (error (string-append "type " t " not available"))))))
             (else (error (string-append "action " a  " not available")))))))
       (else #f)))))

(define (make-source-response v c s full)
  (let(
       (buffer (make-u8vector buffer-size))
       (size (file-size full)))
    (response
     v c s
     (header
      Content-Type: "text/plain"
      Content-Length: size
      Pragma: "no-cache")
     (call-with-input-file full
       (lambda (in)
         (let loop ((j 0))
           (if (< j size)
               (let(
                    (delta (read-subu8vector buffer 0 buffer-size in)))
                 (write-subu8vector buffer 0 delta)
                 (loop (+ j delta))))))))))

(define (make-bytecode-response v c s full)
  (let(
       (str (call-with-output-u8vector
             (u8vector)
             (lambda (out)
               (call-with-input-file full
                 (lambda (in)
                   (yerabc (path-directory full) in out)))))))
    (response
     v c s
     (header
      Content-Type: "text/plain"
      Content-Length: (u8vector-length str)
      Pragma: "no-cache")
     (write-subu8vector str 0 (u8vector-length str)))))

  
(define (make-javascript-response v c s full)
  (let(
       (str (call-with-output-u8vector
             (u8vector)
             (lambda (out)
               (call-with-input-file full
                 (lambda (in)
                   (yerac (path-directory full) in out)))))))
    (response
     v c s
     (header
      Content-Type: "text/ecmascript"
      Content-Length: (u8vector-length str)
      Pragma: "no-cache")
     (write-subu8vector str 0 (u8vector-length str)))))
  

