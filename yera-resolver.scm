(##namespace ("yera-resolver#"))

(##include "~~/lib/gambit#.scm")
(include "ehwas-request#.scm")
(include "ehwas-response#.scm")
(include "ehwas-query#.scm")

(include "rfc3986#.scm")
(include "yera-parser#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (fixnum)
         (block))

(include "yera-rts.scm")

(define (fold-left f i l)
  (let fold ((i i) (l l))
    (if (null? l) i
        (fold (f i (car l)) (cdr l)))))


(define (yerac pwd in out)
  (yera->js pwd in out))

(define (yerabc pwd in out)
  (write (yera->bytecode pwd in) out))

(define (last l)
  (cond
   ((null? l) '())
   ((null? (cdr l)) (car l))
   (else (last (cdr l)))))

(define (make-yera-resolver d)
  (lambda (r)
    (let(
         (path (uri-path (request-uri r))))
      (cond
       ((null? path) #f)
       ((string-ci=? (path-extension (last path)) ".yera")
        (let(
             (full (fold-left
                    (lambda (d v) (string-append d "/" v))
                    d path))
             (query (url-decode (uri-query (request-uri r)))))
          (let(
               (a (table-ref query "action" #f)))
            (cond
             ((not a)
              (make-javascript-response (request-version r) "OK" 200 full))
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
                (else (raise (string-append "type " t " not available"))))))
             (else (raise (string-append "action " a  " not available")))))))
       ((equal? path '("yera" "rts.js"))
        (make-rts-response r))
       (else #f)))))

(define buffer-size 4096)


(define (make-rts-response r)
  (make-response
   (request-version r) 200 "OK"
   (header
    ("Content-type" "text/plain")
    ("Content-length" *-rts-size-*))
   (lambda (p)
     (write-subu8vector *-rts-data-* 0 *-rts-size-* p))))

(define (make-source-response v c s full)
  (let(
       (buffer (make-u8vector buffer-size))
       (size (file-size full)))
    (make-response
     v c s
     (header ("Content-type" "text/plain")
             ("Content-length" size)
             ("Pragma" "no-cache"))
     (lambda (out)
       (call-with-input-file full
         (lambda (in)
           (let loop ((j 0))
             (if (< j size)
                 (let(
                      (delta (read-subu8vector buffer 0 buffer-size in)))
                   (write-subu8vector buffer 0 delta out)
                   (loop (+ j delta)))))))))))

(define (make-bytecode-response v c s full)
  (let(
       (str (call-with-output-u8vector
             (u8vector)
             (lambda (out)
               (call-with-input-file full
                 (lambda (in)
                   (yerabc (path-directory full) in out)))))))
    (make-response
     v c s
     (header
      ("Content-type" "text/plain")
      ("Content-length" (u8vector-length str))
      ("Pragma" "no-cache"))
     (lambda (p)
       (write-subu8vector str 0 (u8vector-length str) p)))))

  
(define (make-javascript-response v c s full)
  (let(
       (str (call-with-output-u8vector
             (u8vector)
             (lambda (out)
               (call-with-input-file full
                 (lambda (in)
                   (yerac (path-directory full) in out)))))))
    (make-response
     v c s
     (header
      ("Content-type" "text/ecmascript")
      ("Content-length" (u8vector-length str))
      ("Pragma" "no-cache"))
     (lambda (p)
       (write-subu8vector str 0 (u8vector-length str) p)))))
  

