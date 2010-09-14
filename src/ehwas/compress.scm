(##namespace ("ehwas-compress#"))

(##include "~~/lib/gambit#.scm")

(include "request#.scm")
(include "response#.scm")
(include "combinators#.scm")
(include "../encode/zlib#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         ;; (not safe)
         )

(define (split s #!optional (sep #\,))
  (let split ((i 0) (j 0))
    (cond
     ((= j (string-length s))
      (if (= i j) '()
          (list (substring s i j))))
     
     ((char=? (string-ref s j) sep)
      (cons (substring s i j)
            (split (+ j 1) (+ j 1))))

     (else
      (split i (+ j 1))))))

(define (compress resolver)
  (lambda (req)
    (let(
         (res (resolver req)))
      (if (not res) #f
          (let*(
                (accepted (split (table-ref (request-header req) "Accept-Encoding" "")))
                (version (response-version res))
                (code (response-code res))
                (status (response-status res))
                (headers (response-header res)))
            (cond
             ((member "gzip" accepted)
              (table-set! headers "Content-encoding" "gzip")
              (response
               version code status headers
               (lambda ()
                 (let*(
                       (clear
                        (with-output-to-u8vector
                         (u8vector)
                         (response-writer res)))
                       (compressed
                        (gzip-u8vector clear)))
                   (write-subu8vector compressed 0 (u8vector-length compressed))))))
             
             ((member "deflate" accepted)
              (table-set! headers "Content-encoding" "deflate")
              (response
               version code status headers
               (lambda ()
                 (let*(
                       (clear
                        (with-output-to-u8vector
                         (u8vector)
                         (response-writer res)))
                       (compressed
                        (deflate-u8vector clear)))
                   (write-subu8vector compressed 0 (u8vector-length compressed))))))
             (else
              res)))))))

(define (can-compress req)
  (let*(
        (accepted (split (table-ref (request-header req) "Accept-Encoding" ""))))
    (or (member "gzip" accepted)
        (member "deflate" accepted))))

(define (cached-compress res)
  (orelse
   (allow can-compress (cache (compress res)))
   (cache res)))
