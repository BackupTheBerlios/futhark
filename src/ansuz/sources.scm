(##namespace ("ansuz-sources#"))
(##include "~~/lib/gambit#.scm")
(include "sources.new#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (safe)
         (fixnum)
         (block))

(define-type char-source
  (head unprintable: read-only:)
  (tail unprintable: read-only:)
  (row unprintable: read-only:)
  (column unprintable: read-only:)
  (position unprintable: read-only:)
  (datum unprintable: read-only:))

(define-type string-datum string position)

(define (string->source str #!optional (p0 0))
  (let(
       (head 
        (lambda (src)
          (let( 
               (str (string-datum-string  src))
               (pos (string-datum-position src)))
            (or (and (< pos (string-length str)) (string-ref str pos))
                #!eof))))
       (tail
        (lambda (src)
          (make-string-datum (string-datum-string src)
                             (fx+ 1 (string-datum-position src)))))
       (row
        (lambda (src)
          (error "row not implemented for strings")))

       (column
        (lambda (src)
          (error "column not implemented for strings")))

       (position
        (lambda (src)
          (string-datum-position src)))
       (datum
        (make-string-datum str p0)))
    (make-char-source head tail row column position datum)))


(define-type port-datum port char position row column)

;; (define (port->source port)
;;   (let(
;;        (tail
;;         (lambda (datum)
;;           (let*(
;;                 (port (port-datum-port datum))
;;                 (char (read-char port))
;;                 (position (fx+ (port-datum-position datum) 1))
;;                 (newline? (and (char? char) (char=? char #\newline)))
;;                 (row (if newline? (fx+ (port-datum-row datum) 1) (port-datum-row datum)))
;;                 (column (if newline? 0 (fx+ 1 (port-datum-column datum)))))
;;             (display char)
;;             (make-port-datum port char position row column)))))
;;     (make-char-source port-datum-char
;;                       tail
;;                       port-datum-row
;;                       port-datum-column
;;                       port-datum-position
;;                       (make-port-datum port (read-char port) 0 1 0))))

(define (port->source port)
  (let*(
        (head (lambda (datum)
                (or (port-datum-char datum)
                    (let(
                         (ch (read-char (port-datum-port datum))))
                      (port-datum-char-set! datum ch)
                      ch))))
        (tail
         (lambda (datum)
           (let*(
                 (char (head datum))
                 (position (fx+ (port-datum-position datum) 1))
                 (newline? (and (char? char) (char=? char #\newline)))
                 (row (if newline? (fx+ (port-datum-row datum) 1) (port-datum-row datum)))
                 (column (if newline? 0 (fx+ 1 (port-datum-column datum)))))
             (make-port-datum port #f position row column)))))
    (make-char-source head
                      tail
                      port-datum-row
                      port-datum-column
                      port-datum-position
                      (make-port-datum port (read-char port) 0 1 0))))

(define (->source s)
  (cond
   ((string? s) (string->source s))
   ((input-port? s) (port->source s))
   ((vector? s) (string->source (list->string (vector->list s))))
   ((list? s) (string->source (list->string s)))
   ((char-source? s) s)
   (else
    (error "unknown type ->source" s))))
