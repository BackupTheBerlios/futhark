(##namespace ("rfc822#"))
 
(##include "~~/lib/gambit#.scm")
(##include "../ansuz/sources/port#.scm")
(##include "../ansuz/char-stream-parser#.scm")
(##include "../ansuz/re#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         (not safe)
         )

(define-regexp field-line "[~\n]*")
(define-regexp field-continue "\n[ \t]?")

(define-parser (field-value-aux cs)
  (>> (<- c (field-line))
      (<- ct (field-continue))
      (if (> (string-length ct) 1)
          (field-value-aux (cons c cs))
          (return (apply string-append (reverse (cons c cs)))))))

(define-parser (field-value)
  (field-value-aux '()))
  
(define-regexp space "[\t ]*")

(define-regexp field-key "[~:]*")

;; (define-parser (field table)
;;   (>> (<- k (field-key))
;;       (char #\:)
;;       (space)
;;       (<- v (field-value))
;;       ;;(return (cons (string->keyword k) v))))
;;       (return (table-set! table (string->symbol k) v))))

;; (define-parser (rfc822-aux table)
;;   (<> (>> (char #\newline)
;;           (return table))
;;       (>> (field table)
;;           (rfc822-aux table))))

;; (define-parser (rfc822)
;;   (rfc822-aux (make-table)))


(define-parser (rfc822)
  (rfc822-aux '()))

(define-parser (rfc822-aux alst)
  (<> (>> (char #\newline)
	  (return (reverse alst)))
      (>> (<- kv (field))
	  (rfc822-aux (cons kv alst)))))

(define-parser (field)
  (>> (<- k (field-key))
      (get #\:)
      (space)
      (<- v (field-value))
      (return (cons (string->symbol k) v))))
