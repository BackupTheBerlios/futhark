(##namespace ("rfc822#"))
 
(##include "~~/lib/gambit#.scm")
(include "ansuz-language#.scm")
(include "ansuz-kernel#.scm")
(include "ansuz-extras#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         (not safe))

(define-parser (value-list)
  (<> (>> (char #\newline) (<> (>> (<> (char #\space)
                                       (char #\tab))
                                   (value-list))
                               (return '())))
      (>> (<- c (any))
          (<- cs (value-list))
          (return (cons c cs)))))

(define-parser (field-value)
            (>> (<- s (value-list))
                (return (list->string s))))
  
(define-parser (space)
            (<> (>> (<- q (<> (char #\space)
                              (char #\tab)))
                    (space))
                (return 'ok)))

(define-parser (key-list)
  (<> (>> (char #\:) (space) (return '()))
      (>> (<- c (any))
          (<- cs (key-list))
          (return (cons c cs)))))

(define-parser (field-key)
  (>> (<- s (key-list))
      (return (list->string s))))

(define-parser (field)
  (>> (<- k (field-key))
      (<- v (field-value))
      (return (cons k v))))

(define-parser (more-header)
  (>> (<- f (field))
      (<- fs (rfc822))
      (return (cons f fs))))

(define-parser (rfc822)
  (<> (>> (char #\newline)
          (return '()))
      (more-header)))

