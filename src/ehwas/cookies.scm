(##namespace  ("ehwas-cookies#"))

(##include "~~/lib/gambit#.scm")

(include "request#.scm")
(include "response#.scm")
(include "resolver#.scm")

(include "../ansuz/language#.scm")
(include "../ansuz/kernel#.scm")
(include "../ansuz/extras#.scm")
(include "../ansuz/sources#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         ;;(not safe)
         )

;; (define-parser (valid-key-char)
;;   (test-token
;;    (lambda (c)
;;      (not (or (char=? c #\=)
;;               (char=? c #\;)
;;               (char=? c #\nul))))))

;; (define-parser (more-key)
;;   (<> (>> (<- c (valid-key-char))
;;           (<- cs (more-key))
;;           (return (cons c cs)))
;;       (return '())))

;; (define-parser (key)
;;   (>> (<- cs (more-key))
;;       (return (list->string cs))))


;; (define-parser (valid-value-char)
;;   (test-token
;;    (lambda (c)
;;      (not (or (char=? c #\;)
;;               (char=? c #\nul))))))

;; (define-parser (more-value)
;;   (<> (>> (<- c (valid-value-char))
;;           (<- cs (more-value))
;;           (return (cons c cs)))
;;       (return '())))

;; (define-parser (value)
;;   (>> (<- cs (more-value))
;;       (return (list->string cs))))

;; (define-parser (spaces)
;;   (<> (>> (whitespace) (spaces))
;;       (return '())))

(define (make-token-parser valid?)
  (parser ()
          (reflect (ts sc fl)
                   (let loop ((ts ts) (st (make-string 4096)) (wpos 0) (lim 4095))
                     (let(
                          (c (source-car ts)))
                       (if (valid? c)
                           (begin
                             (string-set! st wpos c)
                             (loop (source-cdr ts)
                                   (if (= lim wpos) (string-append st (make-string (+ 1 lim))) st)
                                   (+ 1 wpos)
                                   (if (= lim wpos) (+ 1 (* 2 lim)) lim)))
                           (sc (substring st 0 wpos) ts fl)))))))

(define key
  (make-token-parser
   (lambda (c)
     (not (or (eof-object? c)
              (char=? c #\=)
              (char=? c #\;)
              (char=? c #\nul))))))

(define value
  (make-token-parser
   (lambda (c)
     (not (or (eof-object? c)
              (char=? c #\;)
              (char=? c #\nul))))))

(define-parser (spaces)
  (reflect (ts sc fl)
           (let loop ((ts ts))
             (let(
                  (c (source-car ts)))
               (if (and (char? c) (char-whitespace? c))
                   (loop (source-cdr ts))
                   (sc '() ts fl))))))
                   
(define-parser (cookie)
  (>> (<- c (cookie-value))
      (<- cs (kleene
              (parser () (>> (spaces) (char #\;) (spaces) (cookie-value)))))
      (return (list->table (cons c cs)))))

(define-parser (cookie-value)
  (>> (<- nam (key))
      (spaces) (char #\=) (spaces)
      (<- val (value))
      (return (cons nam val))))


(define (string->cookie s)
  (run (cookie) (->source s)))

(define (request-cookies request)
  (let(
       (str (table-ref (request-header request) "Cookie" #f)))
    (and str (string->cookie str))))

;; (define (fold-left f i l)
;;   (if (null? l) i
;;       (fold-left f (f i (car l)) (cdr l))))

(define (fold-left f i l)
  (let fold ((i i) (l l))
    (if (null? l) i
        (fold (f i (car l)) (cdr l)))))

(define (set-cookie! response k v . avs)
  (response-header-set!
   response
   "Set-Cookie"
   (fold-left (lambda (p av)
                (cond
                 ((pair? av)
                  (string-append p ";" (car av) "=" (cdr av)))
                 (else
                  (string-append p av ";"))))
              (string-append k "=" v)
              avs)))
