(##namespace  ("ehwas-cookies#"))

(##include "~~/lib/gambit#.scm")

(include "request#.scm")
(include "response#.scm")

(include "../ansuz/sources/string#.scm")
(include "../ansuz/char-stream-parser#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         ;;(not safe)
         )

(define (make-token-parser valid?)
  (parser ()
          (reflect (st sc fl) ;;(ts sc fl)
                   (let loop ((st st) (buf (make-string 4096)) (wpos 0) (lim 4095))
                     (let(
                          (c (stream-car st)))
                       (if (valid? c)
                           (begin
                             (string-set! buf wpos c)
                             (loop (stream-cdr st)
                                   (if (= lim wpos) (string-append buf (make-string (+ 1 lim))) buf)
                                   (+ 1 wpos)
                                   (if (= lim wpos) (+ 1 (* 2 lim)) lim)))
                           (sc (substring buf 0 wpos) st fl)))))))

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
  (reflect (st sc fl)
           (let loop ((st st))
             (let(
                  (c (stream-car st)))
               (if (and (char? c) (char-whitespace? c))
                   (loop (stream-cdr st))
                   (sc 'spaces st fl))))))

(define-parser (cookie)
  (<- c (cookie-value))
  (<- cs (kleene (>> (spaces) (char #\;) (spaces) (cookie-value))))
  (return (list->table (cons c cs))))

(define-parser (cookie-value)
  (<- nam (key))
  (spaces) (char #\=) (spaces)
  (<- val (value))
  (return (cons nam val)))

(define (string->cookie s)
  (run (cookie) s))

(define (make-request-cookies request)
  (let(
       (str (assq 'Cookie (request-header request))))
    (and str (string->cookie (cdr str)))))

(define *-memo-* (make-table weak-keys: #t))

(define (request-cookies request)
  (or (table-ref *-memo-* request #f)
      (let(
           (cookies (make-request-cookies request)))
        (table-set! *-memo-* request cookies)
        cookies)))

(define (fold-left f i l)
  (let fold ((i i) (l l))
    (if (null? l) i
        (fold (f i (car l)) (cdr l)))))

;; (define (set-cookie! response k v . avs)
;;   ;; todo what if Set-cookie already present?
;;   (response-header-set!
;;    `((Set-cookie . ,(fold-left (lambda (p av)
;; 				 (cond
;; 				  ((pair? av)
;; 				   (string-append p ";" (car av) "=" (cdr av)))
;; 				  (else
;; 				   (string-append p av ";"))))
;; 			       (string-append k "=" v)
;; 			       avs))
;;      ,@(response-header response))))

(define (response-cookie-set response k v . avs)
  (make-response
   (response-code res)
   (response-status res)
   (set-cookie (response-header response) k v avs)
   (response-writer res)))

(define (set-cookie header k v avs)
  (let set ((header header) (rs '()))
    (cond
     ((null? header)
      (reverse `((Set-cookie . ,(cookie-val k v avs)) ,@rs)))
     ((eq? (caar header) 'Set-cookie)
      (append (reverse (cons `((Set-cookie . ,(string-append (cookie-val k v avs) "; " (cdar header))) rs)))
	      (cdr header)))
     (else
      (set (cdr header) (cons (car header) res))))))

(define (cookie-val k v avs)
  (fold-left (lambda (p av)
	       (cond
		((pair? av)
		 (string-append p ";" (car av) "=" (cdr av)))
		(else
		 (string-append p av ";"))))
	     (string-append k "=" v)
	     avs))

