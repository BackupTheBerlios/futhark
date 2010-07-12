(##namespace
 ("ansuz-language#"
  run
  run-ndet
  parser-eval
  parser
  define-parser))

(include "reflect#.scm")
(include "monad#.scm")
(include "orelse#.scm")
(include "sources#.scm")

 
;; (define-macro (run p s #!optional (fl0 'raise))
;;   (let(
;;        (v (gensym 'v))
;;        (ts (gensym 'ts))
;;        (fl (gensym 'fl)))
;;     `(run-monad ,p (->source ,s) (lambda (,v ,ts ,fl) ,v) ,fl0)))

;; (define-macro (run-ndet p s)
;;   (let(
;;        (v (gensym 'v))
;;        (ts (gensym 'ts))
;;        (fl (gensym 'fl))
;;        (r (gensym 'r)))
;;     `(run-monad ,p (->source ,s) (lambda (,v ,ts ,fl) (cons ,v (delay (,fl 'ndet)))) (lambda (,r) '()))))

;; (define (run p src #!optional (fail raise))
;;   (let*(
;;         (head (char-source-head src))
;;         (tail (char-source-tail src))
;;         (row (char-source-row src))
;;         (col (char-source-column src))
;;         (pos (char-source-position src))
;;         (datum (char-source-datum src)))
;;     (run-monad (p) head tail row col pos datum (lambda (v datum fl) v) fail)))

;; (define (run-ndet p src)
;;   (let*(
;;         (head (char-source-head src))
;;         (tail (char-source-tail src))
;;         (row (char-source-row src))
;;         (col (char-source-column src))
;;         (pos (char-source-position src))
;;         (datum (char-source-datum src)))
;;     (run-monad (p) head tail row col pos datum (lambda (v datum fl) (cons v (delay (fl 'nder)))) (lambda (r) '()))))

(define-macro (run p s #!optional (fail 'raise))
  (let(
       (src (gensym 'src))
       (head (gensym 'head))
       (tail (gensym 'tail))
       (row (gensym 'row))
       (col (gensym 'col))
       (pos (gensym 'pos))
       (datum (gensym 'datum))
       (v (gensym 'v))
       (fl (gensym 'fl)))
    `(let*(
           (,src (->source ,s))
           (,head (char-source-head ,src))
           (,tail (char-source-tail ,src))
           (,row (char-source-row ,src))
           (,col (char-source-column ,src))
           (,pos (char-source-position ,src))
           (,datum (char-source-datum ,src)))
       (run-monad ,p ,head ,tail ,row ,col ,pos ,datum (lambda (,v ,datum ,fl) ,v) ,fail))))

(define (run-ndet p s)
  (let(
       (src (gensym 'src))
       (head (gensym 'head))
       (tail (gensym 'tail))
       (row (gensym 'row))
       (col (gensym 'col))
       (pos (gensym 'pos))
       (datum (gensym 'datum))
       (v (gensym 'v))
       (fl (gensym 'fl))
       (r (gensym 'r)))
    `(let*(
           (,src ,s)
           (head (char-source-head ,src))
           (tail (char-source-tail ,src))
           (row (char-source-row ,src))
           (col (char-source-column ,src))
           (pos (char-source-position ,src))
           (datum (char-source-datum ,src)))
       (run-monad ,p ,head ,tail ,row ,col ,pos ,datum (lambda (,v ,datum ,fl) (cons ,v (delay (,fl 'ndet)))) (lambda (,r) '())))))

               
(define-macro (parser-eval e . x)
  (cond
   ((and (list? e) (eq? (car e) '>>) (= 2 (length e)))
    `(with-state ,x (parser-eval ,(cadr e))))
   
   ((and (list? e) (eq? (car e) '<>) (= 2 (length e)))
    `(with-state ,x (parser-eval ,(cadr e))))
   
   ((and (list? e) (eq? (car e) '<*>) (= 2 (length e)))
    `(with-state ,x (parser-eval ,(cadr e))))

   ((and (list? e) (eq? (car e) '>>))
    (let(
         (c (cadr e)))
      (if (and (list? c) (eq? (car c) '<-) (= (length c) 3))
          `(with-state ,x (bind (,(cadr c) (parser-eval ,(caddr c)))
                               (parser-eval (>> ,@(cddr e)))))
          (let(
               (__ (gensym '__)))
            `(with-state ,x (bind (,__ (parser-eval ,(cadr e)))
                                 (parser-eval (>> ,@(cddr e)))))))))

   ((and (list? e) (eq? (car e) '<>))
    `(with-state ,x (orelse (parser-eval ,(cadr e))
                           (parser-eval (<> ,@(cddr e))))))
   
   ((and (list? e) (eq? (car e) '<*>))
    `(with-state ,x (orelse* (parser-eval ,(cadr e))
                            (parser-eval (<*> ,@(cddr e))))))

   ((and (list? e) (eq? (car e) 'if) (= (length e) 3))
    `(if ,(cadr e)
         (with-state ,x (parser-eval ,(caddr e)))
         (with-state ,x (parser-eval (return 'if)))))
   
   ((and (list? e) (eq? (car e) 'if) (= (length e) 4))
    `(if ,(cadr e)
         (with-state ,x (parser-eval ,(caddr e)))
         (with-state ,x (parser-eval ,(cadddr e)))))

   ((and (list? e) (eq? (car e) 'cond))
    `(cond
      ,@(map (lambda (p) `(,(car p) (with-state ,x (parser-eval (cadr p)))))
             (cdr e))))
   
   ((and (list? e)
         (or (eq? (car e) 'let)
             (eq? (car e) 'let*)
             (eq? (car e) 'letrec)))
    `(,(car e) ,(cadr e) (with-state ,x (parser-eval ,(caddr e)))))

   (else
    `(with-state ,x ,e))))

(define-macro (parser f b)
  `(lambda+ ,f (parser-eval ,b)))

(define-macro (define-parser s b)
  `(define ,(car s) (parser ,(cdr s) ,b)))
