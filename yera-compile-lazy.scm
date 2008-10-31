(##namespace ("yera-compile#"))

(##include "~~/lib/gambit#.scm")
(include "yera-mangle#.scm")

(define-macro (test s)
  (let(
       (vv (gensym 'vv)))
  `(lambda (,vv) (and (pair? ,vv) (eq? (car ,vv) (quote ,s))))))

(define-macro (emit p . es)
  (let(
       (_p (gensym '_p)))
    `(let(
          (,_p ,(car p)))
       ,@(map (lambda (e) `(display ,e ,_p)) es))))
  
(define let? (test let))
(define lambda? (test lambda))
(define object? (test obj))
(define array? (test arr))
(define call? pair?)
(define struct? (test struct))
(define interface? (test interface))
(define box? (test box))
(define unbox? (test unbox))

;; (define (expr-compile e p)
;;   (cond
;;    ;; ((subscribe? e) (subscribe-compile e p))
;;    ((let? e) (let-compile e p))
;;    ((lambda? e) (lambda-compile e p))
;;    ((struct? e) (struct-compile e p))
;;    ((interface? e) (interface-compile e p))
;;    ((object? e) (object-compile e p))
;;    ((call? e) (call-compile e p))
;;    ((symbol? e) (symbol-compile e p))
;;    ((string? e) (value-compile e p))
;;    ((number? e) (value-compile e p))
;;    (else (error "Unknown element in the ast" e))))
  
;; (define (let-compile e p)
;;   (emit (p) "(function(){")
;;   (bindings-compile (cadr e) p)
;;   (emit (p) "return ")
;;   (expr-compile (caddr e) p)
;;   (emit (p) "})()"))

;; (define (bindings-compile e p)
;;   (for-each
;;    (lambda (b)
;;      (cond
;;       ((eq? (car b) 'open) (open-compile b p))
;;       (else (assignment-compile b p))))
;;    e))

;; (define (assignment-compile e p)
;;   (emit (p) "var " (mangle (car e)) "=")
;;   (expr-compile (cadr e) p)
;;   (emit (p) ";"))

;; (define (lambda-compile e p)
;;   (emit (p) "box(function(){return function(" (mangle (cadr e)) "){" "return ")
;;   (expr-compile (caddr e) p)
;;   (emit (p) "()}})"))

;; (define (array-compile e p)
;;   (emit (p) "box(function(){return [")
;;   (if (null? (cdr e)) '()
;;       (let ()
;;         (expr-compile (cadr e) p)
;;         (for-each (lambda (j)
;;                     (emit (p) ",")
;;                     (expr-compile j p))
;;                   (cddr e))))
;;   (emit (p) "]})"))

;; (define (object-compile e p)
;;   (emit (p) "box(function(){return {")
;;   (if (null? (cdr e)) '()
;;       (let ()
;;         (emit (p) (caadr e) ":")
;;         (expr-compile (cadadr e) p)
;;         (for-each (lambda (j)
;;                     (emit (p) "," (car j) ":")
;;                     (expr-compile (cadr j) p))
;;                   (cddr  e))))
;;   (emit (p) "}})"))
      
;; (define (symbol-compile e p)
;;   (emit (p) (mangle e)))
  
;; (define (value-compile e p)
;;   (emit (p) "box(function(){return ")
;;   (write e p)
;;   (emit (p) "})"))

;; (define (call-compile e p)
;;   (emit (p) "box(function(){return (")
;;   (expr-compile (car e) p)
;;   (emit (p) "())(")
;;   (expr-compile (cadr e) p)
;;   (emit (p) ")})"))
      
;; (define (open-compile e p)
;;   (emit (p) "var $op=")
;;   (expr-compile (cadr e) p) (emit (p) "();")
;;   (emit (p) "for(var $k in $op)eval('var '+$k+'=$op[\"'+$k+'\"]');"))

;; (define (interface-compile e p)
;;   (emit (p) "box(function(){return[")
;;   (if (null? (cdr e)) '()
;;       (let ()
;;         (emit (p) "\"" (mangle (cadr e)) "\"")
;;         (for-each (lambda (j)
;;                     (emit (p) ",")
;;                     (emit (p) "\"" (mangle j) "\""))
;;                   (cddr e))))
;;   (emit (p) "]})"))

;; ; `(struct ,is ,bs)
;; (define (struct-compile e p)
;;   (emit (p) "box(function(){")
;;   (bindings-compile (caddr e) p)
;;   (emit-struct-footer e p)
;;   (emit (p) "})"))

;; (define (emit-struct-footer e p)
;;   (emit (p) "var $st={};" "var $in=") (expr-compile (cadr e) p) (emit (p) "();")
;;   (emit (p) "for(var $j=0;$j<$in.length;$j++)$st[$in[$j]]=eval($in[$j]);")
;;   (emit (p) "return $st;"))


;; new
(define (expr-lazy->eager e)
  (cond
   ((array? e) (array-lazy->eager e))
   ((object? e) (object-lazy->eager e))
   ((let? e) (let-lazy->eager e))
   ((lambda? e) (lambda-lazy->eager e))
   ((struct? e) (struct-lazy->eager e))
   ((interface? e) (interface-lazy->eager e))
   ((call? e) (call-lazy->eager e))
   ((symbol? e) (symbol-lazy->eager e))
   ((string? e) (value-lazy->eager e))
   ((number? e) (value-lazy->eager e))
   (else (error "Unknown element in the ast" e))))

;; questi due qui fanno gli oggetti composti tutti unboxed.
;; (define (array-lazy->eager e)
;;   `(box (arr ,@(map (lambda (j) `(unbox ,(expr-lazy->eager j))) (cdr e)))))

;; (define (object-lazy->eager e)
;;   `(box (obj ,@(map (lambda (p) `(,(car p) (unbox ,(expr-lazy->eager (cadr p))))) (cdr e)))))

(define (array-lazy->eager e)
  `(box (arr ,@(map expr-lazy->eager (cdr e)))))

(define (object-lazy->eager e)
  `(box (obj ,@(map (lambda (p) `(,(car p) ,(expr-lazy->eager (cadr p)))) (cdr e)))))

(define (let-lazy->eager e)
  `(box (let ,(bindings-lazy->eager (cadr e))
     (unbox ,(expr-lazy->eager (caddr e))))))


(define (bindings-lazy->eager e)
  (map (lambda (p)
         (cond
          ((eq? (car p) 'open) (open-lazy->eager p))
          (else (assignment-lazy->eager p))))
       e))

(define (assignment-lazy->eager e)
  `(,(car e) ,(expr-lazy->eager (cadr e))))

(define (lambda-lazy->eager e)
  `(box (lambda ,(cadr e) (unbox ,(expr-lazy->eager (caddr e))))))

(define (symbol-lazy->eager e) e)

(define (value-lazy->eager e)
  `(box ,e))

(define (call-lazy->eager e)
  `(box ((unbox ,(expr-lazy->eager (car e)))
         ,(expr-lazy->eager (cadr e)))))

(define (open-lazy->eager e)
  `(open (unbox ,(cadr e))))

(define (interface-lazy->eager e)
  `(box ,e))

(define (struct-lazy->eager e)
  `(box (struct ,(expr-lazy->eager (cadr e))
                ,(bindings-lazy->eager (caddr e)))))


(define (eager-simplify-boxes e)
  (cond
   ((not (pair? e)) e)
   ((and (eq? (car e) 'unbox)
         (pair? (cadr e))
         (eq? (car (cadr e)) 'box))
    (eager-simplify-boxes (cadr (cadr e))))   
   (else
    (cons
     (eager-simplify-boxes (car e))
     (eager-simplify-boxes (cdr e))))))


(define (expr-compile e p)  
  (cond
   ((array? e) (array-compile e p))
   ((object? e) (object-compile e p))
   ((box? e) (box-compile e p))
   ((unbox? e) (unbox-compile e p))
   ((let? e) (let-compile e p))
   ((lambda? e) (lambda-compile e p))
   ((struct? e) (struct-compile e p))
   ((interface? e) (interface-compile e p))
   ((call? e) (call-compile e p))
   ((symbol? e) (symbol-compile e p))
   ((string? e) (value-compile e p))
   ((number? e) (value-compile e p))
   (else (error "Unknown element in the ast" e))))


(define (box-compile e p)
  (emit (p) "box(function(){return ")
  (expr-compile (cadr e) p)
  (emit (p) "})"))

(define (unbox-compile e p)
  (expr-compile (cadr e) p)
  (emit (p) "()"))

(define (let-compile e p)
  (emit (p) "function(){")
  (bindings-compile (cadr e) p)
  (emit (p) ";return ")
  (expr-compile (caddr e) p)
  (emit (p) "}()"))

(define (bindings-compile e p)
  (for-each
   (lambda (b)
     (cond
      ((eq? (car b) 'open) (open-compile b p))
      (else (assignment-compile b p))))
   e))


(define (assignment-compile e p)
  (emit (p) "var " (mangle (car e)) "=")
  (expr-compile (cadr e) p)
  (emit (p) ";"))

(define (lambda-compile e p)
  (emit (p) "function(" (mangle (cadr e)) "){" "return ")
  (expr-compile (caddr e) p)
  (emit (p) "}"))

(define (array-compile e p)
  (emit (p) "[")
  (if (null? (cdr e)) '()
      (let ()
        (expr-compile (cadr e) p)
        (for-each (lambda (j)
                    (emit (p) ",")
                    (expr-compile j p))
                  (cddr e))))
  (emit (p) "]"))

(define (object-compile e p)
  (emit (p) "{")
  (if (null? (cdr e)) '()
      (let ()
        (emit (p) (caadr e) ":")
        (expr-compile (cadadr e) p)
        (for-each (lambda (j)
                    (emit (p) "," (car j) ":")
                    (expr-compile (cadr j) p))
                  (cddr  e))))
  (emit (p) "}"))

(define (symbol-compile e p)
  (emit (p) (mangle e)))

(define (value-compile e p) (write e p))

(define (call-compile e p)
  (expr-compile (car e) p)
  (emit (p) "(")
  (expr-compile (cadr e) p)
  (emit (p) ")"))

(define (open-compile e p)
  (emit (p) "var $$op=")
  (expr-compile (cadr e) p)
  (emit (p) ";")
  (emit (p) "for(var $$k in $$op)eval('var '+$$k+'=$$op[\"'+$$k+'\"]');"))

(define (interface-compile e p)
  (emit (p) "[")
  (if (null? (cdr e)) '()
      (let ()
        (emit (p) "\"" (mangle (cadr e)) "\"")
        (for-each (lambda (j)
                    (emit (p) ",")
                    (emit (p) "\"" (mangle j) "\""))
                  (cddr e))))
  (emit (p) "]"))

(define (struct-compile e p)
  (emit (p) "function(){")
  (bindings-compile (caddr e) p)
  (emit (p) "var $$st={};" "var $$in=") (expr-compile (cadr e) p) (emit (p) "();")
  (emit (p)
        "for(var $$j=0;$$j<$$in.length;$$j++)$$st[$$in[$$j]]=eval($$in[$$j]);"
        "return $$st;"
        "}()"))
