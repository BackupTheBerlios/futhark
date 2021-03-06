(##namespace ("yera-parser#"))

(##include "~~/lib/gambit#.scm")

;; (include "../ansuz/on-ports#.scm")
(include "../ansuz/on-ports-with-position#.scm")
(include "../ansuz/expressions#.scm")

(include "compile#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         (not safe))

(define-structure bindings assignments operator-table)

(define integer-digit
  (let(
       (i0 (char->integer #\0)))
    (parser ()
            (>> (<- c (digit))
                (return (- (char->integer c) i0))))))

(define (list->integer ds)
  (let list->integer ((ds ds) (v 0))
    (if (null? ds) v
        (list->integer (cdr ds) (+ (* 10 v) (car ds))))))

(define (list->fractional ds)
  (/ (let list->fractional ((ds (reverse ds)) (v 0))
       (if (null? ds) v
           (list->fractional (cdr ds) (+ (/ v 10) (car ds)))))
     10))

(define *yera-keywords* (make-table init: #f))

(define-macro (set-kw . ks)
  `(begin ,@(map (lambda (k) `(table-set! *yera-keywords* ',k #t)) ks)))

(set-kw
 let
 in
 where
 true
 false
 null
 end
 struct
 interface
 open
 files
 export
 infix
 prefix
 postfix
 yera
 javascript
 ->
 :
 |\\|
 publish
 subscribe)
  
(define (yera-keyword? s)
  (table-ref *yera-keywords* s))

(define (symbol-append a b)
  (string->symbol
   (string-append
    (symbol->string a)
    (symbol->string b))))

(define (make-unary-parser y)
  (let(
       (t (symbol->string y)))
    (parser ()
            (>> ;; (whitespace)
;;                 (yera-space)  
             (word t)
             (whitespace)
             (yera-space)
             (return (lambda (x) `(,y ,x)))))))

(define (make-builtin-unary-parser y)
  (let(
       (t (symbol->string y)))
    (parser ()
            (>> ;;(yera-space)
                (word t)
                (whitespace)
                (return (lambda (x) `(,y ,x)))))))

(define (make-binary-parser y)
  (let(
       (t (symbol->string y)))
    (parser ()
            (>> ;; (whitespace) (yera-space)
             (word t)
             (whitespace)
             (yera-space)
             (return (lambda (x z) `(,y ,x ,z)))))))
    
(define (make-builtin-binary-parser y)
  (let(
       (t (symbol->string y)))
    (parser ()
            (>> ;; (yera-space)
             (word t)
             (yera-space)
             (return (lambda (x z) `(,y ,x ,z)))))))

(define-parser (nonempty-list p)
  (<- s (p))
  (yera-space)
  (<- ss (kleene
          (>> (char #\,)
              (yera-space)
              (<- s (p))
              (yera-space)
              (return s))))
  (return (cons s ss)))

(define-parser (list-of o p c)
  (word o)
  (yera-space)
  (<- i (<> (nonempty-list p)
            (return '())))
  (yera-space)
  (word c)
  (return i))

(define-parser (yera-integer)
  (<- d (integer-digit))
  (<- ds (kleene (integer-digit)))
  (return (list->integer (cons d ds))))

(define-parser (yera-fractional)
  (char #\.)
  (<- ds (kleene (integer-digit)))
  (return (list->fractional ds)))

(define-parser (sign)
  (<> (>> (char #\+) (return (lambda (n) n)))
      (>> (char #\-) (return (lambda (n) (- n))))
      (return (lambda (n) n))))

(define-parser (power-of-ten)
  (<> (char #\e) (char #\E))
  (<- s (sign))
  (<- i (yera-integer))
  (return (s i)))

(define-parser (yera-number)
  (<- s (sign))
  (<- ip (yera-integer))
  (<- fp (<> (yera-fractional) (return 0)))
  (<- ex (<> (power-of-ten) (return 0)))
  (return (s (exact->inexact (* (+ ip fp) (expt 10 ex))))))

(define-parser (yera-symbol)
  (<- c (<> (alpha) (char #\_)))
  (<- cs (kleene (<> (alpha) (digit) (char #\_))))
  (let(
       (sym
        (string->symbol
         (list->string
          (cons c cs)))))
    (if (yera-keyword? sym)
        (fail "symbol expected, keyword found")
        (return sym))))

(define-parser (yera-string)
  (char #\")
  (<- s (kleene (yera-string-char)))
  (char #\")
  (return (list->string s)))

(define-parser (yera-string-char)
  (<> (>> (char #\\) (any))
      (get-if (lambda (c)
                (not (or (char=? c #\")
                         (char=? c #\nul)))))))

(define-parser (yera-js)
  (>> (char #\')
      (<- s (kleene (yera-js-char)))
      (char #\')
      (return `(javascript ,(list->string s)))))

(define-parser (yera-js-char)
  (<> (>> (char #\\) (any))
      (get-if (lambda (c)
                (not (or (char=? c #\')
                         (char=? c #\nul)))))))

(define-parser (yera-true)
  (word "true")
  (return 'true))

(define-parser (yera-false)
  (word "false")
  (return 'false))

(define-parser (yera-null)
  (word "null")
  (return 'null))

(define-parser (yera-comment)
  (word "--")
  (consume-comment))

(define-parser (consume-comment)
  (<> (char #\newline)
      (eos)
      (>> (any) (consume-comment))))

(define-parser (yera-space)
  (<> (>> (<> (whitespace) (yera-comment)) (yera-space))
      (return #\space)))

(define-parser (yera-array pwd ot)
  (<- s (list-of "[" (parser () (yera-expression pwd ot)) "]"))
  (return (if (null? s)
              `(const (arr))
              `(lift-array (arr ,@s)))))

(define-parser (yera-object pwd ot)
  (<- ps (list-of "{" (parser () (yera-object-pair pwd ot)) "}"))
  (return (if (null? ps)
              `(const (obj))
              `(lift-object (obj ,@ps)))))

(define-parser (yera-array-flat pwd ot)
  (<- s (list-of "_[" (parser () (yera-expression pwd ot)) "]"))
  (return `(arr ,@s)))

(define-parser (yera-object-flat pwd ot)
  (<- ps (list-of "_{" (parser () (yera-object-pair pwd ot)) "}"))
  (return `(obj ,@ps)))

(define-parser (yera-object-key)
  (<> (yera-number)
      (yera-string)
      (yera-symbol)))

(define-parser (yera-object-pair pwd ot)
  (<- k (yera-object-key))
  (yera-space)
  (char #\:)
  (yera-space)
  (<- v (yera-expression pwd ot))
  (return (list k v)))
  
(define-parser (yera-function pwd ot)
  (char #\\)
  (yera-space)
  (<- a (yera-symbol))
  (<- as (kleene (>> (yera-space) (yera-symbol))))
  (yera-space)
  (word "->")
  (yera-space)
  (<- e (yera-expression pwd ot))
  (return
   `(lambda ,(cons a as) ,e)))

(define-parser (yera-let pwd ot)
  (word "let")
  (yera-space)
  (<- bs (yera-bindings pwd ot))
  (yera-space)
  (word "in")
  (yera-space)
  (<- e (yera-expression pwd (bindings-operator-table bs)))
  (return `(let ,(bindings-assignments bs) ,e)))

(define-parser (yera-where pwd ot)
  (word "where")
  (yera-space)
  (<- bs (yera-bindings pwd ot))
  (yera-space)
  (word "end")
  (return (bindings-assignments bs)))

(define-parser (yera-assign pwd ot)
  (<- a (yera-symbol))
  (<- as (kleene (>> (yera-space) (yera-symbol))))
  (yera-space)
  (char #\=)
  (yera-space)
  (<- e (yera-expression pwd ot))
  (yera-space)
  (<> (>> (<- bs (yera-where pwd ot))
          (if (null? as)
              (if (null? bs)
                  (return `(,a ,e))
                  (return `(,a (let ,bs ,e))))
              (if (null? bs)
                  (return `(,a (lambda ,as ,e)))
                  (return `(,a (lambda ,as (let ,bs ,e)))))))
      (if (null? as)
          (return `(,a ,e))
          (return `(,a (lambda ,as ,e))))))

(define-parser (yera-assignment pwd ot)
  (<- a (yera-assign pwd ot))
  (return
   (make-bindings
    (list a) ot)))

(define-parser (yera-prefix-declaration ot)
  (word "prefix")
  (yera-space)
  (<- p (yera-integer))
  (yera-space)
  (<- k (yera-symbol))
  (yera-space)
  (return
   (make-bindings
    '()
    (operator-table-add-prefix
     ot
     (list (make-unary-parser k) p 'none k)))))

(define-parser (yera-infix-declaration ot)
  (word "infix")
  (yera-space)
  (<- s (<> (>> (word "left") (return 'left))
            (>> (word "right") (return 'right))))
  (yera-space)
  (<- p (yera-integer))
  (yera-space)
  (<- k (yera-symbol))
  (yera-space)
  (return
   (make-bindings
    '()
    (operator-table-add-infix
     ot
     (list (make-binary-parser k) p s k)))))

(define-parser (yera-postfix-declaration ot)
  (word "postfix")
  (yera-space)
  (<- p (yera-integer))
  (yera-space)
  (<- k (yera-symbol))
  (yera-space)
  (return
   (make-bindings
    '()
    (operator-table-add-postfix
     ot
     (list (make-unary-parser k) p 'none k)))))
  
(define-parser (precedence-declaration ot)
  (<> (yera-prefix-declaration ot)
      (yera-infix-declaration ot)
      (yera-postfix-declaration ot)))

(define-parser (files-inclusion pwd ot)
  (word "files")
  (whitespace)
  (yera-space)
  (<- s (yera-string))
  (<- ss (kleene
          (>> (yera-space)
              (char #\,)
              (yera-space)
              (yera-string))))
  (return
   (let loop ((ss (cons s ss)) (bs (make-bindings '() ot)))
     (if (null? ss) bs
         (loop (cdr ss)
               (let(
                    (f1 (string-append pwd (car ss))))
                 (call-with-input-file f1
                   (lambda (p)
                     (let(
                          (b1 (run (yera-file (path-directory f1) (bindings-operator-table bs)) p)))
                       ;; (lambda (r)
                       ;;   (error
                       ;;    (string-append r " in " (car ss) " @("
                       ;;                   (number->string (input-port-line p)) ","
                       ;;                   (number->string (input-port-column p))  ")"))))))
                       (make-bindings
                        (append (bindings-assignments bs) (bindings-assignments b1))
                        (bindings-operator-table b1)))))))))))

(define-parser (struct-expression pwd ot)
  (yera-expression pwd ot))

(define-parser (structs-opening pwd ot)
  (>> (word "open")
      (whitespace)
      (yera-space)
      (<- s (struct-expression pwd ot))
      (<- ss (kleene (>> (yera-space)
                         (char #\,)
                         (yera-space)
                         (struct-expression pwd ot))))
      (return (make-bindings (map (lambda (s) `(open ,s)) (cons s ss)) ot))))

(define-parser (yera-line pwd ot)
  (<- x (<> (files-inclusion pwd ot)
            (structs-opening pwd ot)
            (precedence-declaration ot)
            (yera-assignment pwd ot)))
  (return x))

(define-parser (yera-bindings pwd ot)
  (<> (nonempty-bindings pwd ot)
      (return (make-bindings '() ot))))

(define-parser (nonempty-bindings pwd ot)
  (>> (<- l (yera-line pwd ot))
      (<> (>> (yera-space)
              (maybe (>> (char #\;) (yera-space)))
              (<- ls (yera-bindings pwd (bindings-operator-table l)))
              (return
               (make-bindings
                (append (bindings-assignments l)
                        (bindings-assignments ls))
                (bindings-operator-table ls))))
          (return l))))

(define-parser (yera-parenthesis pwd ot)
  (>> (char #\()
      (yera-space)
      (<- e (yera-expression pwd ot))
      (yera-space)
      (char #\))
      (return e)))

(define-parser (pchar)
  (<> (>> (whitespace) (yera-space))
      (>> (char #\\) (any))
      (get-if
       (lambda (t)
         (not (or (char=? t #\>)
                  (char=? t #\<)
                  (char=? t #\@)
                  (char=? t #\$)
                  (char=? t #\nul)))))))

(define-parser (xml-name)
  (>> (<- s (yera-symbol))
      (<> (>> (char #\:)
              (<- t (yera-symbol))
              (return (list s t)))
          (return (list '() s)))))

(define-parser (xml-attribute pwd ot)
  (>> (<- k (xml-name))
      (yera-space)
      (char #\=)
      (yera-space)
      (<- v (yera-term pwd ot))
      (return`(attr ,(car k)
                    ,(cadr k)
                    ,v))))

(define-parser (xml-attributes pwd ot)
  (<> (>> (<- a (xml-attribute pwd ot))
          (yera-space)
          (<- as (xml-attributes pwd ot))
          (return (cons a as)))
      (return '())))

(define (split-attributes/namespaces as)
  (if (null? as) (values '() '())
      (let(
           (a (car as)))
        (receive (xs ns) (split-attributes/namespaces (cdr as))
          (if (or (eq? 'xmlns (cadr a))
                    (eq? 'xmlns (caddr a)))
              (values xs (cons a ns))
              (values (cons a xs) ns))))))

(define (stringify-attributes as)
  (map (lambda (a)
         (let(
              (ns  (if (null? (cadr a)) `(const "") (symbol-append 'xmlns_ (cadr a)))))
           `(lift-object
             (obj (namespaceURI ,ns)
                  (name (const ,(symbol->string (caddr a))))
                  (value ,(cadddr a))))))
       as))

(define (namespace-bindings bs)
  (map (lambda (n)
         (if (eq? (cadr n) 'xmlns)
             (list (symbol-append 'xmlns_ (cadddr n)))
             (list 'xmlns (cadddr n))))
       bs))
  
(define-parser (xml-node pwd ot)
  (char #\<)
  (<- n (xml-name))
  (yera-space)
  (<- xs (xml-attributes pwd ot))
  (yera-space)
  (let*(
        (as '())
        (bs '())
        (_ (receive (as0 ns0) (split-attributes/namespaces xs)
             (set! as (stringify-attributes as0))
             (set! bs (namespace-bindings ns0))))
        (ns (if (null? (car n)) 'xmlns (symbol-append 'xmlns_ (car n)))))
    (<>
     (>> (word "/>") (return
                      (let(
                           (lo
                            `(lift-object
                              (obj (namespaceURI ,ns)
                                   (nodeName (const ,(symbol->string (cadr n))))
                                   (attributes (lift-array (arr ,@as)))
                                   (childNodes (const (arr)))))))
                        (if (null? bs) lo `(let ,bs ,lo)))))
     (>> (char #\>)
         (yera-space)
         (<- es (xml-childs pwd ot n))
         (return
          (let(
               (lo
                `(lift-object
                  (obj (namespaceURI ,ns)
                       (nodeName (const ,(symbol->string (cadr n))))
                       (attributes (lift-array (arr ,@as)))
                       (childNodes (lift-array (arr ,@es)))))))
            (if (null? bs) lo `(let ,bs ,lo))))))))

(define-parser (xml-close t)
  (word "</")
  (<- w (xml-name))
  (yera-space)
  (word ">")
  (if (and (eq? (car w) (car t))
           (eq? (cadr w) (cadr t)))
      (return '())
      (fail "xml-close")))

(define-parser (xml-text)
  (<- c (pchar))
  (<- cs (kleene (pchar)))
  (return (list->string (cons c cs))))

(define-parser (xml-unquote pwd ot)
  (<> (xml-unquote-node pwd ot)
      (xml-unquote-value pwd ot)))

(define-parser (xml-unquote-node pwd ot)
  (char #\@)
  (yera-term pwd ot))

(define-parser (xml-unquote-value pwd ot)
  (char #\$)
  (<- s (yera-term pwd ot))
  (return s))

(define-parser (xml-childs pwd ot t)
  (<> (>> (<- c (xml-unquote pwd ot))
          (yera-space)
          (<- cs (xml-childs pwd ot t))
          (return (cons c cs)))
      (>> (<- c (xml-node pwd ot))
          (yera-space)
          (<- cs (xml-childs pwd ot t))
          (return (cons c cs)))
      (>> (<- c (xml-text))
          (<- cs (xml-childs pwd ot t))
          (return (cons `(const ,c) cs)))
      (xml-close t)))

(define-parser (yera-quasiquote pwd ot)
  (char #\`)
  (yera-space)
  (xml-node pwd ot))

(define-parser (yera-quasiquote-flat pwd ot)
  (char #\_) (char #\`)
  (yera-space)
  (<- n (xml-node pwd ot))
  (return (unlift n)))

(define (unlift n)
  (cond
   ((not (pair? n)) n)
   ((eq? (car n) 'lift-object) (unlift (cadr n)))
   ((eq? (car n) 'lift-array) (unlift (cadr n)))
   ((eq? (car n) 'const) (unlift (cadr n)))
   (else
    (cons
     (unlift (car n))
     (unlift (cdr n))))))
   

(define-parser (yera-constant)
  (<- c (<> (yera-true)
            (yera-false)
            (yera-null)
            (yera-number)
            (yera-string)))
  (return `(const ,c)))

(define-parser (yera-constant-flat)
  (char #\_)
  (<- c (<> (yera-true)
            (yera-false)
            (yera-null)
            (yera-number)
            (yera-string)))
  (return c))

(define-parser (yera-term pwd ot)
  (<> (yera-parenthesis pwd ot)
      (yera-let pwd ot)
      (yera-function pwd ot)
      
      (yera-array-flat pwd ot)
      (yera-object-flat pwd ot)
      (yera-array pwd ot)
      (yera-object pwd ot)
      
      (yera-struct pwd ot)
      (yera-interface ot)
      
      (yera-quasiquote-flat pwd ot)
      (yera-quasiquote pwd ot)
      
      (yera-js)
      (yera-constant-flat)
      (yera-constant)
      (function-ref)
      (symbol-ref ot)
      ))

(define-parser (symbol-ref ot)
  (<- s (yera-symbol))
  (if (is-operator? ot s)
      (fail "symbol is an operator")
      (return s)))

(define-parser (function-ref)
  (char #\$)
  (yera-symbol))

(define (is-operator? ot s)
  (or (member s (map cadddr (operator-table-prefix ot)))
      (member s (map cadddr (operator-table-infix ot)))
      (member s (map cadddr (operator-table-postfix ot)))))

(define-parser (yera-call pwd ot)
  (<- t (yera-term+ pwd ot))
  (<- ts (kleene (>> (whitespace) (yera-space) (yera-term+ pwd ot))))
  (yera-space)
  (return
   (if (null? ts) t (cons t ts))))

(define-parser (yera-expression pwd ot)
  (expr ot (parser () (yera-call pwd ot))))

(define-parser (yera-term+ pwd ot)
  (<- t (yera-term pwd ot))
  (<> (>> (yera-space)
          (char #\.)
          (yera-space)
          (<- s (yera-symbol))
          (<- ss (kleene (>> (yera-space) (char #\.) (yera-symbol))))
          (return
           (let loop ((t `(ref ,t (const ,(symbol->string s)))) (ss ss))
             (if (null? ss) t
                     (loop `(ref ,t (const ,(symbol->string (car ss))))
                           (cdr ss))))))
      (return t)))
  
(define-parser (object-ref)
  (char #\.)
  (yera-space)
  (<- s (yera-symbol))
  (return (lambda (e) `(ref ,e (const ,(symbol->string s))))))

(define-parser (array-ref)
  (char #\!)
  (yera-space)
  (return (lambda (e s) `(ref ,e ,s))))

(define-parser (negate)
  (char #\-)
  (yera-space)
  (return (lambda (e) `(negate ,e))))
 
(define *-operator-table-*
  (let(
       (pre `((,(make-unary-parser 'not) 8 none not)
              (,(make-builtin-unary-parser '!) 8 none !)
              (,(make-unary-parser 'futr) 5 none futr)
              (,(make-unary-parser 'pres) 5 none pres)
              (,negate 8 none -)))
       (in  `((,(make-binary-parser 'until) 1 right until)
              (,(make-binary-parser 'untilI) 1 right untilI)
              (,(make-binary-parser 'or) 3 right or)
              (,(make-binary-parser 'and) 5 right and)
              (,(make-builtin-binary-parser '&&) 5 right &&)
              (,(make-builtin-binary-parser '|\|\||) 3 right |\|\||)
              (,(make-builtin-binary-parser '==) 7 left ==)
              (,(make-builtin-binary-parser '!=) 7 left !=)   
              (,(make-builtin-binary-parser '>=) 7 left >=)
              (,(make-builtin-binary-parser '<=) 7 left <=)
              (,(make-builtin-binary-parser '>) 7 left  >)
              (,(make-builtin-binary-parser '<) 7 left  <)
              (,(make-builtin-binary-parser '+) 7 left  +)
              (,(make-builtin-binary-parser '-) 7 left  -)
              (,(make-builtin-binary-parser '*) 9 left  *)
              (,(make-builtin-binary-parser '/) 9 left  /)
              (,(make-builtin-binary-parser '^) 9 left  ^)
              (,(make-builtin-binary-parser '%) 9 left  %)
              
              (,(make-binary-parser '_or_) 3 right _or_)
              (,(make-binary-parser '_and_) 5 right _and_)
              (,(make-builtin-binary-parser '_&&_) 5 right _&&_)
              (,(make-builtin-binary-parser '|_\|\|_|) 3 right |_\|\|_|)
              (,(make-builtin-binary-parser '_==_) 7 left _==_)
              (,(make-builtin-binary-parser '_>=_) 7 left _>=_)
              (,(make-builtin-binary-parser '_<=_) 7 left _<=_)
              (,(make-builtin-binary-parser '_>_) 7 left  _>_)
              (,(make-builtin-binary-parser '_<_) 7 left  _<_)
              (,(make-builtin-binary-parser '_+_) 7 left  _+_)
              (,(make-builtin-binary-parser '_-_) 7 left  _-_)
              (,(make-builtin-binary-parser '_*_) 9 left  _*_)
              (,(make-builtin-binary-parser '_/_) 9 left  _/_)
              (,(make-builtin-binary-parser '_^_) 9 left  _^_)
              (,(make-builtin-binary-parser '_%_) 9 left  _%_)
              
              ;; (,function-application 10 left fnap)
              (,array-ref 10 left !)))
       
       (post `((,object-ref 11 none |.|))))
    (make-operator-table pre in post)))

(define-parser (yera-interface-term)
  (<> (yera-interface-selfeval)
      (yera-symbol)))

(define-parser (yera-interface-selfeval)
  (<- is (list-of "(" yera-symbol ")"))
  (return `(interface ,@is)))

(define-parser (yera-interface-expression)
  (expr interface-expression-ot
        yera-interface-term))

(define-parser (yera-interface ot)
  (word "interface")
  (yera-space)
  (yera-interface-expression))
      
(define-parser (yera-struct pwd ot)
  (word "struct")
  (yera-space)
  (char #\:)
  (yera-space)
  (<- is (yera-interface-expression))
  (yera-space)
  (word "->")
  (yera-space)
  (<- bs (yera-bindings pwd ot))
  (word "end")
  (return `(struct ,is ,(bindings-assignments bs))))

(define interface-expression-ot
  (make-operator-table
   `()
   `((
      ,(parser ()
               (yera-space)
               (char #\U)
               (yera-space)
               (return
                (lambda (x y)
                  `(($union ,x) ,y))))
      1 left))
   `()))

(define-parser (yera-file pwd ot)
  (yera-space)
  (<- bs (yera-bindings pwd ot))
  (yera-space)
  (eos)
  (return bs))
 
(define (yera->sexp s)
  (call-with-input-string (lambda (p) (run (yera-expression *-operator-table-*) p))))

(define (yera->bytecode pwd in)
  (bindings-assignments (run (yera-file pwd *-operator-table-*) in)))

(define bytecode->js bindings-compile)

;; (define (call? e)
;;   (and (pair? e) (eq? (car e) 'call)))

;; (define (flatten-call e)
;;   (cond
;;    ((call? e) (really-flatten e))
;;    ((pair? e)
;;     (cons (flatten-call (car e))
;;           (flatten-call (cdr e))))
;;    (else
;;     e)))

;; (define (really-flatten e)
;;   (if (call? (caddr e))
;;       (cons (cadr e) (really-flatten (caddr e)))
;;       (list (cadr e) (caddr e))))
   
(define (yera->js pwd in #!optional (out (current-output-port)))
  (print port: out "with(Yera){")
  (bytecode->js
   (yera->bytecode pwd in)
   out)
  (print port: out "}"))
