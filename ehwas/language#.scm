;; @author francesco bracchi <frbracch@gmail.com>

;; a configuration language for futhark servers

;; (define-server <symbol> <defs> ...) -> (define <symbol> (server <defs> ...))

;; server-def ::= (define-server <symbol> <defs> ...)
;;              | (define <symbol> <server>)

;; handler-def ::= (define-handler name <prog> ...)

;; server ::= (server <defs> ...)
;; defs ::= (host <symbol>)
;;        | (port <number>)
;;        | (resolve <resolver> ...)

;; resolver ::= (tables <table> ...)
;;            | (allow <test> <resolver> ...)
;;            | (deny <test> <resolver> ...)
;;            | (orelse <resolver> ...)
;;            | (with-prefix <resolver> ...)
;;            | (with-cache <resolver> ...)
;;            | (gebo)
;;            | (yera <directory>)
;;            | (static <directory>)
;;            | (dynamic <directory>)
;;            | (files <directory>)
;;            | (paths <assoc> ...)
;;            | (handler <prog> ...)

;; <test>   ::= (all)
;;            | (none)
;;            | (extensions <ext> ...)
;;            | (or <test> ...)
;;            | (and <test> ...)
;;            | (not <test>)

;; <directory> ::= <symbol>
;; <exts> ::= <symbol>

;; <table-def> ::= (define-table <symbol> <assoc> ...)
;;                 (define <symbol> (table <assoc> ...))
;;               | (define <symbol> (make-table ...))
;;               | (define <symbol> (list->table ...))

;; <assoc> ::= (<key> <value>)
;; <key>   ::= (<symbol> ...)
;; <value> ::= <resolver>

;; <start> ::= (start! <symbol>)
;; <stop>  ::= (stop! <symbol>)

;; <command> ::= <start>
;;             | <stop>
;;             | <key>
            

;; TODO introduce new test expressions


(##namespace
 ("ehwas-language#"
  define-server
  server
  orelse
  with-prefix
  with-cache
  with-indexes
  gebo
  yera
  allow
  deny
  extensions
  all
  none
  static
  dynamic
  files
  define-table
  tables
  set-resolver!
  ;; utility macros
  test
  test->function
  table
  paths
  ))
  
    
(define-macro (define-server name . args)
  `(define ,name (server ,@args)))

(define-macro (server . args)
  (let*(
        (default '((port 80)
                   (host *)
                   (ssl? #f)
                   (resolve not-found-resolver)))
        (getv
         (lambda (n)
           (or (assq n args)
               (assq n default)
               (raise `(syntax-error ,n not a valid keyword))))))
    `(make-server
      ,(symbol->string (cadr (getv 'host)))
      ,(cadr (getv 'port))
      (make-guarded-resolver (orelse ,@(cdr (getv 'resolve)) not-found-resolver))
      ,(cadr (getv 'ssl?)))))

(define-macro (orelse r . rs)
  (if (null? rs) r
      `(orelse-resolver ,r ,@rs)))

(define-macro (with-prefix s . r)
  `(with-prefix-resolver ',(map symbol->string s) (orelse ,@r)))

(define-macro (with-cache . r)
  `(make-cached-resolver (orelse ,@r)))

(define-macro (with-indexes is . rs)
  (let(
       (r (gensym 'r)))
    `(let ((,r (orelse ,@rs)))
       (orelse
        ,@(map (lambda (i)
                 `(with-index-resolver ,(symbol->string i) ,r))
               is)
        ,r))))

(define-macro (gebo)
  'gebo-resolver)

(define-macro (allow t . r)
  `(allow-resolver (test ,t) (orelse ,@r)))

(define-macro (deny t . r)
  `(deny-resolver (test ,t) (orelse ,@r)))

(define-macro (test t)
  (cond
   ((not (pair? t)) t)
   ((memq (car t) '(not and or))
    `(test->function ,(car t) ,(cdt t)))
   (else t)))

(define-macro (test->function sop ts)
  (let(
       (r (gensym 'r)))
    `(lambda (,r)
       (,sop (map (lambda (t) `((test ,t) ,r)) ts)))))
  
(define-macro (extensions . es)
  (let(
       (last (gensym 'last))
       (p (gensym 'p))
       (r (gensym 'r)))
    `(lambda (,r)
       (member
        (path-extension
         (let ,last ((,p (request-path ,r)))
              (cond
               ((null? ,p) "")
               ((null? (cdr ,p))  (car ,p))
               (else (,last (cdr ,p))))))
        ',(map (lambda (e) (symbol->string e)) es)))))

(define-macro (all)
  (let(
       (_ (gensym '_)))
    `(lambda (,_) #t)))

(define-macro (none)
  (let(
       (_ (gensym '_)))
    `(lambda (,_) #f)))

(define-macro (static s #!optional (dir? #f))
  `(make-filesystem-resolver
    ,(symbol->string s)
    ,dir?))

(define-macro (dynamic s)
  `(make-serverpage-resolver
    ,(symbol->string s)))

(define-macro (yera s)
  `(make-yera-resolver
    ,(symbol->string s)))

(define-macro (files s)
  `(orelse
    (allow (extensions .ehwas)
           (dynamic ,s))
    (allow (extensions .yera .js)
           (yera ,s))
    (deny  (extensions .scm .o1 .ehwas .yera)
           (static ,s))))

(define-macro (table . as)
  `(list->table
    (list ,@(map (lambda (a)
                   `(cons ',(map symbol->string (car a))
                          ,(cadr a)))
                 as))
    init: #f))
  
(define-macro (define-table n . as)
  `(define ,n (table ,@as)))

(define-macro (tables . ts)
  `(orelse
    ,@(map (lambda (t) `(make-table-resolver ,t)) ts)))

(define-macro (paths . ts)
  `(tables (table ,@ts)))

;; (define-macro (set-resolver! t s v)
;;   `(table-set! t (map symbol->string s) v))
