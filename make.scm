(define output "fgi")

(define files
  (list
   "ansuz-expressions"
   "ansuz-extras"
   "ansuz-kernel"
   "ansuz-streams"
   
   "rfc3986"
   "rfc822"
   "uids"
   "base64"
   
   "ehwas-cookies"
   "ehwas-errors"
   "ehwas-pages"
   "ehwas-query"
   "ehwas-request"
   "ehwas-resolver"
   "ehwas-response"
   "ehwas-server"
   "ehwas-sessions"
   "file-sessions"
   "pg-sessions"
   
   "gebo-json"
   "gebo-resolver"

   "postgresql"

   "openssl"
   "openssl-ports"

   "yera-compile"
   "yera-mangle"
   "yera-parser"
   "yera-resolver"))

(define-macro (defined? e)
  (let(
       (ex (gensym 'ex)))
  `(with-exception-catcher
    (lambda (,ex) #f)
    (lambda ()
      ,e #t))))


;; add termite support

(define (compile-all)
  (shell-command
   (apply string-append
          `("gsc -link -flat -o " ,output ".o1.c"
            ,@(map (lambda (f) (string-append " " f)) files)))))

(define (link)
  (shell-command
   (apply string-append
          `("gcc -shared -lssl -lcrypto -ldl -D___DYNAMIC -fPIC -I/usr/local/Gambit-C/current/include"
            ,@(map (lambda (f) (string-append " " f ".c")) files)
            " " ,output ".o1.c -o " ,output ".o1"))))

(define (clean)
  (shell-command
   (apply string-append
          `("rm "
            ,@(map (lambda (f) (string-append " " f ".c")) files)
            " " ,output ".o1.c"))))

(define (regendata-dir d)
  (current-directory d)
  (shell-command "gsi regen.scm")
  (current-directory ".."))


(define (make)
  (compile-all)
  (link)
  (clean))

;(regendata-dir "yera")
;(regendata-dir "gebo")
(make)

(with-exception-catcher
 (lambda (ex)
   (pp '(Warning unable to load termite)))
 (lambda ()
   (load "~~/lib/termite/termite")
   (set! files (cons "gebo-termite" files))
   (set! output "fti")
   (make)))
  
