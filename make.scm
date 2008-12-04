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

   "yera-compile"
   "yera-mangle"
   "yera-parser"
   "yera-resolver"))

(define (compile-all)
  (shell-command
   (apply string-append
          `("gsc -link -flat -o " ,output ".o1.c"
            ,@(map (lambda (f) (string-append " " f)) files)))))

(define (link)
  (shell-command
   (apply string-append
          `("gcc -shared -D___DYNAMIC -fPIC -I/usr/local/Gambit-C/current/include"
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
  (regendata-dir "yera")
  (regendata-dir "gebo")
  (compile-all)
  (link)
  (clean))

(make)
