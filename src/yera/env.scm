(load "streams")
(load "kernel")
(load "extras")
(load "expressions")
(load "mangle")
(load "compile")
(load "parser")

(include "streams#.scm")
(include "language#.scm")
(include "compile#.scm")
(include "parser#.scm")

(define-macro (anyway e . es)
  (let(
       (ex (gensym 'ex)))
    `(with-exception-catcher
      (lambda (,ex) (begin ,@es (raise ,ex)))
      (lambda ()
        (begine ,e ,@es)))))
        
              
(define (yerac yf #!optional
               (jf (string-append
                    (path-strip-extension yf)
                    ".js")))
  
  ;; (pp `(,yf -> ,jf))
  (call-with-input-file yf
    (lambda (in)
      (call-with-output-file jf
        (lambda (out)
          (yera->js (path-directory yf) in out))))))
  
(define (yera-bc yf)
  (call-with-input-file yf
    (lambda (p)
      (yera-parser#yera->bytecode (path-directory yf) p))))
    
(define (test)
  (let(
       (o (current-directory)))
    (current-directory "yera")
    (with-exception-catcher
     (lambda (ex)
       (current-directory o)
       (raise ex))
     (lambda ()
       (call-with-input-file "packages.yera" yera->js)
       (current-directory o)))))

