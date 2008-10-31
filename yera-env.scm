(load "ansuz-streams")
(load "ansuz-kernel")
(load "ansuz-extras")
(load "ansuz-expressions")
(load "yera-mangle")
(load "yera-compile")
(load "yera-parser")

(include "ansuz-streams#.scm")
(include "ansuz-language#.scm")
(include "yera-compile#.scm")
(include "yera-parser#.scm")

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
  
  (pp `(,yf -> ,jf))
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

