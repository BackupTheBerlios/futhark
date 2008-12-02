(define squish? #t)
(define files 
  (list
   "compat_dom.js"
   "patch.js"
   "yera.js"
   "yera_core.js"
   "yera_dom.js"
   "yera_math.js"
   "yera_userevent.js"))


(define (squish files)
  (shell-command "mkdir temp")
  (for-each (lambda (file)
              (shell-command
               (string-append
                "js_compactor --opt"
                " --src " file
                " --dest temp/" file)))
            files)
  (shell-command
   (string-append
    "cat" (apply string-append
                 (map (lambda (c) (string-append " temp/" c)) files))
    ">rts.js"))
  (shell-command "rm -r temp"))


(define (concat-files files)
  (shell-command
   (string-append "cat "
                  (apply
                   string-append
                   (map (lambda (c) (string-append " " c)) files))
                  " > rts.js")))

(if squish?
    (squish files)
    (concat-files files))


(define size (file-size "rts.js"))

(define out-vector (make-u8vector size))

(call-with-input-file "rts.js"
  (lambda (p)
    (read-subu8vector out-vector 0 size p)))

(call-with-output-file "../yera-rts.scm"
  (lambda (p)
    (pp `(define *-rts-data-* ',out-vector) p)
    (pp `(define *-rts-size-* ,size) p)))

(shell-command "rm rts.js")
