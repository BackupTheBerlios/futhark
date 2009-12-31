(define yuicompress?
  (= 256 (shell-command "yuicompress")))

(define files 
  (list
   "actors.js"
   "actors_test.js"
   "actors_match.js"
   "actors_remote.js"))

(define (compress-files files)
  (shell-command "mkdir temp")
  (for-each (lambda (file) (shell-command (string-append "yuicompress " file " > temp/" file)))
            files)
  (shell-command
   (string-append
    "cat" (apply string-append (map (lambda (c) (string-append " temp/" c)) files))
    ">rts.js"))
  (shell-command "rm -r temp"))


(define (concat-files files)
  (shell-command
   (string-append "cat "
                  (apply
                   string-append
                   (map (lambda (c) (string-append " " c)) files))
                  " > rts.js")))

(if yuicompress?
    (compress-files files)
    (concat-files files))
;; (shell-command
;;  (string-append "cat "
;;                 (apply
;;                  string-append
;;                  (map (lambda (c) (string-append " " c)) files))
;;                 " > rts.js"))
 

(define size (file-size "rts.js"))

(define out-vector (make-u8vector size))

(call-with-input-file "rts.js"
  (lambda (p)
    (read-subu8vector out-vector 0 size p)))

(call-with-output-file "../rts.scm"
  (lambda (p)
    (pp `(define *-rts-data-* ',out-vector) p)
    (pp `(define *-rts-size-* ,size) p)))

(shell-command "rm rts.js")
