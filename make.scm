;; (define files
;;   (list
;;    "ansuz-expressions.scm"
;;    "ansuz-extras.scm"
;;    "ansuz-kernel.scm"
;;    "ansuz-streams.scm"
   
;;    "rfc3986"
;;    "rfc822"
;;    "uids"
;;    "base64"
   
;;    "ehwas-cookies"
;;    "ehwas-errors"
;;    "ehwas-pages"
;;    "ehwas-query"
;;    "ehwas-request"
;;    "ehwas-resolver"
;;    "ehwas-response"
;;    "ehwas-server"
;;    "file-sessions"
;;    "pg-sessions"

;;    "gebo-json"
;;    "gebo-resolver"

;;    "postgresql.scm"

;;    "yera-compile"
;;    "yera-mangle"
;;    "yera-parser"
;;    "yera-resolver"))

;; (define (compile-all) (for-each compile-file files))


(define (gendata-dir d)
  (current-directory d)
  (shell-command "gsi regen.scm")
  (current-directory ".."))


(define (make)
  (gendata-dir "yera")
  (gendata-dir "gebo")
  (compile-file "fgi"))

(make)
