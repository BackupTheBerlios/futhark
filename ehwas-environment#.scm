#! /usr/bin/env gsi 

(load "ansuz-streams")
(load "ansuz-kernel")
(load "ansuz-extras")

(load "rfc3986")
(load "rfc822")
(load "uids")
(load "ehwas-request")	
(load "ehwas-response")
(load "ehwas-errors")				
(load "ehwas-resolver")
(load "ehwas-server")

(load "ehwas-query")
(load "ehwas-cookies")
(load "ehwas-sessions")
(load "ehwas-pages")

;; (load "brillo/json")
;; (load "brillo/brillo")

(include "ehwas-server#.scm")
(include "ehwas-resolver#.scm")
(include "ehwas-pages#.scm")
(include "ehwas-sessions#.scm")

;; (include "brillo/brillo#.scm")

(setenv "FUTHARK_HOME" (current-directory))

(define server
  (make-server
   #f 9080
   (let(
        (res0
          (make-cached-resolver
           (make-filesystem-resolver "www")))
        (res1
         (make-serverpage-resolver "www")))
     
     (make-guarded-resolver
      (orelse-resolver
       (with-index-resolver "index.ehwas" res1)
       (with-index-resolver "index.html" res0)
       (with-index-resolver "index.htm" res0)
       res1
       res0
       not-found-resolver)))))

(set-session-dir! "sessions")

(start! server)
