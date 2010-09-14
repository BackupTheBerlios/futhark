(define gsc (make-parameter "gsc -expansion"))

(define rm (make-parameter "rm"))
(define cp (make-parameter "cp"))

(define src-dir (make-parameter "src"))
(define dest-dir (make-parameter (path-expand "~~/site-scheme/futhark")))