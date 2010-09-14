(include "common.scm")

(define (clean)
  (clean-ansuz)
  (clean-database)
  (clean-ehwas)
  (clean-encode)
  (clean-gebo)
  (clean-laguz)
  (clean-yera)
  (clean-test)
  (clean-make))


(define (clean-ansuz)
  (sh (rm) (string-append (src-dir) "/ansuz/*~"))
  (sh (rm) (string-append (src-dir) "/ansuz/*.o*"))
  (sh (rm) (string-append (src-dir) "/ansuz/sources/*~"))
  (sh (rm) (string-append (src-dir) "/ansuz/sources/*.o*"))
  (sh (rm) (string-append (src-dir) "/ansuz/re/*~"))
  (sh (rm) (string-append (src-dir) "/ansuz/re/*.o*")))

(define (clean-database)
  (sh (rm) (string-append (src-dir) "/database/*~"))
  (sh (rm) (string-append (src-dir) "/database/*.o*")))

(define (clean-ehwas)
  (sh (rm) (string-append (src-dir) "/ehwas/*~"))
  (sh (rm) (string-append (src-dir) "/ehwas/*.o*"))
  (sh (rm) (string-append (src-dir) "/ehwas/sessions/*~"))
  (sh (rm) (string-append (src-dir) "/ehwas/sessions/*.o*")))

(define (clean-encode)
  (sh (rm) (string-append (src-dir) "/encode/*~"))
  (sh (rm) (string-append (src-dir) "/encode/*.o*")))

(define (clean-gebo)
  (sh (rm) (string-append (src-dir) "/gebo/*~"))
  (sh (rm) (string-append (src-dir) "/gebo/*.o*")))

(define (clean-laguz)
  (sh (rm) (string-append (src-dir) "/laguz/*~"))
  (sh (rm) (string-append (src-dir) "/laguz/*.o*")))

(define (clean-yera)
  (sh (rm) (string-append (src-dir) "/yera/*~"))
  (sh (rm) (string-append (src-dir) "/yera/*.o*")))

(define (clean-make)
  (sh (rm) (string-append "make/*~"))
  (sh (rm) (string-append "make/*.o*")))

(define (clean-test)
  (sh (rm) (string-append  "test/*~"))
  (sh (rm) (string-append "test/*.o*"))
  (sh (rm) (string-append "test/poll/*~"))
  (sh (rm) (string-append "test/poll/*.o*"))
  (sh (rm) (string-append "test/yera/*~"))
  (sh (rm) (string-append "test/yera/*.o*"))
  (sh (rm) (string-append "test/chatroom/*~"))
  (sh (rm) (string-append "test/chatroom/*.o*")))

(clean)
