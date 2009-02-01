(file-session-dir "sessions")
(current-session-driver file-session-driver)
(ehwas-pages-compile? #t)

(define (echo)
  (let(
       (c (thread-receive)))
    (cond
     ((eq? c 'die) 'ok)
     ((pair? c)
      (gebo-send (car c) (string-append "this is the echo scheme actor: " (cadr c)))
      (echo))
     (else
      (echo)))))

(define *echo*
  (thread-start!
   (make-thread echo)))

(load "www/chatroom/chatroom")
(load "www/poll/poll")

(define-table registry
  ((echo) (lambda (r) (json-response r *echo*)))
  ((poll) (lambda (r) (json-response r poll#*poll*)))
  ((chatroom) (lambda (r) (json-response r chatroom#*chat*))))

(define-server server0
  (host *)
  (port 1080)
  (resolve
   (allow (extensions .png .jpg .html)
          (with-cache (static www)))
   (with-indexes (index.ehwas index.html) (files www))
   (gebo)
   (with-prefix (registry) (tables registry))))

(pp 'starting)
(start! server0)

                        
                        
                        