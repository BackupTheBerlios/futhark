(define (poll name)
  (let loop (
             (con (connect "polls" "francesco")))
    (recv
     (('vote cid)
      (execute `("SELECT vote (" ,(c cid) ")"))
      (loop con))
     
     (('result vid from)
      (let(
           (res (execute `("SELECT partial_result "
                           "FROM partial_results "
                           "WHERE choice_id IN "
                           "SELECT 
       