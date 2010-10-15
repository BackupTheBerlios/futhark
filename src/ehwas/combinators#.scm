(##namespace
 ("ehwas-combinators#"
  orelse
  cache
  filesystem
  with-table
  with-index
  with-prefix
  allow
  deny
  redir
  redirect
  catch-exception
  paths
  get?
  post?
  ))

(define-macro (paths . ls)
  (let(
       (tbl (gensym 'table)))
    `(let(
          (,tbl (make-table)))
       (begin ,@(map (lambda (l) `(table-set! ,tbl ',(car l) ,(cadr l))) ls))
       (with-table ,tbl))))
