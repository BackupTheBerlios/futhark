(##namespace
 ("ehwas-resolver#"
  not-found-resolver
  orelse-resolver
  ;; make-guarded-resolver
  make-cached-resolver
  make-filesystem-resolver
  make-table-resolver
  with-index-resolver
  with-prefix-resolver
  allow-resolver
  deny-resolver
  buffer-size
  with-request
  with-request-keys
  path
  query
  cookies
  session
  call-with-request
  redirect
  ))

;; (define-macro (with-request request fn)
;;   (let(
;;        (req (gensym 'req))
;;        (path (gensym 'path))
;;        (query (gensym 'query))
;;        (cookies (gensym 'cookies))
;;        (sess-id (gensym 'sess-id))
;;        (session (gensym 'session))
;;        (response (gensym 'response)))
;;   `(let*(
;;          (,req ,request)
;;          (,path
;;           (request-path ,req))
;;         (,query
;;          (request-parse-query ,req))
;;         (,cookies
;;          (request-cookies ,req))
;;         (,sess-id
;;          (and ,cookies (table-ref ,cookies "Session-id" #f)))
;;         (,session
;;          (session-init ,sess-id))
;;         (,response
;;          (,fn ,path ,query ,cookies ,session)))
;;     (if (not ,sess-id)
;;         (set-cookie! ,response "Session-id"
;;                      (session-identifier ,session) `("path" . "/")))
;;     ,response)))
