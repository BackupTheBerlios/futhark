(##namespace
 ("ehwas-response#"
  make-response
  response?
  response-version response-version-set!
  response-code response-code-set!
  response-status response-status-set!
  response-header response-header-set!
  response-writer response-writer-set!
  response
  header

  write-http-response
  ))

;; ;; (define-macro (header . ps)
;; ;;   (let(
;; ;;        (m (gensym 'm)))
;; ;;     `(let(
;; ;;           (,m (make-table init: #f)))
;; ;;        ,@(map (lambda (p)
;; ;;                `(table-set! ,m ,(if (symbol? (car p)) (symbol->string (car p)) (car p)) ,(cadr p)))
;; ;;               ps)
;; ;;        ,m)))

(define-macro (header . ps)
  `(list ,@(let loop ((ps ps) (rs '()))
             (cond
              ((null? ps) (reverse rs))
              ((keyword? (car ps)) (loop (cddr ps) (cons `(cons ,(keyword->string (car ps)) ,(cadr ps)) rs)))
              (else (error "wrong response header format" ps))))))

(define-macro (response v c s h . w)
  `(make-response ,c ,s ,h (lambda () ,@w)))