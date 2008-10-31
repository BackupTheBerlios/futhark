(##namespace
 ("ansuz-streams#"
  stream-cons
  stream-car
  stream-cdr
  port->stream
  string->stream
  nul-stream
  list->stream
  stream
  stream->list
  stream->string
  stream->port
  stream-append))

(define-macro (stream-cons a d)
  `(delay (cons ,a ,d)))