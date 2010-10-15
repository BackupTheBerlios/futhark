(load "../src/ehwas/rfc3986")
(load "../src/ehwas/rfc822")
(load "../src/ehwas/request")

(include "../src/ehwas/request#.scm")

(define *request-1.1* #<<end
GET /foo/bar.ciao HTTP/1.1
Host: www.foo.biz
Content-type: text/html
Content-length: 10

01234567890
end
)

(define *request-1.0* #<<end
GET /foo/bar.ciao 
Host: www.foo.biz
Content-type: text/html
Content-length: 10

01234567890
end
)

(time (pp 
       (with-input-from-string
	   *request-1.1*
	 read-http-request)))

(time (pp 
       (with-input-from-string
	   *request-1.0*
	 read-http-request)))



