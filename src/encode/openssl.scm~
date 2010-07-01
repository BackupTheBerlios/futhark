(##namespace ("encode-openssl#"))
(##include "~~/lib/gambit#.scm")

(declare
 (fixnum)
 (standard-bindings)
 (extended-bindings)
 ;; (block)
 ;; (not safe)
 )
(c-declare #<<end-of-decls
           
#include <openssl/crypto.h>

#include <openssl/dh.h>

#include <openssl/dsa.h>

#include <openssl/err.h>

#include <openssl/evp.h>

#include <openssl/opensslv.h>

#include <openssl/pem.h>

#include <openssl/rand.h>

#ifndef NO_RSA
 #include <openssl/rsa.h>
#endif

#include <openssl/ssl.h>

#include <openssl/x509.h>

#include <openssl/x509_vfy.h>

end-of-decls
)

(c-initialize #<<end-of-init

  SSL_load_error_strings ();
  SSL_library_init ();
  
end-of-init
)

;;; TYPES
(c-define-type SSL_METHOD "SSL_METHOD")
(c-define-type SSL_METHOD* (pointer SSL_METHOD))


(c-define-type SSL_CIPHER "SSL_CIPHER")
(c-define-type SSL_CIPHER* (pointer SSL_CIPHER))

(c-define-type SSL_CTX "SSL_CTX")
(c-define-type SSL_CTX* (pointer SSL_CTX))

(c-define-type SSL_SESSION "SSL_SESSION")
(c-define-type SSL_SESSION* (pointer SSL_SESSION))

(c-define-type SSL "SSL")
(c-define-type SSL* (pointer SSL))

(c-define-type BIO "BIO")
(c-define-type BIO* (pointer BIO))

(c-define-type BIO_METHOD "BIO_METHOD")
(c-define-type BIO_METHOD* (pointer BIO_METHOD))

(define ssl-v2-client-method
  (c-lambda () SSL_METHOD* "SSLv2_client_method"))

(define ssl-v2-server-method
  (c-lambda () SSL_METHOD* "SSLv2_server_method"))

(define ssl-v2-method
  (c-lambda () SSL_METHOD* "SSLv2_method"))

(define ssl-v3-client-method
  (c-lambda () SSL_METHOD* "SSLv3_client_method"))

(define ssl-v3-server-method
  (c-lambda () SSL_METHOD* "SSLv3_server_method"))

(define ssl-v3-method
  (c-lambda () SSL_METHOD* "SSLv3_method"))

(define tls-v1-client-method
  (c-lambda () SSL_METHOD* "TLSv1_client_method"))

(define tls-v1-server-method
  (c-lambda () SSL_METHOD* "TLSv1_server_method"))

(define tls-v1-method
  (c-lambda () SSL_METHOD* "TLSv1_method"))

(define ssl-v23-client-method
  (c-lambda () SSL_METHOD* "SSLv23_client_method"))

(define ssl-v23-server-method
  (c-lambda () SSL_METHOD* "SSLv23_server_method"))

(define ssl-v23-method
  (c-lambda () SSL_METHOD* "SSLv23_method"))

(define ssl-ctx-new
  (c-lambda (SSL_METHOD*) SSL_CTX*
    "SSL_CTX_new"))

(define ssl-ctx-use-certificate-chain-file
  (c-lambda (SSL_CTX* char-string) int
    "SSL_CTX_use_certificate_chain_file"))

(define ssl-ctx-load-verify-locations
  (c-lambda (SSL_CTX* char-string char-string) int
    "SSL_CTX_load_verify_locations"))


(define SSL_FILETYPE_PEM 1) ;; X509_FILETYPE_PEM in x509.h
(define SSL_FILETYPE_ASN1 2) ;; X509_FILETYPE_ASN1 in x509.h
  
(define ssl-ctx-use-certificate-file
  (c-lambda (SSL_CTX* char-string int) int
    "SSL_CTX_use_certificate_file"))

(define ssl-ctx-use-private-key-file
  (c-lambda (SSL_CTX* char-string int) int
    "SSL_CTX_use_PrivateKey_file"))

(define ssl-ctx-check-private-key
  (c-lambda (SSL_CTX*) int "SSL_CTX_check_private_key"))

(define ssl-ctx-free
  (c-lambda (SSL_CTX*) void
    "SSL_CTX_free"))

(define ssl-ctx-set-session-id-context
  (c-lambda (SSL_CTX* char-string unsigned-int) int
    "___result=SSL_CTX_set_session_id_context(___arg1,(unsigned char*) ___arg2, ___arg3);"))

;; (define ssl-ctx-set-session-cache-mode
;;   (c-lambda (SSL_CTX* long-int) void
;;       "SSL_CTX_set_session_cache_mode"))

;; (define ssl-ctx-get-session-cache-mode
;;   (c-lambda (SSL_CTX*) long-int
;;       "SSL_CTX_get_session_cache_mode"))

;; (include "more-ssl-ctx-context")

;; ssl context
(define ssl-new
  (c-lambda (SSL_CTX*) SSL*
    "SSL_new"))

(define ssl-accept
  (c-lambda (SSL*) int
    "SSL_accept"))

(define ssl-connect
  (c-lambda (SSL*) int
    "SSL_connect"))

(define ssl-check-private-key
  (c-lambda (SSL*) int
    "SSL_check_private_key"))

(define ssl-clear
  (c-lambda (SSL*) void
    "SSL_clear"))

(define ssl-ctrl
  (c-lambda (SSL* int long char-string) long
      "SSL_ctrl"))

(define ssl-do-handshake
  (c-lambda (SSL*) int
    "SSL_do_handshake"))

(define ssl-free
  (c-lambda (SSL*) void
    "SSL_free"))

(define ssl-get-ctx
  (c-lambda (SSL*) SSL_CTX*
    "SSL_get_SSL_CTX"))

(define ssl-get-rbio
  (c-lambda (SSL*) BIO*
    "SSL_get_rbio"))

(define ssl-get-wbio
  (c-lambda (SSL*) BIO*
    "SSL_get_wbio"))

(define ssl-pending
  (c-lambda (SSL*) int
    "SSL_pending"))

(define ssl-set-accept-state
  (c-lambda (SSL*) void
    "SSL_set_accept_state"))

(define ssl-set-connect-state
  (c-lambda (SSL*) void
    "SSL_set_connect_state"))

(define ssl-shutdown
  (c-lambda (SSL*) int
    "SSL_shutdown"))

(define ssl-free
  (c-lambda (SSL*) void
    "SSL_free"))

(define ssl-clear
  (c-lambda (SSL*) void
    "SSL_clear"))

(define ssl-get-shutdown
  (c-lambda (SSL*) int
    "SSL_get_shutdown"))

(define ssl-set-shutdown
  (c-lambda (SSL* int) void
    "SSL_set_shutdown"))
  

;; pay attention to wrong scheme object (must be u8vector)
(define ssl-read
  (c-lambda (SSL* scheme-object int) int
#<<end-ssl-read
    if (! ___U8VECTORP (___arg2)) return ___FIX (___UNKNOWN_ERR);
    ___result = SSL_read (___arg1, (void*) ___BODY_AS (___arg2, ___tSUBTYPED),___arg3);
end-ssl-read
))

(define ssl-write
  (c-lambda (SSL* scheme-object int) int
#<<end-ssl-write
    if (! ___U8VECTORP (___arg2)) return ___FIX (___UNKNOWN_ERR);
    ___result = SSL_write (___arg1, (void*) ___BODY_AS (___arg2, ___tSUBTYPED),___arg3);
end-ssl-write
))

(define ssl-set-bio
  (c-lambda (SSL* BIO* BIO*) void
    "SSL_set_bio"))

;; BIO context

(define bio-new
  (c-lambda (BIO_METHOD*) BIO*
    "BIO_new"))

(define bio-set
  (c-lambda (BIO* BIO_METHOD*) int
    "BIO_set"))

(define bio-free
  (c-lambda (BIO*) void
    "BIO_free"))

;; are these macros or functions?
(define bio-pending
  (c-lambda (BIO*) int
    "BIO_ctrl_pending"))

(define bio-eof
  (c-lambda (BIO*) int
    "BIO_eof"))

(define bio-read
  (c-lambda (BIO* scheme-object int) int
#<<end-bio-read
    if (! ___U8VECTORP (___arg2)) return ___FIX (___UNKNOWN_ERR);
    ___result = BIO_read (___arg1, (void*) ___BODY_AS (___arg2, ___tSUBTYPED),___arg3);
end-bio-read
))

(define bio-write
  (c-lambda (BIO* scheme-object int) int
#<<end-bio-write
if (! ___U8VECTORP (___arg2)) return ___FIX (___UNKNOWN_ERR);
___result = BIO_write (___arg1, (void*) ___BODY_AS (___arg2, ___tSUBTYPED),___arg3);
end-bio-write
))

(define bio-s-mem
  (c-lambda () BIO_METHOD*
    "BIO_s_mem"))

(define bio-s-file
  (c-lambda () BIO_METHOD*
    "BIO_s_file"))

;;; CIPHERS

(define ssl-cipher-description
  (c-lambda (SSL_CIPHER* char-string int) char-string
    "SSL_CIPHER_description"))

(define ssl-cipher-get-bits
  (c-lambda (SSL_CIPHER* (pointer int)) int
    "SSL_CIPHER_get_bits"))

(define ssl-cipher-get-name
  (c-lambda (SSL_CIPHER*) char-string
    "SSL_CIPHER_get_name"))

(define ssl-cipher-get-version
  (c-lambda (SSL_CIPHER*) char-string
    "SSL_CIPHER_get_version"))

;;; ERRORS
(define err-get-error
  (c-lambda () int64
    "ERR_get_error"))

(define err-peek-error
  (c-lambda () int64
    "ERR_peek_error"))

(define err-peek-last-error
  (c-lambda () int64
    "ERR_peek_last_error"))

(define err-error-string
  (c-lambda (int64) char-string
    "___result = ERR_error_string (___arg1, NULL);"))
    

(define ssl-get-error
  (c-lambda (SSL* int) int
    "SSL_get_error"))

(define SSL_ERROR_NONE			0)
(define SSL_ERROR_SSL			1)
(define SSL_ERROR_WANT_READ		2)
(define SSL_ERROR_WANT_WRITE		3)
(define SSL_ERROR_WANT_X509_LOOKUP	4)
(define SSL_ERROR_SYSCALL		5)
(define SSL_ERROR_ZERO_RETURN		6)
(define SSL_ERROR_WANT_CONNECT		7)
(define SSL_ERROR_WANT_ACCEPT		8)

