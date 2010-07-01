;;;============================================================================

;;; File: "zlib.scm", Time-stamp: <2007-04-05 00:55:20 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.
;;; Copyright (c) 2006 by Manuel Serrano, All Rights Reserved.

;;;============================================================================

;;; Contains procedures to compress and decompress data in DEFLATE and
;;; GZIP format.

;;; The following documents are specifications of the various formats:
;;;
;;;  RFC 1950 "ZLIB Compressed Data Format Specification version 3.3"
;;;  RFC 1951 "DEFLATE Compressed Data Format Specification version 1.3"
;;;  RFC 1952 "GZIP file format specification version 4.3"

(##namespace
 ("encode-zlib#"
  gzip-port
  deflate-port
  gzip-u8vector
  deflate-u8vector
  gunzip-port
  inflate-port
  gunzip-u8vector
  inflate-u8vector
  zlib-exception?
  zlib-exception-message))
