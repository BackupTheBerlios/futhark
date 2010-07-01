
(include "cgen#.scm")
(include "fsm#.scm")
(include "parser#.scm")
(include "sets#.scm")
(include "../language#.scm")

(load (string-append (path-directory (this-source-file)) "cgen.scm"))
(load (string-append (path-directory (this-source-file)) "fsm.scm"))
(load (string-append (path-directory (this-source-file)) "parser.scm"))
(load (string-append (path-directory (this-source-file)) "sets.scm"))
(load (string-append (path-directory (this-source-file)) "../sources.scm"))
(load (string-append (path-directory (this-source-file)) "../extras.scm"))
(load (string-append (path-directory (this-source-file)) "../kernel.scm"))
