(load "../src/encode/base64")
(load "../src/ehwas/mime-types")
(load "../src/ehwas/base64-inline")
(include "../src/ehwas/base64-inline#.scm")

(pp (base64-inline "star.png"))