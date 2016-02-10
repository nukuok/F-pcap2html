(defpackage sipcheck)
(in-package sipcheck)

(defvar *path*)
(setf *path* "c:/Users/Administrator/Desktop/program/little-things/pcap-output-for-sip-check/sipcheck/")

(defmacro module-load (filename)
  `(load ,(concatenate 'string *path* filename)))

(module-load "data-extract2.lisp")

(provide 'sipcheck)
