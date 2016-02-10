(defpackage processpcap)
(in-package processpcap)

(defvar *path*)
(setf *path* "c:/Users/Administrator/Desktop/program/little-things/pcap-output-for-sip-check/processpcap/")

(defmacro module-load (filename)
  `(load ,(concatenate 'string *path* filename)))

(module-load "tools.lisp")
(module-load "test-05-udp.lisp")
(module-load "output-to-var.lisp")
(module-load "rtp.lisp")

(provide 'processpcap)
