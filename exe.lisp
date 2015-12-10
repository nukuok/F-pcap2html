(load "test-04-2html.lisp")

(defun run-pcap2html ()
  (with-open-file
      (in "color-rule.lisp" :if-does-not-exist :create))
  (with-open-file
      (in "ip-header.lisp" :if-does-not-exist :create))
  (load "ip-header.lisp")
  (load "color-rule.lisp")
  (loop for x in (directory "*.pcap*") do
     (format t "Processing-file: ~A~%" (namestring x))
       (output2html-page (namestring x))))

(ccl:save-application "pcap2html.exe"
		  :toplevel-function #'run-pcap2html
		  :prepend-kernel t)

	
