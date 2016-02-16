(ql:quickload :cl-who)
;;(load "processpcap/module.lisp")
;;(load "sipcheck/module.lisp")

(load "sipcheck/data-extract2.lisp")
(load "sipcheck/check.lisp")
(load "sipcheck/compare-message.lisp")
(load "sipcheck/data.lisp")

(load "processpcap/tools.lisp")
(load "processpcap/test-05-udp.lisp")
(load "processpcap/output-to-var.lisp")
(load "processpcap/rtp.lisp")

(load "ip-header.lisp")
(load "tools.lisp")
(load "html.lisp")

(defvar *ip-list* nil)

(defun push-to-ip-list (number ip)
  (unless (member ip (assoc number *ip-list*) :test #'equal)
    (nconc (assoc number *ip-list*) (list ip))))

(defun determine-column-and-array (ip)
  (let* ((iplist (coerce ip 'list))
	 (result    
	  (apply #'min (loop for x in *ip-header-list*
			  collect (- 5 (length
					  (member iplist x :test #'equal)))))))
    (push-to-ip-list result iplist)
    result))

(defun get-messages-from-ppcap (p-pcap-result)
  (let (left-messages right-messages)
    (loop for x in (remove-duplicates p-pcap-result) do
	 (let ((source (determine-column-and-array (car x))))
	       (cond
		 ((or (= source 1) (= source 2))
		  (push (seq-to-string
			 (remove 13 (caddr x))) left-messages)) ;; remove #\return
		 ((or (= source 3) (= source 4))
		  (push (seq-to-string
			 (remove 13 (caddr x))) right-messages))
		 (t nil))))
    (list (reverse left-messages) (reverse right-messages))))

;;(cl-who:with-html-output-to-string (*standard-output*) (:font :color "red" "def"))

;;(defmacro set-color (a b)
;;  `(progn (print ,a) (print ,b)
;;	  (cl-who:with-html-output-to-string (s) (:font :color ,b ,a))))

(defun set-color (x)
  (format nil "<font color='~A'>~A</font>" (cadr x) (car x)))

(defun convert-ppcap-to-html (filename)
  (let ((text-result (get-messages-from-ppcap (pcap-process filename))))
    (output-2html (car text-result) (cadr text-result))))

(defun set-color-list (mame)
  (progn
    (print mame)
    (mapcar (lambda (x)
	      (if (consp x)
		  (set-color x)
		  x)) mame)))
  
(defun output-2html (left-messages right-messages)
  (let ((temp1 (car left-messages))
	(temp2 (car right-messages)))
    (multiple-value-bind (temp1-result temp2-result index-pairs)
	(compare-message (make-string-input-stream temp1)
			 (make-string-input-stream temp2))
      (standard-page (:title "test")
	(:div :id "\"contents\""
	      (:table
	       (loop
		  for tr1 in (html-escape temp1-result)
		  for z in index-pairs
		  for tr2 in (html-escape temp2-result) do
		    (cl-who:htm
		     (:tr (:td (cl-who:fmt
				(format nil "~{~a~}" (set-color-list tr1))))
			  (:td (cl-who:fmt (format nil "~a" z)))
			  (:td (cl-who:fmt
				(format nil "~{~a~}" (set-color-list tr2))))
			  )))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; html escape

(defun html-escape (tree)
  (process-tree (lambda (x) (if (stringp x)
				(substitute-for-html-string x)
				x)) tree))

(defmacro substitute-char-for-html-string (special-char special-string the-line)
  `(loop
      (let ((position-special-char (position ,special-char ,the-line)))
	(if position-special-char
	    (setf ,the-line (concatenate
			   'string
			   (subseq ,the-line 0 position-special-char)
			   ,special-string
			   (subseq ,the-line (+ 1 position-special-char))))
	    (return)))))
  
(defun substitute-for-html-string (line)
  (let ((result line))
    (substitute-char-for-html-string #\< "&lt;" result)
    (substitute-char-for-html-string #\> "&gt;" result)
    (substitute-char-for-html-string #\" "&quot;" result)
    result))

;; (get-messages (processpcap::pcap-process "123.pcapng"))
;; (setf *temp* *)
;; (process-a-message (make-string-input-stream (car (car *temp*))))


