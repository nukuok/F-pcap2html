(load "processpcap/module.lisp")
(load "sipcheck/module.lisp")

(load "ip-header.lisp")

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
		  (push (processpcap::seq-to-string
			 (remove 13 (caddr x))) left-messages)) ;; remove #\return
		 ((or (= source 3) (= source 4))
		  (push (processpcap::seq-to-string
			 (remove 13 (caddr x))) right-messages))
		 (t nil))))
    (list (reverse left-messages) (reverse right-messages))))

(defun process-a-message (instream &optional (result nil))
  (let ((currentline (read-line instream nil "eofeofeof")))
    (cond ((equal currentline "eofeofeof")
	   (reverse (cons (reverse (car result)) (cdr result))));;all over
	  ((equal currentline "")
	   (process-a-message instream
			      (list nil (reverse (car result)))));;sip part over
	  (t
	   (process-a-message instream
			      (cons (cons (process-a-line currentline)
					  (car result))
				    (cdr result)))))))

(defun process-a-line (line &optional charlist (result nil))
  (if (= (length line) 0)
      (reverse (cons (coerce (reverse charlist) 'string) result))
      (let ((currentchar (aref line 0)))
	(cond ((char-equal currentchar #\ )
	       (process-a-line (subseq line 1) nil
			       (cons (coerce (reverse charlist) 'string) result)))
	      ((or (char-equal currentchar #\@) (char-equal currentchar  #\:)
		   (char-equal currentchar  #\,)
		   (char-equal currentchar  #\;) (char-equal currentchar #\=))
	       (process-a-line (subseq line 1) nil
			       (cons currentchar
				     (cons (coerce (reverse charlist) 'string)
					   result))))
	      (t (process-a-line (subseq line 1) (cons currentchar charlist)
		 result))))))

;; (get-messages (processpcap::pcap-process "123.pcapng"))
;; (setf *temp* *)
;; (process-a-message (make-string-input-stream (car (car *temp*))))


