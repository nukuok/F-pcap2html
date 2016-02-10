(ql:quickload :cl-ppcre)
;;(defvar *color-rule*)
;;(load "color-rule.lisp")


(defvar *left-current-row-number* 0)
(defvar *right-current-row-number* 0)

(defvar *left-messages* nil)
(defvar *right-messages* nil)

(defvar *left-array* )
(setf *left-array* "-left-")
(defvar *right-array* )
(setf *right-array* "-right-")

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

(defun message-list-initial ()
  (progn
    (setf *ip-list* nil)
    (setf *ip-list* (list (list 1) (list 2) (list 3) (list 4)))
    (setf *left-current-row-number* 0)
    (setf *right-current-row-number* 0)
    (setf *left-messages* nil)
    (setf *right-messages* nil)))

(defmacro split-by-0a0d (data)
  `(let ((tempdata ,data)
	 (tempresult nil))
     (loop
	(let ((position-of-0d (position 13 tempdata))
	      (position-of-0a (position 10 tempdata)))
	  (cond ((not position-of-0a)
		 (push "" tempresult)
		 (return (reverse tempresult)))
		((equal (+ 1 position-of-0d) position-of-0a)
		 (push (seq-to-string (subseq tempdata 0 position-of-0d))
		       tempresult)
		 (setf tempdata (subseq tempdata (+ 1 position-of-0a))))
		(position-of-0a
		 (push (seq-to-string (subseq tempdata 0 position-of-0a))
		       tempresult)
		 (setf tempdata (subseq tempdata (+ 1 position-of-0a))))
		(t (print "never-be-here-split-by-0a0d")))))))

(defmacro seq-to-string (seq)
  `(coerce (mapcar
	    #'code-char
	    (coerce ,seq 'list))
	   'string))

(defmacro push-to-messages (splitted-line array messages current-row-number)
  `(let ((message-to-push (cons ,array ,splitted-line))
	 (row-number (+ 1 ,current-row-number)))
     (loop for line in message-to-push do
	  (push (list row-number line) ,messages)
	  (setf row-number (+ row-number 1)))
     (setf ,current-row-number (+ 1 row-number))))

(defun make-arrow (arrow index)
  (concatenate 'string arrow " F" (write-to-string index) " " arrow))


(defun process-result (result index)
  (let ((message (caddar result))
	(column-and-array (determine-column-and-array (caar result))))
    (when result
      (cond ((= column-and-array 1)
	     (setf *right-current-row-number*
		   (max *right-current-row-number*
			(+ 1 *left-current-row-number*)))
	     (push-to-messages (split-by-0a0d message)
			       (make-arrow *right-array* index)
			       *left-messages* *left-current-row-number*))
	    ((= column-and-array 2)
	     (push-to-messages (split-by-0a0d message)
			       (make-arrow *left-array* index)
			       *left-messages* *left-current-row-number*))
	    ((= column-and-array 3)
	     (push-to-messages (split-by-0a0d message)
			       (make-arrow *right-array* index)
			       *right-messages* *right-current-row-number*))
	    ((= column-and-array 4)
	     (setf *left-current-row-number*
		   (max *left-current-row-number*
			(+ 1 *right-current-row-number*)))
	     (push-to-messages (split-by-0a0d message)
			       (make-arrow *left-array* index)
			       *right-messages* *right-current-row-number*)))
      (if (< column-and-array 5)
	  (process-result (cdr result) (+ index 1))
	  (process-result (cdr result) index)))))

(defun output-html-header (outstream)
  (format outstream "~{~A~%~}"
	  '("<html>"
	    "  <head>"
	    "    <link rel=\"stylesheet\" type=\"text/css\" href=\"..\\style.css\">"
	    "  </head>"
	    "  <body>"
	    "    <div id=\"contents\">"
	    "      <table class=\"style\">")))
(defun output-html-tail (outstream)
  (format outstream "~{~A~%~}"
	  '("      </table>"
	    "    </div>"
	    "  </body>"
	    "</html>")))
(defun output-tr-begin (outstream)
  (format outstream "~A~%" "        <tr>"))
(defun output-tr-end (outstream)
  (format outstream "~A~%" "        </tr>"))

(defun output-td (outstream line)
  (let* ((pre-result (if (consp line) (car line) line))
	 (result (funcall (arrow-modify) pre-result)))
    (loop for (a b) in *color-rule* do
	 (setf result 
	       (funcall (show-color a (coerce b 'sequence)) result)))
    (cond ((and (consp line) (equal "left" (cadr line)))
	   (format outstream "~A~%"
		   (concatenate 'string
				"          <td align=\"left\">" result "</td>")))
	  ((and (consp line) (equal "right" (cadr line)))
	   (format outstream "~A~%"
		   (concatenate 'string
				"          <td align=\"right\">" result "</td>")))
	  (t (format outstream "~A~%"
		     (concatenate 'string
				  "          <td>" result "</td>"))))))

(defun output-item (left-messages right-messages outstream index cc)
  (when (> cc 0)
    (output-tr outstream
	       (write-to-string index)
	       (substitute-for-html-string (cadr (assoc index left-messages)))
	       nil
	       (substitute-for-html-string (cadr (assoc index right-messages)))
	       nil)
    (output-item left-messages right-messages outstream (+ index 1) (- cc 1))))

(defun output-tr (outstream a b c d e)
  (progn
    (output-tr-begin outstream)
    (output-td outstream a)
    (output-td outstream b)
    (output-td outstream c)
    (output-td outstream d)
    (output-td outstream e)
    (output-tr-end outstream)))

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

(defun remove-messages-duplicates (x)
  (remove-duplicates x
		     :test (lambda (x y)
			     (equal (coerce (caddr x) 'list)
				    (coerce (caddr y) 'list)))))

(defun ip-2string (x)
  (if (> (length x) 4)
      (format nil "~{~2,'0x~2,'0x:~2,'0x~2,'0x:~2,'0x~2,'0x:~2,'0x~2,'0x:~2,'0x~2,'0x:~2,'0x~2,'0x:~2,'0x~2,'0x:~2,'0x~2,'0x~}" x)
      (format nil "~{~d.~d.~d.~d~}" x)))
 
(defun ip-list-2html (x)
  (labels ((iter (list acc)
	     (if list
		 (iter (cdr list)
		       (concatenate 'string
				    acc
				    "---newline---"
				    (ip-2string (car list))))
		 acc)))
    (iter (cdr x) (ip-2string (car x)))))
	       
(defun output2html-page (filename)
  (let ((result (remove-messages-duplicates (pcap-process filename))))
    (message-list-initial)
    (process-result result 0)
    (with-open-file
	(outstream (concatenate 'string (subseq filename 0 (position #\. filename))
				"-d/result.html")
	;;(outstream (concatenate 'string
	;;			filename
	;;			"-d/result.html")
		   :direction :output
		   :if-exists :supersede
		   :if-does-not-exist :create)
      (output-html-header outstream)
      (output-tr outstream
		 "ngn"
		 nil
		 "sbc"
		 nil
		 "carrier")
      (output-tr outstream
		 (list "ip->" "right")
		 (ip-list-2html (cdr (assoc 1 *ip-list*)))
		 (list "ip->" "right")
		 (ip-list-2html (cdr (assoc 3 *ip-list*)))
		 nil)
      (output-tr outstream
		 nil
		 (list (ip-list-2html (cdr (assoc 2 *ip-list*))) "right")
		 "->ip"
		 (list (ip-list-2html (cdr (assoc 4 *ip-list*))) "right")
		 "->ip")
      (output-item (reverse *left-messages*)
		   (reverse *right-messages*)
		   outstream
		   1
		   (max (or (caar *left-messages*) 0)
			(or (caar *right-messages*) 0)))
      (print *left-messages*)
      (output-html-tail outstream))))


(defun show-color (line-to-be-colored color)
  (lambda (line-to-be-replaced)
    (cl-ppcre:regex-replace-all
     line-to-be-colored
     line-to-be-replaced
     (list
      (concatenate 'string "<strong><font color=" color ">")
      :match
      "</font></strong>"))))

(defun arrow-modify ()
  (lambda (line-to-be-replaced)
    (let* ((result-a
	   (cl-ppcre:regex-replace 
	    "-left-[\ F0-9]*-left-"
	    line-to-be-replaced
	    (list 
	    "<strong><center> <<<<<<<<<<<<<<<<<<<<"
	    :match
	    "<<<<<<<<<<<<<<<<<<<< </center></strong>")))
	   (result-b
	    (cl-ppcre:regex-replace 
	     "-right-[\ F0-9]*-right-"
	     result-a
	     (list
	      "<strong><center> >>>>>>>>>>>>>>>>>>>>"
	      :match
	      ">>>>>>>>>>>>>>>>>>>> </center></strong>")))
	   (result-c
	    (cl-ppcre:regex-replace-all
	     "-right-"
	     result-b
	     ""))
	   (result-d
	    (cl-ppcre:regex-replace-all
	     "-left-"
	     result-c
	     ""))
	   (result-e
	    (cl-ppcre:regex-replace-all
	     "---newline---"
	     result-d
	     "<br>")))
      result-e)))

