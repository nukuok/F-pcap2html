(ql:quickload :cxml)

(defmacro setpath (symbol path)
  (let
      ((default-path "c:/Users/Administrator/Desktop/program/little-things/sip-check/xlsx/"))
    `(progn
       (defvar ,symbol)
       (setf ,symbol ,(concatenate 'string default-path path)))))

;;(setpath *path1* "xl/workbook.xml")
;;(setpath *path2* "xl/worksheets/sheet16.xml")
;;(setpath *path3* "xl/sharedStrings.xml")

;;*path1*
(defun get-sheet-names (filepath)
  (let* ((temp0 (cxml:parse-file filepath (cxml-dom:make-dom-builder)))
	 (temp1 (dom:document-element temp0))
	 (temp2 (dom:get-elements-by-tag-name temp1 "sheets"))
	 (temp3 (dom:get-elements-by-tag-name (aref temp2 0) "sheet")))
    (loop for x in (coerce temp3 'list) collect
	 (let ((name (dom:get-attribute x "name"))
	       (sheetId (dom:get-attribute x "sheetId"))
	       (rid (dom:get-attribute x "r:id")))
	   (list name sheetId rid)))))

;;*path2*
(defun get-shared-data (filepath)
  (let* ((temp0 (cxml:parse-file filepath (cxml-dom:make-dom-builder)))
	 (temp1 (dom:document-element temp0))
	 (temp2 (dom:get-elements-by-tag-name temp1 "si")))
    temp2))


(defun ref-shared-data (data-vector index) 
  (let ((temp0 (dom:child-nodes (aref data-vector index))))
    (format nil "~{~A~}"
	    (loop for x in (coerce temp0 'list) append
		 (if (equal "t" (dom:tag-name x))
		     (list (dom:node-value (aref (dom:child-nodes x) 0)))
		     (if (equal "r" (dom:tag-name x))
			 (list (dom:node-value
				(aref (dom:child-nodes
				       (aref (dom:get-elements-by-tag-name x "t")
					     0)) 0)))))))))

;;*path3*
(defun get-sheet-items (filepath)
  (let* ((temp0 (cxml:parse-file filepath (cxml-dom:make-dom-builder)))
	 (temp1 (dom:document-element temp0))
	 (temp2 (dom:get-elements-by-tag-name temp1 "c")))
    temp2))

(defun filter-sheet-items (sheet-items-tag-c prefix)
  (loop for x in (coerce sheet-items-tag-c 'list) append
       (let ((v (dom:get-elements-by-tag-name x "v"))
	     (r (dom:get-attribute x "r")))
	 (when (and (> (length v) 0)
		    (search (format nil "~A" prefix) r))
	   (list (list r (dom:node-value (aref (dom:child-nodes (aref v 0)) 0))))))))

;;string process check

;;(defclass SIPmessage ()
;;  ((sip :accessor m-SIP :initial-form '(SIP))
;;   (sdp :accessor m-SDP :initial-form '(SDP))))
	
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

;;
(setf *work-folder* "data/")

(defun get-date ()
  (multiple-value-bind (sec min hour d m y) (get-decoded-time)
    (format nil "~{~2,'0d-~}" (list y m d hour min sec))))
;;    (concatenate 'string (mapcar #'princ-to-string (list y m d hour min sec)))))

(defun find-last-in-path (path)
  (let ((position (search "/" path)))
    (if position
	(find-last-in-path (subseq path (+ position 1)))
	path)))

(defun get-messages (prefix p-worksheet shared-data )
  (let ((items (filter-sheet-items (get-sheet-items p-worksheet) prefix))
	(result nil))
    (loop for x in items do
	 (push ">>> Data Reference" result)
	 (push #\newline result)
	 (push (ref-shared-data shared-data (parse-integer (cadr x))) result)
	 (push #\newline result))
    (reverse result)))

(defun save-scenario-files (p-xlsx-folder
			    &optional (indexes nil) (prefix1 #\D) (prefix2 #\G))
  (let
      ((date (get-date))
       (p-workbook (concatenate 'string p-xlsx-folder "xl/workbook.xml"))
       (p-worksheet (concatenate 'string p-xlsx-folder "xl/worksheets/sheet"))
       (p-sharedstrings (concatenate 'string p-xlsx-folder "xl/sharedStrings.xml")))
    (let ((sheet-names (get-sheet-names p-workbook))
	  (shared-data (get-shared-data p-sharedstrings)))
      (loop for x from 1 to (length sheet-names) do
	   (when (or (null indexes) (member x indexes))
	     (let* ((output-sheet (concatenate 'string *work-folder* "/" date
					       (find-last-in-path p-xlsx-folder)
					       (car (nth (- x 1) sheet-names))
					       ".txt"))
		    (p-worksheet-complete (concatenate 'string p-worksheet
						       (princ-to-string x) ".xml"))
		    (left-messages
		     (get-messages prefix1 p-worksheet-complete shared-data))
		    (right-messages
		     (get-messages prefix2 p-worksheet-complete shared-data)))
	       (with-open-file (out output-sheet :direction :output
				    :if-does-not-exist :create
				    :if-exists :supersede)
		 (format out "~A~%" ">>> LEFT MESSAGES")
		 (format out "~{~A~}" left-messages)
		 (format out "~A~%" ">>> RIGHT MESSAGES")
		 (format out "~{~A~}" right-messages))))))))
	       

;;(save-scenario-files "xlsx/" (loop for x from 17 to 19 collect x))
;;(save-scenario-files "xlsx/" (loop for x from 2 to 15 collect x))
;;(save-scenario-files "xlsx/" '(16) #\E #\H)
