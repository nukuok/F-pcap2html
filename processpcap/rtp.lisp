(defvar *rtp-data* nil)
(defvar *rtp-file-list* nil)

(defun r-initial-rtp ()
  (setf *rtp-data* nil)
  (setf *rtp-file-list* nil))

;;'((ip-src ip-dst port-src port-dst payload-type)
;; (sequence-number data)
;; (sequence-number data))

(defun r-push-rtp (x)
  (unless (assoc (car x) *rtp-data* :test #'equal)
    (push (list (car x)) *rtp-data*))
  (unless (assoc (caadr x)
		 (assoc (car x) *rtp-data* :test #'equal)
		 :test #'equal)
    (nconc (assoc (car x) *rtp-data* :test #'equal) (cdr x))))

(defun r-write-bytes (sequence outstream)
  (loop for x in (coerce sequence 'list) do
       (write-byte x outstream)))

(defun r-make-file (od ext-name format-header x)
  (let ((out-wav-name
	 (concatenate
	  'string
	  od
	  "-d/"
	  (if (> (length (caar x)) 4)
	   (format nil "~{~{~2,'0x~2,'0x_~2,'0x~2,'0x_~2,'0x~2,'0x_~2,'0x~2,'0x_~2,'0x~2,'0x_~2,'0x~2,'0x_~2,'0x~2,'0x_~2,'0x~2,'0x~}-~{~2,'0x~2,'0x_~2,'0x~2,'0x_~2,'0x~2,'0x_~2,'0x~2,'0x_~2,'0x~2,'0x_~2,'0x~2,'0x_~2,'0x~2,'0x_~2,'0x~2,'0x~}~}" (subseq (car x) 0 2))
	   (format nil "~{~{~d_~d_~d_~d~}-~{~d_~d_~d_~d~}~}"
		   (subseq (car x) 0 2)))
	  (format nil "~{-~d~}" (subseq (car x) 2))
	  ext-name)))
    (with-open-file
	(outstream
	 out-wav-name
	 :direction :output
	 :if-exists :supersede
	 :if-does-not-exist :create
	 :element-type '(unsigned-byte 8))
      (r-write-bytes format-header outstream)
      (loop for y in (cdr x) do
	   (r-write-bytes (cadr y) outstream)))))

(defun r-rtp-done (filename)
  (let ((output-directory
	 (subseq filename 0 (position #\. filename))))
    (loop for x in *rtp-data* do
	 (cond ((< (length (cadadr x)) 40)
		(r-make-file output-directory ".amr"
			     #(#x23 #x21 #x41 #x4d #x52 #x0A) x))
	       ((< (length (cadadr x)) 130)
		(r-make-file output-directory ".awb"
			     #(#x23 #x21 #x41 #x4d #x52 #x2d #x57 #x42 #x0a) x))
	       (t (let* ((payload-length (length (cadadr x)))
			 (payload-number (length (cdr x)))
			 (sample-number (* payload-length payload-number))
			 (file-bytes (+ 58 sample-number))
			 (format-header
			  (coerce
			   (list
			    #x52 #x49 #x46 #x46;; RIFF
			    (logand #xff file-bytes) 
			    (ash (logand #xffff file-bytes) -8) 
			    (ash (logand #xffffff file-bytes) -16) 
			    (ash file-bytes -24) 
			    #x57 #x41 #x56 #x45;; WAVE
			    #x66 #x6D #x74 #x20;; fmt 
			    #x12 #x00 #x00 #x00;; format byte number

			    #x07 #x00;; format id
			    #x01 #x00;; channel number
			    #x40 #x1F #x00 #x00;; sampling rate
			    #x40 #x1F #x00 #x00;;
			    #x01 #x00 #x08 #x00;;
			    #x00 #x00;; extended format size

			    #x66 #x61 #x63 #x74;; fact
			    #x04 #x00 #x00 #x00;; fact chunch byte number
			    (logand #xff sample-number) 
			    (ash (logand #xffff sample-number) -8) 
			    (ash (logand #xffffff sample-number) -16) 
			    (ash sample-number -24) 
			    #x64 #x61 #x74 #x61;; data                     
			    (logand #xff sample-number) 
			    (ash (logand #xffff sample-number) -8) 
			    (ash (logand #xffffff sample-number) -16) 
			    (ash sample-number -24)) 
			   'array)))
		    (r-make-file output-directory ".pcmu"
				 format-header x)))))))
			 

	       
		    
