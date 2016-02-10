(defun mame (instream current-position number-list function)
  (let ((result
	 (funcall function
		(mapcar
		 (lambda (x) 
		   (progn
		     (file-position instream (+ current-position x))
		     (read-byte instream)))
		 number-list))))
    (file-position instream current-position)
    result))

(defun mame2 (instream number-list function)
  (let* ((initial-position (file-position instream))
	 (read-data (mapcar
		     (lambda (x) 
		      (progn
			(file-position instream (+ initial-position x))
			(read-byte instream)))
		     number-list))
	 (result (funcall function read-data)))
    (file-position instream initial-position)
    result))

(defun pp-pcapng-p (instream current-position)
  (mame instream current-position '(0 1 2 3)
	(lambda (x) (equal x '(#x0a #x0d #x0d #x0a)))))

(defun pp-length-calculate (x)
  (if (null x)
      0
      (+ (car x) (* 256 (pp-length-calculate (cdr x))))))

(defun pp-pcapng-packet-skip (instream)
  (unless (mame2 instream '(0 1 2 3)
		 (lambda (x) (equal x '(#x06 #x00 #x00 #x00))))
    (file-position instream (mame2 instream '(4 5 6 7) #'pp-length-calculate))))

(defun header-skip-function-pcap (instream)
  (let ((initial-position (file-position instream))
	(result (mame2 instream '(8 9 10 11) #'pp-length-calculate)))
    (file-position-forward instream 16)
    (+ initial-position 16 result)))

(defun header-skip-function-pcapng (instream)
  (let ((initial-position (file-position instream))
	(result (mame2 instream '(4 5 6 7) #'pp-length-calculate)))
    ;;(print result)
    (if (mame2 instream '(0 1 2 3)
	       (lambda (x) (equal x '(#x06 #x00 #x00 #x00))))
	(progn (file-position-forward instream 28) ;; (+ 16 12)
	       (+ initial-position result))
	(file-position instream (+ initial-position result)))))

(defun L2-header-length-detect (instream)
  (if (mame2 instream '(0 2 4) (lambda (x) (equal 0 (apply #'+ x))))
      16
      14))

(defun vlan-tag-skip (instream)
  (when (mame2 instream '(-2 -1) (lambda (x) (equal x '(129 0))))
    (file-position-forward instream 4)))

(defun pcap-process (filename)
  (with-open-file
      (instream filename :direction :input :element-type '(unsigned-byte 8))
    (let ((header-skip-function #'header-skip-function-pcap)
	  (L2-header-length 0)
	  (filelength (file-length instream)))
      (if (pp-pcapng-p instream 0) 
	(setf header-skip-function #'header-skip-function-pcapng)
	(file-position instream 24))
      (otv-initial-result)
      (r-initial-rtp)
      (loop 
	 ;;(print (incf index))
	 (let ((next-header-position (funcall header-skip-function instream)))
	   (unless (equal next-header-position (file-position instream))
	     (when (equal L2-header-length 0)
	       (setf L2-header-length (L2-header-length-detect instream)))
	     (cond ((equal 16 L2-header-length) (process-L2-16 instream))
		   ((equal 14 L2-header-length) (process-L2-14 instream))))
	   (file-position instream next-header-position)
	   (when (equal (file-position instream) filelength) (return))))
      (r-rtp-done filename)
      (otv-result-done))))
	

(defun process-L2-16 (instream)
  (when (mame2 instream '(1)
	       (lambda (x) (or (equal 0 (car x)) (equal 4 (car x)))))
    (file-position-forward instream 16)
    (vlan-tag-skip instream)
    (let ((ip-version (mame2 instream '(0) (lambda (x) (ash (car x) -4)))))
      (cond ((equal ip-version 4) (process-ip4 instream))
	    ((equal ip-version 6) (process-ip6 instream))))))

(defun process-L2-14 (instream)
  (file-position-forward instream 14)
  (let ((ip-version (mame2 instream '(0) (lambda (x) (ash (car x) -4)))))
    (cond ((equal ip-version 4) (process-ip4 instream))
	  ((equal ip-version 6) (process-ip6 instream)))))

(defun pp-length-calculate-from-left (x)
  (reduce (lambda (x y) (+ (* 256 x) y)) x))

(defun process-ip4 (instream)
  (let* ((next-protocol (car (mame2 instream '(9) #'lib)))
	 (ip-header-length
	  (mame2 instream '(0)
		 (lambda (x) (ash (logand (car x) #x0f) 2))))
	 (payload-length
	  (- (mame2 instream '(2 3) #'pp-length-calculate-from-left)
	   ip-header-length))
	 (ip-src (mame2 instream '(12 13 14 15) #'lib))
	 (ip-dst (mame2 instream '(16 17 18 19) #'lib)))
    (file-position-forward instream ip-header-length)
    (cond ((equal next-protocol 6)
	   (process-tcp instream ip-src ip-dst payload-length))
	  ((equal next-protocol 17)
	   (process-udp instream ip-src ip-dst)))))
	   ;;(process-udp instream ip-src ip-dst payload-length)))))

(defun process-ip6 (instream)
  (let ((next-protocol (car (mame2 instream '(6) #'lib)))
	(ip-header-length 40)
	(payload-length
	 (mame2 instream '(4 5) #'pp-length-calculate-from-left))
	(ip-src (mame2 instream '( 8  9 10 11 12 13 14 15
				  16 17 18 19 20 21 22 23) #'lib))
	(ip-dst (mame2 instream '(24 25 26 27 28 29 30 31
				  32 33 34 35 36 37 38 39) #'lib)))
    (file-position-forward instream ip-header-length)
    (cond ((equal next-protocol 6)
	   (process-tcp instream ip-src ip-dst payload-length))
	  ((equal next-protocol 17)
	   (process-udp instream ip-src ip-dst)))))
	   ;;(process-udp instream ip-src ip-dst payload-length)))))

(defun process-tcp (instream ip-src ip-dst payload-length)
  (let* ((header-length (mame2 instream '(12)
			       (lambda (x) (ash (ash (car x) -4) 2))))
	 (data-length (- payload-length header-length)))
    (file-position-forward instream header-length)
    (when (> data-length 0)
      (let ((data (mk-byte-array data-length)))
	(read-sequence data instream)
	(when (data-sip-p data)
	  (otv-2result (list (coerce ip-src 'array)
			     (coerce ip-dst 'array)
			     data)))))))

(defun data-sip-p (inarray)
  (let ((templength (length inarray)))
    (member '(83 73 80)
	    (loop for x from 0 to (- templength 3)
	       collect (list (aref inarray x)
			     (aref inarray (+ x 1))
			     (aref inarray (+ x 2))))
	    :test #'equal)))

;;(defun process-udp (instream ip-src ip-dst payload-length-from-up)
(defun process-udp (instream ip-src ip-dst)
  (let* ((port-src (mame2 instream '(0 1) #'pp-length-calculate-from-left))
	 (port-dst (mame2 instream '(2 3) #'pp-length-calculate-from-left))
	 (total-length (mame2 instream '(4 5) #'pp-length-calculate-from-left))
	 (payload-type (mame2 instream '(9)
			      (lambda (x) (logand #x7f (car x)))))
	 (sequence-number (mame2 instream '(10 11) #'pp-length-calculate-from-left))
	 (data-length (- total-length 8))
	 (payload-length (- total-length 20)))
    (file-position-forward instream 8)
    (let ((data (mk-byte-array data-length)))
      (read-sequence data instream)
      (file-position-forward instream (- data-length))
      (if (data-sip-p data)
	  (otv-2result (list (coerce ip-src 'array)
			     (coerce ip-dst 'array)
			     data))
	  (when (> payload-length 0)
	    ;; in some udp packet, some packet is shorter than 17 byte
	    (let ((payload (mk-byte-array payload-length)))
	      (file-position-forward instream 12)
	      (read-sequence payload instream)
	      (r-push-rtp
	       (list
		(list ip-src ip-dst port-src port-dst payload-type)
		(list sequence-number payload)))))))))

;; M-? who-calls
