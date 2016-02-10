(defun get-message-type (parted-message)
  (let ((first-word (caaar parted-message)))
    (if (equal first-word "SIP/2.0")
	(cadaar parted-message)
	first-word)))

;;;; for sequence
(unintern '*cms-methods*)
(defvar *cms-methods* nil)
(defmacro defun-cms-method (name args &rest body)
  `(push (defun ,name ,args ,@body) *cms-methods*))
;;     ,(format nil "~A" (type-of (type-of name)))))
;;     ,(format nil "~A" (type-of name))))


(defun-cms-method cms-method1 (list1 list2 outstream)
  (unless (= (length list1) (length list2))
    (format outstream "Error a01: メッセージの数が合わない~%")))

(defun-cms-method cms-method2 (list1 list2 outstream)
  (loop for x in list1 for y in list2 for z from 1 do
       (unless (equal x y)
	 (format outstream "Error a02: ~A番目のSIPメッセージタイプが合わない[~A]!=[~A]~%" z x y))))

;;(let ((abc (make-string-output-stream))) (loop for m in *cms-methods* append (funcall m '(1 2 3) '(2 3 4 5) abc)) (get-output-stream-string abc))

(defun compare-message-sequence (sequence1 sequence2 outstream)
  (let ((type1 (loop for x in sequence1 collect (get-message-type x)))
	(type2 (loop for x in sequence2 collect (get-message-type x))))
    (loop for m in *cms-methods* append
	 (funcall m type1 type2 outstream))))

;;;; for parted sentence
(unintern '*ps-methods*)
(defvar *ps-methods* nil)
(defmacro defun-ps-method (name args &rest body)
  `(push (defun ,name ,args ,@body) *ps-methods*))

(defun-ps-method ps-method1 (parted-sentence parted-base-sentence outstream)
  (unless (= (length parted-sentence) (length parted-base-sentence))
    (format outstream "Error b01: ~Aヘッダーの構文が合わない~%"
	    (car parted-base-sentence))))

(defun-ps-method ps-method2 (parted-sentence parted-base-sentence outstream)
  (let ((result1 nil) (result2 nil)) 
    (loop for x in parted-sentence for y in parted-base-sentence do
	 (cond
	   ((or (equal x y) (not (member y *fixed-list* :test #'equal))
		(and (equal 'string (type-of y)) (equal (subseq y 0 1) #\[)))
	    (push x result1) (push y result2))
	   (t
	    (push (list x "red") result1) (push (list y "red") result2)
	    (format outstream "Error b02: 単語が合わない[~A]!=[~A]~%" x y))))
    (list (reverse result1) (reverse result2))))

(defun compare-message-sentence (parted-sentence parted-base-sentence outstream)
  (loop for m in *ps-methods* append
	 (funcall m parted-sentence parted-base-sentence outstream)))

;;(defvar *fixed-list* nil)
;;(setf *fixed-list* '(2 3))
;;(let ((abc (make-string-output-stream))) (loop for m in *ps-methods* append (funcall m '(1 2 3) '(2 3 4 5) abc)) (get-output-stream-string abc))

;;;; compact
(unintern '*compact-pairs*)
(defvar *compact-pairs* '(("i" "Call-ID")("m" "Contact")("e" "Content-Encoding")("l" "Content-Length")("c" "Content-Type")("f" "From")("s" "Subject")("k" "Supported")("t" "To")("v" "Via")))

(defun compact-to-normal (parted-message)
  (labels ((compact-member (x y)
	     (equal x (car y))))
    (loop for sentence in parted-message collect
	 (let ((compact-pair (member (car sentence) *compact-pairs*
				    :test #'compact-member)))
	   (if compact-pair
	       (cons (cadar compact-pair) (cdr sentence))
	       sentence)))))

;;(compact-to-normal '(("i"  a b c) ("m" ads de r)))

;;;;   
(defun compare-message-block (eva-message base-message outstream) ;;sip sdp block
  (let ((parted-message1 (process-a-message eva-message))
	(parted-message2 (process-a-message base-message)))
    (let ((sip-m1 (compact-to-normal (car parted-message1)))
	  (sip-m2 (compact-to-normal (car parted-message2)))
	  (sdp-m1 (cadr parted-message1))
	  (sdp-m2 (cadr parted-message2)))
      (compare-message-sequence sip-m1 sip-m2 outstream)
      (compare-message-sequence sdp-m1 sdp-m2 outstream)
      

