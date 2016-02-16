;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; for sequence
(unintern '*cms-methods*)
(defvar *cms-methods* nil)
(defmacro defun-cms-method (name args &rest body)
  `(push (defun ,name ,args ,@body) *cms-methods*))
;;     ,(format nil "~A" (type-of (type-of name)))))
;;     ,(format nil "~A" (type-of name))))


(defun-cms-method cms-method1 (list1 list2 &optional (outstream t))
  (unless (= (length list1) (length list2))
    (format outstream "Error a01: メッセージの数が合わない~%")))

(defun-cms-method cms-method2 (list1 list2 &optional (outstream t))
  (loop for x in list1 for y in list2 for z from 1 do
       (unless (equal x y)
	 (format outstream "Error a02: ~A番目のSIPメッセージタイプが合わない[~A]!=[~A]~%" z x y))))

;;(let ((abc (make-string-output-stream))) (loop for m in *cms-methods* append (funcall m '(1 2 3) '(2 3 4 5) abc)) (get-output-stream-string abc))

(defun compare-message-sequence (sequence1 sequence2 &optional (outstream t))
  (let ((type1 (loop for x in sequence1 collect (get-message-type x)))
	(type2 (loop for x in sequence2 collect (get-message-type x))))
    (loop for m in *cms-methods* append
	 (funcall m type1 type2 outstream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; for parted sentence
;;;; ps-method result quantification(unintern '*ps-methods*)
(unintern '*ps-methods*)
(defvar *ps-methods* nil)
(defmacro defun-ps-method (name args &rest body)
  `(push (defun ,name ,args ,@body) *ps-methods*))

(unintern '*ps-q-methods*) ; q for quantification which will be used in min-difference
(defvar *ps-q-methods* nil)
(defmacro defun-ps-q-method (name args &rest body)
  `(push (defun ,name ,args ,@body) *ps-q-methods*))

;;;; length difference
(defun-ps-method ps-method1 (parted-sentence parted-base-sentence &optional (outstream t))
  (let ((len1 (length parted-sentence))
	(len2 (length parted-base-sentence)))
  (unless (= len1 len2)
    (format outstream "Error b01: ~Aヘッダーの構文が合わない~%"
	    (car parted-base-sentence))
        (list len1 len2))))

(defun-ps-q-method ps-q-method1 (len-list)
  (+ 1 (if (null len-list)
      0
      (abs (- (car len-list) (cadr len-list))))))

(defun-ps-method ps-method2 (parted-sentence parted-base-sentence &optional (outstream t))
  (let ((result1 nil) 
	(result2 nil))
 ;;(masked1 (mask-sentence parted-sentence))
 ;;(masked2 (mask-sentence parted-base-sentence)))
    (loop for x in parted-sentence for y in parted-base-sentence do
	 (cond ((or (equal x y) (not (member y *fixed-list* :test #'equal)))
		(push x result1) (push y result2))
	       (t
		(push (list x "red") result1) (push (list y "red") result2)
		(format outstream "Error b02: 単語が合わない[~A]!=[~A]~%" x y))))
    (let ((len1 (length parted-sentence))
	  (len2 (length parted-base-sentence)))
      (if (> len1 len2)
	  (loop for x from len2 to (- len1 1) do (push (nth x parted-sentence) result1))
	  (loop for x from len1 to (- len2 1) do (push (nth x parted-base-sentence) result2)))
    (list (list "sentence-diff" (reverse result1) (reverse result2))))))

(defun-ps-q-method ps-q-method2 (result-list)
  (apply #'+ (cons 1 (mapcar (lambda (x) (if (atom x) 0 1)) (cadar result-list)))))

(defun compare-message-sentence (parted-sentence parted-base-sentence &optional (outstream t))
  (loop for m in *ps-methods* append
	 (funcall m parted-sentence parted-base-sentence outstream)))

;;(defvar *fixed-list* nil)
;;(setf *fixed-list* '(2 3))
;;(let ((abc (make-string-output-stream))) (print (loop for m in *ps-methods* append (funcall m '(1 2 3) '(2 3 4 5) abc))) (get-output-stream-string abc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; fixed-list
(defvar *fixed-list* nil)
(setf *fixed-list* '("v" "o" "s" "-" "c" "IN" "t" "m" "audio" "RTP/AVP" "sendrecv" "fmtp:0" "rtpmap:0" "PCMU/8000" "AMR-WB/16000/1" "fmtp:96" "rtpmap:96" "2" "AMR/8000/1" "fmtp:97" "rtpmap:97" "mode-set" "7" "telephone-event/16000" "fmtp:98" "rtpmap:98" "rtpmap" "telephone-event/8000" "red" "a" #\= "fmtp" #\: "fmtp:99" "rtpmap:99" "0-15""0" "96" "97" "98" "99" "cpc" "ordinary" "user" "phone"))

