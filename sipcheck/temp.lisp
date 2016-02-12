(defun-ps-method ps-method2 (parted-sentence parted-base-sentence outstream);; unuse
  (let ((result1 nil) 
	(result2 nil)) 
    (loop for x in parted-sentence for y in parted-base-sentence do
	 (cond
	   ((or (equal x y) (not (member y *fixed-list* :test #'equal))
		(and (equal 'string (type-of y)) (equal (subseq y 0 1) #\[)))
	    (push x result1) (push y result2))
	   (t
	    (push (list x "red") result1) (push (list y "red") result2)
	    (format outstream "Error b02: ’PŒê‚ª‡‚í‚È‚¢[~A]!=[~A]~%" x y))))
    (list (reverse result1) (reverse result2))))
