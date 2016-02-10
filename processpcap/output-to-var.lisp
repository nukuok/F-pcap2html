(defvar *sip-sequence* nil)
(defun otv-initial-result ()
  (setf *sip-sequence* nil))

(defun otv-2result (x)
  (push x *sip-sequence*))

(defun otv-result-done ()
  (nreverse *sip-sequence*))




