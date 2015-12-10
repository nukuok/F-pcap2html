(defvar *ip-header-list-v4*)
(setf *ip-header-list-v4*
      '((222 231 228 186)
	(222 231 228 139)
	(222 231 228 11)
	(222 231 228 58)))

(defvar *ip-header-list-v4-2*)
(setf *ip-header-list-v4-2*
      '((222 231 228 187)
	(222 231 228 140)
	(222 231 228 12)
	(222 231 228 59)))

(defvar *ip-header-list-v6*)
(setf *ip-header-list-v6*
      '((32 1 167 255 33 0 0 13 0 0 0 0 0 0 240 1)
	(32 1 167 255 33 0 0 13 0 0 0 0 0 0 0 17)
	(32 1 167 255 33 0 0 12 0 0 0 0 0 0 0 17)
	(32 1 167 255 33 0 0 12 0 0 0 0 0 0 240 1)))

(defvar *ip-header-list* nil)
(setf *ip-header-list*
      (list
       *ip-header-list-v4*
       *ip-header-list-v4-2*
       *ip-header-list-v6*
       ))
