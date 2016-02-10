(defun last1 (lst)
  (car (last lst)))

(defmacro seq-to-string (seq)
  `(coerce (mapcar
	    #'code-char
	    (coerce ,seq 'list))
	   'string))

(defmacro mk-byte-array (length)
  `(make-array ,length :initial-element 0 :element-type '(unsigned-byte 8)))

(defun pcap-output (filename)
  (let ()
    (with-open-file
	(inputfile filename :direction :input :element-type '(unsigned-byte 8))
      (let (buff)
	(dotimes (x 2000)
	;(loop
	   (unless (setq buff (read-byte inputfile nil ))
	     (return))
	   ;;(format t "~x" (coerce buff 'list)))))))
	   (format t "~2,'0x" buff))))))

(defun file-position-forward (instream byte-number)
  (file-position instream
		 (+ byte-number (file-position instream))))

(defun lib (x) x) ;; let it be 

(defun output-byte-data (data)
    (format t "~{~C~} " (mapcar #'code-char (coerce data 'list))))

(defun output-byte-data-x (data)
    (format t "~{~2,'0x~} " data))

;; pcapng: 
;; 06000000 block type           # does not exist in pcap file
;; 6C000000 block total length   # does not exist in pcap file
;; 00000000 interface id         # does not exist in pcap file
;; 56210500 timestamp (high)
;; AA390A68 timestramp (low)
;; 4A000000 captured len
;; 4A000000 packet len
;; 000087E4 72590002 BBA8BED9 08004500 003C2B5D 0000FF01 0A0BDEE7 E447DEE7
;; E4410800 30C51C45 4ADB0000 00005612 25B40000 0000000A 11775A5A 5A5A5A5A
;; 5A5A5A5A 5A5A5A5A 5A5A0000    # isn't resized to 32bits in pcap file
;; 6C000000 block total length   # does not exist in pcap file

;; pcap:
;; 56210500 timestamp (high)
;; AA390A68 timestramp (low)
;; 4A000000 captured len
;; 4A000000 packet len
;; 000087E4 72590002 BBA8BED9 08004500 003C2B5D 0000FF01 0A0BDEE7 E447DEE7
;; E4410800 30C51C45 4ADB0000 00005612 25B40000 0000000A 11775A5A 5A5A5A5A
;; 5A5A5A5A 5A5A5A5A 5A5A


;;(defun pp-length-calculate (x)
;;  (reduce (lambda (ix iy) (+ (* 16 ix) iy)) x))

;;			 (lambda (x) (pp-length-calculate x))))))

;;(defmacro if-let (condition yy nn )
;;  ( 


;;(current-position 0)
	  ;;(next-packet-position 0)
	  ;;(is-pcapng-format nil)
	  ;;(index 0)
