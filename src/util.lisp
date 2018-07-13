;;;; This file contains various useful aliases and functions that aren't strongly bound to any one aspect of the program

(defun lookup (key alist)
  "Returns only the resultant value of an assoc call"
  (cdr (assoc key alist)))

(defun msg-type (msg)
  "Returns the mime type of the message it's called on"
  (lookup :|content_type| msg))

(defun format-uri (uri-str)
  "Formats content URIs so they can be fetched with adb. It drops the `file://`, decodes %-encoded characters, and replaces emulated storage paths with the actual path"
  (let ((sdcard "/storage/emulated/0/")
	(uri (quri:url-decode (quri:uri-path (quri:uri uri-str)))))
    (if (search sdcard uri)
	(concatenate 'string "/sdcard/" (subseq uri (length sdcard)))
	uri)))

(defun unix-to-date (unix-milli)
  "Converts millisecond Unix timestamps into human-readable dates"
  (let* ((unix (round (/ unix-milli 1000)))
	 (universal (+ unix (encode-universal-time 0 0 0 1 1 1970 0))))
    (multiple-value-bind (second minute hour date month year) (decode-universal-time universal)
      (format nil "~D.~D.~D (~2,'0D:~2,'0D:~2,'0D)" year month date hour minute second))))

(defun kvlist-to-alist (lst)
  "Converts a list in (:key val :key2 val2) form to an alist"
  (labels ((convert (alist kvlist)
	     (if kvlist
		 (convert (cons (cons (car kvlist) (cadr kvlist)) alist) (nthcdr 2 kvlist))
		 alist)))
    (convert () lst)))
