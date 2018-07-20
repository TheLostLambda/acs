;;;; This file contains various useful aliases and functions that aren't strongly bound to any one aspect of the program

(defun lookup (key alist)
  "Returns only the resultant value of an assoc call"
  (cdr (assoc key alist)))

(defun ensure-string-content (str)
  "Returns NIL if the string is empty. Returns the string otherwise."
  (unless (= (length (string-trim " " str)) 0) str))

(defun msg-type (msg)
  "Returns the mime type of the message it's called on"
  (lookup :|content_type| msg))

(defun msg-valid (msg)
  (every (lambda (data) (or (not (stringp data)) (ensure-string-content data))) msg))

(defun resolve-media (uri)
  "Takes a remote uri and returns the local path to that media object."
  (let* ((safe-uri (format-uri uri))
	 (file-type (pathname-type safe-uri)))
    (flet ((type-path (media type) (merge-pathnames media (concatenate 'string (pathname-name safe-uri) "." type))))
      (if file-type
	  (when (probe-file (type-path *media-dir* file-type)) (type-path "media/" file-type))
	  (cond ((probe-file (type-path *media-dir* "png")) (type-path "media/" "png"))
		((probe-file (type-path *media-dir* "webp")) (type-path "media/" "webp"))
		(t nil))))))

(defun format-uri (uri-str)
  "Formats content URIs so they can be fetched with adb. It drops the `file://`, decodes %-encoded characters, and replaces emulated storage paths with the actual path"
  (let ((sdcard "/storage/emulated/0/")
	(uri (quri:url-decode (quri:uri-path (quri:uri uri-str)))))
    (if (search sdcard uri)
	(concatenate 'string "/sdcard/" (subseq uri (length sdcard)))
	uri)))

(defun unix-to-date (unix-nano)
  "Converts millisecond Unix timestamps into human-readable dates"
  (let* ((unix (round (/ unix-nano 1000000)))
	 (universal (+ unix (encode-universal-time 0 0 0 1 1 1970 0))))
    (multiple-value-bind (second minute hour day month year) (decode-universal-time universal)
      (format nil "~D.~D.~D (~2,'0D:~2,'0D:~2,'0D)" year month day hour minute second))))

(defun date-to-unix (date-str)
  (let ((time-values (mapcar #'parse-integer (cl-ppcre:split "\\.|:|\\(|\\)" date-str)))
	(unix-shift (encode-universal-time 0 0 0 1 1 1970 0)))
    (multiple-value-bind (year month day hour minute second) (values-list time-values)
      (* 1000000 (- (encode-universal-time (or second 0) (or minute 0) (or hour 0) day month year) unix-shift)))))

(defun kvlist-to-alist (lst)
  "Converts a list in (:key val :key2 val2) form to an alist"
  (labels ((convert (alist kvlist)
	     (if kvlist
		 (convert (cons (cons (car kvlist) (cadr kvlist)) alist) (nthcdr 2 kvlist))
		 alist)))
    (convert () lst)))
